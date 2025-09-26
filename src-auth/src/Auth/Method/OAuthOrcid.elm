module Auth.Method.OAuthOrcid exposing (configuration)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Auth.Protocol.OAuth
import Dict exposing (Dict)
import Http
import JWT exposing (..)
import JWT.JWS as JWS
import Json.Decode as Json
import OAuth.AuthorizationCode as OAuth
import Task exposing (Task)
import Url exposing (Url)


configuration :
    String
    -> String
    ->
        Method
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
configuration clientId clientSecret =
    ProtocolOAuth
        { id = "OAuthOrcid"
        , authorizationEndpoint = { defaultHttpsUrl | host = "sandbox.orcid.org", path = "/oauth/authorize", query = Just "prompt=login" }
        , tokenEndpoint = { defaultHttpsUrl | host = "sandbox.orcid.org", path = "/oauth/token" }
        , logoutEndpoint = Home { returnPath = "/signout" }
        , allowLoginQueryParameters = True
        , clientId = clientId
        , clientSecret = clientSecret
        , scope = [ "openid" ]
        , getUserInfo = getUserInfo
        , onFrontendCallbackInit = Auth.Protocol.OAuth.onFrontendCallbackInit
        , placeholder = \_ -> ()

        -- , onAuthCallbackReceived = Debug.todo "onAuthCallbackReceived"
        }


getUserInfo :
    OAuth.AuthenticationSuccess
    -> Task Auth.Common.Error UserInfo
getUserInfo authenticationSuccess =
    let
        extract : String -> Json.Decoder a -> Dict String Json.Value -> Result String a
        extract k d v =
            Dict.get k v
                |> Maybe.map
                    (\v_ ->
                        Json.decodeValue d v_
                            |> Result.mapError Json.errorToString
                    )
                |> Maybe.withDefault (Err <| "Key " ++ k ++ " not found")

        extractOptional : a -> String -> Json.Decoder a -> Dict String Json.Value -> Result String a
        extractOptional default k d v =
            Dict.get k v
                |> Maybe.map
                    (\v_ ->
                        Json.decodeValue d v_
                            |> Result.mapError Json.errorToString
                    )
                |> Maybe.withDefault (Ok <| default)

        tokenR =
            case authenticationSuccess.idJwt of
                Nothing ->
                    Err "Identity JWT missing in authentication response. Please report this issue."

                Just idJwt ->
                    case JWT.fromString idJwt of
                        Ok (JWS t) ->
                            Ok t

                        Err err ->
                            Err <| jwtErrorToString err

        stuff =
            tokenR
                |> Result.andThen
                    (\token ->
                        let
                            meta =
                                token.claims.metadata
                        in
                        Result.map4
                            (\name family_name given_name sub ->
                                { name = name
                                , family_name = family_name
                                , given_name = given_name
                                , sub = sub
                                }
                            )
                            (extractOptional Nothing "name" (Json.string |> Json.nullable) meta)
                            (extractOptional Nothing "family_name" (Json.string |> Json.nullable) meta)
                            (extractOptional Nothing "given_name" (Json.string |> Json.nullable) meta)
                            (extract "sub" Json.string meta)
                    )

        debug =
            Debug.log "tokenR: " tokenR

        nothingInsteadOfJustEmptyString a =
            if String.length a > 0 then
                Just a

            else
                Nothing

        orElse ma mb =
            case mb of
                Nothing ->
                    ma

                Just _ ->
                    mb
    in
    Task.mapError (Auth.Common.ErrAuthString << HttpHelpers.httpErrorToString) <|
        case stuff of
            Ok result ->
                Task.succeed
                    { email = Nothing
                    , name =
                        result.name
                            |> orElse
                                ([ result.given_name, result.family_name ]
                                    |> List.filterMap identity
                                    |> String.join " "
                                    |> nothingInsteadOfJustEmptyString
                                )
                    , username = Just result.sub
                    , unique = result.sub
                    }

            Err err ->
                Task.fail (Http.BadBody err)


jwtErrorToString err =
    case err of
        TokenTypeUnknown ->
            "Unsupported auth token type."

        JWSError decodeError ->
            case decodeError of
                JWS.Base64DecodeError ->
                    "Base64DecodeError"

                JWS.MalformedSignature ->
                    "MalformedSignature"

                JWS.InvalidHeader jsonError ->
                    "InvalidHeader: " ++ Json.errorToString jsonError

                JWS.InvalidClaims jsonError ->
                    "InvalidClaims: " ++ Json.errorToString jsonError
