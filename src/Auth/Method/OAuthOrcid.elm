module Auth.Method.OAuthOrcid exposing (configuration)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Auth.Protocol.OAuth
import Dict exposing (Dict)
import Env
import Http
import JWT exposing (..)
import Json.Decode as Json
import OAuth.AuthorizationCode as OAuth
import Task exposing (Task)
import Time
import Url exposing (Url)


configuration :
    String
    -> String
    -> Bool
    ->
        Method
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
configuration clientId clientSecret sandbox =
    let
        host =
            if sandbox then
                "sandbox.orcid.org"

            else
                "orcid.org"
    in
    ProtocolOAuth
        { id = "OAuthOrcid"
        , authorizationEndpoint = { defaultHttpsUrl | host = host, path = "/oauth/authorize", query = Nothing }
        , tokenEndpoint = { defaultHttpsUrl | host = host, path = "/oauth/token" }
        , logoutEndpoint = Home { returnPath = "/signout" }
        , allowLoginQueryParameters = True
        , clientId = clientId
        , clientSecret = clientSecret
        , scope = [ "openid" ]
        , getUserInfo = getUserInfo host
        , onFrontendCallbackInit = Auth.Protocol.OAuth.onFrontendCallbackInit
        , placeholder = \_ -> ()
        }


getUserInfo :
    String
    -> OAuth.AuthenticationSuccess
    -> Time.Posix
    -> Task Auth.Common.Error UserInfo
getUserInfo issuer authenticationSuccess now =
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
                    case
                        JWT.fromString idJwt
                    of
                        Ok (JWS t) ->
                            Ok t

                        Err err ->
                            Err <| jwtErrorToString err

        signature =
            tokenR
                |> Result.andThen
                    (\token ->
                        JWT.isValid
                            { issuer = Just <| "https://" ++ issuer
                            , audience = Just Env.orcidAppClientId
                            , subject = Nothing
                            , jwtID = Nothing
                            , leeway = 1000
                            }
                            "RSA keys are not supported yet, so it is currently not possible to verify the JWT signature."
                            now
                            (JWS token)
                            -- TODO: get better JWT signature error to String conversion
                            |> Result.mapError (\_ -> "JWT Signature Error")
                    )

        claims =
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
            Debug.log "claims: " claims

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
        case claims of
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
    -- TODO: find better way to get more detailed error messages than vendoring JWT package.
    case err of
        TokenTypeUnknown ->
            "Is that really a JWT token? I don't recognize this format. I expected something like bli.bla.blu with 2 dots separating three parts of a base64 encoded string."

        _ ->
            "JWT token decoding trouble: either base64 problem, or invalid header or claims, or signature malformed."
