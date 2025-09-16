module Auth.Protocol.OAuth exposing (onAuthCallbackReceived, onFrontendCallbackInit)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Browser.Navigation as Navigation
import Dict
import Http
import Json.Decode as Json
import OAuth.AuthorizationCode as OAuth
import Task exposing (Task)
import Time
import Url exposing (Url)


onFrontendCallbackInit :
    { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> Url
    -> Navigation.Key
    -> (Auth.Common.ToBackend -> Cmd frontendMsg)
    -> ( { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }, Cmd frontendMsg )
onFrontendCallbackInit model methodId origin navigationKey toBackendFn =
    let
        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString model.authRedirectBaseUrl)
    in
    case OAuth.parseCode origin of
        OAuth.Empty ->
            ( { model | authFlow = Idle }
            , Cmd.none
            )

        OAuth.Success { code, state } ->
            let
                state_ =
                    state |> Maybe.withDefault ""

                model_ =
                    { model | authFlow = Authorized code state_ }

                ( newModel, newCmds ) =
                    accessTokenRequested model_ methodId code state_
            in
            ( newModel
            , Cmd.batch [ toBackendFn newCmds, clearUrl ]
            )

        OAuth.Error error ->
            ( { model | authFlow = Errored <| ErrAuthorization error }
            , clearUrl
            )


accessTokenRequested :
    { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> OAuth.AuthorizationCode
    -> Auth.Common.State
    -> ( { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }, Auth.Common.ToBackend )
accessTokenRequested model methodId code state =
    ( { model | authFlow = Authorized code state }
    , AuthCallbackReceived methodId model.authRedirectBaseUrl code state
    )


onAuthCallbackReceived sessionId clientId method receivedUrl code state now asBackendMsg backendModel =
    ( backendModel
    , validateCallbackToken method.clientId method.clientSecret method.tokenEndpoint receivedUrl code
        |> Task.andThen
            (\authenticationResponse ->
                case backendModel.pendingAuths |> Dict.get sessionId of
                    Just pendingAuth ->
                        if pendingAuth.state == state then
                            method.getUserInfo
                                authenticationResponse
                                |> Task.map
                                    (\userInfo ->
                                        let
                                            authToken =
                                                Just (makeToken method.id authenticationResponse now)
                                        in
                                        ( userInfo, authToken )
                                    )

                        else
                            Task.fail <| Auth.Common.ErrAuthString "Invalid auth state. Please log in again or report this issue."

                    Nothing ->
                        Task.fail <| Auth.Common.ErrAuthString "Couldn't validate auth, please login again."
            )
        |> Task.attempt (Auth.Common.AuthSuccess sessionId clientId method.id now >> asBackendMsg)
    )


validateCallbackToken :
    String
    -> String
    -> Url
    -> Url
    -> OAuth.AuthorizationCode
    -> Task Auth.Common.Error OAuth.AuthenticationSuccess
validateCallbackToken clientId clientSecret tokenEndpoint redirectUri code =
    let
        req =
            OAuth.makeTokenRequest (always ())
                { credentials =
                    { clientId = clientId
                    , secret = Just clientSecret
                    }
                , code = code
                , url = tokenEndpoint
                , redirectUri = { redirectUri | query = Nothing, fragment = Nothing }
                }
    in
    { method = req.method
    , headers = req.headers ++ [ Http.header "Accept" "application/json" ]
    , url = req.url
    , body = req.body
    , resolver = HttpHelpers.jsonResolver OAuth.defaultAuthenticationSuccessDecoder
    , timeout = req.timeout
    }
        |> Http.task
        |> Task.mapError parseAuthenticationResponseError


parseAuthenticationResponseError : Http.Error -> Auth.Common.Error
parseAuthenticationResponseError httpErr =
    case httpErr of
        Http.BadBody body ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    Auth.Common.ErrAuthentication error

                _ ->
                    Auth.Common.ErrHTTPGetAccessToken

        _ ->
            Auth.Common.ErrHTTPGetAccessToken


makeToken : Auth.Common.MethodId -> OAuth.AuthenticationSuccess -> Time.Posix -> Auth.Common.Token
makeToken methodId authenticationSuccess now =
    { methodId = methodId
    , token = authenticationSuccess.token
    , created = now
    , expires =
        (Time.posixToMillis now
            + ((authenticationSuccess.expiresIn |> Maybe.withDefault 0) * 1000)
        )
            |> Time.millisToPosix
    }
