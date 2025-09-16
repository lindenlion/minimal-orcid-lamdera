module Auth.Flow exposing (BackendUpdateConfig, backendUpdate, methodLoader, setError, updateFromFrontend)

import Auth.Common exposing (MethodId)
import Auth.Protocol.OAuth
import Dict exposing (Dict)
import List.Extra as List
import Task
import Time


updateFromFrontend { asBackendMsg } clientId sessionId authToBackend model =
    case authToBackend of
        Auth.Common.AuthCallbackReceived methodId receivedUrl code state ->
            ( model
            , Time.now
                |> Task.perform
                    (\now ->
                        asBackendMsg <|
                            Auth.Common.AuthCallbackReceived_
                                sessionId
                                clientId
                                methodId
                                receivedUrl
                                code
                                state
                                now
                    )
            )


type alias BackendUpdateConfig frontendMsg backendMsg toFrontend frontendModel backendModel =
    { asToFrontend : Auth.Common.ToFrontend -> toFrontend
    , asBackendMsg : Auth.Common.BackendMsg -> backendMsg
    , sendToFrontend : Auth.Common.SessionId -> toFrontend -> Cmd backendMsg
    , backendModel : { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }
    , loadMethod : Auth.Common.MethodId -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel)
    , handleAuthSuccess :
        Auth.Common.SessionId
        -> Auth.Common.ClientId
        -> Auth.Common.UserInfo
        -> MethodId
        -> Maybe Auth.Common.Token
        -> Time.Posix
        -> ( { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }, Cmd backendMsg )
    , renewSession : Auth.Common.SessionId -> Auth.Common.ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    , logout : Auth.Common.SessionId -> Auth.Common.ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    , isDev : Bool
    }


backendUpdate :
    BackendUpdateConfig
        frontendMsg
        backendMsg
        toFrontend
        frontendModel
        { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }
    -> Auth.Common.BackendMsg
    -> ( { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }, Cmd backendMsg )
backendUpdate { asToFrontend, asBackendMsg, sendToFrontend, backendModel, loadMethod, handleAuthSuccess, renewSession, logout, isDev } authBackendMsg =
    let
        authError str =
            asToFrontend (Auth.Common.AuthError (Auth.Common.ErrAuthString str))

        withMethod methodId clientId fn =
            case loadMethod methodId of
                Nothing ->
                    ( backendModel
                    , sendToFrontend clientId <| authError ("Unsupported auth method: " ++ methodId)
                    )

                Just method ->
                    fn method
    in
    case authBackendMsg of
        Auth.Common.AuthCallbackReceived_ sessionId clientId methodId receivedUrl code state now ->
            withMethod methodId
                clientId
                (\method ->
                    case method of
                        Auth.Common.ProtocolEmailMagicLink config ->
                            config.onAuthCallbackReceived sessionId clientId receivedUrl code state now asBackendMsg backendModel

                        Auth.Common.ProtocolOAuth config ->
                            Auth.Protocol.OAuth.onAuthCallbackReceived sessionId clientId config receivedUrl code state now asBackendMsg backendModel
                )

        Auth.Common.AuthSuccess sessionId clientId methodId now res ->
            let
                removeSession backendModel_ =
                    { backendModel_ | pendingAuths = backendModel_.pendingAuths |> Dict.remove sessionId }
            in
            withMethod methodId
                clientId
                (\_ ->
                    case res of
                        Ok ( userInfo, authToken ) ->
                            handleAuthSuccess sessionId clientId userInfo methodId authToken now
                                |> Tuple.mapFirst removeSession

                        Err err ->
                            ( backendModel, sendToFrontend sessionId (asToFrontend <| Auth.Common.AuthError err) )
                )


setError :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> Auth.Common.Error
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Cmd msg )
setError model err =
    setAuthFlow model <| Auth.Common.Errored err


setAuthFlow :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> Auth.Common.Flow
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Cmd msg )
setAuthFlow model flow =
    ( { model | authFlow = flow }, Cmd.none )


methodLoader : List (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel) -> Auth.Common.MethodId -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel)
methodLoader methods methodId =
    methods
        |> List.find
            (\config ->
                case config of
                    Auth.Common.ProtocolEmailMagicLink method ->
                        method.id == methodId

                    Auth.Common.ProtocolOAuth method ->
                        method.id == methodId
            )
