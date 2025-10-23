module Auth exposing (backendConfig, updateFromBackend)

import Auth.Common exposing (Method(..), UserInfo)
import Auth.Flow
import Auth.Method.OAuthOrcid
import Dict exposing (Dict)
import Dict.Extra as Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)


orcidConfig =
    Auth.Method.OAuthOrcid.configuration Env.orcidAppClientId Env.orcidAppClientSecret Env.useOrcidSandbox


backendConfig : BackendModel -> Auth.Flow.BackendUpdateConfig FrontendMsg BackendMsg ToFrontend FrontendModel BackendModel
backendConfig model =
    { asToFrontend = AuthToFrontend
    , asBackendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = model
    , loadMethod = Auth.Flow.methodLoader [ orcidConfig ]
    , handleAuthSuccess = handleAuthSuccess model
    , isDev =
        case Env.mode of
            Env.Production ->
                False

            Env.Development ->
                True
    , renewSession = renewSession
    , logout = logout
    }



-- TODO: Implement the possibility to logout current user from all devices.


logout : SessionId -> ClientId -> BackendModel -> ( BackendModel, Cmd msg )
logout sessionId _ model =
    ( { model | sessions = Dict.remove sessionId model.sessions }, Lamdera.sendToFrontend sessionId BackendLoggedOut )


updateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err

        Auth.Common.AuthSessionChallenge _ ->
            ( model, Cmd.none )


renewSession : Lamdera.SessionId -> Lamdera.ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession _ _ model =
    Debug.todo "renewSession is not implemented yet"


handleAuthSuccess :
    BackendModel
    -> SessionId
    -> ClientId
    -> Auth.Common.UserInfo
    -> Auth.Common.MethodId
    -> Maybe Auth.Common.Token
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess backendModel sessionId clientId userInfo _ _ _ =
    let
        newSessions =
            Dict.insert sessionId ( False, userInfo ) backendModel.sessions

        response =
            AuthSuccess userInfo
    in
    ( { backendModel | sessions = newSessions }
    , Lamdera.sendToFrontend clientId response
    )
