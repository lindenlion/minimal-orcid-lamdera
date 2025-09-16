module Auth.Common exposing (AuthChallengeReason(..), AuthCode, BackendMsg(..), ClientId, Config, ConfigurationEmailMagicLink, ConfigurationOAuth, Error(..), Flow(..), FrontendMsg(..), LogoutEndpointConfig(..), Method(..), MethodId, PendingAuth, Provider(..), SessionId, State, ToBackend(..), ToFrontend(..), Token, UserInfo, defaultHttpsUrl, nothingIfEmpty)

import Browser.Navigation exposing (Key)
import OAuth
import OAuth.AuthorizationCode as OAuth
import Task exposing (Task)
import Time
import Url exposing (Protocol(..), Url)


type alias Config frontendMsg toBackend backendMsg toFrontend frontendModel backendModel =
    { toBackend : ToBackend -> toBackend
    , toFrontend : ToFrontend -> toFrontend
    , backendMsg : BackendMsg -> backendMsg
    , sendToFrontend : SessionId -> toFrontend -> Cmd backendMsg
    , sendToBackend : toBackend -> Cmd frontendMsg
    , methods : List (Method frontendMsg backendMsg frontendModel backendModel)
    , renewSession : SessionId -> ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    }


type Method frontendMsg backendMsg frontendModel backendModel
    = ProtocolOAuth (ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel)
    | ProtocolEmailMagicLink (ConfigurationEmailMagicLink frontendMsg backendMsg frontendModel backendModel)


type alias ConfigurationEmailMagicLink frontendMsg backendMsg frontendModel backendModel =
    { id : String
    , initiateSignin :
        SessionId
        -> ClientId
        -> backendModel
        -> { username : Maybe String }
        -> Time.Posix
        -> ( backendModel, Cmd backendMsg )
    , onFrontendCallbackInit :
        frontendModel
        -> MethodId
        -> Url
        -> Key
        -> (ToBackend -> Cmd frontendMsg)
        -> ( frontendModel, Cmd frontendMsg )
    , onAuthCallbackReceived :
        SessionId
        -> ClientId
        -> Url
        -> AuthCode
        -> State
        -> Time.Posix
        -> (BackendMsg -> backendMsg)
        -> backendModel
        -> ( backendModel, Cmd backendMsg )
    , placeholder : frontendMsg -> backendMsg -> frontendModel -> backendModel -> ()
    }


type alias ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel =
    { id : String
    , authorizationEndpoint : Url
    , tokenEndpoint : Url
    , logoutEndpoint : LogoutEndpointConfig
    , allowLoginQueryParameters : Bool
    , clientId : String
    , clientSecret : String
    , scope : List String
    , getUserInfo : OAuth.AuthenticationSuccess -> Task Error UserInfo
    , onFrontendCallbackInit :
        frontendModel
        -> MethodId
        -> Url
        -> Key
        -> (ToBackend -> Cmd frontendMsg)
        -> ( frontendModel, Cmd frontendMsg )
    , placeholder : ( backendModel, backendMsg ) -> ()
    }


type FrontendMsg
    = AuthSigninRequested Provider


type ToBackend
    = AuthCallbackReceived MethodId Url AuthCode State


type BackendMsg
    = AuthCallbackReceived_ SessionId ClientId MethodId Url String String Time.Posix
    | AuthSuccess SessionId ClientId MethodId Time.Posix (Result Error ( UserInfo, Maybe Token ))


type ToFrontend
    = AuthError Error


type AuthChallengeReason
    = AuthSessionMissing


type alias Token =
    { methodId : MethodId
    , token : OAuth.Token
    , created : Time.Posix
    , expires : Time.Posix
    }


type LogoutEndpointConfig
    = Home { returnPath : String }
    | Tenant { url : Url, returnPath : String }


type Provider
    = OAuthGoogle


type Flow
    = Idle
    | Pending
    | Authorized AuthCode String
    | Errored Error


type Error
    = ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
      -- Lazy string error until we classify everything nicely
    | ErrAuthString String


type alias State =
    String


type alias MethodId =
    String


type alias AuthCode =
    String


type alias UserInfo =
    { email : String
    , name : Maybe String
    , username : Maybe String
    }


type alias PendingAuth =
    { created : Time.Posix
    , sessionId : SessionId
    , state : String
    }



--
-- Helpers
--


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


nothingIfEmpty s =
    let
        trimmed =
            String.trim s
    in
    if trimmed == "" then
        Nothing

    else
        Just trimmed



-- Lamdera aliases


type alias SessionId =
    String


type alias ClientId =
    String
