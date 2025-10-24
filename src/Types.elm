module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), LoginState(..), ToBackend(..), ToFrontend(..))

import Auth.Common exposing (UserInfo)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (SessionId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , linden : Int
    , lion : Int
    , login : LoginState
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    }


type alias BackendModel =
    { message : String
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , sessions : Dict SessionId ( Bool, UserInfo )
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Linden Int
    | Lion Int
    | OrcidLoginRequested
    | ConfirmLoginAs UserInfo
    | OrcidPromptLoginRequested
    | OrcidSignoutRequested
    | BackendSignoutRequested


type ToBackend
    = NoOpToBackend
    | AuthToBackend Auth.Common.ToBackend
    | ConfirmLoginOnBackend UserInfo
    | GetUser


type BackendMsg
    = NoOpBackendMsg
    | AuthBackendMsg Auth.Common.BackendMsg


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess UserInfo
    | UserInfoMsg (Maybe ( Bool, UserInfo ))
    | BackendLoggedOut


type LoginState
    = Loading -- Asking backend
    | Anonymous -- Confirmed by backend
    | LoginRequest -- Login Intention registered
    | LoginProposal UserInfo -- Asking user to check they signed in with the correct user
    | LoggedIn UserInfo -- Successful authentication completed
    | SignedOut -- Optional fragile state after local signout, IdProvider still signed in



-- TODO: LoginProposal UserInfo (only when no password was entered on orcid.org)
