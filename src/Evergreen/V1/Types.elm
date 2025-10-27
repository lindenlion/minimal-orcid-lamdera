module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V1.Auth.Common
import Lamdera
import Url


type LoginState
    = Loading
    | Anonymous
    | LoginRequest
    | LoginProposal Evergreen.V1.Auth.Common.UserInfo
    | LoggedIn Evergreen.V1.Auth.Common.UserInfo
    | SignedOut


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , linden : Int
    , lion : Int
    , login : LoginState
    , authFlow : Evergreen.V1.Auth.Common.Flow
    , authRedirectBaseUrl : Url.Url
    }


type alias BackendModel =
    { message : String
    , pendingAuths : Dict.Dict Lamdera.SessionId Evergreen.V1.Auth.Common.PendingAuth
    , sessions : Dict.Dict Lamdera.SessionId ( Bool, Evergreen.V1.Auth.Common.UserInfo )
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Linden Int
    | Lion Int
    | OrcidLoginRequested
    | ConfirmLoginAs Evergreen.V1.Auth.Common.UserInfo
    | OrcidPromptLoginRequested
    | OrcidSignoutRequested
    | BackendSignoutRequested


type ToBackend
    = NoOpToBackend
    | AuthToBackend Evergreen.V1.Auth.Common.ToBackend
    | ConfirmLoginOnBackend Evergreen.V1.Auth.Common.UserInfo
    | GetUser


type BackendMsg
    = NoOpBackendMsg
    | AuthBackendMsg Evergreen.V1.Auth.Common.BackendMsg


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Evergreen.V1.Auth.Common.ToFrontend
    | AuthSuccess Evergreen.V1.Auth.Common.UserInfo
    | UserInfoMsg (Maybe ( Bool, Evergreen.V1.Auth.Common.UserInfo ))
    | BackendLoggedOut
