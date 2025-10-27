module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V2.Auth.Common
import Lamdera
import Url


type LoginState
    = Loading
    | Anonymous
    | LoginRequest
    | LoginProposal Evergreen.V2.Auth.Common.UserInfo
    | LoggedIn Evergreen.V2.Auth.Common.UserInfo
    | SignedOut


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , linden : Int
    , lion : Int
    , login : LoginState
    , authFlow : Evergreen.V2.Auth.Common.Flow
    , authRedirectBaseUrl : Url.Url
    }


type alias BackendModel =
    { pendingAuths : Dict.Dict Lamdera.SessionId Evergreen.V2.Auth.Common.PendingAuth
    , sessions : Dict.Dict Lamdera.SessionId ( Bool, Evergreen.V2.Auth.Common.UserInfo )
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Linden Int
    | Lion Int
    | OrcidLoginRequested
    | ConfirmLoginAs Evergreen.V2.Auth.Common.UserInfo
    | OrcidPromptLoginRequested
    | OrcidSignoutRequested
    | BackendSignoutRequested


type ToBackend
    = NoOpToBackend
    | AuthToBackend Evergreen.V2.Auth.Common.ToBackend
    | ConfirmLoginOnBackend Evergreen.V2.Auth.Common.UserInfo
    | GetUser


type BackendMsg
    = NoOpBackendMsg
    | AuthBackendMsg Evergreen.V2.Auth.Common.BackendMsg


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Evergreen.V2.Auth.Common.ToFrontend
    | AuthSuccess Evergreen.V2.Auth.Common.UserInfo
    | UserInfoMsg (Maybe ( Bool, Evergreen.V2.Auth.Common.UserInfo ))
    | BackendLoggedOut
