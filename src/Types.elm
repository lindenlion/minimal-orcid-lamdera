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
    , sessions : Dict SessionId UserInfo
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Linden Int
    | Lion Int


type ToBackend
    = AuthToBackend Auth.Common.ToBackend


type BackendMsg
    = AuthBackendMsg Auth.Common.BackendMsg


type ToFrontend
    = AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess UserInfo


type LoginState
    = NotLogged
    | LoggedIn UserInfo
