module Backend exposing (Model, app)

import Auth
import Auth.Common exposing (UserInfo)
import Auth.Flow
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , pendingAuths = Dict.empty
      , sessions = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        AuthBackendMsg authMsg ->
            let
                _ =
                    Debug.log "AUTH BACKEND msg" authMsg
            in
            Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        AuthToBackend authMsg ->
            Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authMsg model

        ConfirmLoginOnBackend user ->
            case Dict.get sessionId model.sessions of
                Just ( False, userKnownToBackend ) ->
                    if userKnownToBackend == user then
                        -- The same user confirmed the intention to login as the one pending on the backend
                        ( { model | sessions = Dict.insert sessionId ( True, user ) model.sessions }
                        , (UserInfoMsg >> Lamdera.sendToFrontend sessionId) <| Just ( True, user )
                        )

                    else
                        -- A stale or forged message arrives
                        ( model, Lamdera.sendToFrontend sessionId <| UserInfoMsg (findUser sessionId model) )

                _ ->
                    ( model, Cmd.none )

        -- send current user info to all open tabs with same session cookie
        GetUser ->
            ( model, Lamdera.sendToFrontend sessionId <| UserInfoMsg (findUser sessionId model) )


findUser : SessionId -> Model -> Maybe ( Bool, Auth.Common.UserInfo )
findUser sessionId model =
    Dict.get sessionId model.sessions
