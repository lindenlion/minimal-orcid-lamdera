module Frontend exposing (Model, app)

import Auth
import Auth.Common exposing (Flow(..))
import Auth.Flow
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Env
import Html as H
import Html.Attributes as A
import Html.Events as E
import Lamdera
import Random
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        model =
            { key = key
            , linden = 0
            , lion = 0
            , login = Loading
            , authFlow = Idle
            , authRedirectBaseUrl = { url | query = Nothing, fragment = Nothing }
            }
    in
    route model url key
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Random.generate Linden (Random.int 0 31), Random.generate Lion (Random.int 0 31) ])


route : Model -> Lamdera.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
route model url key =
    let
        { path } =
            url
    in
    case path of
        "/login/OAuthOrcid/callback" ->
            Auth.Flow.init
                model
                "OAuthOrcid"
                url
                key
                (\msg -> Lamdera.sendToBackend (AuthToBackend msg))

        _ ->
            ( model, Lamdera.sendToBackend GetUser )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        Linden i ->
            ( { model | linden = i }, Cmd.none )

        Lion i ->
            ( { model | lion = i }, Cmd.none )

        OrcidSigninRequested ->
            Auth.Flow.signInRequested "OAuthOrcid" model Nothing
                |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)

        OrcidSignoutRequested1 ->
            ( { model | login = NotLogged }, Lamdera.sendToBackend OrcidSignoutRequested2 )

        BackendSignoutRequested ->
            ( { model | login = SignedOut }
            , Lamdera.sendToBackend Signout
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        AuthToFrontend authMsg ->
            Auth.updateFromBackend authMsg model

        AuthSuccess userInfo ->
            ( { model | login = LoggedIn userInfo }, Nav.pushUrl model.key "/" )

        UserInfoMsg maybeUserinfo ->
            case maybeUserinfo of
                Just userInfo ->
                    ( { model | login = LoggedIn userInfo }, Cmd.none )

                Nothing ->
                    ( { model | login = NotLogged }, Cmd.none )

        BackendLoggedOut ->
            ( { model | login = SignedOut }, Cmd.none )

        OrcidSignoutRequested3 ->
            ( { model | login = NotLogged }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = viewTitle model.login
    , body = [ viewBody model ]
    }


viewTitle : LoginState -> String
viewTitle login =
    case login of
        Loading ->
            "Loading"

        NotLogged ->
            "Hello, world"

        LoginTokenSent ->
            "Token sent"

        LoggedIn { name } ->
            "Hello, " ++ Maybe.withDefault "NoName" name

        SignedOut ->
            "Bye bye"


viewBody : Model -> H.Html FrontendMsg
viewBody model =
    H.div
        [ A.style "margin" "0 auto"
        , A.style "padding" "20px calc(50vw - 120px) 0 calc(50vw - 120px)"
        , A.style "width" "240px"
        , A.style "display" "block"
        , A.style "background-color" "#999"
        , A.style "height" "100vh"
        ]
    <|
        [ H.div
            [ A.id "linden"
            , A.title "linden"
            , A.style "background-image" "url('https://lindenlion.net/logo/lindenlion.jpg')"
            , A.style "height" "120px"
            , A.style "width" "120px"
            , A.style "background-position" <| "-15px " ++ String.fromInt (model.linden * 150 - 15) ++ "px"
            , A.style "float" "inline-start"
            , A.style "color" "transparent"
            ]
            [ H.text "linden" ]
        , H.div
            [ A.id "lion"
            , A.title "lion"
            , A.style "background-image" "url('https://lindenlion.net/logo/lindenlion.jpg')"
            , A.style "height" "100px"
            , A.style "width" "100px"
            , A.style "background-position" <| "125px " ++ String.fromInt (model.linden * 150 - 25) ++ "px"
            , A.style "float" "inline-end"
            , A.style "position" "relative"
            , A.style "top" "20px"
            , A.style "color" "transparent"
            ]
            [ H.text "lion" ]
        ]
            ++ viewUser model


viewUser : Model -> List (H.Html FrontendMsg)
viewUser model =
    let
        break =
            H.p [ A.style "clear" "both", A.style "text-align" "center", A.style "padding" "20px" ]
    in
    case model.login of
        Loading ->
            [ break [ H.text "Loading..." ] ]

        NotLogged ->
            [ break [ H.text "Hello, world" ]
            , H.div []
                [ H.label []
                    [ H.button [ E.onClick OrcidSigninRequested ] [ H.text "Login with Orcid" ] ]
                ]
            ]

        LoginTokenSent ->
            [ break [ H.text "Login request sent... waiting for response from server." ] ]

        LoggedIn user ->
            [ break [ H.text "Hello, ", H.a [ A.href <| "#" ++ user.unique ] [ H.text <| Maybe.withDefault "NoName" user.name ] ]
            , H.p [ A.style "text-align" "center", A.style "padding" "20px" ]
                [ H.button [ E.onClick BackendSignoutRequested ]
                    [ H.text "Logout" ]
                ]
            ]

        SignedOut ->
            [ break [ H.text "Bye bye! Your session on this site has ended, but you may still be signed in on orcid.org!" ]
            , H.div []
                [ H.label []
                    [ H.button [ E.onClick OrcidSigninRequested ] [ H.text "Login with Orcid" ]
                    , H.button [ E.onClick OrcidSignoutRequested1 ] [ H.a [ A.href "https://sandbox.orcid.org/signout", A.target "_blank" ] [ H.text "Sign out on Orcid" ] ]
                    ]
                ]
            ]
