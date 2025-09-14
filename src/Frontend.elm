module Frontend exposing (Model, app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
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
    ( { key = key
      , linden = 0
      , lion = 0
      }
    , Cmd.batch [ Random.generate Linden (Random.int 0 31), Random.generate Lion (Random.int 0 31) ]
    )


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

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Linden i ->
            ( { model | linden = i }, Cmd.none )

        Lion i ->
            ( { model | lion = i }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ H.div
            [ A.style "margin" "20px auto"
            , A.style "width" "240px"
            , A.style "display" "block"
            ]
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
        , H.p [ A.style "clear" "both", A.style "text-align" "center", A.style "padding" "40px" ] [ H.text "Hello, world" ]
        ]
    }
