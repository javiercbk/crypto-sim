module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Url.Parser as Parser
import Maybe




-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL

type Route
  = Home
  | Sign
  | Block
  | NotFound

routeParser : Parser.Parser (Route -> a) a
routeParser =
  Parser.oneOf
    [ Parser.map Home (Parser.s "")
    , Parser.map Sign (Parser.s "sign")
    , Parser.map Block (Parser.s "block") 
    ]

routeAndTitle: Url -> (Route, String)
routeAndTitle url =
    let
        route = Maybe.withDefault NotFound (Parser.parse routeParser url)
    in
    (route, titleFromRoute route)

titleFromRoute : Route -> String
titleFromRoute r = 
  case r of
    Home -> "Crypto-sym Home"
    Sign -> "Crypto-sym Sign"
    Block -> "Crypto-sym Block"
    NotFound -> "Crypto-sym ???"

type alias Model =
    { key : Nav.Key
    , history: List Route
    , route: Route
    , title: String
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
      (route, title) = routeAndTitle url
    in ( Model key [] route title, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
          let
              (route, title) = routeAndTitle url
          in
          
            ( { model | route = route
            , history = route :: model.history
            , title = title }
            , Cmd.none
            )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = model.title
    , body =
        [ Grid.container []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Grid.row []
                [ Grid.col []
                    [ text "Some content for my view here..." ]
                ]
            ]
        ]
    }


