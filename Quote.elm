port module Quote exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Task


main: Program Never
main =
    App.program
        { init = init "famous"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model =
    { quote: String
    , author: String
    , category: String
    }

init: String -> (Model, Cmd Msg)
init category =
    ( Model "Loading..." "" category
    , getRandomQuote category
    )


-- UPDATE

type Msg
    = GetQuote
    | FetchSucceed Model
    | FetchFail Http.Error

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetQuote ->
            (model, getRandomQuote model.category)

        FetchSucceed newQuote->
            (newQuote, Cmd.none)

        FetchFail _ ->
            (model, Cmd.none)


-- VIEW

view: Model -> Html Msg
view model =
    div []
      [ p [ class "quote" ] [ text model.quote ]
      , p [ class "author" ] [ text ( "-" ++ model.author ) ]
      , button [ onClick GetQuote ] [ text "New Quote" ]
      ]


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none


-- HTTP

randomQuoteRequest: String -> Request
randomQuoteRequest url =
    { verb = "POST"
    , headers =
        [ ("X-Mashape-Key", "erXFDGNgTomshfFpfs0CiXuj3jiPp1RmPM3jsnBXHIveVntZ4Y")
        , ("Content-Type", "application/x-www-form-urlencoded")
        , ("Accept", "application/json")
        ]
    , url = url
    , body = empty
    }

getRandomQuote: String -> Cmd Msg
getRandomQuote category =
    let
        url =
            "https://andruxnet-random-famous-quotes.p.mashape.com/?cat=" ++ category
        req =
            randomQuoteRequest url
    in
        Task.perform FetchFail FetchSucceed
            ( fromJson decodeNewQuote (send defaultSettings req) )

decodeNewQuote: Json.Decoder Model
decodeNewQuote =
    Json.object3 Model
        ("quote" := Json.string)
        ("author" := Json.string)
        ("category" := Json.string)
