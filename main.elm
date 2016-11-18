module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Frame =
    { function: String
    }

type alias Model =
    { frames : List Frame
    , errors : List Http.Error
    }


init : ( Model, Cmd Msg )
init =
    ( (Model [] [])
    , getFrames
    )



-- UPDATE


type Msg
    = MorePlease
    | Frames (Result Http.Error (List Frame))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getFrames )

        Frames (Ok frames) ->
            ( { model | frames = frames }, Cmd.none )

        Frames (Err error) ->
            ( { model | errors = (List.append model.errors [error]) }, Cmd.none )



-- VIEW

trTd : String -> Html Msg
trTd s =
    tr [] [ td [] [ text s ] ]

printFrame : Frame -> Html Msg
printFrame frame =
    trTd frame.function

printError : Http.Error -> Html Msg
printError error =
    case error of
        Http.BadUrl s ->
            trTd ("bad url: " ++ s)

        Http.Timeout ->
            trTd "timeout"

        Http.NetworkError ->
            trTd "network error"

        Http.BadStatus (_) ->
            trTd "bad status"

        Http.BadPayload _ (_) ->
            trTd "bad payload"

view : Model -> Html Msg
view model =
    div []
        [ table [] (List.map printFrame model.frames)
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , table [] (List.map printError model.errors)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getFrames : Cmd Msg
getFrames =
    let
        url =
            "http://localhost:54321/frames"
    in
        Http.send Frames (Http.get url decodeFramesJson)

frameDecoder : Decode.Decoder Frame
frameDecoder =
  DecodePipeline.decode Frame
    |> DecodePipeline.required "function" Decode.string

decodeFramesJson : Decode.Decoder (List Frame)
decodeFramesJson =
    Decode.list frameDecoder
