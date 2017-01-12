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
    { function : String
      ,locals: List Local
    }

type alias Local = String


type alias Model =
    { frames : List Frame
    , log : List String
    }


init : ( Model, Cmd Msg )
init =
    ( (Model [] [])
    , getFrames
    )



-- UPDATE


type Msg
    = Reload
    | Command DebuggerCommand
    | CommandFinished (Result Http.Error CommandResult)
    | Frames (Result Http.Error (List Frame))


type DebuggerCommand
    = Continue
    | Step
    | Return
    | Next


type CommandResult
    = Success
    | InterpretationFinished
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reload ->
            ( model, getFrames )

        Command cmd ->
            ( model, (debuggerCommand cmd) )

        CommandFinished (Ok Success) ->
            ( model, getFrames )

        CommandFinished (Ok InterpretationFinished) ->
            ( { model | log = (List.append model.log [ "interpretation finished" ]), frames = [] }, Cmd.none )

        CommandFinished (Ok (Error e)) ->
            ( { model | log = (List.append model.log [ e ]) }, Cmd.none )

        Frames (Ok frames) ->
            ( { model | frames = frames }, Cmd.none )

        CommandFinished (Err error) ->
            ( { model | log = (List.append model.log [ "network error: " ++ (toString error) ]) }, Cmd.none )

        Frames (Err error) ->
            ( { model | log = (List.append model.log [ "network error: " ++ (toString error) ]) }, Cmd.none )



-- VIEW


trTd : String -> Html Msg
trTd s =
    tr [] [ td [] [ text s ] ]


printFrame : Frame -> Html Msg
printFrame frame =
    trTd frame.function

printLocals: Local -> Html Msg
printLocals local = trTd local


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Reload ] [ text "Reload Stackframes" ]
        , button [ onClick (Command Step) ] [ text "Step" ]
        , button [ onClick (Command Next) ] [ text "Next" ]
        , button [ onClick (Command Continue) ] [ text "Continue" ]
        , button [ onClick (Command Return) ] [ text "Return" ]
        , h1 [] [ text "Stackframes" ]
        , table [] (List.map printFrame model.frames)
        , h1 [] [ text "Locals" ]
        , table [] (model.frames
            |> List.head
            |> Maybe.map .locals
            |> Maybe.map (List.map printLocals)
            |> Maybe.withDefault ([trTd "No Locals"])
            )
        , h1 [] [ text "Logs" ]
        , table [] (List.map trTd model.log)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


debuggerCommand : DebuggerCommand -> Cmd Msg
debuggerCommand cmd =
    Http.send CommandFinished (Http.get (base_url ++ "cmd/" ++ (String.toLower (toString cmd))) cmdResultDecoder)


base_url : String
base_url =
    "http://localhost:54321/"


getFrames : Cmd Msg
getFrames =
    Http.send Frames (Http.get (base_url ++ "frames") decodeFramesJson)


frameDecoder : Decode.Decoder Frame
frameDecoder =
    DecodePipeline.decode Frame
        |> DecodePipeline.required "function" Decode.string
        |> DecodePipeline.hardcoded [] 


decodeSuccess : Decode.Decoder CommandResult
decodeSuccess =
    Decode.bool
        |> Decode.andThen
            (\v ->
                if v then
                    Decode.succeed Success
                else
                    Decode.succeed InterpretationFinished
            )


cmdResultDecoder : Decode.Decoder CommandResult
cmdResultDecoder =
    Decode.oneOf
        [ decodeSuccess
        , Decode.string |> Decode.andThen (\v -> Decode.succeed (Error v))
        ]


decodeFramesJson : Decode.Decoder (List Frame)
decodeFramesJson =
    Decode.list frameDecoder
