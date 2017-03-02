module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
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
    , locals : List Local
    , return : ReturnLvalue
    }


type alias Local =
    { name : Maybe String
    , type_ : String
    , data : String
    }


type alias ReturnLvalue =
    { type_ : String
    , lvalue : String
    }


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
    | Locals (Result Http.Error (List Local))
    | UpdateReturn (Result Http.Error ReturnLvalue)


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
            ( { model | frames = frames }, requestLocals ((List.length frames) - 1) )

        Locals (Ok locals) ->
            ( { model
                | frames =
                    (model.frames
                        |> List.head
                        |> Maybe.map (\frame -> { frame | locals = locals })
                        |> Maybe.map2 (\tail head -> head :: tail) (List.tail model.frames)
                        |> Maybe.withDefault []
                    )
              }
            , Cmd.none
            )

        UpdateReturn (Ok retval) ->
            ( { model
                | frames =
                    (model.frames
                        |> List.head
                        |> Maybe.map (\frame -> { frame | return = retval })
                        |> Maybe.map2 (\tail head -> head :: tail) (List.tail model.frames)
                        |> Maybe.withDefault []
                    )
              }
            , Cmd.none
            )

        CommandFinished (Err error) ->
            ( { model | log = (List.append model.log [ "network error when fetching command: " ++ (toString error) ]) }, Cmd.none )

        Frames (Err (Http.BadPayload text _)) ->
            ( { model | log = (List.append model.log [ "error fetching stack frame: " ++ text ]) }, Cmd.none )

        Frames (Err error) ->
            ( { model | log = (List.append model.log [ "network error when fetching frames: " ++ (toString error) ]) }, Cmd.none )

        Locals (Err (Http.BadPayload text _)) ->
            ( { model | log = (List.append model.log [ "error fetching frame locals: " ++ text ]) }, Cmd.none )

        Locals (Err error) ->
            ( { model | log = (List.append model.log [ "network error when fetching locals: " ++ (toString error) ]) }, Cmd.none )

        UpdateReturn (Err error) ->
            ( { model | log = (List.append model.log [ "network error when fetching return lvalue: " ++ (toString error) ]) }, Cmd.none )


request : Decode.Decoder a -> String -> (Result Http.Error a -> msg) -> Cmd msg
request decoder url item =
    decoder
        |> Http.get (base_url ++ url)
        |> Http.send item


(+/) : String -> String -> String
(+/) a b =
    a ++ "/" ++ b


requestLocals : number -> Cmd Msg
requestLocals frame =
    request decodeLocals ("frame" +/ (toString frame) +/ "locals") Locals


requestRet : number -> Cmd Msg
requestRet frame =
    request returnDecoder ("frame" +/ (toString frame) +/ "return") UpdateReturn



-- VIEW


trTd : String -> Html Msg
trTd s =
    tr [] [ td [] [ text s ] ]


printFrame : Frame -> Html Msg
printFrame frame =
    trTd frame.function


printRet : ReturnLvalue -> Html Msg
printRet ret =
    text (ret.type_ ++ " @ " ++ ret.lvalue)


printLocal : Local -> Html Msg
printLocal local =
    let
        name =
            local.name
                |> Maybe.withDefault ""
                |> text

        type_ =
            local.type_
                |> text

        data =
            local.data
                |> text
    in
        [ name, type_, data ]
            |> List.map (\x -> td [ style [ ( "border-top", "1px solid black" ) ] ] [ x ])
            |> tr []


lastFrame : List Frame -> (Frame -> a1) -> (a1 -> b) -> b -> b
lastFrame frames what fn els =
    frames
        |> List.head
        |> Maybe.map what
        |> Maybe.map fn
        |> Maybe.withDefault els


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
        , h1 [] [ text "Return Value" ]
        , lastFrame model.frames .return printRet (text "No Return value")
        , h1 [] [ text "Locals" ]
        , lastFrame model.frames
            .locals
            (\list -> (tr [] [ td [] [ text "Name" ], td [] [ text "Type" ], td [] [ text "Value" ] ]) :: List.map printLocal list)
            [ trTd "No Locals" ]
            |> table
                [ style [ ( "border", "1px solid black" ) ] ]
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
        |> DecodePipeline.hardcoded { type_ = "unavailable", lvalue = "unavailable" }


localDecoder : Decode.Decoder Local
localDecoder =
    DecodePipeline.decode Local
        |> DecodePipeline.required "name" (Decode.nullable Decode.string)
        |> DecodePipeline.required "type_" Decode.string
        |> DecodePipeline.required "data" Decode.string


returnDecoder : Decode.Decoder ReturnLvalue
returnDecoder =
    DecodePipeline.decode ReturnLvalue
        |> DecodePipeline.required "type_" Decode.string
        |> DecodePipeline.required "lvalue" Decode.string


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


decodeLocals : Decode.Decoder (List Local)
decodeLocals =
    Decode.list localDecoder


decodeFramesJson : Decode.Decoder (List Frame)
decodeFramesJson =
    Decode.list frameDecoder
