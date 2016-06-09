import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import String
import WebSocket

import Osc
import Slider

websocketLocation = "ws://64.255.16.184:3000"

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- APP --

type alias Model =
  { sentMessages : List Osc.Message
  , sliders : List Slider.Model
  }

init : (Model, Cmd Msg)
init =
  (Model [] [], Cmd.none)

type Msg
  = Send Osc.Message
  | NewMessage Osc.Bundle
  | SliderMsg String Slider.Msg
  | AddSlider Slider.Model
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send msg ->
      ({model | sentMessages = msg :: model.sentMessages}, Osc.sendPacket (Osc.PacketMessage msg))

    NewMessage msgs ->
      let
        parsed = List.concat <| List.map (\m -> List.map (\p -> Osc.parse NoOp p m) (parsers model)) msgs
        update' msg (m, c) =
          let
            (m', c') =
              update msg m
          in
            (m', Cmd.batch [c, c'])
        (model', cmds) = List.foldl update' (model, Cmd.none) (Debug.log "parsed: " parsed)
      in
        (model', cmds)

    SliderMsg name sliderMsg ->
      let
        (sliderModels, sliderCmds) =
          List.unzip (List.map (updateHelp name sliderMsg) model.sliders)
      in
        ({ model | sliders = sliderModels }, Cmd.batch sliderCmds)

    AddSlider slider ->
      ({ model | sliders = model.sliders ++ [slider]}, Cmd.none)

    NoOp ->
      (model, Cmd.none)

parsers : Model -> List (Osc.Message -> Maybe Msg)
parsers model =
  (::) parseMessage <| (flip List.map) model.sliders <| \s -> Maybe.map (SliderMsg s.name) << Slider.parseMessage s

updateHelp : String -> Slider.Msg -> Slider.Model -> (Slider.Model, Cmd Msg)
updateHelp name msg slider =
  if slider.name /= name then
     ( slider, Cmd.none )
  else
    let
      (newSlider, cmds) =
        Slider.update msg slider
    in
      ( newSlider, Cmd.map (SliderMsg name) cmds )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map (Maybe.withDefault NoOp << Maybe.map NewMessage << Result.toMaybe) <| WebSocket.listen websocketLocation (D.decodeString Osc.decodeBundle)

parseMessage : Osc.Message -> Maybe Msg
parseMessage =
  Maybe.map AddSlider << Slider.init

defaultOscMessage : Msg
defaultOscMessage =
  Send (Osc.Message ["test"] [ Osc.StringArg "one", Osc.FloatArg 1 ])

view : Model -> Html Msg
view model =
  div []
    [ button [onClick defaultOscMessage] [text "Send"]
    , div []
        (List.map (\s -> Html.map (SliderMsg s.name) (Slider.view s))model.sliders)
    ]
