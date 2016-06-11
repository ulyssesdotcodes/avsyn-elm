import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import String
import WebSocket

import ChoiceVis
import Osc
import Slider

websocketLocation = "ws://localhost:3000"

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
  , choiceA : ChoiceVis.Model
  }

init : (Model, Cmd Msg)
init =
  (Model [] (ChoiceVis.init ["cinder"] "visA" "A" ["B"]), Cmd.none)

type Msg
  = Send Osc.Message
  | NewMessage Osc.Bundle
  | ChoiceMsg ChoiceVis.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send msg ->
      ({model | sentMessages = msg :: model.sentMessages}, Osc.sendPacket (Osc.PacketMessage msg))

    NewMessage msgs ->
      let
        msgs' = (snd << List.unzip) <| List.filterMap Osc.extractTopAddress  msgs
        parsed = List.concat <| List.map (\m -> List.map (\p -> Osc.parse NoOp p (Debug.log "Message" m)) (parsers model)) msgs'
        update' msg (m, c) =
          let
            (m', c') =
              update msg m
          in
            (m', Cmd.batch [c, c'])
        (model', cmds) = List.foldl update' (model, Cmd.none) parsed
      in
        (model', cmds)

    ChoiceMsg msg ->
      let
        (choiceA', cmds) =
          ChoiceVis.update msg model.choiceA
      in
        ({ model | choiceA = choiceA' }, Cmd.map ChoiceMsg cmds )

    NoOp ->
      (model, Cmd.none)

parsers : Model -> List (Osc.Message -> Maybe Msg)
parsers model =
  [ Maybe.map ChoiceMsg << ChoiceVis.parseMessage model.choiceA ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map (Debug.log "Message received" << Maybe.withDefault NoOp << Maybe.map NewMessage << Result.toMaybe) <| WebSocket.listen websocketLocation (D.decodeString Osc.decodeBundle)

defaultOscMessage : Msg
defaultOscMessage =
  Send (Osc.Message ["test"] [ Osc.StringArg "one", Osc.FloatArg 1 ])

view : Model -> Html Msg
view model =
  div []
    [ button [onClick defaultOscMessage] [text "Send"]
    , div []
       [ Html.map ChoiceMsg <| ChoiceVis.view model.choiceA
       ]
    ]
