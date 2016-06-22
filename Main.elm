import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import List.Extra as LE
import String
import WebSocket

import ChoiceVis
import Paths
import Osc
import Slider
import Types exposing (..)

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
  , choiceB : ChoiceVis.Model
  }

init : (Model, Cmd Msg)
init =
  (Model [] (ChoiceVis.init "visA") (ChoiceVis.init "visB" ), Cmd.none)

type Msg
  = Send Osc.Message
  | NewMessage Osc.Bundle
  | ChoiceAMsg ChoiceVis.Msg
  | ChoiceBMsg ChoiceVis.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send msg ->
      ({model | sentMessages = msg :: model.sentMessages}, Osc.sendPacket (Osc.PacketMessage msg))

    NewMessage msgs ->
      let
        parsed =
          List.filterMap
            (\m -> (Result.toMaybe << Result.map (\a -> ParsedMessage a m.args))
               (Paths.parse m.address)) msgs
        childmsgs = List.concat <| List.map (parser model) parsed
        update' msg (m, c) =
          let
            (m', c') =
              update msg m
          in
            (m', Cmd.batch [c, c'])
        (model', cmds) = List.foldl update' (model, Cmd.none) childmsgs
      in
        (model', cmds)

    ChoiceAMsg msg ->
      let
        (choiceA', cmds, msgs) =
          ChoiceVis.update msg model.choiceA
      in
        ({ model | choiceA = choiceA' }
        , Cmd.batch [ Cmd.map ChoiceAMsg cmds
                    , Osc.sendPacket
                      <| Osc.PacketBundle
                        <| List.map (toOsc << mapMessage (Paths.Cinder << Paths.Vis)) msgs
                    ]
        )

    ChoiceBMsg msg ->
      let
        (choiceB', cmds, msgs) =
          ChoiceVis.update msg model.choiceB
      in
        ({ model | choiceB = choiceB' }
        , Cmd.batch [ Cmd.map ChoiceBMsg cmds
                    , Osc.sendPacket
                      <| Osc.PacketBundle
                        <| List.map (toOsc << mapMessage (Paths.Cinder << Paths.Vis)) msgs
                    ]
        )

    NoOp ->
      (model, Cmd.none)

parser : Model -> ParsedMessage Paths.Path -> List Msg
parser model {address, args} =
  let
    msgAddr = (\(Paths.Cinder path) -> path) address
  in
    case msgAddr of
      Paths.Vis vp ->
        List.filterMap ((|>) <| ParsedMessage vp args)
          [ Maybe.map ChoiceAMsg << ChoiceVis.parseMessage model.choiceA
          , Maybe.map ChoiceBMsg << ChoiceVis.parseMessage model.choiceB
          ]
      _ ->
        []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map (Debug.log "Message received" << Maybe.withDefault NoOp << Maybe.map NewMessage << Result.toMaybe) <| WebSocket.listen websocketLocation (D.decodeString Osc.decodeBundle << Debug.log "In")

defaultOscMessage : Msg
defaultOscMessage =
  Send (Osc.Message ["test"] [ Osc.StringArg "one", Osc.FloatArg 1 ])

view : Model -> Html Msg
view model =
  div []
    [ button [onClick defaultOscMessage] [text "Send"]
    , div []
       [ Html.map ChoiceAMsg <| ChoiceVis.view model.choiceA
       , Html.map ChoiceBMsg <| ChoiceVis.view model.choiceB
       ]
    ]
