module Slider exposing (Model, Msg, init, update, view, parseMessage)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (..)
import Json.Decode as D
import List exposing (..)
import Maybe exposing (..)
import String as S

import Osc

type alias Model =
 { name : String
 , address : List String
 , min : Float
 , max : Float
 , value : Float
 }

type Msg
  = ChangeValue Float

parseMessage : Model -> Osc.Message -> Maybe Msg
parseMessage model msg =
  case (List.reverse msg.address, msg.args) of
    (v::name::_, (Osc.FloatArg value)::_ )->
      if v == "value" && name == model.name then
        Just <| ChangeValue value
      else
        (\_ -> Nothing) (Debug.log "name: " name)

    _ -> (\_ -> Nothing) (Debug.log "msg: " msg)

init : Osc.Message -> Maybe Model
init msg =
  case msg.args of
    (Osc.FloatArg cf)
      ::(Osc.StringArg tf)
      ::(Osc.StringArg name)
      ::(Osc.FloatArg value)
      ::(Osc.FloatArg min)
      ::(Osc.FloatArg max)::_ ->
      if cf == 32 && tf == "f" then
        Just <| Model name msg.address min max value
      else
        Nothing
    _ -> Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeValue newValue ->
      let
        oscMessage = Osc.Message model.address [Osc.FloatArg newValue]
      in
        ({ model | value = newValue }, Osc.sendPacket (Osc.PacketMessage {oscMessage | address = oscMessage.address ++ ["value"]}))

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ label [] [ text model.name ]
        , text ": "
        , text (toString model.value)
        ]
    , input
        [ A.type' "range"
        , A.min (toString model.min)
        , A.max (toString model.max)
        , A.value (toString model.value)
        , A.step (toString ((model.max - model.min) / 100.0))
        , onInput (\e -> (ChangeValue (Result.withDefault model.value (S.toFloat e))))
        ]
        []
    ]

-- HELPER FUNCTIONS --

encodeAddress : List String -> String
encodeAddress addr =
  "/" ++ (S.join "/" addr)
