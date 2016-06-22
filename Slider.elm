module Slider exposing (Model, Msg, init, update, view, parseMessage)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (..)
import Json.Decode as D
import List exposing (..)
import Maybe exposing (..)
import String as S

import Osc
import Paths exposing (..)
import Types exposing (..)

type alias Model =
 { name : String
 , min : Float
 , max : Float
 , value : Float
 }

type Msg
  = OnInput Float
  | ChangeValue Float

type SliderArgs = SliderArgs String Float Float Float
type ValueArgs = ValueArgs Float

init : ParsedMessage String -> Maybe Model
init msg =
  let
    args = Osc.parse SliderArgs (Osc.f 32 `Osc.and` Osc.s "f" `Osc.and` Osc.string `Osc.and` Osc.float `Osc.and` Osc.float `Osc.and` Osc.float) msg.args
  in
    Result.toMaybe <| Result.map (\(SliderArgs name default min max) -> Model name min max default) args

parseMessage : Model -> ParsedMessage String -> Maybe Msg
parseMessage model msg =
  case (msg.address == model.name, Osc.parse ValueArgs (Osc.float) msg.args) of
    (True, Ok (ValueArgs val)) ->
      Just <| ChangeValue val
    _ ->
      Nothing

update : Msg -> Model -> (Model, Cmd Msg, List (ParsedMessage ControlPath))
update msg model =
  case msg of
    OnInput newValue ->
      let
        msgs =
          if model.value /= newValue then
            [ParsedMessage (Paths.Value model.name) [Osc.FloatArg newValue]]
          else
            []
      in
        ({ model | value = newValue }, Cmd.none, msgs)

    ChangeValue newValue ->
      ({ model | value = newValue }, Cmd.none, [])

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
        , A.defaultValue (toString model.value)
        , A.step (toString ((model.max - model.min) / 100.0))
        , onInput (\e -> (OnInput (Result.withDefault model.value (S.toFloat e))))
        ]
        []
    ]

-- HELPER FUNCTIONS --

encodeAddress : List String -> String
encodeAddress addr =
  "/" ++ (S.join "/" addr)
