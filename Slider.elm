module Slider exposing (Model, Msg, init, update, view)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (..)
import Json.Decode as D
import String as S

import Osc exposing (..)

type alias Model =
 { name : String
 , address : List String
 , min : Float
 , max : Float
 , value : Float
 }

type Msg
  = ChangeValue Float

init : (Model, Cmd Msg)
init =
  (Model "" [] 0 0 0, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeValue newValue ->
      let
        oscMessage = OscMessage (encodeAddress model.address) [OscFloat newValue]
      in
        ({ model | value = newValue }, sendMessage oscMessage)

view : Model -> Html Msg
view model =
  div []
    [ label [] [ text model.name ]
    , input
        [ A.type' "range"
        , A.min (toString model.min)
        , A.max (toString model.max)
        , A.value (toString model.value)
        , A.step (toString ((model.max - model.min) / 100.0))
        , onInput (\e -> (ChangeValue (Result.withDefault model.value (S.toFloat e))))
        ]
        []
    , div [] [ text (toString model.value)]
    ]

-- HELPER FUNCTIONS --

encodeAddress : List String -> String
encodeAddress addr =
  "/" ++ (S.join "/" addr)
