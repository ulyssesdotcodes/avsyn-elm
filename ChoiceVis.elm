module ChoiceVis exposing (Model, Msg, parseMessage, init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes

import Osc
import Slider

type alias Model =
  { choice : String
  , choices : List String
  , controls : List ( Int, Slider.Model )
  , address : List String
  , name : String
  , nextId : Int
  }

type Msg
  = AddSlider Slider.Model
  | UpdateSlider Int Slider.Msg

init : List String -> String -> String -> List String -> Model
init address name choice choices =
  { choice = choice
  , choices = choice :: choices
  , controls = []
  , address = address
  , name = name
  , nextId = 0
  }

parseMessage : Model -> Osc.Message -> Maybe Msg
parseMessage model msg =
  if List.head msg.address == Just model.name then
    Maybe.map AddSlider <| Slider.init <| Osc.Message (model.address ++ msg.address) msg.args
  else
    Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddSlider slider ->
      ( { model | controls = ( model.nextId, slider )::model.controls , nextId = model.nextId + 1 }, Cmd.none )
    UpdateSlider id msg ->
      let
        (controls', cmds) =
          List.unzip <| List.map (updateSlider id msg) model.controls
      in
        { model | controls = controls' } ! cmds

updateSlider : Int -> Slider.Msg -> (Int, Slider.Model) -> ((Int, Slider.Model), Cmd Msg)
updateSlider id msg model =
  let
    mapUpdate id update =
      (fst update, Cmd.map (UpdateSlider id) <| snd update )
    (model', cmds) =
      if id == fst model then mapUpdate (fst model) <| Slider.update msg (snd model) else (snd model, Cmd.none)
  in
    (( fst model,  model' ), cmds )

view : Model -> Html.Html Msg
view model =
  div []
    (List.map (\idS -> Html.map (UpdateSlider <| fst idS) (Slider.view <| snd idS)) model.controls)
