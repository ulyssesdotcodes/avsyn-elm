module ChoiceVis exposing (Model, Msg, parseMessage, init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes

import Choice
import Paths
import Osc
import Slider
import Types exposing (..)

type alias Model =
  { choiceControl : Maybe Choice.Model
  , controls : List ( Int, Slider.Model )
  , name : String
  , nextId : Int
  }

type Msg
  = AddSlider Slider.Model
  | UpdateSlider Int Slider.Msg
  | AddChoice Choice.Model

init : String -> Model
init name =
  { choiceControl = Nothing
  , controls = []
  , name = name
  , nextId = 0
  }

parseMessage : Model -> ParsedMessage Paths.VisPath -> Maybe Msg
parseMessage model {address, args} =
  let
    controlsAddress = (\(Paths.VisPath name controlsAddress) -> controlsAddress) address
  in
    case controlsAddress of
      Paths.Effects (Paths.Control name) ->
        Maybe.oneOf
          [ Maybe.map AddSlider <| Slider.init <| ParsedMessage name args
          , Maybe.map AddChoice <| Choice.init <| ParsedMessage name args
          ]
      _ ->
        Nothing

update : Msg -> Model -> (Model, Cmd Msg, List (ParsedMessage Paths.VisPath))
update msg model =
  case msg of
    AddSlider slider ->
      ( { model | controls = ( model.nextId, slider )::model.controls , nextId = model.nextId + 1 }
      , Cmd.none
      , []
      )

    UpdateSlider id msg ->
      let
        (controls', cmds, msgs) =
          unzip3 <| List.map (updateSlider id msg) model.controls
      in
        ({ model | controls = controls' }
        , Cmd.batch cmds
        , List.map (mapMessage <| Paths.VisPath model.name << Paths.Effects) <| List.concat msgs
        )

    AddChoice choice ->
      ({ model | choiceControl = Just choice }, Cmd.none, [])

unzip3 : List (a, b, c) -> (List a, List b, List c)
unzip3 l =
  case l of
    (a, b, c)::rest ->
      (\(la, lb, lc) -> (a::la, b::lb, c::lc)) << unzip3 <| rest
    [] ->
      ([], [], [])

updateSlider : Int -> Slider.Msg -> (Int, Slider.Model) ->
               ((Int, Slider.Model), Cmd Msg, List (ParsedMessage Paths.ControlPath))
updateSlider id msg model =
  if (fst model) == id then
    let
      idUpdate (m, c, p) = ((id, m), Cmd.map (UpdateSlider id) c, p)
    in
      idUpdate <| Slider.update msg (snd model)
  else
    (model, Cmd.none, [])

view : Model -> Html.Html Msg
view model =
  div []
    <| (Maybe.withDefault ( text "" ) <| Maybe.map Choice.view model.choiceControl)
    ::
    (List.map (\idS -> Html.map (UpdateSlider <| fst idS) (Slider.view <| snd idS)) model.controls)
