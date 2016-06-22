module Choice exposing (Model, Msg, init, update, view)

import Html exposing (..)
import List as L

import Osc
import Types exposing (..)

type alias Model =
  { name : String
  , chosen : String
  , choices : List String
  }

type Msg
  = ChangeChoice String

type ChoiceArgs = ChoiceArgs String (List String)
type ValueArgs = ValueArgs String

init : ParsedMessage String -> Maybe Model
init msg =
  let
    args = Osc.parse ChoiceArgs (Osc.string `Osc.and` Osc.stringList) msg.args
  in
    Result.toMaybe
      <| args `Result.andThen` (\(ChoiceArgs name choices) ->
        (Result.map (\c -> Model name c choices) (Result.fromMaybe "No choices" <| L.head choices)))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> Html.Html msg
view model =
  div []
    <| List.map text model.choices


