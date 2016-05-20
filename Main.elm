import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import WebSocket

import Osc exposing (..)
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
  { sentMessages : List OscMessage
  , receivedMessages : List (Result String OscBundle)
  , slider : Slider.Model
  }

init : (Model, Cmd Msg)
init =
  (Model [] [] (Slider.Model "Rad" ["rad"] 0 1 0.5), Cmd.none)

type Msg
  = Send OscMessage
  | NewMessage String
  | SliderMsg Slider.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send msg ->
      ({model | sentMessages = msg :: model.sentMessages}, sendMessage msg)

    NewMessage str ->
      Debug.log (toString ((D.decodeString decodeOscBundle str) :: model.receivedMessages))
      ({ model | receivedMessages = ((D.decodeString decodeOscBundle str) :: model.receivedMessages) }, Cmd.none)

    SliderMsg sliderMsg ->
      let
        (sliderModel, sliderCmds) =
          Slider.update sliderMsg model.slider
      in
        ({ model | slider = sliderModel }, Cmd.map SliderMsg sliderCmds)


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen websocketLocation NewMessage

sendDefaultMessage =
  Send (OscMessage "/test" [ OscString "one", OscFloat 1 ])

view : Model -> Html Msg
view model =
  div []
    [ button [onClick sendDefaultMessage] [text "Send"]
    , Html.map SliderMsg (Slider.view model.slider)
    ]
