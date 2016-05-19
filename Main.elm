import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

import Json.Decode as D

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type OSCArg
  = OSCFloat Float
  | OSCString String

decodeOSCArg : D.Decoder OSCArg
decodeOSCArg =
  D.oneOf
    [ D.map OSCFloat D.float
    , D.map OSCString D.string
    ]

encodeOSCArg : OSCArg -> String
encodeOSCArg arg =
  case arg of
    OSCFloat f ->
      toString f
    OSCString s ->
      s


type alias OSCMessage =
  { address : String
  , args: List OSCArg }

decodeOSCMessage : D.Decoder OSCMessage
decodeOSCMessage =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      address :: args ->
        Result.map2 OSCMessage
          (D.decodeValue D.string address)
          ((List.foldr (Result.map2 (::)) (Ok [])) (List.map (D.decodeValue decodeOSCArg) args))

      _ -> Result.Err "Expecting at least an address")

encodeOSCMessage : OSCMessage -> String
encodeOSCMessage { address, args } =
  address ++ "," ++ (List.foldr ((++) << encodeOSCArg) "" args)

type alias Model =
  { sentMessages : List OSCMessage
  , receivedMessages : List (Result String OSCMessage)
  }

websocketLocation = "ws://localhost:3030"

init : (Model, Cmd Msg)
init =
  (Model [] [], Cmd.none)

type Msg
  = Send OSCMessage
  | NewMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg { sentMessages, receivedMessages } =
  case msg of
    Send msg ->
      (Model (msg :: sentMessages) receivedMessages, WebSocket.send websocketLocation (encodeOSCMessage msg))
    NewMessage str ->
      (Model sentMessages ((D.decodeString decodeOSCMessage str) :: receivedMessages), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen websocketLocation NewMessage

sendDefaultMessage =
  Send (OSCMessage "/test" [ OSCString "one", OSCFloat 1 ])

view : Model -> Html Msg
view model =
  div []
    [ button [onClick sendDefaultMessage] [text "Send"]
    ]
