import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)
import WebSocket

import Json.Decode as D

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- OSC Models --

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
      "\"" ++ s ++ "\""


type alias OSCMessage =
  { address : String
  , args : List OSCArg }

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
  "[\"" ++ address ++ "\"," ++ (join "," (List.map encodeOSCArg args)) ++ "]"


type alias OSCBundle = List OSCMessage

decodeOSCBundle : D.Decoder OSCBundle
decodeOSCBundle =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      bundleText :: timing :: msgs ->
        (List.foldr (Result.map2 (::)) (Ok []) (List.map (D.decodeValue decodeOSCMessage) msgs))

      _ ->
        Result.Err "expecing a bundle")


encodeOSCBundle : OSCBundle -> String
encodeOSCBundle bundle =
  "[\"#bundle\"," ++ (join "," (List.map encodeOSCMessage bundle)) ++ "]"


-- APP --

type alias Model =
  { sentMessages : List OSCMessage
  , receivedMessages : List (Result String OSCBundle)
  }

websocketLocation = "ws://localhost:3000"

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
      Debug.log (toString ((D.decodeString decodeOSCBundle str) :: receivedMessages))
      (Model sentMessages ((D.decodeString decodeOSCBundle str) :: receivedMessages), Cmd.none)

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
