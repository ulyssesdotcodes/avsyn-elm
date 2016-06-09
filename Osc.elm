module Osc exposing (Message, Bundle, Arg(..), Packet(..), decodeBundle, encodeBundle, parse, sendPacket)
import Json.Decode as D
import String exposing (..)
import WebSocket

-- TODO: calculate this --
websocketLocation = "ws://localhost:3000"

type Arg
  = FloatArg Float
  | StringArg String


type Packet
  = PacketMessage Message
  | PacketBundle Bundle

parse : msg -> (Message -> Maybe msg) -> Message -> msg
parse noOp parser msg =
  Maybe.withDefault noOp <| parser msg

decodeArg : D.Decoder Arg
decodeArg =
  D.oneOf
    [ D.map FloatArg D.float
    , D.map StringArg D.string
    ]

encodeArg : Arg -> String
encodeArg arg =
  case arg of
    FloatArg f ->
      toString f
    StringArg s ->
      "\"" ++ s ++ "\""

type alias Message =
  { address : List String
  , args : List Arg }

decodeMessage : D.Decoder Message
decodeMessage =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      address :: args ->
        Result.map2 Message
          (Result.map (split "/") (D.decodeValue D.string address))
          ((List.foldr (Result.map2 (::)) (Ok [])) (List.map (D.decodeValue decodeArg) args))

      _ -> Result.Err "Expecting at least an address")

encodeMessage : Message -> String
encodeMessage { address, args } =
  "[\"" ++ (join "/" address) ++ "\"," ++ (join "," (List.map encodeArg args)) ++ "]"


type alias Bundle = List Message

decodeBundle : D.Decoder Bundle
decodeBundle =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      bundleText :: timing :: msgs ->
        (List.foldr (Result.map2 (::)) (Ok []) (List.map (D.decodeValue decodeMessage) msgs))

      _ ->
        Result.Err "expecing a bundle")


encodeBundle : Bundle -> String
encodeBundle bundle =
  "[\"#bundle\",{\"timestamp\":0}," ++ (join "," (List.map encodeMessage bundle)) ++ "]"

sendPacket : Packet -> Cmd msg
sendPacket p =
  case p of
    PacketMessage m ->
      sendPacket (PacketBundle [m])
    PacketBundle b ->
      WebSocket.send websocketLocation (encodeBundle b)
