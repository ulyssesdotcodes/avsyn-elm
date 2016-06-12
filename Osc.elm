module Osc exposing
  ( Message
  , MessageArgs (..)
  , FloatControlArgs
  , Bundle
  , Arg(..)
  , Packet(..)
  , decodeBundle
  , encodeBundle
  , extractTopAddress
  , decodeString
  , decodeFloat
  , parse
  , sendPacket
  )

import Json.Decode as D
import String exposing (..)
import WebSocket

-- TODO: calculate this --
websocketLocation = "ws://localhost:3000"

type Arg
  = FloatArg Float
  | StringArg String

decodeString : Arg -> Maybe String
decodeString a =
  case a of
    StringArg s ->
      Just s
    _ -> Nothing

decodeFloat : Arg -> Maybe Float
decodeFloat a =
  case a of
    FloatArg f ->
      Just f
    _ -> Nothing


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
  , args : MessageArgs
  }

type MessageArgs
  = FloatRange FloatControlArgs
  | Value Arg
  | Raw (List Arg)

type alias FloatControlArgs =
  { name : String
  , min : Float
  , max : Float
  , default : Float
  }

-- Control Messages --

decodeMessage : D.Decoder Message
decodeMessage =
  let
    parsedArgs args = ((List.foldr (Result.map2 (::)) (Ok [])) (List.map (D.decodeValue decodeArg) args))
  in
    D.customDecoder (D.list D.value)
      (\jsonList ->
        case jsonList of
          address :: args ->
            Result.map2 Message
              (Result.andThen (D.decodeValue D.string address) (Result.fromMaybe "error parsing address" << List.tail << split "/") )
              (Result.map Raw <| parsedArgs args)

          _ -> Result.Err "Expecting at least an address"
      )

decodeControlMessage : Message -> Maybe Message
decodeControlMessage { address, args } =
  (List.head << List.reverse) address `Maybe.andThen` (\name ->
      case args of
        Raw rawArgs ->
          case rawArgs of
            (FloatArg value)
              ::(FloatArg min)
              ::(FloatArg max)::_ ->
              Just <| Message address <| FloatRange <| FloatControlArgs name min max value
            _ ->
              Nothing
        _ ->
          Nothing)

encodeMessage : Message -> String
encodeMessage { address,  args } =
  let
    parsedArgs =
      case args of
        Value v ->
          [ v ]
        Raw rawArgs ->
          rawArgs
        _ ->
          []
  in
    "[\"/" ++ (join "/" address) ++ "\"," ++ (join "," <| List.map encodeArg parsedArgs) ++ "]"

type alias Bundle = List Message

decodeBundle : D.Decoder Bundle
decodeBundle =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      bundleText :: timing :: msgs ->
        (List.foldr (Result.map2 (::)) (Ok []) (List.map (D.decodeValue decodeMessage) msgs))

      _ ->
        Result.Err "expecing a bundle")


encodeBundle : List Message -> String
encodeBundle bundle =
  "[\"#bundle\",{\"timestamp\":0}," ++ (join "," (List.map encodeMessage bundle)) ++ "]"

extractTopAddress : Message -> Maybe (String, Message)
extractTopAddress { address, args } =
  case address of
      head::rest ->
        Just ( head, Message rest args )
      _ ->
        Nothing

sendPacket : Packet -> Cmd msg
sendPacket p =
  case p of
    PacketMessage m ->
      sendPacket (PacketBundle [m])
    PacketBundle b ->
      WebSocket.send websocketLocation (encodeBundle b)
