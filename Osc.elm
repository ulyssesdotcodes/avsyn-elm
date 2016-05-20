module Osc exposing (OscMessage, OscBundle, OscArg(..), decodeOscBundle, sendMessage)
import Json.Decode as D
import String exposing (..)
import WebSocket

-- TODO: calculate this --
websocketLocation = "ws://64.255.16.184:3000"

type OscArg
  = OscFloat Float
  | OscString String

decodeOscArg : D.Decoder OscArg
decodeOscArg =
  D.oneOf
    [ D.map OscFloat D.float
    , D.map OscString D.string
    ]

encodeOscArg : OscArg -> String
encodeOscArg arg =
  case arg of
    OscFloat f ->
      toString f
    OscString s ->
      "\"" ++ s ++ "\""


type alias OscMessage =
  { address : String
  , args : List OscArg }

decodeOscMessage : D.Decoder OscMessage
decodeOscMessage =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      address :: args ->
        Result.map2 OscMessage
          (D.decodeValue D.string address)
          ((List.foldr (Result.map2 (::)) (Ok [])) (List.map (D.decodeValue decodeOscArg) args))

      _ -> Result.Err "Expecting at least an address")

encodeOscMessage : OscMessage -> String
encodeOscMessage { address, args } =
  "[\"" ++ address ++ "\"," ++ (join "," (List.map encodeOscArg args)) ++ "]"


type alias OscBundle = List OscMessage

decodeOscBundle : D.Decoder OscBundle
decodeOscBundle =
  D.customDecoder (D.list D.value) (\jsonList ->
    case jsonList of
      bundleText :: timing :: msgs ->
        (List.foldr (Result.map2 (::)) (Ok []) (List.map (D.decodeValue decodeOscMessage) msgs))

      _ ->
        Result.Err "expecing a bundle")


encodeOscBundle : OscBundle -> String
encodeOscBundle bundle =
  "[\"#bundle\"," ++ (join "," (List.map encodeOscMessage bundle)) ++ "]"

sendMessage : OscMessage -> Cmd msg
sendMessage msg =
  Debug.log (encodeOscMessage msg)
  WebSocket.send websocketLocation (encodeOscMessage msg)
