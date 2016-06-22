module Osc exposing
  ( Message
  , Bundle
  , Arg(..)
  , Packet(..)
  , ArgsParser
  , s
  , f
  , string
  , float
  , stringList
  , oneOf
  , format
  , and
  , decodeBundle
  , encodeBundle
  , extractTopAddress
  , decodeString
  , decodeFloat
  , parse
  , sendPacket
  )

import Json.Decode as D
import List as L
import String as S
import WebSocket

-- TODO: calculate this --
websocketLocation = "ws://localhost:3000"

type Arg
  = FloatArg Float
  | StringArg String

decodeString : Arg -> Result String String
decodeString a =
  case a of
    StringArg str ->
      Ok str
    _ ->
      Err "Can't parse string"

decodeFloat : Arg -> Result String Float
decodeFloat a =
  case a of
    FloatArg flt ->
      Ok flt
    _ ->
      Err "Can't parse float"

isStringArg : Arg -> Bool
isStringArg a =
  case a of
    StringArg _ ->
      True
    _ ->
      False

type Packet
  = PacketMessage Message
  | PacketBundle Bundle

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

encodeArgs : List Arg -> String
encodeArgs = S.join "," << List.map encodeArg

type alias Bundle = List Message

type alias Message =
  { address : List String
  , args : List Arg
  }

-- Args Parser --

type ArgsParser formatter result =
  ArgsParser (ArgChunks -> formatter -> Result String (ArgChunks, result))

type alias ArgChunks =
  { seen : List Arg
  , rest : List Arg
  }

parse : formatter -> ArgsParser formatter a -> List Arg -> Result String a
parse f (ArgsParser actuallyParse) input =
  case actuallyParse (ArgChunks [] input) f of
    Err msg ->
      Err msg
    Ok ({rest}, result) ->
      case rest of
        [] ->
          Ok result
        _ ->
          Err <| "The parser worked but /" ++ encodeArgs rest ++ " was left over."

s : String -> ArgsParser a a
s str =
  ArgsParser <| \{seen, rest} result ->
    case rest of
      [] ->
        Err ("Got to the end of the input without seeing /" ++ str)
      arg :: remaining ->
        if arg == StringArg str then
          Ok ( ArgChunks (arg :: seen) remaining, result )
        else
          Err ("Wanted /" ++ str ++" but got " ++ encodeArgs rest)

string : ArgsParser (String -> a) a
string =
  custom "STRING" decodeString

stringList : ArgsParser (List String -> a) a
stringList =
  ArgsParser <| \{seen, rest} func ->
    let
      args = takeWhile isStringArg rest
      strings = (List.filterMap Result.toMaybe << List.map decodeString) args
    in
      Ok ( ArgChunks (args ++ seen) (L.drop (L.length args) rest), func strings )

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []

f : Float -> ArgsParser a a
f flt =
  ArgsParser <| \{seen, rest} result ->
    case rest of
      [] ->
        Err ("Got to the end of the input without seeing /" ++ toString flt)
      arg :: remaining ->
        if arg == FloatArg flt then
          Ok ( ArgChunks (arg :: seen) remaining, result )
        else
          Err ("Wanted /" ++ toString flt ++" but got " ++ encodeArgs rest)

float : ArgsParser (Float -> a) a
float =
  custom "FLOAT" decodeFloat

custom : String -> (Arg -> Result String a) -> ArgsParser (a -> output) output
custom tipe argToSomething =
  ArgsParser <| \{seen, rest} func ->
    case rest of
      [] ->
        Err ("Got to the end of input but wanted /" ++ tipe)
      chunk :: remaining ->
        case argToSomething chunk of
          Ok something ->
            Ok (ArgChunks (chunk::seen) remaining, func something)
          Err msg ->
            Err ("Parsing `" ++ (encodeArg chunk) ++ "` went wrong: " ++ msg)

and : ArgsParser a b -> ArgsParser b c -> ArgsParser a c
and (ArgsParser parseFirst) (ArgsParser parseSecond) =
   ArgsParser <| \chunks f ->
     parseFirst chunks f
       `Result.andThen` \(nextChunks, nextF) ->
         parseSecond nextChunks nextF

oneOf : List (ArgsParser a b) -> ArgsParser a b
oneOf choices =
  ArgsParser (oneOfHelp choices)

oneOfHelp : List (ArgsParser a b) -> ArgChunks -> a -> Result String (ArgChunks, b)
oneOfHelp choices chunks formatter =
  case choices of
    [] ->
      Err "Tried many parsers, but none of them worked!"

    ArgsParser parser :: otherParsers ->
      case parser chunks formatter of
        Err _ ->
          oneOfHelp otherParsers chunks formatter

        Ok answerPair ->
          Ok answerPair

format : formatter -> ArgsParser formatter a -> ArgsParser (a -> result) result
format input (ArgsParser parse) =
  ArgsParser <| \chunks func ->
    case parse chunks input of
      Err msg ->
        Err msg

      Ok (newChunks, value) ->
        Ok (newChunks, func value)

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
              (Result.andThen (D.decodeValue D.string address) (Result.fromMaybe "error parsing address" << List.tail << S.split "/") )
              (parsedArgs args)

          _ -> Result.Err "Expecting at least an address"
      )

encodeMessage : Message -> String
encodeMessage { address,  args } =
  "[\"/" ++ (S.join "/" address) ++ "\"," ++ (S.join "," <| List.map encodeArg args) ++ "]"

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
  "[\"#bundle\",{\"timestamp\":0}," ++ (S.join "," (List.map encodeMessage bundle)) ++ "]"

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
      case b of
        [] ->
          Cmd.none
        _ ->
          WebSocket.send websocketLocation (Debug.log "Sending " <| encodeBundle b)
