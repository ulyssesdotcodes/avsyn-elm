module Parser exposing (..)

import String as S

type Parser formatter result =
  Parser (Chunks -> formatter -> Result String (Chunks, result))

type alias Chunks =
  { seen: List String
  , rest : List String
  }

parse : formatter -> Parser formatter a -> List String -> Result String a
parse f (Parser actuallyParse) input =
  case actuallyParse (Chunks [] input) f of
    Err msg ->
      Err msg
    Ok ({rest}, result) ->
      case rest of
        [] ->
          Ok result
        [""] ->
          Ok result
        _ ->
          Err <| "The parser worked but /" ++ S.join "/" rest ++ " was left over."

s : String -> Parser a a
s str =
  Parser <| \{seen, rest} result ->
    case rest of
      [] ->
        Err ("Got to the end of the input without seeing /" ++ str)
      chunk :: remaining ->
        if chunk == str then
          Ok ( Chunks (chunk :: seen) remaining, result )
        else
          Err ("Wanted /" ++ str ++" but got " ++ S.join "/" rest)

string : Parser (String -> a) a
string =
  custom "STRING" Ok

int : Parser (Int -> a) a
int =
  custom "INT" S.toInt

custom : String -> (String -> Result String a) -> Parser (a -> output) output
custom tipe stringToSomething =
  Parser <| \{seen, rest} func ->
    case rest of
      [] ->
        Err ("Got to the end of input but wanted /" ++ tipe)
      chunk :: remaining ->
        case stringToSomething chunk of
          Ok something ->
            Ok (Chunks (chunk::seen) remaining, func something)
          Err msg ->
            Err ("Parsing `" ++ chunk ++ "` went wrong: " ++ msg)


infixr 8 </>

(</>) : Parser a b -> Parser b c -> Parser a c
(</>) (Parser parseFirst) (Parser parseSecond) =
   Parser <| \chunks f ->
     parseFirst chunks f
       `Result.andThen` \(nextChunks, nextF) ->
         parseSecond nextChunks nextF

oneOf : List (Parser a b) -> Parser a b
oneOf choices =
  Parser (oneOfHelp choices)


oneOfHelp : List (Parser a b) -> Chunks -> a -> Result String (Chunks, b)
oneOfHelp choices chunks formatter =
  case choices of
    [] ->
      Err "Tried many parsers, but none of them worked!"

    Parser parser :: otherParsers ->
      case parser chunks formatter of
        Err _ ->
          oneOfHelp otherParsers chunks formatter

        Ok answerPair ->
          Ok answerPair

format : formatter -> Parser formatter a -> Parser (a -> result) result
format input (Parser parse) =
  Parser <| \chunks func ->
    case parse chunks input of
      Err msg ->
        Err msg

      Ok (newChunks, value) ->
        Ok (newChunks, func value)
