module Args exposing (..)

import Osc exposing (..)

type Args
  = FloatValueArgs Float
  | StringValueArgs String
  | SliderArgs String Float Float Float
  | ToggleArgs String Bool
  | ChoiceArgs String String (List String)

args : Parser (Args -> a) a
args =
  oneOf
    [ format FloatValueArgs (float)
    , format StringValueArgs (string)
    , format SliderArgs (f 32 </> s "f" </> string </> float </> float </> float)
    , format ToggleArgs (f 32 </> s "b" </> string </> (format asBool float))
    , format ChoiceArgs (f 32 </> s "c" </> string </> (format asBool float))
    ]

asBool : Float -> Bool
asBool f =
  if f == 1 then True else False

parse : List Args -> Args
parse 
