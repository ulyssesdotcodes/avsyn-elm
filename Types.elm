module Types exposing (..)

import Paths
import Osc

type alias ParsedMessage a =
  { address : a
  , args : List Osc.Arg
  }

mapMessage : (a -> b) -> ParsedMessage a -> ParsedMessage b
mapMessage f a =
  ParsedMessage (f a.address) a.args

replaceAddress : b -> ParsedMessage a -> ParsedMessage b
replaceAddress b m =
  ParsedMessage b m.args

toOsc : ParsedMessage Paths.Path -> Osc.Message
toOsc msg =
  Osc.Message (Paths.pathAddress msg.address) msg.args
