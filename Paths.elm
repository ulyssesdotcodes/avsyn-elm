module Paths exposing (..)

import Parser exposing (..)

type Path
  = Cinder CinderPath

type CinderPath
  = MixControls String
  | Vis VisPath

type VisPath
  = VisPath String VisControlsPath

type VisControlsPath
  = Controls ControlPath
  | Effects ControlPath
  | Choices

type ControlPath
  = Control String
  | Value String

pathAddress : Path -> List String
pathAddress p =
  case p of
    Cinder c ->
      "cinder" :: cinderAddress c

cinderAddress : CinderPath -> List String
cinderAddress c =
  case c of
    MixControls m ->
      ["mix", m]
    Vis (VisPath name p) ->
      name :: visControlsAddress p

visControlsAddress : VisControlsPath -> List String
visControlsAddress v =
  case v of
    Controls p ->
      "controls" :: controlPathAddress p
    Effects p ->
      "effects" :: controlPathAddress p
    Choices ->
      ["choices"]

controlPathAddress : ControlPath -> List String
controlPathAddress c =
  case c of
    Value str ->
      ["value", str]
    Control str ->
      [str]

path : Parser (Path -> a) a
path =
  oneOf
    [ format Cinder (s "cinder" </> cinderPath)
    ]

cinderPath : Parser (CinderPath -> a) a
cinderPath =
  oneOf
    [ format MixControls (s "mix" </> s "controls" </> string)
    , format (\s p -> Vis <| VisPath s p) (string </> visControlsPath)
    ]

visControlsPath : Parser (VisControlsPath -> a) a
visControlsPath =
  oneOf
    [ format Effects (s "effects" </> controlPath)
    , format Controls (s "controls" </> controlPath)
    , format Choices (s "choices")
    ]

controlPath : Parser (ControlPath -> a) a
controlPath =
  oneOf
    [ format Control string
    , format Value (string </> s "value")
    ]

parse : List String -> Result String Path
parse addr =
  Parser.parse identity path addr
