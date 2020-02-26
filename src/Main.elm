module Main exposing (..)

import Html exposing (text)


main =
  text <| Debug.toString
       <| map ((+) 5) [1,2,3,4,5]


map : (a->b) -> List a -> List b
map fun zoz=
  case zoz of
    [] -> []
    first :: rest -> fun first :: map fun rest
