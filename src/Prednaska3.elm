module Main exposing (..)

import Html exposing (text)


main =
  text <| Debug.toString
       <| count (Node (Leaf 5) (Node (Leaf 4) Empty))

type Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

depth tree =
  case tree of
    Empty -> 0
    Leaf _ -> 1
    Node l r -> 1 + (max (depth l) (depth r))

count tree =
  case tree of
    Empty -> 0
    Leaf _ -> 1
    Node l r -> (count l) + count r

sum tree =
  case tree of
    Empty -> 0
    Leaf n -> n
    Node l r -> (sum l) + sum  r
