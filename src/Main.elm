module Main exposing (..)

import Html exposing (text)


main =
  text <| Debug.toString
       <| preorder (Value 10
                        (Value 5 Empty Empty)
                        (Value 15 Empty
                                       (Value 20 Empty Empty)))

type BVS number = Empty | Value number (BVS number) (BVS number)

insert : number -> BVS number -> BVS number
insert num tree =
  case tree of
    Empty -> Value num Empty Empty
    Value n l r -> if num < n
                   then Value n (insert num l) r
                   else if num > n
                   then Value n l (insert num r)
                   else tree

bvsFromList : List number -> BVS number
bvsFromList zoz =
  case zoz of
    [] -> Empty
    f :: r -> insert f ( bvsFromList r )

bvsFromList2 zoz =
  List.foldl (insert) Empty zoz

preorder : BVS number -> List number
preorder tree =
  case tree of
    Empty -> []
    Value n l r -> (n :: []) ++ (preorder l) ++ (preorder r)
