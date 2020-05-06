{-
Priklad na spracovanie rekurzii v elm - na chvoste je parada

recSum : number -> number -> number
recSum n v =
  if n<=0
    then v
  else
    recSum (n-1) (v+1)

TEST!
Vytvorte program pomocou
a) rekurzia
b) funkcional - foldl, map, filter a pod

1.
-- Zevraj funcional map - NASTUDOVAT!
convIfEven2 zoz =
  List.map(\ f -> (f, modBy 2 f == 0)) zoz

--Pomocou rekurzie
convIfEven : List Int -> List (Int, Bool)
convIfEven zoz =
  case zoz of
    [] -> []
    f::r -> (f, modBy 2 f == 0) :: convIfEven r
-- Pozor pridavanie pri :: ma byt na konci!

2.
--rekurzia
between : number -> number -> List number -> List number
between min_ max_ zoz =
  case zoz of
    [] -> []
    f :: r -> if f > min_ && f < max_
              then f :: between min_ max_ r
              else between min_ max_ r

--funkcional
between2 min_ max_ zoz =
  List.filter (\ f -> f > min_ && f < max_) zoz

3. strom
type BVS number = Empty | Value number (BVS number) (BVS number)

hlbka : BVS number -> Int
hlbka strom =
  case strom of
    Empty -> 0
    Value _ l r -> 1 + max (hlbka l) (hlbka r)

count : BVS number -> Int
count strom =
  case strom of
    Empty -> 0
    Value _ l r -> 1 + (hlbka l) + (hlbka r)

4.
main =
  text <| Debug.toString
       <| isIn 5 (Value 10
                        (Value 5 Empty Empty)
                        (Value 15 Empty
                                       (Value 20 Empty Empty)))
--prikladam aj main aby som vedel replikovat

type BVS number = Empty | Value number (BVS number) (BVS number)

isIn : number -> BVS number -> Bool
isIn num tree=
  case tree of
    Empty -> False
    Value n l r -> if num < n
                   then isIn num l
                   else if num > n
                   then isIn num r
                   else True

insert : number -> BVS number -> BVS number
insert num tree =
  case tree of
    Empty -> Value num Empty Empty
    Value n l r -> if num < n
                   then Value n (insert num l) r
                   else if num > n
                   then Value n l (insert num r)
                   else tree
            

-}
