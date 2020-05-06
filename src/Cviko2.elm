-- CVICENIE 2
-- Length
{-
main = text <| Debug.toString
            <| length [1,2,3]

length : List a -> Int
length a =
  case a of
    [] -> 0
    first :: rest -> 1 + length rest
    -- Mozeme vymenit poradie pre zlepsenie rychlosti

-- sum bonus code
sum : List Int -> Int
sum a =
  case a of
    [] -> 0
    first :: rest -> first + sum rest
-}

-- Member
{-
main = text <| Debug.toString
            <| member 6 [1,2,3,4]

member : a -> List a -> Bool
member prvok zoznam =
    case zoznam of
      [] -> False
      first :: rest -> if first == prvok then True else member prvok
-}

--spocita vsetky prvky zoznamu, ktore su vacsie ako dany prvok
{-
main = text <| Debug.toString <| countBigger 2 [1,2,3,4,0]

countBigger : comparable -> List comparable -> Int
countBigger prvok zoznam =
  case zoznam of
    [] -> 0
    first :: rest -> if first > prvok then 1 + countBigger prvok rest
                                      else countBigger prvok rest
-}

{-
main =
  text <| Debug.toString
       <| smaller 3 [1,2,3,4,7]

smaller : comparable -> List comparable -> List comparable
smaller prvok zoznam =
  case zoznam of
    [] -> []
    first :: rest -> if first < prvok then first :: (smaller prvok rest)
                                      else smaller prvok rest
-}


{-
main =
  text <| Debug.toString
       <| afterElem 2 [1,2,3,4,0]

afterElem : a -> List a -> List a
afterElem prvok zoznam =
  case zoznam of
    [] -> []
    first :: rest -> if first == prvok
                     then rest
                     else afterElem prvok rest
-}

{-
main =
  text <| Debug.toString
       <| zip [1,2,3,4] [5,6,7,8]

zip : List a -> List b -> List (a,b)
zip zoz1 zoz2 =
  case zoz1 of
    [] -> []
    first1 :: rest1 ->
      case zoz2 of
        [] -> []
        first2 :: rest2 -> (first1, first2) :: zip rest1 rest2
-}

{-
main =
  text <| Debug.toString
       <| map ((+) 5) [1,2,3,4,5]


map : (a->b) -> List a -> List b
map fun zoz=
  case zoz of
    [] -> []
    first :: rest -> fun first :: map fun rest
-}

{-
main =
  text <| Debug.toString
       <| fromTuple String.fromInt identity (1,"aaa")


fromTuple : (a->String) -> (b->String) -> (a,b) -> String
fromTuple fun1 fun2 (a,b) =
  "(" ++ fun1 a ++ "," ++ fun2 b ++ ")"
-}

{-
main =
  text <| Debug.toString
       <| foldl (::) [] [1,2,3]

foldl : (a->b->b) ->b -> List a -> b
foldl fun acc zoz =
  case zoz of
    [] -> acc
    f :: r -> foldl fun (fun f acc) r

-}

-- DOROBIT FOLDR!

{-

foldr : (a->b->b) ->b -> List a -> b
foldr fun acc zoz =
  case zoz of
    [] -> acc
    f :: r -> fun f (foldr fun acc r)

-}
