-- CVIKO 1
-- main = text "Hello World"

{-}
main = text <| fromTuple(20,"Test")

fromTuple: (Int,String) -> String

fromTuple (a,b) = (String.fromInt a)++" "++b


--fromBool : Bool -> String
-- fromBool boolean = if boolean == True then "true" else "false"

Case varianta
fromBool b =
  case b of
    True -> "True"
    False -> "False"
-}
