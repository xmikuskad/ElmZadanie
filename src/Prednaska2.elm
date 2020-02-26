-- PREDNASKA 2
-- foldl pozriet dokumentaciu
{-main = text
            <| fromList String.fromFloat
            <| map (\a -> a*2.1) [4,5,6]

fromList : (a->String) -> List a -> String
fromList fun zoz =
  let
    fromL f z =
      case z of
        []->""
        first::rest ->
          f first ++
          if rest == [] then "" else ","
          ++ fromL f rest
  in
    "[" ++ fromL fun zoz ++ "]"



map : (a->b) -> List a -> List b
map fun zoz =
  case zoz of
    [] -> []
    first :: rest -> fun first :: map fun rest
-}
