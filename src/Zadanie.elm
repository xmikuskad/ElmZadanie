module Zadanie exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = (Stavy, List (List String))

type alias Stavy =
  { meno : String    -- Meno zoznamu
  , prvok : String   -- Hodnota prvku (pre insert) alebo meno druheho zoznamu pre append
  , zoznam : String  -- zoznam pre fromList
  , vystup : String  -- Vystup, generovany tlacidlom
  }
  

init : Model
init = (Stavy "" "" "" "", [["a","a","b","c"],["b","a","b","c"]])

-- UPDATE

type Msg
      -- Vstupy
  = Meno String
  | Prvok String
  | Zoznam String
      -- Tlacidla
  | Empty
  | ShowRep
  | Insert
  | IsIn
  | FromList


update : Msg -> Model -> Model
update msg (st, li) =
  case msg of
    Meno text ->
      ({ st | meno = text, vystup = "" }, li)

    Prvok text ->
      ({ st | prvok = text, vystup = "" }, li)

    Zoznam text ->
      ({ st | zoznam = text, vystup = "" }, li)

    Empty ->
      if st.meno == "" 
      then (Stavy "" "" "" "Treba vlozit meno noveho zoznamu", li)
      else (Stavy "" "" "" ("Vytvoreny novy zoznam: " ++ st.meno)
            , empty st.meno li)

    ShowRep ->
      (Stavy "" "" "" (if st.meno == "" 
                       then "Treba vlozit meno zoznamu" 
                       else if List.member st.meno (getFirsts li)
                       then "Hodnota zoznamu " ++ st.meno ++ " je " ++ showRep st.meno li
                       else "Neznamy zoznam "  ++ st.meno)
       , li)
      
    Insert ->
      if st.meno == "" 
      then (Stavy "" "" "" "Treba vlozit meno zoznamu", li)
      else if st.prvok == ""
      then (Stavy "" "" "" "Treba vlozit prvok", li)
      else if List.member st.meno (getFirsts li)
      then (Stavy "" "" "" ("Prvok "++st.prvok++" pridany do zoznamu " ++st.meno)
           , insert st.meno st.prvok li)
      else (Stavy "" "" "" ("Neznamy zoznam "  ++ st.meno), li)

    IsIn ->
      (Stavy "" "" "" (if st.meno == "" 
                       then "Treba vlozit meno zoznamu" 
                       else if st.prvok == ""
                       then "Treba vlozit prvok"
                       else if List.member st.meno (getFirsts li)
                       then isIn st.meno st.prvok li
                       else "Neznamy zoznam "  ++ st.meno )
      ,li)      

    FromList ->
      if st.meno == "" 
      then (Stavy "" "" "" "Treba vlozit meno zoznamu", li)
      else if st.zoznam == ""
      then (Stavy "" "" "" "Treba vlozit zoznam prvkov", li)
      else (Stavy "" "" "" ("Zoznam "++st.zoznam++" pridany do zoznamu " ++st.meno)
           , fromList st.meno (empty st.meno li) (String.split " " st.zoznam))

-- VIEW


view : Model -> Html Msg
view (st, li) =
  div [style "marginLeft" "20px"]
    [ h3 [][text "Zadanie Elm - Udajovy typ mnozina"]
    , h3 [][text "Dominik Mikuska"]
    , div [][text "Vstupy:"]
    , viewInput "text" "Meno zoznamu" st.meno Meno
    , br [][]
    , viewInput "text" "Prvok/druhy zoznam" st.prvok Prvok
    , br [][]
    , viewInput "text" "Zoznam prvkov" st.zoznam Zoznam
    , br [][]
    , br [][]
    , button [ onClick Empty ] [ text "Empty" ]
    , button [ onClick ShowRep ] [ text "ShowRep" ]
    , button [ onClick Insert ] [ text "Insert" ]   
    , button [ onClick IsIn ] [ text "IsIn" ] 
    , button [ onClick FromList ] [ text "FromList" ]        
    , br [][]
    , br [][]
    , div [][text "Definovane zoznamy: "]
    , viewNames li
--    , div [][text <| listToString identity (List.map (listToString identity) li)]
    , p [] [text st.vystup]
    ]


-- POMOCNE FUNKCIE

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewNames : List (List String) -> Html msg
viewNames lists =
  case lists of
    [] ->
      div [ style "color" "red" ] [ text "Ziadny zoznam nie je definovany." ]
    _ :: _ ->
      div [][text (listToString identity (getFirsts lists))]


getFirsts : List (List a) -> List a
getFirsts list =
  case list of
    [] -> []
    first1 :: rest1 ->
      case first1 of
        [] -> getFirsts rest1
        first :: _ -> first :: getFirsts rest1


listToString : (a -> String) -> List a -> String
listToString fun list =
  let
    lts f li =
      case li of
        [] -> ""
        first :: rest ->
          (f first)
          ++ if rest == [] then "" else ", "
          ++ lts f rest
  in
    "[" ++ (lts fun list) ++ "]"

isInHelper : String -> List (String) -> String
isInHelper prvok list =
  case list of
    [] -> "Prvok nie je v mnozine"
    first :: rest ->
      if first == prvok
      then "Prvok je v mnozine"
      else isInHelper prvok rest

-- FUNKCIE K UDAJOVEMU TYPU

empty : String -> List (List String) -> List (List String)
empty meno list =
  case list of
    [] -> [[meno]]
    first1 :: rest1 ->
      case first1 of
        [] -> empty meno rest1
        first :: _ ->
          if first == meno
          then [meno] :: rest1
          else first1 :: empty meno rest1


showRep : String -> List (List String) -> String
showRep meno lists =
 case lists of
   [] -> ""
   first1 :: rest1 ->
     case first1 of
       [] -> showRep meno rest1
       first :: rest ->
         if first == meno
         then listToString identity rest
         else showRep meno rest1

insert : String -> String -> List (List String) -> List (List String)
insert meno prvok list =
 case list of
   [] -> [[]]
   first1 :: rest1 ->
     case first1 of
     [] -> insert meno prvok rest1
     first :: rest ->
      if first == meno
      then ((first :: (rest ++ [prvok])) :: rest1)
      else first1 :: (insert meno prvok rest1) 
  
isIn : String -> String -> List (List String) -> String
isIn meno prvok list = 
 case list of
   [] -> ""
   first1 :: rest1 ->
     case first1 of
       [] -> isIn meno prvok rest1
       first :: rest ->
         if first == meno
         then isInHelper prvok rest
         else isIn meno prvok rest1

fromList : String -> List (List String) -> List String -> List (List String)
fromList meno list vkladame =
 case list of
   [] -> [[]]
   first1 :: rest1 ->
     case first1 of
     [] -> fromList meno rest1 vkladame
     first :: rest ->
      if first == meno
      then (first :: vkladame) :: rest1
      else first1 :: (fromList meno rest1 vkladame) 
