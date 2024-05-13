module Korean.Parts.Tool exposing (..)

import List.Extra


type StackObject
    = Unstackable Char
    | Stackable Char
    | Stacked Char Char


getVal : a -> List a -> Int
getVal chr list =
    case List.Extra.elemIndex chr list of
        Just index ->
            index

        Nothing ->
            -1
