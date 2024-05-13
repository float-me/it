module Korean.Parts.Jong exposing (..)

import Korean.Parts.Tool as Tool exposing (StackObject(..))
import List.Extra


type Jong
    = Jong StackObject


jongList : List Jong
jongList =
    [ Jong <| Stackable 'r'
    , Jong <| Unstackable 'R'
    , Jong <| Stacked 'r' 't'
    , Jong <| Stackable 's'
    , Jong <| Stacked 's' 'w'
    , Jong <| Stacked 's' 'g'
    , Jong <| Unstackable 'e'
    , Jong <| Stackable 'f'
    , Jong <| Stacked 'f' 'r'
    , Jong <| Stacked 'f' 'a'
    , Jong <| Stacked 'f' 'q'
    , Jong <| Stacked 'f' 't'
    , Jong <| Stacked 'f' 'x'
    , Jong <| Stacked 'f' 'v'
    , Jong <| Stacked 'f' 'g'
    , Jong <| Unstackable 'a'
    , Jong <| Stackable 'q'
    , Jong <| Stacked 'q' 't'
    , Jong <| Unstackable 't'
    , Jong <| Unstackable 'T'
    , Jong <| Unstackable 'd'
    , Jong <| Unstackable 'w'
    , Jong <| Unstackable 'c'
    , Jong <| Unstackable 'z'
    , Jong <| Unstackable 'x'
    , Jong <| Unstackable 'v'
    , Jong <| Unstackable 'g'
    ]


unstackableJongList : List Char
unstackableJongList =
    [ 'R'
    , 'e'
    , 'a'
    , 't'
    , 'T'
    , 'd'
    , 'w'
    , 'c'
    , 'z'
    , 'x'
    , 'v'
    , 'g'
    ]


stackableJongList : List Char
stackableJongList =
    [ 'r'
    , 's'
    , 'f'
    , 'q'
    ]


stackedJongList : List ( Char, Char )
stackedJongList =
    [ ( 'r', 't' )
    , ( 's', 'w' )
    , ( 's', 'g' )
    , ( 'f', 'r' )
    , ( 'f', 'a' )
    , ( 'f', 'q' )
    , ( 'f', 't' )
    , ( 'f', 'x' )
    , ( 'f', 'v' )
    , ( 'f', 'g' )
    , ( 'q', 't' )
    ]


fromVal : Int -> Maybe Jong
fromVal val =
    List.Extra.getAt (val - 1) jongList


toVal : Jong -> Int
toVal jong =
    1 + Tool.getVal jong jongList


isUnstackable : Char -> Bool
isUnstackable key =
    List.member key unstackableJongList


isStackable : Char -> Bool
isStackable key =
    List.member key stackableJongList


isStacked : ( Char, Char ) -> Bool
isStacked key =
    List.member key stackedJongList
