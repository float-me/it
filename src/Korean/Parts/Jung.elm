module Korean.Parts.Jung exposing (..)

import Korean.Parts.Tool as Tool exposing (StackObject(..))
import List.Extra


type Jung
    = Jung StackObject


jungList : List Jung
jungList =
    [ Jung <| Unstackable 'k'
    , Jung <| Unstackable 'o'
    , Jung <| Unstackable 'i'
    , Jung <| Unstackable 'O'
    , Jung <| Unstackable 'j'
    , Jung <| Unstackable 'p'
    , Jung <| Unstackable 'u'
    , Jung <| Unstackable 'P'
    , Jung <| Stackable 'h'
    , Jung <| Stacked 'h' 'k'
    , Jung <| Stacked 'h' 'o'
    , Jung <| Stacked 'h' 'l'
    , Jung <| Unstackable 'y'
    , Jung <| Stackable 'n'
    , Jung <| Stacked 'n' 'j'
    , Jung <| Stacked 'n' 'p'
    , Jung <| Stacked 'n' 'l'
    , Jung <| Unstackable 'b'
    , Jung <| Stackable 'm'
    , Jung <| Stacked 'm' 'l'
    , Jung <| Unstackable 'l'
    ]


unstackableJungList : List Char
unstackableJungList =
    [ 'k'
    , 'o'
    , 'i'
    , 'O'
    , 'j'
    , 'p'
    , 'u'
    , 'P'
    , 'y'
    , 'b'
    , 'l'
    ]


stackableJungList : List Char
stackableJungList =
    [ 'h', 'n', 'm' ]


stackedJungList : List ( Char, Char )
stackedJungList =
    [ ( 'h', 'k' )
    , ( 'h', 'o' )
    , ( 'h', 'l' )
    , ( 'n', 'j' )
    , ( 'n', 'p' )
    , ( 'n', 'l' )
    , ( 'm', 'l' )
    ]


fromVal : Int -> Maybe Jung
fromVal val =
    List.Extra.getAt val jungList


toVal : Jung -> Int
toVal jung =
    Tool.getVal jung jungList


isUnstackable : Char -> Bool
isUnstackable key =
    List.member key unstackableJungList


isStackable : Char -> Bool
isStackable key =
    List.member key stackableJungList


isStacked : ( Char, Char ) -> Bool
isStacked key =
    List.member key stackedJungList
