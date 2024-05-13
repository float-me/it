module Korean.Parts.Cho exposing (Cho(..), fromChar, fromVal, isKey, toChar, toVal)

import Korean.Parts.Tool as Tool
import List.Extra


type Cho
    = Cho Char


choList : List Char
choList =
    [ 'r'
    , 'R'
    , 's'
    , 'e'
    , 'E'
    , 'f'
    , 'a'
    , 'q'
    , 'Q'
    , 't'
    , 'T'
    , 'd'
    , 'w'
    , 'W'
    , 'c'
    , 'z'
    , 'x'
    , 'v'
    , 'g'
    ]


choShapes : List Char
choShapes =
    [ 'ㄱ'
    , 'ㄲ'
    , 'ㄴ'
    , 'ㄷ'
    , 'ㄸ'
    , 'ㄹ'
    , 'ㅁ'
    , 'ㅂ'
    , 'ㅃ'
    , 'ㅅ'
    , 'ㅆ'
    , 'ㅇ'
    , 'ㅈ'
    , 'ㅉ'
    , 'ㅊ'
    , 'ㅋ'
    , 'ㅌ'
    , 'ㅍ'
    , 'ㅎ'
    ]


toChar : Cho -> Char
toChar cho =
    -- '?' 케이스는 발생하지 않음
    Maybe.withDefault '?' <| List.Extra.getAt (toVal cho) choShapes


toVal : Cho -> Int
toVal (Cho chr) =
    Tool.getVal chr choList


fromVal : Int -> Maybe Cho
fromVal val =
    Maybe.map Cho (List.Extra.getAt val choList)


fromChar : Char -> Maybe Cho
fromChar chr =
    Maybe.andThen fromVal <| List.Extra.elemIndex chr choShapes


isKey : Char -> Bool
isKey key =
    List.member key choList
