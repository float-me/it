module Korean.Word exposing (KoWord, apply, delChar, first, firstKoChar, fromKoChar, fromString, last, lastKoChar, toString)

import Korean.Char as KoChar exposing (KoChar)
import List.Extra


type alias WordForm =
    { prevChars : List KoChar
    , curChar : KoChar
    , str : String
    }


type alias KoWord =
    Maybe WordForm


fromKoChar : KoChar -> KoWord
fromKoChar koChr =
    Just
        { prevChars = []
        , curChar = koChr
        , str = String.fromChar (KoChar.toChar koChr)
        }


apply : Char -> KoWord -> KoWord
apply key word =
    case word of
        Just form ->
            case KoChar.apply key form.curChar of
                ( Just completeChr, newChr ) ->
                    Just
                        { form
                            | prevChars = completeChr :: form.prevChars
                            , curChar = newChr
                            , str =
                                String.dropRight 1 form.str
                                    ++ String.fromChar (KoChar.toChar completeChr)
                                    ++ String.fromChar (KoChar.toChar newChr)
                        }

                ( Nothing, newChr ) ->
                    Just
                        { form
                            | curChar = newChr
                            , str =
                                String.dropRight 1 form.str
                                    ++ String.fromChar (KoChar.toChar newChr)
                        }

        Nothing ->
            case KoChar.fromSoleKey key of
                Just chr ->
                    Just
                        { prevChars = []
                        , curChar = chr
                        , str = String.fromChar (KoChar.toChar chr)
                        }

                Nothing ->
                    word


delChar : KoWord -> KoWord
delChar word =
    Maybe.andThen delCharOfForm word


delCharOfForm : WordForm -> KoWord
delCharOfForm form =
    case form.prevChars of
        [] ->
            Nothing

        newChr :: newPrevChars ->
            Just
                { form
                    | prevChars = newPrevChars
                    , curChar = newChr
                    , str = String.dropRight 1 form.str
                }


lastKoChar : KoWord -> Maybe KoChar
lastKoChar word =
    Maybe.map (\form -> form.curChar) word


last : KoWord -> Maybe Char
last word =
    Maybe.map KoChar.toChar (lastKoChar word)


firstKoChar : KoWord -> Maybe KoChar
firstKoChar word =
    Maybe.andThen firstKoCharOfForm word


firstKoCharOfForm : WordForm -> Maybe KoChar
firstKoCharOfForm form =
    case form.prevChars of
        [] ->
            Just form.curChar

        list ->
            List.Extra.last list


first : KoWord -> Maybe Char
first word =
    Maybe.map KoChar.toChar (firstKoChar word)


fromString : String -> Maybe KoWord
fromString str =
    Maybe.map (constructKoWord str << List.reverse) (getKoCharList str)


toString : KoWord -> String
toString word =
    case word of
        Just form ->
            form.str

        Nothing ->
            ""


getKoCharList : String -> Maybe (List KoChar)
getKoCharList str =
    case String.uncons str of
        Just ( head, tail ) ->
            Maybe.map2 (::)
                (KoChar.fromChar head)
                (getKoCharList tail)

        Nothing ->
            Just []


constructKoWord : String -> List KoChar -> KoWord
constructKoWord str list =
    case list of
        chr :: prev ->
            Just
                { prevChars = prev
                , curChar = chr
                , str = str
                }

        [] ->
            Nothing
