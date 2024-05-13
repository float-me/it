module Rule exposing (HeadingState, Msg(..), RuleState(..), check, fetch, fromText, getInitialHeading, getInitialHeadingByChar, heading, unheading)

import Dict exposing (Dict)
import Graph exposing (..)
import Http
import Korean.Char as KoChar exposing (KoChar(..))
import Korean.Parts.Cho as Cho exposing (Cho(..))
import Korean.Parts.Jung as Jung exposing (Jung(..))
import List exposing (map)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)


words_url : String
words_url =
    "https://singrum.github.io/KoreanDict/oldict/db/%EB%AA%85%EC%82%AC"


type alias Rule =
    { words : List String

    -- , chars : List Char
    -- , charToId : Dict Char NodeId
    -- , graph : Graph Char String
    }


type RuleState
    = RuleState (WebData Rule)


type alias HeadingState =
    Maybe Bool


type Msg
    = RecvRule (Result Http.Error String)


fetch : Cmd Msg
fetch =
    Http.get
        { url = words_url
        , expect = Http.expectString RecvRule
        }


check : RuleState -> String -> Bool
check ruleState word =
    case ruleState of
        RuleState (Success rule) ->
            if String.length word > 1 && List.member word rule.words then
                True

            else
                False

        _ ->
            True


headingLike : (Cho -> Jung -> ( Cho, Bool )) -> KoChar -> ( KoChar, Bool )
headingLike headingFunc koChr =
    case koChr of
        UntilCho _ ->
            ( koChr, False )

        UntilJung cho jung ->
            let
                ( newCho, succ ) =
                    headingFunc cho jung
            in
            ( UntilJung newCho jung, succ )

        UntilJong cho jung jong ->
            let
                ( newCho, succ ) =
                    headingFunc cho jung
            in
            ( UntilJong newCho jung jong, succ )


fromText : String -> Rule
fromText text =
    let
        wordList =
            String.split "\u{000D}\n" text

        -- charList =
        --     getCharList wordList
        -- charToId =
        --     Dict.fromList <| List.map (\( a, b ) -> ( b, a )) (List.indexedMap Tuple.pair charList)
    in
    { words = wordList

    -- , chars = charList
    -- , charToId = charToId
    -- , graph = getGraph charList charToId wordList
    }


getId : Dict Char NodeId -> Char -> NodeId
getId charToId chr =
    case Dict.get chr charToId of
        Just id ->
            id

        Nothing ->
            -1


getGraph : List Char -> Dict Char NodeId -> List String -> Graph Char String
getGraph charList charToId wordList =
    let
        nodeList =
            List.map (\chr -> Node (getId charToId chr) chr) charList

        edgeList =
            List.map (\word -> Edge (getId charToId <| getFirst word) (getId charToId <| getLast word) word) wordList
    in
    fromNodesAndEdges nodeList edgeList


getCharList : List String -> List Char
getCharList list =
    List.Extra.unique <| map getFirst list ++ map getLast list


getFirst : String -> Char
getFirst str =
    case String.uncons str of
        Just ( first, _ ) ->
            first

        Nothing ->
            '?'


getLast : String -> Char
getLast str =
    getFirst <| String.reverse str


heading : KoChar -> ( KoChar, Bool )
heading koChr =
    headingLike headingCalc koChr


headingCalc : Cho -> Jung -> ( Cho, Bool )
headingCalc cho jung =
    let
        jungVal =
            Jung.toVal jung
    in
    case Cho.toVal cho of
        5 ->
            if List.member jungVal [ 0, 1, 8, 11, 13, 18 ] then
                ( Maybe.withDefault cho <| Cho.fromVal 2, True )
                -- Default는 발생하지 않음

            else if List.member jungVal [ 2, 6, 7, 12, 17, 20 ] then
                ( Maybe.withDefault cho <| Cho.fromVal 11, True )
                -- Default는 발생하지 않음

            else
                ( cho, False )

        2 ->
            if List.member jungVal [ 6, 12, 17, 20 ] then
                ( Maybe.withDefault cho <| Cho.fromVal 11, True )
                -- Default는 발생하지 않음

            else
                ( cho, False )

        _ ->
            ( cho, False )


unheading : KoChar -> ( KoChar, Bool )
unheading koChr =
    headingLike unheadingCalc koChr


unheadingCalc : Cho -> Jung -> ( Cho, Bool )
unheadingCalc cho jung =
    let
        jungVal =
            Jung.toVal jung
    in
    case Cho.toVal cho of
        2 ->
            if List.member jungVal [ 0, 1, 8, 11, 13, 18 ] then
                ( Maybe.withDefault cho <| Cho.fromVal 5, True )
                -- Default는 발생하지 않음

            else
                ( cho, False )

        11 ->
            if List.member jungVal [ 2, 6, 7, 12, 17, 20 ] then
                ( Maybe.withDefault cho <| Cho.fromVal 5, True )
                -- Default는 발생하지 않음

            else if List.member jungVal [ 6, 12, 17, 20 ] then
                ( Maybe.withDefault cho <| Cho.fromVal 2, True )
                -- Default는 발생하지 않음

            else
                ( cho, False )

        _ ->
            ( cho, False )


getInitialHeading : KoChar -> HeadingState
getInitialHeading koChr =
    case koChr of
        UntilCho _ ->
            Nothing

        UntilJung cho jung ->
            if isHeadingable cho jung then
                Just False

            else
                Nothing

        UntilJong cho jung _ ->
            if isHeadingable cho jung then
                Just False

            else
                Nothing


getInitialHeadingByChar : Char -> HeadingState
getInitialHeadingByChar chr =
    Maybe.andThen getInitialHeading (KoChar.fromChar chr)


isHeadingable : Cho -> Jung -> Bool
isHeadingable cho jung =
    let
        jungVal =
            Jung.toVal jung
    in
    case Cho.toVal cho of
        5 ->
            if List.member jungVal [ 0, 1, 8, 11, 13, 18 ] then
                True

            else if List.member jungVal [ 2, 6, 7, 12, 17, 20 ] then
                True

            else
                False

        2 ->
            if List.member jungVal [ 6, 12, 17, 20 ] then
                True

            else
                False

        _ ->
            False
