module Korean.Char exposing (KoChar(..), apply, fromChar, fromSoleKey, toChar)

import Korean.Parts.Cho as Cho exposing (Cho(..))
import Korean.Parts.Jong as Jong exposing (Jong(..))
import Korean.Parts.Jung as Jung exposing (Jung(..))
import Korean.Parts.Tool exposing (StackObject(..))


type KoChar
    = UntilCho Cho
    | UntilJung Cho Jung
    | UntilJong Cho Jung Jong


apply : Char -> KoChar -> ( Maybe KoChar, KoChar )
apply chr koChr =
    case koChr of
        UntilCho cho ->
            ( Nothing, applyToCho cho chr )

        UntilJung cho jung ->
            applyToJung cho jung chr

        UntilJong cho jung jong ->
            applyToJong cho jung jong chr


applyToCho : Cho -> Char -> KoChar
applyToCho cho chr =
    if Jung.isUnstackable chr then
        let
            jung =
                Jung <| Unstackable chr
        in
        UntilJung cho jung

    else if Jung.isStackable chr then
        let
            jung =
                Jung <| Stackable chr
        in
        UntilJung cho jung

    else
        UntilCho cho


applyToJung : Cho -> Jung -> Char -> ( Maybe KoChar, KoChar )
applyToJung cho jung chr =
    case jung of
        Jung (Stackable s) ->
            if Jung.isStacked ( s, chr ) then
                let
                    newJung =
                        Jung <| Stacked s chr
                in
                ( Nothing, UntilJung cho newJung )

            else
                addJong cho jung chr

        _ ->
            addJong cho jung chr


addJong : Cho -> Jung -> Char -> ( Maybe KoChar, KoChar )
addJong cho jung chr =
    if Jong.isUnstackable chr then
        let
            jong =
                Jong <| Unstackable chr
        in
        ( Nothing, UntilJong cho jung jong )

    else if Jong.isStackable chr then
        let
            jong =
                Jong <| Stackable chr
        in
        ( Nothing, UntilJong cho jung jong )

    else if Cho.isKey chr then
        ( Just <| UntilJung cho jung, UntilCho cho )

    else
        ( Nothing, UntilJung cho jung )


applyToJong : Cho -> Jung -> Jong -> Char -> ( Maybe KoChar, KoChar )
applyToJong cho jung jong chr =
    case jong of
        Jong (Stackable s) ->
            if Jong.isStacked ( s, chr ) then
                let
                    newJong =
                        Jong <| Stacked s chr
                in
                ( Nothing, UntilJong cho jung newJong )

            else
                addKoChar s cho jung jong chr

        Jong (Unstackable s) ->
            addKoChar s cho jung jong chr

        Jong (Stacked s c) ->
            if Jung.isUnstackable chr then
                ( Just <| UntilJong cho jung <| Jong (Stackable s)
                , UntilJung (Cho c) <| Jung (Unstackable chr)
                )

            else if Jung.isStackable chr then
                ( Just <| UntilJong cho jung <| Jong (Stackable s)
                , UntilJung (Cho c) <| Jung (Stackable chr)
                )

            else
                addKoChar s cho jung jong chr


addKoChar : Char -> Cho -> Jung -> Jong -> Char -> ( Maybe KoChar, KoChar )
addKoChar s cho jung jong chr =
    if Jung.isUnstackable chr then
        ( Just <| UntilJung cho jung
        , UntilJung (Cho s) <| Jung (Unstackable chr)
        )

    else if Jung.isStackable chr then
        ( Just <| UntilJung cho jung
        , UntilJung (Cho s) <| Jung (Stackable chr)
        )

    else
        addCho cho jung jong chr


addCho : Cho -> Jung -> Jong -> Char -> ( Maybe KoChar, KoChar )
addCho cho jung jong chr =
    let
        koChr =
            UntilJong cho jung jong
    in
    if Cho.isKey chr then
        ( Just koChr, UntilCho (Cho chr) )

    else
        ( Nothing, koChr )


toChar : KoChar -> Char
toChar chr =
    case chr of
        UntilCho cho ->
            Cho.toChar cho

        UntilJung cho jung ->
            Char.fromCode (44032 + Cho.toVal cho * 588 + Jung.toVal jung * 28)

        UntilJong cho jung jong ->
            Char.fromCode (44032 + Cho.toVal cho * 588 + Jung.toVal jung * 28 + Jong.toVal jong)


fromChar : Char -> Maybe KoChar
fromChar chr =
    let
        code =
            Char.toCode chr - 44032

        ( maybeCho, maybeJung, maybeJong ) =
            if code > 0 && code < 11172 then
                ( Cho.fromVal <| code // 588, Jung.fromVal <| modBy 21 (code // 28), Jong.fromVal <| modBy 28 code )

            else
                ( Cho.fromChar chr, Nothing, Nothing )
    in
    case ( maybeCho, maybeJung, maybeJong ) of
        ( Nothing, _, _ ) ->
            Nothing

        ( Just cho, Nothing, _ ) ->
            Just <| UntilCho cho

        ( Just cho, Just jung, Nothing ) ->
            Just <| UntilJung cho jung

        ( Just cho, Just jung, Just jong ) ->
            Just <| UntilJong cho jung jong


fromSoleKey : Char -> Maybe KoChar
fromSoleKey key =
    if Cho.isKey key then
        let
            chr =
                UntilCho (Cho key)
        in
        Just chr

    else
        Nothing
