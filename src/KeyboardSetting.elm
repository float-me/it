module KeyboardSetting exposing (..)

import Browser.Events
import Json.Decode as Decode


type alias KeyTrack =
    { ctrl : Bool
    , shift : Bool
    }


type KeyMode
    = Ctrl
    | CtrlShift
    | None


trackToMode : KeyTrack -> KeyMode
trackToMode track =
    case ( track.ctrl, track.shift ) of
        ( True, False ) ->
            Ctrl

        ( True, True ) ->
            CtrlShift

        _ ->
            None


type Msg
    = KeyPressedMsg KeyEventMsg
    | KeyReleasedMsg KeyEventMsg


type KeyEventMsg
    = KeyEventControl
    | KeyEventAlt
    | KeyEventShift
    | KeyEventMeta
    | KeyEventEnter
    | KeyEventSpaceBar
    | KeyEventBackspace
    | KeyEventLetter Char
    | KeyEventUnknown String


keyPressedDecoder : Decode.Decoder Msg
keyPressedDecoder =
    Decode.map (toKeyEventMsg >> KeyPressedMsg) (Decode.field "key" Decode.string)


keyReleasedDecoder : Decode.Decoder Msg
keyReleasedDecoder =
    Decode.map (toKeyEventMsg >> KeyReleasedMsg) (Decode.field "key" Decode.string)


toKeyEventMsg : String -> KeyEventMsg
toKeyEventMsg eventKeyString =
    case eventKeyString of
        "Control" ->
            KeyEventControl

        "Shift" ->
            KeyEventShift

        "Alt" ->
            KeyEventAlt

        "Meta" ->
            KeyEventMeta

        "Enter" ->
            KeyEventEnter

        " " ->
            KeyEventSpaceBar

        "Backspace" ->
            KeyEventBackspace

        string_ ->
            case String.uncons string_ of
                Just ( char, "" ) ->
                    KeyEventLetter char

                _ ->
                    KeyEventUnknown eventKeyString


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Browser.Events.onKeyDown keyPressedDecoder
        , Browser.Events.onKeyUp keyReleasedDecoder
        ]
