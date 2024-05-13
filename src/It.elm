module It exposing (main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import KeyboardSetting exposing (..)
import Korean.Char as KoChar
import Korean.Word as KoWord exposing (KoWord)
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData(..))
import Rule exposing (RuleState(..))


type alias Model =
    { previousStates : List State
    , futureStates : List State
    , currentInitState : State
    , currentState : State
    , keyTrack : KeyTrack
    , ruleState : RuleState
    }


type State
    = EmptyState Mode
    | SemiEmptyState Mode Char
    | State Form


type alias Form =
    { currentPlay : List String
    , first : Char
    , heading : Rule.HeadingState
    , editingWord : KoWord
    , mode : Mode
    }


type Mode
    = Edit
    | Group


type Msg
    = KeyboardMsg KeyboardSetting.Msg
    | RuleMsg Rule.Msg



--INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.map RuleMsg Rule.fetch )


initialModel : Model
initialModel =
    { previousStates = []
    , futureStates = []
    , currentInitState = EmptyState Edit
    , currentState = EmptyState Edit
    , keyTrack = { ctrl = False, shift = False }
    , ruleState = RuleState RemoteData.Loading
    }



--CMDS
--VIEW


view : Model -> Html Msg
view model =
    viewState model.currentState


getStringList : State -> List String
getStringList state =
    case state of
        EmptyState _ ->
            []

        SemiEmptyState _ first ->
            [ String.fromChar first ]

        State form ->
            String.cons form.first (KoWord.toString form.editingWord) :: form.currentPlay


viewState : State -> Html Msg
viewState state =
    div []
        [ lazy viewPlay <| getStringList state
        ]


viewPlay : List String -> Html Msg
viewPlay wordList =
    div [ class "char-btn-card", id "tagEditor" ] <| List.reverse (List.map wordTag wordList)


wordTag : String -> Html Msg
wordTag word =
    div [ class "char-container" ]
        [ span [ class "char-btn", spellcheck False ] [ text word ]
        ]



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            case keyMsg of
                KeyPressedMsg keyEventMsg ->
                    updateKeyDown keyEventMsg model

                KeyReleasedMsg keyEventMsg ->
                    updateKeyUp keyEventMsg model

        RuleMsg (Rule.RecvRule result) ->
            case result of
                Ok text ->
                    ( applyNewRule text model, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( { model | ruleState = RuleState <| RemoteData.Failure error }, Cmd.none )


applyNewRule : String -> Model -> Model
applyNewRule text model =
    let
        webData =
            RemoteData.Success <| Debug.log "rule" (Rule.fromText text)
    in
    { model | ruleState = RuleState webData }


updateKeyDown : KeyEventMsg -> Model -> ( Model, Cmd Msg )
updateKeyDown keyEventMsg model =
    case keyEventMsg of
        KeyEventShift ->
            ( toggleShift model, Cmd.none )

        KeyEventControl ->
            ( toggleCtrl model, Cmd.none )

        KeyEventEnter ->
            case model.currentState of
                State form ->
                    ( modify heading form model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyEventSpaceBar ->
            case model.currentState of
                State form ->
                    ( modify (addWord model) form model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyEventBackspace ->
            updateBackspace model

        KeyEventLetter letter ->
            case trackToMode model.keyTrack of
                None ->
                    updateLetterInput letter model

                Ctrl ->
                    case letter of
                        'z' ->
                            ( goBack model, Cmd.none )

                        'y' ->
                            ( goFuture model, Cmd.none )

                        'r' ->
                            ( { model | ruleState = RuleState RemoteData.Loading }, Cmd.map RuleMsg Rule.fetch )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyEventUnknown string ->
            let
                _ =
                    Debug.log "string" string
            in
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


heading : Form -> ( State, Bool )
heading form =
    case form.heading of
        Just headed ->
            case KoChar.fromChar form.first of
                Just chr ->
                    if headed then
                        let
                            ( newChr, _ ) =
                                Rule.unheading chr

                            newForm =
                                { form | first = KoChar.toChar newChr, heading = Just False }
                        in
                        ( State newForm, False )

                    else
                        let
                            ( newChr, _ ) =
                                Rule.heading chr

                            newForm =
                                { form | first = KoChar.toChar newChr, heading = Just True }
                        in
                        ( State newForm, False )

                Nothing ->
                    ( State form, False )

        -- 존재하지 않는 케이스
        Nothing ->
            ( State form, False )


updateBackspace : Model -> ( Model, Cmd Msg )
updateBackspace model =
    case model.currentState of
        EmptyState _ ->
            ( model, Cmd.none )

        SemiEmptyState _ _ ->
            ( { model | currentState = EmptyState Edit }, Cmd.none )

        State form ->
            let
                word =
                    form.editingWord
            in
            case word of
                Nothing ->
                    ( modify delWord form model, Cmd.none )

                Just _ ->
                    let
                        newWord =
                            KoWord.delChar word

                        newForm =
                            { form | editingWord = newWord }
                    in
                    ( { model | currentState = State newForm }, Cmd.none )


updateLetterInput : Char -> Model -> ( Model, Cmd Msg )
updateLetterInput letter model =
    case model.currentState of
        EmptyState _ ->
            case KoChar.fromSoleKey letter of
                Just chr ->
                    let
                        first =
                            KoChar.toChar chr
                    in
                    ( { model | currentState = SemiEmptyState Edit first }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SemiEmptyState _ first ->
            case Maybe.map (KoChar.apply letter) (KoChar.fromChar first) of
                Just ( Nothing, koChr ) ->
                    ( { model | currentState = SemiEmptyState Edit (KoChar.toChar koChr) }, Cmd.none )

                Just ( Just newFirst, koChr ) ->
                    ( { model
                        | currentState =
                            State
                                { currentPlay = []
                                , first = KoChar.toChar newFirst
                                , editingWord = KoWord.fromKoChar koChr
                                , heading = Rule.getInitialHeading koChr
                                , mode = Edit
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        State form ->
            let
                newWord =
                    KoWord.apply letter form.editingWord

                newForm =
                    { form | editingWord = newWord }
            in
            ( { model | currentState = State newForm }, Cmd.none )


updateKeyUp : KeyEventMsg -> Model -> ( Model, Cmd Msg )
updateKeyUp keyEventMsg model =
    case keyEventMsg of
        KeyEventShift ->
            ( toggleShift model, Cmd.none )

        KeyEventControl ->
            ( toggleCtrl model, Cmd.none )

        _ ->
            ( model, Cmd.none )


toggleCtrl : Model -> Model
toggleCtrl model =
    let
        track =
            model.keyTrack

        modelCtrl =
            track.ctrl
    in
    { model | keyTrack = { track | ctrl = not modelCtrl } }


toggleShift : Model -> Model
toggleShift model =
    let
        track =
            model.keyTrack

        modelShift =
            track.shift
    in
    { model | keyTrack = { track | shift = not modelShift } }


modify : (Form -> ( State, Bool )) -> Form -> Model -> Model
modify formToState form model =
    let
        ( newState, doUpdate ) =
            formToState form
    in
    if doUpdate then
        updateState newState model

    else
        { model | currentState = newState }


updateState : State -> Model -> Model
updateState state model =
    { model
        | currentState = state
        , currentInitState = state
        , previousStates = model.currentInitState :: model.previousStates
        , futureStates = []
    }


goBack : Model -> Model
goBack model =
    case model.previousStates of
        [] ->
            model

        prevState :: morePrevStates ->
            { model
                | previousStates = morePrevStates
                , currentState = prevState
                , currentInitState = prevState
                , futureStates = model.currentInitState :: model.futureStates
            }


goFuture : Model -> Model
goFuture model =
    case model.futureStates of
        [] ->
            model

        futureState :: moreFutureStates ->
            { model
                | previousStates = model.currentState :: model.previousStates
                , currentState = futureState
                , currentInitState = futureState
                , futureStates = moreFutureStates
            }


addWord : Model -> Form -> ( State, Bool )
addWord model form =
    let
        completeWord =
            String.cons form.first (KoWord.toString form.editingWord)
    in
    if Rule.check model.ruleState completeWord then
        ( State (constructAddedForm completeWord form), True )

    else
        ( State
            { form
                | editingWord = Nothing
            }
        , False
        )


constructAddedForm : String -> Form -> Form
constructAddedForm completeWord form =
    case form.editingWord of
        Just wordForm ->
            { form
                | currentPlay = completeWord :: form.currentPlay
                , editingWord = Nothing
                , first = KoChar.toChar wordForm.curChar
                , heading = Rule.getInitialHeading wordForm.curChar
            }

        Nothing ->
            -- 존재하지 않는 케이스
            form


delWord : Form -> ( State, Bool )
delWord form =
    case form.currentPlay of
        word :: play ->
            case play of
                [] ->
                    ( EmptyState Edit, True )

                _ ->
                    case String.uncons word of
                        Just ( first, _ ) ->
                            ( State
                                { form
                                    | currentPlay = play
                                    , first = first
                                    , heading = Rule.getInitialHeadingByChar first
                                }
                            , True
                            )

                        Nothing ->
                            -- 존재하지 않는 경우
                            ( State form, False )

        [] ->
            ( EmptyState Edit, False )



-- CMDS
--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyboardMsg KeyboardSetting.subscriptions



--MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
