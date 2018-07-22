module Container where

import Prelude

import Audio.SoundFont (Instrument, loadPianoSoundFont)
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import Data.Abc (AbcTune)
import Data.Abc.Midi (toMidi)
import Data.Abc.Parser (PositionedParseError)
import Lessons as Lessons
import Data.Array (singleton) as A
import Data.Either (Either(..), either)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.EditorComponent as ED
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError AbcTune
  , lessonIndex :: Int
  }

data Query a =
    Init a
  | HandleNewTuneText ED.Message a
  | Move Int a          -- Move lesson to this number
  | HandleTuneIsPlaying PC.Message a

data MoveButtonType =
    Start
  | Forward
  | Back
  | Finish

emptyTune :: AbcTune
emptyTune =
  { headers : Nil, body: Nil }

-- the player is generic over a variety of playable sources of music
-- so we must specialize to MidiRecording
type PlayerQuery = PC.Query MidiRecording

type ChildQuery = Coproduct2 ED.Query PlayerQuery

-- slots and slot numbers
type PlayerSlot = Unit
type EditorSlot = Unit

type ChildSlot = Either2 Unit Unit

editorSlotNo :: CP.ChildPath ED.Query ChildQuery EditorSlot ChildSlot
editorSlotNo = CP.cp1

playerSlotNo :: CP.ChildPath PlayerQuery ChildQuery PlayerSlot ChildSlot
playerSlotNo = CP.cp2

component ::  H.Component HH.HTML Query Unit Void Aff
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { instruments: []
    , tuneResult: ED.nullTune
    , lessonIndex : 0
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render state = HH.div_
    [ HH.h1
       [HP.class_ (H.ClassName "center") ]
       [HH.text "ABC Tutorial"]
      , HH.div
          [ HP.id_ "instruction" ]
          [ HH.text $ Lessons.instruction state.lessonIndex ]

      -- left pane - editor and controls
      , HH.div
        [ HP.class_ (H.ClassName "leftPane") ]
        [ HH.div_
          -- left pane
          [ HH.div
             [ HP.class_ (H.ClassName "leftPanelComponent") ]
               [
                 HH.slot' editorSlotNo unit ED.component unit (HE.input HandleNewTuneText)
               ]
          , HH.div
            [ HP.class_ (H.ClassName "leftPanelComponent") ]
            [
              renderMoveIndexButton Start state
            , renderMoveIndexButton Forward state
            , renderMoveIndexButton Back state
            , renderMoveIndexButton Finish state
            ]
            , renderPlayer state
          ]
        ]

        -- right pane - score and hint
        , HH.div
          [ HP.class_ (H.ClassName "rightPane") ]
            [ HH.div_
              [HH.img
                [ HP.src (Lessons.scoreUrl state.lessonIndex) ]
              ]
              , HH.div
                 [ HP.id_ "hint" ]
                 [ HH.text $ Lessons.hint state.lessonIndex ]
            ]
      ]


  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (Init next) = do
    instrument <- H.liftAff $ loadPianoSoundFont "assets/soundfonts"
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent (Lessons.example 0))
    _ <- H.modify (\st -> st { instruments = A.singleton instrument } )
    pure next
  eval (HandleNewTuneText (ED.TuneResult r) next) = do
    _ <- refreshPlayerState r
    let
      abcTune = either (\_ -> emptyTune) (identity) r
    _ <- H.modify (\st -> st { tuneResult = r } )
    pure next
  eval (Move lessonIndex next) = do
    _ <- H.modify (\st -> st { lessonIndex = lessonIndex } )
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent (Lessons.example lessonIndex))
    _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
    pure next
  eval (HandleTuneIsPlaying (PC.IsPlaying p) next) = do
    -- we ignore this message, but if we wanted to we could
    -- disable any button that can alter the editor contents whilst the player
    -- is playing and re-enable when it stops playing
    {-
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateEnabled (not p))
    _ <- H.query' clearTextSlotNo unit $ H.action (Button.UpdateEnabled (not p))
    -}
    pure next

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState ::
       Either PositionedParseError AbcTune
    -> H.ParentDSL State Query ChildQuery ChildSlot Void Aff Unit
refreshPlayerState tuneResult = do
  _ <- either
    (\_ -> H.query' playerSlotNo unit $ H.action (PC.StopMelody))
    (\abcTune -> H.query' playerSlotNo unit $ H.action (PC.HandleNewPlayable (toPlayable abcTune)))
    tuneResult
  pure unit

-- helpers

-- | convert a tune to a format recognized by the player
toPlayable :: AbcTune -> MidiRecording
toPlayable abcTune =
   MidiRecording $ toMidi abcTune

-- rendering functions
renderPlayer ::  State -> H.ParentHTML Query ChildQuery ChildSlot Aff
renderPlayer state =
  case state.tuneResult of
    Right abcTune ->
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ HH.slot' playerSlotNo unit (PC.component (toPlayable abcTune) state.instruments) unit (HE.input HandleTuneIsPlaying)  ]
    Left err ->
      HH.div_
        [  ]

-- | render any of the four buttons that move the lesson index
renderMoveIndexButton :: MoveButtonType -> State -> H.ParentHTML Query ChildQuery ChildSlot Aff
renderMoveIndexButton buttonType state =
  let
    enabled =
      case buttonType of
        Start ->
          (state.lessonIndex > 0)
        Forward ->
          (state.lessonIndex < Lessons.last)
        Back ->
          (state.lessonIndex > 0)
        Finish ->
          (state.lessonIndex < Lessons.last)
    targetIndex =
      case buttonType of
        Start ->
          0
        Forward ->
          state.lessonIndex + 1
        Back ->
          state.lessonIndex - 1
        Finish ->
          Lessons.last
    className =
      case buttonType of
        Start ->
          if (state.lessonIndex > 0) then "hoverable" else "unhoverable"
        Forward ->
          if (state.lessonIndex < Lessons.last) then "hoverable" else "unhoverable"
        Back ->
          if (state.lessonIndex > 0) then "hoverable" else "unhoverable"
        Finish ->
          if (state.lessonIndex < Lessons.last) then "hoverable" else "unhoverable"
    label =
      case buttonType of
        Start ->
          "first"
        Forward ->
          "next"
        Back ->
          "previous"
        Finish ->
          "last"
  in
    HH.button
      [ HE.onClick (HE.input_ (Move targetIndex))
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]
