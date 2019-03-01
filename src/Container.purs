module Container where

import Prelude

import Audio.SoundFont (Instrument, loadPianoSoundFont)
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import Data.Abc (AbcTune)
import Data.Abc.Midi (toMidi)
import Data.Abc.Parser (PositionedParseError)
import Data.Array (singleton) as A
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.EditorComponent as ED
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC
import Lessons as Lessons

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError AbcTune
  , lessonIndex :: Int
  }

data Action =
    Move Int
  | Init
  | HandleNewTuneText ED.Message
  | HandleTuneIsPlaying PC.Message

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

type ChildSlots =
  ( editor :: ED.Slot Unit
  , player :: (PC.Slot MidiRecording) Unit
  )

_editor = SProxy :: SProxy "editor"
_player = SProxy :: SProxy "player"

component ::  forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { instruments: []
    , tuneResult: ED.nullTune
    , lessonIndex : 0
    }

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state = HH.div_
    [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text ("ABC Tutorial: Lesson "
             <> show (state.lessonIndex + 1)
             <> " - "
             <> Lessons.title state.lessonIndex)]
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
                 HH.slot _editor unit ED.component unit (Just <<< HandleNewTuneText)
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
            [ HH.div
               [ HP.class_ (H.ClassName "rightPanelComponent") ]
                 [HH.img
                   [ HP.src (Lessons.scoreUrl state.lessonIndex) ]
                 ]
              , HH.div
                 [ HP.id_ "hint"
                 , HP.class_ (H.ClassName "rightPanelComponent")
                 ]
                   [ HH.text $ Lessons.hint state.lessonIndex ]
            ]
      ]

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o Aff Unit
  handleAction = case _ of
    Init -> do
      instrument <- H.liftAff $ loadPianoSoundFont "assets/soundfonts"
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent (Lessons.example 0))
      _ <- H.modify (\st -> st { instruments = A.singleton instrument } )
      pure unit
    Move lessonIndex -> do
      _ <- H.modify (\st -> st { lessonIndex = lessonIndex } )
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent (Lessons.example lessonIndex))
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit
    HandleNewTuneText (ED.TuneResult r) -> do
      _ <- refreshPlayerState r
      let
        abcTune = either (\_ -> emptyTune) (identity) r
      _ <- H.modify (\st -> st { tuneResult = r } )
      pure unit
    HandleTuneIsPlaying message -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      pure unit

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o.
       Either PositionedParseError AbcTune
    -> H.HalogenM State Action ChildSlots o Aff Unit
refreshPlayerState tuneResult = do
  _ <- either
     (\_ -> H.query _player unit $ H.tell PC.StopMelody)
     (\abcTune -> H.query _player unit $ H.tell (PC.HandleNewPlayable (toPlayable abcTune)))
     tuneResult
  pure unit

-- helpers

-- | convert a tune to a format recognized by the player
toPlayable :: AbcTune -> MidiRecording
toPlayable abcTune =
   MidiRecording $ toMidi abcTune

-- rendering functions
renderPlayer :: State -> H.ComponentHTML Action ChildSlots Aff
renderPlayer state =
  case state.tuneResult of
    Right abcTune ->
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [
          HH.slot _player unit (PC.component (toPlayable abcTune) state.instruments) unit (Just <<< HandleTuneIsPlaying)
        ]
    Left err ->
      HH.div_
        [  ]

-- | render any of the four buttons that move the lesson index
renderMoveIndexButton :: MoveButtonType -> State -> H.ComponentHTML Action ChildSlots Aff
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
      [ HE.onClick  \_ -> Just (Move targetIndex)
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]
