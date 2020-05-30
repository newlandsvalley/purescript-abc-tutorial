module Tutorial.Container where

import Prelude

import Audio.SoundFont (Instrument)
import Data.Abc (AbcTune)
import Data.Abc.Parser (PositionedParseError)
import Data.Abc.PlayableAbc (PlayableAbc(..))
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Data.Const (Const)
import Halogen as H
import Tutorial.EditorComponent as ED
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC
import Tutorial.Lessons as Lessons

type Slot = H.Slot (Const Void) Void

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
  | Finalize

data MoveButtonType =
    Start
  | Forward
  | Back
  | Finish

type Input =
  { instruments :: Array Instrument }

emptyTune :: AbcTune
emptyTune =
  { headers : Nil, body: Nil }

-- the player is generic over a variety of playable sources of music
-- so we must specialize to MidiRecording
type PlayerQuery = PC.Query PlayableAbc

type ChildSlots =
  ( editor :: ED.Slot Unit
  , player :: (PC.Slot PlayableAbc) Unit
  )

_editor = SProxy :: SProxy "editor"
_player = SProxy :: SProxy "player"

component :: ∀ q o m. MonadAff m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Just Finalize
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { instruments: input.instruments
    , tuneResult: ED.nullTune
    , lessonIndex : 0
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div
    [ HP.id_ "abcTutorial" ]
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

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o m Unit

  handleAction = case _ of
    Init -> do
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent (Lessons.example 0))
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
    Finalize -> do
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o m.
       Either PositionedParseError AbcTune
    -> H.HalogenM State Action ChildSlots o m Unit
refreshPlayerState tuneResult = do
  _ <- either
     (\_ -> H.query _player unit $ H.tell PC.StopMelody)
     (\abcTune -> H.query _player unit $ H.tell (PC.HandleNewPlayable (toPlayable abcTune)))
     tuneResult
  pure unit

-- helpers

-- | convert a tune to a format recognized by the player
toPlayable :: AbcTune -> PlayableAbc
toPlayable abcTune =
   PlayableAbc { abcTune: abcTune, bpm : 120, phraseSize : 0.7, generateIntro : false  }


-- rendering functions
renderPlayer :: ∀ m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
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
renderMoveIndexButton :: ∀ m. MoveButtonType -> State -> H.ComponentHTML Action ChildSlots m
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
