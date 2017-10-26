module App where

-- import CSS.Geometry (paddingTop, paddingBottom, marginLeft, marginRight, marginTop, width)

import Network.HTTP.Affjax (AJAX)
import Audio.Midi.Player as MidiPlayer
import Lessons as Lessons
import Audio.SoundFont (AUDIO, Instrument, loadPianoSoundFont)
import Data.Abc (AbcTune)
import Data.Abc.Parser (PositionedParseError(..), parse)
import Data.Array (length, singleton, slice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, toCharArray)
import View.CSS
import Prelude (const, bind, discard, max, min, pure, show, ($), (#), (<>), (+), (-), (<=), (>=), (>), (<))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onInput, onClick, targetValue)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (button, div, h1, img, p, span, textarea)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (text, (#!), (!), (!?))

-- import Debug.Trace (trace, traceShow, traceShowM)

data Event
    = NoOp
    | RequestLoadPianoFont String
    | FontLoaded Instrument
    | Abc String
    | Move Boolean           -- Move lesson up/down
    | MoveToEnd Boolean      -- Move lesson to start/finish
    | PlayerEvent MidiPlayer.Event

type State = {
    abc :: String
  , tuneResult :: Either PositionedParseError AbcTune
  , lessonIndex :: Int
  , playerState :: MidiPlayer.State
}

-- | there is no tune yet
nullTune :: Either PositionedParseError AbcTune
nullTune =
  Left (PositionedParseError { pos : 0, error : "" })

initialState :: State
initialState =
  { abc : (Lessons.example 0) <> " |\r\n"
  , tuneResult : nullTune
  , lessonIndex : 0
  , playerState : MidiPlayer.initialState []
  }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, au :: AUDIO)
foldp NoOp state = noEffects $ state
foldp (RequestLoadPianoFont fontDir) state =
  let
    effects =
      [
        do  -- request the fonts are loaded
          instrument <- loadPianoSoundFont fontDir
          pure $ Just (FontLoaded instrument)
      ]
  in
    {state: state, effects: effects}
foldp (FontLoaded instrument) state =
  let
    playerState = MidiPlayer.initialState (singleton instrument)
  in
    -- we need to react to a changed Euterpea after each instrument font loads.  This is because the user may edit
    -- the tne text to incorporate the new instrument names before loading their soundfonts
    onChangedAbc state.abc $ state { playerState = playerState }
foldp (Abc s) state = onChangedAbc s state
foldp (Move isUp) state =
  let
    next =
      if isUp then
        min (state.lessonIndex + 1) Lessons.last
      else
        max (state.lessonIndex - 1) 0
  in
    onChangedAbc (Lessons.example next) (state { lessonIndex = next })
foldp (MoveToEnd isUp) state =
  let
    next =
      if isUp then
        Lessons.last
      else
        0
  in
    onChangedAbc (Lessons.example next) (state { lessonIndex = next })
foldp (PlayerEvent e) state =
  MidiPlayer.foldp e state.playerState
    # mapEffects PlayerEvent
    # mapState \pst -> state { playerState = pst }


-- | make sure everything is notified if the ABC changes for any reason
-- | we'll eventually have to add effects
onChangedAbc  :: forall e. String -> State ->  EffModel State Event e
onChangedAbc abc state =
  let
    tuneResult =
      parse $ abc <> " |\r\n"
    newState =
      state { tuneResult = tuneResult, abc = abc }
  in
    case tuneResult of
      Right tune ->
        {state: newState
          , effects:
            [
              do
                pure $ Just (PlayerEvent (MidiPlayer.SetAbc tune))
            ]

        }
      Left err ->
        noEffects newState


debugPlayer :: State -> HTML Event
debugPlayer state =
  do
    text ("player melody size: " <> (show $ length state.playerState.basePlayer.melody))

-- | display a snippet of text with the error highlighted
viewParseError :: State -> HTML Event
viewParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = toCharArray state.abc
  in
    case state.tuneResult of
      Left (PositionedParseError pe) ->
        let
          -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
          startPhrase =
            max (pe.pos - textRange) 0
          errorPrefix =
            slice startPhrase pe.pos txt
          startSuffix =
            min (pe.pos + 1) (length txt)
          endSuffix =
            min (pe.pos + textRange + 1) (length txt)
          errorSuffix =
            slice startSuffix endSuffix txt
          errorChar =
            slice pe.pos (pe.pos + 1) txt
        in
          p do
              text $ pe.error <> " - "
              text $ fromCharArray errorPrefix
              span ! errorHighlightStyle $ text (fromCharArray errorChar)
              text $ fromCharArray errorSuffix
      _ ->
        text ""

-- | only display the player if we have a Melody
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.tuneResult of
    Right _ ->
      child PlayerEvent MidiPlayer.view $ state.playerState
    _ ->
      text ""

view :: State -> HTML Event
view state =
   div do
      -- title and instructions at the top
      h1 ! centreStyle $ text ("ABC Tutorial: Lesson " <> (show $ state.lessonIndex + 1) <> " - " <> Lessons.title state.lessonIndex)
      div ! instructionStyle $ do
        p $ text (Lessons.instruction state.lessonIndex)
      -- the ABC text on the left
      div ! leftPaneStyle $ do
        div ! leftMarginStyle $ do
          textarea ! taStyle ! At.cols "50" ! At.rows "15" ! At.value state.abc
              ! At.spellcheck "false" ! At.autocomplete "false" ! At.autofocus "true"
               #! onInput (\e -> Abc (targetValue e) ) $ text ""
          viewParseError state

          div ! leftComponentStyle $ do
            (button !? (state.lessonIndex <= 0)) (At.disabled "disabled") ! (buttonStyle (state.lessonIndex > 0)) ! At.className "hoverable"
                #! onClick (const $ MoveToEnd false) $ text "first"
            (button !? (state.lessonIndex <= 0)) (At.disabled "disabled") ! (buttonStyle (state.lessonIndex > 0)) ! At.className "hoverable"
                #! onClick (const $ Move false) $ text "previous"
            (button !? (state.lessonIndex >= Lessons.last)) (At.disabled "disabled") ! (buttonStyle (state.lessonIndex < Lessons.last)) ! At.className "hoverable"
                #! onClick (const $ Move true) $ text "next"
            (button !? (state.lessonIndex >= Lessons.last)) (At.disabled "disabled") ! (buttonStyle (state.lessonIndex < Lessons.last)) ! At.className "hoverable"
                #! onClick (const $ MoveToEnd true) $ text "last"
            viewPlayer state

      -- the score and player on the right
      div ! floatLeftStyle $ do
        div do
          img ! At.src (Lessons.scoreUrl state.lessonIndex)
        div ! hintStyle $ do
            p $ text (Lessons.hint state.lessonIndex)
