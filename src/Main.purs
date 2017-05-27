module Main where

import App (Event(Init), foldp, initialState, view)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (Canceler, launchAff)
import Audio.SoundFont (AUDIO, loadPianoSoundFont)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Signal (constant)
import Pux.Renderer.React (renderToDOM)

initialiseApp :: forall e. Eff (exception :: EXCEPTION | e) (Canceler e)
initialiseApp = do
  launchAff (loadPianoSoundFont "assets/soundfonts")

-- | Start and render the app
main :: Eff (CoreEffects (au:: AUDIO)) Unit
main = do

  _ <- initialiseApp

  let
    initSignal = constant Init

  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: [initSignal]
    }

  renderToDOM "#app" app.markup app.input
