module Main where

import App (Event(RequestLoadPianoFont), foldp, initialState, view)
import Network.HTTP.Affjax (AJAX)
import Audio.SoundFont (AUDIO)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind, ($))
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Signal (Signal, constant)

initFont :: Signal Event
initFont = constant $ RequestLoadPianoFont "assets/soundfonts"

-- | Start and render the app
main :: Eff (CoreEffects (ajax :: AJAX, au:: AUDIO)) Unit
main = do

  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: [ initFont ]
    }

  renderToDOM "#app" app.markup app.input
