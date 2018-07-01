module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Component (ui)

-- | Run the app.
main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX, random :: RANDOM, dom :: DOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
