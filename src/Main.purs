module Main where

import Concur.Core (Widget)
import Concur.VDom (HTML, el, handle, run, text)
import Control.Applicative (pure)
import Control.Bind (discard, (>>=))
import Data.CommutativeRing ((+))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Data.Void (Void)
import Effect (Effect)

counter :: Int -> Widget HTML Int
counter i = do
  el "button" [ handle "click" ] [ text ("Count: " <> show i) ]
  pure (i + 1)

sample :: Widget HTML Void
sample = go 0
  where
  go n = counter n >>= go

main :: Effect Unit
main = run sample

