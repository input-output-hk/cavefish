module Main (main) where

import qualified Spec
import Test.Hspec (hspec)

main :: IO ()
main = hspec Spec.spec
