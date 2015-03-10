module TSBotSpec (main, spec) where

import           TSBot      ()

import           Test.Hspec

spec :: Spec
spec = describe "main" . it "returns the unit" $ pending

main :: IO ()
main = hspec spec
