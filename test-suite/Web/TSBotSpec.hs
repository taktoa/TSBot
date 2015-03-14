{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A test specification for Web.TSBot
module Web.TSBotSpec (spec) where

import           Web.TSBot  ()

import           Test.Hspec

spec :: Spec
spec = it "is" pending
