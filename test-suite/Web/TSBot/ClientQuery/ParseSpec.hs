{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | A test specification for Web.TSBot.ClientQuery.Parse
module Web.TSBot.ClientQuery.ParseSpec (spec) where

import           Control.Applicative                ((<*))
import           Control.Arrow                      (first)
import           Data.Attoparsec.Text
import           Data.Char                          (isAlpha, isControl,
                                                     isSpace)
import           Data.Functor                       ((<$>))
import           Data.Text                          (Text, pack, unpack)
import           Web.TSBot                          ()
import           Web.TSBot.ClientQuery.Parse
import           Web.TSBot.ClientQuery.PrettyPrint
import           Web.TSBot.ClientQuery.Response
import           Web.TSBot.ClientQuery.ResponseSpec (CQRL (..))

import           Test.Hspec
import           Test.QuickCheck


testParse :: Text -> Either String CQResponse
testParse = parseOnly (responseP <* endOfInput)

parseInv :: CQRL -> Property
parseInv (CQRL c) = testParse (resPrint c) === Right c

parseIs :: (Eq c, Show c) =>
           (a -> b)
        -> (b -> Text)
        -> (CQResponse -> c)
        -> (b -> c)
        -> a -> Property
parseIs f tf tg cf i = case testParse $ tf $ f i of
                         Left  _ -> property False
                         Right p -> tg p === cf (f i)

spec :: Spec
spec =
  describe "responseP" $ do
    context "when given an empty string" $ do
      it "returns [fromListCQR []]" $ do
        testParse "" `shouldBe` Right [fromListCQR []]
    context "when given a string of n pipes ('|')" $ do
      it "gives a list with a length n + 1" $ do
        property $ parseIs getPositive (pack . flip replicate '|') length (+1)
    context "when given ugly-printed [CQR]" $ do
      it "is an involution" $ do
        property parseInv
