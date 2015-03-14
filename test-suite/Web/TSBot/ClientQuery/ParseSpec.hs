{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | A test specification for Web.TSBot.ClientQuery.Parse
module Web.TSBot.ClientQuery.ParseSpec (spec) where

import           Control.Applicative               ((<*))
import           Control.Arrow                     (first)
import           Data.Attoparsec.Text
import           Data.Char                         (isAlpha, isControl, isSpace)
import           Data.Functor                      ((<$>))
import           Data.Text                         (Text, pack, unpack)
import           Web.TSBot                         ()
import           Web.TSBot.ClientQuery.Parse
import           Web.TSBot.ClientQuery.PrettyPrint
import           Web.TSBot.ClientQuery.Response

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary AName where
  arbitrary = AName . pack <$> listOf1 anameCharGen
    where
      whitelistAname x = isAlpha x || x == '_'
      anameCharGen = suchThat arbitrary whitelistAname

instance Arbitrary CQValue where
  arbitrary = oneof [ return CQVNil
                    , CQVStr . pack . ('s':) <$> valid
                    , CQVInt . getPositive <$> arbitrary
                    , CQVBool <$> arbitrary
                    ]
    where
      validChar ' ' = True
      validChar x = not (isSpace x || isControl x)
      valid = listOf1 $ suchThat arbitrary validChar

instance Arbitrary CQR where
  arbitrary = fromListCQR . map (first unAName) <$> arbitrary

newtype CQRL = CQRL [CQR] deriving (Show)

instance Arbitrary CQRL where
  arbitrary = CQRL <$> vector 5

testParse :: Text -> Either String CQResponse
testParse = parseOnly (responseP <* endOfInput)

parseInv :: CQRL -> Property
parseInv (CQRL c) = testParse (resPrint c) === Right c

parseIs :: (Eq b, Show b) =>
           (a -> Text)
        -> (CQResponse -> b)
        -> (a -> b)
        -> a
        -> Property
parseIs tf tg cf i = case testParse $ tf i of
                         Left  _ -> property False
                         Right p -> tg p === cf i

spec :: Spec
spec =
  describe "responseP" $ do
    context "when given an empty string" $ do
      it "returns [fromListCQR []]" $ do
        testParse "" `shouldBe` Right [fromListCQR []]
    context "when given a string of n pipes ('|')" $ do
      it "gives a list with a length n + 1" $ do
        property $ \(Positive i) -> parseIs (pack . flip replicate '|') length (+1) i
    context "when given ugly-printed [CQR]" $ do
      it "is an involution" $ do
        property parseInv
