{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}

-- | A test specification for Web.TSBot.ClientQuery.Response
module Web.TSBot.ClientQuery.ResponseSpec (spec, CQRL (..)) where

import           Web.TSBot.ClientQuery.Response

import           Control.Arrow                  (first)
import           Data.Char                      (isAlpha, isControl, isSpace)
import           Data.Functor                   ((<$>))
import           Data.Text                      (Text, pack, unpack)
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

spec :: Spec
spec = it "is" pending
