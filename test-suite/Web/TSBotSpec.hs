{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | A test specification for Web.TSBot
--module Web.TSBotSpec (spec) where
module Web.TSBotSpec where

import           Control.Applicative               ((<*))
import           Control.Arrow                     (first)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Writer.Lazy
import           Data.Attoparsec.Text
import           Data.Char                         (isAlpha, isControl, isDigit,
                                                    isSpace)
import           Data.Conduit
import           Data.Functor                      ((<$>))
import           Data.String                       (IsString (..))
import           Data.Text                         (Text (..), pack, unpack)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T (putStrLn)
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

testParse :: CQRL -> Property
testParse (CQRL c) = parseOnly (responseP <* endOfInput) (resPrint c) === Right c

mn :: IO ()
mn = quickCheck $ \x -> let (r1, s) = tp x in whenFail (putStrLn $ unpack s) $ property r1

tp :: CQRL -> (Bool, Text)
tp (CQRL x) = runWriter $ do
  sep
  let rx = resPrint x
  tprn $ show rx
  newline' 2
  let p = parseOnly (responseP <* endOfInput) rx
  case p of
   Left  s  -> do
     tputln ("Failed to parse: " <> pack s)
     newline' 2
     tprn $ show $ parseOnly responseP rx
     sep
     return False
   Right rp -> if rp == x
               then return True
               else do
                 tprn $ show $ resPrint rp
                 newline' 3
                 tprn $ show x
                 newline' 2
                 tprn $ show rp
                 sep
                 return False
  where
    tput = writer . ((),) :: Monoid a => a -> Writer a ()
    tputln = tput . (<> "\n") :: Text -> Writer Text ()
    newline' i = replicateM_ i $ tputln "\n" :: Writer Text ()
    newline = newline' 1 :: Writer Text ()
    sep = tputln "------------------" :: Writer Text ()
    tprn = tputln . pack :: String -> Writer Text ()


spec :: Spec
spec =
  describe "responseP" $
    context "when given ugly-printed [CQR]" $
      it "is an involution" $
        property testParse
