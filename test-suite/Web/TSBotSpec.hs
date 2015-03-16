{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A test specification for Web.TSBot
module Web.TSBotSpec (spec) where

import           Control.Applicative                ((*>), (<*), (<**>), (<*>),
                                                     (<|>))
import           Control.Monad                      (forM_)
import           Data.Attoparsec.Text
import           Data.Conduit
import           Data.Functor                       ((<$>))
import           Data.Monoid                        (Monoid (..))
import           Data.Text                          (Text, pack, unpack)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Web.TSBot
import           Web.TSBot.ClientQuery.Parse
import           Web.TSBot.ClientQuery.PrettyPrint
import           Web.TSBot.ClientQuery.ResponseSpec (CQRL (..))

type PCQR = Either String CQResponse

condTest :: (Monad m, Monoid e) => Conduit a m (Either e b) -> a -> m (Either e b)
condTest c i = do
  pr <- yield i $$ c =$ await
  case pr of
   Nothing -> return $ Left mempty
   Just a  -> return a

nonErrorCond :: Monad m => Conduit a m (Either e a)
nonErrorCond = awaitForever (yield . Right)

condTestNE :: (Monad m, Monoid e) => Conduit a m b -> a -> m (Either e b)
condTestNE c = condTest (c =$= nonErrorCond)

parseCondTest :: Text -> IO PCQR
parseCondTest = condTest parseCond

parseCondInv :: Property
parseCondInv = monadicIO $ do
  (CQRL input) <- pick arbitrary
  inv <- run $ condTest (rprintCond' =$= parseCond) input
  assert $ inv == Right input

spec :: Spec
spec = do
  describe "parseCond" $ do
    context "when given an empty string" $ do
      it "returns [fromListCQR []]" $ do
        parseCondTest "" `shouldReturn` Right [fromListCQR []]
    context "when given an ugly-printed CQResponse" $ do
      it "returns the original CQResponse" $ do
        property parseCondInv
