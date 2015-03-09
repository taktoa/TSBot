{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO
module TSBot.ClientQuery.Parse ( CQResponse (..)
                               , CQValue (..)
                               , RName (..)
                               , AName (..)
                               , responseP
                               ) where

import           Control.Applicative  ((*>), (<*), (<**>), (<*>), (<|>))
import           Data.Attoparsec.Text
import           Data.ByteString
import           Data.Functor         ((<$>))
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T

newtype RName = RName  Text deriving (Eq, Ord, Show)
newtype AName = AName  Text deriving (Eq, Ord, Show)

data CQValue  = CQVStr Text
              | CQVBool Bool
              | CQVInt  Int
                deriving (Eq, Show)

data CQResponse = CQResponse { cqName  :: RName
                             , cqAttrs :: Map AName CQValue
                             }
                  deriving (Eq, Show)

letters :: Parser String
letters = many1 letter

lettersT :: Parser Text
lettersT = T.pack <$> letters

cqvP :: Parser CQValue
cqvP =     (CQVBool <$> cqvBoolP)
       <|> (CQVInt  <$> cqvIntP)
       <|> (CQVStr  <$> cqvStrP)
  where
    cqvBoolP =     ((string "true")  *> return True)
               <|> ((string "false") *> return False)
    cqvIntP = decimal
    cqvStrP = lettersT

rnameP :: Parser RName
rnameP = RName <$> lettersT

anameP :: Parser AName
anameP = AName <$> lettersT

attrP :: Parser (AName, CQValue)
attrP = do
  an  <- anameP
  _   <- char '='
  cqv <- cqvP
  return (an, cqv)

responseP' :: Parser CQResponse
responseP' = do
  cqn <- rnameP
  atr <- many' (skipSpace >> attrP)
  return $ CQResponse cqn $ M.fromList atr

responseP :: Parser (Either Text CQResponse)
responseP = do
  res <- ((try (Right <$> responseP')) <|> (Left  <$> takeText))
  _ <- endOfLine
  return res
