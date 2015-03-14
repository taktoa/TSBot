{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | TODO
module Web.TSBot.ClientQuery.Parse (responseP) where

import           Control.Applicative            ((*>), (<*), (<*>), (<|>))
import           Control.Monad                  (MonadPlus (..))
import           Data.Attoparsec.Text           as AP hiding (take)
import           Data.Char                      (isSpace)
import           Data.Functor                   ((<$>))
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Web.TSBot.ClientQuery.Escape
import           Web.TSBot.ClientQuery.Response

cqvP :: Parser CQValue
cqvP =     (CQVBool <$> cqvBoolP)
       <|> (CQVInt  <$> cqvIntP)
       <|> (CQVStr  <$> cqvStrP)
  where
    cqvBoolP =     (string "true"  *> return True)
               <|> (string "false" *> return False)
    cqvIntP = decimal
    isEsc x = isSpace x || (x == '|')
    cqvStrP = unescape <$> AP.takeWhile (not . isEsc)

anameP :: Parser AName
anameP = AName <$> T.pack <$> many1 (char '_' <|> letter)

attrP :: Parser (AName, CQValue)
attrP = nnAttrP <|> nilP
  where
    nilP = (,CQVNil) <$> anameP
    nnAttrP = do
      an  <- anameP
      _   <- char '='
      cqv <- cqvP
      return (an, cqv)

sepBy0 :: Parser a -> Parser s -> Parser [a]
sepBy0 val sep = (++) <$> many' (val <* sep) <*> ((return <$> val) <|> return [])

cqrP :: Parser CQR
cqrP = do
  attrs <- attrP `sepBy0` many1 (char ' ')
  return . CQR $ M.fromList attrs

-- | A parser for 'CQResponse's
responseP :: Parser CQResponse
responseP = do
  r <- cqrP `sepBy0` char '|'
  _ <- many' endOfLine
  return r
