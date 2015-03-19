{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | De-escape strings provided by ClientQuery, and escape those you give it
module Web.TSBot.ClientQuery.Escape (unescape, escape) where

import           Control.Applicative  ((*>), (<*), (<|>))
import           Data.Attoparsec.Text as AP hiding (take)
import           Data.Functor         ((<$>))
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T

data Esc = EEsc Char
         | EChar Char
         deriving (Eq, Show)

unescP :: Parser [Esc]
unescP = many' (unescapeP <|> (EChar <$> anyChar))
  where
    unescapeP = do
      _ <- char '\\'
      e <- anyChar
      return $ EEsc e

unescPP :: Esc -> Char
unescPP (EEsc 'n')  = '\n'
unescPP (EEsc 's')  = ' '
unescPP (EEsc 'p')  = '|'
unescPP (EEsc '/')  = '/'
unescPP (EEsc '\\') = '\\'
unescPP (EEsc c)    = error $ "unknown escaped character: " <> [c]
unescPP (EChar c)   = c

-- | Unescape the given text
unescape :: Text -> Text
unescape = T.pack . either id (fmap unescPP) . parseOnly (unescP <* endOfInput)

escP :: Parser Text
escP = T.pack . concat <$> many' escapeP
  where
    escapeP =     (char ' '  *> return "\\s")
              <|> (char '\n' *> return "\\n")
              <|> (char '|'  *> return "\\p")
              <|> (char '/'  *> return "\\/")
              <|> (char '\\' *> return "\\\\")
              <|> ((: []) <$> anyChar)

-- | Escape the given text
escape :: Text -> Text
escape = either T.pack id . parseOnly (escP <* endOfInput)
