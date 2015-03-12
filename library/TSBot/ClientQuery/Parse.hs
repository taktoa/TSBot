{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | TODO
module TSBot.ClientQuery.Parse ( CQR (..)
                               , CQValue (..)
                               , AName (..)
                               , CQResponse
                               , resP
                               , resPretty
                               , Esc (..)
                               , escP
                               ) where


import           Control.Applicative  ((*>), (<*), (<**>), (<*>), (<|>))
import           Data.Attoparsec.Text as AP hiding (take)
import qualified Data.ByteString      as B
import           Data.Char            (isSpace)
import           Data.Functor         ((<$>))
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T

newtype AName = AName  Text deriving (Eq, Ord, Show)

data CQValue  = CQVStr  Text
              | CQVBool Bool
              | CQVInt  Int
              | CQVNil
                deriving (Eq, Show)

data CQR = CQR { unCQR :: Map AName CQValue }
         deriving (Eq, Show)

type CQResponse = [CQR]

ident :: Parser String
ident = many1 $ (char '_' <|> letter)

identT :: Parser Text
identT = T.pack <$> ident

cqvP :: Parser CQValue
cqvP =     ((CQVBool <$> cqvBoolP) <?> "cqvBoolP")
       <|> ((CQVInt  <$> cqvIntP) <?> "cqvIntP")
       <|> ((CQVStr  <$> cqvStrP) <?> "cqvStrP")
  where
    cqvBoolP =     (string "true"  *> return True)
               <|> (string "false" *> return False)
    cqvIntP = decimal
    cqvStrP = unescape <$> AP.takeWhile (not . isSpace)

anameP :: Parser AName
anameP = AName <$> identT

nilP :: Parser (AName, CQValue)
nilP = (,CQVNil) <$> anameP

nnAttrP :: Parser (AName, CQValue)
nnAttrP = do
  an  <- anameP
  _   <- char '='
  cqv <- cqvP
  return (an, cqv)

attrP :: Parser (AName, CQValue)
attrP = (nnAttrP <?> "nnAttrP") <|> (nilP <?> "nilP")

whitespace :: Parser String
whitespace = many1 (char ' ')

responseP :: Parser CQR
responseP = do
  attrs <- attrP `sepBy` whitespace
  return . CQR $ M.fromList attrs

resP :: Parser CQResponse
resP = do
  r <- responseP `sepBy` char '|'
  _ <- many' endOfLine
  return r

data Esc = EEsc Char
         | EChar Char
         deriving (Eq, Show)

escP :: Parser [Esc]
escP = many' (escapeP <|> (EChar <$> anyChar))
  where
    escapeP = do
      _ <- char '\\'
      e <- anyChar
      return $ EEsc e

escPP :: Esc -> Char
escPP (EEsc 's')  = ' '
escPP (EEsc 'p')  = '|'
escPP (EEsc '/')  = '/'
escPP (EEsc '\\') = '\\'
escPP (EEsc c)    = error $ "unknown escaped character: " ++ [c]
escPP (EChar c)   = c

unescape :: Text -> Text
unescape = T.pack . either id (map escPP) . parseOnly (escP <* endOfInput)

cqvPretty :: CQValue -> String
cqvPretty (CQVNil) = error "GHC is broken"
cqvPretty (CQVStr s) = "\"" ++ T.unpack s ++ "\""
cqvPretty (CQVInt i) = show i
cqvPretty (CQVBool True) = "true"
cqvPretty (CQVBool False) = "false"

attrPretty :: AName -> CQValue -> String
attrPretty (AName a) CQVNil = T.unpack a
attrPretty (AName a) c = T.unpack a ++ " => " ++ cqvPretty c

cqrPretty :: CQR -> String
cqrPretty = wrapPretty . map (uncurry attrPretty) . M.toList . unCQR
  where
    sep = ",\n   "
    ls = length sep
    wrapPretty' i = let s = concatMap (++ sep) i
                    in  take (length s - ls) s
    wrapPretty i = "{ " ++ wrapPretty' i ++ " }"

resPretty' :: CQResponse -> String
resPretty' []     = []
resPretty' [c]    = "{" ++ cqrPretty c ++ "}"
resPretty' (c:cs) = "{" ++ first ++ "\n" ++ rest ++ "}"
  where
    rp r = " " ++ cqrPretty r ++ "\n"
    first = cqrPretty c
    rest = concatMap rp cs

resPretty :: Either String CQResponse -> Text
resPretty (Left a)  = T.pack a
resPretty (Right a) = T.pack $ resPretty' a

{-

Response parsing:
split on |
split on space
split into identifier and value
deescape value

-}
