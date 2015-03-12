{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO
module Web.TSBot.ClientQuery.PrettyPrint (resPretty) where

import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Web.TSBot.ClientQuery.Response

cqvPretty :: CQValue -> String
cqvPretty (CQVNil) = error "GHC is broken"
cqvPretty (CQVStr s) = "\"" <> T.unpack s <> "\""
cqvPretty (CQVInt i) = show i
cqvPretty (CQVBool True) = "true"
cqvPretty (CQVBool False) = "false"

attrPretty :: AName -> CQValue -> String
attrPretty (AName a) CQVNil = T.unpack a
attrPretty (AName a) c = T.unpack a <> " => " <> cqvPretty c

cqrPretty :: CQR -> String
cqrPretty = wrapPretty . fmap (uncurry attrPretty) . toList
  where
    sep = ",\n   "
    ls = length sep
    wrapPretty' i = let s = concatMap (<> sep) i
                    in  take (length s - ls) s
    wrapPretty i = "{ " <> wrapPretty' i <> " }"

resPretty' :: CQResponse -> String
resPretty' []     = []
resPretty' [c]    = "{" <> cqrPretty c <> "}"
resPretty' (c:cs) = "{" <> first <> "\n" <> rest <> "}"
  where
    rp r = " " <> cqrPretty r <> "\n"
    first = cqrPretty c
    rest = concatMap rp cs

-- | Pretty print a 'CQResponse'
resPretty :: CQResponse -> Text
resPretty = T.pack . resPretty'
