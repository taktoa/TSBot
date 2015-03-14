{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty- and ugly-printers for 'CQResponse'
module Web.TSBot.ClientQuery.PrettyPrint (resPretty, resPrint) where

import           Data.List                      (partition)
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Web.TSBot.ClientQuery.Escape   (escape)
import           Web.TSBot.ClientQuery.Response

cqvPretty :: CQValue -> String
cqvPretty (CQVNil) = ""
cqvPretty (CQVStr s) = "\"" <> T.unpack s <> "\""
cqvPretty (CQVInt i) = show i
cqvPretty (CQVBool True) = "true"
cqvPretty (CQVBool False) = "false"

attrPretty :: Text -> CQValue -> String
attrPretty a CQVNil = T.unpack a
attrPretty a c = T.unpack a <> " => " <> cqvPretty c

cqrPretty :: CQR -> String
cqrPretty = wrapPretty . fmap (uncurry attrPretty) . toListCQR
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


cqvPrint :: CQValue -> String
cqvPrint CQVNil = ""
cqvPrint (CQVInt i) = show (i :: Int)
cqvPrint (CQVBool True) = "true"
cqvPrint (CQVBool False) = "false"
cqvPrint (CQVStr s) =  T.unpack (escape s)

attrPrint :: Text -> CQValue -> String
attrPrint a CQVNil = T.unpack a
attrPrint a c = T.unpack a <> "=" <> cqvPrint c

cqrPrint :: CQR -> String
cqrPrint c = takeExcept 1 $ concatMap ((<> " ") . uncurry attrPrint) sorted
  where
    isNil n = case n of
               (_, CQVNil) -> True
               _           -> False
    (s1, s2) = partition isNil $ toListCQR c
    sorted = s1 <> s2

resPrint :: CQResponse -> Text
resPrint = T.pack . takeExcept 1 . concatMap ((<> "|") . cqrPrint )

takeExcept :: Int -> [a] -> [a]
takeExcept _ [] = []
takeExcept i xs = take (length xs - i) xs
