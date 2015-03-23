{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Pretty print commands
module Web.TSBot.ClientQuery.Command.Quote (com) where

import           Data.Text                               (Text, pack, unpack)
import           Language.Haskell.TH                     (Exp (..), Pat (..), Q)
import qualified Language.Haskell.TH                     as TH
import           Language.Haskell.TH.Quote

import           Web.TSBot.ClientQuery.Command
import           Web.TSBot.ClientQuery.Command.Boomerang

com :: QuasiQuoter
com = QuasiQuoter { quoteExp = qqComE
                  , quotePat = qqComP
                  }

qqComE :: String -> Q Exp
qqComE s = case commandParse $ pack s of
            Left  a -> fail $ show a
            Right o -> case o of
              CommandMS c -> toExp c
              CommandFT c -> toExp c
              CommandBN c -> toExp c
              CommandCL c -> toExp c
              CommandCH c -> toExp c
              CommandCM c -> toExp c
              CommandMG c -> toExp c
              CommandSV c -> toExp c
              CommandTK c -> toExp c
  where
    toExp = dataToExpQ (const Nothing)

qqComP :: String -> Q Pat
qqComP s = case commandParse $ pack s of
            Left  a -> fail $ show a
            Right o -> case o of
              CommandMS c -> toPat c
              CommandFT c -> toPat c
              CommandBN c -> toPat c
              CommandCL c -> toPat c
              CommandCH c -> toPat c
              CommandCM c -> toPat c
              CommandMG c -> toPat c
              CommandSV c -> toPat c
              CommandTK c -> toPat c
  where
    toPat = dataToPatQ (const Nothing)

