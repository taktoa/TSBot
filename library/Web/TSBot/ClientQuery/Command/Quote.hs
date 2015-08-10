{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
            Right o -> toExp o
  where
    toExp = dataToExpQ (const Nothing)

qqComP :: String -> Q Pat
qqComP s = case commandParse $ pack s of
            Left  a -> fail $ show a
            Right o -> toPat o
  where
    toPat = dataToPatQ (const Nothing)

