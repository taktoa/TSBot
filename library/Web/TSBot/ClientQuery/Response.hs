{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The ClientQuery response ADT
module Web.TSBot.ClientQuery.Response
       ( CQR (..)
       , CQValue (..)
       , AName (..)
       , CQResponse
       , toListCQR
       , fromListCQR
       , retrieve
       , insert
       )
       where

import           Control.Arrow (first)
import           Data.Map      (Map)
import qualified Data.Map      as M (fromList, insert, lookup, toList)
import           Data.Monoid   ((<>))
import           Data.Text     (Text)

-- | An attribute name
newtype AName = AName { unAName :: Text } deriving (Eq, Ord, Show)

-- | A value stored in an attribute
data CQValue  = CQVStr  Text
              | CQVBool Bool
              | CQVInt  Int
              | CQVNil
                deriving (Eq, Show)

-- | A map from attribute names to values
data CQR = CQR { unCQR :: Map AName CQValue }
         deriving (Eq)

instance Show CQR where
  show m = "fromListCQR " <> show (toListCQR m)

-- | Type alias for a list of CQR
type CQResponse = [CQR]

-- Should probably figure out how to just make these functions
-- Traversable/Foldable instances or whatever

-- | Convert a CQR to a list of pairs of attribute names and values
toListCQR :: CQR -> [(Text, CQValue)]
toListCQR = map (first unAName) . M.toList . unCQR

-- | Convert a list of pairs of attribute names and values to a CQR
fromListCQR :: [(Text, CQValue)] -> CQR
fromListCQR = CQR . M.fromList . map (first AName)

-- | Lookup a value based on its attribute name
retrieve :: AName -> CQR -> Maybe CQValue
retrieve a (CQR m) = M.lookup a m

-- | Update a CQR with a new value
insert :: AName -> CQValue -> CQR -> CQR
insert a v (CQR m) = CQR $ M.insert a v m
