{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The ClientQuery response ADT
module Web.TSBot.ClientQuery.Response
       ( CQR (..)
       , CQValue (..)
       , AName (..)
       , CQResponse
       , toList
       , fromList
       , retrieve
       , insert
       )
       where

import           Data.Map  (Map)
import qualified Data.Map  as M (fromList, insert, lookup, toList)
import           Data.Text (Text)

-- | An attribute name
newtype AName = AName Text deriving (Eq, Ord, Show)

-- | A value stored in an attribute
data CQValue  = CQVStr  Text
              | CQVBool Bool
              | CQVInt  Int
              | CQVNil
                deriving (Eq, Show)

-- | A map from attribute names to values
data CQR = CQR { unCQR :: Map AName CQValue }
         deriving (Eq, Show)

-- | Type alias for a list of CQR
type CQResponse = [CQR]

-- Should probably figure out how to just make these functions
-- Traversable/Foldable instances or whatever

-- | Convert a CQR to a list of pairs of attribute names and values
toList :: CQR -> [(AName, CQValue)]
toList = M.toList . unCQR

-- | Convert a list of pairs of attribute names and values to a CQR
fromList :: [(AName, CQValue)] -> CQR
fromList = CQR . M.fromList

-- | Lookup a value based on its attribute name
retrieve :: AName -> CQR -> Maybe CQValue
retrieve a (CQR m) = M.lookup a m

-- | Update a CQR with a new value
insert :: AName -> CQValue -> CQR -> CQR
insert a v (CQR m) = CQR $ M.insert a v m
