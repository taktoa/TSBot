{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The ClientQuery response ADT
module Web.TSBot.ClientQuery.Response
       ( CQR (..)
       , CQValue (..)
       , AName (..)
       , CQError (..)
       , ECQR
       , CQResponse
       , toListCQR
       , fromListCQR
       , matching, matching_
       , retrieve
       , insert
       )
       where

import           Control.Arrow (first)
import           Data.Map      (Map)
import qualified Data.Map      as M (fromList, insert, lookup, toList)
import           Data.Maybe    (isJust)
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

data CQError = CQParseError Text
             deriving (Eq, Show, Read)

type ECQR = Either CQError CQResponse

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

-- | Get all responses that include a certain (Text, CQValue)
matching :: (Text, CQValue) -> CQResponse -> CQResponse
matching a = filter (elem a . toListCQR)

-- | Get all responses that include a certain AName
matching_ :: AName -> CQResponse -> CQResponse
matching_ a = filter (isJust . retrieve a)

-- | Update a CQR with a new value
insert :: AName -> CQValue -> CQR -> CQR
insert a v (CQR m) = CQR $ M.insert a v m
