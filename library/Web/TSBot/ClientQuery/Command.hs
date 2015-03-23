{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | ClientQuery command types
module Web.TSBot.ClientQuery.Command
       ( IsCommand
       , Command (..)
       , MSCommand (..)
       , BNCommand (..)
       , CLCommand (..)
       , CHCommand (..)
       , SVCommand (..)
       ) where

import           Data.Data

-- | Miscellaneous commands
data MSCommand = MSCHelp
               | MSCQuit
               | MSCUse
               | MSCCurrentSCHandlerId
               | MSCSendTextMessage
               | MSCWhoAmI
               deriving (Eq, Show, Read, Data, Typeable)

-- | Ban-related commands
data BNCommand = BNCAdd
               | BNCDel
               | BNCDelAll
               | BNCList
               deriving (Eq, Show, Read, Data, Typeable)

-- | Client commands
data CLCommand = CLCGetDbIdFromUId
               | CLCGetIds
               | CLCGetNameFromUId
               | CLCGetUIdFromClId
               | CLCKick
               | CLCList
               | CLCMove
               | CLCMute
               | CLCUnmute
               | CLCNotifyRegister
               | CLCNotifyUnregister
               | CLCPermList
               | CLCPoke
               | CLCUpdate
               | CLCVariable
               deriving (Eq, Show, Read, Data, Typeable)

-- | Channel commands
data CHCommand = CHCConnectInfo
               | CHCCreate
               | CHCDelete
               | CHCEdit
               | CHCList
               | CHCMove
               | CHCPermList
               | CHCVariable
               deriving (Eq, Show, Read, Data, Typeable)

-- | Server commands
data SVCommand = SVCVariable
               | SVCConnectInfo
               | SVCConnHandlerList
               deriving (Eq, Show, Read, Data, Typeable)

class (Typeable a, Data a) => IsCommand a where {}

instance IsCommand MSCommand
instance IsCommand BNCommand
instance IsCommand CLCommand
instance IsCommand CHCommand
instance IsCommand SVCommand

-- | Command GADT
data Command a where
  CommandMS :: MSCommand -> Command a
  CommandBN :: BNCommand -> Command a
  CommandCL :: CLCommand -> Command a
  CommandCH :: CHCommand -> Command a
  CommandSV :: SVCommand -> Command a

deriving instance Typeable Command

deriving instance Data a => Data (Command a)

instance Show (Command a) where
  show (CommandMS t) = show t
  show (CommandBN t) = show t
  show (CommandCL t) = show t
  show (CommandCH t) = show t
  show (CommandSV t) = show t
