{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | ClientQuery command types
module Web.TSBot.ClientQuery.Command
       ( IsCommand
       , Command (..)
       , MSCommand (..)
       , FTCommand (..)
       , BNCommand (..)
       , CMCommand (..)
       , CLCommand (..)
       , CHCommand (..)
       , MGCommand (..)
       , SVCommand (..)
       , TKCommand (..)
       ) where

import           Data.Data

-- | Miscellaneous commands
data MSCommand = MSCHelp
               | MSCQuit
               | MSCUse
               | MSCCurrentSCHandlerId
               | MSCHashPassword
               | MSCPermOverview
               | MSCSendTextMessage
               | MSCWhoAmI
               | MSCVerifyChannelPW
               | MSCVerifyServerPW
               | MSCSetClientChanGroup
               deriving (Eq, Show, Read, Data, Typeable)

-- | File transfer commands
data FTCommand = FTCCreateDir
               | FTCDeleteFile
               | FTCGetFileInfo
               | FTCGetFileList
               | FTCInitDownload
               | FTCInitUpload
               | FTCList
               | FTCRenameFile
               | FTCStop
               deriving (Eq, Show, Read, Data, Typeable)

-- | Ban-related commands
data BNCommand = BNCAdd
               | BNCClient
               | BNCDel
               | BNCDelAll
               | BNCList
               deriving (Eq, Show, Read, Data, Typeable)

-- | Client commands
data CLCommand = CLCAddPerm
               | CLCDbDelete
               | CLCDbEdit
               | CLCDbList
               | CLCDelPerm
               | CLCGetDbIdFromUId
               | CLCGetIds
               | CLCGetNameFromDbId
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
data CHCommand = CHCAddPerm
               | CHCClientAddPerm
               | CHCClientDelPerm
               | CHCClientPermList
               | CHCConnectInfo
               | CHCCreate
               | CHCDelete
               | CHCDelPerm
               | CHCEdit
               | CHCGroupAdd
               | CHCGroupAddPerm
               | CHCGroupClientList
               | CHCGroupDel
               | CHCGroupDelPerm
               | CHCGroupList
               | CHCGroupPermList
               | CHCList
               | CHCMove
               | CHCPermList
               | CHCVariable
               deriving (Eq, Show, Read, Data, Typeable)

-- | Complaint commands
data CMCommand = CMCAdd
               | CMCDel
               | CMCDelAll
               | CMCList
               deriving (Eq, Show, Read, Data, Typeable)

-- | Message commands
data MGCommand = MGCAdd
               | MGCDel
               | MGCGet
               | MGCList
               | MGCUpdateFlag
               deriving (Eq, Show, Read, Data, Typeable)

-- | Server commands
data SVCommand = SVCVariable
               | SVCConnectInfo
               | SVCConnHandlerList
               | SVCGroupAdd
               | SVCGroupAddClient
               | SVCGroupAddPerm
               | SVCGroupDel
               | SVCGroupDelClient
               | SVCGroupDelPerm
               | SVCGroupClientList
               | SVCGroupList
               | SVCGroupPermList
               | SVCGroupsByClientId
               deriving (Eq, Show, Read, Data, Typeable)

-- | Token commands
data TKCommand = TKCAdd
               | TKCDel
               | TKCList
               | TKCUse
               deriving (Eq, Show, Read, Data, Typeable)

class (Typeable a, Data a) => IsCommand a where {}

instance IsCommand MSCommand
instance IsCommand FTCommand
instance IsCommand BNCommand
instance IsCommand CLCommand
instance IsCommand CHCommand
instance IsCommand CMCommand
instance IsCommand MGCommand
instance IsCommand SVCommand
instance IsCommand TKCommand

-- | Command GADT
data Command a where
  CommandMS :: MSCommand -> Command a
  CommandFT :: FTCommand -> Command a
  CommandBN :: BNCommand -> Command a
  CommandCL :: CLCommand -> Command a
  CommandCH :: CHCommand -> Command a
  CommandCM :: CMCommand -> Command a
  CommandMG :: MGCommand -> Command a
  CommandSV :: SVCommand -> Command a
  CommandTK :: TKCommand -> Command a

deriving instance Typeable Command

deriving instance Data a => Data (Command a)

instance Show (Command a) where
  show (CommandMS t) = show t
  show (CommandFT t) = show t
  show (CommandBN t) = show t
  show (CommandCL t) = show t
  show (CommandCH t) = show t
  show (CommandCM t) = show t
  show (CommandMG t) = show t
  show (CommandSV t) = show t
  show (CommandTK t) = show t
  show _             = error "Failed pattern match when showing a (Command a)"
