{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs #-}

-- | ClientQuery command types
module Web.TSBot.ClientQuery.Command
       ( Command (..)
       , MSCommand (..)
       , FTCommand (..)
       , MGCommand (..)
       , SVCommand (..)
       , TKCommand (..)
       , cmdPretty
       ) where

import           Data.Text (Text)

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
               deriving (Eq, Show, Read)

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
               deriving (Eq, Show, Read)

-- | Message commands
data MGCommand = MGCAdd
               | MGCDel
               | MGCGet
               | MGCList
               | MGCUpdateFlag
               deriving (Eq, Show, Read)

-- | Server commands
data SVCommand = SVCVariable
               | SVCConnectInfo
               | SVCConnectionHandlerList
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
               deriving (Eq, Show, Read)

-- | Token commands
data TKCommand = TKCAdd
               | TKCDel
               | TKCList
               | TKCUse
               deriving (Eq, Show, Read)

data Command a where
  CommandMS :: MSCommand -> Command MSCommand
  CommandFT :: FTCommand -> Command FTCommand
  CommandMG :: MGCommand -> Command MGCommand
  CommandSV :: SVCommand -> Command SVCommand
  CommandTK :: TKCommand -> Command TKCommand

msCmdPretty :: MSCommand -> Text
msCmdPretty MSCHelp               = "help"
msCmdPretty MSCQuit               = "quit"
msCmdPretty MSCUse                = "use"
msCmdPretty MSCCurrentSCHandlerId = "currentschandlerid"
msCmdPretty MSCHashPassword       = "hashpassword"
msCmdPretty MSCPermOverview       = "permoverview"
msCmdPretty MSCSendTextMessage    = "sendtextmessage"
msCmdPretty MSCWhoAmI             = "whoami"
msCmdPretty MSCVerifyChannelPW    = "verifychannelpassword"
msCmdPretty MSCVerifyServerPW     = "verifyserverpassword"

ftCmdPretty :: FTCommand -> Text
ftCmdPretty FTCCreateDir    = "ftcreatedir"
ftCmdPretty FTCDeleteFile   = "ftdeletefile"
ftCmdPretty FTCGetFileInfo  = "ftgetfileinfo"
ftCmdPretty FTCGetFileList  = "ftgetfilelist"
ftCmdPretty FTCInitDownload = "ftinitdownload"
ftCmdPretty FTCInitUpload   = "ftinitupload"
ftCmdPretty FTCList         = "ftlist"
ftCmdPretty FTCRenameFile   = "ftrenamefile"
ftCmdPretty FTCStop         = "ftstop"

mgCmdPretty :: MGCommand -> Text
mgCmdPretty MGCAdd        = "messageadd"
mgCmdPretty MGCDel        = "messagedel"
mgCmdPretty MGCGet        = "messageget"
mgCmdPretty MGCList       = "messagelist"
mgCmdPretty MGCUpdateFlag = "messageupdateflag"

svCmdPretty :: SVCommand -> Text
svCmdPretty SVCVariable              = "servervariable"
svCmdPretty SVCConnectInfo           = "serverconnectinfo"
svCmdPretty SVCConnectionHandlerList = "serverconnectionhandlerlist"
svCmdPretty SVCGroupAdd              = "servergroupadd"
svCmdPretty SVCGroupAddClient        = "servergroupaddclient"
svCmdPretty SVCGroupAddPerm          = "servergroupaddperm"
svCmdPretty SVCGroupDel              = "servergroupdel"
svCmdPretty SVCGroupDelClient        = "servergroupdelclient"
svCmdPretty SVCGroupDelPerm          = "servergroupdelperm"
svCmdPretty SVCGroupClientList       = "servergroupclientlist"
svCmdPretty SVCGroupList             = "servergrouplist"
svCmdPretty SVCGroupPermList         = "servergrouppermlist"
svCmdPretty SVCGroupsByClientId      = "servergroupsbyclientid"

tkCmdPretty :: TKCommand -> Text
tkCmdPretty TKCAdd  = "tokenadd"
tkCmdPretty TKCDel  = "tokendelete"
tkCmdPretty TKCList = "tokenlist"
tkCmdPretty TKCUse  = "tokenuse"

-- | Pretty print a command
cmdPretty :: Command a -> Text
cmdPretty (CommandMS c) = msCmdPretty c
cmdPretty (CommandFT c) = ftCmdPretty c
cmdPretty (CommandMG c) = mgCmdPretty c
cmdPretty (CommandSV c) = svCmdPretty c
cmdPretty (CommandTK c) = tkCmdPretty c
