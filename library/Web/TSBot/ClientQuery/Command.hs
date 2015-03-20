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
               | MSCSetClientChanGroup
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

-- | Ban-related commands
data BNCommand = BNCAdd
               | BNCClient
               | BNCDel
               | BNCDelAll
               | BNCList
               deriving (Eq, Show, Read)

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
               deriving (Eq, Show, Read)

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
               deriving (Eq, Show, Read)

-- | Complaint commands
data CMCommand = CMCAdd
               | CMCDel
               | CMCDelAll
               | CMCList
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
  CommandBN :: BNCommand -> Command BNCommand
  CommandCL :: CLCommand -> Command CLCommand
  CommandCH :: CHCommand -> Command CHCommand
  CommandCM :: CMCommand -> Command CMCommand
  CommandMG :: MGCommand -> Command MGCommand
  CommandSV :: SVCommand -> Command SVCommand
  CommandTK :: TKCommand -> Command TKCommand

-- Pretty printing

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
msCmdPretty MSCSetClientChanGroup = "setclientchannelgroup"

ftCmdPretty :: FTCommand -> Text
ftCmdPretty FTCCreateDir          = "ftcreatedir"
ftCmdPretty FTCDeleteFile         = "ftdeletefile"
ftCmdPretty FTCGetFileInfo        = "ftgetfileinfo"
ftCmdPretty FTCGetFileList        = "ftgetfilelist"
ftCmdPretty FTCInitDownload       = "ftinitdownload"
ftCmdPretty FTCInitUpload         = "ftinitupload"
ftCmdPretty FTCList               = "ftlist"
ftCmdPretty FTCRenameFile         = "ftrenamefile"
ftCmdPretty FTCStop               = "ftstop"

bnCmdPretty :: BNCommand -> Text
bnCmdPretty BNCAdd                = "banadd"
bnCmdPretty BNCClient             = "banclient"
bnCmdPretty BNCDel                = "bandel"
bnCmdPretty BNCDelAll             = "bandelall"
bnCmdPretty BNCList               = "banlist"

clCmdPretty :: CLCommand -> Text
clCmdPretty CLCAddPerm            = "clientaddperm"
clCmdPretty CLCDbDelete           = "clientdbdelete"
clCmdPretty CLCDbEdit             = "clientdbedit"
clCmdPretty CLCDbList             = "clientdblist"
clCmdPretty CLCDelPerm            = "clientdelperm"
clCmdPretty CLCGetDbIdFromUId     = "clientgetdbidfromuid"
clCmdPretty CLCGetIds             = "clientgetids"
clCmdPretty CLCGetNameFromDbId    = "clientgetnamefromdbid"
clCmdPretty CLCGetNameFromUId     = "clientgetnamefromuid"
clCmdPretty CLCGetUIdFromClId     = "clientgetuidfromclid"
clCmdPretty CLCKick               = "clientkick"
clCmdPretty CLCList               = "clientlist"
clCmdPretty CLCMove               = "clientmove"
clCmdPretty CLCMute               = "clientmute"
clCmdPretty CLCUnmute             = "clientunmute"
clCmdPretty CLCNotifyRegister     = "clientnotifyregister"
clCmdPretty CLCNotifyUnregister   = "clientnotifyunregister"
clCmdPretty CLCPermList           = "clientpermlist"
clCmdPretty CLCPoke               = "clientpoke"
clCmdPretty CLCUpdate             = "clientupdate"
clCmdPretty CLCVariable           = "clientvariable"

chCmdPretty :: CHCommand -> Text
chCmdPretty CHCAddPerm            = "channeladdperm"
chCmdPretty CHCClientAddPerm      = "channelclientaddperm"
chCmdPretty CHCClientDelPerm      = "channelclientdelperm"
chCmdPretty CHCClientPermList     = "channelclientpermlist"
chCmdPretty CHCConnectInfo        = "channelconnectinfo"
chCmdPretty CHCCreate             = "channelcreate"
chCmdPretty CHCDelete             = "channeldelete"
chCmdPretty CHCDelPerm            = "channeldelperm"
chCmdPretty CHCEdit               = "channeledit"
chCmdPretty CHCGroupAdd           = "channelgroupadd"
chCmdPretty CHCGroupAddPerm       = "channelgroupaddperm"
chCmdPretty CHCGroupClientList    = "channelgroupclientlist"
chCmdPretty CHCGroupDel           = "channelgroupdel"
chCmdPretty CHCGroupDelPerm       = "channelgroupdelperm"
chCmdPretty CHCGroupList          = "channelgrouplist"
chCmdPretty CHCGroupPermList      = "channelgrouppermlist"
chCmdPretty CHCList               = "channellist"
chCmdPretty CHCMove               = "channelmove"
chCmdPretty CHCPermList           = "channelpermlist"
chCmdPretty CHCVariable           = "channelvariable"

cmCmdPretty :: CMCommand -> Text
cmCmdPretty CMCAdd                = "complainadd"
cmCmdPretty CMCDel                = "complaindel"
cmCmdPretty CMCDelAll             = "complaindelall"
cmCmdPretty CMCList               = "complainlist"

mgCmdPretty :: MGCommand -> Text
mgCmdPretty MGCAdd                = "messageadd"
mgCmdPretty MGCDel                = "messagedel"
mgCmdPretty MGCGet                = "messageget"
mgCmdPretty MGCList               = "messagelist"
mgCmdPretty MGCUpdateFlag         = "messageupdateflag"

svCmdPretty :: SVCommand -> Text
svCmdPretty SVCVariable           = "servervariable"
svCmdPretty SVCConnectInfo        = "serverconnectinfo"
svCmdPretty SVCConnHandlerList    = "serverconnectionhandlerlist"
svCmdPretty SVCGroupAdd           = "servergroupadd"
svCmdPretty SVCGroupAddClient     = "servergroupaddclient"
svCmdPretty SVCGroupAddPerm       = "servergroupaddperm"
svCmdPretty SVCGroupDel           = "servergroupdel"
svCmdPretty SVCGroupDelClient     = "servergroupdelclient"
svCmdPretty SVCGroupDelPerm       = "servergroupdelperm"
svCmdPretty SVCGroupClientList    = "servergroupclientlist"
svCmdPretty SVCGroupList          = "servergrouplist"
svCmdPretty SVCGroupPermList      = "servergrouppermlist"
svCmdPretty SVCGroupsByClientId   = "servergroupsbyclientid"

tkCmdPretty :: TKCommand -> Text
tkCmdPretty TKCAdd                = "tokenadd"
tkCmdPretty TKCDel                = "tokendelete"
tkCmdPretty TKCList               = "tokenlist"
tkCmdPretty TKCUse                = "tokenuse"

-- | Pretty print a command
cmdPretty :: Command a -> Text
cmdPretty (CommandMS c) = msCmdPretty c
cmdPretty (CommandFT c) = ftCmdPretty c
cmdPretty (CommandBN c) = bnCmdPretty c
cmdPretty (CommandCM c) = cmCmdPretty c
cmdPretty (CommandCL c) = clCmdPretty c
cmdPretty (CommandCH c) = chCmdPretty c
cmdPretty (CommandMG c) = mgCmdPretty c
cmdPretty (CommandSV c) = svCmdPretty c
cmdPretty (CommandTK c) = tkCmdPretty c
