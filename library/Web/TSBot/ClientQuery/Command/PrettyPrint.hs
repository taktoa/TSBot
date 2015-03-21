{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs #-}

-- | Pretty print commands
module Web.TSBot.ClientQuery.Command.PrettyPrint (cmdPretty) where

import           Data.Text                     (Text)
import           Web.TSBot.ClientQuery.Command

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
