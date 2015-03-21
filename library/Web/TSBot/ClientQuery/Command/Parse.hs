{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- | Parse commands
module Web.TSBot.ClientQuery.Command.Parse where

import           Prelude                       hiding (id, (.))

import           Control.Applicative           ((*>), (<*), (<**>), (<*>),
                                                (<|>))
import           Control.Category              (Category, id, (.))
import           Control.Monad                 (forever)
import           Data.Data
import           Data.Functor                  ((<$>))
import           Data.Monoid                   (Monoid)
import           Data.String                   (IsString)
import           Data.Text                     (Text, pack, unpack)
import           Language.Haskell.TH           (Exp (..), Pat (..), Q (..))
import qualified Language.Haskell.TH           as TH
import           Language.Haskell.TH.Quote
import           Text.Boomerang
import           Text.Boomerang.String
import           Text.Boomerang.TH

import           Web.TSBot.ClientQuery.Command

$(makeBoomerangs ''MSCommand)
$(makeBoomerangs ''FTCommand)
$(makeBoomerangs ''BNCommand)
$(makeBoomerangs ''CLCommand)
$(makeBoomerangs ''CHCommand)
$(makeBoomerangs ''CMCommand)
$(makeBoomerangs ''MGCommand)
$(makeBoomerangs ''SVCommand)
$(makeBoomerangs ''TKCommand)

qqComE :: String -> Q Exp
qqComE s = case commandParse $ pack s of
            Left  a -> fail $ show a
            Right o -> dataToExpQ (const Nothing) o

msP :: StringBoomerang () (MSCommand :- ())
msP = toP [ (rMSCHelp,               "help")
          , (rMSCQuit,               "quit")
          , (rMSCUse,                "use")
          , (rMSCCurrentSCHandlerId, "currentschandlerid")
          , (rMSCHashPassword,       "hashpassword")
          , (rMSCPermOverview,       "permoverview")
          , (rMSCSendTextMessage,    "sendtextmessage")
          , (rMSCWhoAmI,             "whoami")
          , (rMSCVerifyChannelPW,    "verifychannelpassword")
          , (rMSCVerifyServerPW,     "verifyserverpassword")
          , (rMSCSetClientChanGroup, "setclientchannelgroup")
          ]

ftP :: StringBoomerang () (FTCommand :- ())
ftP = toP [ (rFTCCreateDir,          "ftcreatedir")
          , (rFTCDeleteFile,         "ftdeletefile")
          , (rFTCGetFileInfo,        "ftgetfileinfo")
          , (rFTCGetFileList,        "ftgetfilelist")
          , (rFTCInitDownload,       "ftinitdownload")
          , (rFTCInitUpload,         "ftinitupload")
          , (rFTCList,               "ftlist")
          , (rFTCRenameFile,         "ftrenamefile")
          , (rFTCStop,               "ftstop")
          ]

bnP :: StringBoomerang () (BNCommand :- ())
bnP = toP [ (rBNCAdd,                "banadd")
          , (rBNCClient,             "banclient")
          , (rBNCDel,                "bandel")
          , (rBNCDelAll,             "bandelall")
          , (rBNCList,               "banlist")
          ]

clP :: StringBoomerang () (CLCommand :- ())
clP = toP [ (rCLCAddPerm,            "clientaddperm")
          , (rCLCDbDelete,           "clientdbdelete")
          , (rCLCDbEdit,             "clientdbedit")
          , (rCLCDbList,             "clientdblist")
          , (rCLCDelPerm,            "clientdelperm")
          , (rCLCGetDbIdFromUId,     "clientgetdbidfromuid")
          , (rCLCGetIds,             "clientgetids")
          , (rCLCGetNameFromDbId,    "clientgetnamefromdbid")
          , (rCLCGetNameFromUId,     "clientgetnamefromuid")
          , (rCLCGetUIdFromClId,     "clientgetuidfromclid")
          , (rCLCKick,               "clientkick")
          , (rCLCList,               "clientlist")
          , (rCLCMove,               "clientmove")
          , (rCLCMute,               "clientmute")
          , (rCLCUnmute,             "clientunmute")
          , (rCLCNotifyRegister,     "clientnotifyregister")
          , (rCLCNotifyUnregister,   "clientnotifyunregister")
          , (rCLCPermList,           "clientpermlist")
          , (rCLCPoke,               "clientpoke")
          , (rCLCUpdate,             "clientupdate")
          , (rCLCVariable,           "clientvariable")
          ]

chP :: StringBoomerang () (CHCommand :- ())
chP = toP [ (rCHCAddPerm,            "channeladdperm")
          , (rCHCClientAddPerm,      "channelclientaddperm")
          , (rCHCClientDelPerm,      "channelclientdelperm")
          , (rCHCClientPermList,     "channelclientpermlist")
          , (rCHCConnectInfo,        "channelconnectinfo")
          , (rCHCCreate,             "channelcreate")
          , (rCHCDelete,             "channeldelete")
          , (rCHCDelPerm,            "channeldelperm")
          , (rCHCEdit,               "channeledit")
          , (rCHCGroupAdd,           "channelgroupadd")
          , (rCHCGroupAddPerm,       "channelgroupaddperm")
          , (rCHCGroupClientList,    "channelgroupclientlist")
          , (rCHCGroupDel,           "channelgroupdel")
          , (rCHCGroupDelPerm,       "channelgroupdelperm")
          , (rCHCGroupList,          "channelgrouplist")
          , (rCHCGroupPermList,      "channelgrouppermlist")
          , (rCHCList,               "channellist")
          , (rCHCMove,               "channelmove")
          , (rCHCPermList,           "channelpermlist")
          , (rCHCVariable,           "channelvariable")
          ]

cmP :: StringBoomerang () (CMCommand :- ())
cmP = toP [ (rCMCAdd,                "complainadd")
          , (rCMCDel,                "complaindel")
          , (rCMCDelAll,             "complaindelall")
          , (rCMCList,               "complainlist")
          ]

mgP :: StringBoomerang () (MGCommand :- ())
mgP = toP [ (rMGCAdd,                "messageadd")
          , (rMGCDel,                "messagedel")
          , (rMGCGet,                "messageget")
          , (rMGCList,               "messagelist")
          , (rMGCUpdateFlag,         "messageupdateflag")
          ]

svP :: StringBoomerang () (SVCommand :- ())
svP = toP [ (rSVCVariable,           "servervariable")
          , (rSVCConnectInfo,        "serverconnectinfo")
          , (rSVCConnHandlerList,    "serverconnectionhandlerlist")
          , (rSVCGroupAdd,           "servergroupadd")
          , (rSVCGroupAddClient,     "servergroupaddclient")
          , (rSVCGroupAddPerm,       "servergroupaddperm")
          , (rSVCGroupDel,           "servergroupdel")
          , (rSVCGroupDelClient,     "servergroupdelclient")
          , (rSVCGroupDelPerm,       "servergroupdelperm")
          , (rSVCGroupClientList,    "servergroupclientlist")
          , (rSVCGroupList,          "servergrouplist")
          , (rSVCGroupPermList,      "servergrouppermlist")
          , (rSVCGroupsByClientId,   "servergroupsbyclientid")
          ]

tkP :: StringBoomerang () (TKCommand :- ())
tkP = toP [ (rTKCAdd,                "tokenadd")
          , (rTKCDel,                "tokendelete")
          , (rTKCList,               "tokenlist")
          , (rTKCUse,                "tokenuse")
          ]

toP :: (Category cat, Monoid (cat a c)) => [(cat b c, cat a b)] -> cat a c
toP x = foldr1 (<>) $ map (uncurry (.)) x

comP :: IsCommand a => StringBoomerang () (Command a :- ())
comP = xmaph CommandMS unCommandMS msP <>
       xmaph CommandFT unCommandFT ftP <>
       xmaph CommandBN unCommandBN bnP <>
       xmaph CommandCM unCommandCM cmP <>
       xmaph CommandCL unCommandCL clP <>
       xmaph CommandCH unCommandCH chP <>
       xmaph CommandMG unCommandMG mgP <>
       xmaph CommandSV unCommandSV svP <>
       xmaph CommandTK unCommandTK tkP

commandParse :: IsCommand a => Text -> Either StringError (Command a)
commandParse = parseString comP . unpack

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) x = f <$> g x

commandPretty :: Command a -> Maybe Text
commandPretty = pack <.> unparseString comP

unCommandMS :: IsCommand a => Command a -> Maybe MSCommand
unCommandMS (CommandMS t) = Just t
unCommandMS _             = Nothing

unCommandFT :: IsCommand a => Command a -> Maybe FTCommand
unCommandFT (CommandFT t) = Just t
unCommandFT _             = Nothing

unCommandBN :: IsCommand a => Command a -> Maybe BNCommand
unCommandBN (CommandBN t) = Just t
unCommandBN _             = Nothing

unCommandCM :: IsCommand a => Command a -> Maybe CMCommand
unCommandCM (CommandCM t) = Just t
unCommandCM _             = Nothing

unCommandCL :: IsCommand a => Command a -> Maybe CLCommand
unCommandCL (CommandCL t) = Just t
unCommandCL _             = Nothing

unCommandCH :: IsCommand a => Command a -> Maybe CHCommand
unCommandCH (CommandCH t) = Just t
unCommandCH _             = Nothing

unCommandMG :: IsCommand a => Command a -> Maybe MGCommand
unCommandMG (CommandMG t) = Just t
unCommandMG _             = Nothing

unCommandSV :: IsCommand a => Command a -> Maybe SVCommand
unCommandSV (CommandSV t) = Just t
unCommandSV _             = Nothing

unCommandTK :: IsCommand a => Command a -> Maybe TKCommand
unCommandTK (CommandTK t) = Just t
unCommandTK _             = Nothing



-- MS, FT, BN, CM, CL, CH, MG, SV, TK


{-

StringBoomerang = Boomerang StringError String

(a -> b)
-> Boomerang e tok () (a :- ())
-> Boomerang e tok () (b :- ())
-}

{-
msParse :: Parser MSCommand
msParse =     (string "sendtextmessage" *> return MSCSendTextMessage)
          <|> (string "sendtextmessage" *> return MSCSendTextMessage)

ftParse :: Parser FTCommand
ftParse = undefined

bnParse :: Parser BNCommand
bnParse = undefined

cmParse :: Parser CMCommand
cmParse = undefined

clParse :: Parser CLCommand
clParse = undefined

chParse :: Parser CHCommand
chParse = undefined

mgParse :: Parser MGCommand
mgParse = undefined

svParse :: Parser SVCommand
svParse = undefined

tkParse :: Parser TKCommand
tkParse = undefined

cmdParse :: Parser (Command a)
cmdParse =     CommandMS <$> msParse
           <|> CommandFT <$> ftParse
           <|> CommandBN <$> bnParse
           <|> CommandCM <$> cmParse
           <|> CommandCL <$> clParse
           <|> CommandCH <$> chParse
           <|> CommandMG <$> mgParse
           <|> CommandSV <$> svParse
           <|> CommandTK <$> tkParse
-}
