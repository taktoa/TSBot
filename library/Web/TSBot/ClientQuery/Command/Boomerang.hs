{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- | Parse commands
module Web.TSBot.ClientQuery.Command.Boomerang
       ( commandParse
       , commandPretty
       ) where

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
import           Text.Boomerang
import           Text.Boomerang.String
import           Text.Boomerang.TH

import           Web.TSBot.ClientQuery.Command

$(makeBoomerangs ''MSCommand)
$(makeBoomerangs ''BNCommand)
$(makeBoomerangs ''CLCommand)
$(makeBoomerangs ''CHCommand)
$(makeBoomerangs ''SVCommand)

msP :: StringBoomerang () (MSCommand :- ())
msP = toP [ (rMSCHelp,               "help")
          , (rMSCQuit,               "quit")
          , (rMSCUse,                "use")
          , (rMSCCurrentSCHandlerId, "currentschandlerid")
          , (rMSCSendTextMessage,    "sendtextmessage")
          , (rMSCWhoAmI,             "whoami")
          ]

bnP :: StringBoomerang () (BNCommand :- ())
bnP = toP [ (rBNCAdd,                "banadd")
          , (rBNCDel,                "bandel")
          , (rBNCDelAll,             "bandelall")
          , (rBNCList,               "banlist")
          ]

clP :: StringBoomerang () (CLCommand :- ())
clP = toP [ (rCLCGetDbIdFromUId,     "clientgetdbidfromuid")
          , (rCLCGetIds,             "clientgetids")
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
chP = toP [ (rCHCConnectInfo,        "channelconnectinfo")
          , (rCHCCreate,             "channelcreate")
          , (rCHCDelete,             "channeldelete")
          , (rCHCEdit,               "channeledit")
          , (rCHCList,               "channellist")
          , (rCHCMove,               "channelmove")
          , (rCHCVariable,           "channelvariable")
          ]

svP :: StringBoomerang () (SVCommand :- ())
svP = toP [ (rSVCVariable,           "servervariable")
          , (rSVCConnectInfo,        "serverconnectinfo")
          , (rSVCConnHandlerList,    "serverconnectionhandlerlist")
          ]

toP :: (Category cat, Monoid (cat a c)) => [(cat b c, cat a b)] -> cat a c
toP x = foldr1 (<>) $ map (uncurry (.)) x

comP :: StringBoomerang () (Command a :- ())
comP = xmaph CommandMS unCommandMS msP <>
       xmaph CommandBN unCommandBN bnP <>
       xmaph CommandCL unCommandCL clP <>
       xmaph CommandCH unCommandCH chP <>
       xmaph CommandSV unCommandSV svP

commandParse :: Text -> Either StringError (Command a)
commandParse = parseString comP . unpack

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) x = f <$> g x

commandPretty :: Command a -> Maybe Text
commandPretty = pack <.> unparseString comP

unCommandMS :: Command a -> Maybe MSCommand
unCommandMS (CommandMS t) = Just t
unCommandMS _             = Nothing

unCommandBN :: Command a -> Maybe BNCommand
unCommandBN (CommandBN t) = Just t
unCommandBN _             = Nothing

unCommandCL :: Command a -> Maybe CLCommand
unCommandCL (CommandCL t) = Just t
unCommandCL _             = Nothing

unCommandCH :: Command a -> Maybe CHCommand
unCommandCH (CommandCH t) = Just t
unCommandCH _             = Nothing

unCommandSV :: Command a -> Maybe SVCommand
unCommandSV (CommandSV t) = Just t
unCommandSV _             = Nothing
