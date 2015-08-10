{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | ClientQuery command types
module Web.TSBot.ClientQuery.Command where

import           Data.Data
import           Data.Natural
import           Data.Set     (Set)
import           Data.Text    (Text, pack, unpack)

data POrdering = LT | GT | EQ | NC

class Eq a => POrd a where
  pcompare :: a -> a -> POrdering

data AType = AGetter | ASetter

class Fusible a where
  aType   :: a -> AType
  fusible :: a -> a -> Maybe a

class Render a where
  render :: a -> Text

data ChannelPropertyGet = ChPGName
                        | ChPGTopic
                        | ChPGDescription
                        | ChPGCodec
                        | ChPGCodecQuality
                        | ChPGOrder
                        | ChPGMaxClients
                        | ChPGMaxFamilyClients
                        | ChPGFlagPermanent
                        | ChPGFlagSemiPermanent
                        | ChPGFlagDefault
                        | ChPGFlagPassword
                        | ChPGFlagAreSubscribed
                        | ChPGNamePhonetic
                        | ChPGIconId

data ChannelCodec = SpeexNarrowband
                  | SpeexWideband
                  | SpeexUltrawideband
                  | CELTMono
                  | OpusVoice
                  | OpusMusic
                  deriving (Eq, Ord, Enum, Show, Read)

data ChannelCodecQuality = CCQ0 | CCQ1 | CCQ2 | CCQ3 | CCQ4
                         | CCQ5 | CCQ6 | CCQ7 | CCQ8 | CCQ9
                         | CCQ10
                         deriving (Eq, Ord, Enum, Show, Read)

newtype NNatural = NNatural (Maybe Natural)

newtype Flag = Flag Bool

data ChannelPropertySet = ChPSName              Text
                        | ChPSTopic             Text
                        | ChPSDescription       Text
                        | ChPSCodec             ChannelCodec
                        | ChPSCodecQuality      ChannelCodecQuality
                        | ChPSOrder             Natural
                        | ChPSMaxClients        NNatural
                        | ChPSMaxFamilyClients  NNatural
                        | ChPSFlagPermanent     Flag
                        | ChPSFlagSemiPermanent Flag
                        | ChPSFlagDefault       Flag
                        | ChPSFlagPassword      Flag
                        | ChPSFlagAreSubscribed Flag
                        | ChPSNamePhonetic      Text
                        | ChPSIconId            Natural

data ClientPropertyGet = ClPGNickname
                       | ClPGAway
                       | ClPGAwayMessage
                       | ClPGInputMuted
                       | ClPGOutputMuted
                       | ClPGNicknamePhonetic
                       | ClPGFlagAvatar
                       | ClPGMetaData

type Hash = Text

data ClientPropertySet = ClPSNickname         Text
                       | ClPSAway             Flag
                       | ClPSAwayMessage      Text
                       | ClPSInputMuted       Flag
                       | ClPSOutputMuted      Flag
                       | ClPSNicknamePhonetic Text
                       | ClPSFlagAvatar       Hash
                       | ClPSMetaData         Text

data RawCommand = RCQuit
                | RCUse
                | RCCurrentSCHandlerId
                | RCSendTextMessage
                | RCWhoAmI
                | RCBanAdd
                | RCBanDel
                | RCBanDelAll
                | RCBanList
                | RCClientGetIds
                | RCClientKick
                | RCClientList
                | RCClientMove
                | RCClientMute
                | RCClientUnmute
                | RCClientNotifyRegister
                | RCClientNotifyUnregister
                | RCClientPermList
                | RCClientPoke
                | RCClientUpdate
                | RCClientVariable
                | RCChannelConnectInfo
                | RCChannelCreate
                | RCChannelDelete
                | RCChannelEdit
                | RCChannelList
                | RCChannelMove
                | RCChannelVariable
                | RCServerVariable
                | RCServerConnectInfo
                | RCServerConnHandlerList
                deriving (Eq, Show, Read, Data, Typeable)

data Event = NotifyTalk
{-
  notifytalkstatuschange
  notifymessage
  notifymessagelist
  notifycomplainlist
  notifybanlist
  notifyclientmoved
  notifyclientleftview
  notifycliententerview
  notifyclientpoke
  notifyclientchatclosed
  notifyclientchatcomposing
  notifyclientupdated
  notifyclientids
  notifyclientdbidfromuid
  notifyclientnamefromuid
  notifyclientnamefromdbid
  notifyclientuidfromclid
  notifyconnectioninfo
  notifychannelcreated
  notifychanneledited
  notifychanneldeleted
  notifychannelmoved
  notifyserveredited
  notifyserverupdated
  channellist
  channellistfinished
  notifytextmessage
  notifycurrentserverconnectionchanged
  notifyconnectstatuschange
-}

type UID = Natural
type Time = Natural
type BanID = Natural
type ClientID = Natural
type ClientUID = Text
type SCHandlerID = Natural
type Msg = Text
type ChannelID = Natural
type ClPS = Set ClientPropertySet
type ChPS = Set ChannelPropertySet
type ClPG = Set ClientPropertyGet
type ChPG = Set ChannelPropertyGet

data Command = CQuit
             | CWhoAmI
             | CCurrentSCHandlerId
             | CServerConnectInfo
             | CServerConnHandlerList
             | CChannelConnectInfo
             | CBanList
             | CClientList
             | CChannelList
             | CClientGetIds    ClientUID
             | CUse             SCHandlerID
             | CNotifyRegister  SCHandlerID Event
             | CNotifyUnregister
             | CPrvMessage      Msg         ClientID
             | CChnMessage      Msg
             | CSrvMessage      Msg
             | CClientPoke      ClientID    Text
             | CBanAdd          UID         Time      Text
             | CBanDel          BanID
             | CBanDelAll
             | CClientKick      ClientID    Text
             | CClientMove      ClientID    ChannelID
             | CClientMute      ClientID
             | CClientUnmute    ClientID
             | CClientUpdate    ClPS
             | CClientVariable  ClientID    ClPG
             | CChannelCreate   Text        ChPS
             | CChannelDelete   ChannelID
             | CChannelEdit     ChannelID   ChPS
             | CChannelMove     ChannelID   ChannelID
             | CChannelVariable ChannelID   ChPG
             deriving (Eq, Show, Read, Data, Typeable)


{-
data Command = CQuit
             | CUse
             | CCurrentSCHandlerId
             | CSendTextMessage
             | CWhoAmI
             | CBanAdd
             | CBanDel
             | CBanDelAll
             | CBanList
             | CClientGetDbIdFromUId
             | CClientGetIds
             | CClientGetNameFromUId
             | CClientGetUIdFromClId
             | CClientKick
             | CClientList
             | CClientMove
             | CClientMute
             | CClientUnmute
             | CClientNotifyRegister
             | CClientNotifyUnregister
             | CClientPermList
             | CClientPoke
             | CClientUpdate
             | CClientVariable
             | CChannelConnectInfo
             | CChannelCreate
             | CChannelDelete
             | CChannelEdit
             | CChannelList
             | CChannelMove
             | CChannelVariable
             | CServerVariable
             | CServerConnectInfo
             | CServerConnHandlerList
             deriving (Eq, Show, Read, Data, Typeable)

instance Fusible Command where
  aType CQuit                   = ASetter
  aType CUse                    = ASetter
  aType CCurrentSCHandlerId     = AGetter
  aType CSendTextMessage        = ASetter
  aType CWhoAmI                 = AGetter
  aType CBanAdd                 = ASetter
  aType CBanDel                 = ASetter
  aType CBanDelAll              = ASetter
  aType CBanList                = AGetter
  aType CClientGetDbIdFromUId   = AGetter
  aType CClientGetIds           = AGetter
  aType CClientGetNameFromUId   = AGetter
  aType CClientGetUIdFromClId   = AGetter
  aType CClientKick             = ASetter
  aType CClientList             = AGetter
  aType CClientMove             = ASetter
  aType CClientMute             = ASetter
  aType CClientUnmute           = ASetter
  aType CClientNotifyRegister   = ASetter
  aType CClientNotifyUnregister = ASetter
  aType CClientPermList         = AGetter
  aType CClientPoke             = ASetter
  aType CClientUpdate           = ASetter
  aType CClientVariable         = AGetter
  aType CChannelConnectInfo     = AGetter
  aType CChannelCreate          = ASetter
  aType CChannelDelete          = ASetter
  aType CChannelEdit            = ASetter
  aType CChannelList            = AGetter
  aType CChannelMove            = ASetter
  aType CChannelVariable        = AGetter
  aType CServerVariable         = AGetter
  aType CServerConnectInfo      = AGetter
  aType CServerConnHandlerList  = AGetter
-}
