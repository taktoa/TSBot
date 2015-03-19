{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The network connection code
module Web.TSBot.ClientQuery.Telnet
       ( Telnet (..)
       , TelnetH
       , defaultTelnet
       , defaultTProc
       , defaultTelnetH
       , telnetText
       ) where

import           Control.Concurrent           (forkIO, killThread)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit
import           Data.Conduit.Async
import           Data.Conduit.Binary          hiding (mapM_)
import           Data.Conduit.Combinators     (decodeUtf8, encodeUtf8)
import           Data.Text                    (Text)
import           Network                      (PortID (..), connectTo)
import           System.IO                    (BufferMode (..), Handle, hClose,
                                               hSetBuffering)

type BS = ByteString

-- | Configuration for a telnet connection
data Telnet = Telnet { tHost :: String -- ^ The hostname
                     , tPort :: Int    -- ^ The port number
                     }

-- | Type alias for telnet connection handles
type TelnetH = ResourceT IO (ReleaseKey, Handle)

-- | Default telnet settings for TSClientQuery
defaultTelnet :: Telnet
defaultTelnet = Telnet "localhost" 25639

-- | Default telnet handle generator.
-- | Wrap this in whatever IO shenanigans you want for initializing
-- | your telnet connection
defaultTProc :: Telnet -> TelnetH
defaultTProc (Telnet h p) = do
  (releaseSock, hsock) <- allocate (connectTo h . PortNumber $ fromIntegral p) hClose
  liftIO $ hsock `hSetBuffering` LineBuffering
  return (releaseSock, hsock)

-- | Default telnet connection handle
defaultTelnetH :: TelnetH
defaultTelnetH = defaultTProc defaultTelnet

telnetRaw :: TelnetH -> Source IO BS -> Sink BS IO () -> IO ()
telnetRaw conn src sink = runResourceT $ do
  (releaseSock, hsock) <- conn
  (releaseThread, _) <- allocate (forkIO $ sourceHandle hsock $$ sink) killThread
  liftIO $ src $$ sinkHandle hsock
  release releaseThread
  release releaseSock

-- telnetRaw :: TelnetH -> Source IO BS -> Sink BS IO () -> IO ()
-- telnetRaw conn src sink = runResourceT $ do
--   (releaseSock, hsock) <- conn
--   liftIO $ sourceHandle hsock $$& sink
--   liftIO $ src $$ sinkHandle hsock
--   release releaseSock

-- | Connect a source/sink to a given telnet handle
telnetText :: TelnetH -> Source IO Text -> Sink Text IO () -> IO ()
telnetText t src sink = telnetRaw t (src $= encodeUtf8) (decodeUtf8 =$ sink)
