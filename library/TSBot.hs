{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main TSBot library
module TSBot (module TSBot) where

-- GENERATE: import New.Module as TSBot
import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Machine as M
import Data.Conduit.Combinators (encodeUtf8, decodeUtf8)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Binary hiding (mapM_)
import Data.Text as T
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import Network (connectTo, PortID (..))
import System.Environment (getArgs, getProgName)
import System.IO (BufferMode (..), hSetBuffering, hClose, stdin)

type BS = ByteString

data Telnet = Telnet { tHost :: String
                     , tPort :: Int
                     }

-- | Default telnet settings for TSClientQuery
defaultTelnet :: Telnet
defaultTelnet = Telnet "localhost" 25639

-- | Raw telnet IO function
telnetRaw :: String -> Int -> Source IO BS -> Sink BS IO () -> IO ()
telnetRaw host port src sink = runResourceT $ do
  (releaseSock, hsock) <- allocate (connectTo host $ PortNumber $ fromIntegral port) hClose
  liftIO $ hsock `hSetBuffering` LineBuffering
  (releaseThread, _) <- allocate (forkIO $ sourceHandle hsock $$ sink) killThread
  liftIO $ src $$ sinkHandle hsock
  release releaseThread
  release releaseSock

-- | Telnet wrapper (accepts/sends 'ByteString')
telnetBS :: Telnet -> Source IO BS -> Sink BS IO () -> IO ()
telnetBS (Telnet host port) = telnetRaw host port

-- | Telnet wrapper (accepts/sends 'Text')
telnetText :: Telnet -> Source IO Text -> Sink Text IO () -> IO ()
telnetText t src sink = telnetBS t (src $= encodeUtf8) (decodeUtf8 =$ sink)

-- | Telnet wrapper (accepts/sends 'String')
telnetStr :: Telnet -> Source IO String -> Sink String IO () -> IO ()
telnetStr t src sink = telnetText t (src $= (CC.map T.pack)) ((CC.map T.unpack) =$ sink)

printSink :: Sink Text IO ()
printSink = awaitForever (liftIO . putStrLn . T.unpack)

readSrc :: Source IO Text
readSrc = liftIO (stdin `hSetBuffering` LineBuffering)
          >> sourceHandle stdin $= decodeUtf8

-- | Main function
main :: IO ()
main = telnetText defaultTelnet readSrc printSink
