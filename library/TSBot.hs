{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main TSBot library
module TSBot (module TSBot) where

-- GENERATE: import New.Module as TSBot
import           Control.Concurrent           (forkIO, killThread)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary          hiding (mapM_)
import           Data.Conduit.Combinators     (decodeUtf8, encodeUtf8)
import qualified Data.Conduit.Combinators     as CC
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T (putStrLn)
import           Network                      (PortID (..), connectTo)
import           System.Environment           (getArgs, getProgName)
import           System.IO                    (BufferMode (..), hClose,
                                               hSetBuffering, stdin)
import           TSBot.ClientQuery.Parse

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
  let telnetConn = connectTo host $ PortNumber $ fromIntegral port
  (releaseSock, hsock) <- allocate telnetConn hClose
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

toText :: (Monad m) => Conduit String m Text
toText = CC.map T.pack

unText :: (Monad m) => Conduit Text m String
unText = CC.map T.unpack

-- | Telnet wrapper (accepts/sends 'String')
telnetStr :: Telnet -> Source IO String -> Sink String IO () -> IO ()
telnetStr t src sink = telnetText t (src $= toText) (unText =$ sink)

ioSink :: (a -> IO ()) -> Sink a IO ()
ioSink f = awaitForever (liftIO . f)

printSink :: Show a => Sink a IO ()
printSink = ioSink print

bsputSink :: Sink ByteString IO ()
bsputSink = ioSink B.putStrLn

tputSink :: Sink Text IO ()
tputSink = ioSink T.putStrLn

sputSink :: Sink String IO ()
sputSink = ioSink putStrLn

readSrc :: Source IO Text
readSrc = liftIO (stdin `hSetBuffering` LineBuffering)
          >> sourceHandle stdin $= decodeUtf8

-- | Main function
main :: IO ()
main = telnetText defaultTelnet readSrc (conduitParserEither responseP =$ printSink)
