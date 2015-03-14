{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main TSBot library
module Web.TSBot (module Web.TSBot) where

-- GENERATE: import New.Module as Web.TSBot
import           Control.Monad.IO.Class            (liftIO)
import           Data.Attoparsec.Text
import           Data.Conduit
import           Data.Conduit.Binary               hiding (mapM_)
import           Data.Conduit.Combinators          (decodeUtf8)
import qualified Data.Conduit.Combinators          as CC
import           Data.Text                         (Text, pack, unpack)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T (putStrLn)
import           System.IO                         (BufferMode (..),
                                                    hSetBuffering, stdin)
import           Web.TSBot.ClientQuery.Escape      as Web.TSBot
import           Web.TSBot.ClientQuery.Parse       as Web.TSBot
import           Web.TSBot.ClientQuery.PrettyPrint as Web.TSBot
import           Web.TSBot.ClientQuery.Response    as Web.TSBot
import           Web.TSBot.ClientQuery.Telnet      as Web.TSBot

toCond :: Monad m => (a -> b) -> Conduit a m b
toCond = CC.map

-- | Generic function that lifts simple IO functions to 'Sink's
ioSink :: (a -> IO ()) -> Sink a IO ()
ioSink f = awaitForever (liftIO . f)

-- | Run 'print' on any data received
printSink :: Show a => Sink a IO ()
printSink = ioSink print

-- | Print out 'Text' line-by-line
tputSink :: Sink Text IO ()
tputSink = ioSink T.putStrLn

-- | Print out 'String's line-by-line
readSrc :: Source IO Text
readSrc = liftIO (stdin `hSetBuffering` LineBuffering)
          >> sourceHandle stdin $= decodeUtf8

-- | A parse conduit for 'responseP'
parseCond :: Conduit Text IO (Either String CQResponse)
parseCond = toCond $ parseOnly responseP

-- | A conduit that pretty-prints 'CQResponse's
prettyCond' :: Conduit CQResponse IO Text
prettyCond' = toCond resPretty

-- | A conduit that pretty-prints 'CQResponse's and handles parse errors
prettyCond :: Conduit (Either String CQResponse) IO Text
prettyCond = toCond $ either T.pack resPretty

-- | A conduit that ugly-prints 'CQResponse's and handles parse errors
rprintCond' :: Conduit CQResponse IO Text
rprintCond' = toCond resPrint

-- | A conduit that ugly-prints 'CQResponse's and handles parse errors
rprintCond :: Conduit (Either String CQResponse) IO Text
rprintCond = toCond $ either T.pack resPrint

-- | Main function
main :: IO ()
main = telnetText
       defaultTelnetH
       readSrc
       (parseCond =$= rprintCond =$ tputSink)
