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
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T (putStrLn)
import           System.IO                         (BufferMode (..),
                                                    hSetBuffering, stdin)
import           Web.TSBot.ClientQuery.Escape      as Web.TSBot
import           Web.TSBot.ClientQuery.Parse       as Web.TSBot
import           Web.TSBot.ClientQuery.PrettyPrint as Web.TSBot
import           Web.TSBot.ClientQuery.Response    as Web.TSBot
import           Web.TSBot.ClientQuery.Telnet      as Web.TSBot

-- | Conduit that packs 'String' into 'Text'
toText :: (Monad m) => Conduit String m Text
toText = CC.map T.pack

-- | Conduit that unpacks 'Text' into 'String'
unText :: (Monad m) => Conduit Text m String
unText = CC.map T.unpack

-- | Telnet wrapper (accepts/sends 'String')
telnetStr :: TelnetH -> Source IO String -> Sink String IO () -> IO ()
telnetStr t src sink = telnetText t (src $= toText) (unText =$ sink)

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
sputSink :: Sink String IO ()
sputSink = ioSink putStrLn

-- | Read input from stdin
readSrc :: Source IO Text
readSrc = liftIO (stdin `hSetBuffering` LineBuffering)
          >> sourceHandle stdin $= decodeUtf8

-- | A parse conduit for 'responseP'
parseCond :: Conduit Text IO (Either String CQResponse)
parseCond = awaitForever (yield . parseOnly responseP)

-- | A conduit that pretty-prints 'CQResponse's
prettyCond' :: Conduit CQResponse IO Text
prettyCond' = awaitForever (yield . resPretty)

-- | A conduit that pretty-prints 'CQResponse's and handles parse errors
prettyCond :: Conduit (Either String CQResponse) IO Text
prettyCond = awaitForever (yield . either T.pack resPretty)

-- | Main function
main :: IO ()
main = telnetText defaultTelnetH readSrc (parseCond =$= prettyCond =$ tputSink)
