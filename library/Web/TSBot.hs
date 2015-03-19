{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main TSBot library
module Web.TSBot (module Web.TSBot) where

import           Web.TSBot.ClientQuery.Command     as Web.TSBot
-- GENERATE: import New.Module as Web.TSBot
import           Control.Concurrent                (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad                     hiding (mapM_)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Attoparsec.Text
import           Data.ByteString.Char8             (hPutStrLn)
import           Data.Conduit
import           Data.Conduit.Binary               hiding (mapM_)
import           Data.Conduit.Combinators          (decodeUtf8)
import qualified Data.Conduit.Combinators          as CC
import           Data.Conduit.TMChan
import           Data.Foldable                     (forM_, mapM_)
import           Data.Functor                      ((<$>))
import           Data.Maybe
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text, pack, unpack)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T (putStrLn)
import           Prelude                           hiding (mapM_)
import           System.IO                         (BufferMode (..),
                                                    hSetBuffering, stdin)
import           Web.TSBot.ClientQuery.Escape      as Web.TSBot
import           Web.TSBot.ClientQuery.Parse       as Web.TSBot
import           Web.TSBot.ClientQuery.PrettyPrint as Web.TSBot
import           Web.TSBot.ClientQuery.Response    as Web.TSBot
import           Web.TSBot.ClientQuery.Telnet      as Web.TSBot

tshow :: Show a => a -> Text
tshow = pack . show

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
parseCond :: Conduit Text IO ECQR
parseCond = toCond (conv . parseOnly responseP)
  where
    conv (Left s)  = Left $ CQParseError $ pack s
    conv (Right r) = Right r

-- | A conduit that pretty-prints 'CQResponse's
prettyCond' :: Conduit CQResponse IO Text
prettyCond' = toCond resPretty

-- | A conduit that pretty-prints 'ECQR's
prettyCond :: Conduit ECQR IO Text
prettyCond = toCond $ either tshow resPretty

-- | A conduit that ugly-prints 'CQResponse's
rprintCond' :: Conduit CQResponse IO Text
rprintCond' = toCond resPrint

-- | A conduit that ugly-prints 'ECQR's
rprintCond :: Conduit ECQR IO Text
rprintCond = toCond $ either tshow resPrint

(|>) :: a -> (a -> b) -> b
x |> y = y x

(~~>) :: (a -> CQResponse) -> (Text, CQValue) -> a -> CQResponse
x ~~> y = x |-> matching y

(|->) :: (a -> b) -> (b -> c) -> a -> c
x |-> y = y . x

mch :: (Text, CQValue) -> CQResponse -> CQResponse
mch = matching

botPrefix :: Text
botPrefix  = "[color=blue]LambdaBot[/color]: "

isTrue' :: (a -> Bool) -> Maybe a -> Maybe a
isTrue' p x = if isJust (x >>= guard . p) then x else Nothing

testCatchEvent :: Conduit ECQR IO Text
testCatchEvent = awaitForever bprd
  where
    bprd (Left  s) = yield $ write $ "There was an error: " <> pack (show s)
    bprd (Right c) = prd c
    prefix = pack $ init $ cqvPretty $ CQVStr botPrefix
    write x = "sendtextmessage targetmode=2 msg=" <>
              escape (botPrefix <> x) <> "\n"
    notPrefixOf x y = not (x `T.isPrefixOf` y)
    put x = T.putStrLn x >> return x
    prd = mch ("notifytextmessage", CQVNil)
          ~~> ("targetmode", CQVInt 2)
          |-> listToMaybe
          |-> (>>= retrieve (AName "msg"))
          |-> isTrue' ((prefix `notPrefixOf`) . pack . cqvPretty)
          |-> mapM_ ((>>= (yield . write)) . liftIO . put . pack . cqvPretty)

-- | Main function
main :: IO ()
main = do
  recv <- atomically (newTBMChan 16 :: STM (TBMChan Text))
  let myTProc t = do
        (r, h) <- defaultTProc t
        liftIO $ hPutStrLn h "clientnotifyregister schandlerid=0 event=any"
        return (r, h)
  let inCond  = CC.map id
  let outCond = parseCond =$= testCatchEvent
  telnetText
    (myTProc defaultTelnet)
    (sourceTBMChan recv $= inCond)
    (outCond =$ sinkTBMChan recv True)
