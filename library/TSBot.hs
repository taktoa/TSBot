{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The main TSBot library
module TSBot (module TSBot) where

-- GENERATE: import New.Module as TSBot
import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import Network (connectTo, PortID (..))
import System.Environment (getArgs, getProgName)
import System.IO

-- | Telnet IO function
telnet :: String -> Int -> IO ()
telnet host port = runResourceT $ do
  (releaseSock, hsock) <- allocate (connectTo host $ PortNumber $ fromIntegral port) hClose
  liftIO $ mapM_ (`hSetBuffering` LineBuffering) [ stdin, stdout, hsock ]
  (releaseThread, _) <- allocate (forkIO $
                                  runResourceT $
                                  sourceHandle stdin $$
                                  sinkHandle hsock) killThread
  sourceHandle hsock $$ sinkHandle stdout
  release releaseThread
  release releaseSock

-- | Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [host, port] -> telnet host (read port :: Int)
        _ -> usageExit
  where
    usageExit = do
        name <- getProgName
        putStrLn $ "Usage : " ++ name ++ " host port"

