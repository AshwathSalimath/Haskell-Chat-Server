module Types where

import Network.BSD
import System.IO
import System.Directory
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent
import Data.IP
import Data.List
import qualified Data.Maybe as M
import Data.String.Utils
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified Data.HashTable.IO as H

data User = User
    { _userID :: Int
    , _userNickName :: String
    , _userConn :: Socket
    }

data Channel = Channel
    { _channelID :: Int
    , _channelName :: String
    , _channelUsers :: [String]
    }

data ServerEnvr = ServerEnvr
    { _serverHost :: HostName
    , _serverPort :: String
    , _serverSock :: Socket
    , _serverSem :: Semaphore
    , _serverChannels :: MVar (Map Int Channel)
    , _serverUsers :: MVar (Map String User)
    }

makeLenses ''User
makeLenses ''Channel
makeLenses ''ServerEnvr

type Server = StateT ServerEnvr IO

maxConnections :: Int
maxConnections = 150

{-
sendResponse :: Handle -> String -> IO ()
sendResponse hdl resp = do
    hSetBuffering hdl $ BlockBuffering $ Just (length resp)
    hPutStr hdl resp

getHostNameNow :: IO String
getHostNameNow = do
    weAreInDocker <- doesFileExist "/.dockerenv"
    host <- if weAreInDocker then getHostByName "dockerhost"
        else (getHostName >>= getHostByName)
    return $ show $ fromHostAddress $ head $ hostAddresses host

hlog :: String -> IO ()
hlog s = putStrLn $ "\n*****************\n" ++ s ++ "\n*****************"-}

