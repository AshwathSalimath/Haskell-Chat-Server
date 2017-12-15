{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Types where

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Hashable
import Data.List (delete, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.Socket
import Network.BSD
import System.Exit (exitSuccess)
import System.Random
import Semaphore

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

