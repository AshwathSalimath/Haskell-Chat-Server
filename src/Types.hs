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

type HashTable k v = H.CuckooHashTable k v

data Client    = Client { clientName :: String
                        , subs       :: HashTable Int (Chan String)
                        , joinId     :: Int
                        }
data Clients   = Clients { lastClientId   :: Int
                         , theClients     :: HashTable Int Client
                         , clientsNames   :: HashTable String Int
                         }
data ChatRoom  = ChatRoom Int (Chan String)
data ChatRooms = ChatRooms { chatRoomFromId     :: HashTable Int ChatRoom
                           , chatRoomIdFromName :: HashTable String Int
                           , numberOfChatRooms  :: Int
                           }


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
hlog s = putStrLn $ "\n*****************\n" ++ s ++ "\n*****************"