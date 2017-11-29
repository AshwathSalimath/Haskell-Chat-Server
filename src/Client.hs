module Client where

import Types

import Network.Socket
import System.IO
import Data.List
import Data.List.Split
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.HashTable.IO as H

-- Function to respond to "HELO" string
helo :: Handle -> String -> String -> IO ()
helo hdl text port = do
    hostname <- getHostNameNow
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID: 17306521"
