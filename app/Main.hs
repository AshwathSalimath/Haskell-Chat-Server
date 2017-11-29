module Main where

import Network.Socket
import System.IO
import System.Environment
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent.ParallelIO.Local
import Control.Concurrent.MVar
import Control.Concurrent
import Data.List
import Data.List.Split
import qualified Data.HashTable.IO as H
import Control.Monad (when, unless)

import Client
import Types

main :: IO ()
main = do
    [port] <- getArgs
    sock <- socket AF_INET Stream 0                            -- create socket
    setSocketOption sock ReuseAddr 1                           -- make socket immediately reusable.
    bind sock (SockAddrInet (toEnum $ read port) iNADDR_ANY)   -- listen on TCP port given by user.
    let nbThreads = 10
    listen sock (nbThreads*2)                                  -- queue of 20 connections max
    chan <- newChan                                            -- Creating a new channel called "chan"
    htSI           <- H.new :: IO (HashTable String Int)
    htIC           <- H.new :: IO (HashTable Int ChatRoom)
    htClients      <- H.new :: IO (HashTable Int Client)
    htClientsNames <- H.new :: IO (HashTable String Int)
    let nbCR = 0                                               -- Initializing number of chatRooms = 0
    chatRooms <- newMVar (ChatRooms {chatRoomFromId=htIC, chatRoomIdFromName=htSI, numberOfChatRooms=nbCR})
    clients <- newMVar (Clients 0 htClients htClientsNames)
    withPool nbThreads $
        \pool -> parallel_ pool (replicate nbThreads (server sock port chan clients chatRooms))
    hlog "Server killed. See you !"


type Msg = (Int, String)

server :: Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> IO ()
server sock port chan clients chatrooms = do
    hlog "Waiting for incoming connection..."
    conn <- try (accept sock) :: IO (Either SomeException (Socket, SockAddr))  -- try to accept a connection and handle it
    case conn of
        Left  _    -> hlog "Socket is now closed. Exiting."
        Right conn -> do
            hlog "Got a client !"
            runConn conn sock port chan clients chatrooms       -- run our client's logic, then
            server sock port chan clients chatrooms               -- repeat


loopConn :: Handle -> Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> [Int] -> IO ()
loopConn hdl originalSocket port chan clients chatrooms joinIds = do
--     (kill, timedOut, input) <- waitForInput hdl chan 0 joinIds clients
--     when (timedOut) (hlog "Client timed out")
--     when (kill || timedOut) (return ())
    input <- fmap init (hGetLine hdl)
    let commandAndArgs = splitOn " " input
    let command = head commandAndArgs
    let args = intercalate " " $ tail commandAndArgs
    case command of
        "HELO"            -> do
            helo hdl args port
            loopConn hdl originalSocket port chan clients chatrooms joinIds


runConn :: (Socket, SockAddr) -> Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> IO ()
runConn (sock, addr) originalSocket port chan clients chatrooms = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    handle (\(SomeException _) -> return ()) $ fix $ (\loop -> (loopConn hdl originalSocket port chan clients chatrooms []))
    -- shutdown sock ShutdownBoth
    hClose hdl
    hlog "Client disconnected"