module Client where

import Types

import Network.Socket
import System.IO
import Data.List
import Data.List.Split
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.HashTable.IO as H

{--- Function to respond to "HELO" string
helo :: Handle -> String -> String -> IO ()
helo hdl text port = do
    hostname <- getHostNameNow
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID: 17306521"-}

parseParam :: String -> String
parseParam str = splitStr !! 1
    where splitStr = splitOn ":" str

sendResponse :: Socket -> String -> Server ()
sendResponse conn response = do
    liftIO $ putStrLn $ "SENDING RESPONSE - " ++ response ++ "\n"
    void $ liftIO $ send conn response

-- Joining
--Joining a chat room is initiated by a client by sending the following message to a chat server.
--JOIN_CHATROOM: [chatroom name]
--CLIENT_IP: [IP Address of client if UDP | 0 if TCP]
--PORT: [port number of client if UDP | 0 if TCP]
--CLIENT_NAME: [string Handle to identifier client user]

handleJoin :: Socket -> String -> String -> Server ()
handleJoin conn reqChannelName clientName = do
    usersMap <- getUsers
    case Map.lookup clientName usersMap of
        Nothing -> do
            joinID <- liftIO (randomIO :: IO Int)
            let newUser = User joinID clientName conn
            updateUsers $ Map.insert clientName newUser usersMap
        Just _ -> return ()

    channelsMap <- getChannels

    let channelHash = hash reqChannelName

    case Map.lookup channelHash channelsMap of
        Nothing -> do
            let newChannel = Channel channelHash reqChannelName [clientName]
            updateChannels $ Map.insert channelHash newChannel channelsMap
        Just channel -> do
            let newChannel = channel & channelUsers .~ clientName : (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) channelHash channelsMap

    sendJoinResponse conn channelHash

sendJoinResponse :: Socket -> Int -> Server ()
sendJoinResponse conn channelHash = do
    channelsMap <- getChannels

    let channel = fromJust (channelsMap ^. at channelHash)
        chatroomName = channel ^. channelName
        roomRef = channel ^. channelID

    joinID <- liftIO (randomIO :: IO Int)
    serverIP <- use serverHost
    port <- use serverPort

    let response = "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
                   "SERVER_IP:" ++ serverIP ++ "\n" ++
                   "PORT:" ++ port ++ "\n" ++
                   "ROOM_REF:" ++ show roomRef ++ "\n" ++
                   "JOIN_ID:" ++ show joinID

    sendResponse conn response

-- Leaving ----A client leaves a chat room by sending the following message to the chat server:
--LEAVE_CHATROOM: [ROOM_REF]
--JOIN_ID: [integer previously provided by server on join]
--CLIENT_NAME: [string Handle to identifier client user]

handleLeave :: Socket -> Int -> String -> String -> Server ()
handleLeave conn roomRef joinID clientName = do
    channelsMap <- getChannels
    case Map.lookup roomRef channelsMap of
        Just channel -> do
            let newChannel = channel & channelUsers .~ delete clientName (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) roomRef channelsMap
        Nothing -> return ()

    sendLeaveResponse conn roomRef joinID

sendLeaveResponse :: Socket -> Int -> String -> Server ()
--The server responds with the following message:
sendLeaveResponse conn roomRef joinID = do
    let response = "LEFT_CHATROOM:" ++ show roomRef ++ "\n" ++
                   "JOIN_ID:" ++ joinID ++ "\n"

    sendResponse conn response

-- Disconnecting

handleDisconect :: Socket -> String -> Server ()
handleDisconect conn clientName = do
    usersMap <- getUsers
    updateUsers $ Map.delete clientName usersMap

    sendDisconnectResponse conn clientName

sendDisconnectResponse :: Socket -> String -> Server ()
sendDisconnectResponse conn clientName = do
    let response = "DISCONNECTED:" ++ clientName
    sendResponse conn response

-- Messaging

handleChat :: Int -> String -> String -> Server ()
handleChat roomRef clientName message = do
    usersMap <- getUsers
    channelsMap <- getChannels
    let users = fromJust (channelsMap ^. at roomRef) ^. channelUsers
    forM_ users $ \userName ->
        case Map.lookup userName usersMap of
            Nothing -> return ()
            Just user -> sendChatResponse (user ^. userConn) roomRef clientName message

sendChatResponse :: Socket -> Int -> String -> String -> Server ()
sendChatResponse conn roomRef clientName message = do
    let response = "CHAT:" ++ show roomRef ++ "\n" ++
					--"JOIN_ID:"++ show joinID ++ "\n" ++
                   "CLIENT_NAME:" ++ clientName ++ "\n" ++
                   "MESSAGE:" ++ message
    sendResponse conn response

-- HELO

handleHELO :: Socket -> String -> Server ()
handleHELO conn message = do
    host <- use serverHost
    port <- use serverPort
    let response = message ++ "\n" ++
                   "IP:" ++ host ++ "\n" ++
                   "Port:" ++ port ++ "\n" ++
                   "StudentID: 17306521"
    sendResponse conn response

getChannels :: Server (Map Int Channel)
getChannels = use serverChannels >>= liftIO . readMVar

updateChannels :: Map Int Channel -> Server ()
updateChannels channels = do
    channelsMVar <- use serverChannels
    void $ liftIO $ swapMVar channelsMVar channels

getUsers :: Server (Map String User)
getUsers = use serverUsers >>= liftIO . readMVar

updateUsers :: Map String User -> Server ()
updateUsers users = do
    usersMVar <- use serverUsers
    void $ liftIO $ swapMVar usersMVar users

getFQDN :: IO HostName
getFQDN = liftM hostName (getHostName >>= getHostByName)
