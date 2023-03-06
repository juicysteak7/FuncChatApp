module Server 
    ( mainServer
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as Byte
import Data.Maybe
import Control.Monad
import Control.Concurrent 
import Data.List

data ClientInfo = ClientInfo 
  {
    clientSocket :: Socket,
    clientName :: String,
    clientRooms :: [String]
  } deriving (Eq)

mainServer :: IO ()
mainServer = withSocketsDo $ do
  serverSock <- socket AF_INET Stream 0
  setSocketOption serverSock ReuseAddr 1
  bind serverSock (SockAddrInet 3000 0)
  listen serverSock 2

  putStrLn "Listening on port 3000"

  clientInfo <- newMVar []

  -- Fork acceptLoop so server can listen for commands
  _ <- forkIO $ acceptLoop serverSock clientInfo

  mainLoop serverSock clientInfo

-- Forked loop that listens for server commands, while the server goes on to accept clients
mainLoop :: Socket -> MVar [ClientInfo] -> IO ()
mainLoop sock clientInfo = do
  msg <- getLine
  let wordsMsg = words msg

  case head wordsMsg of
    -- Closes server
    "/quit" -> do
      putStrLn "Closing Server"
      close sock
    -- Allows server communication to specified chat room
    "/sendTo" -> do
      let room = wordsMsg !! 1
      clientList <- readMVar clientInfo
      let msgToSend = Byte.pack $ "(Server): " ++ drop (length (head wordsMsg) + length (wordsMsg !! 1) + 2) msg
      mapM_ (\c -> when (room `elem` clientRooms c) $ sendAll (clientSocket c) msgToSend) clientList
      mainLoop sock clientInfo
    -- Allows server to communicate to all clients
    "/sendToAll" -> do
      clientList <- readMVar clientInfo
      let msgToSend = Byte.pack $ "(Server): " ++ drop (length (head wordsMsg) + 1) msg
      mapM_ (\c -> sendAll (clientSocket c) msgToSend) clientList
      mainLoop sock clientInfo
    -- Disconnects a specific client from the server
    "/disconnect" -> do
      let client = drop (length (head wordsMsg) + 1) msg
      clientList <- readMVar clientInfo
      let clientMaybe = getClientByName client clientList
      if isJust clientMaybe
        then sendAll (clientSocket (getMaybeClient clientMaybe)) (Byte.pack "exit")
      else putStrLn "Client not found."
      mainLoop sock clientInfo
    _ -> do
      mainLoop sock clientInfo

-- Accept client loop, adds clients to the client MVar
acceptLoop :: Socket -> MVar [ClientInfo] -> IO ()
acceptLoop serverSock clientInfo = do
  (sock, addr) <- accept serverSock
  putStrLn $ "Accepted connection from " ++ show addr

  -- add the new client to the list
  modifyMVar_ clientInfo $ \clientInfos -> return ( ClientInfo sock "" ["General"] : clientInfos)

  -- fork a new thread to handle the client
  _ <- forkIO $ handleClient sock clientInfo

  -- continue accepting new clients
  acceptLoop serverSock clientInfo

-- The forked individual client handler
handleClient :: Socket -> MVar [ClientInfo] -> IO ()
handleClient sock clientInfo = do

  -- loop to receive and broadcast messages
  loop
  where
    loop = do
      msg <- recv sock 1024
      let msg' = Byte.unpack msg
      let wordsMsg = words msg'

      clientList <- readMVar clientInfo
      let currentClient = getCurrentClient sock clientList

      case head wordsMsg of
        -- Save inital name givin to server
        "/init" -> do
          let name = drop (length (head wordsMsg) + 1) msg'
          putStrLn (name ++ " has entered the server!")
          modifyMVar_ clientInfo $ \clients -> return (editClientName (ClientInfo sock name (clientRooms currentClient)) clients)
          sendAll (clientSocket currentClient) (Byte.pack "Successfully saved name.")
          sendAll (clientSocket currentClient) (Byte.pack "Joined chat room [\"General\"].")
          loop
        -- Private message between two clients
        "/whisper" -> do
          let maybeClient = getClientByMsg msg' clientList
          if isJust maybeClient 
            then do
              let client = getMaybeClient maybeClient
              let msg'' = drop (length (head wordsMsg) + length (clientName client) + 2) msg'
              sendAll (clientSocket client) (Byte.pack ("(Whisper) " ++ clientName currentClient ++ ": " ++ msg''))
          else sendAll (clientSocket currentClient) (Byte.pack "Client not found.")
          loop
        -- Lets client re-name themselves
        "/name" -> do
          let name = drop (length (head wordsMsg) + 1) msg'
          putStrLn (clientName currentClient ++ " has changed thier name to " ++ name)
          modifyMVar_ clientInfo $ \clients -> return (editClientName (ClientInfo sock name (clientRooms currentClient)) clients)
          sendAll (clientSocket currentClient) (Byte.pack "Successfully saved name.")
          loop
        -- Lets client check current saved name
        "/myName" -> do
          let name = clientName currentClient
          sendAll (clientSocket currentClient) (Byte.pack ("Your saved name is: " ++ name))
          loop
        -- List all current client chat rooms
        "/myChatRooms" -> do
          sendAll (clientSocket currentClient) (Byte.pack (show (clientRooms currentClient)))
          loop
        -- Lists all active chat rooms on server
        "/chatRooms" -> do
          sendAll (clientSocket currentClient) (Byte.pack (show (listChatRooms clientList [])))
          loop
        -- Allows client to sent to a specific chat room
        "/sendTo" -> do
          let room = wordsMsg !! 1
          let msgToSend = Byte.pack $ "[" ++ show room ++ "] " ++ clientName currentClient ++ ": " ++ drop (length (head wordsMsg) + length (wordsMsg !! 1) + 2) msg'
          mapM_ (\c -> when (clientSocket c /= sock && room `elem` clientRooms c) $ sendAll (clientSocket c) msgToSend) clientList
          loop
        -- Lists all members of specific chat room
        "/members" -> do
          let room = wordsMsg !! 1
          sendAll (clientSocket currentClient) (Byte.pack (show (listClients room clientList)))
          loop
        -- Removes client from desired chat room/s
        "/leave" -> do
          let rooms = drop 1 wordsMsg
          modifyMVar_ clientInfo $ \clients -> return (leaveClientRooms (ClientInfo sock "" rooms) clients)
          sendAll (clientSocket currentClient) (Byte.pack ("Successfully left chat rooms: " ++ show rooms))
          loop
        -- Adds client to desired chat room/s
        "/join" -> do
          let rooms = drop 1 wordsMsg
          modifyMVar_ clientInfo $ \clients -> return (addClientRooms (ClientInfo sock "" rooms) clients)
          mapM_ (\c -> when (clientSocket c /= sock && (clientRooms c `isInfixOf` rooms 
            || rooms `isInfixOf` clientRooms c) && clientRooms c /= []) $ sendAll (clientSocket c) 
            (Byte.pack (clientName currentClient ++ " has joined the chat room!"))) clientList
          sendAll (clientSocket currentClient) (Byte.pack ("Successfully joined chat rooms: " ++ show rooms))
          loop
        -- Client terminated connection
        "exit" -> do
          putStrLn  $ clientName currentClient ++ " has disconnected from server."
          modifyMVar_ clientInfo $ \clients -> return (removeClients currentClient clients)
          close sock
        -- Relay message. Don't send to self, or chat rooms you aren't in. Clients in empty chat rooms shouldn't see
        -- into other chat rooms, but should see other clients in empty chat rooms
        _ -> do
          putStrLn (clientName currentClient ++ ": " ++ msg')
          mapM_ (\c -> when (clientSocket c /= sock && (clientRooms c `isInfixOf` clientRooms currentClient 
            || clientRooms currentClient `isInfixOf` clientRooms c) 
            && (null (clientRooms c) && null (clientRooms currentClient) || clientRooms c /= [] && clientRooms currentClient /= [])) 
            $ sendAll (clientSocket c) (Byte.pack (show (getMatchingRooms (clientRooms c) (clientRooms currentClient)) 
            ++ " " ++ clientName currentClient ++ ": " ++ msg'))) clientList
          loop

-- Helper functions

-- Gets all active chat rooms
listChatRooms :: [ClientInfo] -> [String] -> [String]
listChatRooms [] [] = []
listChatRooms (c:cs) [] = listChatRooms cs (clientRooms c)
listChatRooms [] rooms = rooms
listChatRooms (c:cs) rooms | clientRooms c `isInfixOf` rooms || rooms `isInfixOf` clientRooms c = listChatRooms cs rooms
                           | otherwise = listChatRooms cs (clientRooms c ++ rooms)

-- Lists all clients in a chatroom
listClients :: String -> [ClientInfo] -> [String]
listClients room clientList = do
  currentClient <- clientList
  if room `elem` clientRooms currentClient
    then return $ clientName currentClient
  else []

-- Gets matching chat rooms from two clients rooms
getMatchingRooms :: [String] -> [String] -> [String]
getMatchingRooms rooms1 rooms2 = do
  room1 <- rooms1
  room2 <- rooms2
  if room1 == room2
    then return room1
  else []

-- Removes client from list of clients
removeClients :: ClientInfo -> [ClientInfo] -> [ClientInfo]
removeClients toRemove clientList = do
  currentClient <- clientList
  if clientSocket currentClient /= clientSocket toRemove 
    then return currentClient
  else []

-- Updates a clients name
editClientName :: ClientInfo -> [ClientInfo] -> [ClientInfo]
editClientName toEdit clientList = do
  currentClient <- clientList
  if clientSocket currentClient == clientSocket toEdit
    then return toEdit
  else return currentClient

--  Updates a clients chat rooms
addClientRooms :: ClientInfo -> [ClientInfo] -> [ClientInfo]
addClientRooms toAdd clientList = do
  currentClient <- clientList
  if clientSocket currentClient == clientSocket toAdd
    then return $ ClientInfo (clientSocket currentClient) (clientName currentClient) 
      (addClientRoomsHelper (clientRooms toAdd) (clientRooms currentClient))
  else return currentClient

addClientRoomsHelper :: [String] -> [String] -> [String]
addClientRoomsHelper [] [] = []
addClientRoomsHelper [] xs = xs
addClientRoomsHelper xs [] = xs
addClientRoomsHelper (x:xs) ys = x : addClientRoomsHelper xs ys

-- Gets current client when in client handler
getCurrentClient :: Socket -> [ClientInfo] -> ClientInfo
getCurrentClient sock [] = ClientInfo sock "" []
getCurrentClient sock (c:cs) | clientSocket c == sock = c
                             | otherwise = getCurrentClient sock cs

-- Matches a client name from a whole message string (For whisper)
getClientByMsg :: String -> [ClientInfo] -> Maybe ClientInfo
getClientByMsg _ [] = Nothing
getClientByMsg [] _ = Nothing
getClientByMsg msg (c:cs) | clientName c `isInfixOf` msg = Just c
                          | otherwise = getClientByMsg msg cs

-- Gets a client by name
getClientByName :: String -> [ClientInfo] -> Maybe ClientInfo
getClientByName _ [] = Nothing
getClientByName [] _ = Nothing
getClientByName name (c:cs) | clientName c == name = Just c
                            | otherwise = getClientByName name cs

-- Non exaustive, always check if the maybe is not nothing before using.
getMaybeClient :: Maybe ClientInfo -> ClientInfo
getMaybeClient (Just client) = client

-- Updates client rooms
leaveClientRooms :: ClientInfo -> [ClientInfo] -> [ClientInfo]
leaveClientRooms toLeave clientList = do
  currentClient <- clientList
  if clientSocket currentClient == clientSocket toLeave
    then return $ ClientInfo (clientSocket currentClient) (clientName currentClient) 
      (leaveClientRoomsHelper (clientRooms toLeave) (clientRooms currentClient))
  else return currentClient

leaveClientRoomsHelper :: [String] -> [String] -> [String]
leaveClientRoomsHelper [] [] = []
leaveClientRoomsHelper _ [] = []
leaveClientRoomsHelper [] xs = xs
leaveClientRoomsHelper (x:xs) (y:ys) | x == y = leaveClientRoomsHelper xs ys
                                     | otherwise = leaveClientRoomsHelper (x:xs) (y:ys)