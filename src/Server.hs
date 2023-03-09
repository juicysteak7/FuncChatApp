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
import Control.Exception

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
    if not (null wordsMsg)
      then do
        case head wordsMsg of
          -- Closes server
          "/quit" -> do
            putStrLn "Closing Server"
            close sock
          -- Allows server communication to specified chat room
          "/sendTo" -> do
            let room = wordsMsg !! 1
            clientList <- readMVar clientInfo
            let msgToSend = "(Server): " ++ drop (length (head wordsMsg) + length (wordsMsg !! 1) + 2) msg
            mapM_ (\c -> when (room `elem` clientRooms c) $ broadcast (clientSocket c) msgToSend) clientList
            mainLoop sock clientInfo
          -- Allows server to communicate to all clients
          "/sendToAll" -> do
            clientList <- readMVar clientInfo
            let msgToSend = "(Server): " ++ drop (length (head wordsMsg) + 1) msg
            mapM_ (\c -> broadcast (clientSocket c) msgToSend) clientList
            mainLoop sock clientInfo
          -- Disconnects a specific client from the server
          "/disconnect" -> do
            let client = drop (length (head wordsMsg) + 1) msg
            clientList <- readMVar clientInfo
            let clientMaybe = getClientByName client clientList
            if isJust clientMaybe
              then broadcast (clientSocket (getMaybeClient clientMaybe)) "exit"
            else putStrLn "Client not found."
            mainLoop sock clientInfo
          "/disconnectAll" -> do
            clientList <- readMVar clientInfo
            mapM_ (\c -> broadcast (clientSocket c) "exit") clientList
            mainLoop sock clientInfo
          _ -> do
            mainLoop sock clientInfo
      else do mainLoop sock clientInfo

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
      clientList <- readMVar clientInfo
      let currentClient = getCurrentClient sock clientList
      msg <- try (recv sock 1024) :: IO (Either SomeException Byte.ByteString)
      case msg of
        Left e -> do
          putStrLn $ "Client " ++ clientName currentClient ++ " Disconneted: " ++ show e
          modifyMVar_ clientInfo $ \clients -> return (removeClients currentClient clients)
          close sock
        Right m -> do
          let msg' = Byte.unpack m
          let wordsMsg = words msg'
          case head wordsMsg of
            -- Save inital name givin to server
            "/init" -> do
              let name = drop (length (head wordsMsg) + 1) msg'
              putStrLn (name ++ " has entered the server!")
              modifyMVar_ clientInfo $ \clients -> return (editClientName (ClientInfo sock name (clientRooms currentClient)) clients)
              broadcast (clientSocket currentClient) "Successfully saved name."
              threadDelay 250000
              broadcast (clientSocket currentClient) "Joined chat room [\"General\"]."
              loop
            -- Private message between two clients
            "/whisper" -> do
              let maybeClient = getClientByMsg msg' clientList
              if isJust maybeClient 
                then do
                  let client = getMaybeClient maybeClient
                  let msg'' = drop (length (head wordsMsg) + length (clientName client) + 2) msg'
                  broadcast (clientSocket client) ("(Whisper) " ++ clientName currentClient ++ ": " ++ msg'')
              else broadcast (clientSocket currentClient) "Client not found."
              loop
            -- Lets client re-name themselves
            "/name" -> do
              let name = drop (length (head wordsMsg) + 1) msg'
              if not (null name)
                then do
                  putStrLn (clientName currentClient ++ " has changed thier name to " ++ name)
                  modifyMVar_ clientInfo $ \clients -> return (editClientName (ClientInfo sock name (clientRooms currentClient)) clients)
                  broadcast (clientSocket currentClient) "Successfully saved name."
                else broadcast (clientSocket currentClient) "Name was empty, save unseccessfull."
              loop
            -- Lets client check current saved name
            "/myName" -> do
              let name = clientName currentClient
              broadcast (clientSocket currentClient) ("Your saved name is: " ++ name)
              loop
            -- List all current client chat rooms
            "/myChatRooms" -> do
              broadcast (clientSocket currentClient) (show (clientRooms currentClient))
              loop
            -- Lists all active chat rooms on server
            "/chatRooms" -> do
              broadcast (clientSocket currentClient) (show (listChatRooms clientList []))
              loop
            -- Allows client to sent to a specific chat room
            "/sendTo" -> do
              let room = wordsMsg !! 1
              let msgToSend = "[" ++ show room ++ "] " ++ clientName currentClient ++ ": " ++ drop (length (head wordsMsg) + length (wordsMsg !! 1) + 2) msg'
              mapM_ (\c -> when (clientSocket c /= sock && room `elem` clientRooms c) $ broadcast (clientSocket c) msgToSend ) clientList
              loop
            -- Lists all members of specific chat room
            "/members" -> do
              let room = wordsMsg !! 1
              broadcast (clientSocket currentClient) (show (listClients room clientList))
              loop
            -- Removes client from desired chat room/s
            "/leave" -> do
              let rooms = drop 1 wordsMsg
              if rooms /= []
                then do
                  clientList' <- readMVar clientInfo
                  mapM_ (\c -> when (clientSocket c /= sock && hasCommonElements (clientRooms c) rooms && clientRooms c /= []) 
                    $ broadcast (clientSocket c) (clientName currentClient ++ " has left the chat room!")) clientList'
                  modifyMVar_ clientInfo $ \clients -> return (leaveClientRooms (ClientInfo sock "" rooms) clients)
                  broadcast (clientSocket currentClient) ("Successfully left chat rooms: " ++ show rooms)
                else broadcast (clientSocket currentClient) ("Please enter a chat room to leave.")
              loop
            -- Adds client to desired chat room/s
            "/join" -> do
              let rooms = drop 1 wordsMsg
              if rooms /= []
                then do
                  modifyMVar_ clientInfo $ \clients -> return (addClientRooms (ClientInfo sock "" rooms) clients)
                  clientList' <- readMVar clientInfo
                  mapM_ (\c -> when (clientSocket c /= sock && hasCommonElements (clientRooms c) rooms && clientRooms c /= []) 
                    $ broadcast (clientSocket c) (clientName currentClient ++ " has joined the chat room!")) clientList'
                  broadcast (clientSocket currentClient) ("Successfully joined chat rooms: " ++ show rooms)
                else broadcast (clientSocket currentClient) ("Please enter a chat room to join.")
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
              mapM_ (\c -> when (clientSocket c /= sock && hasCommonElements (clientRooms c) (clientRooms currentClient)
                && (null (clientRooms c) && null (clientRooms currentClient) || clientRooms c /= [] && clientRooms currentClient /= [])) 
                $ broadcast (clientSocket c) (show (getMatchingRooms (clientRooms c) (clientRooms currentClient)) 
                ++ " " ++ clientName currentClient ++ ": " ++ msg')) clientList
              loop

-- Helper functions

-- broadcast using exception handling
broadcast :: Socket -> String -> IO ()
broadcast sock msg = do
  trySend <- try (sendAll sock (Byte.pack msg)) :: IO (Either SomeException ())
  case trySend of
    Left e -> putStrLn $ "Error sending message: " ++ show e
    Right _ -> pure ()

-- Check if two lists of chat rooms have common elements
hasCommonElements :: Eq a => [a] -> [a] -> Bool
hasCommonElements xs ys = not (null (intersect xs ys))

-- Gets all active chat rooms
listChatRooms :: [ClientInfo] -> [String] -> [String]
listChatRooms [] [] = []
listChatRooms [] rooms = nub rooms
listChatRooms (c:cs) rooms = listChatRooms cs (clientRooms c ++ rooms)

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
addClientRoomsHelper (x:xs) (y:ys) | x `elem` (y:ys) = addClientRoomsHelper xs (y:ys)
                                   | otherwise = x : addClientRoomsHelper xs (y:ys)

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
leaveClientRoomsHelper [] ys = ys
leaveClientRoomsHelper xs (y:ys) | y `elem` xs = leaveClientRoomsHelper (delete y xs) ys
                                 | otherwise = y : leaveClientRoomsHelper xs ys
