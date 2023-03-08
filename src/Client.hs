module Client 
    ( mainClient
    ) where

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as Byte
import System.IO
import Control.Concurrent
import Data.List

mainClient :: IO ()
mainClient = withSocketsDo $ do
  -- create a TCP socket
  sock <- socket AF_INET Stream defaultProtocol

  -- connect to the server

  -- Desktop IP Address
  connect sock $ SockAddrInet 3000 $ tupleToHostAddress (192, 168, 1, 8)

  -- Laptop IP Address @ School
  -- connect sock $ SockAddrInet 3000 $ tupleToHostAddress (10, 200, 224, 219)

  -- read and sendAll messages to the server until the user types "/quit"
  loop sock

loop :: Socket -> IO ()
loop sock = do
  msgVar <- newMVar []
  putStrLn "Connected!"

  -- Get name from client
  putStrLn "Please enter your name."
  name <- getLine

  sendAll sock $ Byte.pack $ "/init " ++ name

  sendThread <- forkIO $ sendLoop msgVar
  receiveLoop sendThread msgVar

  -- sendAllLoop needs to be non-blocking IO so when server closes connection the client doen't hang
  where
    sendLoop msgVar = do
      inputReady <- hWaitForInput stdin 0
      if inputReady
        then do
          msg <- hGetLine stdin
          case msg of
            -- sendAlling signal to close connection from client side
            "/quit" -> do
             sendAll sock $ Byte.pack "exit"
             putStrLn "Goodbye!"
            -- Start the test suite
            "/test" -> do
              testJoinRooms sock msgVar
              testLeaveChatRooms sock msgVar
              testChangeName sock msgVar
              sendLoop msgVar
            -- Otherwise sendAll message and loop
            _ -> do
              sendAll sock $ Byte.pack msg
              sendLoop msgVar
      else do sendLoop msgVar

    receiveLoop sendThread msgVar = do
      response <- recv sock 1024
      let msg = Byte.unpack response
      if Byte.null response
        then putStrLn "Server closed the connection."
        else do
          case msg of
            -- Server force closing connection
            "exit" -> do
              sendAll sock $ Byte.pack "exit"
              putStrLn "Server closed the connection."
              killThread sendThread
            -- Otherwise print message and loop
            _ -> do
              putStrLn msg
              modifyMVar_ msgVar (\messages -> return (msg : messages))
              receiveLoop sendThread msgVar

-- Test Functions
testJoinRooms :: Socket -> MVar [String] -> IO ()
testJoinRooms sock msgVar = do
  sendAll sock $ Byte.pack "/myChatRooms"
  -- Need to wait for server response
  threadDelay 1000000
  messages <- readMVar msgVar
  let originalRooms = read (head messages) :: [String]
  sendAll sock $ Byte.pack "/join"
  -- Need to wait for server response
  threadDelay 1000000
  sendAll sock $ Byte.pack "/myChatRooms"
  -- Need to wait for server response
  threadDelay 1000000
  messages' <- readMVar msgVar
  let newRooms = read (head messages') :: [String]
  if newRooms == originalRooms
    then do
      let rooms = "Room1 Room2 1502 BasketBall ComputerScience Testing"
      let wordRooms = words rooms
      sendAll sock $ Byte.pack $ "/join " ++ rooms
      -- Need to wait for server response
      threadDelay 1000000
      sendAll sock $ Byte.pack "/myChatRooms"
      -- Need to wait for server response
      threadDelay 1000000
      messages'' <- readMVar msgVar
      let newRooms' = read (head messages'') :: [String]
      if newRooms' == (wordRooms ++ originalRooms)
        then putStrLn "Test Join: Passed"
      else putStrLn "Test Join: Failed"
    else putStrLn "Test Join: Failed"

testLeaveChatRooms :: Socket -> MVar [String] -> IO ()
testLeaveChatRooms sock msgVar = do
  let command = "/myChatRooms"
  sendAll sock $ Byte.pack command
  -- Wait for server response
  threadDelay 1000000
  messages <- readMVar msgVar
  let originalRooms = read (head messages) :: [String]
  sendAll sock $ Byte.pack "/leave"
  -- Wait for server response
  threadDelay 1000000
  sendAll sock $ Byte.pack command
  -- Wait for server response
  threadDelay 1000000
  messages' <- readMVar msgVar
  let newRooms = read (head messages') :: [String]
  if newRooms == originalRooms
    then do
      let rooms = "Room2 1502 BasketBall Testing"
      let wordRooms = words rooms
      sendAll sock $ Byte.pack $ "/leave " ++ rooms
      -- Wait for server response
      threadDelay 1000000
      sendAll sock $ Byte.pack command
      -- Wait for server response
      threadDelay 1000000
      messages'' <- readMVar msgVar
      let newRooms' = read (head messages'') :: [String]
      if newRooms' == (originalRooms \\ wordRooms)
        then putStrLn "Test Leave: Passed"
      else putStrLn "Test Leave: Failed"
    else putStrLn "Test Leave: Failed"

testChangeName :: Socket -> MVar [String] -> IO ()
testChangeName sock msgVar = do
  let name = "This is a test"
  sendAll sock $ Byte.pack $ "/name " ++ name
  -- Wait for server response
  threadDelay 1000000
  sendAll sock $ Byte.pack "/myName"
  -- Wait for server response
  threadDelay 1000000
  messages <- readMVar msgVar
  let msg = drop (length (head messages) - length name) (head messages)
  if msg == name
    then do
      let name' = "This is also a test. Testing... Testing..."
      sendAll sock $ Byte.pack $ "/name " ++ name'
      -- Wait for server response
      threadDelay 1000000
      sendAll sock $ Byte.pack "/myName"
      -- Wait for server response
      threadDelay 1000000
      messages' <- readMVar msgVar
      let msg' = drop (length (head messages') - length name') (head messages')
      if msg' == name'
        then do 
          let name'' = ""
          sendAll sock $ Byte.pack $ "/name " ++ name''
          -- Wait for server response
          threadDelay 1000000
          messages'' <- readMVar msgVar
          let msg'' = head messages''
          if msg'' == "Name was empty, save unsuccessfull."
            then putStrLn "Test Name: Passed"
          else putStrLn "Test Name: Failed"
      else putStrLn "Test Name: Failed"
  else putStrLn "Test Name: Failed"
