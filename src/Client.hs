module Client 
    ( mainClient
    ) where

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as Byte
import System.IO
import Control.Concurrent
import System.Timeout
import Control.Exception

mainClient :: IO ()
mainClient = withSocketsDo $ do
  -- create a TCP socket
  sock <- socket AF_INET Stream defaultProtocol

  -- connect to the server
  connect sock $ SockAddrInet 3000 $ tupleToHostAddress (192, 168, 1, 8)

  -- read and send messages to the server until the user types "/quit"
  loop sock

loop :: Socket -> IO ()
loop sock = do
  putStrLn "Connected!"

  -- Get name from client
  putStrLn "Please enter your name."
  name <- getLine

  send sock $ Byte.pack $ "/init " ++ name

  sendThread <- forkIO $ sendLoop sock
  receiveLoop sock sendThread

  -- sendLoop needs to be non-blocking IO so when server closes connection the client doen't hang
  where
    sendLoop sock = do
      inputReady <- hWaitForInput stdin 0
      if inputReady
        then do
          msg <- hGetLine stdin
          case msg of
            -- Sending signal to close connection from client side
            "/quit" -> do
             send sock $ Byte.pack "exit"
             putStrLn "Goodbye!"
            -- Otherwise send message and loop
            _ -> do
              send sock $ Byte.pack msg
              sendLoop sock
      else do sendLoop sock

    receiveLoop sock sendThread = do
      response <- recv sock 1024
      let msg = Byte.unpack response
      if Byte.null response
        then putStrLn "Server closed the connection."
        else do
          case msg of
            -- Server force closing connection
            "exit" -> do
              send sock $ Byte.pack "exit"
              putStrLn "Server closed the connection."
              killThread sendThread
            -- Otherwise print message and loop
            _ -> do
              putStrLn msg
              receiveLoop sock sendThread