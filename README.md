# Haskell Chat (IRC) App
This is a haskell stack chat application.

### To Build Application
Make sure to install latest version of stack

Run "stack build"

### To Run Server
Use the stack command "stack run server"

### To Run Client
You must first update the src/Client.hs file and add your computers IP Address

This line specifically: connect sock $ SockAddrInet 3000 $ tupleToHostAddress (xxx, xxx, xxx, xxx)

Use the stack command "stack run client"

## Example Client Commands
/whisper Alice Hey Alice -- Allows private messaging between two clients

/join BasketBall Hockey ComputerScience -- Allows the client to join specified chat rooms

/leave Hockey ComputerScience -- Allows client to leave specified chat rooms

/name Bob -- Allows client to re-name themselves

/myName -- Gets your saved name from the server

/chatRooms -- Allows client to list all active chat rooms on server

/myChatRooms -- Allows client to list thier own chat rooms

/sendTo BasketBall Did you see the game last night? -- Allows messages to a single specified chat room

/members BasketBall -- Allows client to get list of all members of a chat room

/quit -- Allows client to disconnect from server

/test -- Runs client side test suite

## Example Server Commands
/sendTo BasketBall The Blazers are up by 10! -- Allows the server to send messages to a specified chat room

/sendToAll Shutting down in 15 minutes. -- Allows the server to send messages to all clients

/disconnect Bob -- Allows the server to disconnect specific clients

/disconnectAll -- Allows the server to disconnect all clients

/quit -- Shuts down server.
