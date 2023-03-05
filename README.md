# newApp
This is a haskell stack application
It is an internet relay chat application

### To Run Server
Use the stack command "stack run server"

### To Run Client
Use the stack command "stack run client"

## Example Client Commands
/whisper Alice Hey Alice -- Allows private messaging between two clients
/join BasketBall Hockey ComputerScience -- Allows the client to join specified chat rooms
/leave Hockey ComputerScience -- Allows client to leave specified chat rooms
/name Bob -- Allows client to re-name themselves
/chatRooms -- Allows client to list all active chat rooms on server
/myChatRooms -- Allows client to list thier own chat rooms
/sendTo BasketBall Did you see the game last night? -- Allows messages to a single specified chat room
/members BasketBall -- Allows client to get list of all members of a chat room
/quit -- Allows client to disconnect from server

## Example Server Commands
/sendTo BasketBall The Blazers are up by 10! -- Allows the server to send messages to a specified chat room
/sendToAll Shutting down in 15 minutes. -- Allows the server to send messages to all clients
/disconnect Bob -- Allows the server to disconnect specific clients
/quit -- Shuts down server.