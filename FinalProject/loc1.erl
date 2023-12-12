% loc1.erl - Distributed Adventure Game Location 1

-module(loc1).
-author('Alan G. Labouseur').
-author('Luca Gristina').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- location 1: ").


%--------
% Public
%--------

-export([start/0, start/1]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this location process.
   io:fwrite("~sStarting Location 1 (pid ~w) on node ~w.~n",[?id, self(), node()]),
   LocPid = spawn(fun locationLoop/0),
   io:fwrite("~sSpawned location with pid ~w",[?id, LocPid]),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(loc1),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(loc1);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(loc1, LocPid),
   io:fwrite(", registered as ~w.~n",[loc1]),
   % Send ourselves to the gameServer.
   io:fwrite("~sNotifying server on node ~w.~n",[?id, ServerNode]),
   {gameServer, ServerNode} ! {node(), registerNewLocation, loc1},
   % Initialize server monitoring.
   loc1 ! {monitor, ServerNode},
   ok.



%---------------------------------
% Private, but accepting messages.
%---------------------------------

locationLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         locationLoop();

      {nodedown, Node} ->
         % This location monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
         exit(normal);

      {_FromNode, startGame, GameClientNode, GameClientState}  ->
         io:fwrite("~sA gameClient on ~w is entering loc1.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), gameStart, gameDescription(), GameClientState},
         locationLoop();

      {_FromNode, enter, GameClientNode, GameClientState}  ->
         io:fwrite("~sA gameClient on ~w is entering loc1.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), getDescribe, describe(), GameClientState},
         locationLoop();

      {_FromNode, search, GameClientNode, GameClientState}  ->
         io:fwrite("~sA gameClient on ~w is searching loc1.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), searchNarrative, narrative(), GameClientState},
         locationLoop();

      {FromNode, _Any}  ->
         io:fwrite("~sReceived request [~p] from node ~w.~n",[?id, _Any, FromNode]),
         locationLoop()
   end.


%--------
% Private
%--------
describe() ->
   io:fwrite("(1) Welcome back! You need to head down another path to continue looking for the code. ~n", []),
   io:fwrite("(1) Or if you have collected all of the code, you can head to the code room [codeRoom]. ~n", []).


narrative() ->
   io:fwrite("You go look for the person with the code fragment... ~n", []),
   io:fwrite("The person who holds the code tells you that it is 'a' ~n", []).

gameDescription() ->
   io:fwrite("(1) Welcome. The year is 2025 (present). You have been selected to go on a mission to secure a code lost in time ~n", []),
   io:fwrite("(1) The rules for time travel are as follows: ~n", []),
   io:fwrite("(1)   1. You can travel forward or backward 20 years at a time depending on your location in time.~n", []),
   io:fwrite("(1)   2. Only in the present can you make a 50 year time jump to the past to head down the other path.~n", []),
   io:fwrite("(1)   3. As you travel, you must find the person that holds a part of the code.~n", []),
   io:fwrite("(1)   4. If you collect all of the code, you can head to the code room.~n", []).