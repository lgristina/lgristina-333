-module(loc3).
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
   SomePlace = whereis(loc3),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(loc3);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(loc3, LocPid),
   io:fwrite(", registered as ~w.~n",[loc3]),
   % Send ourselves to the gameServer.
   io:fwrite("~sNotifying server on node ~w.~n",[?id, ServerNode]),
   {gameServer, ServerNode} ! {node(), registerNewLocation, loc3},
   % Initialize server monitoring.
   loc3 ! {monitor, ServerNode},
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

      {_FromNode, enter, GameClientNode, ClientState}  ->
         io:fwrite("~sA gameClient on ~w is entering loc3.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), describe(ClientState)},
         locationLoop();

      {_FromNode, search, GameClientNode}  ->
         io:fwrite("~sA gameClient on ~w is searching loc3.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), narrative()},
         locationLoop();

      {FromNode, _Any}  ->
         io:fwrite("~sReceived request [~p] from node ~w.~n",[?id, _Any, FromNode]),
         locationLoop()
   end.


%--------
% Private
%--------
describe(ClientState) ->
   Visited = lists:member("2025", ClientState),
   case Visited of
      true ->
         io:fwrite("(1) Welcome back! You need to head down another path to continue looking for the code. ~n", []),
         io:fwrite("(1) Or if you have collected all of the code, you can head to the code room [codeRoom]. ~n", []);
      false ->
         io:fwrite("(1) Welcome. The year is 2025. ~n  
                        You have been selected to go on a mission to secure a code lost in time ~n", []),
         io:fwrite("(1) The rules for time travel are as follows: ~n", []),
         io:fwrite("(1)   1. You can travel forward or backward 20 years at a time.~n", []),
         io:fwrite("(1)   2. Only in the present can you make a 50 year time jump to the past.~n", []),
         io:fwrite("(1)   3. As you travel, you must find the person that holds the a part of the code.~n", [])
   end.

narrative() ->
   io:fwrite("The person who holds the code is: ~n", []).