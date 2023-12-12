-module(loc4).
-author('Alan G. Labouseur').
-author('Luca Gristina').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- location 4: ").


%--------
% Public
%--------

-export([start/0, start/1]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this location process.
   io:fwrite("~sStarting Location 4 (pid ~w) on node ~w.~n",[?id, self(), node()]),
   LocPid = spawn(fun locationLoop/0),
   io:fwrite("~sSpawned location with pid ~w",[?id, LocPid]),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(loc4),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(loc4);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(loc4, LocPid),
   io:fwrite(", registered as ~w.~n",[loc4]),
   % Send ourselves to the gameServer.
   io:fwrite("~sNotifying server on node ~w.~n",[?id, ServerNode]),
   {gameServer, ServerNode} ! {node(), registerNewLocation, loc4},
   % Initialize server monitoring.
   loc4 ! {monitor, ServerNode},
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

      {_FromNode, enter, GameClientNode, GameClientState}  ->
         io:fwrite("~sA gameClient on ~w is entering loc4.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), getDescribe, describe(), GameClientState},
         locationLoop();

      {_FromNode, search, GameClientNode, GameClientState}  ->
         io:fwrite("~sA gameClient on ~w is searching loc4.~n",[?id, GameClientNode]),
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
   io:fwrite("(4) You wake up disoriented in Las Vegas in 1995. You are at the third stop before you get to head back to the present ~n", []),
   io:fwrite("(4) You need to find the person with the code fragment! [search 1995]. ~n", []),
   io:fwrite("(4) Or if you have already collected the code fragment, keep moving into the future [go 2015]. ~n", []).


narrative() ->
   io:fwrite("(4) You go out to look for the person with the code fragment... ~n", []),
   io:fwrite("(4) You head into the bar you were instructed to go to and find someone you would not have expected~n", []),
   io:fwrite("(4) The person who holds the code tells you that it is 'a' ~n", []).