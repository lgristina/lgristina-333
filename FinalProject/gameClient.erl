% gameClient.erl - A Distributed Adventure Game Client

-module(gameClient).
-author('Alan G. Labouseur').
-author('Luca Gristina').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game client: ").


%--------
% Public
%--------

-export([start/0, start/1]).

-record(state, {currentLocation, codeProgress = "______", visitedLocations = []}).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this game client process.
   io:fwrite("~sStarting Distributed Adventure Game Client (pid ~w) on node ~w.~n",[?id, self(), node()]),
   GameClientPid = spawn(fun clientLoop/0),
   io:fwrite("~sSpawned game client with pid ~w",[?id, GameClientPid]),
   % We want to publish this process in Erlang's local process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(gameClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(gameClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's local process registry.
   register(gameClient, GameClientPid),
   io:fwrite(", registered as ~w.~n",[gameClient]),

   InitState = #state{currentLocation = start, codeProgress = "______", visitedLocations = []},

   io:fwrite("Welcome to Time Travelers. Enter \"go 2025\" to start playing.~n", []),

   % Initialize server monitoring.
   gameClient ! {monitor, ServerNode},
   % -- Begin the play loop
   playLoop(ServerNode, InitState).


%---------------------------------
% Private, but accepting messages.
%---------------------------------
clientLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         clientLoop();

      {nodedown, Node} ->
         % This client monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
         % TODO: exit the playLoop too.
         exit(normal);

      {FromNode, _Any}  ->
         io:fwrite("~sReceived message [~p] from node ~w.~n",[?id, _Any, FromNode]),
         clientLoop()
   end.


%---------
% Private
%---------

playLoop(ServerNode, State) ->
   % -- Get a line of input from the user.
   Line = io:get_line(io_lib:format("~s[play] Enter action or help -] ", [?id])),  % Line is returned as a string.
   {ResultAtom, ResultText} = processCommand(Line, ServerNode, State),
   %
   % -- Update the display.
   io:fwrite("~s~s~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit) ->
      io:fwrite("~sThank you for playing.~n", [?id]);
   ?else ->
     playLoop(ServerNode, State)  % This is tail recursion, so it's really a jump to the top of playLoop.
   end. % if


processCommand(Line, ServerNode, State) ->
   % Do some elementary parsing of the line in two parts:
   % 1. Remove the trailing newline charater.
   Command = lists:sublist(Line, length(Line)-1),  % (Because Line is a character list ending with a linefeed.)
   % 2. Break the line into two parts: before the space and after the space (if there's even a space)
   Verb = lists:takewhile( fun(Element) -> Element /= 32 end, Command),
   Noun = lists:dropwhile( fun(Element) -> Element /= 32 end, Command),
   %
   case Verb of
      "help"   ->  {help,    helpText()};
      "quit"   ->  {quit,    "Quitting."};
      "q"      ->  {quit,    "Quitting."};
      "nodes"  ->  {nodes,   listNodes()};
      "server" ->  {server,  server(ServerNode)};
      "go"     ->  {go,      go(Noun, ServerNode, State)};
      "search" ->  {search,  search(Noun, ServerNode, State)};
      "code"  ->   {code,    State#state.codeProgress};
      "choices" -> {choices, getValidChoices(State#state.currentLocation)};
      "visited" -> {visited, State#state.visitedLocations};
      % -- Otherwise...
      _Else  -> {unknownCommand, "Silly human."}
   end.

helpText() ->
   io_lib:format("Commands: [help], [quit], [nodes], [server], [go <location>], [search <location>], [choices], [code]", []).

listNodes() ->
   io_lib:format("This node: ~w~n", [node()]) ++   % No ?id here because it will be supplied when printed above.
   io_lib:format("~sOther nodes in our cluster: ~w", [?id, nodes()]).

server(ServerNode) ->
   KnownNode = lists:member(ServerNode, nodes()),
   if KnownNode ->
      io_lib:format("Talking to game server on node ~w, which is known to be in our cluster.", [ServerNode]);
   ?else ->
      io_lib:format("Talking to game server on node ~w, which is NOT known to be in our cluster, and that may be a problem.", [ServerNode])
   end. % if


go([_Space | Destination], ServerNode, State = #state{visitedLocations = Visited}) ->
    % Get the location module for the destination.
    LocationName = list_to_atom(Destination),
    DestAtom = getLocationModule(Destination),
    io:fwrite("~s[debug] This is the location and destatom: [~w], [~w].~n", [?id, LocationName, DestAtom]),

    case LocationName of
        '2025' ->
            FirstVisit = not lists:member('2025', Visited),
            NewVisitedLocations = Visited ++ ['2025'],
            NewState = State#state{visitedLocations = NewVisitedLocations},
            case FirstVisit of
                true -> 
                  % First visit to 2025.
                  {gameServer, ServerNode} ! {node(), startLoc1, DestAtom};
                false ->
                  % Subsequent visits to 2025.
                  {gameServer, ServerNode} ! {node(), goToLocation, DestAtom, NewState}
            end;
        _ -> 
            % Add the location to the list of visited locations for other destinations.
            NewVisitedLocations = Visited ++ [LocationName],
            NewState = State#state{visitedLocations = NewVisitedLocations},
            {gameServer, ServerNode} ! {node(), goToLocation, DestAtom, NewState}
    end,

    ok;
go([], _ServerNode, _State) ->
    io_lib:format("Where do you want to go?", []).

 
% ----------
% Additions
% ----------

search(CurrentLocation, ServerNode, State = #state{codeProgress = CodeProgress}) ->
   
   case updateCode(list_to_atom(CurrentLocation), CodeProgress) of
      {ok, UpdatedCodeProgress} ->
         io:format("Updated Code:~s~n", [UpdatedCodeProgress]),
         % Get the next location.
         NextLocation = getNextLocation(CurrentLocation),
         % Update the player state.
         NewState = State#state{
               currentLocation = NextLocation,
               codeProgress = UpdatedCodeProgress
         },
         {gameServer, ServerNode} ! {node(), searchLocation, NewState#state.currentLocation},
         go([NextLocation], ServerNode, NewState);
      {error, Reason} ->
         io:fwrite("Error in updating code: ~s~n", [Reason]),
         % Decide how to handle the error. For example, you might want to retry, skip, or exit.
         % This is just a placeholder action:
         State
   end.

   

getLocationModule(Year) ->
   % Get the location module based on the year.
   %io:fwrite("~s[debug] Year: [~w] and is a string?: [~w].~n", [?id, Year, is_list(Year)]),
   case list_to_atom(Year) of
      '1975' -> loc6;
      '1985' -> loc5;
      '1995' -> loc4;
      '2005' -> loc3;
      '2015' -> loc2;
      '2025' -> loc1;

      % -- Otherwise...
      _ -> noLoc % Needed a no op
   end.

getLocationData(Location) ->
   % Get the location data based on the location module.
   case Location of
      loc1 -> "a";
      loc2 -> "l";
      loc3 -> "p";
      loc4 -> "a";
      loc5 -> "c";
      loc6 -> "a";

      % -- Otherwise...
      _ -> ""

   end.

getNextLocation(CurrentLocation) ->
   % Get the next location based on the current location.
   Choices = getValidChoices(CurrentLocation),
   io:format("You can tavel to the following locations: ~p. Enter your choice: ", [Choices]),
   {ok, [NextLocation]} = io:fread("", "~d"),
   case lists:member(NextLocation, Choices) of
      true  -> 
         NextLocation;
      false ->
         io:fwrite("Invalid Choice. Please Try Again.~n"), 
         getNextLocation(CurrentLocation)
   end.

getValidChoices(CurrentLocation) ->
   case list_to_atom(CurrentLocation) of
      1975 -> [1995];
      1985 -> [2005];
      1995 -> [2015];
      2005 -> [1985];
      2015 -> [2025];
      2025 -> [1975, 2005];

      % -- Otherwise...
      _ -> []
   end.

updateCode(CurrentLocation, CodeProgress) ->
   LocationModule = getLocationModule(CurrentLocation),
   Choices = getValidChoices(CurrentLocation),
   IsValid = not lists:member(CurrentLocation, Choices),

 case IsValid of
        false ->
            io:fwrite("Invalid Location. Please Try Again.~s~n", [LocationModule]),
            {error, invalid_location};
        true ->
            case getLocationData(LocationModule) of
                "" ->
                    io:fwrite("Invalid Location. Cannot Retrieve Code. Please Try Again.~s~n", [LocationModule]),
                    {error, invalid_code_fragment};
                CodeFragment ->
                    case string:substr(atom_to_list(CurrentLocation), 4, 1) of
                        "" ->
                            io:fwrite("Invalid Letter Location. Please Try Again.~s~n", [LocationModule]),
                            {error, invalid_letter_location};
                        LetterLocation ->
                            UpdatedCodeProgress = string:replace(CodeProgress, LetterLocation, CodeFragment, first),
                            io:fwrite("Updated Code: ~s~n", [UpdatedCodeProgress]),
                            {ok, UpdatedCodeProgress}
                    end
            end
    end.
