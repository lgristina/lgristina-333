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

      {FromNode, gameStart, GameDescription, GameState} ->
         io:fwrite("~sReceived game start from node ~w.~n",[?id, FromNode]),
         io:fwrite("~s~s~n", [?id, GameDescription]),
         io:fwrite("~s~w~n", [?id, GameState]),
         
         clientLoop();

      {FromNode, getDescribe, LocationDescription, GameState} ->
         io:fwrite("~sReceived location description from node ~w.~n",[?id, FromNode]),
         io:fwrite("~s~s~n", [?id, LocationDescription]),
         io:fwrite("~s~w~n", [?id, GameState]),
         
         clientLoop();

      {FromNode, searchNarrative, SearchNarrative, GameState} ->
         io:fwrite("~sReceived search narrative from node ~w.~n",[?id, FromNode]),
         io:fwrite("~s~s~n", [?id, SearchNarrative]),
         io:fwrite("~s~w~n", [?id, GameState]),
         
         clientLoop();

      {FromNode, ServerNode, attemptCode, GameClientState} ->
         io:fwrite("~sReceived code attempt from node ~w.~n",[?id, FromNode]),
         io:fwrite("~s~w~n", [?id, GameClientState]),
         
         case attemptCode() of
            {win, WinText} ->
               io:fwrite("~s~s~n", [?id, WinText]),
               playAgain(ServerNode);
               
            {quit, QuitText} ->
               io:fwrite("~s~s~n", [?id, QuitText]),
               io:fwrite("Enter \"go 2025\" to keep looking.~n", []),
               clientLoop()
         end;

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

   case ResultAtom of
      go ->
         ResultTextSize = tuple_size(ResultText),
         ResultState = element(ResultTextSize, ResultText);
      search -> 
         ResultTextSize = tuple_size(ResultText),
         ResultState = element(ResultTextSize, ResultText);
      _ ->
         ResultState = State
   end,

   %
   % -- Update the display.
   io:fwrite("~s~p~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit) ->
      io:fwrite("~sThank you for playing.~n", [?id]),
      {gameServer, ServerNode} ! {node(), exit};
   ?else ->
     playLoop(ServerNode, ResultState)  % This is tail recursion, so it's really a jump to the top of playLoop.
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
            NewState = State#state{visitedLocations = NewVisitedLocations, currentLocation = LocationName},
            case FirstVisit of
                true -> 
                  % First visit to 2025.
                  io:fwrite("~s[debug] First visit to 2025. DestAtom: ~s~p~n", [?id, DestAtom, NewState]),
                  {gameServer, ServerNode} ! {node(), startLoc1, DestAtom, NewState};
                false ->
                  % Subsequent visits to 2025.
                  {gameServer, ServerNode} ! {node(), goToLocation, DestAtom, NewState}
            end;
         _ -> 
            % Add the location to the list of visited locations for other destinations.
            FirstVisit = not lists:member(LocationName, Visited),
            if(FirstVisit) ->
               NewVisitedLocations = Visited ++ [LocationName],
               NewState = State#state{visitedLocations = NewVisitedLocations, currentLocation = LocationName},
               {gameServer, ServerNode} ! {node(), goToLocation, DestAtom, NewState};
            ?else ->
               {gameServer, ServerNode} ! {node(), goToLocation, DestAtom, State}
            end
    end;
go([], _ServerNode, _State) ->
    io_lib:format("Where do you want to go: [~w]?", [#state.visitedLocations]).

 
% ----------
% Additions
% ----------

search([_Space | Destination], ServerNode, State = #state{codeProgress = CodeProgress}) ->

   % Get the location module for the destination.
   LocationName = list_to_atom(Destination),
   DestAtom = getLocationModule(Destination),
   LocationModule = atom_to_list(DestAtom),
   
   io:fwrite("~s[debug] This is the location: [~w].~n", [?id, LocationName]),
   
   case updateCode(LocationName, CodeProgress, LocationModule) of
      {ok, UpdatedCodeProgress} ->
         io:format("Updated Code: ~s~n", [UpdatedCodeProgress]),
         % Update the player state.
         NewState = State#state{codeProgress = UpdatedCodeProgress},

         io:fwrite("~s[debug] DestAtom: ~s~p~n", [?id, DestAtom, NewState]),

         {gameServer, ServerNode} ! {node(), searchLocation, DestAtom, NewState};
         
      {error, Reason} ->
         io:fwrite("Error in updating code: ~s~n", [Reason]),
         % Decide how to handle the error. For example, you might want to retry, skip, or exit.
         % This is just a placeholder action:
         gameClient ! {node(), enter, State}
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
      _ -> {error, "Invalid Location"}
   end.

getLocationData(Location) ->
   % Get the location data based on the location module.
   case Location of
      '2025' -> "a";
      '2015' -> "l";
      '2005' -> "p";
      '1995' -> "a";
      '1985' -> "c";
      '1975' -> "a";

      % -- Otherwise...
      _ -> {error, "Invalid Location"}

   end.

getValidChoices(CurrentLocation) ->
   case CurrentLocation of
      '1975' -> ['1995'];
      '1985' -> ['2025'];
      '1995' -> ['2015'];
      '2005' -> ['1985'];
      '2015' -> ['2025'];
      '2025' -> ['1975', '2005'];

      % -- Otherwise...
      _ -> []
   end.

updateCode(CurrentLocation, CodeProgress, LocationModule) ->
    LocationData = getLocationData(CurrentLocation),
    Choices = getValidChoices(CurrentLocation),
    LetterLocation = string:split(LocationModule, "loc"),
    LetterLookupTuple = string:to_integer(tl(LetterLocation)),
    LetterLookup = element(1, LetterLookupTuple),

    io:fwrite("~s[debug] LocationData: [~w], Choices: [~w], LetterLookup: [~w] .~n", [?id, LocationData, Choices, LetterLookup]),

   case LocationData of
         {error, "Invalid Location"} ->
            io:fwrite("Invalid Location. Cannot Retrieve Code. Please Try Again.~s~n", [CurrentLocation]),
            {error, invalid_code_fragment};
         _ ->
            case LetterLocation of
               "" ->
                     io:fwrite("Invalid Letter Location. Please Try Again.~s~n", [CurrentLocation]),
                     {error, invalid_letter_location};
               _ ->
                     % Replace character at the specified index
                     UpdatedCodeProgress = lists:sublist(CodeProgress, LetterLookup - 1) ++
                                          [LocationData] ++
                                          lists:sublist(CodeProgress, LetterLookup + 1, length(CodeProgress) - LetterLookup),
                     io:fwrite("Updated Code: ~s~n", [UpdatedCodeProgress]),
                     {ok, UpdatedCodeProgress}
            end
   end.
   

attemptCode() ->
   io:fwrite("~s[debug] Attempting code.~n", [?id]),
   io:fwrite("Enter the code or enter anything else to go back to 2025: ", []),
   {ok, Code} = io:fread("", "~s"),
   io:fwrite("~s[debug] Code: ~s~n", [?id, Code]),

   case Code of
      "alpaca" -> 
         {win, "Congratulations, you win!"};
      _ -> 
         {quit, "Going back to the present"}
   end.

playAgain(ServerNode) ->
   io:fwrite("Would you like to play again? [y/n]: ", []),
   {ok, Response} = io:fread("", "~s"),
   io:fwrite("~s[debug] Response: ~s~n", [?id, Response]),
   case Response of
      "y" -> start();
      _ -> {gameServer, ServerNode} ! {node(), exit}
   end.
