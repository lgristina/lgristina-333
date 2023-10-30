%%%
%%% woa.erl - World of Alan. It's like World of Warcraft if it were Zork.
%%%

-module(timeTravelers).
-author('Luca Gristina').
-author('Alan G. Labouseur').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.

%%%
%%% Public
%%%
-export([start/0]).

start() ->
   % -- Spawn the server process.
   Server = spawn(fun serverLoop/0),
   % -- Display the initial location description.
   io:fwrite(locations(0), []),
   io:fwrite("~n~n", []),
   % -- Kick off the game loop.
   gameLoop(Server, 0).


%%%
%%% Private
%%%
gameLoop(Server, CurrentLocale) ->
   % -- Get input from the player.
   {ok, Input} = io:fread("Enter one of the given choices or quit -] ", "~s"),  % Input gets returned as a list from io:fread.
   [Command | _] = Input,                                                  % Because Input is a list.
   %
   % -- Process the player's input/command into a NewLocale and Description.
   {NewLocale, Description} = processCommand(CurrentLocale, Command, Server),
   %
   % -- Update the display.
   io:fwrite("~n", []),
   io:fwrite(Description, []),
   io:fwrite("~n~n", []),
   %
   % -- Quit or Recurse.
   if (NewLocale < 0) ->
     io:fwrite("I guess you wont be able to study... unless you won then good luck studying~n",[]);
   ?else ->
     gameLoop(Server, NewLocale)  % This is tail recursion, so it's really a jump to the top of gameLoop.
   end. % if

processCommand(CurrentLocale, Command, Server) ->
   case Command of
      % -- Time travel directions
      "past" -> makeMove(Server, {CurrentLocale, past});
      "20" -> makeMove(Server, {CurrentLocale, 20});
      "50" -> makeMove(Server, {CurrentLocale, 50});
      "test"     -> makeMove(Server, {CurrentLocale, test});
      "search25" -> makeMove(Server, {CurrentLocale, search25});
      "search15"     -> makeMove(Server, {CurrentLocale, search15});
      "search05"  -> makeMove(Server, {CurrentLocale, search05});
      "search95"     -> makeMove(Server, {CurrentLocale, search95});
      "search85"  -> makeMove(Server, {CurrentLocale, search85});
      "search75"     -> makeMove(Server, {CurrentLocale, search75});
      "2025"     -> makeMove(Server, {CurrentLocale, 2025});
      "2015"     -> makeMove(Server, {CurrentLocale, 2015});
      "1995"     -> makeMove(Server, {CurrentLocale, 1995});
      "1985"     -> makeMove(Server, {CurrentLocale, 1985});
      "alpaca"   -> makeMove(Server, {CurrentLocale, alpaca});
      % -- Other commands
      "quit"  -> {-1, "Thank you for playing."};
      "look"  -> {CurrentLocale, locations(CurrentLocale)};
      "help"  -> {CurrentLocale, helpText()};
      % -- Otherwise...
      _Else   -> {CurrentLocale, "I don't understand."}  % Starting _Else wiht "_" prevents the "unused" error.
   end.

makeMove(ServerPid, MoveTuple) -> remote(ServerPid, MoveTuple).

remote(ToPid, Request) ->
   ToPid ! {self(), Request},
   receive
      {ToPid, Response} -> Response  % -- This waits for a response from ToPid.
   end.

serverLoop() ->
   receive
      {FromPid, {CurrentLocale, Direction}} ->
         {NewLocale, Description} = mapper(CurrentLocale, Direction),
         FromPid ! {self(), {NewLocale, Description}},
         serverLoop();
      {FromPid, _} ->
         FromPid ! {self(), "Internal error: You are lost in time. Nice going, Traveler."},
         serverLoop()
   end.

% -- Inital Decisions for travel
% present (2025)
mapper(0, past)   -> {14, locations(14)}; % sends to choice between 1975 and 2005
mapper(0, search25) -> {6, locations(6)}; % search for part of code
mapper(0, test) -> {12, locations(12)}; % after getting full code, test answer.

% 2005
mapper(1, 1985)   -> {2, locations(2)}; % go further into the past (to 1985)
mapper(1, search05)    -> {7, locations(7)}; % search for part of code

% 1985
mapper(2, 2025)    -> {0, locations(0)}; % head back to the present (to 2025)
mapper(2, search85)    -> {8, locations(8)}; % search for part of code

% 1975
mapper(3, 1995)    -> {4, locations(4)}; % head back into the future (to 1995)
mapper(3, search75)    -> {9, locations(9)}; % search for part of code

% 1995
mapper(4, 2015)    -> {5, locations(5)}; % head further into the future (to 2015)
mapper(4, search95)    -> {10, locations(10)}; % search for part of code

% 2015
mapper(5, 2025)    -> {1, locations(1)}; % Head back to the present
mapper(5, search15)    -> {11, locations(11)}; % search for the part of code


% -- After searching the decade for the portion of the code...
mapper(6, past)    -> {14, locations(14)}; 
mapper(6, test)    -> {12, locations(12)}; 

mapper(7, 1985)   -> {2, locations(2)};
mapper(7, _)  -> {0, locations(0)};

mapper(8, 2025)   -> {0, locations(0)};
mapper(8, _)  -> {0, locations(0)};

mapper(9, 1995)   -> {4, locations(4)};
mapper(9, _)  -> {0, locations(0)};

mapper(10, 2015)   -> {5, locations(5)};
mapper(10, _)  -> {0, locations(0)};

mapper(11, 2025)   -> {0, locations(0)};
mapper(11, _)  -> {0, locations(0)};

mapper(12, alpaca)   -> {13, locations(13)};
mapper(12, _)  -> {0, locations(0)};

mapper(14, 20) -> {1, locations(1)};
mapper(14, 50) -> {3, locations(3)};

mapper(Current, _) -> {Current, "You are breaking the rules of time travel."}.


%% These location descriptions DO NOT end with ~n newlines. The newline is taken care of up in the display code.
locations(0) -> "0. 2025~nThis is the present. You have three choices:~nYou can travel 20 or 50 years into the past (past)~nYou can search for a piece of the code (search25)~nYou can try your code (test): ";
locations(1) -> "1. 2005~nYou are 20 years in the past.~nYou can start your search for the code (search05) or you can travel another 20 yrs into the past (1985): ";
locations(2) -> "2. 1985~nYou are 40 years in the past~nYou can search for he piece of the code (search85) or you can travel back to the present (2025): ";
locations(3) -> "3. 1975~nYou are 50 years in the past~nYou can search for he piece of the code (search75) or you can travel 20 yrs in the future (1995): ";
locations(4) -> "4. 1995~nYou are 30 years in the past~nYou can search for he piece of the code (search95) or you can travel 20 yrs in the future (2015): ";
locations(5) -> "5. 2015~nYou are 10 years in the past~nYou can search for he piece of the code (search15) or you can travel back to the present (2025): ";
locations(6) -> "6. 2025~nYou track down the man who knows the last value of the code~nAfter some convincing he whispers \"The last letter to the code is \'a\'\"~nHave you found the entire code? (test) if not, time to head to the past! (past): ";
locations(7) -> "7. 2005~nYou track down the man who knows the fourth value of the code~nAfter some convincing he whispers \"The last letter to the code is \'a\'\"~nYou must keep looking... head futher into the past (1985): ";
locations(8) -> "8. 1985~nYou track down the man who knows the second value of the code~nAfter some convincing he whispers \"The last letter to the code is \'l\'\"~nYou must keep looking... head to the present and take another route (2025): ";
locations(9) -> "9. 1975~nYou track down the man who knows the first value of the code~nAfter some convincing he whispers \"The last letter to the code is \'a\'\"~nYou must keep looking... start heading toward the present (1995): ";
locations(10) -> "10. 1995~nYou track down the man who knows the third value of the code~nAfter some convincing he whispers \"The last letter to the code is \'p\'\"~n You must keep looking... start heading toward the present (2015): ";
locations(11) -> "11. 2015~nYou track down the man who knows the fifth value of the code~nAfter some convincing he whispers \"The last letter to the code is \'c\'\"~nYou must keep looking... head to the present to take another route or test your code (2025): ";
locations(12) -> "12. 2025~nYou go the monitor and it reads~n\"Please enter the code: \" ";
locations(13) -> "13. Congratulations you are now able to access Alans presentations on his website";
locations(14) -> "14. Do you want to travel to 20 years (20) or 50 years (50) into the past?".

helpText() -> "You can enter the commands provided by the previous prompt".
