%
% tttServer.erl
%
-module(tttServer).
-author('Alan G. Labouseur').
-author('Luca Gristina').

-define(else, true).
-define(id, "-- server: ").


%
% Public
%
-export([start/0]).

start() ->
   io:fwrite("~sTTT server started on node ~w (pid ~w) ", [?id, node(), self()]),
   ServerPid = spawn(fun serverLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttServer),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttServer);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttServer, ServerPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ServerPid, tttServer]).


%
% Private, but accepting messages sent to serverLoop because of the way it was spawned.
%
serverLoop() -> 
   receive
      {FromNode, start_game} ->
         io:fwrite("~sReceived [start_game] request from node ~w.~n",[?id, FromNode]),
         io:fwrite("~sSending [player_turn] response to node ~w.~n",[?id, FromNode]),
         InitialBoard = [0,0,0, 0,0,0, 0,0,0],
         {tttClient, FromNode} ! {node(), player_turn, InitialBoard},
         serverLoop();

      {FromNode, process_player_turn, Board, PlayerPos} ->
         io:fwrite("~sReceived [process_player_turn] request from node ~w with board ~w and player move ~w.~n",[?id, FromNode, Board, PlayerPos]),
         NewBoard = processPlayerMove(PlayerPos, Board),
         case checkWin(NewBoard, 1) of
            true ->
               {tttClient, FromNode} ! {node(), player_win, NewBoard};
            false ->
               {tttClient, FromNode} ! {node(), computer_turn, NewBoard},
               serverLoop()
         end;

      {FromNode, computer_turn, Board} ->
         io:fwrite("~sReceived [computer_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
         NewBoard = computerTurn(Board),
         case checkWin(NewBoard, 2) of
            true ->
               {tttClient, FromNode} ! {node(), computer_win, NewBoard};
            false ->
               {tttClient, FromNode} ! {node(), player_turn, NewBoard},
               serverLoop()
         end;

      {FromNode, _Any} ->
         io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
         serverLoop()
   end.


%
% Private (not even accepting messages)
%
processPlayerMove(Position, Board) ->
   Target = lists:nth(Position, Board),
   if(Target == 0) ->
      io:fwrite("~sPlacing an X into position ~w.~n", [?id, Position]),
      UpdatedBoard = replaceInList(1, Position, Board),
      UpdatedBoard;
   ?else ->
      io:fwrite("~sCannot place an X into position ~w.~n", [?id, Position]),
      Board
   end.


replaceInList(Value, Position, List) ->
   {Part1, Part2} = lists:split(Position-1, List),     
   [_ | Tail] = Part2,                                 
   Part1 ++ [Value] ++ Tail.                           


checkWin(Board, Player) ->
    Rows = lists:chunk(3, Board),
    Columns = [lists:nth(Index, Board) || Index <- [1, 2, 3]],
    Diagonals = [[lists:nth(1, Board), lists:nth(5, Board), lists:nth(9, Board)],
                 [lists:nth(3, Board), lists:nth(5, Board), lists:nth(7, Board)]],
    checkLines(Rows, Player) orelse
    checkLines(Columns, Player) orelse
    checkLines(Diagonals, Player).


checkLines(Lines, Player) ->
    lists:any(fun(Line) -> 
              lists:all(fun(Mark) -> 
                        Mark == Player end, Line)
              end, Lines).


computerTurn(Board) ->
   WinPatterns = [{1,2,3}, {4,5,6}, {7,8,9}, {1,4,7}, {2,5,8}, {3,6,9}, {1,5,9}, {3,5,7}],
   EmptySpaces = lists:foldl(fun(Element, {Index, Acc}) ->
                                 case Element of
                                    0 -> [Index | Acc];
                                    -1 -> Acc;
                                    1 -> Acc
                                 end
                              end, {1, []}, Board),       
   case length(EmptySpaces) of
      9 ->
         % Pick the bottom left corner (or any corner)
         UpdatedBoard = replaceInList(1, 1, Board),
         UpdatedBoard;

      7 ->
         % Play the center if it is available
         Target = lists:nth(5, Board),
         if
            Target =:= 0 ->
               io:fwrite("~sPlacing an O into position ~w.~n", [?id, Target]),
               UpdatedBoard = replaceInList(1, Target, Board),
               UpdatedBoard;
            true ->
               % List of corners that neighbor the bottom left
               NeighboringCorners = [3, 7],
               
               % Checks if either corner was taken
               RemainingCorners = [Corner || Corner <- NeighboringCorners, lists:nth(Corner, Board) == 0],
            
               % Randomly chooses one of the 2 other corners to play
               Target = lists:nth(rand:uniform(length(RemainingCorners)), RemainingCorners),
               io:fwrite("~sPlacing an O into position ~w.~n", [?id, Target]),
               UpdatedBoard = replaceInList(1, Target, Board),
               UpdatedBoard
         end;

               
      5 ->
         % Check for a winning move
         WinningMove = lists:foldl(fun(Patterns, Acc) -> 
                        case computeMove(Board, Patterns, -1) of
                           -1 -> Acc;
                           Position -> [Position | Acc]
                        end
                     end, [], WinPatterns),
         Corners = [1, 3, 7, 9],
         RemainingCorners = [Corner || Corner <- Corners, lists:nth(Corner, Board) =:= 0],
         BlockingMove = lists:foldl(fun(Patterns, Acc) -> 
                        case computeMove(Board, Patterns, 1) of
                           -1 -> Acc;
                           Position -> [Position | Acc]
                        end
                     end, [], WinPatterns),

         case WinningMove of
            [FirstWinningMove|_] -> 
                  FirstWinningMove;
            [] -> 
                  case RemainingCorners of
                     [] -> 
                        case BlockingMove of
                              [FirstBlockingMove|_] -> 
                                 FirstBlockingMove
                        end;
                     _ -> 
                        CornerMove = lists:nth(rand:uniform(length(RemainingCorners)), RemainingCorners),
                        CornerMove
                  end
         end;

      _ ->
         WinningMove = lists:foldl(fun(Patterns, Acc) -> 
                        case computeMove(Board, Patterns, -1) of
                           -1 -> Acc;
                           Position -> [Position | Acc]
                        end
                     end, [], WinPatterns),
         BlockingMove = lists:foldl(fun(Patterns, Acc) -> 
                        case computeMove(Board, Patterns, 1) of
                           -1 -> Acc;
                           Position -> [Position | Acc]
                        end
                     end, [], WinPatterns),
         case WinningMove of
            -1 -> BlockingMove;
            _ -> WinningMove
         end
   end.

computeMove(Board, {Pos1, Pos2, Pos3}, Player) ->

    Positions = [Pos1, Pos2, Pos3],
    
    % Finds positions the player occupies
    PlayerPositions = lists:filter(fun(Pos) -> lists:nth(Pos, Board) == Player end, Positions),

    % finds empty position
    EmptyPositions = lists:filter(fun(Pos) -> lists:nth(Pos, Board) == 0 end, Positions),

    % if it finds a winning move -> returns that position
    case {length(PlayerPositions), length(EmptyPositions)} of
      {2, 1} -> hd(EmptyPositions);
      _ -> -1
    end.




