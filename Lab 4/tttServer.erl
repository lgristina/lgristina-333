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
         NewBoard = computerTurn(InitialBoard),
         {tttClient, FromNode} ! {node(), player_turn, NewBoard},
         serverLoop();

      {FromNode, process_player_turn, Board, PlayerPos} ->
         io:fwrite("~sReceived [process_player_turn] request from node ~w with board ~w and player move ~w.~n",[?id, FromNode, Board, PlayerPos]),
         NewBoard = processPlayerMove(PlayerPos, Board),
         case checkWin(NewBoard, -1) of
            true ->
               {tttClient, FromNode} ! {node(), player_win, NewBoard};
            false ->
               {tttClient, FromNode} ! {node(), computer_turn, NewBoard},
               serverLoop()
         end;

      {FromNode, computer_turn, Board} ->
         io:fwrite("~sReceived [computer_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
         NewBoard = computerTurn(Board),
         Win = checkWin(NewBoard, 1),
         EmptySpaces = [Position || Position <- lists:seq(1, 9), lists:nth(Position, NewBoard) =:= 0],
         case Win of
            true ->
               io:fwrite("~sSending [computer_win] message to node ~w.~n", [?id, FromNode]),
               {tttClient, FromNode} ! {node(), computer_win, NewBoard};
            false ->
               case EmptySpaces of
                  [] ->
                     io:fwrite("~sSending [computer_tie] message to node ~w.~n", [?id, FromNode]),
                     {tttClient, FromNode} ! {node(), computer_tie, NewBoard};
                  _ ->
                     io:fwrite("~sSending [player_turn] message to node ~w.~n", [?id, FromNode]),
                     {tttClient, FromNode} ! {node(), player_turn, NewBoard},
                     serverLoop()
               end
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
      UpdatedBoard = replaceInList(-1, Position, Board),
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
    Rows = [lists:sublist(Board, 1, 3),
            lists:sublist(Board, 4, 3),
            lists:sublist(Board, 7, 3)],
    Columns = [[lists:nth(1 + X, Board), lists:nth(4 + X, Board), lists:nth(7 + X, Board)] || X <- [0, 1, 2]],
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


pick_random_element(List) ->
    case List of
        [] -> 
            none;
        _ ->       
            Index = rand:uniform(length(List)),
            Elem = lists:nth(Index, List),
            Elem
    end.

computerTurn(Board) ->
   WinPatterns = [{1,2,3}, {4,5,6}, {7,8,9}, {1,4,7}, {2,5,8}, {3,6,9}, {1,5,9}, {3,5,7}],
   EmptySpaces = [Position || Position <- lists:seq(1, 9), lists:nth(Position, Board) == 0],       
   case length(EmptySpaces) of
      9 ->
         % Pick the bottom left corner (or any corner)
         UpdatedBoard = replaceInList(1, 7, Board),
         UpdatedBoard;


         %
         %  NEED TO FIX THIS
         %
      7 ->
         % Play the center if it is available
         Target = lists:nth(5, Board),
         case Target of
            0 ->
               io:fwrite("~sPlacing an O into position ~w.~n", [?id, Target]),
               UpdatedBoard = replaceInList(1, 5, Board),
               UpdatedBoard;
            _ ->
               % List of corners that neighbor the bottom left
               Corners = [1, 3, 7, 9],
                  
               % Checks if either corner was taken
               RemainingCorners = [Corner || Corner <- Corners, lists:nth(Corner, Board) == 0],

               % Randomly chooses one of the other corners to play
               Corner = pick_random_element(RemainingCorners),
               %io:fwrite("~sPlacing an O into position ~w.~n", [?id, Target]),
               UpdatedBoard = replaceInList(1, Corner, Board),
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
                  io:fwrite("~sPlacing an O into position ~w.~n", [?id, FirstWinningMove]),
                  UpdatedBoard = replaceInList(1, FirstWinningMove, Board),
                  UpdatedBoard;
            [] -> 
                  case RemainingCorners of
                     [] -> 
                        case BlockingMove of
                              [FirstBlockingMove|_] -> 
                                 UpdatedBoard = replaceInList(1, FirstBlockingMove, Board),
                                 UpdatedBoard
                        end;
                     _ ->                                   
                        Target = pick_random_element(RemainingCorners),
                        io:fwrite("~sPlacing an O into position ~w.~n", [?id, Target]),
                        UpdatedBoard = replaceInList(1, Target, Board),
                        UpdatedBoard
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
            [] -> 
               case BlockingMove of
                  [] ->
                     RandomMove = pick_random_element(EmptySpaces),
                     io:fwrite("~sPlacing an O into position ~w.~n", [?id, RandomMove]),
                     UpdatedBoard = replaceInList(1, RandomMove, Board),
                     UpdatedBoard;
                  [FirstBlockingMove|_] -> 
                     io:fwrite("~sPlacing an O into position ~w.~n", [?id, FirstBlockingMove]),
                     UpdatedBoard = replaceInList(1, FirstBlockingMove, Board),
                     UpdatedBoard
               end;
            [FirstWinningMove|_] ->
               io:fwrite("~sPlacing an O into position ~w.~n", [?id, FirstWinningMove]),
               UpdatedBoard = replaceInList(1, FirstWinningMove, Board),
               UpdatedBoard
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
