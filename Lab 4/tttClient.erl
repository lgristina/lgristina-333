%
% tttClient.erl
%
-module(tttClient).
-author('Alan G. Labouseur').
-author('Luca Gristina').

-define(else, true).
-define(id, "-- client: ").


%
% Public
%
-export([start/0, play/0, play/1]).

start() ->
   io:fwrite("~sTTT client started on node ~w (pid ~w) ", [?id, node(), self()]),
   ClientPid = spawn(fun clientLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttClient, ClientPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ClientPid, tttClient]).


play() ->
   io:fwrite("You must supply a node.~n", []).

play(ServerNode) ->
   io:fwrite("~sSending [start_game] request to node ~w.~n",[?id, ServerNode]),
   {tttServer, ServerNode} ! {node(), start_game}.


%
% Private, but accepting messages sent to clientLoop because of the way it was spawned.
%
clientLoop() -> receive
                   {FromNode, player_turn, Board} ->
                      io:fwrite("~sReceived [player_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      displayBoard(Board),
                      io:fwrite("~s", [?id]),
                      {ok, PlayerMove} = io:fread("Where do you want to move [1-9]? ", "~d"),   % PlayerMove gets read as a list.
                      io:fwrite("~sSending [process_player_turn] response to node ~w with board ~w and player move ~w.~n",[?id, FromNode, Board, hd(PlayerMove)]),
                      {tttServer, FromNode} ! {node(), process_player_turn, Board, hd(PlayerMove)},
                      clientLoop();

                   {FromNode, computer_turn, Board} ->
                      io:fwrite("~sReceived [computer_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      io:fwrite("~s", [?id]),
                      {tttServer, FromNode} ! {node(), computer_turn, Board},
                      displayBoard(Board),
                      clientLoop();

                   {FromNode, player_win, Board} ->
                      io:fwrite("~sReceived [player_win] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      displayBoard(Board),
                      io:fwrite("~sYou won!~n", [?id]),
                      clientLoop();

                   {FromNode, computer_win, Board} ->
                      io:fwrite("~sReceived [computer_win] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      displayBoard(Board),
                      io:fwrite("~sI won!~n", [?id]),
                      clientLoop();

                   {FromNode, computer_tie, Board} ->
                      io:fwrite("~sReceived [computer_tie] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      displayBoard(Board),
                      io:fwrite("~sIt's a tie!~n", [?id]),
                      clientLoop();

                   {FromNode, _Any}  ->
                      io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
                      clientLoop()
                end.


%
% Private; no messages either.
%
displayBoard(Board) ->
    Symbols = lists:map(fun convertToSymbol/1, Board),
    Rows = [lists:sublist(Symbols, 1, 3),
            lists:sublist(Symbols, 4, 3),
            lists:sublist(Symbols, 7, 3)],
    lists:foreach(fun(Row) ->
                      io:fwrite("  ~s | ~s | ~s~n", Row),
                      io:fwrite(" -----------~n", [])
                  end, Rows).

convertToSymbol(0) -> " ";
convertToSymbol(1) -> "X";
convertToSymbol(-1) -> "O".