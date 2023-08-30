%
% My second program.
% By Luca.
%

-module(zork).
-export([start/0]).

%
% -- Public -- 
%
start() ->
   io:format("Zork!~n"),
   welcome().

%
% -- Private -- 
%
welcome() ->
   io:format("West of House~n").