%
% Apple Store Utilities.
% By Alan.
%

-module(storeUtils).
-export([total/1]).

%
% -- Public -- 
%
total([])            -> 0;
total([Head | Tail]) -> intVal(store:cost(Head)) + total(Tail).

%
% -- Private -- 
%

intVal(Val) when is_integer(Val) -> Val;
intVal(_)                        -> 0.