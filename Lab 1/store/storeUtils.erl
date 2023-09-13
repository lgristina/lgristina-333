%
% Apple Store Utilities.
% By Alan.
%

-module(storeUtils).
-export([total/1]).

%
% -- Public -- 
%
total([Head | Tail]) -> store:cost(Head) + total(Tail);
total([])            -> 0.

%
%
%