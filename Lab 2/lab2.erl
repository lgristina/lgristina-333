-module(lab2).

%
% -- Public --
%

-export([func/2, formatLists/1]).


% Debugged and slightly altared by chatgpt
% The inner list comprehension generates the N-length sequences spaced by M.
% The outer list comprehension generates the M lists.

func(N, M) -> [ [X + (M*Y) || Y <- lists:seq(0, N-1)] || X <- lists:seq(M, 1, -1) ].


% Formats the list to avoid the pattern matching of ascii values in the lists.
formatLists([]) -> ok;
formatLists([HeadList | TailLists]) -> io:format("~w~n", [HeadList]), formatLists(TailLists).

% -- Private --
