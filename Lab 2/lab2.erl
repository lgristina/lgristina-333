-module(lab2).

%
% -- Public --
%

-export([func/2]).

func(N, M) -> [ [X + (M*Y) || Y <- lists:seq(0, N-1)] || X <- lists:seq(M, 1, -1) ].

% -- Private --