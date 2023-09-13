%
% Factorials.
% By Luca.
%

-module(facs).
-export([fac1/1]).

%
% -- Public -- 
%
fac1(0) -> 1;
fac1(N) -> N * fac1(N-1).


%
% -- Private -- 
%   