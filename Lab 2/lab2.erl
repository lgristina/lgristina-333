-module(lab2).

%
% -- Public --
%

-export([generateLists/2, formatLists/1, test/0]).


% Debugged and slightly altared by chatgpt
% The inner list comprehension generates the N-length sequences spaced by M.
% The outer list comprehension generates the M lists.

generateLists(N, M) -> [ [X + (M*Y) || Y <- lists:seq(0, N-1)] || X <- lists:seq(M, 1, -1) ].


% Formats the list to avoid the pattern matching of ascii values in the lists.

formatLists([]) -> ok; % prints ok instead of an empty list
formatLists([HeadList | TailLists]) -> io:format("~w~n", [HeadList]), formatLists(TailLists).


% Added two test cases.
% First is the example given on the lab sheet, the other uses the values of 5, and 10.

test() -> 
    [14,28,42,56,70,84],
    [13,27,41,55,69,83],
    [12,26,40,54,68,82],
    [11,25,39,53,67,81],
    [10,24,38,52,66,80],
    [9,23,37,51,65,79],
    [8,22,36,50,64,78],
    [7,21,35,49,63,77],
    [6,20,34,48,62,76],
    [5,19,33,47,61,75],
    [4,18,32,46,60,74],
    [3,17,31,45,59,73],
    [2,16,30,44,58,72],
    [1,15,29,43,57,71],
    ok = lab2:formatLists(lab2:generateLists(6, 14)),
    
    [10,20,30,40,50],
    [9,19,29,39,49],
    [8,18,28,38,48],
    [7,17,27,37,47],
    [6,16,26,36,46],
    [5,15,25,35,45],
    [4,14,24,34,44],
    [3,13,23,33,43],
    [2,12,22,32,42],
    [1,11,21,31,41],
    ok = lab2:formatLists(lab2:generateLists(5, 10)),
    "All tests are passed".

% -- Private --
