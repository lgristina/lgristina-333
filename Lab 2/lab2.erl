-module(lab2).

%
% -- Public --
%

-export([main/2, test/0, test2/0, test3/0, test4/0]).

% Checking for positive integers then creating the lists
main(Length, Spacer) when is_integer(Length), Length >= 0,
                          is_integer(Spacer), Spacer >= 0 -> 
    
    GeneratedLists = generateLists(Length, Spacer), % Creates variable that can be returned
    formatLists(GeneratedLists), % Formats the list to ignore the ascii value pattern matching
    GeneratedLists; % Returns the list of lists from the function

% Character and negative number handling
main(_,_) -> 
    {error, "Invalid input: Please enter a non-negative integer."}.


% The orginal test case from the lab page.
test() -> 
    ExpectedOutput = [
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
    [1,15,29,43,57,71]],

    Result = main(6,14),

    % Better test case format
    case Result of 
        ExpectedOutput ->
            io:format("test passed~n");
        _ -> 
            io:format("test failed~n")
    end.

% Test case with different integer values.
test2() ->
    ExpectedOutput = [
    [10,20,30,40,50],
    [9,19,29,39,49],
    [8,18,28,38,48],
    [7,17,27,37,47],
    [6,16,26,36,46],
    [5,15,25,35,45],
    [4,14,24,34,44],
    [3,13,23,33,43],
    [2,12,22,32,42],
    [1,11,21,31,41]],

    Result = main(5,10),
    
    % Test case output
    case Result of 
        ExpectedOutput ->
            io:format("test passed~n");
        _ -> 
            io:format("test failed~n")
    end.


% Test case with a negative integer.
test3() ->
    ExpectedOutput = {error, "Invalid input: Please enter a non-negative integer."},

    Result = main(-5,10),
    
    % Test case output
    case Result of 
        ExpectedOutput ->
            io:format("test passed~n");
        _ -> 
            io:format("test failed~n")
    end.

% Test case with both a negative integer and a character.
test4() ->
    ExpectedOutput = {error, "Invalid input: Please enter a non-negative integer."},

    Result = main(-5,"e"),
    
    % Test case output
    case Result of 
        ExpectedOutput ->
            io:format("test passed~n");
        _ -> 
            io:format("test failed~n")
    end.

% -- Private --

% Debugged and slightly altared by chatgpt
% The inner list comprehension generates the N-length sequences spaced by M.
% The outer list comprehension generates the M lists.

generateLists(Length, Spacer) -> [ [X + (Spacer*Y) || Y <- lists:seq(0, Length-1)] || X <- lists:seq(Spacer, 1, -1) ].


% Formats the list to avoid the pattern matching of ascii values in the lists.
% Debugged by chatgpt to have it just format the lists instead of returning the list as well.

formatLists([]) -> ok;
formatLists([HeadList | TailLists]) -> 
    io:format("~w~n", [HeadList]), formatLists(TailLists).