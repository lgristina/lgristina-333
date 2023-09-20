%
% Utils
%

-module(utils).

%
% -- Public --
%

-export([myMap/2]).
-export([subst/3]).
-export([reverse/1]).

myMap(_ , []  )  -> [];
myMap(Fn, List) -> [ Fn(hd(List)) | myMap(Fn, tl(List)) ].


% Reverse a list without using lists:reverse()
reverse([H|T]) -> reverse(T) ++ [H];
reverse([])    -> [].


% subst(this, that, list)
% Replaces all occurences of this with that in the list.
%
% Example: subst(neil, alan, [neil, alex, geddy])
%          produces [alan, alex, geddy]
subst(This, That, List) ->
   io:fwrite("Substituting ~w with ~w in ~w.~n", [This, That, List]),
   doSubst(This, That, List).

% -- Private --
doSubst(_, _, [])                                     -> [];
doSubst(This, That, [Head|Tail]) when (is_list(Head)) -> [doSubst(This, That, Head) | doSubst(This, That, Tail)];
doSubst(This, That, [Head|Tail]) when (not is_list(Head)), (Head == This)  -> [That | doSubst(This, That, Tail)];
doSubst(This, That, [Head|Tail]) when (Head /= This)  -> [Head | doSubst(This, That, Tail)].
