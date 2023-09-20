%
% Apple Store Utilities.
% By Alan.
%

-module(storeUtils2).
-export([total1/1, total2/1, test/0, myMap/2]).

%
% -- Public -- 
%
total1([])            -> 0;
total1([Head | Tail]) -> intVal(store:cost(Head) + total1(Tail)).


total2([ {What, HowMany} | Tail ]) -> (intVal(store:cost(What))*HowMany) + total2(Tail);
total2([  Head           | Tail ]) -> (intVal(store:cost(Head))) + total2(Tail);
total2([])                         -> 0.

test() -> 2899 = total1([iMac, iPad]),
          2899 = total2([iMac, iPad]), 
          36099 = total1([pencil, visionPro]),
          577386 = total2([{pencil, 14}, {visionPro, 16}]),
          'The tests have passed.'.

myMap(Function, [])   -> [];
myMap(Function, List) -> [ Function(hd(List)) | myMap(Function, tl(List)) ].

%
% -- Private -- 
%   
intVal(Val) when is_integer(Val) -> Val;
intVal(_)                        -> 0.