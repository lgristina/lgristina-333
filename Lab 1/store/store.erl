%
% Apple Store.
% By Alan.
%

-module(store).
-export([cost/1]).

%
% -- Public -- 
%
cost(iPhone)       ->  999;
cost(iPhoneProMax) -> 1499;
cost(iPad)         ->  499;
cost(macBook)      -> 1900;
cost(iMac)         -> 2400;
cost(visionPro)    -> 36000;
cost(pencil)       -> 99;
cost(appleCare)   -> {"false"};
cost(insight)      -> priceless;
cost(_)            -> notSold.

%
% -- Private -- 
%   