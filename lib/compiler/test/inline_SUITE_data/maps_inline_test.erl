%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

-module(maps_inline_test).

-export([?MODULE/0]).

?MODULE() ->
    21 = mval(#{val => 1}) +
	 mval(#{val => 2}) +
	 mval(#{val => 3}) +
	 mval(#{val => 4}) +
	 mval(#{val => 5}) +
	 mval(#{val => 6}),

    21 = gval(#{id => 1}) +
	 gval(#{id => 2}) +
	 gval(#{id => 3}) +
	 gval(#{id => 4}) +
	 gval(#{id => 5}) +
	 gval(#{id => 6}),

    21 = sval(#{id => 1}) +
	 sval(#{id => 2}) +
	 sval(#{id => 3}) +
	 sval(#{id => 4}) +
	 sval(#{id => 5}) +
	 sval(#{id => 6}),

    M = #{v => 1, m => #{v => 21, m => #{v => 7, m => 13}}},

    42 = decompose(M).

% switch key orders
decompose(#{ m := M, v := V}) when is_map(M) ->
    V + decompose(M);
decompose(#{ v := V, m := M}) -> V + M.


mval(#{val := V}) -> V.

sval(#{id := 1}) -> 6;
sval(#{id := 2}) -> 5;
sval(#{id := 3}) -> 4;
sval(#{id := 4}) -> 3;
sval(#{id := 5}) -> 2;
sval(#{id := 6}) -> 1.

gval(M) when is_map(M) andalso M =:= #{ id => 1} -> 1;
gval(M) when is_map(M) andalso M =:= #{ id => 2} -> 4;
gval(M) when is_map(M) andalso M =:= #{ id => 3} -> 2;
gval(M) when is_map(M) andalso M =:= #{ id => 4} -> 5;
gval(M) when is_map(M) andalso M =:= #{ id => 5} -> 3;
gval(M) when is_map(M) andalso M =:= #{ id => 6} -> 6.
