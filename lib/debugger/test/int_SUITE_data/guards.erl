%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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

%%
-module(guards).

-export([guards/0]).

guards() ->
    ok = t(),
    ok = f(),
    ok = ct(1),
    ok = multi(1),
    ok = multi(2),
    ok = multi(3).

%% The following tests are always true.
t() when is_integer(42) ->
    ok;
t() when is_float(2.0) ->
    ok;
t() when is_number(7) ->
    ok;
t() when is_number(3.14) ->
    ok;
t() when is_atom(error) ->
    ok;
t() when is_list([a]) ->
    ok;
t() when is_tuple({}) ->
    ok;
t() when is_tuple({1, 2}) ->
    ok.

%% The following tests are always false.
f() when is_integer(a) ->
    ok;
f() when is_float(b) ->
    ok;
f() when is_number(c) ->
    ok;
f() when is_atom(42) ->
    ok;
f() when is_list(33) ->
    ok;
f() when is_list({}) ->
    ok;
f() when is_list({1, 2}) ->
    ok;
f() when is_tuple(33) ->
    ok;
f() when is_tuple([a]) ->
    ok;
f() when is_tuple([]) ->
    ok;
f() when is_tuple(35) ->
    ok;
f() ->
    ok.

%% The following tests are always true.
ct(X) ->
    case X of
	Y when is_integer(42) ->
	    ok;
	Y when is_float(2.0) ->
	    ok;
	Y when is_number(7) ->
	    ok;
	Y when is_number(3.14) ->
	    ok;
	Y when is_atom(error) ->
	    ok;
	Y when is_list([a]) ->
	    ok;
	Y when is_tuple({}) ->
	    ok;
	Y when is_tuple({1, 2}) ->
	    ok
    end.

multi(X) ->
    case X of
	Y when is_float(Y) ; is_integer(Y) ->
	    ok;
	Y when Y > 1, Y < 10 ; is_atom(Y) ->
	    ok;
	Y when Y == 4, is_number(Y) ; is_list(Y) ->
	    pannkaka;
	Y when Y==3 ; Y==5 ; Y==6 ->
	    ok
    end.
