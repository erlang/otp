%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
t() when integer(42) ->
    ok;
t() when float(2.0) ->
    ok;
t() when number(7) ->
    ok;
t() when number(3.14) ->
    ok;
t() when atom(error) ->
    ok;
t() when list([a]) ->
    ok;
t() when tuple({}) ->
    ok;
t() when tuple({1, 2}) ->
    ok.

%% The following tests are always false.
f() when integer(a) ->
    ok;
f() when float(b) ->
    ok;
f() when number(c) ->
    ok;
f() when atom(42) ->
    ok;
f() when list(33) ->
    ok;
f() when list({}) ->
    ok;
f() when list({1, 2}) ->
    ok;
f() when tuple(33) ->
    ok;
f() when tuple([a]) ->
    ok;
f() when tuple([]) ->
    ok;
f() when tuple(35) ->
    ok;
f() ->
    ok.

%% The following tests are always true.
ct(X) ->
    case X of
	Y when integer(42) ->
	    ok;
	Y when float(2.0) ->
	    ok;
	Y when number(7) ->
	    ok;
	Y when number(3.14) ->
	    ok;
	Y when atom(error) ->
	    ok;
	Y when list([a]) ->
	    ok;
	Y when tuple({}) ->
	    ok;
	Y when tuple({1, 2}) ->
	    ok
    end.

multi(X) ->
    case X of
	Y when float(Y) ; integer(Y) ->
	    ok;
	Y when Y > 1, Y < 10 ; atom(Y) ->
	    ok;
	Y when Y == 4, number(Y) ; list(Y) ->
	    pannkaka;
	Y when Y==3 ; Y==5 ; Y==6 ->
	    ok
    end.
