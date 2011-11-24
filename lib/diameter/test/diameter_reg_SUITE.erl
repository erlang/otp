%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Tests of the server implemented by diameter_reg.erl.
%%

-module(diameter_reg_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([add/1,
         add_new/1,
         del/1,
         repl/1,
         terms/1,
         pids/1]).

-define(reg,  diameter_reg).
-define(util, diameter_util).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [{group, all} | tc()].

groups() ->
    [{all, [parallel], tc()}].

tc() ->
    [add,
     add_new,
     del,
     repl,
     terms,
     pids].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

%% ===========================================================================

add(_) ->
    Ref = make_ref(),
    true = ?reg:add(Ref),
    true = ?reg:add(Ref),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self().

add_new(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    false = ?reg:add_new(Ref).

del(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    true = ?reg:add_new({Ref}),
    true = ?reg:del({Ref}),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self().

repl(_) ->
    Ref = make_ref(),
    true = ?reg:add_new({Ref}),
    true = ?reg:repl({Ref}, Ref),
    false = ?reg:add_new(Ref),
    false = ?reg:repl({Ref}, Ref),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self().

terms(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    [[Pid]] = [L || {T,L} <- ?reg:terms(), T == Ref],
    Pid = self().

pids(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    %% Don't match [[Ref]] since this will only necessarily be the
    %% case when the test is run in its own process.
    [_|_] = [L || {P,L} <- ?reg:pids(), P == self()].
