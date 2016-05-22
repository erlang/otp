%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(fun_r13_SUITE).
-compile(r13).

-export([all/0, suite/0,
         dist_old_release/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [dist_old_release].

dist_old_release(Config) when is_list(Config) ->
    case test_server:is_release_available("r12b") of
        true -> do_dist_old(Config);
        false -> {skip,"No R12B found"}
    end.

do_dist_old(Config) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = fun_dist_r12,
    {ok,Node} = test_server:start_node(Name, peer,
                                       [{args,"-pa "++Pa},
                                        {erl,[{release,"r12b"}]}]),

    Pid = spawn_link(Node,
                     fun() ->
                             receive
                                 Fun when is_function(Fun) ->
                                     R12BFun = fun(H) -> cons(H, [b,c]) end,
                                     Fun(Fun, R12BFun)
                             end
                     end),
    Self = self(),
    Fun = fun(F, R12BFun) ->
                  {pid,Self} = erlang:fun_info(F, pid),
                  {module,?MODULE} = erlang:fun_info(F, module),
                  Self ! {ok,F,R12BFun}
          end,
    Pid ! Fun,
    receive
        {ok,Fun,R12BFun} ->
            [a,b,c] = R12BFun(a);
        Other ->
            ct:fail({bad_message,Other})
    end,
    true = test_server:stop_node(Node),
    ok.

cons(H, T) ->
    [H|T].
