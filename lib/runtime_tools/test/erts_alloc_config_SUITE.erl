%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(erts_alloc_config_SUITE).

%-define(line_trace, 1).

-include_lib("test_server/include/test_server.hrl").

%-compile(export_all).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

%% Testcases
-export([basic/1]).

%% internal export
-export([make_basic_config/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(2)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_list(Config) ->
    [{testcase, Case},
     {watchdog, ?t:timetrap(?DEFAULT_TIMEOUT)},
     {erl_flags_env, save_env()} | Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    ?t:timetrap_cancel(?config(watchdog, Config)),
    restore_env(?config(erl_flags_env, Config)),
    ok.

%%%
%%% The test cases ------------------------------------------------------------
%%%

basic(doc) -> [];
basic(suite) -> [];
basic(Config) when is_list(Config) ->
    ?line ErtsAllocConfig = privfile("generated", Config),

    SbctMod = " +MBsbct 1024 +MHsbct 4096",

    %% Make sure we have enabled allocators
    ZFlgs = os:getenv("ERL_ZFLAGS", "") ++ " +Mea max +Mea config",

    ?line os:putenv("ERL_ZFLAGS", ZFlgs ++ SbctMod),

    ?line {ok, Node1} = start_node(Config),
    ?line ok = rpc:call(Node1, ?MODULE, make_basic_config, [ErtsAllocConfig]),
    ?line stop_node(Node1),

    ?line display_file(ErtsAllocConfig),

    ?line ManualConfig = privfile("manual", Config),
    ?line {ok, IOD} = file:open(ManualConfig, [write]),
    ?line io:format(IOD, "~s", ["+MBsbct 2048"]),
    ?line file:close(IOD),
    ?line display_file(ManualConfig),

    ?line os:putenv("ERL_ZFLAGS", ZFlgs),

    ?line {ok, Node2} = start_node(Config,
				   "-args_file " ++ ErtsAllocConfig
				   ++ " -args_file " ++ ManualConfig),

    ?line {_, _, _, Cfg} = rpc:call(Node2, erlang, system_info, [allocator]),

    ?line stop_node(Node2),

    ?line {value,{binary_alloc, BCfg}} = lists:keysearch(binary_alloc, 1, Cfg),
    ?line {value,{sbct, 2097152}} = lists:keysearch(sbct, 1, BCfg),
    ?line {value,{eheap_alloc, HCfg}} = lists:keysearch(eheap_alloc, 1, Cfg),
    ?line {value,{sbct, 4194304}} = lists:keysearch(sbct, 1, HCfg),

    ?line ok.

make_basic_config(ErtsAllocConfig) ->
    %% Save some different scenarios
    Tester = self(),
    SSBegun = make_ref(),
    SSDone = make_ref(),
    SSFun = fun (F) ->
		    receive
			SSDone ->
			    ok = erts_alloc_config:save_scenario(),
			    Tester ! SSDone
		    after 500 ->
			    ok = erts_alloc_config:save_scenario(),
			    F(F)
		    end
	    end,
    SS = spawn_link(fun () ->
			    ok = erts_alloc_config:save_scenario(),
			    Tester ! SSBegun,
			    SSFun(SSFun)
		    end),
    receive SSBegun -> ok end,
    Ref = make_ref(),
    Tab = ets:new(?MODULE, [bag, public]),
    Ps = lists:map(
	   fun (_) ->
		   spawn_link(
		     fun () ->
			     ets:insert(Tab,
					{self(),
					 lists:seq(1, 1000)}),
			     receive after 1000 -> ok end,
			     Tester ! {Ref, self()}
		     end)
	   end,
	   lists:seq(1, 10000)),
    lists:foreach(fun (P) -> receive {Ref, P} -> ok end end, Ps),
    ets:delete(Tab),
    SS ! SSDone,
    receive SSDone -> ok end,

    ok = erts_alloc_config:make_config(ErtsAllocConfig).



%%
%% Utils ----------------------------------------------------------------------
%%

display_file(FileName) ->
    ?t:format("filename: ~s~n", [FileName]),
    {ok, Bin} = file:read_file(FileName),
    io:format("~s", [binary_to_list(Bin)]),
    ?t:format("eof: ~s~n", [FileName]),
    ok.

mk_name(Config) when is_list(Config) ->
    {A, B, C} = now(),
    list_to_atom(atom_to_list(?MODULE)
		 ++ "-" ++ atom_to_list(?config(testcase, Config))
		 ++ "-" ++ integer_to_list(A)
		 ++ "-" ++ integer_to_list(B)
		 ++ "-" ++ integer_to_list(C)).

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line ?t:start_node(mk_name(Config),
			slave,
			[{args, "-pa " ++ Pa ++ " " ++ Args}]).

stop_node(Node) ->
    ?line true = ?t:stop_node(Node).

privfile(Name, Config) ->
    filename:join([?config(priv_dir, Config),
		   atom_to_list(?config(testcase, Config)) ++ "." ++ Name]).

save_env() ->
    {erl_flags,
     os:getenv("ERL_AFLAGS"),
     os:getenv("ERL_FLAGS"),
     os:getenv("ERL_"++erlang:system_info(otp_release)++"_FLAGS"),
     os:getenv("ERL_ZFLAGS")}.

restore_env(EVar, false) when is_list(EVar) ->
    restore_env(EVar, "");
restore_env(EVar, "") when is_list(EVar) ->
    case os:getenv(EVar) of
	false -> ok;
	"" -> ok;
	" " -> ok;
	_ -> os:putenv(EVar, " ")
    end;
restore_env(EVar, Value) when is_list(EVar), is_list(Value) ->
    case os:getenv(EVar) of
	Value -> ok;
	_ -> os:putenv(EVar, Value)
    end.

restore_env({erl_flags, AFlgs, Flgs, RFlgs, ZFlgs}) ->
    restore_env("ERL_AFLAGS", AFlgs),
    restore_env("ERL_FLAGS", Flgs),
    restore_env("ERL_"++erlang:system_info(otp_release)++"_FLAGS", RFlgs),
    restore_env("ERL_ZFLAGS", ZFlgs),
    ok.
