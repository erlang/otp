%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2012. All Rights Reserved.
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

-module(smoke_test_SUITE).

-include_lib("test_server/include/test_server.hrl").

%-compile(export_all).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([boot_combo/1, native_atomics/1, jump_table/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(2)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [boot_combo, native_atomics, jump_table].

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


init_per_testcase(boot_combo = Case, Config) when is_list(Config) ->
    case erlang:system_info(build_type) of
	opt ->
	    init_per_tc(Case, Config);
	_ ->
	    {skip,"Cannot test boot_combo in special builds since beam.* may not exist"}
    end;
init_per_testcase(Case, Config) when is_list(Config) ->
    init_per_tc(Case, Config).

init_per_tc(Case, Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    [{testcase, Case},{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

boot_combo(Config) when is_list(Config) ->
    ZFlags = os:getenv("ERL_ZFLAGS"),
    NOOP = fun () -> ok end,
    A42 = fun () ->
		  case erlang:system_info(threads) of
		      true ->
			  42 = erlang:system_info(thread_pool_size);
		      false ->
			  ok
		  end
	  end,
    SMPDisable = fun () -> false = erlang:system_info(smp_support) end,
    try
	chk_boot(Config, "+Ktrue", NOOP),
	chk_boot(Config, "+A42", A42),
	chk_boot(Config, "-smp disable", SMPDisable),
	chk_boot(Config, "+Ktrue +A42", A42),
	chk_boot(Config, "-smp disable +A42",
		 fun () -> SMPDisable(), A42() end),
	chk_boot(Config, "-smp disable +Ktrue", SMPDisable),
	chk_boot(Config, "-smp disable +Ktrue +A42",
		 fun () -> SMPDisable(), A42() end),
	%% A lot more combos could be implemented...
	ok
    after
	os:putenv("ERL_ZFLAGS", case ZFlags of
				    false -> "";
				    _ -> ZFlags
				end)
    end.

native_atomics(Config) when is_list(Config) ->
    NA32Key = "32-bit native atomics",
    NA64Key = "64-bit native atomics",
    DWNAKey = "Double word native atomics",
    EthreadInfo = erlang:system_info(ethread_info),
    ?t:format("~p~n", [EthreadInfo]),
    {value,{NA32Key, NA32, _}} = lists:keysearch(NA32Key, 1, EthreadInfo),
    {value,{NA64Key, NA64, _}} = lists:keysearch(NA64Key, 1, EthreadInfo),
    {value,{DWNAKey, DWNA, _}} = lists:keysearch(DWNAKey, 1, EthreadInfo),
    case {erlang:system_info(build_type), erlang:system_info(smp_support), NA32, NA64, DWNA} of
	{opt, true, "no", "no", _} ->
	    ?t:fail(optimized_smp_runtime_without_native_atomics);
	{_, false, "no", "no", _} ->
	    {comment, "No native atomics"};
	_ ->
	    {comment,
	     NA32 ++ " 32-bit, "
	     ++ NA64 ++ " 64-bit, and "
	     ++ DWNA ++ " double word native atomics"}
    end.

jump_table(Config) when is_list(Config) ->
    case erlang:system_info(beam_jump_table) of
	true ->
	    ok;
	false ->
	    case erlang:system_info(build_type) of
		opt ->
		    ?t:fail(optimized_without_beam_jump_table);
		BT ->
		    {comment, "No beam jump table, but build type is " ++ atom_to_list(BT)}
	    end
    end.
	    

%%%
%%% Aux functions --------------------------------------------------------------
%%%

chk_boot(Config, Args, Fun) ->
    true = os:putenv("ERL_ZFLAGS", Args),
    Success = make_ref(),
    Parent = self(),
    ?t:format("--- Testing ~s~n", [Args]),
    {ok, Node} = start_node(Config),
    Pid = spawn_link(Node, fun () ->
				   Fun(),
				   Parent ! {self(), Success}
			   end),
    receive
	{Pid, Success} ->
	    Node = node(Pid),
	    stop_node(Node),
	    ?t:format("--- Success!~n", []),
	    ok
    end.

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
	      ++ "-"
	      ++ atom_to_list(?config(testcase, Config))
	      ++ "-"
	      ++ integer_to_list(erlang:system_time(seconds))
	      ++ "-"
	      ++ integer_to_list(erlang:unique_integer([positive]))),
    Opts = [{args, "-pa "++Pa++" "++Args}],
    ?t:start_node(Name, slave, Opts).

stop_node(Node) ->
    ?t:stop_node(Node).

