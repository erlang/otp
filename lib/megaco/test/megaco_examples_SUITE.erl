%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2022. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_examples_SUITE).

-export([
         suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
         init_per_group/2,    end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2, 

         simple/1,
         
         meas/1,
         mstone1/1,
         mstone2/1

        ]).


-include_lib("common_test/include/ct.hrl").
-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, debug).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     simple,
     {group, meas}
    ].

groups() -> 
    [
     {meas, [], meas_cases()}
    ].

meas_cases() ->
    [
     meas,
     mstone1,
     mstone2
    ].



%%
%% -----
%%

init_per_suite(suite) ->
    [];
init_per_suite(doc) ->
    [];
init_per_suite(Config0) when is_list(Config0) ->

    ?ANNOUNCE_SUITE_INIT(),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            %% We need a (local) monitor on this node also
            megaco_test_sys_monitor:start(),

            p("init_per_suite -> end when"
              "~n      Config: ~p"
              "~n      Nodes:  ~p", [Config1, erlang:nodes()]),

            Config1
    end.

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    megaco_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%
%% -----
%%

init_per_testcase(simple = Case, Config) ->

    p("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    megaco_test_global_sys_monitor:reset_events(),

    put(dbg,true),
    purge_example_simple(),
    load_example_simple(),
    megaco:enable_trace(max, io),

    megaco_test_lib:init_per_testcase(Case, Config);

init_per_testcase(meas = Case, Config) ->

    p("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    case megaco_flex_scanner:is_enabled() of
        true ->
            megaco_test_global_sys_monitor:reset_events(),
            init_per_testcase_meas(Case, example_meas_meas_modules(), Config);
        false ->
            ?SKIP(flex_scanner_not_enabled)
    end;

init_per_testcase(mstone1 = Case, Config) ->

    p("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    case megaco_flex_scanner:is_enabled() of
        true ->
            megaco_test_global_sys_monitor:reset_events(),
            init_per_testcase_meas(Case, example_meas_mstone1_modules(), Config);
        false ->
            ?SKIP(flex_scanner_not_enabled)
    end;

init_per_testcase(mstone2 = Case, Config) ->

    p("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    case megaco_flex_scanner:is_enabled() of
        true ->
            megaco_test_global_sys_monitor:reset_events(),
            init_per_testcase_meas(Case, example_meas_mstone2_modules(), Config);
        false ->
            ?SKIP(flex_scanner_not_enabled)
    end.


init_per_testcase_meas(Case, Mods, Config) ->
    purge_example(Mods),
    load_example_meas(),

    p("try start worker node"),
    try start_unique_node(Case) of
        WorkerNode ->
            megaco_test_lib:init_per_testcase(Case, [{worker_node, WorkerNode} | Config])
    catch
        _:{failed_starting_node, NodePre, Reason} ->
            {skip, ?F("Failed starting node ~p: ~p", [NodePre, Reason])}
    end.
    


end_per_testcase(simple = Case, Config) ->

    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    purge_example_simple(),
    erase(dbg),
    megaco:disable_trace(),

    megaco_test_lib:end_per_testcase(Case, Config);

end_per_testcase(meas = Case, Config) ->

    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    end_per_testcase_meas(Case, example_meas_meas_modules(), Config);

end_per_testcase(mstone1 = Case, Config) ->

    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    end_per_testcase_meas(Case, example_meas_mstone1_modules(), Config);

end_per_testcase(mstone2 = Case, Config) ->

    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    end_per_testcase_meas(Case, example_meas_mstone2_modules(), Config).

end_per_testcase_meas(Case, Mods, Config) ->
    purge_example(Mods),
    case lists:keysearch(worker_node, 1, Config) of
        {value, {worker_node, WorkerNode}} ->
            ?STOP_NODE(WorkerNode),
            megaco_test_lib:end_per_testcase(Case, lists:keydelete(worker_node, 1, Config));
        false ->
            megaco_test_lib:end_per_testcase(Case, Config)
    end.


load_example_simple() ->
    load_example(simple).

load_example_meas() ->
    load_example(meas).

load_example(Example) ->
    case code:lib_dir(megaco) of
	{error, Reason} ->
	    {error, Reason};
	Dir ->
	    ExampleDir = filename:join([Dir, examples, Example]),
	    case code:add_path(ExampleDir) of
		true ->
		    ok;
		{error, What} ->
		    error_logger:error_msg("failed adding examples ~p path: "
					   "~n   ~p"
					   "~n", [Example, What]),
		    {error, {failed_add_path, Example, What}}
	    end
    end.


example_simple_modules() ->
    [
     megaco_simple_mg,
     megaco_simple_mgc
    ].

purge_example_simple() ->
    purge_example(example_simple_modules()).


example_meas_meas_modules() ->
    [
     megaco_codec_meas,
     megaco_codec_transform
    ].

example_meas_mstone1_modules() ->
    [
     megaco_codec_mstone1,
     megaco_codec_mstone_lib,
     megaco_codec_transform
    ].

example_meas_mstone2_modules() ->
    [
     megaco_codec_mstone2,
     megaco_codec_mstone_lib,
     megaco_codec_transform
    ].


purge_example(Mods) ->
    case code:lib_dir(megaco) of
	{error, Reason} ->
	    {error, Reason};
	_Dir ->
	    [code:purge(M) || M <- Mods]
    end.



%% ------------------ simple ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple(suite) ->
    [];
simple(Config) when is_list(Config) ->
    Pre  = fun() ->
                   d("simple -> "
                     "create (3) node name(s) (includes the own node)"),
                   %% We actually need two *new* nodes,
                   %% but the function includes the own node,
                   %% so we need to ask for one more.
                   [_Local, MGC, MG] = ?MK_NODES(3),
                   Nodes = [MGC, MG],

                   d("simple -> start nodes: "
                     "~n      ~p", [Nodes]),
                   ok = ?START_NODES(Nodes, true),
                   Nodes
           end,
    Case = fun(Nodes) ->
                   do_simple(Config, Nodes)
           end,
    Post = fun(Nodes) ->
                   d("simple -> stop nodes"
                     "~n      ~p", [Nodes]),
                   ?STOP_NODES(Nodes)
           end,
    try_tc(?FUNCTION_NAME, Pre, Case, Post).


do_simple(_Config, [MGC, MG]) ->
    MGCId = "MGC",
    MGId  = "MG",

    d("simple -> MGC proxy start (on ~p)", [MGC]),
    MGCProxy = megaco_test_lib:proxy_start(MGC, "MGC"),
    ?SLEEP(1000),

    d("simple -> MG proxy start (on ~p)", [MG]),
    MGProxy  = megaco_test_lib:proxy_start(MG, "MG"),
    ?SLEEP(1000),

    MegacoStart       = fun() -> megaco:start() end,
    MegacoStartVerify =
	 fun(_, ok)    -> ok;
	    (Id, Else) -> ?ERROR({failed_starting_megaco, Id, Else})
	 end,

    d("simple -> start MGC megaco"),
    exec(MGCProxy, MGCId, MegacoStart,
	 fun(Res) -> MegacoStartVerify(MGCId, Res) end),
    %% ?APPLY(MGCProxy, fun() -> ok = megaco:start() end),
    ?SLEEP(1000),
    
    d("simple -> start MG megaco"),
    exec(MGProxy, MGId, MegacoStart,
	 fun(Res) -> MegacoStartVerify(MGId, Res) end),
    %% ?APPLY(MGProxy, fun() -> ok = megaco:start() end),
    ?SLEEP(1000),
    
    d("simple -> start mgc"),
    start_mgc(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC info (no mg)"),
    info(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC system_info(users) (no mg)"),
    users(MGCProxy),
    ?SLEEP(1000),

    d("simple -> start mg"),
    start_mg(MGProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC info (mg)"),
    info(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC system_info(users) (mg)"),
    users(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MG info"),
    info(MGProxy),
    ?SLEEP(1000),

    d("simple -> verify MG system_info(users)"),
    users(MGProxy),
    ?SLEEP(1000),

    d("simple -> stop mgc"),
    exec(MGCProxy, MGCId,
	 fun() -> megaco_simple_mgc:stop() end,
	 fun([_]) -> ok;
	    (L) when is_list(L) ->
		 ?ERROR({invalid_users, L});
	    (X) ->
		 ?ERROR({invalid_result, X})
	 end),
    %% ?VERIFY(5, length()),
    ?SLEEP(1000),

    d("simple -> verify MGC info (no mgc)"),
    info(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MG info (no mgc)"),
    info(MGProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC system_info(users) (no mgc)",[]),
    users(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MG system_info(users) (no mgc)",[]),
    users(MGProxy),
    ?SLEEP(1000),

    MegacoStop       = fun() -> megaco:stop() end,
    MegacoStopVerify =
	 fun(_, ok)    -> ok;
	    (Id, Else) -> ?ERROR({failed_stop_megaco, Id, Else})
	 end,

    d("simple -> stop MG megaco",[]),
    exec(MGProxy, MGId, MegacoStop,
	 fun(Res) -> MegacoStopVerify(MGId, Res) end),
    %% ?VERIFY(ok, megaco:stop()),
    ?SLEEP(1000),

    d("simple -> stop MGC megaco",[]),
    exec(MGCProxy, MGCId, MegacoStop,
	 fun(Res) -> MegacoStopVerify(MGCId, Res) end),
    %% ?VERIFY(ok, megaco:stop()),
    ?SLEEP(1000),

    d("simple -> kill (exit) MG Proxy: ~p", [MGProxy]),
    MGProxy ! {stop, self(), normal},
    receive
	{'EXIT', MGProxy, _} ->
	    d("simple -> MG Proxy terminated"),
	    ok
    end,

    d("simple -> kill (exit) MGC Proxy: ~p", [MGCProxy]),
    MGCProxy ! {stop, self(), normal},
    receive
	{'EXIT', MGCProxy, _} ->
	    d("simple -> MGC Proxy terminated"),
	    ok
    end,

    d("simple -> done", []),
    ok.


exec(Proxy, Id, Cmd, Verify) ->
    ?APPLY(Proxy, Cmd),
    receive
	{res, Id, Res} ->
	    Verify(Res)
    end.


start_mgc(Proxy) ->
    ?APPLY(Proxy,
	   fun() ->
		   try megaco_simple_mgc:start() of
		       Res ->
			   Res
		   catch
		       C:E:S ->
			   {error, {{catched, C, E, S}}, code:get_path()}
		   end
	   end),
    receive
	{res, _, {ok, MgcAll}} when is_list(MgcAll) ->
	    MgcBad = [MgcRes || MgcRes <- MgcAll, element(1, MgcRes) /= ok],
	    ?VERIFY([], MgcBad),
	    ok;
	Error ->
	    ?ERROR(Error)
    end.


start_mg(Proxy) ->
    ?APPLY(Proxy, fun() -> 
			  try megaco_simple_mg:start() of
			      Res ->
				  Res
			  catch
			      C:E:S ->
				  {error, {{catched, C, E, S}}, code:get_path()}
			  end
		  end),
    receive
	{res, _, MGs} when is_list(MGs) andalso (length(MGs) =:= 4) ->
	    verify_mgs(MGs);
	Error ->
	    ?ERROR(Error)
    end.

verify_mgs(MGs) ->
    Verify = 
	fun({_MgMid, {TransId, Res}}) when (TransId =:= 1) ->
		case Res of
		    {ok, [AR]} when is_record(AR, 'ActionReply') ->
			case AR#'ActionReply'.commandReply of
			    [{serviceChangeReply, SCR}] ->
				case SCR#'ServiceChangeReply'.serviceChangeResult of
				    {serviceChangeResParms, MgcMid} 
				      when (MgcMid =/= asn1_NOVALUE) ->
					ok;
				    Error ->
					?ERROR(Error)
				end;
			    Error ->
				?ERROR(Error)
			end;
		    Error ->
			?ERROR(Error)
		end;
	   (Error) ->
		?ERROR(Error)
	end,
    lists:map(Verify, MGs).
    
    
info(Proxy) ->
    ?APPLY(Proxy,
	   fun() ->
		   try megaco:info() of
		       I -> I
		   catch
		       C:E:S ->
			   {error, {C, E, S}}
		   end
	   end),
    receive
	{res, _, Info} when is_list(Info) ->
	    ?LOG("Ok, ~p~n", [Info]);
	{res, _, Error} ->
	    ?ERROR(Error)
    end.


users(Proxy) ->
    ?APPLY(Proxy,
	   fun() ->
		   try megaco:system_info(users) of
		       I -> I
		   catch
		       C:E:S ->
			   {error, {C, E, S}}
		   end
	   end),
    receive
	{res, _, Info} when is_list(Info) ->
	    ?LOG("Ok, ~p~n", [Info]);
	{res, _, Error} ->
	    ?ERROR(Error)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------ meas:meas ------------------------

meas(suite) ->
    [];
meas(Config) when is_list(Config) ->
    Pre  = fun() ->
                   MFactor = ?config(megaco_factor, Config),
                   {Time, Factor} =
                       if
                           (MFactor =:= 1) ->
                               {3,  100};
                           (MFactor =:= 2) ->
                               {4,  100};
                           (MFactor =:= 3) ->
                               {4,  200};
                           (MFactor =:= 4) ->
                               {5,  300};
                           (MFactor =:= 5) ->
                               {5,  400};
                           (MFactor =:= 6) ->
                               {6,  500};
                           true ->
                               {10, 600}
                       end,
                   p("Run with: "
                     "~n      Timetrap: ~p mins"
                     "~n      Factor:   ~p", [Time, Factor]),
                   ct:timetrap(?MINS(Time)),
                   WorkerNode = ?config(worker_node, Config),
                   {Factor, WorkerNode}
           end,
    Opts = #{verbose => false},
    Case = fun({Factor, WorkerNode}) ->
                   do_meas(WorkerNode, megaco_codec_meas, start, [Factor, Opts])
           end,
    Post = fun(_) -> ok end,
    try_tc(?FUNCTION_NAME, Pre, Case, Post).

do_meas(Node, Mod, Func, Args) ->
    F = fun() ->
                exit( rpc:call(Node, Mod, Func, Args) )
        end,
    p("start worker process"),
    {Pid, MRef} = spawn_monitor(F),
    p("await completion"),
    receive
        {'DOWN', MRef, process, Pid,
         {error, {failed_loading_flex_scanner_driver, Reason}}} ->
            p("<ERROR> worker process failed loading flex scanner: "
              "~n      ~p", [Reason]),
            ?SKIP(Reason);
        {'DOWN', MRef, process, Pid, {error, Reason}} ->
            p("<ERROR> worker process terminated: "
              "~n      ~p", [Reason]),
            ?FAIL(Reason);
        {'DOWN', MRef, process, Pid, {badrpc, BadRpc}} ->
            p("<ERROR> worker process terminated - badrpc: "
              "~n      ~p", [BadRpc]),
            ?FAIL(BadRpc);
        {'DOWN', MRef, process, Pid, Reason} ->
            p("worker process terminated: "
              "~n      ~p", [Reason]),
            ok;

        {'EXIT', TCPid, {timetrap_timeout = R, TCTimeout, TCSTack}} ->
            p("received timetrap timeout (~w ms) from ~p => "
              "Kill executor process"
              "~n      TC Stack: ~p", [TCTimeout, TCPid, TCSTack]),
            exit(Pid, kill),
            ?SKIP(R)
    end,
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------ meas:mstone1 ------------------------

mstone1(suite) ->
    [];
mstone1(Config) when is_list(Config) ->
    Pre  = fun() ->
                   %% The point of this is to make sure we
                   %% utilize as much of the host as possible...
                   RunTime   = 1, % Minute
                   NumSched  =
                       try erlang:system_info(schedulers_online) of N -> N
                       catch _:_:_ -> 1
                       end,
                   Factor    = 1 + (NumSched div 12),
                   ct:timetrap(?MINS(RunTime + 1)),
                   {RunTime, Factor, ?config(worker_node, Config)}
           end,
    Case = fun({RunTime, Factor, WorkerNode}) ->
                   Mod  = megaco_codec_mstone1,
                   Func = start,
                   Args = [RunTime, Factor],
                   p("Run with: "
                     "~n      Run Time: ~p min(s)"
                     "~n      Factor:   ~p", [RunTime, Factor]),
                   do_meas(WorkerNode, Mod, Func, Args)
           end,
    Post = fun(_) -> ok end,
    try_tc(?FUNCTION_NAME, Pre, Case, Post).
                   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------ meas:mstone2 ------------------------

mstone2(suite) ->
    [];
mstone2(Config) when is_list(Config) ->
    Pre  = fun() ->
                   RunTime  = 1, % Minutes
                   NumSched =
                       try erlang:system_info(schedulers_online) of N -> N
                       catch _:_:_ -> 1
                       end,
                   Factor   = 1 + (NumSched div 12),
                   ct:timetrap(?MINS(RunTime + 1)),
                   {Factor, RunTime, ?config(worker_node, Config)}
           end,
    Case = fun({Factor, RunTime, WorkerNode}) ->
                   Mode = standard,
                   Mod  = megaco_codec_mstone2,
                   Func = start,
                   Args = [Factor, RunTime, Mode],
                   p("Run with: "
                     "~n      Factor:   ~p"
                     "~n      Run Time: ~p min(s)"
                     "~n      Mode:     ~p", [Factor, RunTime, Mode]),
                   do_meas(WorkerNode, Mod, Func, Args)
           end,
    Post = fun(_) -> ok end,
    try_tc(?FUNCTION_NAME, Pre, Case, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_unique_node(Pre) ->
    start_unique_node(Pre, 3).

start_unique_node(Pre, 0) -> 
    ?FAIL({failed_starting_node, Pre, tries});
start_unique_node(Pre, N) when is_integer(N) andalso (N > 0) ->
    Node = unique_node_name(Pre),
    try ?START_NODE(Node, true) of
        ok ->
            Node
    catch
        exit:{skip, {fatal, {node_already_running, _}, _, _}} ->
            ?SLEEP(100),
            start_unique_node(Pre, N-1);
        exit:{skip, {fatal, {cannot_start_node, _, Reason}, _, _}} ->
            ?FAIL({failed_starting_node, Pre, Reason})
    end.


unique_node_name(Pre) ->
    [_, Host] = string:tokens(atom_to_list(node()), [$@]),
    list_to_atom(?F("~s@~s", [?UNIQUE(Pre), Host])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Pre, Case, Post) ->
    try_tc(TCName, "EX-TESTER", ?TEST_VERBOSITY, Pre, Case, Post).

try_tc(TCName, Name, Verbosity, Pre, Case, Post) ->
    ?TRY_TC(TCName, Name, Verbosity, Pre, Case, Post).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F) ->
    p(F, []).

p(F, A) ->
    p(get(sname), F, A).

p(S, F, A) when is_list(S) ->
    io:format("*** [~s] ~p ~s ***" 
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self(), S | A]);
p(_S, F, A) ->
    io:format("*** [~s] ~p *** "
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).


d(F) ->
    d(F, []).
d(F, A) ->
    d(get(dbg), F, A).

d(true, F, A) ->
    io:format("DBG: ~s " ++ F ++ "~n", [?FTS() | A]);
d(_, _F, _A) ->
    ok.
