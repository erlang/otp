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
%%----------------------------------------------------------------------
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------

-module(diameter_test_server).

-export([
	 t/1, t/2,

	 init_per_testcase/2, 
	 fin_per_testcase/2
	]).

-include("diameter_test_lib.hrl").


-define(GLOGGER, diameter_global_logger).


%% ----------------------------------------------------------------
%%

t([Case]) when is_atom(Case) ->
    t(Case);
t(Case) ->
    process_flag(trap_exit, true),
    MEM = fun() -> case (catch erlang:memory()) of
			{'EXIT', _} ->
			    [];
			Res ->
			    Res
		    end
	  end,
    Alloc1 = diameter_test_lib:alloc_info(),
    Mem1   = MEM(),
    Res    = lists:flatten(t(Case, default_config())),
    Alloc2 = diameter_test_lib:alloc_info(),
    Mem2   = MEM(),
    %% io:format("Res: ~p~n", [Res]),
    display_result(Res, Alloc1, Mem1, Alloc2, Mem2),
    Res.


groups(Mod) when is_atom(Mod) ->
    try Mod:groups() of
        Groups when is_list(Groups) ->
            Groups;
        BadGroups ->
            exit({bad_groups, Mod, BadGroups})
    catch
        _:_ ->
            []
    end.

init_suite(Mod, Config) ->
    io:format("~w:init_suite -> entry with"
	      "~n   Mod:    ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Mod, Config]),
    Mod:init_per_suite(Config).

end_suite(Mod, Config) ->
    Mod:end_per_suite(Config).

init_group(Mod, Group, Config) ->
    Mod:init_per_group(Group, Config).

end_group(Mod, Group, Config) ->
    Mod:init_per_group(Group, Config).

%% This is for sub-SUITEs
t({_Mod, {NewMod, all}, _Groups}, _Config) when is_atom(NewMod) ->
    io:format("~w:t(all) -> entry with"
	      "~n   NewMod: ~p"
	      "~n", [?MODULE, NewMod]),
    t(NewMod);
t({Mod, {group, Name} = Group, Groups}, Config)
  when is_atom(Mod) andalso is_atom(Name) andalso is_list(Groups) ->
    io:format("~w:t(group) -> entry with"
	      "~n   Mod:    ~p"
	      "~n   Name:   ~p"
	      "~n   Groups: ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Mod, Name, Groups, Config]),
    case lists:keysearch(Name, 1, Groups) of
        {value, {Name, _Props, GroupsAndCases}} ->
            try init_group(Mod, Name, Config) of
                Config2 when is_list(Config2) ->
                    Res = [t({Mod, Case, Groups}, Config2) ||
                              Case <- GroupsAndCases],
                    (catch end_group(Mod, Name, Config2)),
                    Res;
                Error ->
                    io:format(" => group (~w) init failed: ~p~n",
                              [Name, Error]),
                    [{failed, {Mod, Group}, Error}]
            catch
                exit:{skipped, SkipReason} ->
                    io:format(" => skipping group: ~p~n", [SkipReason]),
                    [{skipped, {Mod, Group}, SkipReason, 0}];
                exit:{undef, _} ->
                    [t({Mod, Case, Groups}, Config) ||
			Case <- GroupsAndCases];
                T:E ->
                    [{failed, {Mod, Group}, {T,E}, 0}]
            end;
        false ->
            exit({unknown_group, Mod, Name, Groups})
    end;
t({Mod, Fun, _}, Config)
  when is_atom(Mod) andalso is_atom(Fun) ->
    io:format("~w:t -> entry with"
	      "~n   Mod:    ~p"
	      "~n   Fun:    ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Mod, Fun, Config]),
    case catch apply(Mod, Fun, [suite]) of
        [] ->
            io:format("Eval:   ~p:", [{Mod, Fun}]),
            Res = eval(Mod, Fun, Config),
            {R, _, _, _} = Res,
            io:format(" ~p~n", [R]),
            Res;

        Cases when is_list(Cases) ->
            io:format("Expand: ~p ...~n", [{Mod, Fun}]),
            Map = fun(Case) when is_atom(Case) -> {Mod, Case};
                     (Case) -> Case
                  end,
            t(lists:map(Map, Cases), Config);

        {'EXIT', {undef, _}} ->
            io:format("Undefined:   ~p~n", [{Mod, Fun}]),
            [{nyi, {Mod, Fun}, ok, 0}];

        Error ->
            io:format("Ignoring:   ~p: ~p~n", [{Mod, Fun}, Error]),
            [{failed, {Mod, Fun}, Error, 0}]
    end;
t(Mod, Config) when is_atom(Mod) ->
    io:format("~w:t -> entry with"
	      "~n   Mod:    ~p"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Mod, Config]),
    %% This is assumed to be a test suite, so we start by calling
    %% the top test suite function(s) (all/0 and groups/0).
    case (catch Mod:all()) of
        Cases when is_list(Cases) ->
            %% The list may contain atoms (actual test cases) and
            %% group-tuples (a tuple naming a group of test cases).
            %% A group is defined by the (optional) groups/0 function.
	    io:format("~w:t -> suite all ok"
		      "~n   Cases: ~p"
		      "~n", [?MODULE, Cases]),
            Groups = groups(Mod),
	    io:format("~w:t -> "
		      "~n   Groups: ~p"
		      "~n", [?MODULE, Groups]),
            try init_suite(Mod, Config) of
                Config2 when is_list(Config2) ->
		    io:format("~w:t -> suite init ok"
			      "~n   Config2: ~p"
			      "~n", [?MODULE, Config2]),
                    Res = [t({Mod, Case, Groups}, Config2) || Case <- Cases],
                    (catch end_suite(Mod, Config2)),
                    Res;
                Error ->
                    io:format(" => suite init failed: ~p~n", [Error]),
                    [{failed, {Mod, init_per_suite}, Error}]
            catch
                exit:{skipped, SkipReason} ->
                    io:format(" => skipping suite: ~p~n", [SkipReason]),
                    [{skipped, {Mod, init_per_suite}, SkipReason, 0}];
                exit:{undef, _} ->
                    io:format("~w:t -> suite init failed. exit undef(1)~n", [?MODULE]),
                    [t({Mod, Case, Groups}, Config) || Case <- Cases];
                exit:undef ->
                    io:format("~w:t -> suite init failed. exit undef(2)~n", [?MODULE]),
                    [t({Mod, Case, Groups}, Config) || Case <- Cases];
                T:E ->
                    io:format("~w:t -> suite init failed. "
			      "~n   T: ~p"
			      "~n   E: ~p"
			      "~n", [?MODULE, T,E]),
                    [{failed, {Mod, init_per_suite}, {T,E}, 0}]
            end;
        {'EXIT', {undef, _}} ->
            io:format("Undefined:   ~p~n", [{Mod, all}]),
            [{nyi, {Mod, all}, ok, 0}];

        Crap ->
	    io:format("~w:t -> suite all failed: "
		      "~n   Crap: ~p"
		      "~n", [?MODULE, Crap]),
            Crap
    end;
t(Bad, _Config) ->
    io:format("~w:t -> entry with"
	      "~n   Bad: ~p"
	      "~n", [?MODULE, Bad]),
    [{badarg, Bad, ok, 0}].

eval(Mod, Fun, Config) ->
    TestCase = {?MODULE, Mod, Fun},
    Label = lists:concat(["TEST CASE: ", Fun]),
    ?REPORT_VERBOSE(Label ++ " started", [TestCase, Config]),
    global:register_name(diameter_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    put(diameter_test_server, true),
    Config2 = Mod:init_per_testcase(Fun, Config),
    Self = self(), 
    Pid = spawn_link(fun() -> do_eval(Self, Mod, Fun, Config2) end),
    R = wait_for_evaluator(Pid, Mod, Fun, Config2, []),
    Mod:fin_per_testcase(Fun, Config2),
    erase(diameter_test_server),    
    global:unregister_name(diameter_test_case_sup),
    process_flag(trap_exit, Flag),
    R.

wait_for_evaluator(Pid, Mod, Fun, Config, Errors) ->
    wait_for_evaluator(Pid, Mod, Fun, Config, Errors, 0).
wait_for_evaluator(Pid, Mod, Fun, Config, Errors, AccTime) ->
    TestCase = {?MODULE, Mod, Fun},
    Label = lists:concat(["TEST CASE: ", Fun]),
    receive
	{done, Pid, ok, Time} when Errors =:= [] ->
	    ?REPORT_VERBOSE(Label ++ " ok", 
			    [{test_case, TestCase}, {config, Config}]),
	    {ok, {Mod, Fun}, Errors, Time};
	{done, Pid, ok, Time} ->
	    ?REPORT_VERBOSE(Label ++ " failed", 
			    [{test_case, TestCase}, {config, Config}]),
	    {failed, {Mod, Fun}, Errors, Time};
	{done, Pid, {ok, _}, Time} when Errors =:= [] ->
	    ?REPORT_VERBOSE(Label ++ " ok",
			    [{test_case, TestCase}, {config, Config}]),
	    {ok, {Mod, Fun}, Errors, Time};
	{done, Pid, {ok, _}, Time} ->
	    ?REPORT_VERBOSE(Label ++ " failed",
			    [{test_case, TestCase}, {config, Config}]),
	    {failed, {Mod, Fun}, Errors, Time};
	{done, Pid, Fail, Time} ->
	    ?REPORT_IMPORTANT(Label ++ " failed",
			      [{test_case, TestCase}, 
			       {config,    Config}, 
			       {return,    Fail}, 
			       {errors,    Errors}]),
	    {failed, {Mod, Fun}, Fail, Time};
	{'EXIT', Pid, {skipped, Reason}, Time} -> 
	    ?REPORT_IMPORTANT(Label ++ " skipped",
			      [{test_case, TestCase}, 
			       {config,    Config}, 
			       {skipped,   Reason}]),
	    {skipped, {Mod, Fun}, Errors, Time};
	{'EXIT', Pid, Reason, Time} -> 
	    ?REPORT_IMPORTANT(Label ++ " crashed",
			      [{test_case, TestCase}, 
			       {config,    Config}, 
			       {'EXIT',    Reason}]),
	    {crashed, {Mod, Fun}, [{'EXIT', Reason} | Errors], Time};
	{fail, Pid, Reason, Time} ->
	    wait_for_evaluator(Pid, Mod, Fun, Config, 
			       Errors ++ [Reason], AccTime + Time)
    end.

do_eval(ReplyTo, Mod, Fun, Config) ->
    diameter_test_lib:display_system_info("before", Mod, Fun),
    case timer:tc(Mod, Fun, [Config]) of
	{Time, {'EXIT', {skipped, Reason}}} ->
	    display_tc_time(Time),
	    diameter_test_lib:display_system_info("after (skipped)", Mod, Fun),
	    ReplyTo ! {'EXIT', self(), {skipped, Reason}, Time};
	{Time, {'EXIT', Reason}} ->
	    display_tc_time(Time),
	    diameter_test_lib:display_system_info("after (crashed)", Mod, Fun),
	    ReplyTo ! {'EXIT', self(), Reason, Time};
	{Time, Other} ->
	    display_tc_time(Time),
	    diameter_test_lib:display_system_info("after", Mod, Fun),
	    ReplyTo ! {done, self(), Other, Time}
    end,
    unlink(ReplyTo),
    exit(shutdown).


display_tc_time(Time) ->
    io:format("~n"
	      "~n*********************************************"
	      "~n"
	      "~nTest case completion time: ~.3f sec (~w)"
	      "~n", [(Time / 1000000), Time]),
    ok.


display_result(Res, Alloc1, Mem1, Alloc2, Mem2) ->
    io:format("~nAllocator info: ~n", []),
    display_alloc(Alloc1, Alloc2),
    io:format("~nMemory info: ~n", []),
    display_memory(Mem1, Mem2),
    display_result(Res).

display_alloc([], []) ->
    io:format("-~n", []),
    ok;
display_alloc(A1, A2) ->
    do_display_alloc(A1, A2).

do_display_alloc([], _) ->
    ok;
do_display_alloc([{Alloc, Mem1}|AllocInfo1], AllocInfo2) ->
    Mem2 = 
	case lists:keysearch(Alloc, 1, AllocInfo2) of
	    {value, {_, Val}} ->
		Val;
	    false ->
		undefined
	end,
    io:format("~15w: ~10w -> ~w~n", [Alloc, Mem1, Mem2]),
    do_display_alloc(AllocInfo1, AllocInfo2).

display_memory([], []) ->
    io:format("-~n", []),
    ok;
display_memory(Mem1, Mem2) ->
    do_display_memory(Mem1, Mem2).


do_display_memory([], _) ->
    ok;
do_display_memory([{Key, Mem1}|MemInfo1], MemInfo2) ->
    Mem2 = 
	case lists:keysearch(Key, 1, MemInfo2) of
	    {value, {_, Val}} ->
		Val;
	    false ->
		undefined
	end,
    io:format("~15w: ~10w -> ~w~n", [Key, Mem1, Mem2]),
    do_display_memory(MemInfo1, MemInfo2).

display_result([]) ->    
    io:format("OK~n", []);
display_result(Res) when is_list(Res) ->
    Ok      = [{MF, Time} || {ok,  MF, _, Time}  <- Res],
    Nyi     = [MF || {nyi, MF, _, _Time} <- Res],
    Skipped = [{MF, Reason} || {skipped, MF, Reason, _Time} <- Res],
    Failed  = [{MF, Reason} || {failed,  MF, Reason, _Time} <- Res],
    Crashed = [{MF, Reason} || {crashed, MF, Reason, _Time} <- Res],
    display_summery(Ok, Nyi, Skipped, Failed, Crashed),
    display_ok(Ok),
    display_skipped(Skipped),
    display_failed(Failed),
    display_crashed(Crashed).

display_summery(Ok, Nyi, Skipped, Failed, Crashed) ->
    io:format("~nTest case summery:~n", []),
    display_summery(Ok,      "successfull"),
    display_summery(Nyi,     "not yet implemented"),
    display_summery(Skipped, "skipped"),
    display_summery(Failed,  "failed"),
    display_summery(Crashed, "crashed"),
    io:format("~n", []).
   
display_summery(Res, Info) ->
    io:format("  ~w test cases ~s~n", [length(Res), Info]).
    
display_ok([]) ->
    ok;
display_ok(Ok) ->
    io:format("Ok test cases:~n", []),
    F = fun({{M, F}, Time}) -> 
		io:format("  ~w : ~w => ~.2f sec~n", [M, F, Time / 1000000]) 
	end,
    lists:foreach(F, Ok),
    io:format("~n", []).

display_skipped([]) ->
    ok;
display_skipped(Skipped) ->
    io:format("Skipped test cases:~n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p~n", [MF, Reason]) end,
    lists:foreach(F, Skipped),
    io:format("~n", []).


display_failed([]) ->
    ok;
display_failed(Failed) ->
    io:format("Failed test cases:~n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p~n", [MF, Reason]) end,
    lists:foreach(F, Failed),
    io:format("~n", []).

display_crashed([]) ->
    ok;
display_crashed(Crashed) ->
    io:format("Crashed test cases:~n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p~n", [MF, Reason]) end,
    lists:foreach(F, Crashed),
    io:format("~n", []).
        
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks
init_per_testcase(_Case, Config) ->
    Pid = group_leader(),
    Name = ?GLOGGER, 
    case global:whereis_name(Name) of
	undefined ->
	    global:register_name(?GLOGGER, Pid);
	Pid ->
	    io:format("~w:init_per_testcase -> "
		      "already registered to ~p~n", [?MODULE, Pid]),
	    ok;
	OtherPid when is_pid(OtherPid) ->
	    io:format("~w:init_per_testcase -> "
		      "already registered to other ~p (~p)~n", 
		      [?MODULE, OtherPid, Pid]),
	    exit({already_registered, {?GLOGGER, OtherPid, Pid}})
    end,
    set_kill_timer(Config).

fin_per_testcase(_Case, Config) ->
    Name = ?GLOGGER, 
    case global:whereis_name(Name) of
	undefined ->
	    io:format("~w:fin_per_testcase -> already un-registered~n", 
		      [?MODULE]),
	    ok;
	Pid when is_pid(Pid) ->
	    global:unregister_name(?GLOGGER),
	    ok
    end,
    reset_kill_timer(Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set kill timer

set_kill_timer(Config) ->
    case init:get_argument(diameter_test_timeout) of
	{ok, _} -> 
	    Config;
	_ ->
	    Time = 
		case lookup_config(tc_timeout, Config) of
		    [] ->
			timer:minutes(5);
		    ConfigTime when is_integer(ConfigTime) ->
			ConfigTime
		end,
	    Dog = 
		case get(diameter_test_server) of
		    true ->
			Self = self(), 
			spawn_link(fun() -> watchdog(Self, Time) end);
		    _ ->
			test_server:timetrap(Time)
		end,
	    [{kill_timer, Dog}|Config]
		    
	    
    end.

reset_kill_timer(Config) ->
    DogKiller = 
	case get(diameter_test_server) of
	    true ->
		fun(P) when is_pid(P) -> P ! stop;
		   (_) -> ok 
		end;
	    _ ->
		fun(Ref) -> test_server:timetrap_cancel(Ref) end
	end,
    case lists:keysearch(kill_timer, 1, Config) of
	{value, {kill_timer, Dog}} ->
	    DogKiller(Dog), 
	    lists:keydelete(kill_timer, 1, Config);
	_ ->
	    Config
    end.

watchdog(Pid, Time) ->
    erlang:now(),
    receive
	stop ->
	    ok
    after Time ->
	    case (catch process_info(Pid)) of
		undefined ->
		    ok;
		Info ->
		    ?LOG("<ERROR> Watchdog in test case timed out "
			"for ~p after ~p min"
			 "~n~p"
			 "~n",
		    [Pid, Time div (1000*60), Info]),
		    exit(Pid, kill)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_config(Key, Config) ->
    diameter_test_lib:lookup_config(Key, Config).

default_config() ->
    [{nodes, default_nodes()}, {ts, diameter}].

default_nodes() ->    
    mk_nodes(2, []).

mk_nodes(0, Nodes) ->
    Nodes;
mk_nodes(N, []) ->
    mk_nodes(N - 1, [node()]);
mk_nodes(N, Nodes) when N > 0 ->
    Head = hd(Nodes),
    [Name, Host] = node_to_name_and_host(Head),
    Nodes ++ [mk_node(I, Name, Host) || I <- lists:seq(1, N)].

mk_node(N, Name, Host) ->
    list_to_atom(lists:concat([Name ++ integer_to_list(N) ++ "@" ++ Host])).
    
%% Returns [Name, Host]    
node_to_name_and_host(Node) ->
    string:tokens(atom_to_list(Node), [$@]).




