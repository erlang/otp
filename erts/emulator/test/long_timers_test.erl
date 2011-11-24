%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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


%%%-------------------------------------------------------------------
%%% File    : long_timer_test.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 21 Aug 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------


-define(MAX_TIMEOUT, 60). % Minutes
-define(MAX_LATE, 10*1000). % Milliseconds
-define(REG_NAME, '___LONG___TIMERS___TEST___SERVER___').

-define(DRV_NAME, timer_driver).

% First byte in communication with the timer driver
-define(START_TIMER, 0).
-define(CANCEL_TIMER, 1).
-define(DELAY_START_TIMER, 2).
-define(TIMER, 3).
-define(CANCELLED, 4).

-module(long_timers_test).

-export([start/1, check_result/0]).

-record(timeout_rec,{pid, type, timeout, timeout_diff}).

start(DrvDir) when is_list(DrvDir) ->
    Starter = self(),
    StartDone = make_ref(),
    stop_node(full_node_name(?REG_NAME)),
    Node = start_node(?REG_NAME),
    Test = spawn(Node, fun () -> test(Starter, DrvDir, StartDone) end),
    Mon = erlang:monitor(process, Test),
    receive
	StartDone ->
	    erlang:demonitor(Mon),
	    net_kernel:disconnect(Node),
	    receive {'DOWN',Mon,_,_,_} -> ok after 0 -> ok end;
	{'DOWN',Mon,_,_,Reason} ->
	    stop_node(full_node_name(?REG_NAME)),
	    {error, Reason}
    end.

check_result() ->
    Node = full_node_name(?REG_NAME),
    LTTS = {?REG_NAME, Node},
    Mon = erlang:monitor(process, LTTS),
    (catch LTTS ! {get_result, ?REG_NAME, self()}),
    receive
	{'DOWN', Mon, process, _, Reason} ->
	    {?REG_NAME, 'DOWN', Reason};
	{result, ?REG_NAME, TORs, Start, End} ->
	    erlang:demonitor(Mon),
	    receive {'DOWN', Mon, _, _, _} -> ok after 0 -> ok end,
	    stop_node(Node),
	    check(TORs, (timer:now_diff(End, Start) div 1000) - ?MAX_LATE, ok)
    end.

check([#timeout_rec{timeout = Timeout,
		    type = Type,
		    timeout_diff = undefined} | TORs],
      NeedRes,
      _Ok) when Timeout < NeedRes ->
    io:format("~p timeout = ~p failed! No timeout.~n",
	      [Type, Timeout]),
    check(TORs, NeedRes, failed);
check([#timeout_rec{timeout_diff = undefined} | TORs],
      NeedRes,
      Ok) ->
    check(TORs, NeedRes, Ok);
check([#timeout_rec{timeout = Timeout,
		    type = Type,
		    timeout_diff = {error, Reason}} | TORs],
      NeedRes,
      _Ok) ->
    io:format("~p timeout = ~p failed! exit reason ~p~n",
	      [Type, Timeout, Reason]),
    check(TORs, NeedRes, failed);
check([#timeout_rec{timeout = Timeout,
		    type = Type,
		    timeout_diff = TimeoutDiff} | TORs],
      NeedRes,
      Ok) ->
    case (0 =< TimeoutDiff) and (TimeoutDiff =< ?MAX_LATE) of
	true ->
	    io:format("~p timeout = ~p succeded! timeout diff = ~p.~n",
		      [Type, Timeout, TimeoutDiff]),
	    check(TORs, NeedRes, Ok);
	false ->
	    io:format("~p timeout = ~p failed! timeout diff = ~p.~n",
		      [Type, Timeout, TimeoutDiff]),
	    check(TORs, NeedRes, failed)
    end;
check([], _NeedRes, Ok) ->
    Ok.

receive_after(Timeout) ->
    Start = now(),
    receive
	{get_result, ?REG_NAME} ->
	    ?REG_NAME ! #timeout_rec{pid = self(),
				     type = receive_after,
				     timeout = Timeout}
    after Timeout ->
	    Stop = now(),
	    receive
		{get_result, ?REG_NAME} ->
	    	    TimeoutDiff = ((timer:now_diff(Stop, Start) div 1000)
				   - Timeout),
		    ?REG_NAME ! #timeout_rec{pid = self(),
					     type = receive_after,
					     timeout = Timeout,
					     timeout_diff = TimeoutDiff}
	    end
    end.

driver(Timeout) ->
    Port = open_port({spawn, ?DRV_NAME},[]),
    link(Port),
    Start = now(),
    erlang:port_command(Port, <<?START_TIMER, Timeout:32>>),
    receive
	{get_result, ?REG_NAME} ->
	    ?REG_NAME ! #timeout_rec{pid = self(),
				     type = driver,
				     timeout = Timeout};
	{Port,{data,[?TIMER]}} ->
	    Stop = now(),
	    unlink(Port),
	    true = erlang:port_close(Port),
	    receive
		{get_result, ?REG_NAME} ->
	    	    TimeoutDiff = ((timer:now_diff(Stop, Start) div 1000)
				   - Timeout),
		    ?REG_NAME ! #timeout_rec{pid = self(),
					     type = driver,
					     timeout = Timeout,
					     timeout_diff = TimeoutDiff}
	    end
    end.

bif_timer(Timeout) ->
    Tmr = erlang:start_timer(Timeout, self(), ok),
    Start = now(),
    receive
	{get_result, ?REG_NAME} ->
	    ?REG_NAME ! #timeout_rec{pid = self(),
				     type = bif_timer,
				     timeout = Timeout};
	{timeout, Tmr, ok} ->
	    Stop = now(),
	    receive
		{get_result, ?REG_NAME} ->
	    	    TimeoutDiff = ((timer:now_diff(Stop, Start) div 1000)
				   - Timeout),
		    ?REG_NAME ! #timeout_rec{pid = self(),
					     type = bif_timer,
					     timeout = Timeout,
					     timeout_diff = TimeoutDiff}
	    end
    end.

test(Starter, DrvDir, StartDone) ->
    erl_ddll:start(),
    ok = load_driver(DrvDir, ?DRV_NAME),
    process_flag(trap_exit, true),
    register(?REG_NAME, self()),
    {group_leader, GL} = process_info(whereis(net_kernel),group_leader),
    group_leader(GL, self()),
    Start = now(),
    TORs = lists:map(fun (Min) ->
			     TO = Min*60*1000,
			     [#timeout_rec{pid = spawn_opt(
						   fun () ->
							   receive_after(TO)
						   end,
						   [link, {priority, high}]),
					   type = receive_after,
					   timeout = TO},
			      #timeout_rec{pid = spawn_opt(
						   fun () ->
							   driver(TO)
						   end,
						   [link, {priority, high}]),
					   type = driver,
					   timeout = TO},
			      #timeout_rec{pid = spawn_opt(
						   fun () ->
							   bif_timer(TO)
						   end,
						   [link, {priority, high}]),
					   type = bif_timer,
					   timeout = TO}]
		     end,
		     lists:seq(1, ?MAX_TIMEOUT)),
    FlatTORs = lists:flatten(TORs),
    Starter ! StartDone,
    test_loop(FlatTORs, Start).

test_loop(TORs, Start) ->
    receive
	{get_result, ?REG_NAME, Pid} ->
	    End = now(),
	    Pid ! {result, ?REG_NAME, get_test_results(TORs), Start, End},
	    erl_ddll:unload_driver(?DRV_NAME),
	    erl_ddll:stop(),
	    exit(bye)
    end.

get_test_results(TORs) ->
    lists:foreach(fun (#timeout_rec{pid = Pid}) ->
			  Pid ! {get_result, ?REG_NAME}
		  end,
		  TORs),
    get_test_results(TORs, []).

get_test_results([#timeout_rec{pid = Pid,
			       timeout = Timeout} = TOR | TORs], NewTORs) ->
    receive
	#timeout_rec{pid = Pid, timeout = Timeout} = NewTOR ->
	    get_test_results(TORs, [NewTOR | NewTORs]);
	#timeout_rec{pid = Pid} = NewTOR ->
	    exit({timeout_mismatch, TOR, NewTOR});
	{'EXIT', Pid, Reason} ->
	    get_test_results(TORs,
			     [TOR#timeout_rec{timeout_diff = {error, Reason}}
			      | NewTORs])
	end;
get_test_results([], NewTORs) ->
    lists:reverse(NewTORs).

mk_node_cmdline(Name) ->
    Static = "-detached -noinput",
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok,[[P]]} -> P;
	       _ -> exit(no_progname_argument_found)
	   end,
    NameSw = case net_kernel:longnames() of
		 false -> "-sname ";
		 true -> "-name ";
		 _ -> exit(not_distributed_node)
	     end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(Name),
    Prog ++ " "
	++ Static ++ " "
	++ NameSw ++ " " ++ NameStr ++ " "
	++ "-pa " ++ Pa ++ " "
	++ "-env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr ++ " "
	++ "-setcookie " ++ atom_to_list(erlang:get_cookie()).

full_node_name(PreName) ->
    HostSuffix = lists:dropwhile(fun ($@) -> false; (_) -> true end,
				 atom_to_list(node())),
    list_to_atom(atom_to_list(PreName) ++ HostSuffix).

ping_node(_Node, 0) ->
    pang;
ping_node(Node, N) when is_integer(N), N > 0 ->
    case catch net_adm:ping(Node) of
	pong -> pong;
	_ ->
	    receive after 100 -> ok end,
	    ping_node(Node, N-1)
    end.

start_node(Name) ->
    FullName = full_node_name(Name),
    CmdLine = mk_node_cmdline(Name),
    io:format("Starting node ~p: ~s~n", [FullName, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when is_port(Port) ->
	    unlink(Port),
	    erlang:port_close(Port),
	    case ping_node(FullName, 50) of
		pong -> FullName;
		Other -> exit({failed_to_start_node, FullName, Other})
	    end;
	Error ->
	    exit({failed_to_start_node, FullName, Error})
    end.

stop_node(Node) ->
    monitor_node(Node, true),
    spawn(Node, fun () -> halt() end),
    receive {nodedown, Node} -> ok end.
	    
load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.
