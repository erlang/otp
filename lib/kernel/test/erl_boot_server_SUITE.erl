%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(erl_boot_server_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, init_per_group/2,end_per_group/2]).

-export([start/1, start_link/1, stop/1, add/1, delete/1, responses/1]).

%%-----------------------------------------------------------------
%% Test suite for erl_boot_server.
%%
%% This module is mainly tested in the erl_prim_loader_SUITE,
%% but the interface functions are tested here.
%%
%% Changed for the new erl_boot_server for R3A by Bjorn Gustavsson.
%%-----------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start, start_link, stop, add, delete, responses].

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


-define(all_ones, {255, 255, 255, 255}).

start(doc) -> "Tests the erl_boot_server:start/1 function.";
start(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(50)),
    ?line [Host1, Host2|_] = good_hosts(Config),

    %% Bad arguments.
    BadHost = "bad__host",
    ?line {error, {badarg, {}}} = erl_boot_server:start({}),
    ?line {error, {badarg, atom}} = erl_boot_server:start(atom),
    ?line {error, {badarg, [atom, BadHost]}} =
	erl_boot_server:start([atom, BadHost]),
    ?line {error, {badarg, [Host1, BadHost]}} =
	erl_boot_server:start([Host1, BadHost]),

    %% Test once.
    ?line {ok, Pid1} = erl_boot_server:start([Host1]),
    ?line {error, {already_started, Pid1}} =
	erl_boot_server:start([Host1]),
    ?line exit(Pid1, kill),

    %% Test again.
    test_server:sleep(1),
    ?line {ok, Pid2} = erl_boot_server:start([Host1, Host2]),
    ?line {error, {already_started, Pid2}} =
	erl_boot_server:start([Host1, Host2]),
    ?line exit(Pid2, kill),
    test_server:sleep(1),

    ?line test_server:timetrap_cancel(Dog),
    ok.

start_link(doc) -> "Tests the erl_boot_server:start_link/1 function.";
start_link(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line [Host1, Host2|_] = good_hosts(Config),

    OldFlag = process_flag(trap_exit, true),
    ?line {error, {badarg, {}}} = erl_boot_server:start_link({}),
    ?line {error, {badarg, atom}} = erl_boot_server:start_link(atom),
    ?line BadHost = "bad__host",
    ?line {error, {badarg, [atom, BadHost]}} =
	erl_boot_server:start_link([atom, BadHost]),

    ?line {ok, Pid1} = erl_boot_server:start_link([Host1]),
    ?line {error, {already_started, Pid1}} =
	erl_boot_server:start_link([Host1]),
    ?line shutdown(Pid1),

    ?line {ok, Pid2} = erl_boot_server:start_link([Host1, Host2]),
    ?line {error, {already_started, Pid2}} =
	erl_boot_server:start_link([Host1, Host2]),
    ?line shutdown(Pid2),
    process_flag(trap_exit, OldFlag),

    ?line test_server:timetrap_cancel(Dog),
    ok.

stop(doc) -> "Tests that no processes are left if a boot server is killed.";
stop(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(50)),
    ?line [Host1|_] = good_hosts(Config),

    %% Start a boot server and kill it.  Make sure that any helper processes
    %% dies.
    % Make sure the inet_gethost_native server is already started,
    % otherwise it will make this test fail:
    ?line inet:getaddr(localhost, inet),
    ?line Before = processes(),
    ?line {ok, Pid} = erl_boot_server:start([Host1]),
    ?line New = processes() -- [Pid|Before],
    ?line exit(Pid, kill),
    ?line receive after 100 -> ok end,
    ?line case [P || P <- New, is_process_alive(P)] of
	      [] ->
		  ok;
	      NotKilled ->
		  test_server:fail({not_killed, NotKilled})
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

add(doc) -> "Tests the erl_boot_server:add/1 function.";
add(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line OldFlag = process_flag(trap_exit, true),
    ?line {ok, Pid1} = erl_boot_server:start_link([]),
    ?line [] = erl_boot_server:which_slaves(),
    ?line [Host1, Host2, Host3|_] = good_hosts(Config),

    %% Try bad values.
    ?line {error, {badarg, {}}} = erl_boot_server:add_slave({}),
    ?line {error, {badarg, [atom]}} = erl_boot_server:add_slave([atom]),
    ?line BadHost = "bad__host",
    ?line {error, {badarg, BadHost}} = erl_boot_server:add_slave(BadHost),
    ?line [] = erl_boot_server:which_slaves(),

    %% Add good host names.
    ?line {ok, Ip1} = inet:getaddr(Host1, inet),
    ?line {ok, Ip2} = inet:getaddr(Host2, inet),
    ?line {ok, Ip3} = inet:getaddr(Host3, inet),
    ?line MIp1 = {?all_ones, Ip1},
    ?line MIp2 = {?all_ones, Ip2},
    ?line MIp3 = {?all_ones, Ip3},
    ?line ok = erl_boot_server:add_slave(Host1),
    ?line [MIp1] = erl_boot_server:which_slaves(),
    ?line ok = erl_boot_server:add_slave(Host2),
    ?line M_Ip1_Ip2 = lists:sort([MIp1, MIp2]),
    ?line M_Ip1_Ip2 = lists:sort(erl_boot_server:which_slaves()),
    ?line ok = erl_boot_server:add_slave(Host3),
    ?line M_Ip1_Ip2_Ip3 = lists:sort([MIp3|M_Ip1_Ip2]),
    ?line M_Ip1_Ip2_Ip3 = erl_boot_server:which_slaves(),

    %% Add duplicate names.
    ?line ok = erl_boot_server:add_slave(Host3),
    ?line M_Ip1_Ip2_Ip3 = erl_boot_server:which_slaves(),

    %% More bad names.
    ?line {error, {badarg, BadHost}} = erl_boot_server:add_slave(BadHost),
    ?line M_Ip1_Ip2_Ip3 = erl_boot_server:which_slaves(),

    %% Cleanup.
    ?line shutdown(Pid1),
    ?line process_flag(trap_exit, OldFlag),
    ?line test_server:timetrap_cancel(Dog),
    ok.

delete(doc) -> "Tests the erl_boot_server:delete/1 function.";
delete(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line OldFlag = process_flag(trap_exit, true),

    ?line [Host1, Host2, Host3|_] = good_hosts(Config),
    ?line {ok, Ip1} = inet:getaddr(Host1, inet),
    ?line {ok, Ip2} = inet:getaddr(Host2, inet),
    ?line {ok, Ip3} = inet:getaddr(Host3, inet),
    ?line MIp1 = {?all_ones, Ip1},
    ?line MIp2 = {?all_ones, Ip2},
    ?line MIp3 = {?all_ones, Ip3},

    ?line {ok, Pid1} = erl_boot_server:start_link([Host1, Host2, Host3]),
    ?line M_Ip123 = lists:sort([MIp1, MIp2, MIp3]),
    ?line M_Ip123 = erl_boot_server:which_slaves(),

    %% Do some bad attempts and check that the list of slaves is intact.
    ?line {error, {badarg, {}}} = erl_boot_server:delete_slave({}),
    ?line {error, {badarg, [atom]}} = erl_boot_server:delete_slave([atom]),
    ?line BadHost = "bad__host",
    ?line {error, {badarg, BadHost}} = erl_boot_server:delete_slave(BadHost),
    ?line M_Ip123 = erl_boot_server:which_slaves(),

    %% Delete Host2 and make sure it's gone.
    ?line ok = erl_boot_server:delete_slave(Host2),
    ?line M_Ip13 = lists:sort([MIp1, MIp3]),
    ?line M_Ip13 = erl_boot_server:which_slaves(),

    ?line ok = erl_boot_server:delete_slave(Host1),
    ?line [MIp3] = erl_boot_server:which_slaves(),
    ?line ok = erl_boot_server:delete_slave(Host1),
    ?line [MIp3] = erl_boot_server:which_slaves(),

    ?line {error, {badarg, BadHost}} = erl_boot_server:delete_slave(BadHost),
    ?line [MIp3] = erl_boot_server:which_slaves(),

    ?line ok = erl_boot_server:delete_slave(Ip3),
    ?line [] = erl_boot_server:which_slaves(),
    ?line ok = erl_boot_server:delete_slave(Ip3),
    ?line [] = erl_boot_server:which_slaves(),

    ?line shutdown(Pid1),
    ?line process_flag(trap_exit, OldFlag),
    ?line test_server:timetrap_cancel(Dog),
    ok.

responses(doc) -> "Tests erl_boot_server responses to slave requests.";
responses(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(30)),
    ?line process_flag(trap_exit, true),
    %% Copy from inet_boot.hrl
    EBOOT_PORT = 4368,
    EBOOT_REQUEST = "EBOOTQ",
    EBOOT_REPLY =   "EBOOTR",

    ?line {ok,Host} = inet:gethostname(),
    ?line {ok,Ip} = inet:getaddr(Host, inet),

    ThisVer = erlang:system_info(version),

    ?line {ok,BootPid} = erl_boot_server:start_link([Host]),

    %% Send junk
    ?line S1 = open_udp(),
    ?line prim_inet:sendto(S1, Ip, EBOOT_PORT, ["0"]),
    receive
	What ->
	    ?line close_udp(S1),
	    ?line ?t:fail({"got unexpected response",What})
    after 100 ->
	    ok
    end,

    %% Req from a slave with same erlang vsn.
    ?line S2 = open_udp(),
    ?line prim_inet:sendto(S2, Ip, EBOOT_PORT, [EBOOT_REQUEST,ThisVer]),
    receive
	{udp,S2,Ip,_Port1,Resp1} ->
	    ?line close_udp(S2),
	    ?line EBOOT_REPLY = string:substr(Resp1, 1, length(EBOOT_REPLY)),
	    ?line Rest1 = string:substr(Resp1, length(EBOOT_REPLY)+1, length(Resp1)),
	    ?line [_,_,_ | ThisVer] = Rest1
    after 2000 ->
	    ?line close_udp(S2),
	    ?line ?t:fail("no boot server response; same vsn")
    end,
    
    %% Req from a slave with other erlang vsn.
    ?line S3 = open_udp(),
    ?line prim_inet:sendto(S3, Ip, EBOOT_PORT, [EBOOT_REQUEST,"1.0"]),
    receive
	Anything ->
	    ?line close_udp(S3),
	    ?line ?t:fail({"got unexpected response",Anything})
    after 100 ->
	    ok
    end,

    %% Kill the boot server and wait for it to disappear.
    ?line unlink(BootPid),
    ?line BootPidMref = erlang:monitor(process, BootPid),
    ?line exit(BootPid, kill),
    receive
	{'DOWN',BootPidMref,_,_,_} -> ok
    end,

    ?line {ok,BootPid2} = erl_boot_server:start_link(["127.0.0.1"]),

    %% Req from slave with invalid ip address.
    ?line S4 = open_udp(),
    Ret =
	case Ip of
	    {127,0,0,1} ->
		{comment,"IP address for this host is 127.0.0.1"};
	    _ ->
		?line prim_inet:sendto(S4, Ip, EBOOT_PORT,
				       [EBOOT_REQUEST,ThisVer]),
		receive
		    Huh ->
			?line close_udp(S4),
			?line ?t:fail({"got unexpected response",Huh})
		after 100 ->
			ok
		end
	end,

    ?line unlink(BootPid2),
    ?line exit(BootPid2, kill),

    %% Now wait for any late unexpected messages.
    receive
	Whatever ->
	    ?line ?t:fail({unexpected_message,Whatever})
    after 4000 ->
	    ?line close_udp(S1),
	    ?line close_udp(S3),
	    ?line close_udp(S4),
	    ok
    end,

    ?line test_server:timetrap_cancel(Dog),
    Ret.

shutdown(Pid) ->
    exit(Pid, shutdown),
    receive
	{'EXIT', Pid, shutdown} ->
	    ok
    after 1000 ->
	    %% The timeout used to be 1 ms, which could be too short time for the
	    %% SMP emulator on a slow computer with one CPU.
	    test_server:fail(shutdown)
    end.

good_hosts(_Config) ->
    %% XXX The hostnames should not be hard-coded like this. Really!

    {ok, GoodHost1} = inet:gethostname(),
    GoodHost2 = "gandalf",
    GoodHost3 = "sauron",
    [GoodHost1, GoodHost2, GoodHost3].

open_udp() ->
    ?line {ok, S} = prim_inet:open(udp, inet, dgram),
    ?line ok = prim_inet:setopts(S, [{mode,list},{active,true}, 
				     {deliver,term},{broadcast,true}]),
    ?line {ok,_} = prim_inet:bind(S, {0,0,0,0}, 0),
    S.

close_udp(S) ->
    prim_inet:close(S).
