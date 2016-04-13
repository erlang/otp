%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(erl_boot_server_SUITE).

-include_lib("common_test/include/ct.hrl").

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

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

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

%% Tests the erl_boot_server:start/1 function.
start(Config) when is_list(Config) ->
    [Host1, Host2|_] = good_hosts(Config),

    %% Bad arguments.
    BadHost = "bad__host",
    {error, {badarg, {}}} = erl_boot_server:start({}),
    {error, {badarg, atom}} = erl_boot_server:start(atom),
    {error, {badarg, [atom, BadHost]}} =
	erl_boot_server:start([atom, BadHost]),
    {error, {badarg, [Host1, BadHost]}} =
	erl_boot_server:start([Host1, BadHost]),

    %% Test once.
    {ok, Pid1} = erl_boot_server:start([Host1]),
    {error, {already_started, Pid1}} =
	erl_boot_server:start([Host1]),
    exit(Pid1, kill),

    %% Test again.
    ct:sleep(1),
    {ok, Pid2} = erl_boot_server:start([Host1, Host2]),
    {error, {already_started, Pid2}} =
	erl_boot_server:start([Host1, Host2]),
    exit(Pid2, kill),
    ct:sleep(1),

    ok.

%% Tests the erl_boot_server:start_link/1 function.
start_link(Config) when is_list(Config) ->
    [Host1, Host2|_] = good_hosts(Config),

    OldFlag = process_flag(trap_exit, true),
    {error, {badarg, {}}} = erl_boot_server:start_link({}),
    {error, {badarg, atom}} = erl_boot_server:start_link(atom),
    BadHost = "bad__host",
    {error, {badarg, [atom, BadHost]}} =
	erl_boot_server:start_link([atom, BadHost]),

    {ok, Pid1} = erl_boot_server:start_link([Host1]),
    {error, {already_started, Pid1}} =
	erl_boot_server:start_link([Host1]),
    shutdown(Pid1),

    {ok, Pid2} = erl_boot_server:start_link([Host1, Host2]),
    {error, {already_started, Pid2}} =
	erl_boot_server:start_link([Host1, Host2]),
    shutdown(Pid2),
    process_flag(trap_exit, OldFlag),

    ok.

%% Tests that no processes are left if a boot server is killed.
stop(Config) when is_list(Config) ->
    [Host1|_] = good_hosts(Config),

    %% Start a boot server and kill it.  Make sure that any helper processes
    %% dies.
    %% Make sure the inet_gethost_native server is already started,
    %% otherwise it will make this test fail:
    inet:getaddr(localhost, inet),
    Before = processes(),
    {ok, Pid} = erl_boot_server:start([Host1]),
    New = processes() -- [Pid|Before],
    exit(Pid, kill),
    receive after 100 -> ok end,
    case [P || P <- New, is_process_alive(P)] of
	[] ->
	    ok;
	NotKilled ->
	    ct:fail({not_killed, NotKilled})
    end,
    ok.

%% Tests the erl_boot_server:add/1 function.
add(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    {ok, Pid1} = erl_boot_server:start_link([]),
    [] = erl_boot_server:which_slaves(),
    [Host1, Host2, Host3|_] = good_hosts(Config),

    %% Try bad values.
    {error, {badarg, {}}} = erl_boot_server:add_slave({}),
    {error, {badarg, [atom]}} = erl_boot_server:add_slave([atom]),
    BadHost = "bad__host",
    {error, {badarg, BadHost}} = erl_boot_server:add_slave(BadHost),
    [] = erl_boot_server:which_slaves(),

    %% Add good host names.
    {ok, Ip1} = inet:getaddr(Host1, inet),
    {ok, Ip2} = inet:getaddr(Host2, inet),
    {ok, Ip3} = inet:getaddr(Host3, inet),
    MIp1 = {?all_ones, Ip1},
    MIp2 = {?all_ones, Ip2},
    MIp3 = {?all_ones, Ip3},
    ok = erl_boot_server:add_slave(Host1),
    [MIp1] = erl_boot_server:which_slaves(),
    ok = erl_boot_server:add_slave(Host2),
    M_Ip1_Ip2 = lists:sort([MIp1, MIp2]),
    M_Ip1_Ip2 = lists:sort(erl_boot_server:which_slaves()),
    ok = erl_boot_server:add_slave(Host3),
    M_Ip1_Ip2_Ip3 = lists:sort([MIp3|M_Ip1_Ip2]),
    M_Ip1_Ip2_Ip3 = erl_boot_server:which_slaves(),

    %% Add duplicate names.
    ok = erl_boot_server:add_slave(Host3),
    M_Ip1_Ip2_Ip3 = erl_boot_server:which_slaves(),

    %% More bad names.
    {error, {badarg, BadHost}} = erl_boot_server:add_slave(BadHost),
    M_Ip1_Ip2_Ip3 = erl_boot_server:which_slaves(),

    %% Cleanup.
    shutdown(Pid1),
    process_flag(trap_exit, OldFlag),
    ok.

%% Tests the erl_boot_server:delete/1 function.
delete(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),

    [Host1, Host2, Host3|_] = good_hosts(Config),
    {ok, Ip1} = inet:getaddr(Host1, inet),
    {ok, Ip2} = inet:getaddr(Host2, inet),
    {ok, Ip3} = inet:getaddr(Host3, inet),
    MIp1 = {?all_ones, Ip1},
    MIp2 = {?all_ones, Ip2},
    MIp3 = {?all_ones, Ip3},

    {ok, Pid1} = erl_boot_server:start_link([Host1, Host2, Host3]),
    M_Ip123 = lists:sort([MIp1, MIp2, MIp3]),
    M_Ip123 = erl_boot_server:which_slaves(),

    %% Do some bad attempts and check that the list of slaves is intact.
    {error, {badarg, {}}} = erl_boot_server:delete_slave({}),
    {error, {badarg, [atom]}} = erl_boot_server:delete_slave([atom]),
    BadHost = "bad__host",
    {error, {badarg, BadHost}} = erl_boot_server:delete_slave(BadHost),
    M_Ip123 = erl_boot_server:which_slaves(),

    %% Delete Host2 and make sure it's gone.
    ok = erl_boot_server:delete_slave(Host2),
    M_Ip13 = lists:sort([MIp1, MIp3]),
    M_Ip13 = erl_boot_server:which_slaves(),

    ok = erl_boot_server:delete_slave(Host1),
    [MIp3] = erl_boot_server:which_slaves(),
    ok = erl_boot_server:delete_slave(Host1),
    [MIp3] = erl_boot_server:which_slaves(),

    {error, {badarg, BadHost}} = erl_boot_server:delete_slave(BadHost),
    [MIp3] = erl_boot_server:which_slaves(),

    ok = erl_boot_server:delete_slave(Ip3),
    [] = erl_boot_server:which_slaves(),
    ok = erl_boot_server:delete_slave(Ip3),
    [] = erl_boot_server:which_slaves(),

    shutdown(Pid1),
    process_flag(trap_exit, OldFlag),
    ok.

%% Tests erl_boot_server responses to slave requests.
responses(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    %% Copy from inet_boot.hrl
    EBOOT_PORT = 4368,
    EBOOT_REQUEST = "EBOOTQ",
    EBOOT_REPLY =   "EBOOTR",

    {ok,Host} = inet:gethostname(),
    {ok,Ip} = inet:getaddr(Host, inet),

    ThisVer = erlang:system_info(version),

    {ok,BootPid} = erl_boot_server:start_link([Host]),

    %% Send junk
    S1 = open_udp(),
    prim_inet:sendto(S1, Ip, EBOOT_PORT, ["0"]),
    receive
	What ->
	    close_udp(S1),
	    ct:fail({"got unexpected response",What})
    after 100 ->
	    ok
    end,

    %% Req from a slave with same erlang vsn.
    S2 = open_udp(),
    prim_inet:sendto(S2, Ip, EBOOT_PORT, [EBOOT_REQUEST,ThisVer]),
    receive
	{udp,S2,Ip,_Port1,Resp1} ->
	    close_udp(S2),
	    EBOOT_REPLY = string:substr(Resp1, 1, length(EBOOT_REPLY)),
	    Rest1 = string:substr(Resp1, length(EBOOT_REPLY)+1, length(Resp1)),
	    [_,_,_ | ThisVer] = Rest1
    after 2000 ->
	    close_udp(S2),
	    ct:fail("no boot server response; same vsn")
    end,

    %% Req from a slave with other erlang vsn.
    S3 = open_udp(),
    prim_inet:sendto(S3, Ip, EBOOT_PORT, [EBOOT_REQUEST,"1.0"]),
    receive
	Anything ->
	    close_udp(S3),
	    ct:fail({"got unexpected response",Anything})
    after 100 ->
	    ok
    end,

    %% Kill the boot server and wait for it to disappear.
    unlink(BootPid),
    BootPidMref = erlang:monitor(process, BootPid),
    exit(BootPid, kill),
    receive
	{'DOWN',BootPidMref,_,_,_} -> ok
    end,

    {ok,BootPid2} = erl_boot_server:start_link(["127.0.0.1"]),

    %% Req from slave with invalid ip address.
    S4 = open_udp(),
    Ret =
	case Ip of
	    {127,0,0,1} ->
		{comment,"IP address for this host is 127.0.0.1"};
	    _ ->
		prim_inet:sendto(S4, Ip, EBOOT_PORT,
				 [EBOOT_REQUEST,ThisVer]),
		receive
		    Huh ->
			close_udp(S4),
			ct:fail({"got unexpected response",Huh})
		after 100 ->
			ok
		end
	end,

    unlink(BootPid2),
    exit(BootPid2, kill),

    %% Now wait for any late unexpected messages.
    receive
	Whatever ->
	    ct:fail({unexpected_message,Whatever})
    after 4000 ->
	    close_udp(S1),
	    close_udp(S3),
	    close_udp(S4),
	    ok
    end,

    Ret.

shutdown(Pid) ->
    exit(Pid, shutdown),
    receive
	{'EXIT', Pid, shutdown} ->
	    ok
    after 1000 ->
	    %% The timeout used to be 1 ms, which could be too short time for the
	    %% SMP emulator on a slow computer with one CPU.
	    ct:fail(shutdown)
    end.

good_hosts(_Config) ->
    %% XXX The hostnames should not be hard-coded like this. Really!

    {ok, GoodHost1} = inet:gethostname(),
    GoodHost2 = "gandalf",
    GoodHost3 = "sauron",
    [GoodHost1, GoodHost2, GoodHost3].

open_udp() ->
    {ok, S} = prim_inet:open(udp, inet, dgram),
    ok = prim_inet:setopts(S, [{mode,list},{active,true},
			       {deliver,term},{broadcast,true}]),
    {ok,_} = prim_inet:bind(S, {0,0,0,0}, 0),
    S.

close_udp(S) ->
    prim_inet:close(S).
