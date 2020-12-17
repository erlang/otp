%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
%% Purpose: Print some info of a running ssh aplication.
%%----------------------------------------------------------------------

-module(ssh_info).

-export([print/0,
	 print/1,
	 string/0,
	 collect_pids/0
	]).

-include("ssh_connect.hrl").

print() ->
    print(fun io:format/2).

print(F) when is_function(F,2) ->
    F("~s", [string()]);
print(File) when is_list(File) ->
    {ok,D} = file:open(File, [write]),
    print(D),
    file:close(D);
print(D) ->
    io:format(D, "~s", [string()]).

string() ->
    try supervisor:which_children(ssh_sup)
    of
	_ ->
	    [io_lib:nl(),
	     print_general(),
	     io_lib:nl(),
	     underline("Client part", $=),
             lists:map(fun print_system/1, children(sshc_sup)),
	     io_lib:nl(),
	     underline("Server part", $=),
             lists:map(fun print_system/1, children(sshd_sup)),
	     io_lib:nl(),
	     underline("Supervisors", $=),
	     walk_sups(ssh_sup),
	     io_lib:nl()]
    catch
	_:_ ->
	    io_lib:format("Ssh not found~n",[])
    end.


%%%================================================================
-define(inc(N), (N+4)).

-define(INDENT, "    ").

print_general() ->
    {_Name, Slogan, Ver} = lists:keyfind(ssh,1,application:which_applications()),
    [underline(io_lib:format("~s  ~s", [Slogan, Ver]), $=),
     io_lib:format('This printout is generated ~s. ~n',[datetime()])
    ].


print_system({{server,ssh_system_sup,Addr,Port,Profile}, Pid, supervisor, [ssh_system_sup]}) ->
    [io_lib:format(?INDENT"Local: ~s (~p children) Profile ~p~n",
                   [fmt_host_port({Addr,Port}),
                    ssh_acceptor:number_of_connections(Pid),
                    Profile
                   ]),
     lists:map(fun print_subsystem/1, children(Pid))
    ];
print_system({{client,ssh_system_sup,Addr,Port,Profile}, Pid, supervisor, [ssh_system_sup]}) ->
    [io_lib:format(?INDENT"Local: ~s Profile ~p~n",
                   [fmt_host_port({Addr,Port}),
                    Profile
                   ]),
     lists:map(fun print_subsystem/1, children(Pid))
    ];
print_system({_, _Pid, worker, [ssh_controller]}) ->
    ""; % io_lib:format(?INDENT"Controller~n",[]);
print_system(_X) ->
    io_lib:format(?INDENT"nyi system ~p~n",[_X]).




print_subsystem({{ssh_acceptor_sup,_Addr,_Port,_Profile}, _Pid, supervisor, [ssh_acceptor_sup]}) ->
    io_lib:format(?INDENT?INDENT"Acceptor~n",[]);
print_subsystem({Ref,Pid,supervisor,[ssh_subsystem_sup]}) when is_reference(Ref),
                                                               is_pid(Pid) ->
    Cs = children(Pid),
    [
     lists:map(
       fun(Sup) ->
                [print_conn(P) || {undefined,P,worker,[ssh_connection_handler]} <- children(Sup)]
       end,
       [P ||  {_Ref,P,supervisor,[ssh_connection_sup]} <- Cs]),

     lists:map(
       fun(Sup) ->
                [print_ch(M,P) || {_Ref,P,worker,[M]} <- children(Sup),
                                lists:member(M, [ssh_channel,
                                                 ssh_channel_sup,
                                                 ssh_client_channel,
                                                 ssh_daemon_channel,
                                                 ssh_server_channel
                                                ])]
       end,
       [P ||  {_Ref,P,supervisor,[ssh_channel_sup]} <- Cs]),

     lists:map(
       fun(Sup) ->
               [io_lib:format(?INDENT?INDENT?INDENT"TCP/IP fwd acceptor: ~p~n", [Pa]) 
                || {undefined,Pa,worker,[ssh_tcpip_forward_acceptor]} <- children(Sup)]
       end,
       [P ||  {_Ref,P,supervisor,[ssh_tcpip_forward_acceptor_sup]} <- Cs])
    ];

print_subsystem(_X) ->
    io_lib:format(?INDENT?INDENT"nyi subsystem ~p~n",[_X]).


print_conn(Pid) ->
    try
	{{_Local,Remote},StrM} = ssh_connection_handler:get_print_info(Pid),
        io_lib:format(?INDENT?INDENT"Remote: ~s ConnectionRef = ~p ~s~n",[fmt_host_port(Remote),Pid,StrM])
    catch
      	C:E ->
	    io_lib:format('****print_conn FAILED for ConnPid ~p: ~p:~p~n',[Pid, C, E])
    end.  
        

print_ch(CBmod, Pid) ->
    try
	{{_ConnManager,ChannelID}, Str} = ssh_server_channel:get_print_info(Pid),
	io_lib:format(?INDENT?INDENT?INDENT"ch ~p ~p ~p: ~s~n",[ChannelID, Pid, CBmod, Str])
    catch
	C:E ->
	    io_lib:format('****print_ch FAILED for ChanPid ~p: ~p:~p~n',[Pid, C, E])
    end.

%%%================================================================
walk_sups(StartPid) ->
    walk_sups(children(StartPid), _Indent=?inc(0)).

walk_sups([H={_,Pid,_,_}|T], Indent) ->
    [indent(Indent),
     io_lib:format('~200p  ~p is ~s~n',[H,Pid,dead_or_alive(Pid)]),
     case H of
	 {_,_,supervisor,[ssh_connection_handler]} -> "";
	 {_,Pid,supervisor,_} -> walk_sups(children(Pid), ?inc(Indent));
	 _ -> ""
     end,
     walk_sups(T, Indent)
    ];
walk_sups([], _) ->
    "".

dead_or_alive(Name) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    "**UNDEFINED**";
	Pid ->
	    dead_or_alive(Pid)
    end;
dead_or_alive(Pid) when is_pid(Pid) ->
    case process_info(Pid) of
	undefined -> "**DEAD**";
	_ -> "alive"
    end.

indent(I) -> io_lib:format('~*c',[I,$ ]).


children(Pid) ->
    Parent = self(),
    Helper = spawn(fun() ->
			   Parent ! {self(),supervisor:which_children(Pid)}
		   end),
    receive
	{Helper,L} when is_list(L) ->
	    L
    after
	2000 ->
	    catch exit(Helper, kill),
	    []
    end.

is_connection_handler(Pid) ->
    try
	{ssh_connection_handler,init,_} =
	    proplists:get_value(
	      '$initial_call',
	      proplists:get_value(
		dictionary,
		process_info(Pid, [dictionary])))
    of
	_ -> true

    catch
	_:_ ->
	    false
    end.

channels(Pid) ->
    case is_connection_handler(Pid) of
	true ->
	    ssh_connection_handler:info(Pid,all);
	false ->
	    false
    end.

%%%================================================================
underline(Str, LineChar) ->
    io_lib:format('~s~n~*c~n',[Str, lists:flatlength(Str), LineChar]).


datetime() ->
    {{YYYY,MM,DD}, {H,M,S}} = calendar:now_to_universal_time(erlang:timestamp()),
    lists:flatten(io_lib:format('~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC',[YYYY,MM,DD, H,M,S])).


fmt_host_port({{A,B,C,D},Port}) -> io_lib:format('~p.~p.~p.~p:~p',[A,B,C,D,Port]);
fmt_host_port({Host,Port}) -> io_lib:format('~s:~p',[Host,Port]).

%%%################################################################
collect_pids() -> collect_pids(ssh_sup).

collect_pids(P) ->
    Collector = pcollect_pids(P, spawn(fun init_collector/0)),
    Collector ! {get_values,self()},
    receive
	{values,Values} ->
	    Values
    end.

%%%----------------
pcollect_pids(undefined, Collector) ->
    Collector;

pcollect_pids(A, Collector) when is_atom(A) ->
    pcollect_pids(whereis(A), Collector);

pcollect_pids(Pid, Collector) when is_pid(Pid) ->
    Collector ! {expect,Pid},
    spawn(fun() ->
		  lists:foreach(
		    fun(P2) ->
			    pcollect_pids(P2,Collector)
		    end, children(Pid)),
		  Collector ! {value,Pid,Pid}
	  end),
    Collector;

pcollect_pids({Ref,Pid,supervisor,_}, Collector) when is_pid(Pid),
						      is_reference(Ref) ->
    pcollect_pids(Pid, Collector);

pcollect_pids({sshc_sup,Pid,supervisor,_}, Collector) when is_pid(Pid) ->
    pcollect_pids(Pid, Collector);

pcollect_pids({sshd_sup,Pid,supervisor,_}, Collector) when is_pid(Pid) ->
    pcollect_pids(Pid, Collector);

pcollect_pids({{ssh_acceptor_sup,_,_,_},Pid,supervisor,_}, Collector) when is_pid(Pid) ->
    pcollect_pids(Pid, Collector);

pcollect_pids({{server,_,_,_},Pid,supervisor,_}, Collector) when is_pid(Pid) ->
    pcollect_pids(Pid, Collector);

pcollect_pids({{server,_,_,_,_},Pid,supervisor,_}, Collector) when is_pid(Pid) ->
    pcollect_pids(Pid, Collector);

pcollect_pids({undefined,Pid,supervisor,[ssh_connection_handler]}, Collector) ->
    Collector ! {value,Pid,Pid},
    case channels(Pid) of
	{ok,L} ->
	    [Collector!{value,P,P} || #channel{user=P} <- L];
	_ ->
	    ok
    end,
    Collector;

pcollect_pids({_,Pid,_,_}, Collector) when is_pid(Pid) ->
    Collector ! {value,Pid,Pid},
    Collector;

pcollect_pids(_, Collector) ->
    Collector.

%%%----------------
init_collector() ->
    loop_collector([],[]).

loop_collector(Expects, Values) ->
    receive
	{expect, Ref} ->
	    loop_collector([Ref|Expects], Values);
	{value, Ref, Val} ->
	    loop_collector(Expects--[Ref], [Val|Values]);
	{get_values, From} when Expects==[] ->
%%				Values=/=[] ->
	    From ! {values,Values}
    end.
