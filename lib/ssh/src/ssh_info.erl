%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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
	 string/0
	]).

print() ->
    print(user).

print(D) ->
    try supervisor:which_children(ssh_sup)
    of
	_ ->
	    io__nl(D),
	    print_general(D),
	    io__nl(D),
	    underline(D, "Client part", $=),
	    print_clients(D),
	    io__nl(D),
	    underline(D, "Server part", $=),
	    print_servers(D),
	    io__nl(D),
	    underline(D, "Supervisors", $=),
	    walk_sups(D, ssh_sup),
	    io__nl(D)
    catch
	_:_ ->
	    io__format(D,"Ssh not found~n",[])
    end.

string() ->
    Pid = spawn(fun init/0),
    print(Pid),
    Pid ! {get,self()},
    receive
	{result,R} -> R
    end.

%%%================================================================
print_general(D) ->
    {_Name, Slogan, Ver} = lists:keyfind(ssh,1,application:which_applications()),
    underline(D, io_lib:format("~s  ~s", [Slogan, Ver]), $=),
    io__format(D, 'This printout is generated ~s. ~n',[datetime()]).

%%%================================================================
-define(INDENT, "    ").

print_clients(D) ->
    PrintClient = fun(X) -> print_client(D,X) end,
    try
	lists:foreach(PrintClient, supervisor:which_children(sshc_sup))
    catch
	C:E ->
	    io__format(D, '***FAILED: ~p:~p~n',[C,E])
    end.

print_client(D, {undefined,Pid,supervisor,[ssh_connection_handler]}) ->
    {{Local,Remote},_Str} = ssh_connection_handler:get_print_info(Pid),
    io__format(D, ?INDENT"Local: ~s  Remote: ~s  ConnectionRef = ~p~n",[fmt_host_port(Local),fmt_host_port(Remote),Pid]);
print_client(D, Other) ->
    io__format(D, "    [[Other 1: ~p]]~n",[Other]).


%%%================================================================
print_servers(D) ->
    PrintServer = fun(X) -> print_server(D,X) end,
    try
	lists:foreach(PrintServer, supervisor:which_children(sshd_sup))
    catch
	C:E ->
	    io__format(D, '***FAILED: ~p:~p~n',[C,E])
    end.


print_server(D, {{server,ssh_system_sup,LocalHost,LocalPort,Profile},Pid,supervisor,[ssh_system_sup]}) when is_pid(Pid) ->
    io__format(D, ?INDENT"Listen: ~s (~p children) Profile ~p~n",[fmt_host_port({LocalHost,LocalPort}),
								  ssh_acceptor:number_of_connections(Pid),
								  Profile]),
    PrintSystemSup = fun(X) -> print_system_sup(D,X) end,
    lists:foreach(PrintSystemSup, supervisor:which_children(Pid)).


print_system_sup(D, {Ref,Pid,supervisor,[ssh_subsystem_sup]}) when is_reference(Ref),
								   is_pid(Pid) ->
    PrintChannels =  fun(X) -> print_channels(D,X) end,
    lists:foreach(PrintChannels, supervisor:which_children(Pid));
print_system_sup(D, {{ssh_acceptor_sup,_LocalHost,_LocalPort,_Profile}, Pid, supervisor, [ssh_acceptor_sup]}) when is_pid(Pid) ->
    io__format(D, ?INDENT?INDENT"[Acceptor Pid ~p]~n",[Pid]).


print_channels(D, {{server,ssh_channel_sup,_,_},Pid,supervisor,[ssh_channel_sup]}) when is_pid(Pid) ->
    Children =  supervisor:which_children(Pid),
    ChannelPids = [P || {R,P,worker,[ssh_channel]} <- Children,
			is_pid(P),
			is_reference(R)],
    case ChannelPids of
	[] -> io__format(D, ?INDENT?INDENT"No channels~n",[]);
	[Ch1Pid|_] ->
	    {{ConnManager,_}, _Str} = ssh_channel:get_print_info(Ch1Pid),
	    {{_,Remote},_} = ssh_connection_handler:get_print_info(ConnManager),
	    io__format(D, ?INDENT?INDENT"Remote: ~s ConnectionRef = ~p~n",[fmt_host_port(Remote),ConnManager]),
	    lists:foreach(fun(P) -> print_ch(D,P) end, ChannelPids)
    end;
print_channels(_D, {{server,ssh_connection_sup,_,_},Pid,supervisor,[ssh_connection_sup]}) when is_pid(Pid) ->
    ok. % The supervisor of the connections socket owning process

print_ch(D, Pid) -> 
    {{ConnManager,ChannelID}, Str} = ssh_channel:get_print_info(Pid),
    {_LocalRemote,StrM} = ssh_connection_handler:get_print_info(ConnManager),
    io__format(D, ?INDENT?INDENT?INDENT"ch ~p: ~s ~s~n",[ChannelID, StrM, Str]).
    
%%%================================================================
-define(inc(N), (N+4)).

walk_sups(D, StartPid) ->
    io__format(D, "Start at ~p, ~s.~n",[StartPid,dead_or_alive(StartPid)]),
    walk_sups(D, children(StartPid), _Indent=?inc(0)).

walk_sups(D, [H={_,Pid,_,_}|T], Indent) ->
    indent(D, Indent), io__format(D, '~200p  ~p is ~s~n',[H,Pid,dead_or_alive(Pid)]),
    case H of
	{_,_,supervisor,[ssh_connection_handler]} -> ok;
	{_,Pid,supervisor,_} -> walk_sups(D, children(Pid), ?inc(Indent));
	_ -> ok
    end,
    walk_sups(D, T, Indent);
walk_sups(_D, [], _) ->
    ok.

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

indent(D, I) -> io__format(D,'~*c',[I,$ ]). 

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

%%%================================================================
underline(D, Str, LineChar) ->
    Len = lists:flatlength(Str),
    io__format(D, '~s~n',[Str]),
    line(D,Len,LineChar).

line(D, Len, Char) ->
    io__format(D, '~*c~n', [Len,Char]).
	    

datetime() ->
    {{YYYY,MM,DD}, {H,M,S}} = calendar:now_to_universal_time(erlang:timestamp()),
    lists:flatten(io_lib:format('~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC',[YYYY,MM,DD, H,M,S])).


fmt_host_port({{A,B,C,D},Port}) -> io_lib:format('~p.~p.~p.~p:~p',[A,B,C,D,Port]);
fmt_host_port({Host,Port}) -> io_lib:format('~s:~p',[Host,Port]).

%%%################################################################

io__nl(D) when is_atom(D) -> io:nl(D);
io__nl(P) when is_pid(P) -> P ! {string,io_lib:nl()}.

io__format(D, Fmt, Args) when is_atom(D) -> io:format(D, Fmt, Args);
io__format(P, Fmt, Args) when is_pid(P) -> P ! {string,io_lib:format(Fmt, Args)}.


init() -> loop([]).

loop(Acc) ->
    receive
	{string,Str} ->
	    loop([Str|Acc]);
	{get,Who} ->
	    Who ! {result,lists:flatten(lists:reverse(Acc))}
    end.
	
