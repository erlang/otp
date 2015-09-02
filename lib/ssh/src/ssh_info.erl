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

-compile(export_all).

print() ->
    print(user).

print(D) ->
    try supervisor:which_children(ssh_sup)
    of
	_ ->
	    io:nl(D),
	    print_general(D),
	    io:nl(D),
	    underline(D, "Client part", $=),
	    print_clients(D),
	    io:nl(D),
	    underline(D, "Server part", $=),
	    print_servers(D),
	    io:nl(D),
	    %% case os:type() of
	    %% 	{unix,_} ->
	    %% 	    io:nl(),
	    %% 	    underline("Linux part", $=),
	    %% 	    underline("Listening"),
	    %% 	    catch io:format(os:cmd("netstat -tpln")),
	    %% 	    io:nl(),
	    %% 	    underline("Other"),
	    %% 	    catch io:format(os:cmd("netstat -tpn"));
	    %% 	_ -> ok
	    %% end,
	    underline(D, "Supervisors", $=),
	    walk_sups(D, ssh_sup),
	    io:nl(D)
    catch
	_:_ ->
	    io:format(D,"Ssh not found~n",[])
    end.

%%%================================================================
print_general(D) ->
    {_Name, Slogan, Ver} = lists:keyfind(ssh,1,application:which_applications()),
    underline(D, io_lib:format("~s  ~s", [Slogan, Ver]), $=),
    io:format(D, 'This printout is generated ~s. ~n',[datetime()]).

%%%================================================================
print_clients(D) ->
    PrintClient = fun(X) -> print_client(D,X) end,
    try
	lists:foreach(PrintClient, supervisor:which_children(sshc_sup))
    catch
	C:E ->
	    io:format(D, '***FAILED: ~p:~p~n',[C,E])
    end.

print_client(D, {undefined,Pid,supervisor,[ssh_connection_handler]}) ->
    {{Local,Remote},_Str} = ssh_connection_handler:get_print_info(Pid),
    io:format(D, "    Local=~s  Remote=~s  ConnectionRef=~p~n",[fmt_host_port(Local),fmt_host_port(Remote),Pid]);
print_client(D, Other) ->
    io:format(D, "    [[Other 1: ~p]]~n",[Other]).


%%%================================================================
print_servers(D) ->
    PrintServer = fun(X) -> print_server(D,X) end,
    try
	lists:foreach(PrintServer, supervisor:which_children(sshd_sup))
    catch
	C:E ->
	    io:format(D, '***FAILED: ~p:~p~n',[C,E])
    end.

print_server(D, {{server,ssh_system_sup,LocalHost,LocalPort},Pid,supervisor,[ssh_system_sup]}) when is_pid(Pid) ->
    io:format(D, 'Local=~s (~p children)~n',[fmt_host_port({LocalHost,LocalPort}),
					     ssh_acceptor:number_of_connections(Pid)]),
    PrintSystemSup = fun(X) -> print_system_sup(D,X) end,
    lists:foreach(PrintSystemSup, supervisor:which_children(Pid));
print_server(D, Other) ->
    io:format(D, "    [[Other 2: ~p]]~n",[Other]).
    
print_system_sup(D, {Ref,Pid,supervisor,[ssh_subsystem_sup]}) when is_reference(Ref),
								is_pid(Pid) ->
    PrintChannels =  fun(X) -> print_channels(D,X) end,
    lists:foreach(PrintChannels, supervisor:which_children(Pid));
print_system_sup(D, {{ssh_acceptor_sup,LocalHost,LocalPort}, Pid,supervisor, [ssh_acceptor_sup]}) when is_pid(Pid) ->
    io:format(D, "    [Acceptor for ~s]~n",[fmt_host_port({LocalHost,LocalPort})]);
print_system_sup(D, Other) -> 
    io:format(D, "    [[Other 3: ~p]]~n",[Other]).

print_channels(D, {{server,ssh_channel_sup,_,_},Pid,supervisor,[ssh_channel_sup]}) when is_pid(Pid) ->
    PrintChannel =  fun(X) -> print_channel(D,X) end,
    lists:foreach(PrintChannel, supervisor:which_children(Pid));
print_channels(D, Other) -> 
    io:format(D, "    [[Other 4: ~p]]~n",[Other]).


print_channel(D, {Ref,Pid,worker,[ssh_channel]}) when is_reference(Ref), 
						      is_pid(Pid)  ->
    {{ConnManager,ChannelID}, Str} = ssh_channel:get_print_info(Pid),
    {{Local,Remote},StrM} = ssh_connection_handler:get_print_info(ConnManager),
    io:format(D, '    ch ~p: ~s ~s',[ChannelID, StrM, Str]),
    io:format(D, "    Local=~s Remote=~s~n",[fmt_host_port(Local),fmt_host_port(Remote)]);
print_channel(D, Other) -> 
    io:format(D, "    [[Other 5: ~p]]~n",[Other]).
	      
%%%================================================================
-define(inc(N), (N+4)).

walk_sups(D, StartPid) ->
    io:format(D, "Start at ~p, ~s.~n",[StartPid,dead_or_alive(StartPid)]),
    walk_sups(D, children(StartPid), _Indent=?inc(0)).

walk_sups(D, [H={_,Pid,_,_}|T], Indent) ->
    indent(D, Indent), io:format(D, '~200p  ~p is ~s~n',[H,Pid,dead_or_alive(Pid)]),
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

indent(D, I) -> io:format(D,'~*c',[I,$ ]). 

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
underline(D, Str) ->
    underline(D, Str, $-).

underline(D, Str, LineChar) ->
    Len = lists:flatlength(Str),
    io:format(D, '~s~n',[Str]),
    line(D,Len,LineChar).

line(D, Len, Char) ->
    io:format(D, '~*c~n', [Len,Char]).
	    

datetime() ->
    {{YYYY,MM,DD}, {H,M,S}} = calendar:now_to_universal_time(erlang:timestamp()),
    lists:flatten(io_lib:format('~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC',[YYYY,MM,DD, H,M,S])).


fmt_host_port({{A,B,C,D},Port}) -> io_lib:format('~p.~p.~p.~p:~p',[A,B,C,D,Port]);
fmt_host_port({Host,Port}) -> io_lib:format('~s:~p',[Host,Port]).



nyi(D) ->
    io:format(D,'Not yet implemented~n',[]),
    nyi.
