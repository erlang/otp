%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2014. All Rights Reserved.
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
%% Purpose: Print some info of a running ssh aplication.
%%----------------------------------------------------------------------

-module(ssh_info).

-compile(export_all).

print() ->
    try supervisor:which_children(ssh_sup)
    of
	_ ->
	    io:nl(),
	    print_general(),
	    io:nl(),
	    underline("Client part", $=),
	    print_clients(),
	    io:nl(),
	    underline("Server part", $=),
	    print_servers(),
	    io:nl(),
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
	    underline("Supervisors", $=),
	    walk_sups(ssh_sup),
	    io:nl()
    catch
	_:_ ->
	    io:format("Ssh not found~n",[])
    end.

%%%================================================================
print_general() ->
    {_Name, Slogan, Ver} = lists:keyfind(ssh,1,application:which_applications()),
    underline(io_lib:format("~s  ~s", [Slogan, Ver]), $=),
    io:format('This printout is generated ~s. ~n',[datetime()]).

%%%================================================================
print_clients() ->
    try
	lists:foreach(fun print_client/1, supervisor:which_children(sshc_sup))
    catch
	C:E ->
	    io:format('***FAILED: ~p:~p~n',[C,E])
    end.

print_client({undefined,Pid,supervisor,[ssh_connection_handler]}) ->
    {{Local,Remote},_Str} = ssh_connection_handler:get_print_info(Pid),
    io:format("    Local=~s Remote=~s~n",[fmt_host_port(Local),fmt_host_port(Remote)]);
print_client(Other) ->
    io:format("    [[Other 1: ~p]]~n",[Other]).


%%%================================================================
print_servers() ->
    try
	lists:foreach(fun print_server/1, supervisor:which_children(sshd_sup))
    catch
	C:E ->
	    io:format('***FAILED: ~p:~p~n',[C,E])
    end.

print_server({{server,ssh_system_sup,LocalHost,LocalPort},Pid,supervisor,[ssh_system_sup]}) when is_pid(Pid) ->
    io:format('Local=~s (~p children)~n',[fmt_host_port({LocalHost,LocalPort}),
					  ssh_acceptor:number_of_connections(Pid)]),
    lists:foreach(fun print_system_sup/1, supervisor:which_children(Pid));
print_server(Other) ->
    io:format("    [[Other 2: ~p]]~n",[Other]).
    
print_system_sup({Ref,Pid,supervisor,[ssh_subsystem_sup]}) when is_reference(Ref),
								is_pid(Pid) ->
    lists:foreach(fun print_channels/1, supervisor:which_children(Pid));
print_system_sup({{ssh_acceptor_sup,LocalHost,LocalPort}, Pid,supervisor, [ssh_acceptor_sup]}) when is_pid(Pid) ->
    io:format("    [Acceptor for ~s]~n",[fmt_host_port({LocalHost,LocalPort})]);
print_system_sup(Other) -> 
    io:format("    [[Other 3: ~p]]~n",[Other]).

print_channels({{server,ssh_channel_sup,_,_},Pid,supervisor,[ssh_channel_sup]}) when is_pid(Pid) ->
    lists:foreach(fun print_channel/1, supervisor:which_children(Pid));
print_channels(Other) -> 
    io:format("    [[Other 4: ~p]]~n",[Other]).


print_channel({Ref,Pid,worker,[ssh_channel]}) when is_reference(Ref), 
						   is_pid(Pid)  ->
    {{ConnManager,ChannelID}, Str} = ssh_channel:get_print_info(Pid),
    {{Local,Remote},StrM} = ssh_connection_handler:get_print_info(ConnManager),
    io:format('    ch ~p: ~s ~s',[ChannelID, StrM, Str]),
    io:format("    Local=~s Remote=~s~n",[fmt_host_port(Local),fmt_host_port(Remote)]);
print_channel(Other) -> 
    io:format("    [[Other 5: ~p]]~n",[Other]).
	      
%%%================================================================
-define(inc(N), (N+4)).

walk_sups(StartPid) ->
    io:format("Start at ~p, ~s.~n",[StartPid,dead_or_alive(StartPid)]),
    walk_sups(children(StartPid), _Indent=?inc(0)).

walk_sups([H={_,Pid,SupOrWorker,_}|T], Indent) ->
    indent(Indent), io:format('~200p  ~p is ~s~n',[H,Pid,dead_or_alive(Pid)]),
    case SupOrWorker of
	supervisor -> walk_sups(children(Pid), ?inc(Indent));
	_ -> ok
    end,
    walk_sups(T, Indent);
walk_sups([], _) ->
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

indent(I) -> io:format('~*c',[I,$ ]). 

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
underline(Str) ->
    underline(Str, $-).

underline(Str, LineChar) ->
    Len = lists:flatlength(Str),
    io:format('~s~n',[Str]),
    line(Len,LineChar).

line(Len, Char) ->
    io:format('~*c~n', [Len,Char]).
	    

datetime() ->
    {{YYYY,MM,DD}, {H,M,S}} = calendar:now_to_universal_time(now()),
    lists:flatten(io_lib:format('~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC',[YYYY,MM,DD, H,M,S])).


fmt_host_port({{A,B,C,D},Port}) -> io_lib:format('~p.~p.~p.~p:~p',[A,B,C,D,Port]);
fmt_host_port({Host,Port}) -> io_lib:format('~s:~p',[Host,Port]).



nyi() ->
    io:format('Not yet implemented~n',[]),
    nyi.
