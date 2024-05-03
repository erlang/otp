%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
%% Purpose: Print some info of a running ssh application.
%%----------------------------------------------------------------------

-module(ssh_info).
-moduledoc false.

-export([print/0,
	 print/1,
	 string/0
	]).

-include("ssh.hrl").
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
	     underline("Client(s)", $-),
	     print_sups(client, sshc_sup),
	     io_lib:nl(),
	     underline("Daemon(s)", $-),
	     print_sups(server, sshd_sup),
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

print_sups(Role, StartPid) ->
    walk_tree(Role, get_subs_tree(StartPid)).

%%%================================================================
get_subs_tree(StartPid) ->
    lists:foldl(fun({Id,_,worker,_}=C, Acc) -> [{C,chspec(StartPid,Id)}|Acc];
                   ({Id,Pid,supervisor,_}=C, Acc) -> [{C,chspec(StartPid,Id),get_subs_tree(Pid)}|Acc]
                end, [], children(StartPid)).

chspec(Sup, Id) ->
    try supervisor:get_childspec(Sup, Id)
    of
        {ok,Spec} -> Spec;
        {error,_} -> undefined
    catch _:_ -> undefined
    end.
    
%%%----------------------------------------------------------------
walk_tree(Role, Tree) ->
    walk_tree(Role, Tree, _Indent=?inc(0)).
 
walk_tree(Role, [{{_,_,supervisor,_},_,_}=H|T], Indent) ->
    [io_lib:format('~s',[format_sup(Role,H,Indent)]) |
     walk_tree(Role, T, Indent)
    ];
walk_tree(Role, [{{_,_,worker,_},_}=H|T], Indent) ->
    [io_lib:format('~s',[format_wrk(Role,H,Indent)]) |
     walk_tree(Role, T, Indent)
    ];
walk_tree(_Role, [], _) ->
    "".


format_sup(server,
           {{{ssh_system_sup,LocalAddress},Pid,supervisor,[ssh_system_sup]}, _Spec,
            [{{{ssh_acceptor_sup,Address},AccSupPid,supervisor,[ssh_acceptor_sup]}, _AccSupSpec,
              [{{{ssh_acceptor_sup,Address},AccPid,worker,[ssh_acceptor]}, _AccSpec}]}
             | Children]
           }, Indent) ->
   [indent(Indent),
    io_lib:format("Local listen: ~s, Daemon_ref = ~s~n"
                  "~ssys_sup=~s, acc_sup=~s, acc=~s~n",
                  [format_address(LocalAddress),print_pid(Pid),
                   indent(Indent),print_pid(Pid),print_pid(AccSupPid),print_pid(AccPid)]),
    walk_tree(server, Children, ?inc(Indent)),
    io_lib:nl() % Separate system supervisors by an empty line
   ];
format_sup(server, {{{ssh_system_sup,LocalAddress},Pid,supervisor,[ssh_system_sup]}, _Spec, Children}, Indent) ->
    [indent(Indent),
     io_lib:format("Local listen: none (was: ~s), Daemon_ref = ~s~n"
                   "~ssys_sup=~s~n",
                   [format_address(LocalAddress),print_pid(Pid),
                    indent(Indent),print_pid(Pid)]),
     walk_tree(server, Children, ?inc(Indent)),
     io_lib:nl() % Separate system supervisors by an empty line
    ];
format_sup(client, {{{ssh_system_sup,LocalAddress},Pid,supervisor,[ssh_system_sup]}, _Spec, Children}, Indent) ->
    [indent(Indent),
     io_lib:format("Local:  ~s sys_sup=~s~n", [format_address(LocalAddress), print_pid(Pid)]),
     walk_tree(client, Children, ?inc(Indent)),
     io_lib:nl() % Separate system supervisors by an empty line
    ];
format_sup(Role,
           {{Ref,SubSysSup,supervisor,[ssh_subsystem_sup]}, _SubSysSpec,
            [{{connection,ConnPid,worker,[ssh_connection_handler]}, _ConnSpec} 
             | Children]
           },
           Indent) when is_reference(Ref) ->
    [io_lib:format("~sRemote: ~s (Version: ~s)~n"
                   "~sConnectionRef=~s, subsys_sup=~s~n",
                   [indent(Indent), peer_addr(ConnPid), peer_version(Role,ConnPid),
                    indent(Indent), print_pid(ConnPid), print_pid(SubSysSup)
                   ]),
     walk_tree(Role,
               [{H,{connref,ConnPid},Cs} || {H,_,Cs} <- Children],
               ?inc(Indent)),
     io_lib:nl() % Separate sub system supervisors by an empty line
    ];
format_sup(Role, {{channel_sup,Pid,supervisor,[ssh_channel_sup]}, {connref,ConnPid}, Children}, Indent) ->
    [indent(Indent),
     case Children of
         [] ->
             io_lib:format("No open channels (chan_sup=~s).~n",[print_pid(Pid)]);
         _ ->
             Cinfo = try
                         {ok,L} = ssh_connection_handler:info(ConnPid),
                         L
                     catch
                         _:_ -> []
                     end,
             [io_lib:format("Open channels (chan_sup=~s):~n",[print_pid(Pid)]),
              walk_tree(Role,
                        [{ChH, lists:keyfind(ChPid,#channel.user,Cinfo)} || {ChH={_,ChPid,_,_},_} <- Children],
                        ?inc(Indent))
             ]
     end
    ];
format_sup(Role, {{tcpip_forward_acceptor_sup,Pid,supervisor,[ssh_tcpip_forward_acceptor_sup]}, {connref,_ConnPid}, Children}, Indent) ->
    [indent(Indent),
     case Children of
         [] ->
             io_lib:format("TCP/IP forwarding not started (fwd_sup=~s)~n", [print_pid(Pid)]);
         _ ->
             [io_lib:format("TCP/IP forwarding (fwd_sup=~s):~n", [print_pid(Pid)]),
              walk_tree(Role, Children, ?inc(Indent))
             ]
     end
    ];
format_sup(Role, {H, Spec, Children}, Indent) ->
    [indent(Indent),
     io_lib:format("?: ~200p ~s ~n", [H,print_spec(Spec)]),
     walk_tree(Role, Children, ?inc(Indent))
    ].


format_wrk(_Role, {{{ssh_acceptor_sup,Address},Pid,worker,[ssh_acceptor]}, _Spec}, Indent) ->
    [indent(Indent),
     io_lib:format("acceptor: ~s ~s~n", [format_address(Address),print_pid(Pid)])
    ];
format_wrk(_Role, {{{From,To},Pid,worker,[ssh_tcpip_forward_acceptor]}, _Spec}, Indent) ->
    io_lib:format("~sssh_tcpip_forward_acceptor ~s From: ~s, To: ~s~n", [indent(Indent),print_pid(Pid),
                                                                         format_address(From), format_address(To)]);
format_wrk(_Role, {{Ref,Pid,worker,[Cb]}, C}, Indent) when is_reference(Ref) ->
    Str =
        try io_lib:format("~p: (remote ~p)~s~s", [C#channel.local_id, C#channel.remote_id,
                                          if_true(C#channel.sent_close, " sent_close"),
                                          if_true(C#channel.recv_close, " recv_close")
                                         ])
        catch
            _:_ -> "?:"
        end,
    ChCb = try
               case Cb of
                   ssh_server_channel -> io_lib:format(" ~s", [Cb:get_print_info(Pid, channel_cb)]);
                   ssh_client_channel -> io_lib:format(" ~s", [Cb:get_print_info(Pid, channel_cb)]);
                   _ -> ""
               end
           catch _:_ -> ""
           end,
    [indent(Indent),
     io_lib:format("ch ~s ~p~s ~s~n", [Str, Cb, ChCb, print_pid(Pid)])
    ];
format_wrk(_Role, {H,Spec}, Indent) ->
    [indent(Indent),
     io_lib:format("?: ~200p ~s~n", [H,print_spec(Spec)])
    ].



if_true(true, Str) -> Str;
if_true(_, _) -> "".
    

peer_version(Role, Pid) ->
    try
        Key =
            case Role of
                client -> server_version;
                server -> client_version
            end,
        [{Key, {{_,_},V}}] =
             ssh_connection_handler:connection_info(Pid, [Key]),
        V
    catch
        _:_ -> "?"
    end.

peer_addr(Pid) ->
    try
        [{peer,{_,AddrPort}}] =
            ssh_connection_handler:connection_info(Pid, [peer]),
        ssh_lib:format_address_port(AddrPort)
    catch
        _:_ -> "?"
    end.
    

format_address(#address{address=Addr, port=Port, profile=Prof}) ->
    io_lib:format("~s (profile ~p)", [ssh_lib:format_address_port({Addr,Port}),Prof]);
format_address(A) ->
    io_lib:format("~p",[A]).


print_pid(Pid) ->
    io_lib:format("~p~s",[Pid, dead_or_alive(Pid)]).

dead_or_alive(Name) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    " **UNDEFINED**";
	Pid ->
	    dead_or_alive(Pid)
    end;
dead_or_alive(Pid) when is_pid(Pid) ->
    case process_info(Pid,message_queue_len) of
	undefined -> " ***DEAD***";
        {message_queue_len,N} when N>10 -> io_lib:format(" ***msg_queue_len: ~p***",[N]);
        {message_queue_len,N} when N>0 -> io_lib:format(" (msg_queue_len: ~p)",[N]);
	_ -> ""
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

%%%================================================================
print_spec(_Spec) -> "".

%%%================================================================
underline(Str, LineChar) ->
    io_lib:format('~s~n~*c~n',[Str, lists:flatlength(Str), LineChar]).


datetime() ->
    {{YYYY,MM,DD}, {H,M,S}} = calendar:now_to_universal_time(erlang:timestamp()),
    lists:flatten(io_lib:format('~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC',[YYYY,MM,DD, H,M,S])).


%%%================================================================

%% ?wr_record(address);
%% wr_record(R) -> io_lib:format('~p~n',[R]).
