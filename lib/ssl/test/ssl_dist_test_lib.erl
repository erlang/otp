%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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

-module(ssl_dist_test_lib).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("ssl_dist_test_lib.hrl").

-export([tstsrvr_format/2, send_to_tstcntrl/1]).
-export([apply_on_ssl_node/4, apply_on_ssl_node/2]).
-export([stop_ssl_node/1, start_ssl_node/2]).
%%
-export([cnct2tstsrvr/1]).

-define(AWAIT_SSL_NODE_UP_TIMEOUT, 30000).



%% ssl_node side api
%%

tstsrvr_format(Fmt, ArgList) ->
    send_to_tstsrvr({format, Fmt, ArgList}).

send_to_tstcntrl(Message) ->
    send_to_tstsrvr({message, Message}).


%%
%% test_server side api
%%

apply_on_ssl_node(
  #node_handle{connection_handler = Hndlr} = Node,
  M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Ref = erlang:monitor(process, Hndlr),
    apply_on_ssl_node(Node, Ref, {apply, self(), Ref, M, F, A}).

apply_on_ssl_node(
  #node_handle{connection_handler = Hndlr} = Node,
  Fun) when is_function(Fun, 0) ->
    Ref = erlang:monitor(process, Hndlr),
    apply_on_ssl_node(Node, Ref, {apply, self(), Ref, Fun}).

apply_on_ssl_node(Node, Ref, Msg) ->
    send_to_ssl_node(Node, Msg),
    receive
        {'DOWN', Ref, process, Hndlr, Reason} ->
            exit({handler_died, Hndlr, Reason});
	{Ref, Result} ->
	    Result
    end.

stop_ssl_node(#node_handle{connection_handler = Handler,
			   socket = Socket,
			   name = Name}) ->
    ?t:format("Trying to stop ssl node ~s.~n", [Name]),
    Mon = erlang:monitor(process, Handler),
    unlink(Handler),
    case gen_tcp:send(Socket, term_to_binary(stop)) of
	ok ->
	    receive
		{'DOWN', Mon, process, Handler, Reason} ->
		    case Reason of
			normal ->
			    ok;
			_ ->
			    ct:pal(
                              "stop_ssl_node/1 ~s Down  ~p ~n",
                              [Name,Reason])
		    end
	    end;
	Error ->
	    erlang:demonitor(Mon, [flush]),
	    ct:pal("stop_ssl_node/1 ~s Warning ~p ~n", [Name,Error])
    end.

start_ssl_node(Name, Args) ->
    {ok, LSock} = gen_tcp:listen(0,
				 [binary, {packet, 4}, {active, false}]),
    {ok, ListenPort} = inet:port(LSock),
    CmdLine = mk_node_cmdline(ListenPort, Name, Args),
    ?t:format("Attempting to start ssl node ~ts: ~ts~n", [Name, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when is_port(Port) ->
	    unlink(Port),
	    erlang:port_close(Port),
	    case await_ssl_node_up(Name, LSock) of
		#node_handle{} = NodeHandle ->
		    ?t:format("Ssl node ~s started.~n", [Name]),
		    NodeName = list_to_atom(Name ++ "@" ++ host_name()),
		    NodeHandle#node_handle{nodename = NodeName};
		Error ->
		    exit({failed_to_start_node, Name, Error})
	    end;
	Error ->
	    exit({failed_to_start_node, Name, Error})
    end.

host_name() ->
    [_, Host] = string:split(atom_to_list(node()), "@"),
    %% [$@ | Host] = lists:dropwhile(fun ($@) -> false; (_) -> true end,
    %%     			  atom_to_list(node())),
    Host.

mk_node_cmdline(ListenPort, Name, Args) ->
    Static = "-detached -noinput",
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok,[[P]]} -> P;
	       _ -> exit(no_progname_argument_found)
	   end,
    NameSw = case net_kernel:longnames() of
		 false -> "-sname ";
		 _ -> "-name "
	     end,
    {ok, Pwd} = file:get_cwd(),
    "\"" ++ Prog ++ "\" "
	++ Static ++ " "
	++ NameSw ++ " " ++ Name ++ " "
	++ "-pa " ++ Pa ++ " "
	++ "-run application start crypto -run application start public_key "
	++ "-eval 'net_kernel:verbose(1)' "
	++ "-run " ++ atom_to_list(?MODULE) ++ " cnct2tstsrvr "
	++ host_name() ++ " "
	++ integer_to_list(ListenPort) ++ " "
	++ Args ++ " "
	++ "-env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ Name ++ " "
	++ "-kernel error_logger \"{file,\\\"" ++ Pwd ++ "/error_log." ++ Name ++ "\\\"}\" "
	++ "-setcookie " ++ atom_to_list(erlang:get_cookie()).

%%
%% Connection handler test_server side
%%

await_ssl_node_up(Name, LSock) ->
    case gen_tcp:accept(LSock, ?AWAIT_SSL_NODE_UP_TIMEOUT) of
	{ok, Socket} ->
	    gen_tcp:close(LSock),
	    case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
		    check_ssl_node_up(Socket, Name, Bin);
		{error, closed} ->
		    gen_tcp:close(Socket),
		    exit({lost_connection_with_ssl_node_before_up, Name})
	    end;
	{error, Error} ->
	    gen_tcp:close(LSock),
            ?t:format("Accept failed for ssl node ~s: ~p~n", [Name,Error]),
	    exit({accept_failed, Error})
    end.

check_ssl_node_up(Socket, Name, Bin) ->
    case catch binary_to_term(Bin) of
	{'EXIT', _} ->
	    gen_tcp:close(Socket),
	    exit({bad_data_received_from_ssl_node, Name, Bin});
	{ssl_node_up, NodeName} ->
	    case list_to_atom(Name++"@"++host_name()) of
		NodeName ->
		    Parent = self(),
		    Go = make_ref(),
		    %% Spawn connection handler on test server side
		    Pid = spawn_link(
			    fun () ->
				    receive Go -> ok end,
                                    process_flag(trap_exit, true),
				    tstsrvr_con_loop(Name, Socket, Parent)
			    end),
		    ok = gen_tcp:controlling_process(Socket, Pid),
		    Pid ! Go,
		    #node_handle{connection_handler = Pid,
				 socket = Socket,
				 name = Name};
		_ ->
		    exit({unexpected_ssl_node_connected, NodeName})
	    end;
	Msg ->
	    exit({unexpected_msg_instead_of_ssl_node_up, Name, Msg})
    end.

send_to_ssl_node(#node_handle{connection_handler = Hndlr}, Term) ->
    Hndlr ! {relay_to_ssl_node, term_to_binary(Term)},
    ok.

tstsrvr_con_loop(Name, Socket, Parent) ->
    ok = inet:setopts(Socket,[{active,once}]),
    receive
	{relay_to_ssl_node, Data} when is_binary(Data) ->
	    case gen_tcp:send(Socket, Data) of
		ok ->
		    ok;
		_Error ->
		    gen_tcp:close(Socket),
		    exit({failed_to_relay_data_to_ssl_node, Name, Data})
	    end;
	{tcp, Socket, Bin} ->
	    try binary_to_term(Bin) of
		{format, FmtStr, ArgList} ->
		    ?t:format(FmtStr, ArgList);
		{message, Msg} ->
		    ?t:format("Got message ~p", [Msg]),
		    Parent ! Msg;
		{apply_res, To, Ref, Res} ->
		    To ! {Ref, Res};
		bye ->
                    {error, closed} = gen_tcp:recv(Socket, 0),
		    ?t:format("Ssl node ~s stopped.~n", [Name]),
		    gen_tcp:close(Socket),
		    exit(normal);
		Unknown ->
		    exit({unexpected_message_from_ssl_node, Name, Unknown})
            catch
                error : _ ->
		    gen_tcp:close(Socket),
		    exit({bad_data_received_from_ssl_node, Name, Bin})
	    end;
	{tcp_closed, Socket} ->
	    gen_tcp:close(Socket),
	    exit({lost_connection_with_ssl_node, Name});
        {'EXIT', Parent, Reason} ->
            exit({'EXIT', parent, Reason});
        Unknown ->
            exit({unknown, Unknown})
    end,
    tstsrvr_con_loop(Name, Socket, Parent).

%%
%% Connection handler ssl_node side
%%

% cnct2tstsrvr() is called via command line arg -run ...
cnct2tstsrvr([Host, Port]) when is_list(Host), is_list(Port) ->
    %% Spawn connection handler on ssl node side
    ConnHandler
	= spawn(fun () ->
			case catch gen_tcp:connect(Host,
						   list_to_integer(Port),
						   [binary,
						    {packet, 4},
						    {active, false}]) of
			    {ok, Socket} ->
				notify_ssl_node_up(Socket),
				ets:new(test_server_info,
					[set,
					 public,
					 named_table,
					 {keypos, 1}]),
				ets:insert(test_server_info,
					   {test_server_handler, self()}),
				ssl_node_con_loop(Socket);
			    Error ->
				halt("Failed to connect to test server " ++
					 lists:flatten(io_lib:format("Host:~p ~n Port:~p~n Error:~p~n",
								     [Host, Port, Error])))
			end
		end),
    spawn(fun () ->
		  Mon = erlang:monitor(process, ConnHandler),
		  receive
		      {'DOWN', Mon, process, ConnHandler, Reason} ->
			  receive after 1000 -> ok end,
			  halt("test server connection handler terminated: " ++
				   lists:flatten(io_lib:format("~p", [Reason])))
		  end
	  end).

notify_ssl_node_up(Socket) ->
    case catch gen_tcp:send(Socket,
			    term_to_binary({ssl_node_up, node()})) of
	ok -> ok;
	_ -> halt("Failed to notify test server that I'm up")
    end.

send_to_tstsrvr(Term) ->
    case catch ets:lookup_element(test_server_info, test_server_handler, 2) of
	Hndlr when is_pid(Hndlr) ->
	    Hndlr ! {relay_to_test_server, term_to_binary(Term)}, ok;
	_ ->
	    receive after 200 -> ok end,
	    send_to_tstsrvr(Term)
    end.

ssl_node_con_loop(Socket) ->
    inet:setopts(Socket,[{active,once}]),
    receive
	{relay_to_test_server, Data} when is_binary(Data) ->
	    case gen_tcp:send(Socket, Data) of
		ok ->
		    ok;
		_Error ->
		    gen_tcp:close(Socket),
		    halt("Failed to relay data to test server")
	    end;
	{tcp, Socket, Bin} ->
	    case catch binary_to_term(Bin) of
		{'EXIT', _} ->
		    gen_tcp:close(Socket),
		    halt("test server sent me bad data");
		{apply, From, Ref, M, F, A} ->
		    spawn_link(
		      fun () ->
			      send_to_tstsrvr({apply_res,
					       From,
					       Ref,
					       (catch apply(M, F, A))})
			  end);
		{apply, From, Ref, Fun} ->
		    spawn_link(fun () ->
				       send_to_tstsrvr({apply_res,
							From,
							Ref,
							(catch Fun())})
			       end);
		stop ->
		    gen_tcp:send(Socket, term_to_binary(bye)),
		    init:stop(),
		    receive after infinity -> ok end;
		_Unknown ->
		    halt("test server sent me an unexpected message")
	    end;
	{tcp_closed, Socket} ->
	    halt("Lost connection to test server")
    end,
    ssl_node_con_loop(Socket).
