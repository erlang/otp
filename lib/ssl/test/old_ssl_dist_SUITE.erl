%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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


%%%-------------------------------------------------------------------
%%% File    : ssl_dist_SUITE.erl
%%% Author  : Rickard Green
%%% Description : Test that the Erlang distribution works over ssl.
%%%
%%% Created : 15 Nov 2007 by Rickard Green
%%%-------------------------------------------------------------------
-module(old_ssl_dist_SUITE).

-include("test_server.hrl").

-define(DEFAULT_TIMETRAP_SECS, 240).

-define(AWAIT_SLL_NODE_UP_TIMEOUT, 30000).

-export([all/1]).
-export([init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 fin_per_testcase/2]).
-export([cnct2tstsrvr/1]).

-export([basic/1]).

-record(node_handle, {connection_handler, socket, name, nodename}).

all(doc) ->
    [];
all(suite) ->
    [basic].

init_per_suite(Config) ->
    add_ssl_opts_config(Config).

end_per_suite(Config) ->
    Config.

init_per_testcase(Case, Config) when list(Config) ->
    Dog = ?t:timetrap(?t:seconds(?DEFAULT_TIMETRAP_SECS)),
    [{watchdog, Dog},{testcase, Case}|Config].

fin_per_testcase(_Case, Config) when list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% Testcases                                                             %%
%%                                                                       %%

basic(doc) ->
    ["Test that two nodes can connect via ssl distribution"];
basic(suite) ->
    [];
basic(Config) when is_list(Config) ->
    ?line NH1 = start_ssl_node(Config),
    ?line Node1 = NH1#node_handle.nodename,
    ?line NH2 = start_ssl_node(Config),
    ?line Node2 = NH2#node_handle.nodename,

    ?line pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    ?line [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    ?line [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end),

    %% The test_server node has the same cookie as the ssl nodes
    %% but it should not be able to communicate with the ssl nodes
    %% via the erlang distribution.
    ?line pang = net_adm:ping(Node1),
    ?line pang = net_adm:ping(Node2),


    %%
    %% Check that we are able to communicate over the erlang
    %% distribution between the ssl nodes.
    %%
    ?line Ref = make_ref(),
    ?line spawn(fun () ->
			apply_on_ssl_node(
			  NH1,
			  fun () -> 
				  tstsrvr_format("Hi from ~p!~n",
						 [node()]),
				  send_to_tstcntrl({Ref, self()}),
				  receive
				      {From, ping} ->
					  From ! {self(), pong}
				  end
			  end)
		end),
    ?line receive
	      {Ref, SslPid} ->
		  ?line ok = apply_on_ssl_node(
			       NH2,
			       fun () ->
				       tstsrvr_format("Hi from ~p!~n",
						      [node()]),
				       SslPid ! {self(), ping},
				       receive
					   {SslPid, pong} ->
					       ok
				       end
			       end)
	  end,

    ?line stop_ssl_node(NH1),
    ?line stop_ssl_node(NH2),
    ?line success(Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% Internal functions                                                    %%
%%                                                                       %%

%%
%% ssl_node side api
%%

tstsrvr_format(Fmt, ArgList) ->
    send_to_tstsrvr({format, Fmt, ArgList}).

send_to_tstcntrl(Message) ->
    send_to_tstsrvr({message, Message}).


%%
%% test_server side api
%%

apply_on_ssl_node(Node, M, F, A) when atom(M), atom(F), list(A) ->
    Ref = make_ref(),
    send_to_ssl_node(Node, {apply, self(), Ref, M, F, A}),
    receive
	{Ref, Result} ->
	    Result
    end.

apply_on_ssl_node(Node, Fun) when is_function(Fun, 0) ->
    Ref = make_ref(),
    send_to_ssl_node(Node, {apply, self(), Ref, Fun}),
    receive
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
			normal -> ok;
			_ -> exit(Reason)
		    end
	    end;
	Error ->
	    erlang:demonitor(Mon, [flush]),
	    exit(Error)
    end.

start_ssl_node(Config) ->
    start_ssl_node(Config, "").

start_ssl_node(Config, XArgs) ->
    Name = mk_node_name(Config),
    SSL = ?config(ssl_opts, Config),
    SSLDistOpts = setup_dist_opts(Name, ?config(priv_dir, Config)),
    start_ssl_node_raw(Name, SSL ++ " " ++ SSLDistOpts ++ XArgs).

start_ssl_node_raw(Name, Args) ->
    {ok, LSock} = gen_tcp:listen(0,
				 [binary, {packet, 4}, {active, false}]),
    {ok, ListenPort} = inet:port(LSock),
    CmdLine = mk_node_cmdline(ListenPort, Name, Args),
    ?t:format("Attempting to start ssl node ~s: ~s~n", [Name, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when port(Port) ->
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

%%
%% command line creation
%%

host_name() ->
    [$@ | Host] = lists:dropwhile(fun ($@) -> false; (_) -> true end,
				  atom_to_list(node())),
    Host.

mk_node_name(Config) ->
    {A, B, C} = erlang:now(),
    Case = ?config(testcase, Config),
    atom_to_list(?MODULE)
	++ "_"
	++ atom_to_list(Case)
	++ "_"
	++ integer_to_list(A)
	++ "-"
	++ integer_to_list(B)
	++ "-"
	++ integer_to_list(C).

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
    Prog ++ " "
	++ Static ++ " "
	++ NameSw ++ " " ++ Name ++ " "
	++ "-pa " ++ Pa ++ " " 
 	++ "-run application start crypto -run application start public_key "
	++ "-run " ++ atom_to_list(?MODULE) ++ " cnct2tstsrvr "
	++ host_name() ++ " "
	++ integer_to_list(ListenPort) ++ " "
	++ Args ++ " "
	++ "-env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ Name ++ " "
	++ "-setcookie " ++ atom_to_list(erlang:get_cookie()).

%%
%% Connection handler test_server side
%%

await_ssl_node_up(Name, LSock) ->
    case gen_tcp:accept(LSock, ?AWAIT_SLL_NODE_UP_TIMEOUT) of
	timeout ->
	    gen_tcp:close(LSock),
	    ?t:format("Timeout waiting for ssl node ~s to come up~n",
		      [Name]),
	    timeout;
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
    inet:setopts(Socket,[{active,once}]),
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
	    case catch binary_to_term(Bin) of
		{'EXIT', _} ->
		    gen_tcp:close(Socket),
		    exit({bad_data_received_from_ssl_node, Name, Bin});
		{format, FmtStr, ArgList} ->
		    ?t:format(FmtStr, ArgList);
		{message, Msg} ->
		    Parent ! Msg;
		{apply_res, To, Ref, Res} ->
		    To ! {Ref, Res};
		bye ->
		    ?t:format("Ssl node ~s stopped.~n", [Name]),
		    gen_tcp:close(Socket),
		    exit(normal);
		Unknown ->
		    exit({unexpected_message_from_ssl_node, Name, Unknown})
	    end;
	{tcp_closed, Socket} ->
	    gen_tcp:close(Socket),
	    exit({lost_connection_with_ssl_node, Name})
    end,
    tstsrvr_con_loop(Name, Socket, Parent).

%%
%% Connection handler ssl_node side
%%

% cnct2tstsrvr() is called via command line arg -run ...
cnct2tstsrvr([Host, Port]) when list(Host), list(Port) ->
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
			    _Error ->
				halt("Failed to connect to test server")
			end
		end),
    spawn(fun () ->
		  Mon = erlang:monitor(process, ConnHandler),
		  receive
		      {'DOWN', Mon, process, ConnHandler, Reason} ->
			  receive after 1000 -> ok end,
			  halt("test server connection handler terminated: "
			       ++
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
	Hndlr when pid(Hndlr) ->
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
		    gen_tcp:close(Socket),
		    init:stop(),
		    receive after infinity -> ok end;
		_Unknown ->
		    halt("test server sent me an unexpected message")
	    end;
	{tcp_closed, Socket} ->
	    halt("Lost connection to test server")
    end,
    ssl_node_con_loop(Socket).

%%
%% Setup ssl dist info
%%

rand_bin(N) ->
    rand_bin(N, []).

rand_bin(0, Acc) ->
    Acc;
rand_bin(N, Acc) ->
    rand_bin(N-1, [random:uniform(256)-1|Acc]).

make_randfile(Dir) ->
    {ok, IoDev} = file:open(filename:join([Dir, "RAND"]), [write]),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    ok = file:write(IoDev, rand_bin(1024)),
    file:close(IoDev).

append_files(FileNames, ResultFileName) ->
    {ok, ResultFile} = file:open(ResultFileName, [write]),
    do_append_files(FileNames, ResultFile).

do_append_files([], RF) ->
    ok = file:close(RF);
do_append_files([F|Fs], RF) ->
    {ok, Data} = file:read_file(F),
    ok = file:write(RF, Data),
    do_append_files(Fs, RF).
			    
setup_dist_opts(Name, PrivDir) ->
    NodeDir = filename:join([PrivDir, Name]),
    RGenDir = filename:join([NodeDir, "rand_gen"]),
    ok = file:make_dir(NodeDir),
    ok = file:make_dir(RGenDir),
    make_randfile(RGenDir),
    make_certs:all(RGenDir, NodeDir),
    SDir = filename:join([NodeDir, "server"]),
    SC = filename:join([SDir, "cert.pem"]),
    SK = filename:join([SDir, "key.pem"]),
    SKC = filename:join([SDir, "keycert.pem"]),
    append_files([SK, SC], SKC),
    CDir = filename:join([NodeDir, "client"]),
    CC = filename:join([CDir, "cert.pem"]),
    CK = filename:join([CDir, "key.pem"]),
    CKC = filename:join([CDir, "keycert.pem"]),
    append_files([CK, CC], CKC),
    "-proto_dist inet_ssl "
	++ "-ssl_dist_opt server_certfile " ++ SKC ++ " "
	++ "-ssl_dist_opt client_certfile " ++ CKC ++ " "
.%	++ "-ssl_dist_opt verify 1 depth 1".

%%
%% Start scripts etc...
%%

add_ssl_opts_config(Config) ->
    %%
    %% Start with boot scripts if on an installed system; otherwise,
    %% just point out ssl ebin with -pa.
    %%
    try
	Dir = ?config(priv_dir, Config),
	LibDir = code:lib_dir(),
	Apps = application:which_applications(),
	{value, {stdlib, _, STDL_VSN}} = lists:keysearch(stdlib, 1, Apps),
	{value, {kernel, _, KRNL_VSN}} = lists:keysearch(kernel, 1, Apps),
	StdlDir = filename:join([LibDir, "stdlib-" ++ STDL_VSN]),
	KrnlDir = filename:join([LibDir, "kernel-" ++ KRNL_VSN]),
	{ok, _} = file:read_file_info(StdlDir),
	{ok, _} = file:read_file_info(KrnlDir),
	SSL_VSN = vsn(ssl),
	VSN_CRYPTO = vsn(crypto),
	VSN_PKEY = vsn(public_key),
	
	SslDir = filename:join([LibDir, "ssl-" ++ SSL_VSN]),
	{ok, _} = file:read_file_info(SslDir),
	%% We are using an installed otp system, create the boot script.
	Script = filename:join(Dir, atom_to_list(?MODULE)),
	{ok, RelFile} = file:open(Script ++ ".rel", [write]),
        io:format(RelFile,
		  "{release, ~n"
		  " {\"SSL distribution test release\", \"~s\"},~n"
		  " {erts, \"~s\"},~n"
		  " [{kernel, \"~s\"},~n"
		  "  {stdlib, \"~s\"},~n"
		  "  {crypto, \"~s\"},~n"
		  "  {public_key, \"~s\"},~n"
		  "  {ssl, \"~s\"}]}.~n",
		  [case catch erlang:system_info(otp_release) of
		       {'EXIT', _} -> "R11B";
		       Rel -> Rel
		   end,
		   erlang:system_info(version),
		   KRNL_VSN,
		   STDL_VSN,
		   VSN_CRYPTO,
		   VSN_PKEY,
		   SSL_VSN]),
	ok = file:close(RelFile),
	ok = systools:make_script(Script, []),
	[{ssl_opts, "-boot " ++ Script} | Config]
    catch
	_:_ ->
	    [{ssl_opts, "-pa " ++ filename:dirname(code:which(ssl))}
	     | add_comment_config(
		 "Bootscript wasn't used since the test wasn't run on an "
		 "installed OTP system.",
		 Config)]
    end.

%%
%% Add common comments to config
%%

add_comment_config(Comment, []) ->
    [{comment, Comment}];
add_comment_config(Comment, [{comment, OldComment} | Cs]) ->
    [{comment, Comment ++ " " ++ OldComment} | Cs];
add_comment_config(Comment, [C|Cs]) ->
    [C|add_comment_config(Comment, Cs)].

%%
%% Call when test case success
%%

success(Config) ->
    case lists:keysearch(comment, 1, Config) of
	{value, {comment, _} = Res} -> Res;
	_ -> ok
    end.

vsn(App) ->
    application:start(App),
    try
	{value,
	 {ssl,
	  _,
	  VSN}} = lists:keysearch(App,
				  1,
				  application:which_applications()),
	VSN
     after
	 application:stop(ssl)
     end.
