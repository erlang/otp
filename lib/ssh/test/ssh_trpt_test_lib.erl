%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

-module(ssh_trpt_test_lib).

%%-compile(export_all).

-export([exec/1, exec/2,
	 instantiate/2,
	 format_msg/1,
	 server_host_port/1
	]
       ).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh.hrl").		% ?UINT32, ?BYTE, #ssh{} ...
-include_lib("ssh/src/ssh_transport.hrl").
-include_lib("ssh/src/ssh_auth.hrl").

%%%----------------------------------------------------------------
-record(s, {
	  socket,
	  listen_socket,
	  opts = [],
	  timeout = 5000,			% ms
	  seen_hello = false,
	  enc = <<>>,
	  ssh = #ssh{},				% #ssh{}
	  alg_neg = {undefined,undefined},      % {own_kexinit, peer_kexinit}
	  alg,                                  % #alg{}
	  vars = dict:new(),
	  reply = [],				% Some repy msgs are generated hidden in ssh_transport :[
	  prints = [],
	  return_value
	 }).

-define(role(S), ((S#s.ssh)#ssh.role) ).


server_host_port(S=#s{}) ->
    {Host,Port} = ok(inet:sockname(S#s.listen_socket)),
    {host(Host), Port}.


%%% Options: {print_messages, false}  true|detail
%%%          {print_seqnums,false}    true
%%%          {print_ops,false}        true

exec(L) ->  exec(L, #s{}).

exec(L, S) when is_list(L) -> lists:foldl(fun exec/2, S, L);

exec(Op, S0=#s{}) ->
    S1 = init_op_traces(Op, S0),
    try seqnum_trace(
	  op(Op, S1))
    of
	S = #s{} ->
	    case proplists:get_value(silent,S#s.opts) of
		true -> ok;
		_ -> print_traces(S)
	    end,
	    {ok,S}
    catch
	{fail,Reason,Se} ->
	    report_trace('', Reason, Se),
	    {error,{Op,Reason,Se}};

	throw:Term ->
	    report_trace(throw, Term, S1),
	    throw({Term,Op});

	error:Error ->
	    report_trace(error, Error, S1),
	    error({Error,Op});

	exit:Exit ->
	    report_trace(exit, Exit, S1),
	    exit({Exit,Op});
        Cls:Err ->
            ct:pal("Class=~p, Error=~p", [Cls,Err]),
            error({"fooooooO",Op})
    end;
exec(Op, {ok,S=#s{}}) -> exec(Op, S);
exec(_, Error) -> Error.


%%%---- Server ops
op(listen, S) when ?role(S) == undefined -> op({listen,0}, S);

op({listen,Port}, S) when ?role(S) == undefined ->
    S#s{listen_socket = ok(gen_tcp:listen(Port, mangle_opts([]))),
	ssh = (S#s.ssh)#ssh{role=server}
       };

op({accept,Opts}, S) when ?role(S) == server ->
    {ok,Socket} = gen_tcp:accept(S#s.listen_socket, S#s.timeout),
    {Host,_Port} = ok(inet:sockname(Socket)),
    S#s{socket = Socket,
	ssh = init_ssh(server, Socket, host(Host), Opts),
	return_value = ok};

%%%---- Client ops
op({connect,Host,Port,Opts}, S) when ?role(S) == undefined -> 
    Socket = ok(gen_tcp:connect(host(Host), Port, mangle_opts([]))),
    S#s{socket = Socket,
	ssh = init_ssh(client, Socket, host(Host), Opts),
	return_value = ok};

%%%---- ops for both client and server
op(close_socket, S) ->
    catch gen_tcp:close(S#s.socket),
    catch gen_tcp:close(S#s.listen_socket),
    S#s{socket = undefined,
	listen_socket = undefined,
	return_value = ok};

op({set_options,Opts}, S) ->
    S#s{opts = Opts};

op({send,X}, S) ->
    send(S, instantiate(X,S));

op(receive_hello, S0) when S0#s.seen_hello =/= true ->
    case recv(S0) of
	S1=#s{return_value={hello,_}} -> S1;
	S1=#s{} -> op(receive_hello, receive_wait(S1))
    end;

op(receive_msg, S) when S#s.seen_hello == true ->
    try recv(S)
    catch
	{tcp,Exc} -> 
	    S1 = opt(print_messages, S, 
		     fun(X) when X==true;X==detail -> {"Recv~n~p~n",[Exc]} end),
	    S1#s{return_value=Exc}
    end;


op({expect,timeout,E}, S0) ->
    try op(E, S0)
    of
	S=#s{} -> fail({expected,timeout,S#s.return_value}, S)
    catch
	{receive_timeout,_} -> S0#s{return_value=timeout}
    end;

op({match,M,E}, S0) ->
    {Val,S2} = op_val(E, S0),
    case match(M, Val, S2) of
	{true,S3} -> 
	    opt(print_ops,S3,
		fun(true) -> 
			case dict:fold(
			       fun(K,V,Acc) ->
				       case dict:find(K,S0#s.vars) of
					   error -> [{K,V}|Acc];
					   _ -> Acc
				       end
			       end, [], S3#s.vars)
			of
			    [] -> {"Matches! No new bindings.",[]};
			    New ->
				Width = lists:max([length(atom_to_list(K)) || {K,_} <- New]),
				{lists:flatten(
				   ["Matches! New bindings:~n" |
				    [io_lib:format(" ~*s = ~p~n",[Width,K,V]) || {K,V}<-New]]),
				 []}
			end
		end);
	false -> 
	    fail({expected,M,Val}, 
		 opt(print_ops,S2,fun(true) -> {"nomatch!!~n",[]} end)
		)
    end;

op({print,E}, S0) ->
    {Val,S} = op_val(E, S0),
    io:format("Result of ~p ~p =~n~s~n",[?role(S0),E,format_msg(Val)]),
    S;

op(print_state, S) ->
    io:format("State(~p)=~n~s~n",[?role(S), format_msg(S)]),
    S;

op('$$', S) ->
    %% For matching etc
    S.


op_val(E, S0) ->
    case catch op(E, S0) of
	{'EXIT',{function_clause,[{ssh_trpt_test_lib,op,[E,S0],_}|_]}} ->
	    {instantiate(E,S0), S0};
	S=#s{} ->
	    {S#s.return_value, S};
	F={fail,receive_timeout,_St} ->
	    throw(F)
    end.


fail(Reason, {Fmt,Args}, S) when is_list(Fmt), is_list(Args) ->
    fail(Reason, save_prints({Fmt,Args}, S)).

fail(Reason, S) ->
    throw({fail, Reason, S}).

%%%----------------------------------------------------------------
%% No optimizations :)

match('$$', V, S) ->
    match(S#s.return_value, V, S);

match('_', _, S) ->
    {true, S};

match({'or',[P]}, V, S) -> match(P,V,S);
match({'or',[Ph|Pt]}, V, S) -> 
    case match(Ph,V,S) of
        false -> match({'or',Pt}, V, S);
	{true,S} -> {true,S}
    end;
	      
match(P, V, S) when is_atom(P) ->
    case atom_to_list(P) of
	"$"++_ ->
	    %% Variable
	    case dict:find(P,S#s.vars) of
		{ok,Val} -> match(Val, V, S);
		error -> {true,S#s{vars = dict:store(P,V,S#s.vars)}}
	    end;
	_ when P==V ->
	    {true,S};
	_ ->
	    false
    end;

match(P, V, S) when P==V -> 
    {true, S};

match(P, V, S) when is_tuple(P),
		     is_tuple(V) ->  
    match(tuple_to_list(P), tuple_to_list(V), S);

match([Hp|Tp], [Hv|Tv], S0) -> 
    case match(Hp, Hv, S0) of
	{true,S} -> match(Tp, Tv, S);
	false -> false
    end;

match(_, _, _) ->
    false.



instantiate('$$', S) ->
    S#s.return_value;	  % FIXME: What if $$ or $... in return_value?

instantiate(A, S) when is_atom(A) ->
    case atom_to_list(A) of
	"$"++_ ->
	    %% Variable
	    case dict:find(A,S#s.vars) of
		{ok,Val} -> Val;   % FIXME: What if $$ or $... in Val?
		error -> throw({unbound,A})
	    end;
	_ ->
	    A
    end;

instantiate(T, S) when is_tuple(T) ->
    list_to_tuple( instantiate(tuple_to_list(T),S) );

instantiate([H|T], S) ->
    [instantiate(H,S) | instantiate(T,S)];

instantiate(X, _S) ->
    X.

%%%================================================================
%%%
init_ssh(Role, Socket, Host, UserOptions0) ->
    UserOptions = [{user_interaction, false},
                   {vsn, {2,0}},
                   {id_string, "ErlangTestLib"}
                   | UserOptions0],
    Opts = ?PUT_INTERNAL_OPT({host,Host},
                             ssh_options:handle_options(Role, UserOptions)),
    ssh_connection_handler:init_ssh_record(Role, Socket, Opts).

mangle_opts(Options) ->
    SysOpts = [{reuseaddr, true},
	       {active, false},
	       {mode, binary}
	      ],
    SysOpts ++ lists:foldl(fun({K,_},Opts) ->
				   lists:keydelete(K,1,Opts)
			   end, Options, SysOpts).
    
host(H) -> ssh_test_lib:ntoa(ssh_test_lib:mangle_connect_address(H)).

%%%----------------------------------------------------------------
send(S=#s{ssh=C}, hello) ->
    Hello = case ?role(S) of
		client -> C#ssh.c_version;
		server -> C#ssh.s_version
	    end ++ "\r\n",
    send(S, list_to_binary(Hello));

send(S0, ssh_msg_kexinit) ->
    {Msg, _Bytes, _C0} = ssh_transport:key_exchange_init_msg(S0#s.ssh),
    send(S0, Msg);

send(S0=#s{alg_neg={undefined,PeerMsg}}, Msg=#ssh_msg_kexinit{}) ->
    S1 = opt(print_messages, S0,
	     fun(X) when X==true;X==detail -> {"Send~n~s~n",[format_msg(Msg)]} end),
    S2 = case PeerMsg of
	     #ssh_msg_kexinit{} ->
		 try ssh_transport:handle_kexinit_msg(PeerMsg, Msg, S1#s.ssh) of
		     {ok,Cx} when ?role(S1) == server ->
			 S1#s{alg = Cx#ssh.algorithms};
		     {ok,_NextKexMsgBin,Cx} when ?role(S1) == client ->
			 S1#s{alg = Cx#ssh.algorithms}
		 catch
		     Class:Exc ->
			 save_prints({"Algoritm negotiation failed at line ~p:~p~n~p:~s~nPeer: ~s~n Own: ~s~n",
				      [?MODULE,?LINE,Class,format_msg(Exc),format_msg(PeerMsg),format_msg(Msg)]},
				     S1)
		 end;
	     undefined ->
		 S1
	 end,
    {Bytes, C} = ssh_transport:ssh_packet(Msg, S2#s.ssh),
    send_bytes(Bytes, S2#s{return_value = Msg,
			  alg_neg = {Msg,PeerMsg},
			  ssh = C});

send(S0, ssh_msg_kexdh_init) when ?role(S0) == client ->
    {OwnMsg, PeerMsg} = S0#s.alg_neg,
    {ok, NextKexMsgBin, C} = 
	try ssh_transport:handle_kexinit_msg(PeerMsg, OwnMsg, S0#s.ssh)
	catch
	    Class:Exc ->
		fail("Algoritm negotiation failed!",
		     {"Algoritm negotiation failed at line ~p:~p~n~p:~s~nPeer: ~s~n Own: ~s",
		      [?MODULE,?LINE,Class,format_msg(Exc),format_msg(PeerMsg),format_msg(OwnMsg)]},
		     S0)
	end,
    S = opt(print_messages, S0,
	    fun(X) when X==true;X==detail -> 
		    #ssh{keyex_key = {{_Private, Public}, {_G, _P}}} = C,
		    Msg = #ssh_msg_kexdh_init{e = Public},
		    {"Send (reconstructed)~n~s~n",[format_msg(Msg)]}
	    end),
    send_bytes(NextKexMsgBin, S#s{ssh = C});

send(S0, ssh_msg_kexdh_reply) ->
    Bytes = proplists:get_value(ssh_msg_kexdh_reply, S0#s.reply),
    S = opt(print_messages, S0,
	    fun(X) when X==true;X==detail -> 
		    {{_Private, Public}, _} = (S0#s.ssh)#ssh.keyex_key,
		    Msg = #ssh_msg_kexdh_reply{public_host_key = 'Key',
					       f = Public,
					       h_sig = 'H_SIG'
					      },
		    {"Send (reconstructed)~n~s~n",[format_msg(Msg)]}
	    end),
    send_bytes(Bytes, S#s{return_value = Bytes});

send(S0, Line) when is_binary(Line) ->
    S = opt(print_messages, S0,
	    fun(X) when X==true;X==detail -> {"Send line~n~p~n",[Line]} end),
    send_bytes(Line, S#s{return_value = Line});

send(S0, {special,Msg,PacketFun}) when is_tuple(Msg),
				       is_function(PacketFun,2) ->
    S = opt(print_messages, S0,
	    fun(X) when X==true;X==detail -> {"Send~n~s~n",[format_msg(Msg)]} end),
    {Packet, C} = PacketFun(Msg, S#s.ssh),
    send_bytes(Packet, S#s{ssh = C, %%inc_send_seq_num(C),
			   return_value = Msg});

send(S0, #ssh_msg_newkeys{} = Msg) ->
    S = opt(print_messages, S0,
	    fun(X) when X==true;X==detail -> {"Send~n~s~n",[format_msg(Msg)]} end),
    {ok, Packet, C} = ssh_transport:new_keys_message(S#s.ssh),
    send_bytes(Packet, S#s{ssh = C});
    
send(S0, Msg) when is_tuple(Msg) ->
    S = opt(print_messages, S0,
	    fun(X) when X==true;X==detail -> {"Send~n~s~n",[format_msg(Msg)]} end),
    {Packet, C} = ssh_transport:ssh_packet(Msg, S#s.ssh),
    send_bytes(Packet, S#s{ssh = C, %%inc_send_seq_num(C),
			   return_value = Msg}).

send_bytes(B, S0) ->
    S = opt(print_messages, S0, fun(detail) -> {"Send bytes~n~p~n",[B]} end),
    ok(gen_tcp:send(S#s.socket, B)),
    S.

%%%----------------------------------------------------------------
recv(S0 = #s{}) ->
    S1 = receive_poll(S0),
    case S1#s.seen_hello of
	{more,Seen} ->
	    %% Has received parts of a line. Has not seen a complete hello.
	    try_find_crlf(Seen, S1);
	false ->
	    %% Must see hello before binary messages
	    try_find_crlf(<<>>, S1);
	true ->
	    %% Has seen hello, therefore no more crlf-messages are alowed.
	    S = receive_binary_msg(S1),
	    case PeerMsg = S#s.return_value of
		#ssh_msg_kexinit{} ->
		    case S#s.alg_neg of
			{undefined,undefined} ->
			    S#s{alg_neg = {undefined,PeerMsg}};

			{undefined,_} ->
			    fail("2 kexint received!!", S);
					
			{OwnMsg, _} ->
			    try ssh_transport:handle_kexinit_msg(PeerMsg, OwnMsg, S#s.ssh) of
				{ok,C} when ?role(S) == server ->
				    S#s{alg_neg = {OwnMsg, PeerMsg},
					alg = C#ssh.algorithms,
					ssh = C};
				{ok,_NextKexMsgBin,C} when ?role(S) == client ->
				    S#s{alg_neg = {OwnMsg, PeerMsg},
					alg = C#ssh.algorithms}
			    catch
				Class:Exc ->
				    save_prints({"Algoritm negotiation failed at line ~p:~p~n~p:~s~nPeer: ~s~n Own: ~s~n",
						 [?MODULE,?LINE,Class,format_msg(Exc),format_msg(PeerMsg),format_msg(OwnMsg)]},
						S#s{alg_neg = {OwnMsg, PeerMsg}})
			    end
		    end;

		#ssh_msg_kexdh_init{} -> % Always the server
		    {ok, Reply, C} = ssh_transport:handle_kexdh_init(PeerMsg, S#s.ssh),
		    S#s{ssh = C,
			reply = [{ssh_msg_kexdh_reply,Reply} | S#s.reply]
		       };
		#ssh_msg_kexdh_reply{} ->
		    {ok, _NewKeys, C} = ssh_transport:handle_kexdh_reply(PeerMsg, S#s.ssh),
                    S#s{ssh = (S#s.ssh)#ssh{shared_secret = C#ssh.shared_secret,
                                            exchanged_hash = C#ssh.exchanged_hash,
                                            session_id = C#ssh.session_id}};
		    %%%S#s{ssh=C#ssh{send_sequence=S#s.ssh#ssh.send_sequence}}; % Back the number
		#ssh_msg_newkeys{} ->
		    {ok, C} = ssh_transport:handle_new_keys(PeerMsg, S#s.ssh),
		    S#s{ssh=C};
		_ ->
		    S
	    end
    end.

%%%================================================================
try_find_crlf(Seen, S0) ->
    case erlang:decode_packet(line,S0#s.enc,[]) of
	{more,_} ->
	    Line = <<Seen/binary,(S0#s.enc)/binary>>,
	    S0#s{seen_hello = {more,Line},
		 enc = <<>>,	       % didn't find a complete line 
				       % -> no more characters to test
		 return_value = {more,Line}
	       };
	{ok,Used,Rest} ->
	    Line = <<Seen/binary,Used/binary>>,
	    case handle_hello(Line, S0) of
		false ->
		    S = opt(print_messages, S0, 
			    fun(X) when X==true;X==detail -> {"Recv info~n~p~n",[Line]} end),
		    S#s{seen_hello = false,
			enc = Rest,
			return_value = {info,Line}};
		S1=#s{} ->
		    S = opt(print_messages, S1,
			fun(X) when X==true;X==detail -> {"Recv hello~n~p~n",[Line]} end),
		    S#s{seen_hello = true,
			enc = Rest,
			return_value = {hello,Line}}
	    end
    end.


handle_hello(Bin, S=#s{ssh=C}) ->
    case {ssh_transport:handle_hello_version(binary_to_list(Bin)),
	  ?role(S)}
    of
	{{undefined,_}, _} ->  false;
	{{Vp,Vs}, client} ->   S#s{ssh = C#ssh{s_vsn=Vp, s_version=Vs}};
	{{Vp,Vs}, server} ->   S#s{ssh = C#ssh{c_vsn=Vp, c_version=Vs}}
    end.

receive_binary_msg(S0=#s{ssh=C0=#ssh{decrypt_block_size = BlockSize,
				     recv_mac_size = MacSize
				    }
			}) ->
    case size(S0#s.enc) >= max(8,BlockSize) of
	false ->
	    %% Need more bytes to decode the packet_length field
	    Remaining = max(8,BlockSize) - size(S0#s.enc),
	    receive_binary_msg( receive_wait(Remaining, S0) );
	true ->
	    %% Has enough bytes to decode the packet_length field
	    {_, <<?UINT32(PacketLen), _/binary>>, _} =
		ssh_transport:decrypt_blocks(S0#s.enc, BlockSize, C0), % FIXME: BlockSize should be at least 4

	    %% FIXME: Check that ((4+PacketLen) rem BlockSize) == 0 ?

	    S1 = if
		     PacketLen > ?SSH_MAX_PACKET_SIZE ->
			 fail({too_large_message,PacketLen},S0);        % FIXME: disconnect

		     ((4+PacketLen) rem BlockSize) =/= 0 ->
			 fail(bad_packet_length_modulo, S0); % FIXME: disconnect

		     size(S0#s.enc) >= (4 + PacketLen + MacSize) ->
			 %% has the whole packet
			 S0;

		     true ->
			 %% need more bytes to get have the whole packet
			 Remaining = (4 + PacketLen + MacSize) - size(S0#s.enc),
			 receive_wait(Remaining, S0)
		 end,

	    %% Decrypt all, including the packet_length part (re-use the initial #ssh{})
	    {C1, SshPacket = <<?UINT32(_),?BYTE(PadLen),Tail/binary>>, EncRest} = 
		ssh_transport:decrypt_blocks(S1#s.enc, PacketLen+4, C0),
	    
	    PayloadLen = PacketLen - 1 - PadLen,
	    <<CompressedPayload:PayloadLen/binary, _Padding:PadLen/binary>> = Tail,

	    {C2, Payload} = ssh_transport:decompress(C1, CompressedPayload),

	    <<Mac:MacSize/binary, Rest/binary>> = EncRest,

	    case {ssh_transport:is_valid_mac(Mac, SshPacket, C2),
		  catch ssh_message:decode(set_prefix_if_trouble(Payload,S1))}
	    of
		{false,         _} -> fail(bad_mac,S1);
		{_,    {'EXIT',_}} -> fail(decode_failed,S1);

		{true, Msg} ->
		    C3 = case Msg of
			     #ssh_msg_kexinit{} ->
				 ssh_transport:key_init(opposite_role(C2), C2, Payload);
			     _ ->
				 C2
			 end,
		    S2 = opt(print_messages, S1,
			     fun(X) when X==true;X==detail -> {"Recv~n~s~n",[format_msg(Msg)]} end),
		    S3 = opt(print_messages, S2, 
			     fun(detail) -> {"decrypted bytes ~p~n",[SshPacket]} end),
		    S3#s{ssh = inc_recv_seq_num(C3),
			 enc = Rest,
			 return_value = Msg
			}
	    end
    end.


set_prefix_if_trouble(Msg = <<?BYTE(Op),_/binary>>, #s{alg=#alg{kex=Kex}})
  when Op == 30;
       Op == 31
       ->
    case catch atom_to_list(Kex) of
	"ecdh-sha2-" ++ _ -> 
	    <<"ecdh",Msg/binary>>;
	"diffie-hellman-group-exchange-" ++ _ ->
	    <<"dh_gex",Msg/binary>>;
	"diffie-hellman-group" ++ _ ->
	    <<"dh",Msg/binary>>;
	_ -> 
	    Msg
    end;
set_prefix_if_trouble(Msg, _) ->
    Msg.


receive_poll(S=#s{socket=Sock}) -> 
    inet:setopts(Sock, [{active,once}]),
    receive
	{tcp,Sock,Data} ->
	    receive_poll( S#s{enc = <<(S#s.enc)/binary,Data/binary>>} );
	{tcp_closed,Sock} ->
	    throw({tcp,tcp_closed});
	{tcp_error, Sock, Reason} ->
	    throw({tcp,{tcp_error,Reason}})
    after 0 ->
	    S
    end.

receive_wait(S=#s{socket=Sock,
		  timeout=Timeout}) -> 
    inet:setopts(Sock, [{active,once}]),
    receive
	{tcp,Sock,Data} ->
	    S#s{enc = <<(S#s.enc)/binary,Data/binary>>};
	{tcp_closed,Sock} ->
	    throw({tcp,tcp_closed});
	{tcp_error, Sock, Reason} ->
	    throw({tcp,{tcp_error,Reason}})
    after Timeout ->
	    fail(receive_timeout,S)
    end.

receive_wait(N, S=#s{socket=Sock,
		     timeout=Timeout,
		     enc=Enc0}) when N>0 ->
    inet:setopts(Sock, [{active,once}]),
    receive
	{tcp,Sock,Data} ->
	    receive_wait(N-size(Data), S#s{enc = <<Enc0/binary,Data/binary>>});
	{tcp_closed,Sock} ->
	    throw({tcp,tcp_closed});
	{tcp_error, Sock, Reason} ->
	    throw({tcp,{tcp_error,Reason}})
    after Timeout ->
	    fail(receive_timeout, S)
    end;
receive_wait(_N, S) ->
    S.

%% random_padding_len(PaddingLen1, ChunkSize) ->
%%     MaxAdditionalRandomPaddingLen = 		% max 255 bytes padding totaö
%% 	(255 - PaddingLen1) - ((255 - PaddingLen1) rem ChunkSize),
%%     AddLen0 = crypto:rand_uniform(0,MaxAdditionalRandomPaddingLen),
%%     AddLen0 - (AddLen0 rem ChunkSize).		% preserve the blocking

inc_recv_seq_num(C=#ssh{recv_sequence=N}) -> C#ssh{recv_sequence=(N+1) band 16#ffffffff}.
%%%inc_send_seq_num(C=#ssh{send_sequence=N}) -> C#ssh{send_sequence=(N+1) band 16#ffffffff}.

opposite_role(#ssh{role=R}) -> opposite_role(R);
opposite_role(client) -> server;
opposite_role(server) -> client.

ok(ok) -> ok;
ok({ok,R}) -> R;
ok({error,E}) -> erlang:error(E).


%%%================================================================
%%%
%%% Formating of records
%%% 

format_msg(M) -> format_msg(M, 0).

format_msg(M, I0) ->
    case fields(M) of
	undefined -> io_lib:format('~p',[M]);
	Fields ->
	    [Name|Args] = tuple_to_list(M),
	    Head = io_lib:format('#~p{',[Name]),
	    I = lists:flatlength(Head)+I0,
	    NL = io_lib:format('~n~*c',[I,$ ]),
	    Sep = io_lib:format(',~n~*c',[I,$ ]),
	    Tail = [begin 
			S0 = io_lib:format('~p = ',[F]),
			I1 = I + lists:flatlength(S0),
			[S0,format_msg(A,I1)]
		    end
		    || {F,A} <- lists:zip(Fields,Args)],
	    [[Head|string:join(Tail,Sep)],NL,"}"]
    end.

fields(M) ->
    case M of
	#ssh_msg_debug{} -> record_info(fields, ssh_msg_debug);
	#ssh_msg_disconnect{} -> record_info(fields, ssh_msg_disconnect);
	#ssh_msg_ignore{} -> record_info(fields, ssh_msg_ignore);
	#ssh_msg_kex_dh_gex_group{} -> record_info(fields, ssh_msg_kex_dh_gex_group);
	#ssh_msg_kex_dh_gex_init{} -> record_info(fields, ssh_msg_kex_dh_gex_init);
	#ssh_msg_kex_dh_gex_reply{} -> record_info(fields, ssh_msg_kex_dh_gex_reply);
	#ssh_msg_kex_dh_gex_request{} -> record_info(fields, ssh_msg_kex_dh_gex_request);
	#ssh_msg_kex_dh_gex_request_old{} -> record_info(fields, ssh_msg_kex_dh_gex_request_old);
	#ssh_msg_kexdh_init{} -> record_info(fields, ssh_msg_kexdh_init);
	#ssh_msg_kexdh_reply{} -> record_info(fields, ssh_msg_kexdh_reply);
	#ssh_msg_kexinit{} -> record_info(fields, ssh_msg_kexinit);
	#ssh_msg_newkeys{} -> record_info(fields, ssh_msg_newkeys);
	#ssh_msg_service_accept{} -> record_info(fields, ssh_msg_service_accept);
	#ssh_msg_service_request{} -> record_info(fields, ssh_msg_service_request);
	#ssh_msg_unimplemented{} -> record_info(fields, ssh_msg_unimplemented);
	#ssh_msg_userauth_request{} -> record_info(fields, ssh_msg_userauth_request);
	#ssh_msg_userauth_failure{} -> record_info(fields, ssh_msg_userauth_failure);
	#ssh_msg_userauth_success{} -> record_info(fields, ssh_msg_userauth_success);
	#ssh_msg_userauth_banner{} -> record_info(fields, ssh_msg_userauth_banner);
	#ssh_msg_userauth_passwd_changereq{} -> record_info(fields, ssh_msg_userauth_passwd_changereq);
	#ssh_msg_userauth_pk_ok{} -> record_info(fields, ssh_msg_userauth_pk_ok);
	#ssh_msg_userauth_info_request{} -> record_info(fields, ssh_msg_userauth_info_request);
	#ssh_msg_userauth_info_response{} -> record_info(fields, ssh_msg_userauth_info_response);
	#s{} -> record_info(fields, s);
	#ssh{} -> record_info(fields, ssh);
	#alg{} -> record_info(fields, alg);
	_ -> undefined
    end.

%%%================================================================
%%%
%%% Trace handling
%%% 

init_op_traces(Op, S0) ->
    opt(print_ops, S0#s{prints=[]},
	fun(true) -> 
		case ?role(S0) of
		    undefined -> {"-- ~p~n",[Op]};
		    Role -> {"-- ~p ~p~n",[Role,Op]}
		end
	end
       ).

report_trace(Class, Term, S) ->
    print_traces(
      opt(print_ops, S,
	  fun(true) -> {"~s ~p",[Class,Term]} end)
     ).

seqnum_trace(S) ->
    opt(print_seqnums, S,
	fun(true) when S#s.ssh#ssh.send_sequence =/= S#s.ssh#ssh.send_sequence,
		       S#s.ssh#ssh.recv_sequence =/= S#s.ssh#ssh.recv_sequence ->
		{"~p seq num: send ~p->~p,  recv ~p->~p~n",
		 [?role(S),
		  S#s.ssh#ssh.send_sequence, S#s.ssh#ssh.send_sequence,
		  S#s.ssh#ssh.recv_sequence, S#s.ssh#ssh.recv_sequence
		 ]};
	   (true) when S#s.ssh#ssh.send_sequence =/=  S#s.ssh#ssh.send_sequence ->
		{"~p seq num: send ~p->~p~n",
		 [?role(S),
		  S#s.ssh#ssh.send_sequence, S#s.ssh#ssh.send_sequence]};
	   (true) when S#s.ssh#ssh.recv_sequence =/=  S#s.ssh#ssh.recv_sequence ->
		{"~p seq num: recv ~p->~p~n",
		 [?role(S),
		  S#s.ssh#ssh.recv_sequence, S#s.ssh#ssh.recv_sequence]}
	end).

print_traces(S) when S#s.prints == [] -> S;
print_traces(S) ->
    Len = length(S#s.prints),
    ct:log("~s",
	   [lists:foldl(
	      fun({Fmt,Args}, Acc) ->
		      [case Len-length(Acc)-1 of
			   0 ->
			       io_lib:format(Fmt,Args);
			   _N ->
			       io_lib:format(lists:concat(['~p --------~n',Fmt]),
					     [Len-length(Acc)-1|Args])
		       end | Acc]
	      end, "", S#s.prints)]
	  ).

opt(Flag, S, Fun) when is_function(Fun,1) ->
    try Fun(proplists:get_value(Flag,S#s.opts))
    of P={Fmt,Args} when is_list(Fmt), is_list(Args) ->
	    save_prints(P, S)
    catch _:_ ->
	    S
    end.

save_prints({Fmt,Args}, S) -> 
    S#s{prints = [{Fmt,Args}|S#s.prints]}.
