%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2020. All Rights Reserved.
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

-module(ssh_eqc_client_server).

-compile(export_all).
 
-ifndef(PROPER).
-else.
%% Only use proper
%% 
%% Previously only EQC was supported, but the changes to support PROPER is not
%% just a wrapper. Since we do not have access to eqc we can't test the changes
%% so therefore eqc is disabeled.
%% However, with access to eqc it ought to be quite easy to re-enable eqc by
%% studying the diff.

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_property_test.hrl").


%% Limit the testing time on CI server... this needs to be improved in % from total budget.
-define(TESTINGTIME(Prop), eqc:testing_time(30,Prop)).
  
-define(SSH_DIR,"ssh_eqc_client_server_dirs").

-define(sec, *1000).
-define(min, *60?sec).

-record(srvr,{ref,
	      address,
	      port
	     }).

-record(chan, {ref,
	       conn_ref,
	       subsystem,
	       client_pid
	      }).
		 
-record(state,{
	  initialized = false,
	  servers = [],       % [#srvr{}]
	  clients = [],
	  connections = [],
	  channels = [],      % [#chan{}]
	  data_dir
	 }).

%%%===============================================================
%%%
%%% Specification of addresses, subsystems and such.
%%%

-define(MAX_NUM_SERVERS, 3).
-define(MAX_NUM_CLIENTS, 3).

-define(SUBSYSTEMS, ["echo1", "echo2", "echo3", "echo4"]).

-define(SERVER_ADDRESS,   {127,0,0,1}). % Server listening IP. Darwin, Solaris & FreeBSD
                                        % dislikes all other in 127.0.0.0/24

-define(SERVER_EXTRA_OPTIONS,  [{parallel_login,bool()}] ).
		

%%%================================================================
%%%
%%% The properties - one sequantial and one parallel with the same model
%%%
%%% Run as
%%%
%%%   $ (cd ..; make)
%%%   $ erl -pz ..
%%%
%%%   eqc:quickcheck( ssh_eqc_client_server:prop_seq() ).
%%%   eqc:quickcheck( ssh_eqc_client_server:prop_parallel() ).
%%%   eqc:quickcheck( ssh_eqc_client_server:prop_parallel_multi() ).
%%%


%% To be called as eqc:quickcheck( ssh_eqc_client_server:prop_seq() ).
prop_seq() ->
    error_logger:tty(false),
    ?TESTINGTIME(do_prop_seq(?SSH_DIR,[])).

%% To be called from a common_test test suite
prop_seq(CT_Config) ->
    error_logger:tty(false),
    do_prop_seq(full_path(?SSH_DIR, CT_Config), CT_Config).


do_prop_seq(DataDir, CT_Config) ->
    setup_rsa(DataDir),
    ?FORALL(Cmds,commands(?MODULE),
	    begin
		{H,Sf,Result} = run_commands(?MODULE,Cmds,[{data_dir,DataDir}]),
                ct_property_test:present_result(?MODULE, Cmds, {H,Sf,Result}, CT_Config, [])
	    end).

full_path(SSHdir, CT_Config) ->
    filename:join(proplists:get_value(property_dir, CT_Config),
		  SSHdir).
%%%----
prop_parallel() ->
    error_logger:tty(false),
    ?TESTINGTIME(do_prop_parallel(?SSH_DIR,[])).

%% To be called from a common_test test suite
prop_parallel(CT_Config) ->
    error_logger:tty(false),
    do_prop_parallel(full_path(?SSH_DIR, CT_Config), CT_Config).

do_prop_parallel(DataDir, CT_Config) ->
    setup_rsa(DataDir),
    ?FORALL(Cmds,parallel_commands(?MODULE),
	    begin
		{H,Sf,Result} = run_parallel_commands(?MODULE,Cmds,[{data_dir,DataDir}]),
                ct_property_test:present_result(?MODULE, Cmds, {H,Sf,Result}, CT_Config, [])
	    end).

%%%----
%% prop_parallel_multi() ->
%%     ?TESTINGTIME(do_prop_parallel_multi(?SSH_DIR, [])).

%% %% To be called from a common_test test suite
%% prop_parallel_multi(CT_Config) ->
%%     do_prop_parallel_multi(full_path(?SSH_DIR, CT_Config), CT_Config).

%% do_prop_parallel_multi(DataDir, CT_Config) ->
%%     setup_rsa(DataDir),
%%     ?FORALL(Repetitions,?SHRINK(1,[10]),
%% 	    ?FORALL(Cmds,parallel_commands(?MODULE),
%% 		    ?ALWAYS(Repetitions,
%% 			    begin
%% 				{H,Sf,Result} = run_parallel_commands(?MODULE,Cmds,[{data_dir,DataDir}]),
%%                              ct_property_test:present_result(?MODULE, Cmds, {H,Sf,Result}, CT_Config, [])
%% 			    end))).

%%%================================================================
%%% State machine spec

%%% called when using commands/1
initial_state() -> 
  #state{}.

%%% called when using commands/2
initial_state(DataDir) ->
    application:stop(ssh),
    ssh:start().

%%%----------------
weight(S, ssh_send) -> 20*length([C || C<-S#state.channels, has_subsyst(C)]);
weight(S, ssh_start_subsyst) -> 10*length([C || C<-S#state.channels, no_subsyst(C)]);
weight(S, ssh_close_channel) -> 2*length([C || C<-S#state.channels, has_subsyst(C)]);
weight(S, ssh_open_channel) ->  2*length(S#state.connections);
weight(_S, _) -> 1.

%%%----------------
fns() -> [initial_state,
          ssh_server,
          ssh_client,
          ssh_open_connection,
          ssh_close_connection,
          ssh_open_channel,
          ssh_close_channel,
          ssh_start_subsyst,
          ssh_send
         ].

call_f(Name, Sfx) -> 
    case get({Name,Sfx}) of
        undefined -> F = list_to_atom(lists:concat([Name,"_",Sfx])),
                     put({Name,Sfx}, F),
                     F;
        F when is_atom(F) -> F
    end.

-define(call(Name, What, Args), apply(?MODULE, call_f(Name,What), Args)).

symbolic_call(S,Name) -> {call, ?MODULE, Name, ?call(Name,args,[S])}.

may_generate(S, F) ->  ?call(F,pre,[S]).

command(S) ->
    frequency([{weight(S,F), symbolic_call(S,F)} || F <- fns(),
                                                    may_generate(S, F)]
             ).

precondition(S,    {call,_M,F,As})      -> try ?call(F, pre, [S,As])
                                           catch _:undef -> try ?call(F,pre,[S]) catch _:undef -> true end
                                           end.
next_state(S, Res, {call,_M,F,As})      -> try ?call(F, next, [S,Res,As]) catch _:undef -> S end.
postcondition(S,   {call,_M,F,As}, Res) -> try ?call(F, post, [S,As,Res]) catch _:undef -> true end.

%%%----------------
%%% Initialize

initial_state_pre(S) -> not S#state.initialized.

initial_state_args(_) -> [{var,data_dir}].

initial_state_next(S, _, _) -> S#state{initialized=true}.

%%%----------------
%%% Start a new daemon
%%% Precondition: not more than ?MAX_NUM_SERVERS started

%%% This is a bit funny because we need to pick an IP address and Port to
%%% run the server on, but there is no way to atomically select a free Port!
%%% 
%%% Therefore we just grab one IP-Port pair randomly and try to start the ssh server
%%% on that pair.  If it fails, we just forget about it and goes on.  Yes, it
%%% is a waste of cpu cycles, but at least it works!

ssh_server_pre(S) -> S#state.initialized andalso 
			 length(S#state.servers) < ?MAX_NUM_SERVERS.

ssh_server_args(_) -> [?SERVER_ADDRESS, {var,data_dir}, ?SERVER_EXTRA_OPTIONS]. 

ssh_server(IP0, DataDir, ExtraOptions) ->
    case ssh:daemon(IP0, 0, 
                    [
                     {system_dir, system_dir(DataDir)},
                     {user_dir, user_dir(DataDir)},
                     {subsystems, [{SS, {ssh_eqc_subsys, [SS]}} || SS <- ?SUBSYSTEMS]}
                     | ExtraOptions
                    ]) of
        {ok,DaemonRef} ->
            case ssh:daemon_info(DaemonRef) of
                {ok, Props} ->
                    Port = proplists:get_value(port,Props),
                    IP = proplists:get_value(ip,Props),
                    #srvr{ref = DaemonRef,
                          address = IP,
                          port = Port};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

ssh_server_post(_S, _Args, #srvr{port=Port}) -> (0 < Port) andalso (Port < 65536);
ssh_server_post(_S, _Args, _) -> false.

ssh_server_next(S, Srvr, _) ->
    S#state{servers=[Srvr | S#state.servers]}.

%%%----------------
%%% Start a new client
%%% Precondition: not more than ?MAX_NUM_CLIENTS started

ssh_client_pre(S) -> S#state.initialized andalso 
			 length(S#state.clients) < ?MAX_NUM_CLIENTS.

ssh_client_args(_S) -> [].

ssh_client() -> spawn(fun client_init/0).

ssh_client_next(S, Pid, _) -> S#state{clients=[Pid|S#state.clients]}.


client_init() -> client_loop().

client_loop() ->
    receive
	{please_do,Fun,Ref,Pid} ->
	    Pid ! {my_pleasure, catch Fun(), Ref},
	    client_loop()
    end.
    
do(Pid, Fun) -> do(Pid, Fun, 30?sec).

do(Pid, Fun, Timeout) when is_function(Fun,0) ->
    Pid ! {please_do,Fun,Ref=make_ref(),self()},
    receive
	{my_pleasure, Result, Ref} -> Result
    after
	Timeout -> {error,do_timeout}
    end.
	    
%%%----------------
%%% Start a new connection
%%% Precondition:  deamon exists

ssh_open_connection_pre(S) -> S#state.servers /= [].
    
ssh_open_connection_args(S) -> [oneof(S#state.servers), {var,data_dir}].
    
ssh_open_connection(#srvr{address=Ip, port=Port}, DataDir) ->
    ok(ssh:connect(ensure_string(Ip), Port, 
		   [
		    {silently_accept_hosts, true},
		    {user_dir, user_dir(DataDir)},
		    {user_interaction, false},
		    {connect_timeout, 2000}
		   ], 2000)).

ssh_open_connection_post(_S, _Args, Result) -> is_ok(Result).

ssh_open_connection_next(S, ConnRef, [_,_]) -> S#state{connections=[ConnRef|S#state.connections]}.

%%%----------------
%%% Stop a new connection
%%% Precondition:  connection exists

ssh_close_connection_pre(S) -> S#state.connections /= [].

ssh_close_connection_args(S) -> [oneof(S#state.connections)].
    
ssh_close_connection(ConnectionRef) -> ssh:close(ConnectionRef).

ssh_close_connection_next(S, _, [ConnRef]) ->
    S#state{connections = S#state.connections--[ConnRef],
            channels = [C || C <- S#state.channels,
                             C#chan.conn_ref /= ConnRef]
	       }.

%%%----------------
%%% Start a new channel without a sub system
%%% Precondition:  connection exists

ssh_open_channel_pre(S) -> S#state.connections /= [].

ssh_open_channel_args(S) -> [oneof(S#state.connections)].

%%% For re-arrangement in parallel tests. 
ssh_open_channel_pre(S,[C]) when is_record(S,state) -> lists:member(C,S#state.connections).

ssh_open_channel(ConnectionRef) -> 
    ok(ssh_connection:session_channel(ConnectionRef, 20?sec)).

ssh_open_channel_post(_S, _Args, Result) -> is_ok(Result).

ssh_open_channel_next(S, ChannelRef, [ConnRef]) ->  
    S#state{channels=[#chan{ref=ChannelRef,
			    conn_ref=ConnRef}
		      | S#state.channels]}.

%%%----------------
%%% Stop a channel
%%% Precondition: a channel exists, with or without a subsystem

ssh_close_channel_pre(S) -> S#state.channels /= [].

ssh_close_channel_args(S) -> [oneof(S#state.channels)].

ssh_close_channel(#chan{ref=ChannelRef, conn_ref=ConnectionRef}) -> 
    ssh_connection:close(ConnectionRef, ChannelRef).

ssh_close_channel_next(S, _, [C]) -> 
    S#state{channels = [Ci || Ci <- S#state.channels,
			      sig(C) /= sig(Ci)]}.

sig(C) -> {C#chan.ref, C#chan.conn_ref}.

%%%----------------
%%% Start a sub system on a channel
%%% Precondition:  A channel without subsystem exists

ssh_start_subsyst_pre(S) -> lists:any(fun no_subsyst/1, S#state.channels) andalso
				S#state.clients /= [].

ssh_start_subsyst_args(S) -> [oneof(lists:filter(fun no_subsyst/1, S#state.channels)),
			      oneof(?SUBSYSTEMS),
			      oneof(S#state.clients)
			     ].

%% For re-arrangement in parallel tests. 
ssh_start_subsyst_pre(S, [C|_]) -> lists:member(C,S#state.channels) 
				       andalso no_subsyst(C).

ssh_start_subsyst(#chan{ref=ChannelRef, conn_ref=ConnectionRef}, SubSystem, Pid) -> 
    do(Pid, fun()->ssh_connection:subsystem(ConnectionRef, ChannelRef, SubSystem, 120?sec) end).

ssh_start_subsyst_post(_S, _Args, Result) -> Result==success.

ssh_start_subsyst_next(S, _Result, [C,SS,Pid|_]) ->
    S#state{channels = [C#chan{subsystem=SS,
			       client_pid=Pid}|(S#state.channels--[C])] }.

%%%----------------
%%% Send a message on a channel
%%% Precondition: a channel exists with a subsystem connected

ssh_send_pre(S) -> lists:any(fun has_subsyst/1, S#state.channels).

ssh_send_args(S) -> 
    [oneof(lists:filter(fun has_subsyst/1, S#state.channels)),
     choose(0,1),
     message()].

%% For re-arrangement in parallel tests. 
ssh_send_pre(S, [C|_]) -> lists:member(C, S#state.channels).

ssh_send(C=#chan{conn_ref=ConnectionRef, ref=ChannelRef, client_pid=Pid}, Type, Msg) ->
    do(Pid, 
       fun() -> 
	       case ssh_connection:send(ConnectionRef, ChannelRef, Type, modify_msg(C,Msg), 10?sec) of
		   ok ->
		       receive
			   {ssh_cm,ConnectionRef,{data,ChannelRef,Type,Answer}} -> Answer
		       after 15?sec -> 
			       %% receive
			       %% 	   Other -> {error,{unexpected,Other}}
			       %% after 0 ->
				       {error,receive_timeout}
			       %% end
		       end;
		   Other ->
		       Other
	       end
       end).

ssh_send_blocking(_S, _Args) ->
   true.

ssh_send_post(_S, [C,_,Msg], Response) when is_binary(Response) ->
   Expected = ssh_eqc_subsys:response(modify_msg(C,Msg), C#chan.subsystem),
    case Response of
	Expected -> true;
	_ -> {send_failed, size(Response), size(Expected)}
    end;
	    
ssh_send_post(_S, _Args, Response) ->
   {error,Response}.
    

modify_msg(_, <<>>) -> <<>>;
modify_msg(#chan{subsystem=SS}, Msg) -> <<(list_to_binary(SS))/binary,Msg/binary>>.

%%%================================================================
%%% Misc functions

message() -> 
    resize(500, binary()).

    %% binary().

    %% oneof([binary(),
    %% 	   ?LET(Size, choose(0,10000), binary(Size))
    %% 	  ]).

has_subsyst(C) -> C#chan.subsystem /= undefined.

no_subsyst(C) -> not has_subsyst(C).


ok({ok,X}) -> X;
ok({error,Err}) -> {error,Err}.

is_ok({error,_}) -> false;
is_ok(_) -> true.

ensure_string({A,B,C,D}) -> lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D]));
ensure_string(X) -> X.

%%%================================================================
%%% The rest is taken and modified from ssh_test_lib.erl
setup_rsa(Dir) ->
    erase_dir(system_dir(Dir)),
    erase_dir(user_dir(Dir)),
    file:make_dir(system_dir(Dir)),
    file:make_dir(user_dir(Dir)),
    ct:log("Dir = ~p~ndata_dir = ~p~nsystem_dir = ~p~nuser = ~p~n",
           [Dir,data_dir(Dir),system_dir(Dir),user_dir(Dir)]),
    ssh_test_lib:setup_all_user_host_keys( data_dir(Dir), user_dir(Dir), system_dir(Dir)).

data_dir(Dir, File) ->   filename:join(Dir, File).
system_dir(Dir, File) -> filename:join([Dir, "system", File]).
user_dir(Dir, File) ->   filename:join([Dir, "user", File]).

data_dir(Dir) -> Dir.
system_dir(Dir) -> system_dir(Dir,"").
user_dir(Dir) -> user_dir(Dir,"").
    
erase_dir(Dir) ->
    case file:list_dir(Dir) of
	{ok,Files} -> lists:foreach(fun(F) -> file:delete(filename:join(Dir,F)) end,
				    Files);
	_ -> ok
    end,
    file:del_dir(Dir).

-endif.
