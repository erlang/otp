%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%% Purpose: megaco sequence generator for the megaco test suite
%%----------------------------------------------------------------------

-module(megaco_test_megaco_generator).

-behaviour(megaco_test_generator).

-compile({no_auto_import,[error/1]}).

%% API
-export([
         start_link/1, start_link/2,
	 stop/1, 
         exec/2, exec/3
        ]).

%% genarator behaviour callback exports
-export([
         init/1,
         handle_parse/2,
         handle_exec/2,
         terminate/2
        ]).

%% Megaco callback api
-export([
         handle_connect/3, handle_connect/4,
         handle_disconnect/4,
         handle_syntax_error/4,        handle_syntax_error/5,
         handle_message_error/4,       handle_message_error/5,
         handle_trans_request/4,       handle_trans_request/5,
         handle_trans_long_request/4,  handle_trans_long_request/5,
         handle_trans_reply/5,         handle_trans_reply/6,
         handle_trans_ack/5,           handle_trans_ack/6,
         handle_trans_request_abort/5, handle_trans_request_abort/6,
         handle_unexpected_trans/4,    handle_unexpected_trans/5
        ]).


%%----------------------------------------------------------------------

-include_lib("megaco/include/megaco.hrl").


%%----------------------------------------------------------------------

-define(DELIVER_MOD, megaco_test_deliver).


%%----------------------------------------------------------------------

-record(state,
	{
	  mid,
	  recv_handle,
	  port,
	  send_handle,
	  conn_handle,

	  transport_sup,
	  ctrl_pid,

	  result = [] % Accumulated results from verification
	 }).


%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

start_link(Name) ->
    megaco_test_generator:start_link(?MODULE, [], Name).

start_link(Name, Node) ->
    megaco_test_generator:start_link(?MODULE, [], Name, Node).

stop(Server) ->
    megaco_test_generator:stop(Server).

exec(Server, Instructions) when is_list(Instructions) ->
    megaco_test_generator:exec(Server, Instructions).

exec(Server, Instructions, Timeout) when is_list(Instructions) ->
    megaco_test_generator:exec(Server, Instructions, Timeout).


%%----------------------------------------------------------------------
%% generator callback functions
%%----------------------------------------------------------------------

init([]) ->
    random_init(),
    {ok, #state{}}.


%% ----- instruction parser -----

handle_parse({debug, Debug} = Instruction, State)
  when (Debug == true) orelse (Debug == false) ->
    {ok, Instruction, State};

handle_parse({expect_nothing, To} = Instruction, State)
  when is_integer(To) andalso (To > 0) ->
    {ok, Instruction, State};

handle_parse({megaco_trace, Level} = Instruction, State)
  when (Level == disable) orelse 
       (Level == max)     orelse 
       (Level == min)     orelse
       is_integer(Level) ->
    {ok, Instruction, State};

handle_parse({sleep, To} = Instruction, State)
  when is_integer(To) andalso (To > 0) ->
    {ok, Instruction, State};

handle_parse(megaco_start = Instruction, State) ->
    {ok, Instruction, State};

handle_parse(megaco_stop = Instruction, State) ->
    {ok, Instruction, State};

handle_parse({megaco_start_user, _Mid, _RecvInfo, Conf} = Instruction, State)
  when is_list(Conf) ->
    {ok, Instruction, State};

handle_parse(megaco_stop_user = Instruction, State) ->
    {ok, Instruction, State};

handle_parse(megaco_info = Instruction, State) ->
    {ok, Instruction, State};

handle_parse(megaco_system_info, State) ->
    Verify = fun(_) -> ok end,
    Instruction = {megaco_system_info, internal_system_info_tag, Verify}, 
    {ok, Instruction, State};

handle_parse({megaco_system_info, Tag}, State) 
  when is_atom(Tag) ->
    Verify = fun(_) -> ok end,
    Instruction = {megaco_system_info, Tag, Verify}, 
    {ok, Instruction, State};

handle_parse({megaco_system_info, Tag, Verify} = Instruction, State) 
  when is_atom(Tag) andalso is_function(Verify) ->
    {ok, Instruction, State};

handle_parse({megaco_user_info, Tag} = Instruction, State)
  when is_atom(Tag) ->
    {ok, Instruction, State};

handle_parse({megaco_update_user_info, Tag, _Val} = Instruction, State)
  when is_atom(Tag) ->
    {ok, Instruction, State};

handle_parse({megaco_conn_info, Tag} = Instruction, State)
  when is_atom(Tag) ->
    {ok, Instruction, State};

handle_parse({megaco_update_conn_info, Tag, _Val} = Instruction, State)
  when is_atom(Tag) ->
    {ok, Instruction, State};

handle_parse(start_transport = Instruction, State) ->
    {ok, Instruction, State};

handle_parse(listen = _Instruction, State) ->
    MeybeRetry  = make_connect_retry_fun2(), 
    Instruction = {listen, [], MeybeRetry}, 
    {ok, Instruction, State};

handle_parse({listen, Opts} = _Instruction, State) 
  when is_list(Opts) ->
    MeybeRetry  = make_connect_retry_fun2(), 
    Instruction = {listen, Opts, MeybeRetry},     
    {ok, Instruction, State};

handle_parse({listen, Opts, MeybeRetry} = Instruction, State) 
  when is_list(Opts) andalso is_function(MeybeRetry) ->
    {ok, Instruction, State};

handle_parse(connect = _Instruction, State) ->
    case inet:gethostname() of
	{ok, LocalHost} ->
	    MeybeRetry  = make_connect_retry_fun2(), 
	    Instruction = {connect, LocalHost, [], MeybeRetry},
	    {ok, Instruction, State};
	Error ->
	    Error
    end;

handle_parse({connect, Opts} = _Instruction, State)
  when is_list(Opts) ->
    verify_connect_opts(Opts),
    case inet:gethostname() of
	{ok, LocalHost} ->
	    MeybeRetry  = make_connect_retry_fun2(), 
	    Instruction = {connect, LocalHost, Opts, MeybeRetry},
	    {ok, Instruction, State};
	Error ->
	    Error
    end;

handle_parse({connect, Host} = _Instruction, State) 
  when is_atom(Host) ->
    MeybeRetry  = make_connect_retry_fun2(), 
    Instruction = {connect, Host, [], MeybeRetry},
    {ok, Instruction, State};

handle_parse({connect, Host, Opts} = _Instruction, State)
  when (is_atom(Host) orelse is_list(Host)) andalso is_list(Opts) ->
    verify_connect_opts(Opts),
    MeybeRetry  = make_connect_retry_fun2(), 
    Instruction = {connect, Host, Opts, MeybeRetry},
    {ok, Instruction, State};

handle_parse({connect, Host, Opts, MeybeRetry} = Instruction, State)
  when (is_atom(Host) orelse is_list(Host)) andalso 
       is_list(Opts) andalso 
       is_function(MeybeRetry) ->
    verify_connect_opts(Opts),
    {ok, Instruction, State};

handle_parse(disconnect = Instruction, State) ->
    {ok, Instruction, State};

handle_parse(megaco_connect = Instruction, State) ->
    {ok, Instruction, State};

handle_parse({megaco_connect, _} = Instruction, State) ->
    {ok, Instruction, State};

handle_parse(megaco_disconnect = Instruction, State) ->
    {ok, Instruction, State};

handle_parse({megaco_disconnect, _Reason} = Instruction, State) ->
    {ok, Instruction, State};

handle_parse({megaco_call, ARs, Opts} = Instruction, State)
  when (is_list(ARs) orelse is_binary(ARs)) andalso is_list(Opts) ->
    {ok, Instruction, State};

handle_parse({megaco_call, _Mid, ARs, Opts} = Instruction, State)
  when (is_list(ARs) orelse is_binary(ARs)) andalso is_list(Opts) ->
    {ok, Instruction, State};

handle_parse({megaco_cast, ARs, Opts} = Instruction, State)
  when (is_list(ARs) orelse is_binary(ARs)) andalso is_list(Opts) ->
    {ok, Instruction, State};

handle_parse({megaco_cast, _Mid, ARs, Opts} = Instruction, State)
  when (is_list(ARs) orelse is_binary(ARs)) andalso is_list(Opts) ->
    {ok, Instruction, State};

handle_parse({megaco_cancel, _Reason} = Instruction, State) ->
    {ok, Instruction, State};

handle_parse({megaco_callback, Tag, TimeoutOrVerify} = Instruction, State)
  when (is_atom(Tag) andalso 
	((is_integer(TimeoutOrVerify) andalso 
	  (TimeoutOrVerify > 0)) orelse 
	 is_function(TimeoutOrVerify))) ->
    {ok, Instruction, State};

handle_parse({megaco_callback, Tag, Verify, Timeout} = Instruction, State)
  when (is_atom(Tag) andalso 
	is_function(Verify) andalso 
	(is_integer(Timeout) andalso (Timeout > 0))) ->
    {ok, Instruction, State};

handle_parse({megaco_callback, Tag, {VMod, VFunc, VArgs}} = _Instruction, 
	     State)
  when (is_atom(Tag) andalso
	(is_atom(VMod) andalso is_atom(VFunc) andalso is_list(VArgs))) ->
    Verify = fun(X) ->
                     io:format("[megaco_callback ~w] calling ~w:~w with"
                               "~n   X:     ~p"
                               "~n   VArgs: ~w"
                               "~n", [Tag, VMod, VFunc, X, VArgs]),
                     (catch apply(VMod, VFunc, [X|VArgs]))
             end,
    Instruction = {megaco_callback, Tag, Verify}, 
    {ok, Instruction, State};

handle_parse({megaco_callback, Verifiers0} = _Instruction, State)
  when is_list(Verifiers0) ->
    Verifiers = [make_verifier(Verifier) || Verifier <- Verifiers0],
    Instruction = {megaco_callback, Verifiers},
    {ok, Instruction, State};

handle_parse({trigger, Trigger} = Instruction, State)
  when is_function(Trigger) ->
    {ok, Instruction, State};

handle_parse(Instruction, _State) ->
    error({invalid_instruction, Instruction}).


make_verifier({Tag, No, VerifyFunc} = Verify)
  when is_atom(Tag) andalso is_integer(No) andalso is_function(VerifyFunc) ->
    Verify;
make_verifier({Tag, No, {VMod, VFunc, VArgs}})
  when is_atom(Tag) andalso is_integer(No) andalso
       (is_atom(VMod) andalso is_atom(VFunc) andalso is_list(VArgs)) ->
    VerifyFunc = fun(X) ->
                         io:format("[megaco_callback ~w] calling ~w:~w with"
                                   "~n   X: ~p"
                                   "~n   VArgs: ~w"
                                   "~n", [Tag, VMod, VFunc, X, VArgs]),
                         (catch apply(VMod, VFunc, [X|VArgs]))
                 end,
    Verify = {Tag, No, VerifyFunc},
    Verify;
make_verifier(BadVerifier) ->
    error({bad_verifier, BadVerifier}).


verify_connect_opts([]) ->
    ok;
verify_connect_opts([{Key, _}|Opts]) when is_atom(Key) ->
    verify_connect_opts(Opts);
verify_connect_opts([H|_]) ->
    error({bad_opts_list, H}).

%% make_connect_retry_fun1() ->
%%      fun(Error, _) -> 
%% 	     {false, Error} 
%%      end.

make_connect_retry_fun2() ->
     fun(Error, noError) -> 
	     Timeout = 250,
	     sleep(random(Timeout) + 100),
	     {true, {3, Timeout*2, Error}};
	(_Error, {0, _Timeout, OriginalError}) ->
	     {false, OriginalError};
	(_Error, {N, Timeout, OriginalError}) ->
	     sleep(random(Timeout) + 100), 
	     {true, {N-1, Timeout*2, OriginalError}}
     end.


%% ----- instruction exececutor -----

handle_exec({debug, Debug}, State) ->
    p("debug: ~p", [Debug]),
    put(debug, Debug),
    {ok, State};

handle_exec({expect_nothing, To}, State) ->
    p("expect_nothing: ~p", [To]),
    receive
        Any ->
            error({expect_nothing, Any})
    after To ->
        {ok, State}
    end;

handle_exec({megaco_trace, disable}, State) ->
    p("megaco trace: disable"),
    megaco:disable_trace(),
    {ok, State};
handle_exec({megaco_trace, Level}, State) ->
    p("megaco trace: enable [~w]", [Level]),
    megaco:enable_trace(Level, io),
    {ok, State};

handle_exec(megaco_start, State) ->
    p("megaco_start"),
    ok = megaco:start(),
    {ok, State};

handle_exec(megaco_stop, State) ->
    p("megaco_stop"),
    ok = megaco:stop(),
    {ok, State};

handle_exec({megaco_start_user, Mid, RecvInfo, Conf}, State) ->
    p("megaco_start_user: ~p", [Mid]),

    d("megaco_start_user -> start user"),
    ok = megaco:start_user(Mid, Conf),

    d("megaco_start_user -> update user info: user_mod"),
    ok = megaco:update_user_info(Mid, user_mod,  ?MODULE),

    d("megaco_start_user -> update user info: user_args"),
    ok = megaco:update_user_info(Mid, user_args,  [self()]),

    Port = get_config(port,             RecvInfo),
    EM   = get_config(encoding_module,  RecvInfo),
    EC   = get_config(encoding_config,  RecvInfo),
    TM   = get_config(transport_module, RecvInfo),
    RH0  = megaco:user_info(Mid, receive_handle),

    RH1  = RH0#megaco_receive_handle{send_mod        = TM,
				     encoding_mod    = EM,
				     encoding_config = EC},

    State1 = State#state{mid = Mid, recv_handle = RH1, port = Port},
    {ok, State1};

handle_exec(megaco_stop_user, #state{mid = Mid} = State)
  when Mid /= undefined ->
    megaco_cleanup(State),
    ok = megaco:stop_user(Mid),
    {ok, State#state{mid = undefined}};

handle_exec(start_transport, #state{recv_handle = RH} = State) ->
    p("start_transport"),
    #megaco_receive_handle{send_mod = TM} = RH,
    case (catch TM:start_transport()) of
	{ok, Sup} -> 
	    d("start_transport -> Sup: ~p", [Sup]),
	    {ok, State#state{transport_sup = Sup}};
	{error, Reason} ->
	    e("failed starting transport (~w): "
	      "~n   ~p", [TM, Reason]),
	    error({failed_starting_transport, TM, Reason});
	Crap ->
	    e("failed starting transport (~w): "
	      "~n   ~p", [TM, Crap]),
	    error({failed_starting_transport, TM, Crap})
    end;

handle_exec({listen, Opts0, MaybeRetry},
     #state{recv_handle = RH, port = Port, transport_sup = Pid} = State)
  when RH#megaco_receive_handle.send_mod =:= megaco_tcp ->
    p("listen(tcp)", []),
    Opts = [{module,         ?DELIVER_MOD}, 
	    {port,           Port}, 
	    {receive_handle, RH},
	    {tcp_options,    [{nodelay, true}]} | Opts0],
    case (catch handle_exec_listen_tcp(Pid, Opts, MaybeRetry)) of
        ok ->
            {ok, State};
        Else ->
            error({tcp_listen_failed, Opts0, Else})
    end;
handle_exec({listen, Opts0, _MaybeRetry},
     #state{recv_handle = RH, port = Port, transport_sup = Pid} = State)
  when RH#megaco_receive_handle.send_mod =:= megaco_udp ->
    p("listen(udp) - open"),
    Opts = [{module, ?DELIVER_MOD}, {port, Port}, {receive_handle, RH}|Opts0],
    case (catch megaco_udp:open(Pid, Opts)) of
        {ok, _SH, _CtrlPid} ->
            {ok, State};
        Else ->
            error({udp_open, Opts0, Else})
    end;
handle_exec({listen, Opts0, _MaybeRetry},
     #state{recv_handle = RH, port = Port, transport_sup = Pid} = State)
  when RH#megaco_receive_handle.send_mod =:= megaco_test_generic_transport ->
    p("listen(generic)"),
    Opts = [{module, ?DELIVER_MOD}, {port, Port}, {receive_handle, RH}|Opts0],
    case (catch megaco_test_generic_transport:listen(Pid, Opts)) of
        {ok, _SH, _CtrlPid} ->
            {ok, State};
        Else ->
            error({udp_open, Opts0, Else})
    end;

handle_exec({connect, Host, Opts0, MaybeRetry},
     #state{transport_sup = Sup,
	     recv_handle  = RH,
	     port         = Port} = State) 
  when RH#megaco_receive_handle.send_mod =:= megaco_tcp ->
    p("connect[megaco_tcp] to ~p:~p", [Host, Port]),
    PrelMid = preliminary_mid,
    Opts = [{host,           Host}, 
	    {port,           Port}, 
	    {receive_handle, RH},
	    {tcp_options,    [{nodelay, true}]} | Opts0],
    case (catch handle_exec_connect_tcp(Host, Opts, Sup, MaybeRetry)) of
	{ok, SH, ControlPid} ->
	    d("tcp connected: ~p, ~p", [SH, ControlPid]),
	    megaco_connector_start(RH, PrelMid, SH, ControlPid),
	    {ok, State#state{send_handle = SH,
			      ctrl_pid    = ControlPid}};
	Error ->
	    error({tcp_connect_failed, Host, Opts0, Error})
    end;

handle_exec({connect, Host, Opts0, _MaybeRetry},
     #state{transport_sup = Sup,
	     recv_handle   = RH,
	     port          = Port} = State) 
  when RH#megaco_receive_handle.send_mod =:= megaco_udp ->
    p("connect[megaco_udp] to ~p", [Host]),
    PrelMid = preliminary_mid,
    Opts = [{port, 0}, {receive_handle, RH}|Opts0],
    d("udp open", []),
    case (catch megaco_udp:open(Sup, Opts)) of
	{ok, Handle, ControlPid} ->
	    d("udp opened: ~p, ~p", [Handle, ControlPid]),
	    SH = megaco_udp:create_send_handle(Handle, Host, Port),
	    megaco_connector_start(RH, PrelMid, SH, ControlPid),
	    {ok, State#state{send_handle = SH,
			      ctrl_pid    = ControlPid}};
	Error ->
	    error({udp_connect_failed, Host, Opts0, Error})
    end;

handle_exec({connect, Host, Opts0, _MaybeRetry},
     #state{transport_sup = Sup,
	     recv_handle   = RH,
	     port          = Port} = State) 
  when RH#megaco_receive_handle.send_mod =:= megaco_test_generic_transport ->
    p("connect[megaco_test_generic_transport] to ~p", [Host]),
    PrelMid = preliminary_mid,
    Opts = [{host, Host}, {port, Port}, {receive_handle, RH}|Opts0],
    case (catch megaco_test_generic_transport:connect(Sup, Opts)) of
	{ok, SH, ControlPid} ->
	    d("generic connected: ~p, ~p", [SH, ControlPid]),
	    megaco_connector_start(RH, PrelMid, SH, ControlPid),
	    {ok, State#state{send_handle = SH,
			      ctrl_pid    = ControlPid}};
	Error ->
	    error({generic_connect_failed, Host, Opts0, Error})
    end;

handle_exec(megaco_connect, State) ->
    p("megaco_connect"),
    receive
        {megaco_connect_result, {ok, CH}} ->
            p("megaco connect succeeded: ~p", [CH]),
            {ok, State#state{conn_handle = CH}};
        {megaco_connect_result, Error} ->
            p("megaco connect failed: ~p", [Error]),
            #state{result = Res} = State,
            {ok, State#state{result = [Error|Res]}}
    end;

handle_exec({megaco_connect, Mid}, 
	    #state{recv_handle = RH,
		   send_handle = SH,
		   ctrl_pid    = ControlPid} = State) ->
    p("megaco_connect: ~p", [Mid]),
    megaco_connector_start(RH, Mid, SH, ControlPid),
    {ok, State};

handle_exec({megaco_user_info, Tag}, #state{mid = Mid, result = Res} = State)
  when Mid /= undefined ->
    p("megaco_user_info: ~w", [Tag]),
    Val = (catch megaco:user_info(Mid, Tag)),
    d("megaco_user_info: ~p", [Val]),
    {ok, State#state{result = [Val|Res]}};

handle_exec({megaco_update_user_info, Tag, Val}, #state{mid = Mid} = State)
  when Mid /= undefined ->
    p("megaco_update_user_info: ~w -> ~p", [Tag, Val]),
    ok = megaco:update_user_info(Mid, Tag, Val),
    {ok, State};

handle_exec({megaco_conn_info, Tag},
     #state{conn_handle = CH, result = Res} = State)
  when CH /= undefined ->
    p("megaco_conn_info: ~w", [Tag]),
    Val = (catch megaco:conn_info(CH, Tag)),
    d("megaco_conn_info: ~p", [Val]),
    {ok, State#state{result = [Val|Res]}};

handle_exec({megaco_update_conn_info, Tag, Val},
     #state{conn_handle = CH} = State)
  when CH /= undefined ->
    p("megaco_update_conn_info: ~w -> ~p", [Tag, Val]),
    case megaco:update_conn_info(CH, Tag, Val) of
        ok ->
            {ok, State};
        Error ->
            error({failed_updating_conn_info, Tag, Val, Error})
    end;

handle_exec(megaco_info, #state{result = Res} = State) ->
    p("megaco_info", []),
    Val = (catch megaco:info()),
    d("megaco_info: ~p", [Val]),
    {ok, State#state{result = [Val|Res]}};

handle_exec({megaco_system_info, Tag, Verify}, #state{result = Res} = State) ->
    p("megaco_system_info: ~w", [Tag]),
    Val = (catch megaco:system_info(Tag)),
    d("megaco_system_info: ~p", [Val]),
    case Verify(Val) of
	ok ->
	    {ok, State#state{result = [Val|Res]}};
	Error ->
	    {error, State#state{result = [Error|Res]}}
    end;

%% This is either a MG or a MGC which is only connected to one MG
handle_exec({megaco_call, ARs, Opts}, #state{conn_handle = CH} = State)
  when CH /= undefined ->
    p("megaco_call"),
    {_PV, UserReply} = megaco:call(CH, ARs, Opts),
    d("megaco_call -> UserReply: ~n~p", [UserReply]),
    {ok, State};

handle_exec({megaco_call, RemoteMid, ARs, Opts}, #state{mid = Mid} = State) ->
    p("megaco_call: ~p", [RemoteMid]),
    %% First we have to find the CH for this Mid
    Conns = megaco:user_info(Mid, connections),
    {value, {_, CH}} =
        lists:keysearch(RemoteMid, #megaco_conn_handle.remote_mid, Conns),
    {_PV, UserReply} = megaco:call(CH, ARs, Opts),
    d("megaco_call -> UserReply: ~n~p", [UserReply]),
    {ok, State};

%% This is either a MG or a MGC which is only connected to one MG
handle_exec({megaco_cast, ARs, Opts}, #state{conn_handle = CH} = State)
  when CH =/= undefined ->
    p("megaco_cast"),
    case megaco:cast(CH, ARs, Opts) of
        ok ->
            {ok, State};
        Error ->
            d("failed sending (cast) message: ~n~p", [Error]),
            #state{result = Acc} = State,
            {error, State#state{result = [Error|Acc]}}
    end;

handle_exec({megaco_cast, RemoteMid, ARs, Opts}, #state{mid = Mid} = State) ->
    p("megaco_cast: ~p", [RemoteMid]),
    %% First we have to find the CH for this Mid
    Conns = megaco:user_info(Mid, connections),
    {value, {_, CH}} =
        lists:keysearch(RemoteMid, #megaco_conn_handle.remote_mid, Conns),
    case megaco:cast(CH, ARs, Opts) of
        ok ->
            {ok, State};
        Error ->
            d("failed sending (cast) message: ~n~p", [Error]),
            #state{result = Acc} = State,
            {error, State#state{result = [Error|Acc]}}
    end;

%% Nothing shall happen for atleast Timeout time
handle_exec({megaco_callback, nocall, Timeout}, State) ->
    p("megaco_callback [~w,~w]", [nocall, Timeout]),
    receive
        {handle_megaco_callback, Type, Msg, Pid} ->
            d("received unexpected megaco callback: ~n~p", [Msg]),
            #state{result = Res} = State,
            Err = {unexpected_callback, Type, Msg, Pid},
            {error, State#state{result = [Err|Res]}}
    after Timeout ->
            {ok, State}
    end;

handle_exec({megaco_callback, Tag, Verify}, State) when is_function(Verify) ->
    p("megaco_callback [~w]", [Tag]),
    receive
        {handle_megaco_callback, Type, Msg, Pid} ->
            d("received megaco callback: ~n~p", [Msg]),
            case Verify(Msg) of
                {VRes, Res, Reply} ->
                    d("megaco_callback [~w] ~w",[Tag, VRes]),
                    handle_megaco_callback_reply(Pid, Type, Reply),
                    validate(VRes, Tag, Res, State);
                {VRes, Delay, Res, Reply} ->
                    d("megaco_callback [~w] ~w, ~w",[Tag,Delay,VRes]),
                    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
                    validate(VRes, Tag, Res, State)
            end
    end;

handle_exec({megaco_callback, Tag, {VMod, VFunc, VArgs}}, State)
  when is_atom(VMod) andalso is_atom(VFunc) andalso is_list(VArgs) ->
    p("megaco_callback [~w]", [Tag]),
    receive
        {handle_megaco_callback, Type, Msg, Pid} ->
            d("received megaco callback: ~n~p"
              "~n   VMod:  ~w"
              "~n   VFunc: ~w"
              "~n   VArgs: ~p", [Msg, VMod, VFunc, VArgs]),
            case apply(VMod, VFunc, [Msg|VArgs]) of
                {VRes, Res, Reply} ->
                    d("megaco_callback [~w] ~w",[Tag, VRes]),
                    handle_megaco_callback_reply(Pid, Type, Reply),
                    validate(VRes, Tag, Res, State);
                {VRes, Delay, Res, Reply} ->
                    d("megaco_callback [~w] ~w, ~w",[Tag,Delay,VRes]),
                    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
                    validate(VRes, Tag, Res, State)
            end
    end;

handle_exec({megaco_callback, Tag, Verify, Timeout}, State)
  when (is_function(Verify) andalso 
	(is_integer(Timeout) andalso (Timeout > 0))) ->
    p("megaco_callback [~w]", [Tag]),
    receive
        {handle_megaco_callback, Type, Msg, Pid} ->
            d("received megaco callback: ~n~p", [Msg]),
            case Verify(Msg) of
                {VRes, Res, Reply} ->
                    d("megaco_callback [~w] ~w",[Tag,VRes]),
                    handle_megaco_callback_reply(Pid, Type, Reply),
                    validate(VRes, Tag, Res, State);
                {VRes, Delay, Res, Reply} ->
                    d("megaco_callback [~w] ~w, ~w",[Tag,Delay,VRes]),
                    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
                    validate(VRes, Tag, Res, State)
            end
    after Timeout ->
            #state{result = Res} = State,
            Err = {callback_timeout, Tag, Timeout},
            {error, State#state{result = [Err|Res]}}
    end;

handle_exec({megaco_callback, Verifiers}, State) ->
    p("megaco_callback"),
    megaco_callback_verify(Verifiers, State);

handle_exec({megaco_cancel, Reason}, #state{conn_handle = CH} = State) ->
    p("megaco_cancel [~w]", [Reason]),
    case megaco:cancel(CH, Reason) of
        ok ->
            {ok, State};
        Error ->
            d("failed cancel: ~n~p", [Error]),
            #state{result = Acc} = State,
            {error, State#state{result = [Error|Acc]}}
    end;

handle_exec({trigger, Trigger}, State) when is_function(Trigger) ->
    p("trigger"),
    (catch Trigger()),
    {ok, State};

handle_exec({sleep, To}, State) ->
    p("sleep ~p", [To]),
    megaco_test_generator:sleep(To),
    {ok, State};

handle_exec(BadInstruction, _State) ->
    error({invalid_instruction, BadInstruction}).


%% --- cleanup ---

megaco_cleanup(#state{mid = Mid}) ->
    Close = fun(CH) -> do_megaco_cleanup(CH) end,
    Conns = 
	case (catch megaco:user_info(Mid, connections)) of
	    Connections when is_list(Connections) ->
		Connections;
	    _ ->
		[]
	end,
    lists:foreach(Close, Conns).

do_megaco_cleanup(CH) ->
    case (catch do_megaco_cleanup2(CH)) of
        ok ->
            ok;
        {'EXIT', {no_such_connection, _}} ->
            ok;
        {'EXIT', Reason} ->
            exit(Reason)
    end.

do_megaco_cleanup2(CH) ->
    d("do_megaco_cleanup2 -> entry with"
      "~n   CH: ~p", [CH]),
    Reason     = {stopped_by_user,self()},
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    d("do_megaco_cleanup2 -> disconnect"),
    megaco:disconnect(CH, Reason),
    d("do_megaco_cleanup2 -> disconnected, now cancel"),
    megaco:cancel(CH, Reason),
    d("do_megaco_cleanup2 -> canceled, now close"),
    case SendMod of
        megaco_tcp -> (catch megaco_tcp:close(SendHandle));
        megaco_udp -> (catch megaco_udp:close(SendHandle));
        SendMod    -> exit(Pid, Reason)
    end,
    ok.


%% --- connector ---

megaco_connector_start(RH, PrelMid, SH, ControlPid) ->
    Self = self(), 
    Fun  = fun() -> megaco_connect(RH, PrelMid, SH, ControlPid, Self) end,
    erlang:spawn_opt(Fun, [link]).

megaco_connect(RH, PrelMid, SH, ControlPid, Parent) ->
    Result = megaco:connect(RH, PrelMid, SH, ControlPid),
    Parent ! {megaco_connect_result, Result},
    exit(normal).


%% --- megaco callback verify ---

%% This is used when a number of callback's is expected, but where
%% the specific order is unknown.
megaco_callback_verify([], State) ->
    d("megaco_callback_verify -> done"),
    {ok, State};
megaco_callback_verify(Verifiers0, State0) ->
    d("megaco_callback_verify -> entry when"
      "~n   length(Verifiers0): ~w", [length(Verifiers0)]),
    receive
        {handle_megaco_callback, Type, Msg, Pid} ->
            d("megaco_callback_verify -> received megaco callback: ~w"
	      "~n   Msg: ~p", [Type, Msg]),
            case megaco_callback_verify(Verifiers0, Type, Msg, Pid, State0) of
                {ok, Verifiers, State} ->
                    megaco_callback_verify(Verifiers, State);
                Error ->
                    Error
            end
    end.

megaco_callback_verify(Verifiers0, Type, Msg, Pid, State0) ->
    d("megaco_callback_verify -> entry"),
    Tag = element(1, Msg),
    d("megaco_callback_verify -> Tag: ~w", [Tag]),
    case lists:keysearch(Tag, 1, Verifiers0) of
        {value, {Tag, N, Verify}} when (N > 0) andalso is_function(Verify) ->
            d("megaco_callback_verify -> N: ~w",[N]),
            case Verify(Msg) of
                {VRes, Res, Reply} ->
                    d("megaco_callback_verify -> VRes: ~w",[VRes]),
                    handle_megaco_callback_reply(Pid, Type, Reply),
                    case validate(VRes, Tag, Res, State0) of
                        {error, _} = EState ->
                            e("megaco_callback_verify -> (1) error", []),
                            throw(EState);
                        {ok, State} when N > 1 ->
                            d("megaco_callback_verify -> (1) validated"),
                            Rec = {Tag, N-1, Verify},
                            Verifiers =
                                lists:keyreplace(Tag, 1, Verifiers0, Rec),
                            {ok, Verifiers, State};
                        {ok, State} ->
                            d("megaco_callback_verify -> (2) validated"),
                            Verifiers = lists:keydelete(Tag, 1, Verifiers0),
                            {ok, Verifiers, State}
                    end;
                {VRes, Delay, Res, Reply} ->
                    d("megaco_callback_verify -> Delay: ~w, VRes: ~w",
                      [Delay,VRes]),
                    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
                    case validate(VRes, Tag, Res, State0) of
                        {error, _} = EState ->
                            e("megaco_callback_verify -> (2) error", []),
                            throw(EState);
                        {ok, State} when N > 1 ->
                            d("megaco_callback_verify -> (3) validated"),
                            Rec = {Tag, N-1, Verify},
                            Verifiers =
                                lists:keyreplace(Tag, 1, Verifiers0, Rec),
                            {ok, Verifiers, State};
                        {ok, State} ->
                            d("megaco_callback_verify -> (4) validated"),
                            Verifiers = lists:keydelete(Tag, 1, Verifiers0),
                            {ok, Verifiers, State}
                    end
            end;
        false ->
            e("megaco_callback_verify -> no such tag ~w~n~p",
              [Tag, Verifiers0]),
            #state{result = Res} = State0,
            State = State0#state{result = [{Type, error, Msg}|Res]},
            error(State)
    end.


%% --- validate verify result ---

validate(ok, handle_connect = Tag, CH, #state{result = Acc} = S) ->
    {ok, S#state{conn_handle = CH, result = [{Tag, ok, CH}|Acc]}};
validate(ok, Tag, Res, #state{result = Acc} = S) ->
    {ok, S#state{result = [{Tag, ok, Res}|Acc]}};
validate(error, Tag, Res, #state{result = Acc} = S) ->
    {error, S#state{result = [{Tag, error, Res}|Acc]}}.


%% ----- termination -----

terminate(normal, #state{result = Result} = _State) ->
    d("terminate -> entry when normal with"
      "~n   Result: ~p", [Result]),
    %% megaco_cleanup(State),
    {ok, Result};

terminate(Reason, #state{result = Result} = State) ->
    d("terminate -> entry with"
      "~n   Reason: ~p"
      "~n   Result: ~p", [Reason, Result]),
    megaco_cleanup(State),
    {error, {Reason, Result}}.


%%----------------------------------------------------------------------

handle_exec_listen_tcp(Sup, Opts, MaybeRetry) ->
    handle_exec_listen_tcp(Sup, Opts, MaybeRetry, noError).

handle_exec_listen_tcp(Sup, Opts, MaybeRetry, Error0) ->
    case (catch megaco_tcp:listen(Sup, Opts)) of
        ok ->
            ok;
	Error1 ->
	    case (catch MaybeRetry(Error1, Error0)) of
		{true, Error2} ->
		    handle_exec_listen_tcp(Sup, Opts, MaybeRetry, Error2);
		{false, Error3} ->
		    {error, Error3}
	    end
    end.
	    

handle_exec_connect_tcp(Host, Opts, Sup, MaybeRetry) 
  when is_function(MaybeRetry) ->
    handle_exec_connect_tcp(Host, Opts, Sup, MaybeRetry, noError).

handle_exec_connect_tcp(Host, Opts, Sup, MaybeRetry, Error0) ->
    case (catch megaco_tcp:connect(Sup, Opts)) of
	{ok, SH, ControlPid} ->
	    d("tcp connected: ~p, ~p", [SH, ControlPid]),
	    {ok, SH, ControlPid};
	Error1 ->
	    case (catch MaybeRetry(Error1, Error0)) of
		{true, Error2} ->
		    handle_exec_connect_tcp(Host, Opts, Sup, 
					    MaybeRetry, Error2);
		{false, Error3} ->
		    {error, Error3}
	    end
    end.
    


%%----------------------------------------------------------------------
%% megaco_user callback functions
%%----------------------------------------------------------------------

handle_connect(CH, PV, P) ->
    Req = {handle_connect, CH, PV},
    handle_megaco_callback_call(P, Req).

handle_connect(CH, PV, Extra, P) ->
    Req = {handle_connect, CH, PV, Extra},
    handle_megaco_callback_call(P, Req).

handle_disconnect(CH, PV, R, P) ->
    Msg   = {handle_disconnect, CH, PV, R},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_syntax_error(RH, PV, ED, P) ->
    Req = {handle_syntax_error, RH, PV, ED},
    handle_megaco_callback_call(P, Req).

handle_syntax_error(RH, PV, ED, Extra, P) ->
    Req = {handle_syntax_error, RH, PV, ED, Extra},
    handle_megaco_callback_call(P, Req).

handle_message_error(CH, PV, ED, P) ->
    Msg   = {handle_message_error, CH, PV, ED},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_message_error(CH, PV, ED, Extra, P) ->
    Msg   = {handle_message_error, CH, PV, ED, Extra},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_request(CH, PV, AR, P) ->
    Req = {handle_trans_request, CH, PV, AR},
    handle_megaco_callback_call(P, Req).

handle_trans_request(CH, PV, AR, Extra, P) ->
    Req = {handle_trans_request, CH, PV, AR, Extra},
    handle_megaco_callback_call(P, Req).

handle_trans_long_request(CH, PV, RD, P) ->
    Req = {handle_trans_long_request, CH, PV, RD},
    handle_megaco_callback_call(P, Req).

handle_trans_long_request(CH, PV, RD, Extra, P) ->
    Req = {handle_trans_long_request, CH, PV, RD, Extra},
    handle_megaco_callback_call(P, Req).

handle_trans_reply(CH, PV, AR, RD, P) ->
    Msg = {handle_trans_reply, CH, PV, AR, RD},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_reply(CH, PV, AR, RD, Extra, P) ->
    Msg = {handle_trans_reply, CH, PV, AR, RD, Extra},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_ack(CH, PV, AS, AD, P) ->
    Msg = {handle_trans_ack, CH, PV, AS, AD},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_ack(CH, PV, AS, AD, Extra, P) ->
    Msg = {handle_trans_ack, CH, PV, AS, AD, Extra},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_unexpected_trans(CH, PV, T, P) ->
    Msg = {handle_unexpected_trans, CH, PV, T},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_unexpected_trans(CH, PV, T, Extra, P) ->
    Msg = {handle_unexpected_trans, CH, PV, T, Extra},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_request_abort(RH, PV, TransNo, Pid, P) ->
    Msg = {handle_trans_request_abort, RH, PV, TransNo, Pid},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_request_abort(RH, PV, TransNo, Pid, Extra, P) ->
    Msg = {handle_trans_request_abort, RH, PV, TransNo, Pid, Extra},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_megaco_callback_cast(P, Msg, Reply) ->
    p("handle_megaco_callback_cast -> entry with Msg: ~n~p", [Msg]),
    P ! {handle_megaco_callback, cast, Msg, self()},
    Reply.

handle_megaco_callback_call(P, Msg) ->
    p("handle_megaco_callback_call -> entry with"
      "~n   P:   ~p"
      "~n   Msg: ~p", [P, Msg]),
    P ! {handle_megaco_callback, call, Msg, self()},
    receive
        {handle_megaco_callback_reply, Reply} ->
            p("handle_megaco_callback_call -> received reply: ~n~p", [Reply]),
            Reply;
        {handle_megaco_callback_reply, Delay, Reply} when is_integer(Delay) ->
            p("handle_megaco_callback_call -> "
              "received reply [~w]: "
              "~n   ~p", [Delay, Reply]),
            sleep(Delay),
            p("handle_megaco_callback_call -> deliver reply after delay [~w]",
              [Delay]),
            Reply;
        {'EXIT', SomePid, SomeReason} ->
            p("handle_megaco_callback_call -> "
              "received unexpected EXIT signal: "
              "~n   SomePid:    ~p"
              "~n   SomeReason: ~p", [SomePid, SomeReason]),
            exit({unexpected_EXIT_signal, SomePid, SomeReason})
    end.


handle_megaco_callback_reply(P, call, Reply) ->
    P ! {handle_megaco_callback_reply, Reply};
handle_megaco_callback_reply(_, _, _) ->
    ok.

handle_megaco_callback_reply(P, call, Delay, Reply) ->
    P ! {handle_megaco_callback_reply, Delay, Reply};
handle_megaco_callback_reply(_, _, _, _) ->
    ok.


%%----------------------------------------------------------------------
%% internal utility functions
%%----------------------------------------------------------------------

random_init() ->
    {A,B,C} = now(),
    random:seed(A,B,C).

random(N) ->
    random:uniform(N).


get_config(Key, Opts) ->
    {value, {Key, Val}} = lists:keysearch(Key, 1, Opts),
    Val.

sleep(X) -> megaco_test_generator:sleep(X).

d(F)    -> megaco_test_generator:debug(F).
d(F, A) -> megaco_test_generator:debug(F, A).

e(F, A) -> megaco_test_generator:error(F, A).

p(F      ) -> p("", F, []).
p(F,    A) -> p("", F, A).
p(P, F, A) -> megaco_test_generator:print(P,    F, A).

error(Reason) ->
    throw({error, Reason}).

