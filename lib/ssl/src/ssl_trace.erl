%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2023. All Rights Reserved.
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

%%% Purpose:
%%%  This module implements support for using the Erlang trace in a simple way
%%%  for ssl tracing.
%%%
%%%  Begin the session with ssl_trace:start(). This will do a dbg:start()
%%%  if needed and  then dbg:p/2 to set some flags.
%%%
%%%  Next select trace profiles to activate: for example plain text
%%%  printouts of messages sent or received. This is switched on and off with
%%%  ssl_trace:on(TraceProfile(s)) and ssl_trace:off(TraceProfile(s)).
%%%  For example:
%%%
%%%      ssl_trace:on(rle)            -- switch on printing role traces
%%%      ssl_trace:on([api, rle])     -- switch on printing role and api traces
%%%      ssl_trace:on()               -- switch on all ssl trace profiles
%%%
%%%  To switch, use the off/0 or off/1 function in the same way, for example:
%%%
%%%      ssl_trace:off(api)           -- switch off api tracing, keep all other
%%%      ssl_trace:off()              -- switch off all ssl tracing
%%%
%%%  Present the trace result with some other method than the default
%%%  io:format/2:
%%%      ssl_trace:start(fun(Format,Args) ->
%%%                        my_special( io_lib:format(Format,Args) )
%%%                      end)
%%%  Write traces to text file with budget of 1000 trace entries:
%%%      ssl_trace:start(IoFmt, [file, {budget, 1000}])
%%%
-module(ssl_trace).

-export([start/0, start/1, start/2, stop/0, on/0,  on/1, off/0, off/1, is_on/0,
         is_off/0, write/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% Internal apply_after:
-export([ets_delete/2]).
%% Test purpose
-export([trace_profiles/0]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(CALL_TIMEOUT, 15000). % 3x the default
-define(TRACE_BUDGET, 10000).
-define(TRACE_FILE, "ssl_trace.txt").

-record(state, {
                file = undefined,
                types_on = [],
                io_device = undefined,
                write_fun
               }).

%%%----------------------------------------------------------------
start() -> start(fun io:format/2).

start(file) ->
    start(fun io:format/2, [file]);
start(IoFmtFun) when is_function(IoFmtFun,2) ; is_function(IoFmtFun,3) ->
    start(IoFmtFun, []).

start(IoFmtFun, TraceOpts) when is_function(IoFmtFun,2);
                                is_function(IoFmtFun,3);
                                is_list(TraceOpts) ->
    WriteFun = fun(F,A,S) -> IoFmtFun(F,A), S end,
    {ok, Pid} = gen_server:start({local,?SERVER}, ?MODULE,
                                 [{write_fun, WriteFun}, TraceOpts], []),
    true = is_process_alive(Pid),
    catch dbg:start(),
    start_tracer(IoFmtFun, TraceOpts),
    dbg:p(all, [timestamp, c]),
    {ok, get_all_trace_profiles()}.

stop() ->
    try
        dbg:stop(),
        ok = gen_server:call(?SERVER, file_close, ?CALL_TIMEOUT),
        gen_server:stop(?SERVER)
    catch
        _:_ -> ok
    end.

on() ->
    on(get_all_trace_profiles()).

on(Type) ->
    switch(on, Type).

off() ->
    off(get_all_trace_profiles()).

off(Type) ->
    switch(off, Type).

is_on() ->
    gen_server:call(?SERVER, get_on, ?CALL_TIMEOUT).

is_off() ->
    get_all_trace_profiles() -- is_on().

write(Fmt, Args) ->
    gen_server:call(?SERVER, {write, Fmt, Args}, ?CALL_TIMEOUT).

%%%----------------------------------------------------------------
init(Args) ->
    try
        ets:new(?MODULE, [public, named_table])
    catch
        exit:badarg ->
            ok
    end,
    {ok, #state{write_fun = proplists:get_value(write_fun, Args)}}.

handle_call({switch,on,Profiles}, _From, State) ->
    [enable_profile(P) || P <- Profiles],
    NowOn = lists:usort(Profiles ++ State#state.types_on),
    {reply, {ok,NowOn}, State#state{types_on = NowOn}};
handle_call({switch,off,Profiles}, _From, State) ->
    StillOn = State#state.types_on -- Profiles,
    [disable_profile(P) || P <- Profiles],
    {reply, {ok,StillOn}, State#state{types_on = StillOn}};
handle_call(get_on, _From, State) ->
    {reply, State#state.types_on, State};
handle_call({file_open, File}, _From, State) ->
    {ok, IODevice} = file:open(File, [write]),
    {reply, {ok, IODevice}, State#state{io_device = IODevice}};
handle_call(file_close, _From, #state{io_device = IODevice} = State) ->
    case is_pid(IODevice) of
        true ->
            ok = file:close(IODevice);
        _ ->
            ok
    end,
    {reply, ok, State#state{io_device = undefined}};
handle_call({write, Fmt, Args}, _From, State) ->
    #state{io_device = IODevice, write_fun = WriteFun0} = State,
    WriteFun = get_write_fun(IODevice, WriteFun0),
    WriteFun(Fmt, Args, processed),
    {reply, ok, State};
handle_call(C, _From, State) ->
    io:format('*** Unknown call: ~p~n',[C]),
    {reply, {error,{unknown_call,C}}, State}.

handle_cast({new_proc,Pid}, State) ->
    monitor(process, Pid),
    {noreply, State};
handle_cast(C, State) ->
    io:format('*** Unknown cast: ~p~n',[C]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    %% Universal real-time synchronization (there might be dbg msgs in the queue to the tracer):
    timer:apply_after(20000, ?MODULE, ets_delete, [?MODULE, Pid]),
    {noreply, State};
handle_info(C, State) ->
    io:format('*** Unknown info: ~p~n',[C]),
    {noreply, State}.

%%%----------------------------------------------------------------
get_proc_stack(Pid) when is_pid(Pid) ->
    try ets:lookup_element(?MODULE, Pid, 2)
    catch
        error:badarg ->
            %% Non-existing item
            new_proc(Pid),
            ets:insert(?MODULE, {Pid,[]}),
            []
    end.

new_proc(Pid) when is_pid(Pid) ->
    gen_server:cast(?SERVER, {new_proc,Pid}).

put_proc_stack(Pid, Stack) when is_pid(Pid),
                               is_list(Stack) ->
    ets:insert(?MODULE, {Pid, Stack}).

ets_delete(Tab, Key) ->
    catch ets:delete(Tab, Key).

start_tracer(WriteFun, TraceOpts) when is_function(WriteFun,2) ->
    start_tracer(fun(F,A,S) -> WriteFun(F,A), S end, TraceOpts);
start_tracer(WriteFun, TraceOpts) when is_function(WriteFun,3) ->
    Acc0 = [{budget, proplists:get_value(budget, TraceOpts, ?TRACE_BUDGET)}],
    Acc1 = case lists:member(file, TraceOpts) of
               true ->
                   TraceFile =
                       case init:get_argument(ssl_trace_file) of
                           {ok, [[Path]]} -> Path;
                           _ -> ?TRACE_FILE
                       end,
                   [{file, TraceFile} | Acc0];
               _ ->
                   Acc0
           end,
    start_dbg_tracer(WriteFun, Acc1).

start_dbg_tracer(WriteFun, InitHandlerAcc0) when is_function(WriteFun, 3) ->
    Handler =
        fun(Arg, Acc0) ->
                try_handle_trace(gen_server:call(?SERVER, get_on, ?CALL_TIMEOUT),
                                             Arg, WriteFun,
                                             Acc0)
        end,
    InitHandlerAcc1 =
        case proplists:get_value(file, InitHandlerAcc0) of
            undefined ->
                InitHandlerAcc0;
            File ->
                {ok, IODevice} = gen_server:call(?SERVER, {file_open, File}, ?CALL_TIMEOUT),
                [{io_device, IODevice} | InitHandlerAcc0]
        end,
    dbg:tracer(process, {Handler,InitHandlerAcc1}).

try_handle_trace(ProfilesOn, Arg, WriteFun0, HandlerAcc) ->
    IODevice = proplists:get_value(io_device, HandlerAcc),
    WriteFun = get_write_fun(IODevice, WriteFun0),
    Budget0 = proplists:get_value(budget, HandlerAcc, 0),
    Timestamp = trace_ts(Arg),
    Pid = trace_pid(Arg),
    TraceInfo = trace_info(Arg),
    Module = trace_module(TraceInfo),
    ProcessStack = get_proc_stack(Pid),
    Role = proplists:get_value(role, ProcessStack, '?'),
    Budget1 =
        lists:foldl(
          fun(Profile, BAcc) ->
                  case BAcc > 1 of
                      true ->
                          try
                              Module:handle_trace(Profile, TraceInfo, ProcessStack)
                          of
                              {skip, NewProcessStack} ->
                                  %% Don't try to process this later
                                  put_proc_stack(Pid, NewProcessStack),
                                  reduce_budget(BAcc, WriteFun);
                              {Txt, NewProcessStack} when is_list(Txt) ->
                                  put_proc_stack(Pid, NewProcessStack),
                                  write_txt(WriteFun, Timestamp, Pid,
                                            common_prefix(TraceInfo, Role,
                                                          Profile) ++ Txt),
                                  reduce_budget(BAcc, WriteFun)
                          catch
                              _:_ ->
                                  %% not processed by custom handler
                                  BAcc
                          end;
                      _ ->
                          BAcc
                  end
          end, Budget0, ProfilesOn),
    %% generate default trace if was not processed by any custom handler
    Budget2 =
        case (Budget1 == Budget0 andalso Budget0 > 0) of
            true ->
                WriteFun("~.100s ~W~n",
                         [io_lib:format("~s ~p ~s ",
                                        [lists:flatten(Timestamp),Pid,
                                         common_prefix(TraceInfo, Role,
                                                       "   ")]),
                          TraceInfo, 7], processed),
                reduce_budget(Budget0, WriteFun);
            _ ->
                Budget1
        end,
    [{budget, Budget2} | proplists:delete(budget, HandlerAcc)].

get_write_fun(IODevice, WriteFun0) ->
    case is_pid(IODevice) of
        true ->
            fun(Format, Args, Return) ->
                    ok = io:format(IODevice, Format, Args),
                    Return
            end;
        false ->
            WriteFun0
    end.

reduce_budget(B, _) when B > 1 ->
    B - 1;
reduce_budget(_, WriteFun) ->
    case get(no_budget_msg_written) of
        undefined ->
            WriteFun("No more trace budget!~n", [], processed),
            put(no_budget_msg_written, true);
        _ ->
            ok
    end,
    0.

write_txt(WriteFun, Timestamp, Pid, Txt) when is_list(Txt) ->
    WriteFun("~s ~p ~ts~n", [Timestamp, Pid, Txt], processed).

get_all_trace_profiles() ->
    Unsorted = [Profile ||
                   {Profile, _TraceOn, _TraceOff, _TracedFuns}
                       <- trace_profiles()],
    lists:usort(Unsorted).

switch(X, Profile) when is_atom(Profile); is_tuple(Profile) ->
    switch(X, [Profile]);
switch(X, Profiles) when is_list(Profiles) ->
    case whereis(?SERVER) of
        undefined ->
            start();
        _ ->
            ok
    end,
    case unknown_types(Profiles, get_all_trace_profiles(), []) of
        [] ->
            gen_server:call(?SERVER, {switch,X,Profiles}, ?CALL_TIMEOUT);
        L ->
            {error, {unknown, L}}
    end.

unknown_types([], _AllProfiles, Acc) -> Acc;
unknown_types([Profile | Tail], AllProfiles, Acc)
  when is_atom(Profile) ->
    case lists:member(Profile, AllProfiles) of
        false -> unknown_types(Tail, AllProfiles, [Profile | Acc]);
        _ -> unknown_types(Tail, AllProfiles, Acc)
    end;
unknown_types([ModProfile = {_Mod, Profile} | Tail], AllProfiles, Acc)
  when is_tuple(ModProfile) ->
    unknown_types([Profile | Tail], AllProfiles, Acc).

%%%----------------------------------------------------------------
%%% Format of trace messages are described in reference manual for erlang:trace/4
%%%   {call,MFA}
%%%   {return_from,{M,F,N},Result}
%%%   {send,Msg,To}
%%%   {'receive',Msg}

%% Pick 2nd element, the Pid
trace_pid(T) when element(1,T)==trace
                  ; element(1,T)==trace_ts ->
    element(2,T).

%% Pick last element, the Time Stamp, and format it
trace_ts(T) when  element(1,T)==trace_ts ->
    ts( element(tuple_size(T), T) ).

ts({_,_,Usec}=Now) when is_integer(Usec) ->
    {_Date,{HH,MM,SS}} = calendar:now_to_local_time(Now),
    io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.6.0w",[HH,MM,SS,Usec]);
ts(_) ->
    "-".

%% Make a tuple of all elements but the 1st, 2nd and last
trace_info(T) ->
    case tuple_to_list(T) of
        [trace,_Pid | Info] -> list_to_tuple(Info);
        [trace_ts,_Pid | InfoTS] -> list_to_tuple(
                                      lists:droplast(InfoTS))
    end.

trace_module(Info) ->
    {Module, _, _} = element(2, Info),
    Module.

common_prefix({call, {M, F, Args}}, Role, Profile) ->
    [io_lib:format("~s (~w) -> ~w:~w/~w ",
                   [Profile, Role, M, F, length(Args)])];
common_prefix({return_from, {M, F, Arity}, _Return}, Role, Profile) ->
    [io_lib:format("~s (~w) <- ~w:~w/~w returned ",
                   [Profile, Role, M, F, Arity])];
common_prefix({exception_from, {M, F, Arity}, Reason}, Role, Profile) ->
    [io_lib:format("~s (~w) exception_from ~w:~w/~w  ~w",
                   [Profile, Role, M, F, Arity, Reason])];
common_prefix(_E, _Role, _Profile) ->
    [].

enable_profile(Profile) when is_atom(Profile) ->
    [enable_profile({M, Profile}) || M <- modules(Profile)];
enable_profile({Module, Profile}) when is_atom(Module); is_atom(Profile) ->
    {Profile, TraceOn, _, AllFuns} = profile(Profile),
    Funs = proplists:get_value(Module, AllFuns),
    process_profile(Module, TraceOn, Funs).

disable_profile(Profile) when is_atom(Profile) ->
    [disable_profile({M, Profile}) || M <- modules(Profile)];
disable_profile({Module, Profile}) when is_atom(Module); is_atom(Profile) ->
    {Profile, _, TraceOff, AllFuns} = profile(Profile),
    Funs = proplists:get_value(Module, AllFuns),
    process_profile(Module, TraceOff, Funs).

process_profile(Module, Action, Funs) when is_atom(Module) ->
    [Action(Module, F, A) || {F, A} <- Funs].

profile(P) ->
    lists:keyfind(P, 1, trace_profiles()).

modules(P) ->
    {_, _, _, Funs} = profile(P),
    proplists:get_keys(Funs).

trace_profiles() ->
    [{api,
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{ssl,
        [{listen,2}, {connect,3}, {handshake,2}, {close, 1}]},
       {ssl_gen_statem,
        [{initial_hello,3}, {connect, 8}, {close, 2}, {terminate_alert, 1}]},
       {tls_gen_connection,
        [{start_connection_tree, 5}, {socket_control, 6}]}
      ]},
     {csp, %% OCSP
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{ssl_handshake, [{maybe_add_certificate_status_request, 4},
                        {client_hello_extensions, 10}, {cert_status_check, 5},
                        {get_ocsp_responder_list, 1}, {handle_ocsp_extension, 2},
                        {path_validation, 10},
                        {handle_server_hello_extensions, 10},
                        {handle_client_hello_extensions, 10},
                        {cert_status_check, 5}]},
       {public_key, [{ocsp_extensions, 1}, {pkix_ocsp_validate, 5},
                     {ocsp_responder_id, 1}, {otp_cert, 1}]},
       {pubkey_ocsp, [{find_responder_cert, 2}, {do_verify_ocsp_signature, 4},
                      {verify_ocsp_response, 3}, {verify_ocsp_nonce, 2},
                      {verify_ocsp_signature, 5}, {do_verify_ocsp_response, 3},
                      {is_responder, 2}, {find_single_response, 3},
                      {ocsp_status, 1}, {match_single_response, 4}]},
       {ssl, [{opt_ocsp, 3}]},
       {ssl_certificate, [{verify_cert_extensions, 4}]},
       {ssl_test_lib, [{init_openssl_server, 3}, {openssl_server_loop, 3}]},
       {tls_connection, [{wait_ocsp_stapling, 3}]},
       {dtls_connection, [{initial_hello, 3}, {hello, 3}, {connection, 3}]},
       {tls_dtls_connection, [{wait_ocsp_stapling, 3}, {certify, 3}]},
       {tls_handshake, [{ocsp_nonce, 1}, {ocsp_expect, 1}, {client_hello, 11}]},
       {dtls_handshake, [{client_hello, 8}]}]},
     {crt, %% certificates
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{public_key, [{pkix_path_validation, 3}, {path_validation, 2},
                     {pkix_decode_cert, 2}]},
       {ssl_certificate, [{validate, 3}, {trusted_cert_and_paths, 4},
                          {certificate_chain, 3}, {certificate_chain, 5},
                          {issuer, 1}]},
       {ssl_cipher, [{filter, 3}]},
       {ssl_gen_statem, [{initial_hello, 3}]},
       {ssl_handshake, [{path_validate, 11}, {path_validation, 10},
                        {select_hashsign, 5}, {get_cert_params, 1},
                        {cert_curve, 3},
                        {maybe_check_hostname, 3}, {maybe_check_hostname, 3}]},
       {ssl_pkix_db, [{decode_cert, 2}]},
       {tls_handshake_1_3, [{path_validation, 10}]},
       {tls_server_connection_1_3, [{init,1}]},
       {tls_client_connection_1_3, [{init,1}]},
       {tls_connection, [{init,1}]},
       {dtls_connection, [{init,1}]}]},
     {kdt, %% key update
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{tls_gen_connection_1_3, [{handle_key_update, 2}]},
       {tls_sender, [{init, 3}, {time_to_rekey, 6},
                     {send_post_handshake_data, 4}]},
       {tls_v1, [{update_traffic_secret, 2}]}]},
     {rle, %% role
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{ssl, [{listen,2}, {connect,3}]},
       {ssl_gen_statem, [{init, 1}]},
       {tls_server_session_ticket, [{init,1}]},
       {tls_sender, [{init, 3}]}]},
     {ssn, %% session
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{tls_server_session_ticket,
        [{handle_call,3}, {handle_cast,2}, {handle_info,2},
         {terminate,2}, {start_link,7},
         {init,1}, {initial_state,1}, {validate_binder,5}, {stateful_store,0},
         {stateful_ticket_store,6}, {stateful_use,4}, {stateful_use,6},
         {stateful_usable_ticket,5}, {stateful_living_ticket,2},
         {stateful_psk_ticket_id,1}, {generate_stateless_ticket,5}, {stateless_use,6},
         {stateless_usable_ticket,5}, {stateless_living_ticket,5}, {in_window,2},
         {stateless_anti_replay,5}]},
       {tls_handshake_1_3,
        [{get_ticket_data,3}]}]},
     {hbn, %% hibernate
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [{tls_sender,
        [{connection, 3}, {hibernate_after, 3}]},
       {dtls_connection,
        [{connection,3},
         {gen_info, 3}]},
       {dtls_gen_connection,
        [{handle_info,3}]},
       {ssl_gen_statem,
        [{hibernate_after, 3}, {handle_common_event, 4}]}]},
     {ct, %% common_test
      fun(M, F, A) -> dbg:tpl(M, F, A, x) end,
      fun(M, F, A) -> dbg:ctpl(M, F, A) end,
      [
       %% {ct_test_support, %% module from test and not src folder, enable it manually if needed
       %% [{run_ct_run_test, 2},
       %%  {run_ct_script_start, 2},
       %%  {run, 2},
       %%  {init_per_suite, 2},
       %%  {start_slave, 3}]},
       {test_server, [
                      {ts_tc, 3},
                      {user_callback, 5},
                      {fw_error_notify, 4},
                      {get_loc, 1},
                      {set_tc_state, 1},
                      {init_per_testcase, 3},
                      {run_test_case_msgloop, 1},
                      {run_test_case_eval1, 6},
                      {do_init_tc_call, 4},
                      {process_return_val, 6},
                      {do_end_tc_call, 4},
                      {end_per_testcase, 3},
                      {call_end_conf, 7},
                      {do_call_end_conf, 7},
                      {call_end_conf, 7},
                      {handle_tc_exit, 2},
                      {capture_start, 0},
                      {capture_stop, 0},
                      {capture_get, 0},
                      {fail, 0},
                      {fail, 1},
                      {timetrap, 4},
                      {start_node, 3},
                      {comment, 1}
                     ]}
       %% ,{ct_util, [{mark_process, 0}]}
      ]
     }].
