%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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
%% Generic connection owner process.
%%

-module(ct_gen_conn).
-moduledoc false.

-behaviour(gen_server).

-export([start/4,
         stop/1,
         get_conn_pid/1]).

-export([call/2,
         call/3,
         return/2,
         do_within_time/2]).

-export([log/3,
         start_log/1,
         cont_log/2,
         cont_log_no_timestamp/2,
         end_log/0]).

%% gen_server callbacks
-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         code_change/3,
         terminate/2]).

%% test
-export([make_opts/1]).

-ifdef(debug).
-define(dbg,true).
-else.
-define(dbg,false).
-endif.

-type handle() :: pid(). %% connection-owning process spawned here

-record(gen_opts, {callback  :: module(),
                   name      :: ct:target_name(),
                   address   :: term(),
                   init_data :: term(),
                   reconnect    = true  :: boolean(),
                   forward      = false :: boolean(),
                   use_existing = true  :: boolean(),
                   old          = false :: boolean(),
                   conn_pid       :: pid() | undefined,
                   cb_state       :: term(),
                   ct_util_server :: pid() | undefined}).

%% ---------------------------------------------------------------------

-spec start(Address, InitData, CallbackMod, [Opt])
   -> {ok, handle()} | {error, Reason :: term()}
 when Address :: term(),
      InitData :: term(),
      CallbackMod :: module(),
      Opt :: {name, Name :: ct:target_name()}
           | {use_existing_connection, boolean()}
           | {reconnect, boolean()}
           | {forward_messages, boolean()}
           | {old, boolean()}
      ;    (Name, Address, InitData, CallbackMod)
   -> {ok, handle()} | {error, Reason :: term()}
 when Name :: ct:target_name(),
      Address :: term(),
      InitData :: term(),
      CallbackMod :: module().

%% Open a connection and start the generic connection owner process.
%%
%% CallbackMod is a specific callback module for
%% each type of connection (e.g. telnet, ftp, netconf). It must export:
%%
%%   init(Name, Address, InitData) -> {ok, ConnectionPid, State}
%%                                  | {error,Reason}.
%%
%% Name defaults to undefined if unspecified.
%%
%% The callback modules must also export:
%%
%%   handle_msg(Msg, From, State) -> {reply, Reply, State}
%%                                 | {noreply, State}
%%                                 | {stop, Reply, State}
%%   terminate(ConnectionPid, State) -> term()
%%   close(Handle) -> term()
%%
%% A Reply of the form {retry, _} results in a new call to the server with
%% the retry tuple as message.
%%
%% The close/1 callback function is actually a callback
%% for ct_util, for closing registered connections when
%% ct_util_server is terminated. Handle is the Pid of
%% the ct_gen_conn process.
%%
%% If option reconnect is true, then the
%% callback must also export:
%%
%%   reconnect(Address, State) -> {ok, ConnectionPid, State}
%%
%% If option forward_messages is true then
%% the callback must also export:
%%
%%   handle_msg(Msg,State) -> {noreply,State}
%%                          | {stop,State}
%%
%% An old interface still exists. This is used by ct_telnet, ct_ftp
%% and ct_ssh. The start function then has an explicit
%% Name argument, and no Opts argument.

start(Address, InitData, CallbackMod, Opts) when is_list(Opts) ->
    do_start(Address, InitData, CallbackMod, Opts);
start(Name, Address, InitData, CallbackMod) ->
    do_start(Address, InitData, CallbackMod, [{name, Name}, {old, true}]).

%% ---------------------------------------------------------------------

-spec stop(handle()) -> ok | {error, Reason :: term()}.

%% Close the connection and stop the process managing it.

stop(Handle) ->
    call(Handle, stop, 5000).

%% ---------------------------------------------------------------------

-spec get_conn_pid(handle()) -> pid().

%% Return the connection pid associated with Handle

get_conn_pid(Handle) ->
    call(Handle, get_conn_pid).

%% ---------------------------------------------------------------------

-spec log(Heading, Format, Args) -> ok
 when Heading :: iodata(),
      Format  :: io:format(),
      Args    :: [term()].

%% Log activities on the current connection.
%% See ct_logs:log/3

log(Heading, Format, Args) ->
    log(log, [Heading, Format, Args]).

%% ---------------------------------------------------------------------

-spec start_log(Heading :: iodata()) -> ok.

%% Log activities on the current connection.
%% See ct_logs:start_log/1

start_log(Heading) ->
    log(start_log, [Heading]).

%% ---------------------------------------------------------------------

-spec cont_log(Format, Args) -> ok
 when Format :: io:format(),
      Args :: [term()].

%% Log activities on the current connection.
%% See ct_logs:cont_log/2

cont_log(Format,Args) ->
    log(cont_log, [Format, Args]).

%% ---------------------------------------------------------------------

-spec cont_log_no_timestamp(Format, Args) -> ok
 when Format :: io:format(),
      Args :: [term()].

%% Log activities on the current connection.
%% See ct_logs:cont_log/2

cont_log_no_timestamp(Format, Args) ->
    log(cont_log_no_timestamp, [Format, Args]).

%% ---------------------------------------------------------------------

-spec end_log() -> ok.

%% Log activities on the current connection.
%% See ct_logs:end_log/0

end_log() ->
    log(end_log, []).

%% ---------------------------------------------------------------------

-spec do_within_time(Fun, Tmo)
   -> Result
 when Fun :: fun(),
      Tmo :: non_neg_integer(),
      Result :: term().

%% Return the result of evaluating Fun, or interrupt after Tmo
%% milliseconds or if the connection is closed. Assumes the caller
%% is trapping exits.

do_within_time(Fun, Tmo) ->
    do_within_time(Fun, Tmo, get(silent), get(conn_pid)).

%% Work around the fact that ct_telnet calls do_within_time/2 in its
%% init callback, before it returns the connection pid for init/1 to
%% write to the process dictionary. Monitoring on self() is pointless,
%% but harmless. Should really be fixed by not using the process
%% dictionary to pass arguments.
do_within_time(Fun, Tmo, Silent, undefined) ->
    do_within_time(Fun, Tmo, Silent, self());

do_within_time(Fun, Tmo, Silent, ConnPid) ->
    MRef = monitor(process, ConnPid),
    Pid = spawn_link(fun() ->
                             ct_util:mark_process(),
                             put(silent, Silent),
                             exit({MRef, Fun()})
                     end),
    down(Pid, MRef, Tmo, failure).

down(Pid, MRef, Tmo, Reason) ->
    receive
        {'EXIT', Pid, T} ->
            infinity == Tmo orelse demonitor(MRef, [flush]),
            rc(MRef, T, Reason);
        {'DOWN', MRef, process, _, _} ->
            down(Pid, MRef, connection_closed)
    after Tmo ->
            demonitor(MRef, [flush]),
            down(Pid, MRef, timeout)
    end.

down(Pid, MRef, Reason) ->
    exit(Pid, kill),
    down(Pid, MRef, infinity, Reason).

rc(Ref, {Ref, RC}, _Reason) ->
    RC;
rc(_, Reason, failure) ->  %% failure before timeout or lost connection
    {error, Reason};
rc(_, _, Reason) ->
    {error, Reason}.

%% ===========================================================================

do_start(Address, InitData, CallbackMod, OptsList) ->
    #gen_opts{name = Name}
        = Opts
        = make_opts(OptsList, #gen_opts{callback  = CallbackMod,
                                        address   = Address,
                                        init_data = InitData}),
    %% Testing for an existing connection is slightly broken as long
    %% as calls to start aren't serialized: concurrent starts can both
    %% end up in the final clause.
    case ct_util:does_connection_exist(Name, Address, CallbackMod) of
        {ok, _Pid} = Ok when Opts#gen_opts.use_existing ->
            log("ct_gen_conn:start","Using existing connection!\n", []),
            Ok;
        {ok, Pid} when not Opts#gen_opts.use_existing ->
            {error, {connection_exists, Pid}};
        false ->
            do_start(Opts)
    end.

do_start(Opts) ->
    try gen_server:start(?MODULE, Opts, []) of
        {ok, _} = Ok    -> Ok;
        {error, Reason} -> {error, rc(Reason)}
    catch
        exit: Reason ->
            log("ct_gen_conn:start",
                "Connection process died: ~tp\n",
                [Reason]),
            {error, {connection_process_died, Reason}}
    end.

%% Unwrap a {shutdown, _} exit reason for backwards compatibility.
rc({shutdown, Reason}) -> Reason;
rc(T)                  -> T.

make_opts(Opts) ->
    make_opts(Opts, #gen_opts{}).

make_opts(Opts, #gen_opts{} = Rec) ->
    lists:foldl(fun opt/2, Rec, Opts).

opt({name, Name}, Rec)                    -> Rec#gen_opts{name = Name};
opt({reconnect, Bool}, Rec)               -> Rec#gen_opts{reconnect = Bool};
opt({forward_messages, Bool}, Rec)        -> Rec#gen_opts{forward = Bool};
opt({use_existing_connection, Bool}, Rec) -> Rec#gen_opts{use_existing = Bool};
opt({old, Bool}, Rec)                     -> Rec#gen_opts{old = Bool}.

%% ===========================================================================

call(Pid, Msg) ->
    call(Pid, Msg, infinity).

-spec call(Handle, Msg, Tmo)
   -> term()
 when Handle :: handle(),
      Msg :: term(),
      Tmo :: non_neg_integer()
           | infinity.

call(Pid, Msg, infinity = Tmo) ->
    gen_call(Pid, Msg, Tmo);

%% Spawn a middleman process if the call can timeout to avoid the
%% possibility of a reply being left in the caller's mailbox after a
%% timeout.
call(Pid, Msg, Tmo) ->
    {_, MRef} = spawn_monitor(fun() -> exit(gen_call(Pid, Msg, Tmo)) end),
    receive {'DOWN', MRef, process, _, RC} -> RC end.

gen_call(Pid, Msg, Tmo) ->
    try gen_server:call(Pid, Msg, Tmo) of
        T -> retry(Pid, T, Tmo)
    catch
        exit: Reason -> {error, {process_down, Pid, rc(Pid, Reason)}}
    end.

retry(Pid, {retry, _} = T, Tmo) -> gen_call(Pid, T, Tmo);
retry(_, T, _)                  -> T.

%% Unwrap the MFA gen_server puts into exit reasons.
rc(Pid, {Reason, {gen_server, call, _}}) ->
    rc(Pid, Reason);

rc(Pid, timeout) ->
    log("ct_gen_conn",
        "Connection process ~w not responding. Killing now!",
        [Pid]),
    exit(Pid, kill),
    forced_termination;

rc(_, Reason) ->
    rc(Reason).

return(From, Result) ->
    gen_server:reply(From, Result).

%% ===========================================================================
%% gen_server callbacks

%% init/1

init(#gen_opts{callback = Mod,
               name = Name,
               address = Addr,
               init_data = InitData}
     = Opts) ->
    process_flag(trap_exit, true),
    ct_util:mark_process(),
    put(silent, false),
    try Mod:init(Name, Addr, InitData) of
        {ok, ConnPid, State} when is_pid(ConnPid) ->
            link(ConnPid),
            put(conn_pid, ConnPid),
            SrvPid = whereis(ct_util_server),
            link(SrvPid),
            ct_util:register_connection(Name, Addr, Mod, self()),
            {ok, Opts#gen_opts{conn_pid = ConnPid,
                               cb_state = State,
                               ct_util_server = SrvPid}};
        {error, Reason} ->
            {stop, {shutdown, Reason}}
    catch
        C: Reason when C /= error ->
            {stop, {shutdown, Reason}}
    end.

%% handle_call/2

handle_call(get_conn_pid, _From, #gen_opts{conn_pid = Pid} = Opts) ->
    {reply, Pid, Opts};

handle_call(stop, _From, Opts) ->
    {stop, normal, ok, Opts};

%% Only retry if failure is because of a reconnection.
handle_call({retry, {Error, _Name, ConnPid, _Msg}},
            _From,
            #gen_opts{conn_pid = ConnPid}
            = Opts) ->
    {reply, error_rc(Error), Opts};

handle_call({retry, {_Error, _Name, _CPid, Msg}},
            _From,
            #gen_opts{callback = Mod,
                      cb_state = State}
            = Opts) ->
    log("Rerunning command","Connection reestablished. "
        "Rerunning command...",
        []),
    {Reply, NewState} = Mod:handle_msg(Msg, State),
    {reply, Reply, Opts#gen_opts{cb_state = NewState}};

handle_call(Msg, _From, #gen_opts{old = true,
                                  callback = Mod,
                                  cb_state = State}
                        = Opts) ->
    {Reply, NewState} = Mod:handle_msg(Msg, State),
    {reply, Reply, Opts#gen_opts{cb_state = NewState}};

handle_call(Msg, From, #gen_opts{callback = Mod,
                                  cb_state = State}
                        = Opts) ->
    case Mod:handle_msg(Msg, From, State) of
        {reply, Reply, NewState} ->
            {reply, Reply, Opts#gen_opts{cb_state = NewState}};
        {noreply, NewState} ->
            {noreply, Opts#gen_opts{cb_state = NewState}};
        {stop, Reply, NewState} ->
            {stop, normal, Reply, Opts#gen_opts{cb_state = NewState}}
    end.

%% handle_cast/2

handle_cast(_, Opts) ->
    {noreply, Opts}.

%% handle_info/2

handle_info({'EXIT', Pid, Reason},
            #gen_opts{reconnect = true,
                      conn_pid = Pid,
                      address = Addr}
            = Opts) ->
    log("Connection down!\nOpening new!",
        "Reason: ~tp\nAddress: ~tp\n",
        [Reason, Addr]),
    case reconnect(Opts) of
        {ok, NewPid, NewState} ->
            link(NewPid),
            put(conn_pid, NewPid),
            {noreply, Opts#gen_opts{conn_pid = NewPid,
                                    cb_state = NewState}};
        Error ->
            log("Reconnect failed. Giving up!",
                "Reason: ~tp\n",
                [Error]),
            {stop, normal, Opts}
    end;

handle_info({'EXIT', Pid, Reason},
            #gen_opts{reconnect = false,
                      conn_pid = Pid}
            = Opts) ->
    log("Connection closed!", "Reason: ~tp\n", [Reason]),
    {stop, normal, Opts};

handle_info({'EXIT', Pid, Reason},
            #gen_opts{ct_util_server = Pid}
            = Opts) ->
    {stop, {shutdown, Reason}, Opts};

handle_info(Msg, #gen_opts{forward = true,
                           callback = Mod,
                           cb_state = State}
                 = Opts) ->
    case Mod:handle_msg(Msg, State) of
        {noreply, NewState} ->
            {noreply, Opts#gen_opts{cb_state = NewState}};
        {stop, NewState} ->
            {stop, normal, Opts#gen_opts{cb_state = NewState}}
    end;

handle_info(_, #gen_opts{} = Opts) ->
    {noreply, Opts}.

%% code_change/2

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% terminate/2

%% Cleanup is only in this case since ct_util also cleans up and
%% expects us not to, which is at least an odd expectation.
terminate(normal, #gen_opts{callback = Mod,
                            conn_pid = Pid,
                            cb_state = State}) ->
    ct_util:unregister_connection(self()),
    unlink(Pid),
    Mod:terminate(Pid, State);

terminate(_, #gen_opts{}) ->
    ok.

%% ===========================================================================

error_rc({error, _} = T) -> T;
error_rc(Reason)         -> {error, Reason}.

reconnect(#gen_opts{callback = Mod,
                    address = Addr,
                    cb_state = State}) ->
    Mod:reconnect(Addr, State).

log(Func, Args) ->
    case get(silent) of
        true when not ?dbg ->
            ok;
        _ ->
            apply(ct_logs, Func, Args)
    end.
