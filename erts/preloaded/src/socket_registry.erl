%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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

%% =========================================================================
%%
%% This is a registry process for the socket module.
%% The nif sends info here about all created and deleted socket(s).
%%
%% =========================================================================

-module(socket_registry).

-export([
         start/0,
         number_of/0,
         which_sockets/1
        ]).


-record(esock_info,
        {
         sock  :: socket:socket(),
         atime :: integer()  % Add time - erlang:monotonic_time(milli_seconds)
        }).


%% =========================================================================

%% This is not a "normal" start function. Instead its the entry
%% function for the socket registry process.
start() ->
    erlang:register(?MODULE, self()),
    process_flag(trap_exit, true),
    loop([]).

number_of() ->
    request(number_of).

which_sockets(Filter)
  when is_boolean(Filter);
       tuple_size(Filter) =:= 2;
       is_function(Filter, 1) ->
    case request({which_sockets, Filter}) of
        badarg ->
            erlang:error(badarg, [Filter]);
        Sockets ->
            Sockets
    end;
which_sockets(Filter) ->
    erlang:error(badarg, [Filter]).


%% =========================================================================

loop(DB) ->
    receive
        {'$socket', add, Socket} ->
            loop( handle_add_socket(DB, Socket) );

        {'$socket', del, Socket} ->
            loop( handle_delete_socket(DB, Socket) );

        {'$socket', sendfile_deferred_close = Fname, Socket} ->
            Closer =
                fun () ->
                        try prim_socket:Fname(Socket) of
                            ok ->
                                ok;
                            Other ->
                                log_msg(
                                  warning,
                                  "socket-registry "
                                  "sendfile_deferred_close:"
                                  "~n   [~p] ~p",
                                  [Socket, Other])
                        catch Class : Reason : Stacktrace->
                                log_msg(
                                  warning,
                                  "socket-registry "
                                  "sendfile_deferred_close:"
                                  "~n   [~p] (~p:~p)"
                                  "~n      ~p",
                                  [Socket, Class, Reason, Stacktrace])
                        end
                end,
            _ = spawn(Closer),
            loop(DB);

        {?MODULE, request, From, ReqId, Req} = _REQ ->
            %% erlang:display(REQ),
            {NewDB, Reply} = handle_request(DB, Req),
            reply(ReqId, From, Reply),
            loop(NewDB);
        
        Msg ->
            NewDB = handle_unexpected_msg(DB, Msg),
            loop(NewDB)
    end.


%% =========================================================================

handle_add_socket(DB, Sock) ->
    [#esock_info{sock = Sock, atime = timestamp()} | DB].

handle_delete_socket(DB, Sock) ->
    lists:keydelete(Sock, #esock_info.sock, DB).


handle_request(DB, number_of) ->
    {DB, db_size(DB)};

handle_request(DB, {which_sockets, Filter}) ->
    {DB, do_which_sockets(DB, Filter)};

handle_request(DB, BadRequest) ->
    {DB, {error, {bad_request, BadRequest}}}.


%% ---

handle_unexpected_msg(DB, {'EXIT', Pid, Reason}) ->
    F = "socket-registry received unexpected exit from ~p:"
        "~n   ~p",
    A = [Pid, Reason],
    log_msg(warning, F, A),
    DB;
handle_unexpected_msg(DB, X) ->
    F = "socket-registry received unexpected:"
        "~n   ~p",
    A = [X],
    log_msg(warning, F, A),
    DB.

%% This is "stolen" from the init process. But level is set to warning instead.
%% This "may" not be what you want, but we can always decrease to info instead...
%% Also, we may receive (unexpected) messages that should be classified
%% differently (info, warning, ...)...
%%
%% This is equal to calling logger:[info|warning]/3 which we don't
%% want to do from this process, at least not during
%% system boot. We don't want to call logger:timestamp()
%% either.
%%

log_msg(Level, F, A) ->
    log_msg( mk_log_msg(Level, F, A) ).
%%
log_msg(Msg) ->
    _ = catch logger ! Msg,
    ok.


%% mk_log_msg(info, F, A) ->
%%     mk_log_msg(info, info_msg, F, A);
mk_log_msg(warning, F, A) ->
    mk_log_msg(warning, warning_msg, F, A).

mk_log_msg(Level, Tag, F, A) ->
    Meta = #{pid          => self(),
             gl           => erlang:group_leader(),
             time         => os:system_time(microsecond),
             error_logger => #{tag => Tag}},
    {log, Level, F, A, Meta}.


%% ---

db_size(DB) ->
    length(DB).

do_which_sockets(DB, Filter) ->
    try
        [Sock ||
            #esock_info{sock = Sock} <- DB,
            if
                is_boolean(Filter) ->
                    Filter;
                true ->
                    SockInfo = socket:info(Sock),
                    case Filter of
                        {Field, Value} ->
                            case SockInfo of
                                #{Field := Value} ->
                                    true;
                                #{} ->
                                    false
                            end;
                        _ when is_function(Filter, 1) ->
                            Filter(SockInfo) =:= true
                    end
            end]
    catch _ : _ ->
            %% Filter/1 must have failed
            badarg
    end.


%% =========================================================================

request(Req) ->
    ReqId  = make_ref(),
    From = self(),
    ReqMsg = {?MODULE, request, From, ReqId, Req},
    Registry = whoami(),    
    erlang:send(Registry, ReqMsg),
    receive
        {?MODULE, reply, Registry, ReqId, Reply} ->
            Reply
    end.

reply(ReqId, From, Reply) ->
    Registry = self(),
    RepMsg = {?MODULE, reply, Registry, ReqId, Reply},
    erlang:send(From, RepMsg).


%% =========================================================================

whoami() ->
    whereis(?MODULE).

timestamp() ->
    erlang:monotonic_time(milli_seconds).
