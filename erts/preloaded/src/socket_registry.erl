%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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
    await_reply( request(number_of) ).

which_sockets(Filter) when is_function(Filter, 1) ->
    await_reply( request({which_sockets, Filter}) ).


%% =========================================================================

loop(DB) ->
    receive
        {'$socket', add, Socket} ->
            loop( handle_add_socket(DB, Socket) );

        {'$socket', del, Socket} ->
            loop( handle_delete_socket(DB, Socket) );

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
    handle_unexpected_msg(warning, F, A),
    DB;
handle_unexpected_msg(DB, X) ->
    F = "socket-registry received unexpected:"
        "~n   ~p",
    A = [X],
    handle_unexpected_msg(warning, F, A),
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

%% handle_unexpected_msg(info, F, A) ->
%%     do_handle_unexpected_msg( mk_unexpected_info_msg(F, A) ).
handle_unexpected_msg(warning, F, A) ->
    do_handle_unexpected_msg( mk_unexpected_warning_msg(F, A) ).

do_handle_unexpected_msg(Msg) ->
    catch logger ! Msg.


%% mk_unexpected_info_msg(F, A) ->
%%     mk_unexpected_msg(info, info_msg, F, A).

mk_unexpected_warning_msg(F, A) ->
    mk_unexpected_msg(warning, warning_msg, F, A).

mk_unexpected_msg(Level, Tag, F, A) ->
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
        begin
            SocksInfo =
                [{Sock, socket:info(Sock)} || #esock_info{sock = Sock} <- DB],
            [Sock || {Sock, SockInfo} <- SocksInfo, Filter(SockInfo)]
        end
    catch
        _:_:_ ->
            [Sock || #esock_info{sock = Sock} <- DB]
    end.
    


%% =========================================================================

request(Req) ->
    ReqId  = make_ref(),
    ReqMsg = {?MODULE, request, self(), ReqId, Req},
    Registry = whoami(),    
    erlang:send(Registry, ReqMsg),
    ReqId.

reply(ReqId, From, Reply) ->
    RepMsg = {?MODULE, reply, self(), ReqId, Reply},
    erlang:send(From, RepMsg).

await_reply(ReqId) ->
    Registry = whoami(),    
    receive
        {?MODULE, reply, Registry, ReqId, Reply} ->
            Reply
    end.


%% =========================================================================

whoami() ->
    whereis(?MODULE).

timestamp() ->
    erlang:monotonic_time(milli_seconds).
