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

start() ->
    erlang:register(?MODULE, self()),
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
        
        _ -> % Make sure queue does not grow
            loop(DB)
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
