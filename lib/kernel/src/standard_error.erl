%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
-module(standard_error).
-moduledoc false.
-behaviour(supervisor_bridge).

-compile(nowarn_deprecated_catch).

-include_lib("kernel/include/logger.hrl").

%% Basic standard i/o server for standard_error.
-export([start_link/0, init/1, terminate/2]).

-define(NAME, standard_error).
-define(PROCNAME_SUP, standard_error_sup).

%% Defines for control ops
-define(ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER, 16#018b0900).
-define(CTRL_OP_GET_WINSIZE, (100 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).

%%
%% The basic server and start-up.
%%
-spec start_link() -> 'ignore' | {'error',term()} | {'ok',pid()}.

start_link() ->
    supervisor_bridge:start_link({local, ?PROCNAME_SUP}, ?MODULE, []).

-spec terminate(term(), pid()) -> 'ok'.

terminate(_Reason,Pid) ->
    (catch exit(Pid,kill)),
    ok.

-spec init([]) -> {'error','no_stderror'} | {'ok',pid(),pid()}.

init([]) ->
    case (catch start()) of
        Pid when is_pid(Pid) ->
            {ok,Pid,Pid};
        _ ->
            {error,no_stderror}
    end.

start() ->
    Id = spawn(fun server/0),
    register(?NAME, Id),
    Id.

server() ->
    process_flag(trap_exit, true),
    ok = prim_tty:load(),
    TTY = prim_tty:init(#{ input => disabled,
                           output => cooked,
                           ofd => stderr}),
    run(TTY).

run(TTY) ->
    put(encoding, encoding(TTY)),
    put(onlcr, prim_tty:isatty(stderr)),
    put(log, none),
    server_loop(TTY).

encoding(TTY) ->
    case prim_tty:unicode(TTY) of
        true -> unicode;
        false -> latin1
    end.

server_loop(TTY) ->
    receive
        {io_request,From,ReplyAs,Request} = IoReq when is_pid(From) ->
            group:log_io_request(IoReq, get(log), ?MODULE),
            case io_request(Request, TTY) of
                {stop, Reason} ->
                    io_reply(From, ReplyAs, {error, Reason}),
                    stop;
                {reply, Reply} ->
                    io_reply(From, ReplyAs, Reply),
                    server_loop(TTY)
            end;
        _Other ->               % Ignore other messages
            server_loop(TTY)
    end.

get_fd_geometry(TTY) ->
    case prim_tty:window_size(TTY) of
        {ok, {W, H}} -> {W, H};
        _ -> error
    end.

%% New in R13B
%% Encoding option (unicode/latin1)
io_request({put_chars,unicode,Chars}, TTY) ->
    case wrap_characters_to_binary(Chars, unicode, get(encoding)) of
        error ->
            {reply,{error,put_chars}};
        Bin ->
            put_chars(Bin, TTY)
    end;
io_request({put_chars,unicode,Mod,Func,Args}, TTY) ->
    case catch apply(Mod, Func, Args) of
        Data when is_list(Data); is_binary(Data) ->
            case wrap_characters_to_binary(Data, unicode, get(encoding)) of
                Bin when is_binary(Bin) ->
                    put_chars(Bin, TTY);
                error ->
                    {reply,{error,put_chars}}
            end;
        _ ->
            {reply,{error,put_chars}}
    end;
io_request({put_chars,latin1,Chars}, TTY) ->
    case catch unicode:characters_to_binary(Chars, latin1, get(encoding)) of
        Data when is_binary(Data) ->
            put_chars(Data, TTY);
        _ ->
            {reply,{error,put_chars}}
    end;
io_request({put_chars,latin1,Mod,Func,Args}, TTY) ->
    case catch apply(Mod, Func, Args) of
        Data when is_list(Data); is_binary(Data) ->
            case
                catch unicode:characters_to_binary(Data, latin1, get(encoding))
            of
                Bin when is_binary(Bin) ->
                    put_chars(Bin, TTY);
                _ ->
                    {reply,{error,put_chars}}
            end;
        _ ->
            {reply,{error,put_chars}}
    end;
%% BC if called from pre-R13 node
io_request({put_chars,Chars}, TTY) ->
    io_request({put_chars,latin1,Chars}, TTY);
io_request({put_chars,Mod,Func,Args}, TTY) ->
    io_request({put_chars,latin1,Mod,Func,Args}, TTY);
%% New in R12
io_request({get_geometry,columns},TTY) ->
    case get_fd_geometry(TTY) of
        {W,_H} ->
            {reply,W};
        _ ->
            {reply,{error,enotsup}}
    end;
io_request({get_geometry,rows},TTY) ->
    case get_fd_geometry(TTY) of
        {_W,H} ->
            {reply,H};
        _ ->
            {reply,{error,enotsup}}
    end;
io_request(getopts, _TTY) ->
    getopts();
io_request({setopts,Opts}, _TTY) when is_list(Opts) ->
    setopts(Opts);
io_request({requests,Reqs}, TTY) ->
    io_requests(Reqs, {reply,ok}, TTY);
io_request(R, _TTY) ->                      %Unknown request
    {reply,{error,{request,R}}}.        %Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, TTY)
%%  Process a list of output requests as long as the previous status is 'ok'.
io_requests([_|_], {reply,{error, _}} = Error, _TTY) ->
    Error;
io_requests([R|Rs], {reply,_Ok}, TTY) ->
    io_requests(Rs, io_request(R, TTY), TTY);
io_requests([_|_], Error, _) ->
    Error;
io_requests([], Stat, _) ->
    Stat.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply},
    ok.

%% put_chars
put_chars(Chars, TTY) when is_binary(Chars) ->
    {ok, MonitorRef} = prim_tty:write(TTY, Chars, self()),
    #{ write := WriteRef } = prim_tty:handles(TTY),
    receive
        {WriteRef, ok} ->
            erlang:demonitor(MonitorRef, [flush]),
            {reply, ok};
        {'DOWN', MonitorRef, _, _, Reason} ->
            ?LOG_INFO("Failed to write to standard error (~p)", [Reason]),
            {stop, Reason}
    end.

%% setopts
setopts(Opts0) ->
    Opts = expand_encoding(Opts0),
    case check_valid_opts(Opts) of
        true ->
            lists:foreach(
              fun({encoding, Enc}) ->
                      put(encoding, Enc);
                 ({onlcr, Bool}) ->
                      put(onlcr, Bool);
                 ({log, Bool}) ->
                      put(log, Bool)
              end, Opts),
            {reply, ok};
        false ->
            {reply,{error,enotsup}}
    end.

check_valid_opts([]) ->
    true;
check_valid_opts([{encoding,Valid}|T]) when Valid =:= unicode; Valid =:= utf8;
                                            Valid =:= latin1 ->
    check_valid_opts(T);
check_valid_opts([{onlcr,Bool}|T]) when is_boolean(Bool) ->
    check_valid_opts(T);
check_valid_opts([{log,Flag}|T]) ->
    case lists:member(Flag, [none, output, input, all]) of
        true -> check_valid_opts(T);
        false -> false
    end;
check_valid_opts(_) ->
    false.

expand_encoding([]) ->
    [];
expand_encoding([latin1 | T]) ->
    [{encoding,latin1} | expand_encoding(T)];
expand_encoding([unicode | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([utf8 | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([{encoding,utf8} | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([H|T]) ->
    [H|expand_encoding(T)].

getopts() ->
    Uni = {encoding,get(encoding)},
    Onlcr = {onlcr, get(onlcr)},
    Log = {log, get(log)},
    {reply,[Uni, Onlcr, Log]}.

wrap_characters_to_binary(Chars,From,To) ->
    TrNl = get(onlcr),
    Limit = case To of
                latin1 ->
                    255;
                _Else ->
                    16#10ffff
            end,
    case catch unicode:characters_to_list(Chars, From) of
        L when is_list(L) ->
            unicode:characters_to_binary(
              [ case X of
                    $\n when TrNl ->
                        "\r\n";
                    High when High > Limit ->
                        ["\\x{",erlang:integer_to_list(X, 16),$}];
                    Low ->
                        Low
                end || X <- L ], unicode, To);
        _ ->
            error
    end.
