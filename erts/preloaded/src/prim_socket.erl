%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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

-module(prim_socket).
-moduledoc false.

-compile(no_native).

-export([on_load/0, on_load/1, init/0]).

-export(
   [
    info/0, info/1,
    debug/1, socket_debug/1, use_registry/1,
    supports/0, supports/1, supports/2,
    is_supported/1, is_supported/2,
    open/2, open/4,
    bind/2, bind/3,
    connect/1, connect/3,
    listen/2,
    accept/2,
    send/4, sendto/4, sendto/5, sendmsg/4, sendmsg/5, sendv/3,
    sendfile/4, sendfile/5, sendfile_deferred_close/1,
    recv/4, recvfrom/4, recvmsg/5,
    close/1, finalize_close/1,
    shutdown/2,
    setopt/3, setopt_native/3,
    getopt/2, getopt_native/3,
    sockname/1, peername/1,
    ioctl/2, ioctl/3, ioctl/4,
    cancel/3
   ]).

-export([enc_sockaddr/1, p_get/1, rest_iov/2]).

-nifs([nif_info/0, nif_info/1, nif_supports/0, nif_supports/1, nif_command/1,
       nif_open/2, nif_open/4, nif_bind/2, nif_connect/1, nif_connect/3,
       nif_listen/2, nif_accept/2,
       nif_send/4, nif_sendto/5, nif_sendmsg/5, nif_sendv/3,
       nif_sendfile/5, nif_sendfile/4, nif_sendfile/1, nif_recv/4,
       nif_recvfrom/4, nif_recvmsg/5, nif_close/1, nif_shutdown/2,
       nif_setopt/5, nif_getopt/3, nif_getopt/4, nif_sockname/1,
       nif_peername/1, nif_ioctl/2, nif_ioctl/3, nif_ioctl/4, nif_cancel/3,
       nif_finalize_close/1]).

%% Also in socket
-define(REGISTRY, socket_registry).


%% ===========================================================================
%%
%% Defaults
%%

-define(ESOCK_SOCKADDR_IN_DEFAULTS,
        (#{family => inet, port => 0, addr => any})).
-define(ESOCK_SOCKADDR_IN6_DEFAULTS,
        (#{family => inet6, port => 0, addr => any,
           flowinfo => 0, scope_id => 0})).
-define(ESOCK_SOCKADDR_LOCAL_DEFAULTS,
        (#{family => local, path => <<"">>})).
-define(ESOCK_SOCKADDR_UNSPEC_DEFAULTS,
        (#{family => unspec, addr => <<>>})).
-define(ESOCK_SOCKADDR_NATIVE_DEFAULTS,
        (#{family => 0, addr => <<>>})).

%% ===========================================================================
%%
%% Constants common to prim_socket_nif.c - has to be "identical"
%%

%% ----------------------------------
%% *** OTP (socket) options

-define(ESOCK_OPT_OTP_DEBUG,           1001).
-define(ESOCK_OPT_OTP_IOW,             1002).
-define(ESOCK_OPT_OTP_CTRL_PROC,       1003).
-define(ESOCK_OPT_OTP_RCVBUF,          1004).
%%-define(ESOCK_OPT_OTP_SNDBUF,          1005).
-define(ESOCK_OPT_OTP_RCVCTRLBUF,      1006).
-define(ESOCK_OPT_OTP_SNDCTRLBUF,      1007).
-define(ESOCK_OPT_OTP_FD,              1008).
-define(ESOCK_OPT_OTP_META,            1009).
-define(ESOCK_OPT_OTP_USE_REGISTRY,    1010).
-define(ESOCK_OPT_OTP_SELECT_READ,     1011).
%%
-define(ESOCK_OPT_OTP_DOMAIN,          1999). % INTERNAL
%%-define(ESOCK_OPT_OTP_TYPE,            1998). % INTERNAL
%%-define(ESOCK_OPT_OTP_PROTOCOL,        1997). % INTERNAL
%%-define(ESOCK_OPT_OTP_DTP,             1996). % INTERNAL



%% ===========================================================================
%% API for 'erl_init'
%%

on_load() ->
    on_load(#{}).

on_load(Extra) when is_map(Extra) ->
    %% This is spawned as a system process to prevent init:restart/0 from
    %% killing it.
    Pid = erts_internal:spawn_system_process(?REGISTRY, start, []),
    %%
    DebugFilename =
        case os:getenv("ESOCK_DEBUG_FILENAME") of
            "*" ->
                "/tmp/esock-dbg-??????";
            F ->
                F
        end,
    UseRegistry =
        case os:getenv("ESOCK_USE_SOCKET_REGISTRY") of
            "true" ->
                true;
            "false" ->
                false;
            _ ->
                undefined
        end,
    %%
    Extra_1 =
        case UseRegistry of
            undefined ->
                Extra#{registry => Pid};
            _ ->
                Extra
                    #{registry => Pid,
                      use_registry => UseRegistry}
        end,
    Extra_2 =
          case DebugFilename of
              false ->
                  Extra_1;
              _ ->
                  Extra_1
                      #{debug => true,
                        socket_debug => true,
                        debug_filename => enc_path(DebugFilename)}
          end,
    %% This will fail if the user has disabled esock support, making all NIFs
    %% fall back to their Erlang implementation which throws `notsup`.
    _ = erlang:load_nif(atom_to_list(?MODULE), Extra_2),
    init().

init() ->
    put_supports_table(protocols,
                       fun (Protocols) -> protocols_table(Protocols) end),
    put_supports_table(options,
                       fun (Options) -> options_table(Options) end),
    put_supports_table(ioctl_requests,
                       fun (Requests) -> Requests end),
    put_supports_table(ioctl_flags, fun (Flags) -> Flags end),
    put_supports_table(msg_flags, fun (Flags) -> Flags end),
    ok.

put_supports_table(Tag, MkTable) ->
    Table =
        try nif_supports(Tag) of
            Data ->
                merge_values(MkTable(Data))
        catch
            error : notsup ->
                #{}
        end,
    p_put(Tag, Table).

%% Like maps:from_list/1 the last duplicate key wins,
%% except if both values are lists; append the second to the first.
%%
merge_values([]) -> #{};
merge_values([{Key, Val} | L]) ->
    M = merge_values(L),
    case M of
        #{ Key := Val2 } ->
            if
                is_list(Val), is_list(Val2) ->
                    M#{ Key := Val ++ Val2 };
                true ->
                    M
            end;
        #{} ->
            M#{ Key => Val }
    end.

%% ->
%% [{Num, [Name, ...]}
%%  {Name, Num} for all names]
protocols_table([{Names, Num} | Protocols]) ->
    [{Num, Names} | protocols_table(Protocols, Names, Num)];
protocols_table([]) ->
    [].
%%
protocols_table(Protocols, [Name | Names], Num) ->
    [{Name, Num} | protocols_table(Protocols, Names, Num)];
protocols_table(Protocols, [], _Num) ->
    protocols_table(Protocols).

%% ->
%% [{{socket,Opt}, {socket,OptNum} | undefined} |
%%  {{Level, Opt}, {LevelNum, OptNum} | undefined}]
options_table([]) ->
    [];
options_table([{socket = Level, _LevelNum, LevelOpts} | Options]) ->
    options_table(Options, Level, Level, LevelOpts);
options_table([{Level, LevelNum, LevelOpts} | Options]) ->
    options_table(Options, Level, LevelNum, LevelOpts).
%%
options_table(Options, _Level, _LevelNum, []) ->
    options_table(Options);
options_table(Options, Level, LevelNum, [LevelOpt | LevelOpts]) ->
    [case LevelOpt of
         {Opt, OptNum} ->
             {{Level, Opt}, {LevelNum,OptNum}};
         Opt when is_atom(Opt) ->
             {{Level, Opt}, undefined}
     end | options_table(Options, Level, LevelNum, LevelOpts)].

%% ===========================================================================
%% API for 'socket'
%%

info() ->
    nif_info().

info(SockRef) ->
    #{protocol := NumProtocol} = Info = nif_info(SockRef),
    case p_get(protocols) of
        #{NumProtocol := [Protocol | _]} ->
            Info#{protocol := Protocol};
        #{} ->
            Info
    end.

%% ----------------------------------

debug(D) ->
    nif_command(#{command => ?FUNCTION_NAME, data => D}).


socket_debug(D) ->
    nif_command(#{command => ?FUNCTION_NAME, data => D}).


use_registry(D) ->
    nif_command(#{command => ?FUNCTION_NAME, data => D}).

%% ----------------------------------

supports() ->
    nif_supports().
     
supports(protocols) ->
    maps:fold(
      fun (Name, _Num, Acc) when is_atom(Name) ->
              [{Name, true} | Acc];
          (Num, _Names, Acc) when is_integer(Num) ->
              Acc
      end, [], p_get(protocols));
supports(ioctl_requests) ->
    maps:fold(
      fun (Name, _Num, Acc) when is_atom(Name) ->
              [{Name, true} | Acc];
          (Num, _Names, Acc) when is_integer(Num) ->
              Acc
      end, [], p_get(ioctl_requests));
supports(options) ->
    maps:fold(
      fun ({_Level,_Opt} = Option, Value, Acc) ->
              [{Option, is_supported_option(Option, Value)} | Acc]
      end, [], p_get(options));
supports(msg_flags) ->
    maps:fold(
      fun (Name, Num, Acc) ->
              [{Name, Num =/= 0} | Acc]
      end, [], p_get(msg_flags));
supports(ioctl_flags) ->
    maps:fold(
      fun (Name, Num, Acc) ->
              [{Name, Num =/= 0} | Acc]
      end, [], p_get(ioctl_flags));
supports(Key) ->
    nif_supports(Key).

supports(options, Level) when is_atom(Level) ->
    maps:fold(
      fun ({L, Opt} = Option, Value, Acc) ->
              if
                  L =:= Level ->
                      [{Opt, is_supported_option(Option, Value)} | Acc];
                  true ->
                      Acc
              end
      end, [], p_get(options));
supports(options, _Level) ->
    [];
supports(_Key1, _Key2) ->
    [].

is_supported_option({socket, peek_off}, _Value) ->
    %% Due to the behaviour of peek when peek_off is used,
    %% this option is reported as not supported.
    %% Can cause an infinite loop when calling recv with
    %% the peek flag (the second of two calls).
    %% So, until we have added extra code to know when
    %% peek-off is used, we do not claim to support this!
    %%
    false;
is_supported_option(_Option, {_NumLevel,_NumOpt}) ->
    true;
is_supported_option(_Option, undefined) ->
    false.

is_supported(Key1) ->
    case Key1 of
        ioctl_requests -> true;
        ioctl_flags    -> true;
        protocols      -> true;
        options        -> true;
        msg_flags      -> true;
        _ ->
            get_is_supported(Key1, nif_supports())
    end.

is_supported(ioctl_requests = Tab, Name) when is_atom(Name) ->
    p_get_is_supported(Tab, Name, fun (_) -> true end);
is_supported(ioctl_requests, _Name) ->
    false;
is_supported(ioctl_flags = Tab, Flag) ->
    p_get_is_supported(Tab, Flag, fun (Value) -> Value =/= 0 end);
is_supported(protocols = Tab, Name) when is_atom(Name) ->
    p_get_is_supported(Tab, Name, fun (_) -> true end);
is_supported(protocols, _Name) ->
    false;
is_supported(options = Tab, {_Level,_Opt} = Option) ->
    p_get_is_supported(
      Tab, Option,
      fun (Value) ->
              is_supported_option(Option, Value)
      end);
is_supported(options, _Option) ->
    false;
is_supported(msg_flags = Tab, Flag) ->
    p_get_is_supported(Tab, Flag, fun (Value) -> Value =/= 0 end);
is_supported(Key1, Key2) ->
    get_is_supported(Key2, nif_supports(Key1)).


p_get_is_supported(Tab, Key, Fun) ->
    case p_get(Tab) of
        #{Key := Val} ->
            Fun(Val);
        #{} ->
            false
    end.

get_is_supported(Key, Supported) ->
    case lists:keyfind(Key, 1, Supported) of
        false ->
            false;
        {_, Value} when is_boolean(Value) ->
            Value
    end.

%% ----------------------------------

open(FD, Opts) when is_map(Opts) ->
    try
        case Opts of
            #{protocol := Protocol} ->
                NumProtocol = enc_protocol(Protocol),
                Opts#{protocol := NumProtocol};
            #{} ->
                Opts
        end
    of
        EOpts ->
            case nif_open(FD, EOpts) of
                {invalid, Reason} ->
                  if
                      Reason =:= domain;
                      Reason =:= type ->
                          {error, {invalid, {options, Reason, Opts}}}
                  end;
                Result -> Result
            end
    catch
        throw : Reason ->
            {error, Reason}
    end.

open(Domain, Type, Protocol, Opts) when is_map(Opts) ->
    try
        {enc_protocol(Protocol),
         case Opts of
            #{netns := Path} when is_list(Path) ->
                Opts#{netns := enc_path(Path)};
            _ ->
                Opts
         end}
    of
        {NumProtocol, EOpts} ->
            case nif_open(Domain, Type, NumProtocol, EOpts) of
                {invalid, Reason} ->
                    {error,
                     {invalid,
                      {Reason,
                       case Reason of
                           domain -> Domain;
                           type -> Type
                       end}}};
                Result -> Result
            end
    catch
        throw : Reason ->
            {error, Reason}
    end.

%% ----------------------------------

bind(SockRef, Addr) ->
    try
        enc_sockaddr(Addr)
    of
        EAddr ->
            case nif_bind(SockRef, EAddr) of
                {invalid, Reason} ->
                    case Reason of
                        sockaddr ->
                            {error, {invalid, {Reason, Addr}}}
                    end;
                Result -> Result
            end
    catch
        throw : Reason ->
            {error, Reason}
    end.

bind(SockRef, Addrs, Action) when is_list(Addrs) ->
    try
        [enc_sockaddr(Addr) || Addr <- Addrs]
    of
        EAddrs ->
            %% Not implemented yet
            case nif_bind(SockRef, EAddrs, Action) of
                {invalid, Reason} ->
                    case Reason of
                        {sockaddr = Cause, N} ->
                            {error,
                             {invalid, {Cause, lists:nth(N, Addrs)}}}
                    end;
                Result -> Result
            end
    catch
        throw : Reason ->
            {error, Reason}
    end.

%% ----------------------------------

connect(SockRef, ConnectRef, SockAddr) ->
    try
        enc_sockaddr(SockAddr)
    of
        ESockAddr ->
            case nif_connect(SockRef, ConnectRef, ESockAddr) of
                {invalid, Reason} ->
                    case Reason of
                        sockaddr ->
                            {error, {invalid, {Reason, SockAddr}}}
                    end;
                Result -> Result
            end
    catch
        throw : Reason ->
            {error, Reason}
    end.

connect(SockRef) ->
    nif_connect(SockRef).

%% ----------------------------------

listen(SockRef, Backlog) ->
    nif_listen(SockRef, Backlog).

%% ----------------------------------

accept(ListenSockRef, AccRef) ->
    nif_accept(ListenSockRef, AccRef).

%% ----------------------------------

send(SockRef, Bin, EFlags, SendRef) when is_integer(EFlags) ->
    %% Continuation after select
    case nif_send(SockRef, Bin, EFlags, SendRef) of
        ok ->
            ok;
        {ok, Written} ->
            <<_:Written/binary, RestBin/binary>> = Bin,
            {ok, RestBin};
        select ->
            {select, EFlags};
        {select, Written} ->
            <<_:Written/binary, RestBin/binary>> = Bin,
            {select, RestBin, EFlags};

        completion = C ->
            C;

        {error, _Reason} = Result ->
            Result
    end;
send(SockRef, Bin, Flags, SendRef) ->
    %% First call; encode argument(s)
    try enc_msg_flags(Flags) of
        EFlags ->
            send(SockRef, Bin, EFlags, SendRef)
    catch throw : Reason ->
            {error, Reason}
    end.


sendto(SockRef, Bin, {_, ETo, EFlags} = Cont, SendRef) ->
    %% Continuation after select
    case nif_sendto(SockRef, Bin, ETo, EFlags, SendRef) of
        {invalid, Cause} ->
            case Cause of
                sockaddr ->
                    To = element(1, Cont),
                    {error, {invalid, {Cause, To}}}
            end;
        ok ->
            ok;
        {ok, Written} ->
            <<_:Written/binary, RestBin/binary>> = Bin,
            {ok, RestBin};
        select ->
            {select, Cont};
        {select, Written} ->
            <<_:Written/binary, RestBin/binary>> = Bin,
            {select, RestBin, Cont};
        {error, _Reason} = Result ->
            Result
    end.

sendto(SockRef, Bin, To, Flags, SendRef) ->
    %% First call; encode arguments
    try {enc_sockaddr(To), enc_msg_flags(Flags)} of
        {ETo, EFlags} ->
            case nif_sendto(SockRef, Bin, ETo, EFlags, SendRef) of
                {invalid, Cause} ->
                    case Cause of
                        sockaddr ->
                            {error, {invalid, {Cause, To}}}
                    end;

                ok ->
                    ok;
                {ok, Written} ->
                    <<_:Written/binary, RestBin/binary>> = Bin,
                    {ok, RestBin};

                select ->
                    Cont = {To, ETo, EFlags},
                    {select, Cont};
                {select, Written} ->
                    <<_:Written/binary, RestBin/binary>> = Bin,
                    Cont = {To, ETo, EFlags},
                    {select, RestBin, Cont};

                completion = C->
                    C;

                {error, _Reason} = Result ->
                    Result
            end
    catch throw : Reason ->
            {error, Reason}
    end.


sendmsg(SockRef, RestIOV, {_, EMsg, EFlags} = Cont, SendRef) ->
    %% Continuation after select
    HasWritten = false,
    sendmsg_result(
      SockRef, RestIOV, Cont, SendRef, HasWritten,
      nif_sendmsg(SockRef, EMsg, EFlags, SendRef, RestIOV)).

sendmsg(SockRef, Msg, Flags, SendRef, IOV) ->
    %% First call; encode arguments
    try {enc_msg(Msg), enc_msg_flags(Flags)} of
        {EMsg, EFlags} ->
            HasWritten = false,
            Cont = {Msg, EMsg, EFlags},
            sendmsg_result(
              SockRef, IOV, Cont, SendRef, HasWritten,
              nif_sendmsg(SockRef, EMsg, EFlags, SendRef, IOV))
    catch throw : Reason ->
            {error, Reason}
    end.

sendmsg_result(
  SockRef, IOV, {_, EMsg, EFlags} = Cont, SendRef, HasWritten,
  Result) ->
    %%
    case Result of
        ok ->
            ok;
        {ok, Written} ->
            RestIOV = rest_iov(Written, IOV),
            {ok, RestIOV};
        {invalid, Cause} ->
            Reason = {invalid, sendmsg_invalid(IOV, Cont, Cause)},
            if
                HasWritten ->
                    {error, {Reason, IOV}};
                true ->
                    {error, Reason}
            end;
        {iov, Written} ->
            RestIOV = rest_iov(Written, IOV),
            sendmsg_result(
              SockRef, RestIOV, Cont, SendRef, true,
              nif_sendmsg(SockRef, EMsg, EFlags, SendRef, RestIOV));

        select ->
            if
                HasWritten ->
                    {select, IOV, Cont};
                true ->
                    {select, Cont}
            end;
        {select, Written} ->
            RestIOV = rest_iov(Written, IOV),
            {select, RestIOV, Cont};

        %% We may have previously been able to send part of
        %% the message: Depends on how long the I/O vector is!
        %% A vector of length > IOV_MAX *will* result in a partial
        %% send (and a return of '{iov, Written}').
        %% On Windows, IOV_MAX can be as low 16, so there is a
        %% good chance this will happen (unless the user has
        %% already pruned the I/O vector).
        completion = C ->
            if
                HasWritten ->
                    {C, IOV, undefined};
                true ->
                    {C, undefined}
            end;

        {error, Reason} = Error->
            if
                HasWritten ->
                    {error, {Reason, IOV}};
                true ->
                    Error
            end
    end.

sendmsg_invalid(IOV, Cont, Cause) ->
    if
        Cause =:= addr;
        Cause =:= ctrl ->
            Msg = element(1, Cont),
            %% Keep only the interesting in Msg
            {msg, Cause, maps:with([Cause], Msg)};
        Cause =:= iov ->
            {iov, invalid_iov(IOV, 0)}
    end.

rest_iov(0, []) ->
    [];
rest_iov(Written, [B|IOV]) when Written >= byte_size(B) ->
    rest_iov(Written - byte_size(B), IOV);
rest_iov(Written, [B|IOV]) ->
    <<_:Written/binary, Rest/binary>> = B,
    [Rest|IOV].

%% Get down to what it is about the IOV that is invalid
invalid_iov([], N) ->
    {list, N};
invalid_iov([H|IOV], N) ->
    if
        is_binary(H) ->
            invalid_iov(IOV, N+1);
        true ->
            {element_not_binary, N+1}
    end;
invalid_iov(_, N) ->
    {improper_list, N}.


sendv(SockRef, IOV, SendRef) ->
    sendv_result(
      SockRef, IOV, SendRef, false,
      nif_sendv(SockRef, IOV, SendRef)).

sendv_result(SockRef, IOV, SendRef, HasWritten, Result) ->
    case Result of
        ok ->
            ok;

        {ok, Written} ->
            RestIOV = rest_iov(Written, IOV),
            {ok, RestIOV};

        {iov, Written} ->
            RestIOV = rest_iov(Written, IOV),
            sendv_result(
              SockRef, RestIOV, SendRef, true,
              nif_sendv(SockRef, RestIOV, SendRef));

        select ->
            if
                HasWritten ->
                    %% Cont is not used for sendv
                    {select, IOV, undefined};
                true ->
                    {select, undefined}
            end;
        {select, Written} ->
            RestIOV = rest_iov(Written, IOV),
            %% Cont is not used for sendv
            {select, RestIOV, undefined};

        %% We may have previously been able to send part of
        %% the message: Depends on how long the I/O vector is!
        %% A vector of length > IOV_MAX *will* result in a partial
        %% send (and a return of '{iov, Written}').
        %% On Windows, IOV_MAX can be as low 16, so there is a
        %% good chance this will happen (unless the user has
        %% already pruned the I/O vector).
        completion = C ->
            if
                HasWritten ->
                    {C, IOV, undefined};
                true ->
                    {C, undefined}
            end;

        {error, _Reason} = Result ->
            Result
    end.


sendfile(SockRef, Offset, Count, SendRef) ->
    nif_sendfile(SockRef, SendRef, Offset, Count).

sendfile(SockRef, FileRef, Offset, Count, SendRef) ->
    nif_sendfile(SockRef, SendRef, Offset, Count, FileRef).

sendfile_deferred_close(SockRef) ->
    nif_sendfile(SockRef).

%% ----------------------------------

recv(SockRef, Length, Flags, RecvRef) ->
    try enc_msg_flags(Flags) of
        EFlags ->
	    nif_recv(SockRef, Length, EFlags, RecvRef)
    catch throw : Reason ->
            {error, Reason}
    end.

recvfrom(SockRef, Length, Flags, RecvRef) ->
    try enc_msg_flags(Flags) of
        EFlags ->
            nif_recvfrom(SockRef, Length, EFlags, RecvRef)
    catch throw : Reason ->
            {error, Reason}
    end.

recvmsg(SockRef, BufSz, CtrlSz, Flags, RecvRef) ->
    try enc_msg_flags(Flags) of
        EFlags ->
            case nif_recvmsg(SockRef, BufSz, CtrlSz, EFlags, RecvRef) of
		{ok, #{ctrl := []}} = Result ->
		    Result;
		{ok, #{ctrl := Cmsgs} = Msg} ->
		    {ok, Msg#{ctrl := dec_cmsgs(Cmsgs, p_get(protocols))}};
		Result ->
		    Result
	    end
    catch throw : Reason ->
            {error, Reason}
    end.

%% ----------------------------------

close(SockRef) ->
    nif_close(SockRef).

finalize_close(SockRef) ->    
    nif_finalize_close(SockRef).

%% ----------------------------------

shutdown(SockRef, How) ->
    nif_shutdown(SockRef, How).

%% ----------------------------------

setopt(SockRef, Option, Value) ->
    NativeValue = 0,
    setopt_common(SockRef, Option, Value, NativeValue).

setopt_native(SockRef, Option, Value) ->
    NativeValue = 1,
    setopt_common(SockRef, Option, Value, NativeValue).

setopt_common(SockRef, Option, Value, NativeValue) ->
    case enc_sockopt(Option, NativeValue) of
        undefined ->
            {error, {invalid, {socket_option, Option}}};
        invalid ->
            {error, {invalid, {socket_option, Option}}};
        {NumLevel,NumOpt} ->
            case nif_setopt(SockRef, NumLevel, NumOpt, Value, NativeValue) of
                {invalid, Reason} ->
                    case Reason of
                        socket_option ->
                            {error,
                             {invalid, {socket_option, Option}}};
                        value ->
                            {error,
                             {invalid, {socket_option, Option, Value}}}
                    end;
                Result ->
                    Result
            end
    end.


getopt(SockRef, Option) ->
    NativeValue = 0,
    case enc_sockopt(Option, NativeValue) of
        undefined ->
            {error, {invalid, {socket_option, Option}}};
        invalid ->
            {error, {invalid, {socket_option, Option}}};
        {NumLevel,NumOpt} ->
            case nif_getopt(SockRef, NumLevel, NumOpt) of
                {invalid, Reason} ->
                    case Reason of
                        socket_option ->
                            {error, {invalid, {socket_option, Option}}}
                    end;
                Result ->
                    getopt_result(Result, Option)
            end
    end.

getopt_result({ok, Val} = Result, Option) ->
    case Option of
        {socket,protocol} ->
            if
                is_atom(Val) ->
                    Result;
                is_integer(Val) ->
                    case p_get(protocols) of
                        #{Val := [Protocol | _]} ->
                            {ok, Protocol};
                        #{} ->
                            Result
                    end
            end;
        _ ->
            Result
    end;
getopt_result(Error, _Option) ->
    Error.

getopt_native(SockRef, Option, ValueSpec) ->
    NativeValue = 1,
    try enc_sockopt(Option, NativeValue) of
        undefined ->
            {error, {invalid, {socket_option, Option}}};
        invalid ->
            {error, {invalid, {socket_option, Option}}};
        {NumLevel,NumOpt} ->
            case nif_getopt(SockRef, NumLevel, NumOpt, ValueSpec) of
                {invalid, Reason} ->
                    case Reason of
                        value ->
                            {error, {invalid, {value_spec, ValueSpec}}}
                    end;
                Result -> Result
            end
    catch throw : Reason ->
            {error, Reason}
    end.

%% ----------------------------------

sockname(Ref) ->
    nif_sockname(Ref).

peername(Ref) ->
    nif_peername(Ref).


%% ----------------------------------

ioctl(SRef, GReq) ->
    case enc_ioctl_request(GReq) of
	undefined ->
	    {error, {invalid, {ioctl_request, GReq}}};
	invalid ->
	    {error, {invalid, {ioctl_request, GReq}}};
	GReqNUM ->
	    nif_ioctl(SRef, GReqNUM)
    end.

ioctl(SRef, GReq, Arg) ->
    case enc_ioctl_request(GReq) of
	undefined ->
	    {error, {invalid, {ioctl_request, GReq}}};
	invalid ->
	    {error, {invalid, {ioctl_request, GReq}}};
	GReqNUM ->
	    nif_ioctl(SRef, GReqNUM, Arg)
    end.


ioctl(SRef, SReq, Arg1, Arg2) ->
    case enc_ioctl_request(SReq) of
	undefined ->
	    {error, {invalid, {ioctl_request, SReq}}};
	invalid ->
	    {error, {invalid, {ioctl_request, SReq}}};
	SReqNUM when (SReq =:= sifflags) ->
	    nif_ioctl(SRef, SReqNUM, Arg1, enc_ioctl_flags(Arg2));
	SReqNUM ->
	    nif_ioctl(SRef, SReqNUM, Arg1, Arg2)
    end.


%% ----------------------------------

cancel(SRef, Op, Ref) ->
    nif_cancel(SRef, Op, Ref).


%% ===========================================================================
%% Encode / decode
%%

enc_ioctl_request(GReq) when is_integer(GReq) ->
    GReq;
enc_ioctl_request(GReq) ->
    case p_get(ioctl_requests) of
	#{GReq := GReqNUM} ->
	    GReqNUM;
	#{} ->
	    invalid
    end.

%% Flags: The flags that shall be set or/and reset
%%        #{foo := boolean()}
enc_ioctl_flags(Flags) ->
    enc_ioctl_flags(Flags, p_get(ioctl_flags)).

enc_ioctl_flags(Flags, Table) ->
    F = fun(Flag, SetOrReset, FlagMap) when is_boolean(SetOrReset) ->
		case Table of
		    #{Flag := FlagValue} ->
			FlagMap#{FlagValue => SetOrReset};
		    #{} ->
			invalid_ioctl_flag(Flag)
		end;
	   (Flag, BadSetOrReset, _) ->
		invalid_ioctl_flag({Flag, BadSetOrReset})
	end,
    try maps:fold(F, #{}, Flags)
    catch throw : Reason ->
            {error, Reason}
    end.


%% These clauses should be deprecated
enc_protocol({raw, ProtoNum}) when is_integer(ProtoNum) ->
    ProtoNum;
enc_protocol(default) ->
    0;
%%
enc_protocol(Proto) when is_atom(Proto) ->
    case p_get(protocols) of
        #{Proto := Num} ->
            Num;
        #{} ->
            throw({invalid, {protocol, Proto}})
    end;
enc_protocol(Proto) when is_integer(Proto) ->
    Proto;
enc_protocol(Proto) ->
    %% Neater than a function clause
    erlang:error({invalid, {protocol, Proto}}).


enc_sockaddr(#{family := inet} = SockAddr) ->
    merge_sockaddr(?ESOCK_SOCKADDR_IN_DEFAULTS, SockAddr);
enc_sockaddr(#{family := inet6} = SockAddr) ->
    merge_sockaddr(?ESOCK_SOCKADDR_IN6_DEFAULTS, SockAddr);
enc_sockaddr(#{family := local, path := Path} = SockAddr) ->
  if
      is_list(Path), 0 =< length(Path), length(Path) =< 255 ->
          BinPath = enc_path(Path),
          enc_sockaddr(SockAddr#{path => BinPath});
      is_binary(Path), 0 =< byte_size(Path), byte_size(Path) =< 255 ->
          merge_sockaddr(?ESOCK_SOCKADDR_LOCAL_DEFAULTS, SockAddr);
      true ->
          %% Neater than an if clause
          throw({invalid, {sockaddr, path, SockAddr}})
  end;
enc_sockaddr(#{family := local} = SockAddr) ->
    %% Neater than a function clause
    throw({invalid, {sockaddr, path, SockAddr}});
enc_sockaddr(#{family := unspec} = SockAddr) ->
    merge_sockaddr(?ESOCK_SOCKADDR_UNSPEC_DEFAULTS, SockAddr);
enc_sockaddr(#{family := Native} = SockAddr) when is_integer(Native) ->
    merge_sockaddr(?ESOCK_SOCKADDR_NATIVE_DEFAULTS, SockAddr);
enc_sockaddr(#{family := _} = SockAddr) ->
    SockAddr;
enc_sockaddr(#{} = SockAddr) ->
    throw({invalid, {sockaddr, family, SockAddr}});
enc_sockaddr(SockAddr) ->
    %% Neater than a function clause
    erlang:error({invalid, {sockaddr, SockAddr}}).

merge_sockaddr(Default, SockAddr) ->
    case
        maps:fold(
          fun (Key, _, Acc) ->
                  if
                      is_map_key(Key, Default) ->
                          Acc;
                      true ->
                          [Key | Acc]
                  end
          end, [], SockAddr)
    of
        [] ->
            maps:merge(Default, SockAddr);
        InvalidKeys ->
            throw({invalid, {sockaddr, {keys,InvalidKeys}, SockAddr}})
    end.

%% File names has to be encoded according to
%% the native file encoding
%%
enc_path(Path) ->
    %% These are all BIFs - will not cause code loading
    case unicode:characters_to_binary(Path, file:native_name_encoding()) of
        {error, _Bin, _Rest} ->
            throw({invalid, {path, Path}});
        {incomplete, _Bin1, _Bin2} ->
            throw({invalid, {path, Path}});
        BinPath when is_binary(BinPath) ->
            BinPath
    end.


enc_msg_flags([]) ->
    0;
enc_msg_flags([_|_] = Flags) ->
    enc_msg_flags(Flags, p_get(msg_flags), 0).

enc_msg_flags([], _Table, Val) ->
    Val;
enc_msg_flags([Flag | Flags], Table, Val) when is_atom(Flag) ->
    case Table of
        #{Flag := V} ->
            enc_msg_flags(Flags, Table, Val bor V);
        #{} ->
            throw({invalid, {msg_flag, Flag}})
    end;
enc_msg_flags([Flag | Flags], Table, Val)
  when is_integer(Flag), 0 =< Flag ->
    enc_msg_flags(Flags, Table, Val bor Flag);
enc_msg_flags(Flags, _Table, _Val) ->
    %% Neater than a function clause
    erlang:error({invalid, {msg_flags, Flags}}).


enc_msg(#{ctrl := []} = M) ->
    enc_msg(maps:remove(ctrl, M));
enc_msg(#{} = M) ->
    maps:map(
      fun (addr, Addr) ->
              enc_sockaddr(Addr);
          (ctrl, Cmsgs) ->
              enc_cmsgs(Cmsgs, p_get(protocols));
          (_, V) ->
              V
      end,
      M);
enc_msg(M) ->
    %% Neater than a function clause
    erlang:error({invalid, {msg, M}}).

enc_cmsgs(Cmsgs, Protocols) ->
    [if
	 is_atom(Level) ->
	     case Protocols of
		 #{} when Level =:= socket ->
		     Cmsg;
		 #{Level := L} ->
		     Cmsg#{level := L};
		 #{} ->
		     throw({invalid, {protocol, Level}})
	     end;
	 true ->
	     Cmsg
     end || #{level := Level} = Cmsg <- Cmsgs].


dec_cmsgs(Cmsgs, Protocols) ->
    [case Protocols of
	 #{Level := [L | _]} when is_integer(Level) ->
	     Cmsg#{level := L};
	 #{} ->
	     Cmsg
     end || #{level := Level} = Cmsg <- Cmsgs].


%% Common to setopt and getopt
%%
enc_sockopt({otp = Level, Opt}, 0 = _NativeValue) ->
    case
        case Opt of
            debug               -> ?ESOCK_OPT_OTP_DEBUG;
            iow                 -> ?ESOCK_OPT_OTP_IOW;
            controlling_process -> ?ESOCK_OPT_OTP_CTRL_PROC;
            rcvbuf              -> ?ESOCK_OPT_OTP_RCVBUF;
            rcvctrlbuf          -> ?ESOCK_OPT_OTP_RCVCTRLBUF;
            sndctrlbuf          -> ?ESOCK_OPT_OTP_SNDCTRLBUF;
            fd                  -> ?ESOCK_OPT_OTP_FD;
            meta                -> ?ESOCK_OPT_OTP_META;
            use_registry        -> ?ESOCK_OPT_OTP_USE_REGISTRY;
            select_read         -> ?ESOCK_OPT_OTP_SELECT_READ;
            domain              -> ?ESOCK_OPT_OTP_DOMAIN;
            _                   -> invalid
        end
    of
        invalid       -> invalid;
        NumOpt          -> {Level, NumOpt}
    end;
enc_sockopt({NumLevel,NumOpt} = NumOption, NativeValue)
  when is_integer(NumLevel), is_integer(NumOpt), NativeValue =/= 0 ->
    NumOption;
enc_sockopt({Level,NumOpt}, NativeValue)
  when is_atom(Level), is_integer(NumOpt), NativeValue =/= 0 ->
    if
        Level =:= socket ->
            {socket,NumOpt};
        true ->
            case p_get(protocols) of
                #{Level := NumLevel} ->
                    {NumLevel,NumOpt};
                #{} ->
                    invalid
            end
    end;
enc_sockopt({Level,Opt} = Option, _NativeValue)
  when is_atom(Level), is_atom(Opt) ->
    case p_get(options) of
        #{Option := NumOpt} ->
            NumOpt;
        #{} ->
            invalid
    end;
enc_sockopt(Option, _NativeValue) ->
    %% Neater than a function clause
    invalid_socket_option(Option).


%% ===========================================================================

-spec invalid_socket_option(Opt :: term()) -> no_return().

invalid_socket_option(Opt) ->
    invalid({socket_option, Opt}).

-spec invalid_ioctl_flag(Flag :: term()) -> no_return().

invalid_ioctl_flag(Flag) ->
    invalid({ioctl_flag, Flag}).

-spec invalid(What :: term()) -> no_return().

invalid(What) ->
    throw({invalid, What}).



%% ===========================================================================
%% Persistent term functions
%%

p_put(Name, Value) ->
    persistent_term:put({?MODULE, Name}, Value).

%% Also called from prim_net
p_get(Name) ->
    persistent_term:get({?MODULE, Name}).


%% ===========================================================================
%% NIF functions
%%

nif_info() -> erlang:nif_error(notsup).
nif_info(_SockRef) -> erlang:nif_error(notsup).

nif_command(_Command) -> erlang:nif_error(notsup).

nif_supports() -> erlang:nif_error(notsup).
nif_supports(_Key) -> erlang:nif_error(notsup).

nif_open(_FD, _Opts) -> erlang:nif_error(notsup).
nif_open(_Domain, _Type, _Protocol, _Opts) -> erlang:nif_error(notsup).

nif_bind(_SockRef, _SockAddr) -> erlang:nif_error(notsup).
nif_bind(_SockRef, _SockAddrs, _Action) -> erlang:nif_error(notsup).

nif_connect(_SockRef) -> erlang:nif_error(notsup).
nif_connect(_SockRef, _ConnectRef, _SockAddr) -> erlang:nif_error(notsup).

nif_listen(_SockRef, _Backlog) -> erlang:nif_error(notsup).

nif_accept(_SockRef, _Ref) -> erlang:nif_error(notsup).

nif_send(_SockRef, _Bin, _Flags, _SendRef) -> erlang:nif_error(notsup).
nif_sendto(_SockRef, _Bin, _Dest, _Flags, _SendRef) -> erlang:nif_error(notsup).
nif_sendmsg(_SockRef, _Msg, _Flags, _SendRef, _IOV) -> erlang:nif_error(notsup).
nif_sendv(_SockRef, _IOVec, _SendRef) -> erlang:nif_error(notsup).

nif_sendfile(_SockRef, _SendRef, _Offset, _Count, _InFileRef) ->
    erlang:nif_error(notsup).
nif_sendfile(_SockRef, _SendRef, _Offset, _Count) ->
    erlang:nif_error(notsup).
nif_sendfile(_SockRef) -> erlang:nif_error(notsup).

nif_recv(_SockRef, _Length, _Flags, _RecvRef) -> erlang:nif_error(notsup).
nif_recvfrom(_SockRef, _Length, _Flags, _RecvRef) -> erlang:nif_error(notsup).
nif_recvmsg(_SockRef, _BufSz, _CtrlSz, _Flags, _RecvRef) ->
    erlang:nif_error(notsup).

nif_close(_SockRef) -> erlang:nif_error(notsup).
nif_finalize_close(_SockRef) -> erlang:nif_error(notsup).
nif_shutdown(_SockRef, _How) -> erlang:nif_error(notsup).

nif_setopt(_SockRef, _Lev, _Opt, _Val, _NativeVal) -> erlang:nif_error(notsup).
nif_getopt(_SockRef, _Lev, _Opt) -> erlang:nif_error(notsup).
nif_getopt(_SockRef, _Lev, _Opt, _ValSpec) -> erlang:nif_error(notsup).

nif_sockname(_SockRef) -> erlang:nif_error(notsup).
nif_peername(_SockRef) -> erlang:nif_error(notsup).

nif_ioctl(_SockRef, _GReq)               -> erlang:nif_error(notsup).
nif_ioctl(_SockRef, _GReq, _Arg)         -> erlang:nif_error(notsup).
nif_ioctl(_SockRef, _SReq, _Arg1, _Arg2) -> erlang:nif_error(notsup).

nif_cancel(_SockRef, _Op, _SelectRef) -> erlang:nif_error(notsup).


%% ===========================================================================
