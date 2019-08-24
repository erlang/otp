%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% Information and debug functions.
%%

-module(diameter_dbg).

-export([table/1,
         tables/0,
         fields/1,
         modules/0,
         versions/0,
         version_info/0,
         compiled/0,
         procs/0,
         latest/0,
         nl/0,
         sizes/0]).

-export([diameter_config/0,
         diameter_peer/0,
         diameter_reg/0,
         diameter_request/0,
         diameter_sequence/0,
         diameter_service/0,
         diameter_stats/0]).

-export([pp/1,
         subscriptions/0,
         children/0]).

%% Trace help.
-export([tracer/0, tracer/1,
         p/0, p/1,
         stop/0,
         tpl/1,
         tp/1]).

-include_lib("diameter/include/diameter.hrl").

-define(APP, diameter).
-define(I, diameter_info).

-define(LOCAL, [diameter_config,
                diameter_peer,
                diameter_reg,
                diameter_request,
                diameter_sequence,
                diameter_service,
                diameter_stats]).

-define(VALUES(Rec), tl(tuple_to_list(Rec))).

%% ----------------------------------------------------------
%% # sizes/0
%%
%% Return sizes of named tables.
%% ----------------------------------------------------------

sizes() ->
    [{T, ets:info(T, size)} || T <- ?LOCAL, T /= diameter_peer].

%% ----------------------------------------------------------
%% # table/1
%%
%% Pretty-print a diameter table. Returns the number of records
%% printed, or undefined.
%% ----------------------------------------------------------

table(T)
  when (T == diameter_peer) orelse (T == diameter_reg) ->
    ?I:format(collect(T), fields(T), fun ?I:split/2);

table(Table)
  when is_atom(Table) ->
    case fields(Table) of
        undefined = No ->
            No;
        Fields ->
            ?I:format(Table, Fields, fun split/2)
    end.

split([started, name | Fs], [S, N | Vs]) ->
    {name, [started | Fs], N, [S | Vs]};
split([[F|FT]|Fs], [Rec|Vs]) ->
    [_, V | VT] = tuple_to_list(Rec),
    {F, FT ++ Fs, V, VT ++ Vs};
split([F|Fs], [V|Vs]) ->
    {F, Fs, V, Vs}.

%% ----------------------------------------------------------
%% # TableName/0
%% ----------------------------------------------------------

-define(TABLE(Name), Name() -> table(Name)).

?TABLE(diameter_config).
?TABLE(diameter_peer).
?TABLE(diameter_reg).
?TABLE(diameter_request).
?TABLE(diameter_sequence).
?TABLE(diameter_service).
?TABLE(diameter_stats).

%% ----------------------------------------------------------
%% # tables/0
%%
%% Pretty-print diameter tables from all nodes. Returns the number of
%% records printed.
%% ----------------------------------------------------------

tables() ->
    ?I:format(field(?LOCAL), fun split/3, fun collect/1).

field(Tables) ->
    lists:map(fun(T) -> {T, fields(T)} end, lists:sort(Tables)).

split(_, Fs, Vs) ->
    split(Fs, Vs).

%% ----------------------------------------------------------
%% # modules/0
%% ----------------------------------------------------------

modules() ->
    Path = filename:join([appdir(), atom_to_list(?APP) ++ ".app"]),
    {ok, [{application, ?APP, Attrs}]} = file:consult(Path),
    {modules, Mods} = lists:keyfind(modules, 1, Attrs),
    Mods.

appdir() ->
    [_|_] = code:lib_dir(?APP, ebin).

%% ----------------------------------------------------------
%% # versions/0
%% ----------------------------------------------------------

versions() ->
    ?I:versions(modules()).

%% ----------------------------------------------------------
%% # version_info/0
%% ----------------------------------------------------------

version_info() ->
    ?I:version_info(modules()).

%% ----------------------------------------------------------
%% # compiled/0
%% ----------------------------------------------------------

compiled() ->
    ?I:compiled(modules()).

%% ----------------------------------------------------------
%% # procs/0
%% ----------------------------------------------------------

procs() ->
    ?I:procs(?APP).

%% ----------------------------------------------------------
%% # latest/0
%% ----------------------------------------------------------

latest() ->
    ?I:latest(modules()).

%% ----------------------------------------------------------
%% # nl/0
%% ----------------------------------------------------------

nl() ->
    lists:foreach(fun(M) -> abcast = c:nl(M) end, modules()).

%% ----------------------------------------------------------
%% # pp/1
%%
%% Description: Pretty-print a message binary.
%% ----------------------------------------------------------

%% Network byte order = big endian.

pp(<<Version:8, MsgLength:24,
     Rbit:1, Pbit:1, Ebit:1, Tbit:1, Reserved:4, CmdCode:24,
     ApplId:32,
     HbHid:32,
     E2Eid:32,
     AVPs/binary>>) ->
    ?I:sep(),
    ppp(["Version",
         "Message length",
         "[Actual length]",
         "R(equest)",
         "P(roxiable)",
         "E(rror)",
         "T(Potential retrans)",
         "Reserved bits",
         "Command code",
         "Application id",
         "Hop by hop id",
         "End to end id"],
        [Version, MsgLength, size(AVPs) + 20,
         Rbit, Pbit, Ebit, Tbit, Reserved,
         CmdCode,
         ApplId,
         HbHid,
         E2Eid]),
    N = avp_loop({AVPs, MsgLength - 20}, 0),
    ?I:sep(),
    N;

pp(<<_Version:8, MsgLength:24, _/binary>> = Bin) ->
    {bad_message_length, MsgLength, size(Bin)};

pp(Bin)
  when is_binary(Bin) ->
    {truncated_binary, size(Bin)};

pp(_) ->
    not_binary.

%% avp_loop/2

avp_loop({Bin, Size}, N) ->
    avp_loop(avp(Bin, Size), N+1);
avp_loop(ok, N) ->
    N;
avp_loop([_E, _Rest] = L, N) ->
    io:format("! ~s: ~p~n", L),
    N;
avp_loop([E, Rest, Fmt | Values], N)
  when is_binary(Rest) ->
    io:format("! ~s (" ++ Fmt ++ "): ~p~n", [E|Values] ++ [Rest]),
    N.

%% avp/2

avp(<<>>, 0) ->
    ok;
avp(<<Code:32, Flags:1/binary, Length:24, Rest/binary>>,
    Size) ->
    avp(Code, Flags, Length, Rest, Size);
avp(Bin, _) ->
    ["truncated AVP header", Bin].

%% avp/5

avp(Code, Flags, Length, Rest, Size) ->
    <<V:1, M:1, P:1, Res:5>>
        = Flags,
    b(),
    ppp(["AVP Code",
         "V(endor)",
         "M(andatory)",
         "P(Security)",
         "R(eserved)",
         "Length"],
        [Code, V, M, P, Res, Length]),
    avp(V, Rest, Length - 8, Size - 8).

%% avp/4

avp(1, <<V:32, Data/binary>>, Length, Size) ->
    ppp({"Vendor-ID", V}),
    data(Data, Length - 4, Size - 4);
avp(1, Bin, _, _) ->
    ["truncated Vendor-ID", Bin];
avp(0, Data, Length, Size) ->
    data(Data, Length, Size).

data(Bin, Length, Size)
  when size(Bin) >= Length ->
    <<AVP:Length/binary, Rest/binary>> = Bin,
    ppp({"Data", AVP}),
    unpad(Rest, Size - Length, Length rem 4);

data(Bin, _, _) ->
    ["truncated AVP data", Bin].

%% Remove padding bytes up to the next word boundary.
unpad(Bin, Size, 0) ->
    {Bin, Size};
unpad(Bin, Size, N) ->
    un(Bin, Size, 4 - N).

un(Bin, Size, N)
  when size(Bin) >= N ->
    ppp({"Padding bytes", N}),
    <<Pad:N/binary, Rest/binary>> = Bin,
    Bits = N*8,
    case Pad of
        <<0:Bits>> ->
            {Rest, Size - N};
        _ ->
            ["non-zero padding", Bin, "~p", N]
    end;

un(Bin, _, _) ->
    ["truncated padding", Bin].

b() ->
    io:format("#~n").

ppp(Fields, Values) ->
    lists:foreach(fun ppp/1, lists:zip(Fields, Values)).

ppp({Field, Value}) ->
    io:format(": ~-22s : ~p~n", [Field, Value]).

%% ----------------------------------------------------------
%% # subscriptions/0
%%
%% Returns a list of {SvcName, Pid}.
%% ----------------------------------------------------------

subscriptions() ->
    diameter_service:subscriptions().

%% ----------------------------------------------------------
%% # children/0
%% ----------------------------------------------------------

children() ->
    diameter_sup:tree().

%% ----------------------------------------------------------

%% tracer/[12]

tracer(Port)
  when is_integer(Port) ->
    dbg:tracer(port, dbg:trace_port(ip, Port));

tracer(Path)
  when is_list(Path) ->
    dbg:tracer(port, dbg:trace_port(file, Path)).

tracer() ->
    dbg:tracer(process, {fun p/2, ok}).

p(T,_) ->
    io:format("+ ~p~n", [T]).

%% p/[01]

p() ->
    p([c,timestamp]).

p(T) ->
    dbg:p(all,T).

%% stop/0

stop() ->
    dbg:ctp(),
    dbg:stop_clear().

%% tpl/1
%% tp/1

tpl(T) ->
    dbg(tpl, T).

tp(T) ->
    dbg(tp, T).

%% dbg/2

dbg(F, L)
  when is_list(L) ->
    [dbg(F, X) || X <- L];

dbg(F, M)
  when is_atom(M) ->
    apply(dbg, F, [M, x]);

dbg(F, T)
  when is_tuple(T) ->
    apply(dbg, F, tuple_to_list(T)).

%% ===========================================================================
%% ===========================================================================

%% collect/1

collect(diameter_peer) ->
    lists:flatmap(fun peers/1, diameter:services());

collect(diameter_reg) ->
    diameter_reg:terms();

collect(Name) ->
    c(ets:info(Name), Name).

c(undefined, _) ->
    [];
c(_, Name) ->
    ets:tab2list(Name).

%% peers/1

peers(Name) ->
    peers(Name, diameter:service_info(Name, transport)).

peers(_, undefined) ->
    [];
peers(Name, Ts) ->
    lists:flatmap(fun(T) -> mk_peers(Name, T) end, Ts).

mk_peers(Name, [_, {type, connect} | _] = Ts) ->
    [[Name | mk_peer(Ts)]];
mk_peers(Name, [R, {type, listen}, O, {accept = A, As} | _]) ->
    [[Name | mk_peer([R, {type, A}, O | Ts])] || Ts <- As].
%% This is a bit lame: service_info works to build this list and out
%% of something like what we want here and then we take it apart.

mk_peer(Vs) ->
    [Type, Ref, State, Opts, WPid, TPid, SApps, Caps]
        = get_values(Vs, [type,ref,state,options,watchdog,peer,apps,caps]),
    [Ref, State, [{type, Type} | Opts], s(WPid), s(TPid), SApps, Caps].

get_values(Vs, Ks) ->
    [proplists:get_value(K, Vs) || K <- Ks].

s(undefined = T) ->
    T;
s({Pid, _Started, _State}) ->
    state(Pid);
s({Pid, _Started}) ->
    state(Pid).

%% Collect states from watchdog/transport pids.
state(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {state, self()},
    receive
        {'DOWN', MRef, process, _, _} ->
            Pid;
        {Pid, _} = T ->
            erlang:demonitor(MRef, [flush]),
            T
    end.

%% fields/1

-define(FIELDS(Table), fields(Table) -> record_info(fields, Table)).

fields(diameter_config) ->
    [];

fields(T)
  when T == diameter_request;
       T == diameter_sequence ->
    fun kv/1;

fields(diameter_stats) ->
    fun({Ctr, N}) when not is_pid(Ctr) ->
            {[counter, value], [Ctr, N]};
       (_) ->
            []
    end;

fields(diameter_service) ->
    [started,
     name,
     record_info(fields, diameter_service),
     watchdogT,
     peerT,
     shared_peers,
     local_peers,
     monitor,
     options];

?FIELDS(diameter_event);
?FIELDS(diameter_uri);
?FIELDS(diameter_avp);
?FIELDS(diameter_header);
?FIELDS(diameter_packet);
?FIELDS(diameter_app);
?FIELDS(diameter_caps);

fields(diameter_peer) ->
    [service, ref, state, options, watchdog, peer, applications, capabilities];

fields(diameter_reg) ->
    [property, pids];

fields(_) ->
    undefined.

kv({_,_}) ->
    [key, value];
kv(_) ->
    [].
