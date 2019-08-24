%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
%% Generic functions for formatting table listings and more. Used by
%% diameter_dbg.
%%

-module(diameter_info).

-export([usage/1,
         format/1,
         format/2,
         format/3,
         format/4,
         table/2,
         tables/1,
         tables/2,
         split/2,
         split/3,
         tab2list/1,
         modules/1,
         versions/1,
         version_info/1,
         attrs/2,
         compiled/1,
         procs/1,
         latest/1,
         list/1]).

%% Support for rolling your own.
-export([sep/0,
         sep/1,
         widest/1,
         p/1,
         p/3]).

-export([collect/2]).

-define(LONG_TIMEOUT, 30000).
-define(VALUES(Rec), tl(tuple_to_list(Rec))).

%%% ----------------------------------------------------------
%%% # usage(String)
%%% ----------------------------------------------------------

usage(Usage) ->
    sep($+),
    io:format("+ ~p~n", [?MODULE]),
    io:format("~n~s~n~n", [compact(Usage)]),
    sep($+).

%%%
%%% The function format/3, for pretty-printing tables, comes in
%%% several flavours.
%%%

%%% ----------------------------------------------------------
%%% # format(TableName, Fields, SplitFun)
%%%
%%% Input:  TableName = atom() name of table.
%%%
%%%         Fields    = List of field names for the records maintained
%%%                     in the specified table. Can be empty, in which
%%%                     case entries are listed unadorned of field names
%%%                     and SplitFun is unused.
%%%                   | Integer, equivalent to a list with this many '' atoms.
%%%                   | Arity 1 fun mapping a table entry to a Fields list
%%%                     or a tuple {Fields, Values} of lists of the same
%%%                     length.
%%%
%%%                   If Fields is a list then its length must be the same
%%%                   as or one less than the size of the tuples contained
%%%                   in the table. (The values printed then being those
%%%                   in the tuple or record in question.)
%%%
%%%         SplitFun  = Arity 3 fun applied as
%%%
%%%                       SplitFun(TableName, Fields, Values)
%%%
%%%                     in order to obtain a tuple
%%%
%%%                       {Field, RestFields, Value, RestValues}
%%%
%%%                     for which Field/Value will be formatted on
%%%                     STDOUT. (This is to allow a value to be
%%%                     transformed before being output by returning a
%%%                     new value and/or replacing the remainder of
%%%                     the list.) The returned lists must have the
%%%                     same length and Field here is an atom, '' causing
%%%                     a value to be listed unadorned of the field name.
%%%
%%%                     Field can also be list of field names, in
%%%                     which case Value must be a record of the
%%%                     corresponding type.
%%%
%%%                   | Arity 2 fun applied as SplitFun(Fields, Values).
%%%
%%% Output:  Count | undefined
%%%
%%%          Count = Number of entries output.
%%%
%%% Description: Pretty-print records in a named table.
%%% ----------------------------------------------------------

format(Table, Fields, SFun)
  when is_atom(Table), is_function(SFun, 2) ->
    ft(ets:info(Table), Table, SFun, Fields);

format(Table, Fields, SFun)
  when is_atom(Table), is_function(SFun, 3) ->
    format(Table, Fields, fun(Fs,Vs) -> SFun(Table, Fs, Vs) end);

%%% ----------------------------------------------------------
%%% # format(Recs, Fields, SplitFun)
%%%
%%% Input:  Recs     = list of records/tuples
%%%         Fields   = As for format(Table, Fields, SplitFun), a table
%%%                    entry there being a member of Recs.
%%%         SplitFun = Arity 3 fun applied as above but with the TableName
%%%                    replaced by the first element of the records in
%%%                    question.
%%%                  | Arity 2 fun as for format/3.
%%%
%%% Output: length(Recs)
%%%
%%% Description: Pretty print records/tuples.
%%% ----------------------------------------------------------

format(Recs, Fields, SFun)
  when is_list(Recs), is_function(SFun, 3) ->
    lists:foldl(fun(R,A) -> f(recsplit(SFun, R), 0, Fields, R, A) end,
                0,
                Recs);

format(Recs, Fields, SFun)
  when is_list(Recs), is_function(SFun, 2) ->
    lists:foldl(fun(R,A) -> f(SFun, 0, Fields, R, A) end,
                0,
                Recs);

%%% ----------------------------------------------------------
%%% # format(Tables, SplitFun, CollectFun)
%%%
%%% Input:  Tables = list of {TableName, Fields}.
%%%         SplitFun = As for format(Table, Fields, SplitFun).
%%%         CollectFun = arity 1 fun mapping a table name to a list
%%%                      of elements. A non-list can be returned to indicate
%%%                      that the table in question doesn't exist.
%%%
%%% Output: Number of entries output.
%%%
%%% Description: Pretty-print records in a named tables as collected
%%%              from known nodes. Each table listing is preceeded by
%%%              a banner.
%%% ----------------------------------------------------------

format(Tables, SFun, CFun)
  when is_list(Tables), is_function(CFun, 1) ->
    format_remote(Tables,
                  SFun,
                  rpc:multicall(nodes(known),
                                ?MODULE,
                                collect,
                                [CFun, lists:map(fun({T,_}) -> T end, Tables)],
                                ?LONG_TIMEOUT));

%%% ----------------------------------------------------------
%%% # format(LocalTables, RemoteTables, SplitFun, CollectFun)
%%% # format(LocalTables, RemoteTables, SplitFun)
%%%
%%% Input:  LocalTables = list of {TableName, Fields}.
%%%                     | list of {TableName, Recs, Fields}
%%%         RemoteTable = list of {TableName, Fields}.
%%%         SplitFun, CollectFun = As for format(Table, CollectFun, SplitFun).
%%%
%%% Output: Number of entries output.
%%%
%%% Description: Pretty-print records in a named tables as collected
%%%              from local and remote nodes. Each table listing is
%%%              preceded by a banner.
%%% ----------------------------------------------------------

format(Local, Remote, SFun) ->
    format(Local, Remote, SFun, fun tab2list/1).

format(Local, Remote, SFun, CFun)
  when is_list(Local), is_list(Remote), is_function(CFun, 1) ->
    format_local(Local, SFun) + format(Remote, SFun, CFun).

%%% ----------------------------------------------------------
%%% # format(Tables, SplitFun)
%%% ----------------------------------------------------------

format(Tables, SFun)
  when is_list(Tables), (is_function(SFun, 2) or is_function(SFun, 3)) ->
    format(Tables, SFun, fun tab2list/1);

format(Tables, CFun)
  when is_list(Tables), is_function(CFun, 1) ->
    format(Tables, fun split/2, CFun).

%%% ----------------------------------------------------------
%%% # format(Table|Tables)
%%% ----------------------------------------------------------

format(Table)
  when is_atom(Table) ->
    format(Table, [], fun split/2);

format(Tables)
  when is_list(Tables) ->
    format(Tables, fun split/2, fun tab2list/1).

%%% ----------------------------------------------------------
%%% # split(TableName, Fields, Values)
%%%
%%% Description: format/3 SplitFun that does nothing special.
%%% ----------------------------------------------------------

split([F|FT], [V|VT]) ->
    {F, FT, V, VT}.

split(_, Fs, Vs) ->
    split(Fs, Vs).

%%% ----------------------------------------------------------
%%% # tab2list(TableName)
%%%
%%% Description: format/4 CollectFun that extracts records from an
%%%              existing ets table.
%%% ----------------------------------------------------------

tab2list(Table) ->
    case ets:info(Table) of
        undefined = No ->
            No;
        _ ->
            ets:tab2list(Table)
    end.

list(Table) ->
    l(tab2list(Table)).

l(undefined = No) ->
    No;
l(List)
  when is_list(List) ->
    io:format("~p~n", [List]),
    length(List).

%%% ----------------------------------------------------------
%%% # table(TableName, Fields)
%%% ----------------------------------------------------------

table(Table, Fields) ->
    format(Table, Fields, fun split/2).

%%% ----------------------------------------------------------
%%% # tables(LocalTables, RemoteTables)
%%% ----------------------------------------------------------

tables(Local, Remote) ->
    format(Local, Remote, fun split/2).

%%% ----------------------------------------------------------
%%% # tables(Tables)
%%% ----------------------------------------------------------

tables(Tables) ->
    format(Tables, fun split/2).

%%% ----------------------------------------------------------
%%% # modules(Prefix|Prefixes)
%%%
%%% Input: Prefix = atom()
%%%
%%% Description: Return the list of all loaded modules with the
%%%              specified prefix.
%%% ----------------------------------------------------------

modules(Prefix)
  when is_atom(Prefix) ->
    lists:sort(mods(Prefix));

modules(Prefixes)
  when is_list(Prefixes) ->
    lists:sort(lists:flatmap(fun modules/1, Prefixes)).

mods(Prefix) ->
    P = atom_to_list(Prefix),
    lists:filter(fun(M) ->
                         lists:prefix(P, atom_to_list(M))
                 end,
                 erlang:loaded()).

%%% ----------------------------------------------------------
%%% # versions(Modules|Prefix)
%%%
%%% Output: Number of modules listed.
%%%
%%% Description: List the versions of the specified modules.
%%% ----------------------------------------------------------

versions(Modules) ->
    {SysInfo, OsInfo, ModInfo} = version_info(Modules),
    sep(),
    print_sys_info(SysInfo),
    sep(),
    print_os_info(OsInfo),
    sep(),
    print_mod_info(ModInfo),
    sep().

%%% ----------------------------------------------------------
%%% # attrs(Modules|Prefix, Attr|FormatFun)
%%%
%%% Output: Number of modules listed.
%%%
%%% Description: List an attribute from module_info.
%%% ----------------------------------------------------------

attrs(Modules, Attr)
  when is_atom(Attr) ->
    attrs(Modules, fun(W,M) -> attr(W, M, Attr, fun attr/1) end);

attrs(Modules, Fun)
  when is_list(Modules) ->
    sep(),
    W = 2 + widest(Modules),
    N = lists:foldl(fun(M,A) -> Fun(W,M), A+1 end, 0, Modules),
    sep(),
    N;

attrs(Prefix, Fun) ->
    attrs(modules(Prefix), Fun).

%% attr/1

attr(T) when is_atom(T) ->
    atom_to_list(T);
attr(N) when is_integer(N) ->
    integer_to_list(N);
attr(V) ->
    case is_list(V) andalso lists:all(fun is_char/1, V) of
        true ->  %% string
            V;
        false ->
            io_lib:format("~p", [V])
    end.

is_char(C) ->
    0 =< C andalso C < 256.

%% attr/4

attr(Width, Mod, Attr, VFun) ->
    io:format(": ~*s~s~n", [-Width, Mod, attr(Mod, Attr, VFun)]).

attr(Mod, Attr, VFun) ->
    Key = key(Attr),
    try
        VFun(val(Attr, keyfetch(Attr, Mod:module_info(Key))))
    catch
        _:_ ->
            "-"
    end.

attr(Mod, Attr) ->
    attr(Mod, Attr, fun attr/1).

key(time) -> compile;
key(_)    -> attributes.

val(time, {_,_,_,_,_,_} = T) ->
    lists:flatten(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                tuple_to_list(T)));
val(_, [V]) ->
    V.

%%% ----------------------------------------------------------
%%% # compiled(Modules|Prefix)
%%%
%%% Output: Number of modules listed.
%%%
%%% Description: List the compile times of the specified modules.
%%% ----------------------------------------------------------

compiled(Modules)
  when is_list(Modules) ->
    attrs(Modules, fun compiled/2);

compiled(Prefix) ->
    compiled(modules(Prefix)).

compiled(Width, Mod) ->
    io:format(": ~*s~19s  ~s~n", [-Width,
                                  Mod,
                                  attr(Mod, time),
                                  opt(attr(Mod, date))]).

opt("-") ->
    "";
opt(D) ->
    "(" ++ D ++ ")".

%%% ----------------------------------------------------------
%%% # procs(Pred|Prefix|Prefixes|Pid|Pids)
%%%
%%% Input:  Pred = arity 2 fun returning true|false when applied to a
%%%                pid and its process info.
%%%
%%% Output: Number of processes listed.
%%%
%%% Description: List process info for all local processes that test
%%%              true with the specified predicate. With the prefix
%%%              form, those processes that are either currently
%%%              executing in, started executing in, or have a
%%%              registered name with a specified prefix are listed.
%%%              With the pid forms, only those process that are local
%%%              are listed and those that are dead list only the pid
%%%              itself.
%%% ----------------------------------------------------------

procs(Pred)
  when is_function(Pred, 2) ->
    procs(Pred, erlang:processes());

procs([]) ->
    0;

procs(Prefix)
  when is_atom(Prefix) ->
    procs(fun(_,I) -> info(fun pre1/2, I, atom_to_list(Prefix)) end);

procs(Prefixes)
  when is_atom(hd(Prefixes)) ->
    procs(fun(_,I) -> info(fun pre/2, I, Prefixes) end);

procs(Pid)
  when is_pid(Pid) ->
    procs(fun true2/2, [Pid]);

procs(Pids)
  when is_list(Pids) ->
    procs(fun true2/2, Pids).

true2(_,_) ->
    true.

%% procs/2

procs(Pred, Pids) ->
    Procs = lists:foldl(fun(P,A) ->
                                procs_acc(Pred, P, catch process_info(P), A)
                        end,
                        [],
                        Pids),
    sep(0 < length(Procs)),
    lists:foldl(fun(T,N) -> p(T), sep(), N+1 end, 0, Procs).

procs_acc(_, Pid, undefined, Acc) ->  %% dead
    [[{pid, Pid}] | Acc];
procs_acc(Pred, Pid, Info, Acc)
  when is_list(Info) ->
    p_acc(Pred(Pid, Info), Pid, Info, Acc);
procs_acc(_, _, _, Acc) ->
    Acc.

p_acc(true, Pid, Info, Acc) ->
    [[{pid, Pid} | Info] | Acc];
p_acc(false, _, _, Acc) ->
    Acc.

%% info/3

info(Pred, Info, T) ->
    lists:any(fun(I) -> i(Pred, I, T) end, Info).

i(Pred, {K, {M,_,_}}, T)
  when K == current_function;
       K == initial_call ->
    Pred(M,T);
i(Pred, {registered_name, N}, T) ->
    Pred(N,T);
i(_,_,_) ->
    false.

pre1(A, Pre) ->
    lists:prefix(Pre, atom_to_list(A)).

pre(A, Prefixes) ->
    lists:any(fun(P) -> pre1(A, atom_to_list(P)) end, Prefixes).

%%% ----------------------------------------------------------
%%% # latest(Modules|Prefix)
%%%
%%% Output: {Mod, {Y,M,D,HH,MM,SS}, Version}
%%%
%%% Description: Return the compile time of the most recently compiled
%%%              module from the specified non-empty list. The modules
%%%              are assumed to exist.
%%% ----------------------------------------------------------

latest(Prefix)
  when is_atom(Prefix) ->
    latest(modules(Prefix));

latest([_|_] = Modules) ->
    {Mod, T}
        = hd(lists:sort(fun latest/2, lists:map(fun compile_time/1, Modules))),
    {Mod, T, app_vsn(Mod)}.

app_vsn(Mod) ->
    keyfetch(app_vsn, Mod:module_info(attributes)).

compile_time(Mod) ->
    T = keyfetch(time, Mod:module_info(compile)),
    {Mod, T}.

latest({_,T1},{_,T2}) ->
    T1 > T2.

%%% ----------------------------------------------------------
%%% version_info(Modules|Prefix)
%%%
%%% Output: {SysInfo, OSInfo, [ModInfo]}
%%%
%%%         SysInfo = {Arch, Vers}
%%%         OSInfo  = {Vers, {Fam, Name}}
%%%         ModInfo = {Vsn, AppVsn, Time, CompilerVsn}
%%% ----------------------------------------------------------

version_info(Prefix)
  when is_atom(Prefix) ->
    version_info(modules(Prefix));

version_info(Mods)
  when is_list(Mods)  ->
    {sys_info(), os_info(), [{M, mod_version_info(M)} || M <- Mods]}.

mod_version_info(Mod) ->
    try
        Info = Mod:module_info(),
        [[Vsn], AppVsn] = get_values(attributes, [vsn, app_vsn], Info),
        [Ver, Time] = get_values(compile, [version, time], Info),
        [Vsn, AppVsn, Ver, Time]
    catch
        _:_ ->
            []
    end.

get_values(Attr, Keys, Info) ->
    As   = proplists:get_value(Attr, Info),
    [proplists:get_value(K, As, "?") || K <- Keys].

sys_info() ->
    [A,V] = [chomp(erlang:system_info(K)) || K <- [system_architecture,
                                                   system_version]],
    {A,V}.

os_info() ->
    {os:version(), os:type()}.

chomp(S) ->
    string:strip(S, right, $\n).

print_sys_info({Arch, Ver}) ->
    io:format("System info:~n"
              "   architecture : ~s~n"
              "   version      : ~s~n",
              [Arch, Ver]).

print_os_info({Vsn, {Fam, Name}}) ->
    io:format("OS info:~n"
              "   family  : ~s ~s~n"
              "   version : ~s~n",
              [str(Fam), bkt(str(Name)), vsn(Vsn)]).

print_mod_info(Mods) ->
    io:format("Module info:~n", []),
    lists:foreach(fun print_mod/1, Mods).

print_mod({Mod, []}) ->
    io:format("   ~w:~n", [Mod]);
print_mod({Mod, [Vsn, AppVsn, Ver, {Year, Month, Day, Hour, Min, Sec}]}) ->
    Time = io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                         [Year, Month, Day, Hour, Min, Sec]),
    io:format("   ~w:~n"
              "      vsn      : ~s~n"
              "      app_vsn  : ~s~n"
              "      compiled : ~s~n"
              "      compiler : ~s~n",
              [Mod, str(Vsn), str(AppVsn), Time, Ver]).

str(A)
  when is_atom(A) ->
    atom_to_list(A);
str(S)
  when is_list(S) ->
    S;
str(T) ->
    io_lib:format("~p", [T]).

bkt("" = S) ->
    S;
bkt(S) ->
    [$[, S, $]].

vsn(T) when is_tuple(T) ->
    case [[$., integer_to_list(N)] || N <- tuple_to_list(T)] of
        [[$.,S] | Rest] ->
            [S | Rest];
        [] = S ->
            S
    end;
vsn(T) ->
    str(T).

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------

%% p/1

p(Info) ->
    W = 2 + widest([K || {K,_} <- Info]),
    lists:foreach(fun({K,V}) -> p(W,K,V) end, Info).

p(Width, Key, Value) ->
    io:format(": ~*s: ~p~n", [-Width, Key, Value]).

%% sep/[01]

sep() ->
    sep($#).

sep(true) ->
    sep();
sep(false) ->
    ok;

sep(Ch) ->
    io:format("~c~65c~n", [Ch, $-]).

%% widest/1

widest(List) ->
    lists:foldl(fun widest/2, 0, List).

widest(T, Max)
  when is_atom(T) ->
    widest(atom_to_list(T), Max);

widest(T, Max)
  when is_integer(T) ->
    widest(integer_to_list(T), Max);

widest(T, Max)
  when is_list(T) ->  %% string
    max(length(T), Max).

pt(T) ->
    io:format(": ~p~n", [T]).

recsplit(SFun, Rec) ->
    fun(Fs,Vs) -> SFun(element(1, Rec), Fs, Vs) end.

keyfetch(Key, List) ->
    {Key,V} = lists:keyfind(Key, 1, List),
    V.

%% ft/4

ft(undefined = No, _, _, _) ->
    No;

ft(_, Table, SFun, Fields) ->
    ets:foldl(fun(R,A) ->
                      f(SFun, 0, Fields, R, A)
              end,
              0,
              Table).

%% f/5

f(SFun, Width, Fields, Rec, Count) ->
    ff(SFun, Width, fields(Fields, Rec), Rec, Count).

ff(SFun, Width, Fields, Rec, Count) ->
    sep(0 == Count),
    f(SFun, Width, Fields, Rec),
    sep(),
    Count+1.

fields(N, _)
  when is_integer(N), N >= 0 ->
    lists:duplicate(N, '');     %% list values unadorned
fields(Fields, R)
  when is_function(Fields, 1) ->
    fields(Fields(R), R);
fields({Fields, Values} = T, _)
  when length(Fields) == length(Values) ->
    T;
fields(Fields, _)
  when is_list(Fields) ->
    Fields.                     %% list field/value pairs, or tuples if []

%% f/4

%% Empty fields list: just print the entry.
f(_, _, [], Rec)
  when is_tuple(Rec) ->
    pt(Rec);

%% Otherwise list field names/values.
f(SFun, Width, {Fields, Values}, _) ->
    f(SFun, Width, Fields, Values);

f(SFun, Width, Fields, Rec)
  when is_tuple(Rec) ->
    f(SFun, Width, Fields, values(Fields, Rec));

f(_, _, [], []) ->
    ok;

f(SFun, Width, [HF | _] = Fields, Values) ->
    {F, FT, V, VT} = SFun(Fields, Values),
    if is_list(F) -> %% V is a record
            break($>, HF),
            f(SFun, Width, F, values(F,V)),
            break($<, HF),
            f(SFun, Width, FT, VT);
       F == '' ->    %% no field name: just list value
            pt(V),
            f(SFun, Width, FT, VT);
       true ->       %% list field/value.
            W = max(Width, 1 + widest(Fields)),
            p(W, F, V),
            f(SFun, W, FT, VT)
    end.

values(Fields, Rec)
  when length(Fields) == size(Rec) - 1 ->
    ?VALUES(Rec);
values(Fields, T)
  when length(Fields) == size(T) ->
    tuple_to_list(T).

%% format_local/2

format_local(Tables, SFun) ->
    lists:foldl(fun(T,A) -> fl(SFun, T, A) end, 0, Tables).

fl(SFun, {Table, Recs, Fields}, Count) ->
    sep(),
    io:format("# ~p~n", [Table]),
    N = fmt(Recs, Fields, SFun),
    sep(0 == N),
    Count + N;

fl(SFun, {Table, Fields}, Count) ->
    fl(SFun, {Table, Table, Fields}, Count).

%% fmt/3

fmt(T, Fields, SFun) ->
    case format(T, Fields, SFun) of
        undefined ->
            0;
        N ->
            N
    end.

%% break/2

break(C, T) ->
    io:format("~c ~p~n", [C, T]).

%% collect/2
%%
%% Output: {[{TableName, Recs}, ...], node()}

collect(CFun, TableNames) ->
    {lists:foldl(fun(N,A) -> c(CFun, N, A) end, [], TableNames), node()}.

c(CFun, TableName, Acc) ->
    case CFun(TableName) of
        Recs when is_list(Recs) ->
            [{TableName, Recs} | Acc];
        _ ->
            Acc
    end.

%% format_remote/3

format_remote(Tables, SFun, {Replies, BadNodes}) ->
    N = lists:foldl(fun(T,A) -> fr(Tables, SFun, T, A) end,
                    0,
                    Replies),
    sep(0 == N andalso [] /= BadNodes),
    lists:foreach(fun(Node) -> io:format("# no reply from ~p~n", [Node]) end,
                  BadNodes),
    sep([] /= BadNodes),
    N.

fr(Tables, SFun, {List, Node}, Count)
  when is_list(List) ->  %% guard against {badrpc, Reason}
    lists:foldl(fun({T,Recs}, C) -> fr(Tables, SFun, Node, T, Recs,C) end,
                Count,
                List);
fr(_, _, _, Count) ->
    Count.

fr(Tables, SFun, Node, Table, Recs, Count) ->
    Fields = keyfetch(Table, Tables),
    sep(),
    io:format("# ~p@~p~n", [Table, Node]),
    N = format(Recs, Fields, tblsplit(SFun, Table)),
    sep(0 == N),
    Count + N.

tblsplit(SFun, Table)
  when is_function(SFun, 3) ->
    fun(Fs,Vs) -> SFun(Table, Fs, Vs) end;
tblsplit(SFun, _)
  when is_function(SFun, 2) ->
    SFun.

%% compact/1
%%
%% Strip whitespace from both ends of a string.

compact(Str) ->
    compact(Str, true).

compact([Ch|Rest], B)
  when Ch == $\n;
       Ch == $ ;
       Ch == $\t;
       Ch == $\v;
       Ch == $\r ->
    compact(Rest, B);

compact(Str, false) ->
    Str;

compact(Str, true) ->
    lists:reverse(compact(lists:reverse(Str), false)).
