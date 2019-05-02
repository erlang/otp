%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
-module(unicode_util_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, extra/1,
         uppercase/1, lowercase/1, titlecase/1, casefold/1,
         cp/1, gc/1,
         nfd/1, nfc/1, nfkd/1, nfkc/1,
         whitespace/1,
         get/1,
         count/1]).

-export([debug/0, id/1, bin_split/1, uc_loaded_size/0,
        time_count/4  %% Used by stdlib_bench_SUITE
        ]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,20}}].

all() ->
    [
     extra,
     uppercase, lowercase, titlecase, casefold,
     cp, gc,
     nfd, nfc, nfkd, nfkc,
     whitespace,
     get,
     count
    ].

debug() ->
    Config = [{data_dir, ?MODULE_STRING++"_data"}],
    [io:format("~p:~p~n",[Test,?MODULE:Test(Config)]) || Test <- all()].

extra(_) ->
    {_, _} = unicode_util:spec_version(),
    #{ccc:=0, compat:=[], canon:=[_,_]} = unicode_util:lookup($å),
    #{fold:=229,lower:=229,title:=197,upper:=197} = unicode_util:get_case($å),
    #{fold:="ss",lower:=223,title:="Ss",upper:="SS"} = unicode_util:get_case($ß),
    ok.

uppercase(_) ->
    [$H] = unicode_util:uppercase([$H]),
    [$H] = unicode_util:uppercase([$h]),
    [$1] = unicode_util:uppercase([$1]),
    ok.

titlecase(_) ->
    [$H] = unicode_util:titlecase([$H]),
    [$H] = unicode_util:titlecase([$h]),
    [$1] = unicode_util:titlecase([$1]),
    ok.

lowercase(_) ->
    [$h] = unicode_util:lowercase([$H]),
    [$h] = unicode_util:lowercase([$h]),
    [$1] = unicode_util:lowercase([$1]),
    [$i] = unicode_util:casefold([$I]), %% no Turkish
    ok.

casefold(_) ->
    [$h] = unicode_util:casefold([$H]),
    [$h] = unicode_util:casefold([$h]),
    [$1] = unicode_util:casefold([$1]),
    [$i] = unicode_util:casefold([$I]),%% no Turkish
    [[$s,$s]|"abC"] = unicode_util:casefold([$ß,$a,$b,$C]),
    [[$s,$s]] = unicode_util:casefold([$ẞ]),
    ok.

whitespace(_) ->
    WS = unicode_util:whitespace(),
    WS = lists:filter(fun unicode_util:is_whitespace/1, WS),
    %% TODO add more tests
    ok.

cp(_) ->
    Get = fun unicode_util:cp/1,
    "hejsan" = fetch("hejsan", Get),
    "hejsan" = fetch(<<"hejsan">>, Get),
    "hejsan" = fetch(["hej",<<"san">>], Get),
    "hejsan" = fetch(["hej"|<<"san">>], Get),
    {error, <<128>>} = Get(<<128>>),
    {error, [<<128>>, 0]} = Get([<<128>>, 0]),
    ok.

gc(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Get = fun unicode_util:gc/1,
    "hejsan" = fetch("hejsan", Get),
    "hejsan" = fetch(<<"hejsan">>, Get),
    "hejsan" = fetch(["hej",<<"san">>], Get),
    "hejsan" = fetch(["hej"|<<"san">>], Get),
    {error, <<128>>} = Get(<<128>>),
    {error, [<<128>>, 0]} = Get([<<128>>, 0]),

    0 = fold(fun verify_gc/3, 0, DataDir ++ "/GraphemeBreakTest.txt"),
    ok.

verify_gc(Line0, N, Acc) ->
    Line = unicode:characters_to_list(Line0),
    Line = fetch(Line0,fun unicode_util:cp/1), %% Test cp
    LineGC = fetch(Line0,fun unicode_util:gc/1), %% Test gc
    LineGC = fetch(Line,fun unicode_util:gc/1), %% Test gc
    LineGC = fetch(LineGC,fun unicode_util:gc/1), %% Test gc
    LineGC = fetch(LineGC,fun unicode_util:cp/1), %% Test cp

    %io:format("Line: ~s~n",[Line]),
    [Data|_Comments] = string:tokens(Line, "#"),
    %% io:format("Data: ~w~n",[string:tokens(Data, " \t")]),
    {Str,Res} = gc_test_data(string:tokens(Data, " \t"), [], [[]]),
    %% io:format("InputStr: ~w ~w~n",[Str,unicode:characters_to_binary(Str)]),
    case verify_gc(Str, Res, N, Line) andalso
        verify_gc(unicode:characters_to_binary(Str), Res, N, Line0) of
        true -> Acc;
        false -> Acc+1
    end.

verify_gc({error,_,[CP|_]}=Err, _Res, N, Line) ->
    IsSurrogate = 16#D800 =< CP andalso CP =< 16#DFFF,
    %% Surrogat is not valid in utf8 encoding only utf16
    IsSurrogate orelse
        io:format("~w: ~ts~n Error in unicode:characters_to_binary ~w~n", [N, Line, Err]),
    IsSurrogate;
verify_gc(Str, Res, N, Line) ->
    try fetch(Str, fun unicode_util:gc/1) of
        Res -> true;
        Other ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[N, Line, Str, Str]),
            io:format("Expected: ~p~n", [Res]),
            io:format("Got: ~w~n", [Other]),
            false
    catch Cl:R:Stacktrace ->
            io:format("~p: ~ts => |~tp|~n",[N, Line, Str]),
            io:format("Expected: ~p~n", [Res]),
            erlang:raise(Cl,R,Stacktrace)
    end.

gc_test_data([[247]|Rest], Str, [First|GCs]) ->
    case First of
        [] -> gc_test_data(Rest, Str, [[]|GCs]);
        [CP] -> gc_test_data(Rest, Str, [[],CP|GCs]);
        _  -> gc_test_data(Rest, Str, [[],lists:reverse(First)|GCs])
    end;
gc_test_data([[215]|Rest], Str, GCs) ->
    gc_test_data(Rest, Str, GCs);
gc_test_data([Hex|Rest], Str, [First|GCs]) ->
    CP = hex_to_int(Hex),
    gc_test_data(Rest, [CP|Str], [[CP|First]|GCs]);
gc_test_data([], Str, [[]|GCs]) ->
    {lists:reverse(Str), lists:reverse(GCs)};
gc_test_data([], Str, GCs) ->
    {lists:reverse(Str), lists:reverse(GCs)}.

nfd(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ok = fold(fun verify_nfd/3, 0, DataDir ++ "/NormalizationTest.txt"),
    ok.

verify_nfd(<<"@Part", _/binary>>, _LineNo, _Acc) -> ok;
verify_nfd(Data0, LineNo, _Acc) ->
    Data1 = unicode:characters_to_list(Data0),
    [Data2|_Comments] = string:tokens(Data1, "#"),
    Columns = string:tokens(Data2, ";"),
    [C1,C2,C3,C4,C5|_] = [[hex_to_int(CP) || CP <- string:tokens(C, " ")] ||
                             C <- Columns],
    C3GC = fetch(C3, fun unicode_util:gc/1),
    try
        C3GC = fetch(C1, fun unicode_util:nfd/1),
        C3GC = fetch(C2, fun unicode_util:nfd/1),
        C3GC = fetch(C3, fun unicode_util:nfd/1)
    catch  _Cl:{badmatch, Other} = _R: Stacktrace ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[LineNo, Data1, C1, C1]),
            io:format("Expected: ~ts ~w~n", [C3GC, C3GC]),
            io:format("Got: ~ts ~w~n", [Other, Other]),
            erlang:raise(_Cl,_R,Stacktrace);
           Cl:R:Stacktrace ->
            io:format("~p: ~ts => |~tp|~n",[LineNo, Data1, C1]),
            io:format("Expected: ~p~n", [C3]),
            erlang:raise(Cl,R,Stacktrace)
    end,
    C5GC = fetch(C5, fun unicode_util:gc/1),
    try
        C5GC = fetch(C4, fun unicode_util:nfd/1),
        C5GC = fetch(C5, fun unicode_util:nfd/1)
    catch  _Cl2:{badmatch, Other2} = _R2:Stacktrace2 ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[LineNo, Data1, C1, C1]),
            io:format("Expected: ~ts ~w~n", [C5GC, C5GC]),
            io:format("Got:      ~ts ~w~n", [Other2, Other2]),
            erlang:raise(_Cl2,_R2,Stacktrace2);
           Cl2:R2:Stacktrace2 ->
            io:format("~p: ~ts => |~tp|~n",[LineNo, Data1, C1]),
            io:format("Expected: ~p~n", [C5]),
            erlang:raise(Cl2,R2,Stacktrace2)
    end,
    ok.

nfc(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ok = fold(fun verify_nfc/3, 0, DataDir ++ "/NormalizationTest.txt"),
    ok.

verify_nfc(<<"@Part", _/binary>>, _LineNo, _Acc) -> ok;
verify_nfc(Data0, LineNo, _Acc) ->
    Data1 = unicode:characters_to_list(Data0),
    [Data2|_Comments] = string:tokens(Data1, "#"),
    Columns = string:tokens(Data2, ";"),
    [C1,C2,C3,C4,C5|_] = [[hex_to_int(CP) || CP <- string:tokens(C, " ")] ||
                             C <- Columns],
    C2GC = fetch(C2, fun unicode_util:gc/1),
    try
        C2GC = fetch(C1, fun unicode_util:nfc/1),
        C2GC = fetch(C2, fun unicode_util:nfc/1),
        C2GC = fetch(C3, fun unicode_util:nfc/1)
    catch  _Cl:{badmatch, Other} = _R:Stacktrace ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[LineNo, Data1, C1, C1]),
            io:format("Expected: ~ts ~w~n", [C2GC, C2GC]),
            io:format("Got:      ~ts ~w~n", [Other, Other]),
            erlang:raise(_Cl,_R,Stacktrace);
           Cl:R:Stacktrace ->
            io:format("~p: ~ts => |~tp|~n",[LineNo, Data1, C1]),
            io:format("Expected: ~p~n", [C3]),
            erlang:raise(Cl,R,Stacktrace)
    end,
    C4GC = fetch(C4, fun unicode_util:gc/1),
    try
        C4GC = fetch(C4, fun unicode_util:nfc/1),
        C4GC = fetch(C5, fun unicode_util:nfc/1)
    catch  _Cl2:{badmatch, Other2} = _R2:Stacktrace2 ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[LineNo, Data1, C1, C1]),
            io:format("Expected: ~ts ~w~n", [C4GC, C4GC]),
            io:format("Got: ~ts ~w~n", [Other2, Other2]),
            erlang:raise(_Cl2,_R2,Stacktrace2);
           Cl2:R2:Stacktrace2 ->
            io:format("~p: ~ts => |~tp|~n",[LineNo, Data1, C1]),
            io:format("Expected: ~p~n", [C5]),
            erlang:raise(Cl2,R2,Stacktrace2)
    end,
    ok.

nfkd(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ok = fold(fun verify_nfkd/3, 0, DataDir ++ "/NormalizationTest.txt"),
    ok.

verify_nfkd(<<"@Part", _/binary>>, _LineNo, _Acc) -> ok;
verify_nfkd(Data0, LineNo, _Acc) ->
    Data1 = unicode:characters_to_list(Data0),
    [Data2|_Comments] = string:tokens(Data1, "#"),
    Columns = string:tokens(Data2, ";"),
    [C1,C2,C3,C4,C5|_] = [[hex_to_int(CP) || CP <- string:tokens(C, " ")] ||
                             C <- Columns],
    C5GC = lists:flatten(fetch(C5, fun unicode_util:gc/1)),
    try
        C5GC = lists:flatten(fetch(C1, fun unicode_util:nfkd/1)),
        C5GC = lists:flatten(fetch(C2, fun unicode_util:nfkd/1)),
        C5GC = lists:flatten(fetch(C3, fun unicode_util:nfkd/1)),
        C5GC = lists:flatten(fetch(C4, fun unicode_util:nfkd/1)),
        C5GC = lists:flatten(fetch(C5, fun unicode_util:nfkd/1))
    catch  _Cl:{badmatch, Other} = _R:Stacktrace ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[LineNo, Data1, C5, C5]),
            io:format("Expected: ~ts ~w~n", [C5GC, C5GC]),
            io:format("Got: ~ts ~w~n", [Other, Other]),
            erlang:raise(_Cl,_R,Stacktrace);
           Cl:R:Stacktrace ->
            io:format("~p: ~ts => |~tp|~n",[LineNo, Data1, C1]),
            io:format("Expected: ~p~n", [C3]),
            erlang:raise(Cl,R,Stacktrace)
    end,
    ok.


nfkc(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ok = fold(fun verify_nfkc/3, 0, DataDir ++ "/NormalizationTest.txt"),
    ok.

verify_nfkc(<<"@Part", _/binary>>, _LineNo, _Acc) -> ok;
verify_nfkc(Data0, LineNo, _Acc) ->
    Data1 = unicode:characters_to_list(Data0),
    [Data2|_Comments] = string:tokens(Data1, "#"),
    Columns = string:tokens(Data2, ";"),
    [C1,C2,C3,C4,C5|_] = [[hex_to_int(CP) || CP <- string:tokens(C, " ")] ||
                             C <- Columns],
    C4GC = lists:flatten(fetch(C4, fun unicode_util:gc/1)),
    try
        C4GC = lists:flatten(fetch(C1, fun unicode_util:nfkc/1)),
        C4GC = lists:flatten(fetch(C2, fun unicode_util:nfkc/1)),
        C4GC = lists:flatten(fetch(C3, fun unicode_util:nfkc/1)),
        C4GC = lists:flatten(fetch(C4, fun unicode_util:nfkc/1)),
        C4GC = lists:flatten(fetch(C5, fun unicode_util:nfkc/1))

    catch  _Cl:{badmatch, Other} = _R:Stacktrace ->
            io:format("Failed: ~p~nInput: ~ts~n\t=> ~w |~ts|~n",[LineNo, Data1, C4, C4]),
            io:format("Expected: ~ts ~w~n", [C4GC, C4GC]),
            io:format("Got:      ~ts ~w~n", [Other, Other]),
            erlang:raise(_Cl,_R,Stacktrace);
           Cl:R:Stacktrace ->
            io:format("~p: ~ts => |~tp|~n",[LineNo, Data1, C1]),
            io:format("Expected: ~p~n", [C3]),
            erlang:raise(Cl,R,Stacktrace)
    end,
    ok.

get(_) ->
    add_get_tests.

count(Config) ->
    Parent = self(),
    Exec = fun() ->
                   do_measure(Config),
                   Parent ! {test_done, self()}
           end,
    ct:timetrap({minutes,5}),
    case ct:get_timetrap_info() of
        {_,{_,Scale}} when Scale > 1 ->
            {skip,{measurments_skipped_debug,Scale}};
        _ -> % No scaling, run at most 2 min
            Tester = spawn(Exec),
            receive {test_done, Tester} -> ok
            after 120000 ->
                    io:format("Timelimit reached stopping~n",[]),
                    exit(Tester, die)
            end,
            ok
    end.

do_measure(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File =  DataDir ++ "/NormalizationTest.txt",
    {ok, Bin} = file:read_file(File),
    Do = fun(Func, Mode) ->
                 {N, Mean, Stddev, Res} = time_count(Func, Mode, Bin, 10),
                 io:format("~4w ~6w ~.10w ~.6wms ±~.2wms #~.2w~n",
                           [Func, Mode, Res, Mean div 1000, Stddev div 1000, N])
         end,
    Do(lref, list),
    Do(bref, binary),
    io:format("----------------------~n"),
    [Do(What,Mode) || What <- [cp, gc, nfd, nfc, nfkd, nfkc], Mode <- [list, deep_l, binary, deep_b]],
    io:format("Size of unicode_util: ~pkB~n",[uc_loaded_size() div 1024]),
    ok.

uc_loaded_size() ->
    uc_loaded_size(binary:split(erlang:system_info(loaded), <<$\n>>, [global])).

uc_loaded_size([<<"unicode_util ", Rest/binary>>|_]) ->
    [StrSize|_] = binary:split(Rest, <<$\s>>),
    binary_to_integer(StrSize);
uc_loaded_size([_|Rest]) ->
    uc_loaded_size(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time_count(Fun, Mode, Bin, Repeat) ->
    timer:sleep(100), %% Let emulator catch up and clean things before test runs
    Self = self(),
    Pid = spawn_link(fun() ->
                             Str = mode(Mode, Bin),
                             Self ! {self(),do_count(0,0,0, Fun, Str, undefined, Repeat)}
                     end),
    receive {Pid,Msg} -> Msg end.

do_count(N,Sum,SumSq, Fun, Str, _, Repeat) when N < Repeat ->
    {Time, Res} = do_count(Fun, Str),
    do_count(N+1,Sum+Time,SumSq+Time*Time, Fun, Str, Res, Repeat);
do_count(N,Sum,SumSq, _, _, Res, _) ->
    Mean = round(Sum / N),
    Stdev = round(math:sqrt((SumSq - (Sum*Sum/N))/(N - 1))),
    {N, Mean, Stdev, Res}.

do_count(Fun, Str) ->
    Count = fun Count(Str0, N) ->
                    case unicode_util:Fun(Str0) of
                        [] -> N;
                        [_|Str1] -> Count(Str1,N+1)
                    end
            end,

    if Fun =/= lref, Fun =/= bref ->
            timer:tc(fun() -> Count(Str, 0) end);
       true ->
            Pick = case Fun of
                       lref -> id(id);
                       bref -> id(bin_split)
                   end,
            Ref = fun LR(Str0, N) ->
                          case ?MODULE:Pick(Str0) of
                              [] -> N;
                              [_|Str1] -> LR(Str1,N+1)
                          end
                  end,
            timer:tc(fun() -> Ref(Str, 0) end)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


id(Op) -> Op.

bin_split(<<>>) -> [];
bin_split(<<CP/utf8,R/binary>>) -> [CP|R].

mode(binary, Bin) -> Bin;
mode(list, Bin) -> unicode:characters_to_list(Bin);
mode(deep_b, Bin) -> [Bin];
mode(deep_l, Bin) -> [unicode:characters_to_list(Bin)].

fetch(Str, F) ->
    case F(Str) of
        [] -> [];
        [CP|R] ->
            %% If input is a binary R should be binary
            if is_binary(Str) == false -> ok;
               is_binary(R); R =:= [] -> ok;
               true ->
                    io:format("Char: ~tc Tail:~tP~n", [CP,R,10]),
                    exit({bug, F})
            end,
            [CP|fetch(R,F)]
    end.

%% *Test.txt file helpers

hex_to_int([]) -> [];
hex_to_int(HexStr) ->
    list_to_integer(string:strip(HexStr, both), 16).

fold(Fun, Acc, File) ->
    io:format("Processing ~s~n",[File]),
    {ok, Fd} = file:open(File, [read, raw, binary, {read_ahead, 100000}]),
    Get = fun() -> file:read_line(Fd) end,
    try
        fold_1(Fun, 1, Acc, Get)
    after
        ok = file:close(Fd)
    end.

fold_1(Fun, Line, Acc, Get) ->
    case Get() of
        eof -> Acc;
        {ok, <<"#",_/binary>>} -> %% Ignore comments
            fold_1(Fun, Line+1, Acc, Get);
        {ok, <<"\n">>} -> %% Ignore empty lines
            fold_1(Fun, Line+1, Acc, Get);
        {ok, Data} ->
            fold_1(Fun, Line+1, Fun(Data, Line, Acc), Get)
    end.
