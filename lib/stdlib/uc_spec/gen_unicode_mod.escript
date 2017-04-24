#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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

-mode(compile).

-record(cp, {name, class, dec, comp, cs}).
-define(MOD, "unicode_util").

main(_) ->
    %%  Parse main table
    {ok, UD} = file:open("../uc_spec/UnicodeData.txt", [read, raw, {read_ahead, 1000000}]),
    Data0 = foldl(fun parse_unicode_data/2, [], UD),
    Data1  = array:from_orddict(lists:reverse(Data0)),
    ok = file:close(UD),

    %%  Special Casing table
    {ok, SC} = file:open("../uc_spec/SpecialCasing.txt", [read, raw, {read_ahead, 1000000}]),
    Data2 = foldl(fun parse_special_casing/2, Data1, SC),
    ok = file:close(SC),
    %%  Casing Folding table
    {ok, CF} = file:open("../uc_spec/CaseFolding.txt", [read, raw, {read_ahead, 1000000}]),
    Data = foldl(fun parse_case_folding/2, Data2, CF),
    ok = file:close(CF),

    %% Normalization
    {ok, ExclF} = file:open("../uc_spec/CompositionExclusions.txt", [read, raw, {read_ahead, 1000000}]),
    ExclData = foldl(fun parse_comp_excl/2, Data, ExclF),
    ok = file:close(ExclF),

    %%  GraphemeBreakProperty table
    {ok, GBPF} = file:open("../uc_spec/GraphemeBreakProperty.txt", [read, raw, {read_ahead, 1000000}]),
    Props0 = foldl(fun parse_properties/2, [], GBPF),
    ok = file:close(GBPF),
    {ok, PropF} = file:open("../uc_spec/PropList.txt", [read, raw, {read_ahead, 1000000}]),
    Props1 = foldl(fun parse_properties/2, Props0, PropF),
    ok = file:close(PropF),
    Props = sofs:to_external(sofs:relation_to_family(sofs:relation(Props1))),

    %% Make module
    {ok, Out} = file:open(?MOD++".erl", [write]),
    gen_file(Out, Data, ExclData, maps:from_list(Props)),
    ok = file:close(Out),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_unicode_data(Line0, Acc) ->
    Line = string:strip(Line0, right, $\n),
    [CodePoint,Name,_Cat,Class,_BiDi,Decomp,
     _N1,_N2,_N3,_BDMirror,_Uni1,_Iso|Case] = tokens(Line, ";"),
    {Dec,Comp} = case to_decomp(Decomp) of
                     {_, _} = Compabil -> {[], Compabil};
                     Canon -> {Canon, []}
                 end,
    [{hex_to_int(CodePoint),
      #cp{name=list_to_binary(Name),class=to_class(Class),
          dec=Dec, comp=Comp, cs=to_case(Case)}}
     |Acc].

to_class(String) ->
    list_to_integer(string:strip(String, both)).

to_decomp("") -> [];
to_decomp("<" ++ Str) ->
    [Tag,Rest]  = string:tokens(Str, ">"),
    {list_to_atom(Tag), to_decomp(Rest)};
to_decomp(CodePoints) ->
    CPL = string:tokens(CodePoints, " "),
    [hex_to_int(CP) || CP <- CPL].

to_case(["","",""]) -> [];
to_case([Upper,Lower,Title]) ->
    {hex_to_int(Upper),hex_to_int(Lower),hex_to_int(Title),[]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_special_casing("03A3;" ++ _, Acc) ->
    %% Break for conditional handling, which we don't support
    {done, Acc};
parse_special_casing(Line, Table) ->
    [CodePoint|CaseStr] = tokens(Line, ";"),
    CP = hex_to_int(CodePoint),
    Entry = array:get(CP, Table),
    Case = to_scase(CaseStr),
    array:set(CP, Entry#cp{cs=Case}, Table).

to_scase([Lower,Title,Upper|_]) ->
    {unlist([hex_to_int(CP) || CP <- string:strip(string:tokens(Upper, " "), both)]),
     unlist([hex_to_int(CP) || CP <- string:strip(string:tokens(Lower, " "), both)]),
     unlist([hex_to_int(CP) || CP <- string:strip(string:tokens(Title, " "), both)]),
     []}.

parse_case_folding(Line, Table) ->
    [CodePoint, Class0, CaseStr |_Comments] = tokens(Line, ";"),
    Class = string:strip(Class0, both),
    if Class =:= "T" -> Table; %% Do not support localization yet
       Class =:= "S" -> Table; %% Ignore simple
       true ->
            CP = hex_to_int(CodePoint),
            Case = unlist([hex_to_int(CPC) ||
                              CPC <- string:strip(string:tokens(CaseStr, " "), both)]),
            #cp{cs={U,L,T,_}} = Entry = array:get(CP, Table),
            array:set(CP, Entry#cp{cs={U,L,T,Case}}, Table)
    end.

unlist([A]) -> A;
unlist(A) -> A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_comp_excl(Line, Table) ->
    [CodePoint|_Comments] = tokens(Line, "#"),
    CP = hex_to_int(CodePoint),
    Entry = array:get(CP, Table),
    array:set(CP, Entry#cp{dec=none}, Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_properties(Line0, Acc) ->
    [Line|_Comments] = tokens(Line0, "#"),
    [CodePoints, Class] = tokens(Line, ";"),
    case tokens(CodePoints, ".") of
        [CodePoint] ->
            [{to_atom(Class), {hex_to_int(CodePoint), undefined}}|Acc];
        [CodePoint1,"",CodePoint2] ->
            [{to_atom(Class), {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}}|Acc]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_file(Fd, Data, ExclData, Props) ->
    gen_header(Fd),
    gen_static(Fd),
    gen_norm(Fd),
    gen_ws(Fd, Props),
    gen_cp(Fd),
    gen_gc(Fd, Props),
    gen_compose_pairs(Fd, ExclData, Data),
    gen_case_table(Fd, Data),
    gen_unicode_table(Fd, Data),
    ok.

gen_header(Fd) ->
    io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
    io:put_chars(Fd, "%% see ../uc_spec/gen_unicode_mod.escript\n\n"),
    io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
    io:put_chars(Fd, "-export([cp/1, gc/1]).\n"),
    io:put_chars(Fd, "-export([nfd/1, nfc/1, nfkd/1, nfkc/1]).\n"),
    io:put_chars(Fd, "-export([whitespace/0, is_whitespace/1]).\n"),
    io:put_chars(Fd, "-export([uppercase/1, lowercase/1, titlecase/1, casefold/1]).\n\n"),
    io:put_chars(Fd, "-export([spec_version/0, lookup/1, get_case/1]).\n"),
    io:put_chars(Fd, "-inline([class/1]).\n"),
    io:put_chars(Fd, "-compile(nowarn_unused_vars).\n"),
    io:put_chars(Fd, "-dialyzer({no_improper_lists, cp/1}).\n"),
    io:put_chars(Fd, "-type gc() :: char()|[char()].\n\n\n"),
    ok.

gen_static(Fd) ->
    io:put_chars(Fd, "-spec lookup(char()) -> #{'canon':=[{byte(),char()}], 'ccc':=byte(), "
                 "'compat':=[] | {atom(),[{byte(),char()}]}}.\n"),
    io:put_chars(Fd, "lookup(Codepoint) ->\n"
                 "    {CCC,Can,Comp} = unicode_table(Codepoint),\n"
                 "    #{ccc=>CCC, canon=>Can, compat=>Comp}.\n\n"),
    io:put_chars(Fd, "-spec get_case(char()) -> #{'fold':=gc(), 'lower':=gc(), 'title':=gc(), 'upper':=gc()}.\n"),
    io:put_chars(Fd, "get_case(Codepoint) ->\n"
                 "    case case_table(Codepoint) of\n"
                 "        {U,L} -> #{upper=>U,lower=>L,title=>U,fold=>L};\n"
                 "        {U,L,T,F} -> #{upper=>U,lower=>L,title=>T,fold=>F}\n"
                 "    end.\n\n"),
    io:put_chars(Fd, "spec_version() -> {9,0}.\n\n\n"),
    io:put_chars(Fd, "class(Codepoint) -> {CCC,_,_} = unicode_table(Codepoint),\n    CCC.\n\n"),
    io:put_chars(Fd, "-spec uppercase(unicode:chardata()) -> "
                 "maybe_improper_list(gc(),unicode:chardata()).\n"),
    io:put_chars(Fd, "uppercase(Str0) ->\n"),
    io:put_chars(Fd, "    case cp(Str0) of\n"),
    io:put_chars(Fd, "        [CP|Str] = Str1 ->\n"),
    io:put_chars(Fd, "            case case_table(CP) of\n"),
    io:put_chars(Fd, "                {Upper,_} -> [Upper|Str];\n"),
    io:put_chars(Fd, "                {Upper,_,_,_} -> [Upper|Str]\n"),
    io:put_chars(Fd, "            end;\n"),
    io:put_chars(Fd, "        [] -> []\n"),
    io:put_chars(Fd, "    end.\n\n"),
    io:put_chars(Fd, "-spec lowercase(unicode:chardata()) -> "
                 "maybe_improper_list(gc(),unicode:chardata()).\n"),
    io:put_chars(Fd, "lowercase(Str0) ->\n"),
    io:put_chars(Fd, "    case cp(Str0) of\n"),
    io:put_chars(Fd, "        [CP|Str] = Str1 ->\n"),
    io:put_chars(Fd, "            case case_table(CP) of\n"),
    io:put_chars(Fd, "                {_,Lower} -> [Lower|Str];\n"),
    io:put_chars(Fd, "                {_,Lower,_,_} -> [Lower|Str]\n"),
    io:put_chars(Fd, "            end;\n"),
    io:put_chars(Fd, "        [] -> []\n"),
    io:put_chars(Fd, "    end.\n\n"),
    io:put_chars(Fd, "-spec titlecase(unicode:chardata()) -> "
                 "maybe_improper_list(gc(),unicode:chardata()).\n"),
    io:put_chars(Fd, "titlecase(Str0) ->\n"),
    io:put_chars(Fd, "    case cp(Str0) of\n"),
    io:put_chars(Fd, "        [CP|Str] = Str1 ->\n"),
    io:put_chars(Fd, "            case case_table(CP) of\n"),
    io:put_chars(Fd, "                {_,_,Title,_} -> [Title|Str];\n"),
    io:put_chars(Fd, "                {Upper,_} -> [Upper|Str]\n"),
    io:put_chars(Fd, "            end;\n"),
    io:put_chars(Fd, "        [] -> []\n"),
    io:put_chars(Fd, "    end.\n\n"),
    io:put_chars(Fd, "-spec casefold(unicode:chardata()) -> "
                 "maybe_improper_list(gc(),unicode:chardata()).\n"),
    io:put_chars(Fd, "casefold(Str0) ->\n"),
    io:put_chars(Fd, "    case cp(Str0) of\n"),
    io:put_chars(Fd, "        [CP|Str] = Str1 ->\n"),
    io:put_chars(Fd, "            case case_table(CP) of\n"),
    io:put_chars(Fd, "                {_,_,_,Fold} -> [Fold|Str];\n"),
    io:put_chars(Fd, "                {_,Lower} -> [Lower|Str]\n"),
    io:put_chars(Fd, "            end;\n"),
    io:put_chars(Fd, "        [] -> []\n"),
    io:put_chars(Fd, "    end.\n\n"),

    ok.

gen_norm(Fd) ->
    io:put_chars(Fd,
                 "-spec nfd(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).\n"
                 "nfd(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when GC < 127 -> [GC|R];\n"
                 "        [GC|Str] -> [decompose(GC)|Str];\n"
                 "        [] -> []\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "-spec nfkd(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).\n"
                 "nfkd(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when GC < 127 -> [GC|R];\n"
                 "        [GC|Str] -> [decompose_compat(GC)|Str];\n"
                 "        [] -> []\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "-spec nfc(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).\n"
                 "nfc(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when GC < 255 -> [GC|R];\n"
                 "        [GC|Str] -> [compose(decompose(GC))|Str];\n"
                 "        [] -> []\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "-spec nfkc(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).\n"
                 "nfkc(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when GC < 127 -> [GC|R];\n"
                 "        [GC|Str] -> [compose_compat_0(decompose_compat(GC))|Str];\n"
                 "        [] -> []\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "decompose(CP) when is_integer(CP), CP < 16#AC00, 16#D7A3 > CP ->\n"
                 "    case unicode_table(CP) of\n"
                 "        {_,[],_} -> CP;\n"
                 "        {_,CPs,_} -> canonical_order(CPs)\n"
                 "    end;\n"
                 "decompose(CP) ->\n"
                 "   canonical_order(decompose_1(CP)).\n"
                 "\n"
                 "decompose_1(CP) when 16#AC00 =< CP, CP =< 16#D7A3 ->\n"
                 "    Syll = CP-16#AC00,\n"
                 "    T = 28,\n"
                 "    N = 588,\n"
                 "    Lead = 16#1100 + Syll div N,\n"
                 "    Vowel = 16#1161 + (Syll rem N) div T,\n"
                 "    case Syll rem T of\n"
                 "        0 -> [{0,Lead},{0,Vowel}];\n"
                 "        Trail -> [{0,Lead}, {0,Vowel}, {0,Trail+16#11A7}]\n"
                 "    end;\n"
                 "decompose_1(CP) when is_integer(CP) ->\n"
                 "    case unicode_table(CP) of\n"
                 "        {CCC, [],_} -> [{CCC,CP}];\n"
                 "        {_, CPs, _} -> CPs\n"
                 "    end;\n"
                 "decompose_1([CP|CPs]) ->\n"
                 "    decompose_1(CP) ++ decompose_1(CPs);\n"
                 "decompose_1([]) -> [].\n"
                 "\n"
                 "canonical_order([{_,CP}]) -> CP;\n"
                 "canonical_order(CPs) ->\n"
                 "    canonical_order_1(CPs).\n"
                 "\n"
                 "canonical_order_1([{0,CP}|TaggedCPs]) ->\n"
                 "    [CP|canonical_order_1(TaggedCPs)];\n"
                 "canonical_order_1([_|_]=TaggedCPs) ->\n"
                 "    canonical_order_2(TaggedCPs, []);\n"
                 "canonical_order_1([]) -> [].\n"
                 "\n"
                 "canonical_order_2([{CCC,_}=First|Cont], Seq) when CCC > 0 ->\n"
                 "    canonical_order_2(Cont, [First|Seq]);\n"
                 "canonical_order_2(Cont, Seq) ->\n"
                 "     [CP || {_, CP} <- lists:keysort(1,lists:reverse(Seq))] ++ canonical_order_1(Cont).\n\n"),

    io:put_chars(Fd,
                 "decompose_compat(CP) when is_integer(CP), CP < 16#AC00, 16#D7A3 > CP ->\n"
                 "    case unicode_table(CP) of\n"
                 "        {_, [], []} -> CP;\n"
                 "        {_, _, {_,CPs}} -> canonical_order(CPs);\n"
                 "        {_, CPs, _} -> canonical_order(CPs)\n"
                 "    end;\n"
                 "decompose_compat(CP) ->\n"
                 "   canonical_order(decompose_compat_1(CP)).\n"
                 "\n"
                 "decompose_compat_1(CP) when 16#AC00 =< CP, CP =< 16#D7A3 ->\n"
                 "    Syll = CP-16#AC00,\n"
                 "    T = 28,\n"
                 "    N = 588,\n"
                 "    Lead = 16#1100 + Syll div N,\n"
                 "    Vowel = 16#1161 + (Syll rem N) div T,\n"
                 "    case Syll rem T of\n"
                 "        0 -> [{0,Lead},{0,Vowel}];\n"
                 "        Trail -> [{0,Lead}, {0,Vowel}, {0,Trail+16#11A7}]\n"
                 "    end;\n"
                 "decompose_compat_1(CP) when is_integer(CP) ->\n"
                 "    case unicode_table(CP) of\n"
                 "        {CCC, [], []} -> [{CCC,CP}];\n"
                 "        {_, _, {_,CPs}} -> CPs;\n"
                 "        {_, CPs, _} -> CPs\n"
                 "    end;\n"
                 "decompose_compat_1([CP|CPs]) ->\n"
                 "    decompose_compat_1(CP) ++ decompose_compat_1(CPs);\n"
                 "decompose_compat_1([]) -> [].\n"),


    io:put_chars(Fd,
                 "compose(CP) when is_integer(CP) -> CP;\n"
                 "compose([Lead,Vowel|Trail]) %% Hangul\n"
                 "  when 16#1100 =< Lead, Lead =< 16#1112 ->\n"
                 "    if 16#1161 =< Vowel, Vowel =< 16#1175 ->\n"
                 "            CP = 16#AC00 + ((Lead - 16#1100) * 588) + ((Vowel - 16#1161) * 28),\n"
                 "            case Trail of\n"
                 "                [T|Acc] when 16#11A7 =< T, T =< 16#11C2 -> nolist(CP+T-16#11A7,Acc);\n"
                 "                Acc -> nolist(CP,Acc)\n"
                 "            end;\n"
                 "       true ->\n"
                 "            case compose([Vowel|Trail]) of\n"
                 "                [_|_] = CPs -> [Lead|CPs];\n"
                 "                CP -> [Lead,CP]\n"
                 "            end\n"
                 "    end;\n"
                 "compose([Base,Accent]=GC0) ->\n"
                 "    case compose_pair(Base,Accent) of\n"
                 "        false -> GC0;\n"
                 "        GC -> GC\n"
                 "    end;\n"
                 "compose([CP|Many]) ->\n"
                 "    compose_many(Many, CP, [], class(CP)).\n"
                 "\n"
                 "compose_many([CP|Rest], Base, Accents, Prev) ->\n"
                 "    Class = class(CP),\n"
                 "    case (Prev =:= 0 orelse Prev < Class) andalso compose_pair(Base, CP) of\n"
                 "        false -> compose_many(Rest, Base, [CP|Accents], Class);\n"
                 "        Combined -> compose_many(Rest, Combined, Accents, Prev)\n"
                 "    end;\n"
                 "compose_many([], Base, [], Prev) ->\n"
                 "    Base;\n"
                 "compose_many([], Base, Accents, Prev) ->\n"
                 "    [Base|lists:reverse(Accents)].\n"
                 "\n\n"),
    io:put_chars(Fd,
                 "compose_compat_0(CP) when is_integer(CP) ->\n"
                 "    CP;\n"
                 "compose_compat_0(L) ->\n"
                 "    case gc(L) of\n"
                 "        [First|Rest] ->\n"
                 "            case compose_compat(First) of\n"
                 "                [_|_] = GC -> GC ++ compose_compat_0(Rest);\n"
                 "                CP -> [CP|compose_compat_0(Rest)]\n"
                 "            end;\n"
                 "        [] -> []\n"
                 "    end.\n\n"
                 "compose_compat(CP) when is_integer(CP) -> CP;\n"
                 "compose_compat([Lead,Vowel|Trail]) %% Hangul\n"
                 "  when 16#1100 =< Lead, Lead =< 16#1112 ->\n"
                 "    if 16#1161 =< Vowel, Vowel =< 16#1175 ->\n"
                 "            CP = 16#AC00 + ((Lead - 16#1100) * 588) + ((Vowel - 16#1161) * 28),\n"
                 "            case Trail of\n"
                 "                [T|Acc] when 16#11A7 =< T, T =< 16#11C2 -> nolist(CP+T-16#11A7,Acc);\n"
                 "                Acc -> nolist(CP,Acc)\n"
                 "            end;\n"
                 "       true ->\n"
                 "            case compose_compat([Vowel|Trail]) of\n"
                 "                [_|_] = CPs -> [Lead|CPs];\n"
                 "                CP -> [Lead,CP]\n"
                 "            end\n"
                 "    end;\n"
                 "compose_compat([Base,Accent]=GC0) ->\n"
                 "    case compose_pair(Base,Accent) of\n"
                 "        false -> GC0;\n"
                 "        GC -> GC\n"
                 "    end;\n"
                 "compose_compat([CP|Many]) ->\n"
                 "    compose_compat_many(Many, CP, [], class(CP)).\n"
                 "\n"
                 "compose_compat_many([CP|Rest], Base, Accents, Prev) ->\n"
                 "    Class = class(CP),\n"
                 "    case (Prev =:= 0 orelse Prev < Class) andalso compose_pair(Base, CP) of\n"
                 "        false -> compose_compat_many(Rest, Base, [CP|Accents], Class);\n"
                 "        Combined -> compose_compat_many(Rest, Combined, Accents, Prev)\n"
                 "    end;\n"
                 "compose_compat_many([], Base, [], Prev) ->\n"
                 "    Base;\n"
                 "compose_compat_many([], Base, Accents, Prev) ->\n"
                 "    [Base|lists:reverse(Accents)].\n"
                 "\n\n"),

    ok.

gen_ws(Fd, Props) ->
    WS0 = maps:get(pattern_white_space, Props),
    WS = merge_ranges(WS0, split),
    io:put_chars(Fd, "%% Useful non-breakable whitespace chars\n"
                 "%% defined as Pattern White Space in Unicode Standard Annex #31\n"),
    io:put_chars(Fd, "-spec whitespace() -> [gc()].\n"),
    WsChars = [CP || {CP, undefined} <- WS],
    io:format(Fd, "whitespace() -> ~w.\n\n", [[[$\r,$\n]|WsChars]]),

    io:put_chars(Fd, "-spec is_whitespace(gc()) -> boolean().\n"),
    IsWS = fun(Range) -> io:format(Fd, "is_whitespace~s true;\n", [gen_single_clause(Range)]) end,
    io:format(Fd, "is_whitespace([13,10]) -> true;\n", []),
    [IsWS(CP) || CP <- WS],
    io:put_chars(Fd, "is_whitespace(_) -> false.\n\n"),
    ok.

gen_cp(Fd) ->
    io:put_chars(Fd, "-spec cp(String::unicode:chardata()) ->"
                 " maybe_improper_list().\n"),
    io:put_chars(Fd, "cp([C|_]=L) when is_integer(C) -> L;\n"),
    io:put_chars(Fd, "cp([List]) -> cp(List);\n"),
    io:put_chars(Fd, "cp([List|R]) ->\n"),
    io:put_chars(Fd, "    case cp(List) of\n"),
    io:put_chars(Fd, "        [] -> cp(R);\n"),
    io:put_chars(Fd, "        [CP] -> [CP|R];\n"),
    io:put_chars(Fd, "        [C|R0] -> [C|[R0|R]]\n"),
    io:put_chars(Fd, "    end;\n"),
    io:put_chars(Fd, "cp([]) -> [];\n"),
    io:put_chars(Fd, "cp(<<C/utf8, R/binary>>) -> [C|R];\n"),
    io:put_chars(Fd, "cp(<<>>) -> [].\n\n"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_gc(Fd, GBP) ->
    %% see http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
    io:put_chars(Fd,
                 "-spec gc(String::unicode:chardata()) ->"
                 " maybe_improper_list().\n"),
    io:put_chars(Fd,
                 "gc(Str) ->\n"
                 "    gc_1(cp(Str)).\n\n"
                 "gc_1([$\\r|R0] = R) ->\n"
                 "    case cp(R0) of % Don't break CRLF\n"
                 "        [$\\n|R1] -> [[$\\r,$\\n]|R1];\n"
                 "        _ -> R\n"
                 "    end;\n"),

    io:put_chars(Fd, "%% Handle control\n"),
    GenControl = fun(Range) -> io:format(Fd, "gc_1~s R0;\n", [gen_clause(Range)]) end,
    CRs0 = merge_ranges(maps:get(cr, GBP) ++ maps:get(lf, GBP) ++ maps:get(control, GBP), false),
    [R1,R2,R3|Crs] = CRs0,
    [GenControl(CP) || CP <- merge_ranges([R1,R2,R3], split), CP =/= {$\r, undefined}],
    %%GenControl(R1),GenControl(R2),GenControl(R3),
    io:format(Fd, "gc_1([CP|R]) when CP < 255 -> gc_extend(R,CP);\n", []),
    [GenControl(CP) || CP <- Crs],
    %% One clause per CP
    %% CRs0 = merge_ranges(maps:get(cr, GBP) ++ maps:get(lf, GBP) ++ maps:get(control, GBP)),
    %% [GenControl(CP) || CP <- CRs0, CP =/= {$\r, undefined}],

    io:put_chars(Fd, "%% Handle ZWJ\n"),
    GenZWJ = fun(Range) -> io:format(Fd, "gc_1~s gc_zwj(R1, [CP]);\n", [gen_clause(Range)]) end,
    [GenZWJ(CP) || CP <- merge_ranges(maps:get(zwj,GBP))],

    io:put_chars(Fd, "%% Handle prepend\n"),
    GenPrepend = fun(Range) -> io:format(Fd, "gc_1~s gc_prepend(R1, CP);\n", [gen_clause(Range)]) end,
    [GenPrepend(CP) || CP <- merge_ranges(maps:get(prepend,GBP))],

    io:put_chars(Fd, "%% Handle Hangul L\n"),
    GenHangulL = fun(Range) -> io:format(Fd, "gc_1~s gc_h_L(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenHangulL(CP) || CP <- merge_ranges(maps:get(l,GBP))],
    io:put_chars(Fd, "%% Handle Hangul V\n"),
    GenHangulV = fun(Range) -> io:format(Fd, "gc_1~s gc_h_V(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenHangulV(CP) || CP <- merge_ranges(maps:get(v,GBP))],
    io:put_chars(Fd, "%% Handle Hangul T\n"),
    GenHangulT = fun(Range) -> io:format(Fd, "gc_1~s gc_h_T(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenHangulT(CP) || CP <- merge_ranges(maps:get(t,GBP))],
    io:put_chars(Fd, "%% Handle Hangul LV and LVT special, since they are large\n"),
    io:put_chars(Fd, "gc_1([CP|_]=R0) when 44000 < CP, CP < 56000 -> gc_h_lv_lvt(R0, []);\n"),

    io:put_chars(Fd, "%% Handle Regional\n"),
    GenRegional = fun(Range) -> io:format(Fd, "gc_1~s gc_regional(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenRegional(CP) || CP <- merge_ranges(maps:get(regional_indicator,GBP))],
    io:put_chars(Fd, "%% Handle E_Base\n"),
    GenEBase = fun(Range) -> io:format(Fd, "gc_1~s gc_e_cont(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenEBase(CP) || CP <- merge_ranges(maps:get(e_base,GBP))],
    io:put_chars(Fd, "%% Handle EBG\n"),
    GenEBG = fun(Range) -> io:format(Fd, "gc_1~s gc_e_cont(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenEBG(CP) || CP <- merge_ranges(maps:get(e_base_gaz,GBP))],

    io:put_chars(Fd, "gc_1([CP|R]) -> gc_extend(R, CP);\n"),
    io:put_chars(Fd, "gc_1([]) -> [].\n\n"),

    io:put_chars(Fd, "%% Handle Prepend\n"),
    io:put_chars(Fd,
                 "gc_prepend(R00, CP0) ->\n"
                 "    case cp(R00) of\n"
                 "      [CP1|_] = R0 ->\n"
                 "          case is_control(CP1) of\n"
                 "            true -> [CP0|R00];\n"
                 "            false ->\n"
                 "                case gc_1(R0) of\n"
                 "                    [GC|R1] when is_integer(GC) -> [[CP0,GC]|R1];\n"
                 "                    [GC|R1] -> [[CP0|GC]|R1]\n"
                 "                end\n"
                 "           end;\n"
                 "      [] -> [CP0]\n"
                 "    end.\n\n"),

    IsCtrl = fun(Range) -> io:format(Fd, "is_control~s true;\n", [gen_single_clause(Range)]) end,
    [IsCtrl(CP) || CP <- merge_ranges(maps:get(cr, GBP) ++ maps:get(lf, GBP) ++ maps:get(control, GBP))],
    io:put_chars(Fd, "is_control(_) -> false.\n\n"),

    io:put_chars(Fd, "%% Handle Extend\n"
                 "%% To simplify binary handling in libraries the tail should be kept binary\n"
                 "%% and not a lookahead CP\n"
                ),
    io:put_chars(Fd, "gc_extend(T, Acc) ->\n"
                 "    gc_extend(cp(T), T, Acc).\n\n"),
    io:put_chars(Fd,
                 "gc_extend([CP|T], T0, Acc0) ->\n"
                 "    case is_extend(CP) of\n"
                 "        zwj ->\n"
                 "            case Acc0 of\n"
                 "                [_|_] -> gc_zwj(T, [CP|Acc0]);\n"
                 "                Acc -> gc_zwj(T, [CP,Acc])\n"
                 "            end;\n"
                 "        true ->\n"
                 "            case Acc0 of\n"
                 "                [_|_] -> gc_extend(T, [CP|Acc0]);\n"
                 "                Acc -> gc_extend(T, [CP,Acc])\n"
                 "            end;\n"
                 "        false ->\n"
                 "            case Acc0 of\n"
                 "                [Acc] -> [Acc|T0];\n"
                 "                [_|_]=Acc -> [lists:reverse(Acc)|T0];\n"
                 "                Acc -> [Acc|T0]\n"
                 "            end\n"
                 "    end;\n"
                 "gc_extend([], _, Acc0) ->\n"
                 "    case Acc0 of\n"
                 "        [_]=Acc -> Acc;\n"
                 "        [_|_]=Acc -> [lists:reverse(Acc)];\n"
                 "        Acc -> [Acc]\n"
                 "    end.\n\n"),
    [ZWJ] = maps:get(zwj, GBP),
    GenExtend = fun(R) when R =:= ZWJ -> io:format(Fd, "is_extend~s zwj;\n", [gen_single_clause(ZWJ)]);
                   (Range) -> io:format(Fd, "is_extend~s true;\n", [gen_single_clause(Range)])
                end,
    Extends = merge_ranges(maps:get(extend,GBP)++maps:get(spacingmark, GBP) ++ maps:get(zwj, GBP), split),
    [GenExtend(CP) || CP <- Extends],
    io:put_chars(Fd, "is_extend(_) -> false.\n\n"),

    io:put_chars(Fd,
                 "gc_e_cont(R0, Acc) ->\n"
                 "    case cp(R0) of\n"
                 "        [CP|R1] ->\n"
                 "            case is_extend(CP) of\n"
                 "                zwj -> gc_zwj(R1, [CP|Acc]);\n"
                 "                true -> gc_e_cont(R1, [CP|Acc]);\n"
                 "                false ->\n"
                 "                    case is_emodifier(CP) of\n"
                 "                        true -> [lists:reverse([CP|Acc])|R1];\n"
                 "                        false ->\n"
                 "                            case Acc of\n"
                 "                                [A] -> [A|R0];\n"
                 "                                _ -> [lists:reverse(Acc)|R0]\n"
                 "                            end\n"
                 "                    end\n"
                 "              end;\n"
                 "        [] ->\n"
                 "            case Acc of\n"
                 "                [A] -> [A];\n"
                 "                _ -> [lists:reverse(Acc)]\n"
                 "            end\n"
                 "    end.\n\n"),

    GenEMod = fun(Range) -> io:format(Fd, "is_emodifier~s true;\n", [gen_single_clause(Range)]) end,
    EMods = merge_ranges(maps:get(e_modifier, GBP), split),
    [GenEMod(CP) || CP <- EMods],
    io:put_chars(Fd, "is_emodifier(_) -> false.\n\n"),

    io:put_chars(Fd, "gc_zwj(R0, Acc) ->\n    case cp(R0) of\n"),
    GenZWJGlue = fun(Range) -> io:format(Fd, "~8c~s gc_extend(R1, R0, [CP|Acc]);\n",
                                         [$\s,gen_case_clause(Range)]) end,
    [GenZWJGlue(CP) || CP <- merge_ranges(maps:get(glue_after_zwj,GBP))],
    GenZWJEBG = fun(Range) -> io:format(Fd, "~8c~s gc_e_cont(R1, [CP|Acc]);\n",
                                        [$\s,gen_case_clause(Range)]) end,
    [GenZWJEBG(CP) || CP <- merge_ranges(maps:get(e_base_gaz,GBP))],
    io:put_chars(Fd,"        R1 -> gc_extend(R1, R0, Acc)\n"
                 "    end.\n\n"),


    %% --------------------
    io:put_chars(Fd, "%% Handle Regional\n"),
    [{RLess,RLarge}] = merge_ranges(maps:get(regional_indicator,GBP)),
    io:put_chars(Fd,"gc_regional(R0, Acc) ->\n"
                 "    case cp(R0) of\n"),
    io:format(Fd,   "        [CP|R1] when ~w =< CP,CP =< ~w-> gc_extend(R1,[CP|Acc]);~n",[RLess, RLarge]),
    io:put_chars(Fd,"        R1 -> gc_extend(R1, R0, Acc)\n"
                 "    end.\n\n"),

    %% Special hangul
    io:put_chars(Fd, "%% Handle Hangul L\n"),
    io:put_chars(Fd, "gc_h_L(R0, Acc) ->\n    case cp(R0) of\n"),
    GenHangulL_1 = fun(Range) -> io:format(Fd, "~8c~s gc_h_L(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulL_1(CP) || CP <- merge_ranges(maps:get(l,GBP))],
    GenHangulL_2 = fun(Range) -> io:format(Fd, "~8c~s gc_h_V(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulL_2(CP) || CP <- merge_ranges(maps:get(v,GBP))],
    io:put_chars(Fd, "        R1 -> gc_h_lv_lvt(R1, Acc)\n    end.\n\n"),

    io:put_chars(Fd, "%% Handle Hangul V\n"),
    io:put_chars(Fd, "gc_h_V(R0, Acc) ->\n    case cp(R0) of\n"),
    GenHangulV_1 = fun(Range) -> io:format(Fd, "~8c~s gc_h_V(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulV_1(CP) || CP <- merge_ranges(maps:get(v,GBP))],
    GenHangulV_2 = fun(Range) -> io:format(Fd, "~8c~s gc_h_T(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulV_2(CP) || CP <- merge_ranges(maps:get(t,GBP))],
    io:put_chars(Fd, "        R1 -> gc_extend(R1, R0, Acc)\n    end.\n\n"),

    io:put_chars(Fd, "%% Handle Hangul T\n"),
    io:put_chars(Fd, "gc_h_T(R0, Acc) ->\n    case cp(R0) of\n"),
    GenHangulT_1 = fun(Range) -> io:format(Fd, "~8c~s gc_h_T(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulT_1(CP) || CP <- merge_ranges(maps:get(t,GBP))],
    io:put_chars(Fd, "        R1 -> gc_extend(R1, R0, Acc)\n    end.\n\n"),

    io:put_chars(Fd, "%% Handle Hangul LV\n"),
    GenHangulLV = fun(Range) -> io:format(Fd, "gc_h_lv_lvt~s gc_h_V(R1,[CP|Acc]);\n",
                                          [gen_clause2(Range)]) end,
    [GenHangulLV(CP) || CP <- merge_ranges(maps:get(lv,GBP))],
    io:put_chars(Fd, "%% Handle Hangul LVT\n"),
    GenHangulLVT = fun(Range) -> io:format(Fd, "gc_h_lv_lvt~s gc_h_T(R1,[CP|Acc]);\n",
                                           [gen_clause2(Range)]) end,
    [GenHangulLVT(CP) || CP <- merge_ranges(maps:get(lvt,GBP))],
    io:put_chars(Fd, "gc_h_lv_lvt([CP|R], []) -> gc_extend(R, CP);\n"), %% From gc_1/1
    io:put_chars(Fd, "gc_h_lv_lvt(R, Acc) -> gc_extend(R, Acc).\n\n"),
    ok.

gen_compose_pairs(Fd, ExclData, Data) ->
    GetPairs =
        fun(CP, #cp{dec=[DCP,_]=Pair}, Acc) ->
                case array:get(DCP,Data) of
                    #cp{class=0} -> [{Pair, CP}|Acc];
                    #cp{} -> Acc %% Ignore Non-Starter Decompositions
                end;
           (_, _, Acc) -> Acc
        end,
    DeCmp2 = array:sparse_foldl(GetPairs, [], ExclData),
    [io:format(Fd, "compose_pair(~w,~w) -> ~w;~n", [A,B,CP]) || {[A,B],CP} <- lists:sort(DeCmp2)],
    io:put_chars(Fd, "compose_pair(_,_) -> false.\n\n"),

    io:put_chars(Fd, "nolist(CP, []) -> CP;\nnolist(CP,L) -> [CP|L].\n\n"),
    ok.

gen_case_table(Fd, Data) ->
    Case = array:foldr(fun(CP, #cp{cs={U0,L0,T0,F0}}, Acc) ->
                               U = def_cp(U0,CP),
                               L = def_cp(L0,CP),
                               T = def_cp(T0,CP),
                               F = def_cp(F0,CP),
                               case T =:= U andalso F =:= L of
                                   true ->
                                       [{CP,{U,L}}|Acc];
                                   false ->
                                       [{CP,{U,L,T,F}}|Acc]
                               end;
                          (_CP, _, Acc) -> Acc
                       end, [], Data),
    [io:format(Fd, "case_table(~w) -> ~w;\n", [CP, Map])|| {CP,Map} <- Case],
    io:format(Fd, "case_table(CP) -> {CP, CP}.\n\n",[]),
    ok.

def_cp([], CP) -> CP;
def_cp(CP, _) -> CP.

gen_unicode_table(Fd, Data) ->
    FixCanon = fun(_, #cp{class=CCC, dec=Dec, comp=Comp}) ->
                       Canon  = decompose(Dec,Data),
                       #{ccc=>CCC, canonical=>Canon, compat=>Comp}
               end,
    AofMaps0 = array:sparse_map(FixCanon, Data),
    FixCompat = fun(_, #{ccc:=CCC, canonical:=Canon, compat:=Comp}) ->
                        Compat = decompose_compat(Canon, Comp, AofMaps0),
                        {CCC, Canon, Compat}
                end,
    AofMaps1 = array:sparse_map(FixCompat, AofMaps0),

    Dict0 = array:sparse_to_orddict(AofMaps1),
    Def = {0, [], []},
    Dict = lists:filter(fun({_, Map}) -> Map =/= Def end, Dict0),

    [io:format(Fd, "unicode_table(~w) -> ~w;~n", [CP, Map]) || {CP,Map} <- Dict],
    io:format(Fd, "unicode_table(_) -> ~w.~n~n",[Def]),
    ok.

decompose([], _Data) -> [];
decompose([CP|CPs], Data) when is_integer(CP) ->
    case array:get(CP,Data) of
        undefined -> [{0,CP}|decompose(CPs,Data)];
        #cp{class=CCC, dec=[]} -> [{CCC,CP}|decompose(CPs,Data)];
        #cp{dec=Dec} -> decompose(Dec, Data) ++ decompose(CPs,Data)
    end.


decompose_compat(Canon, [], Data) ->
    case decompose_compat(Canon, Data) of
        Canon -> [];
        Other -> {compat, Other}
    end;
decompose_compat([], {CompatMode, CPs}, Data) ->
    {CompatMode, decompose_compat(CPs, Data)}.

decompose_compat([], _Data) -> [];
decompose_compat([CP|CPs], Data) when is_integer(CP) ->
    case array:get(CP,Data) of
        undefined ->
            [{0,CP}|decompose_compat(CPs,Data)];
        #{ccc:=CCC, canonical:=[], compat:=[]} ->
            [{CCC,CP}|decompose_compat(CPs,Data)];
        #{canonical:=[], compat:={_,Dec}} ->
            decompose_compat(Dec, Data) ++ decompose_compat(CPs,Data);
        #{canonical:=Dec} ->
            decompose_compat(Dec, Data) ++ decompose_compat(CPs,Data)
    end;
decompose_compat([{_,CP}|CPs], Data) ->
    decompose_compat([CP|CPs], Data).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_clause({R0, undefined}) ->
    io_lib:format("([~w=CP|R1]=R0) ->", [R0]);
gen_clause({R0, R1}) ->
    io_lib:format("([CP|R1]=R0) when ~w =< CP, CP =< ~w ->", [R0,R1]).

gen_clause2({R0, undefined}) ->
    io_lib:format("([~w=CP|R1], Acc) ->", [R0]);
gen_clause2({R0, R1}) ->
    io_lib:format("([CP|R1], Acc) when ~w =< CP, CP =< ~w ->", [R0,R1]).

gen_case_clause({R0, undefined}) ->
    io_lib:format("[~w=CP|R1] ->", [R0]);
gen_case_clause({R0, R1}) ->
    io_lib:format("[CP|R1] when ~w =< CP, CP =< ~w ->", [R0,R1]).


gen_single_clause({R0, undefined}) ->
    io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
    io_lib:format("(CP) when ~w =< CP, CP =< ~w ->", [R0,R1]).


merge_ranges(List) ->
    merge_ranges(List, true).

merge_ranges(List, Opt) ->
    Res0 = merge_ranges_1(lists:sort(List), []),
    case Opt of
        split ->
            split_ranges(Res0,[]);     % One clause per CP
        true ->
            Res = Res0,
            OptRes = optimize_ranges(Res),
            true = lists:sort(Res) =:= lists:sort(OptRes), %Assertion.
            OptRes;
        false ->
            Res0
    end.

merge_ranges_1([{Next, Stop}|R], [{Start,undefined}|Acc]) when Start+1 =:= Next ->
    case Stop of
        undefined -> merge_ranges_1(R, [{Start, Next}|Acc]);
        _ -> merge_ranges_1(R, [{Start,Stop}|Acc])
    end;
merge_ranges_1([{Next, Stop}|R], [{Start,Prev}|Acc]) when Prev+1 =:= Next ->
    case Stop of
        undefined -> merge_ranges_1(R, [{Start, Next}|Acc]);
        _ -> merge_ranges_1(R, [{Start,Stop}|Acc])
    end;
merge_ranges_1([{Next, Stop}|R], [{Start,undefined}|Acc]) when Start+1 =:= Next ->
    case Stop of
        undefined -> merge_ranges_1(R, [{Start, Next}|Acc]);
        _ -> merge_ranges_1(R, [{Start,Stop}|Acc])
    end;
merge_ranges_1([Next|R], Acc) ->
    merge_ranges_1(R, [Next|Acc]);
merge_ranges_1([], Acc) ->
    lists:reverse(Acc).

split_ranges([{_,undefined}=CP|Rs], Acc) ->
    split_ranges(Rs,[CP|Acc]);
split_ranges([{F,L}|Rs], Acc) when F < L ->
    split_ranges([{F+1,L}|Rs],[{F,undefined}|Acc]);
split_ranges([{L,L}|Rs], Acc) ->
    split_ranges(Rs,[{L, undefined}|Acc]);
split_ranges([], Acc) ->
    lists:reverse(Acc).

optimize_ranges(Rs0) ->
    PF = fun({N,undefined}) when is_integer(N) -> true;
            (_) -> false
         end,
    {Singles,Rs} = lists:partition(PF, Rs0),
    Singles ++ optimize_ranges_1(Rs).

optimize_ranges_1(Rs) ->
    case length(Rs) of
        N when N >= 4 ->
            {R0,[Mid|R1]} = lists:split(N div 2, Rs),
            [Mid|optimize_ranges_1(R0)++optimize_ranges_1(R1)];
        _ ->
            Rs
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hex_to_int([]) -> [];
hex_to_int(HexStr) ->
    list_to_integer(string:strip(HexStr, both), 16).

to_atom(Str) ->
    list_to_atom(string:to_lower(string:strip(Str, both))).

foldl(Fun, Acc, Fd) ->
    Get = fun() -> file:read_line(Fd) end,
    foldl_1(Fun, Acc, Get).

foldl_1(_Fun, {done, Acc}, _Get) -> Acc;
foldl_1(Fun, Acc, Get) ->
    case Get() of
        eof -> Acc;
        {ok, "#" ++ _} -> %% Ignore comments
            foldl_1(Fun, Acc, Get);
        {ok, "\n"} -> %% Ignore empty lines
            foldl_1(Fun, Acc, Get);
        {ok, Line} ->
            foldl_1(Fun, Fun(Line, Acc), Get)
    end.



%% Differs from string:tokens, it returns empty string as token between two delimiters
tokens(S, [C]) ->
    tokens(lists:reverse(S), C, []).

tokens([Sep|S], Sep, Toks) ->
    tokens(S, Sep, [[]|Toks]);
tokens([C|S], Sep, Toks) ->
    tokens_2(S, Sep, Toks, [C]);
tokens([], _, Toks) ->
    Toks.

tokens_2([Sep|S], Sep, Toks, Tok) ->
    tokens(S, Sep, [Tok|Toks]);
tokens_2([C|S], Sep, Toks, Tok) ->
    tokens_2(S, Sep, Toks, [C|Tok]);
tokens_2([], _Sep, Toks, Tok) ->
    [Tok|Toks].
