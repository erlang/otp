#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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

-record(cp, {name, class, dec, comp, cs, cat}).
-define(MOD, "unicode_util").

main(Args) ->
    %%  Parse main table
    UD = file_open("../uc_spec/UnicodeData.txt"),
    Data0 = foldl(fun parse_unicode_data/2, [], UD),
    Data1  = array:from_orddict(lists:reverse(Data0)),
    ok = file:close(UD),

    %%  Special Casing table
    SC = file_open("../uc_spec/SpecialCasing.txt"),
    Data2 = foldl(fun parse_special_casing/2, Data1, SC),
    ok = file:close(SC),
    %%  Casing Folding table
    CF = file_open("../uc_spec/CaseFolding.txt"),
    Data = foldl(fun parse_case_folding/2, Data2, CF),
    ok = file:close(CF),

    %% Normalization
    ExclF = file_open("../uc_spec/CompositionExclusions.txt"),
    ExclData = foldl(fun parse_comp_excl/2, Data, ExclF),
    ok = file:close(ExclF),

    %%  GraphemeBreakProperty table
    Emoji = file_open("../uc_spec/emoji-data.txt"),
    Props00 = foldl(fun parse_properties/2, [], Emoji),
    %% Filter Extended_Pictographic class which we are interested in.
    Props0 = [EP || {extended_pictographic, _} = EP <- Props00],
    ok = file:close(Emoji),
    GBPF = file_open("../uc_spec/GraphemeBreakProperty.txt"),
    Props1 = foldl(fun parse_properties/2, Props0, GBPF),
    ok = file:close(GBPF),
    PropF = file_open("../uc_spec/PropList.txt"),
    Props2 = foldl(fun parse_properties/2, Props1, PropF),
    ok = file:close(PropF),
    Indic = file_open("../uc_spec/IndicSyllabicCategory.txt"),
    Props3 = foldl(fun parse_properties/2, Props2, Indic),
    ok = file:close(Indic),
    Props = sofs:to_external(sofs:relation_to_family(sofs:relation(Props3))),

    WidthF = file_open("../uc_spec/EastAsianWidth.txt"),
    WideCs = foldl(fun parse_widths/2, [], WidthF),
    ok = file:close(WidthF),

    %% Make module
    UpdateTests = case Args of
                      ["update_tests"] -> true;
                      _ -> false
                  end,

    {ok, Out} = file:open(?MOD++".erl", [write]),
    gen_file(Out, Data, ExclData, maps:from_list(Props), WideCs, UpdateTests),
    ok = file:close(Out),
    ok.

file_open(File) ->
    {ok, Fd} = file:open(File, [read, raw, {read_ahead, 1000000}]),
    Fd.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_unicode_data(Line0, Acc) ->
    Line = string:chomp(Line0),
    [CodePoint,Name0,Cat,Class,_BiDi,Decomp,
     _N1,_N2,_N3,_BDMirror,_Uni1,_Iso|Case] = tokens(Line, ";"),
    {Dec,Comp} = case to_decomp(Decomp) of
                     {_, _} = Compabil -> {[], Compabil};
                     Canon -> {Canon, []}
                 end,
    {Range, Name} = pick_range(Name0),
    case Range of
        last ->
            CP = #cp{name=list_to_binary(Name),class=to_class(Class),
                     dec=Dec, comp=Comp, cs=to_case(Case), cat=Cat},
            fill_range(Acc, CP, hex_to_int(CodePoint));
        _ ->
            [{hex_to_int(CodePoint),
              #cp{name=list_to_binary(Name),class=to_class(Class),
                  dec=Dec, comp=Comp, cs=to_case(Case), cat=Cat}}
            |Acc]
    end.


to_class(String) ->
    list_to_integer(string:trim(String, both)).

to_decomp("") -> [];
to_decomp("<" ++ Str) ->
    [Tag,Rest]  = string:lexemes(Str, ">"),
    {list_to_atom(Tag), to_decomp(Rest)};
to_decomp(CodePoints) ->
    CPL = string:lexemes(CodePoints, " "),
    [hex_to_int(CP) || CP <- CPL].

to_case(["","",""]) -> [];
to_case([Upper,Lower,Title]) ->
    {hex_to_int(Upper),hex_to_int(Lower),hex_to_int(Title),[]}.

pick_range([$<|Rest]) ->
    range_1(tokens(Rest, ","));
pick_range(Name) ->
    {false, Name}.

range_1([Name, " First>"]) ->
    {first, Name};
range_1([Name, " Last>"]) ->
    {last, Name};
range_1(Name) ->
    {false, lists:droplast(Name)}.

fill_range([{Start, CP}|_]=Acc, CP, Last) ->
    fill_range_1(Start+1, Last, CP, Acc).

fill_range_1(Start, Last, CP, Acc) when Start =< Last ->
    fill_range_1(Start+1, Last, CP, [{Start,CP}|Acc]);
fill_range_1(_Start, _Last, _CP, Acc) ->
    Acc.

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
    {unlist([hex_to_int(CP) || CP <- string:lexemes(Upper, " ")]),
     unlist([hex_to_int(CP) || CP <- string:lexemes(Lower, " ")]),
     unlist([hex_to_int(CP) || CP <- string:lexemes(Title, " ")]),
     []}.

parse_case_folding(Line, Table) ->
    [CodePoint, Class0, CaseStr |_Comments] = tokens(Line, ";"),
    Class = string:trim(Class0, both),
    if Class =:= "T" -> Table; %% Do not support localization yet
       Class =:= "S" -> Table; %% Ignore simple
       true ->
            CP = hex_to_int(CodePoint),
            Case = unlist([hex_to_int(CPC) ||
                              CPC <- string:lexemes(CaseStr, " ")]),
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

%% Pick ranges that are wide when seen from a non East Asian context,
%% That way we can minimize the data, every other code point is considered narrow.
%% We loose information but hopefully keep the important width for a standard
%% terminal.
parse_widths(Line0, Acc) ->
    [{WidthClass, {From, _To}=Range}] = parse_properties(Line0, []),
    case is_default_width(From, WidthClass) of
        {true, narrow} ->
            Acc;
        {false, narrow} ->
            [Range|Acc];
        {true, RuleRange} ->
            [RuleRange|Acc]
%%%     {false, rule_execption} ->  i.e. narrow codepoint in wide range
%%%        Should not happen in current specs
    end.

is_default_width(Index, WD) ->
    if
        16#3400 =< Index, Index =< 16#4DBF ->
            if WD =:= w orelse WD =:= f ->
                    {true, {16#3400, 16#4DBF}};
               true ->
                    {false, rule_execption}
            end;
        16#4E00 =< Index, Index =< 16#9FFF ->
            if WD =:= w orelse WD =:= f ->
                    {true, {16#4E00, 16#9FFF}};
               true ->
                    {false, rule_execption}
            end;
        16#F900 =< Index, Index =< 16#FAFF ->
            if WD =:= w orelse WD =:= f ->
                    {true, {16#F900, 16#FAFF}};
               true ->
                    {false, rule_execption}
            end;
        16#20000 =< Index, Index =< 16#2FFFD ->
            if WD =:= w orelse WD =:= f ->
                    {true, {16#20000, 16#2FFFD}};
               true ->
                    {false, rule_execption}
            end;
        16#30000 =< Index, Index =< 16#3FFFD ->
            if WD =:= w orelse WD =:= f ->
                    {true, {16#30000, 16#3FFFD}};
               true ->
                    {false, rule_execption}
            end;
        true ->
            {WD =:= n orelse WD =:= na orelse WD == h orelse WD =:= a, narrow}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_file(Fd, Data, ExclData, Props, WideCs, UpdateTests) ->
    gen_header(Fd),
    gen_static(Fd),
    gen_norm(Fd),
    gen_ws(Fd, Props),
    gen_cp(Fd),
    gen_gc(Fd, Props),
    gen_compose_pairs(Fd, ExclData, Data),
    gen_case_table(Fd, Data),
    gen_unicode_table(Fd, Data, UpdateTests),
    gen_width_table(Fd, WideCs),
    ok.

gen_header(Fd) ->
    io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
    io:put_chars(Fd, "%% see ../uc_spec/gen_unicode_mod.escript\n\n"),
    io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
    io:put_chars(Fd, "-moduledoc false.\n"),
    io:put_chars(Fd, "-export([cp/1, gc/1]).\n"),
    io:put_chars(Fd, "-export([nfd/1, nfc/1, nfkd/1, nfkc/1]).\n"),
    io:put_chars(Fd, "-export([whitespace/0, is_whitespace/1]).\n"),
    io:put_chars(Fd, "-export([uppercase/1, lowercase/1, titlecase/1, casefold/1]).\n\n"),
    io:put_chars(Fd, "-export([spec_version/0, lookup/1, get_case/1]).\n"),
    io:put_chars(Fd, "-export([is_wide/1]).\n"),
    io:put_chars(Fd, "-compile({inline, [class/1]}).\n"),
    io:put_chars(Fd, "-compile(nowarn_unused_vars).\n"),
    io:put_chars(Fd, "-dialyzer({no_improper_lists, [cp/1, gc/1, gc_prepend/2]}).\n"),
    io:put_chars(Fd, "-type gc() :: char()|[char()].\n\n"),
    io:put_chars(Fd, "-define(IS_CP(CP), (is_integer(CP) andalso 0 =< CP andalso CP < 16#110000)).\n\n\n"),
    ok.

gen_static(Fd) ->
    io:put_chars(Fd, "-spec lookup(char()) -> #{'canon':=[{byte(),char()}], 'ccc':=byte(), "
                 "'compat':=[] | {atom(),[{byte(),char()}]}, 'category':={atom(),atom()}}.\n"),
    io:put_chars(Fd, "lookup(Codepoint) when ?IS_CP(Codepoint) ->\n"
                 "    {CCC,Can,Comp,Cat} = unicode_table(Codepoint),\n"
                 "    #{ccc=>CCC, canon=>Can, compat=>Comp, category=>category(Codepoint,Cat)}.\n\n"),

    io:put_chars(Fd, "-spec get_case(char()) -> #{'fold':=gc(), 'lower':=gc(), 'title':=gc(), 'upper':=gc()}.\n"),
    io:put_chars(Fd, "get_case(Codepoint) when ?IS_CP(Codepoint) ->\n"
                 "    case case_table(Codepoint) of\n"
                 "        {U,L} -> #{upper=>U,lower=>L,title=>U,fold=>L};\n"
                 "        {U,L,T,F} -> #{upper=>U,lower=>L,title=>T,fold=>F}\n"
                 "    end.\n\n"),

    io:put_chars(Fd, "spec_version() -> {16,0}.\n\n\n"),
    io:put_chars(Fd, "class(Codepoint) when ?IS_CP(Codepoint) -> \n"
                 "    {CCC,_,_,_} = unicode_table(Codepoint),\n    CCC.\n\n"),

    io:put_chars(Fd, "-spec uppercase(unicode:chardata()) -> "
                 "maybe_improper_list(gc(),unicode:chardata()).\n"),
    io:put_chars(Fd, "uppercase(Str0) ->\n"),
    io:put_chars(Fd, "    case cp(Str0) of\n"),
    io:put_chars(Fd, "        [CP|Str] = Str1 ->\n"),
    io:put_chars(Fd, "            case case_table(CP) of\n"),
    io:put_chars(Fd, "                {Upper,_} -> [Upper|Str];\n"),
    io:put_chars(Fd, "                {Upper,_,_,_} -> [Upper|Str]\n"),
    io:put_chars(Fd, "            end;\n"),
    io:put_chars(Fd, "        [] -> [];\n"),
    io:put_chars(Fd, "        {error,Err} -> error({badarg, Err})\n"),
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
    io:put_chars(Fd, "        [] -> [];\n"),
    io:put_chars(Fd, "        {error,Err} -> error({badarg, Err})\n"),
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
    io:put_chars(Fd, "        [] -> [];\n"),
    io:put_chars(Fd, "        {error,Err} -> error({badarg, Err})\n"),
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
    io:put_chars(Fd, "        [] -> [];\n"),
    io:put_chars(Fd, "        {error,Err} -> error({badarg, Err})\n"),
    io:put_chars(Fd, "    end.\n\n"),

    io:put_chars(Fd, "%% Returns true if the character is considered wide in non east asian context.\n"),
    io:put_chars(Fd, "-spec is_wide(gc()) -> boolean().\n"),
    io:put_chars(Fd, "is_wide(C) when ?IS_CP(C) ->\n"),
    io:put_chars(Fd, "    is_wide_cp(C);\n"),
    io:put_chars(Fd, "is_wide([_, 16#FE0E|Cs]) -> true; %% Presentation sequence\n"),
    io:put_chars(Fd, "is_wide([_, 16#FE0F|Cs]) -> true; %% Presentation sequence\n"),
    io:put_chars(Fd, "is_wide([C|Cs]) when ?IS_CP(C) ->\n"),
    io:put_chars(Fd, "    is_wide_cp(C) orelse is_wide(Cs);\n"),
    io:put_chars(Fd, "is_wide([]) ->\n    false.\n\n"),

    io:put_chars(Fd, "category(CP, lookup_category) ->\n"
                 "    cat_translate(lookup_category(CP));\n"
                 "category(_, Def) -> cat_translate(Def).\n\n"),
    ok.

gen_norm(Fd) ->
    io:put_chars(Fd,
                 "-spec nfd(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.\n"
                 "nfd(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when is_integer(GC), 0 =< GC, GC < 128 -> [GC|R];\n"
                 "        [GC|Str] -> [decompose(GC)|Str];\n"
                 "        [] -> [];\n"
                 "        {error,_}=Error -> Error\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "-spec nfkd(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.\n"
                 "nfkd(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when is_integer(GC), 0 =< GC, GC < 128 -> [GC|R];\n"
                 "        [GC|Str] -> [decompose_compat(GC)|Str];\n"
                 "        [] -> [];\n"
                 "        {error,_}=Error -> Error\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "-spec nfc(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.\n"
                 "nfc(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when is_integer(GC), 0 =< GC, GC < 256 -> [GC|R];\n"
                 "        [GC|Str] -> [compose(decompose(GC))|Str];\n"
                 "        [] -> [];\n"
                 "        {error,_}=Error -> Error\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "-spec nfkc(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.\n"
                 "nfkc(Str0) ->\n"
                 "    case gc(Str0) of\n"
                 "        [GC|R] when is_integer(GC), 0 =< GC, GC < 128 -> [GC|R];\n"
                 "        [GC|Str] -> [compose_compat_0(decompose_compat(GC))|Str];\n"
                 "        [] -> [];\n"
                 "        {error,_}=Error -> Error\n    end.\n\n"
                ),

    io:put_chars(Fd,
                 "decompose(CP) when is_integer(CP), CP < 16#AC00, 16#D7A3 > CP ->\n"
                 "    case unicode_table(CP) of\n"
                 "        {_,[],_,_} -> CP;\n"
                 "        {_,CPs,_,_} -> canonical_order(CPs)\n"
                 "    end;\n"
                 "decompose(CP) ->\n"
                 "   canonical_order(decompose_1(CP)).\n"
                 "\n"
                 "decompose_1(CP) when is_integer(CP), 16#AC00 =< CP, CP =< 16#D7A3 ->\n"
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
                 "        {CCC, [],_,_} -> [{CCC,CP}];\n"
                 "        {_,CPs,_,_} -> CPs\n"
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
                 "        {_, [], [], _} -> CP;\n"
                 "        {_, _, {_,CPs}, _} -> canonical_order(CPs);\n"
                 "        {_, CPs, _, _} -> canonical_order(CPs)\n"
                 "    end;\n"
                 "decompose_compat(CP) ->\n"
                 "   canonical_order(decompose_compat_1(CP)).\n"
                 "\n"
                 "decompose_compat_1(CP) when is_integer(CP), 16#AC00 =< CP, CP =< 16#D7A3 ->\n"
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
                 "        {CCC, [], [], _} -> [{CCC,CP}];\n"
                 "        {_, _, {_,CPs}, _} -> CPs;\n"
                 "        {_, CPs, _, _} -> CPs\n"
                 "    end;\n"
                 "decompose_compat_1([CP|CPs]) ->\n"
                 "    decompose_compat_1(CP) ++ decompose_compat_1(CPs);\n"
                 "decompose_compat_1([]) -> [].\n\n"),


    %% See: https://www.unicode.org/versions/Unicode16.0.0/core-spec/chapter-3/#G49537

    io:put_chars(Fd,
                 """
                 compose(CP) when is_integer(CP) -> CP;
                 compose([Lead,Vowel|Trail]) %% Hangul
                   when is_integer(Lead), 16#1100 =< Lead, Lead =< 16#1112, is_integer(Vowel) ->
                     if 16#1161 =< Vowel, Vowel =< 16#1175 ->
                             CP = 16#AC00 + ((Lead - 16#1100) * 588) + ((Vowel - 16#1161) * 28),
                             case Trail of
                                 [T|Acc] when is_integer(T), 16#11A7 =< T, T =< 16#11C2 ->
                                      nolist(CP+T-16#11A7,Acc);
                                 Acc -> nolist(CP,Acc)
                             end;
                        true ->
                             case compose([Vowel|Trail]) of
                                 [_|_] = CPs -> [Lead|CPs];
                                 CP -> [Lead,CP]
                             end
                     end;
                 compose([Base,Accent]=GC0) ->
                     case compose_pair(Base,Accent) of
                         false -> GC0;
                         GC -> GC
                     end;
                 compose([CP|Many]) ->
                     compose_many(Many, CP, [], class(CP)).

                 compose_many([CP|Rest], Base, Accents, Prev) ->
                     Class = class(CP),
                     case (Prev =:= 0 orelse Prev < Class) andalso compose_pair(Base, CP) of
                         false ->
                             if Class =:= 0 ->
                                   Begin = [Base|lists:reverse(Accents)],
                                   case compose_many(Rest, CP, [], 0) of
                                       [_|_] = GC -> Begin ++ GC;
                                       Composed -> Begin ++ [Composed]
                                   end;
                                true ->
                                   compose_many(Rest, Base, [CP|Accents], Class)
                             end;
                         Combined ->
                             compose_many(Rest, Combined, Accents, Prev)
                     end;
                 compose_many([], Base, [], Prev) ->
                     Base;
                 compose_many([], Base, Accents, Prev) ->
                     [Base|lists:reverse(Accents)].


                 """
                 ),
    io:put_chars(Fd,
                 """
                 compose_compat_0(CP) when is_integer(CP) ->
                     CP;
                 compose_compat_0(L) ->
                     case gc(L) of
                         [First|Rest] ->
                             case compose_compat(First) of
                                 [_|_] = GC -> GC ++ compose_compat_0(Rest);
                                 CP -> [CP|compose_compat_0(Rest)]
                             end;
                         [] -> []
                     end.

                 compose_compat(CP) when is_integer(CP) -> CP;
                 compose_compat([Lead,Vowel|Trail]) %% Hangul
                   when is_integer(Lead), 16#1100 =< Lead, Lead =< 16#1112, is_integer(Vowel) ->
                     if 16#1161 =< Vowel, Vowel =< 16#1175 ->
                             CP = 16#AC00 + ((Lead - 16#1100) * 588) + ((Vowel - 16#1161) * 28),
                             case Trail of
                                 [T|Acc] when is_integer(T), 16#11A7 =< T, T =< 16#11C2 ->
                                     nolist(CP+T-16#11A7,Acc);
                                 Acc -> nolist(CP,Acc)
                             end;
                        true ->
                             case compose_compat([Vowel|Trail]) of
                                 [_|_] = CPs -> [Lead|CPs];
                                 CP -> [Lead,CP]
                             end
                     end;
                 compose_compat([Base,Accent]=GC0) ->
                     case compose_pair(Base,Accent) of
                         false -> GC0;
                         GC -> GC
                     end;
                 compose_compat([CP|Many]) ->
                     compose_compat_many(Many, CP, [], class(CP)).

                 compose_compat_many([CP|Rest], Base, Accents, Prev) ->
                     Class = class(CP),
                     case (Prev =:= 0 orelse Prev < Class) andalso compose_pair(Base, CP) of
                         false ->
                             if Class =:= 0 ->
                                   Begin = [Base|lists:reverse(Accents)],
                                   case compose_compat_many(Rest, CP, [], 0) of
                                       [_|_] = GC -> Begin ++ GC;
                                       Composed -> Begin ++ [Composed]
                                   end;
                                true ->
                                   compose_compat_many(Rest, Base, [CP|Accents], Class)
                             end;
                         Combined ->
                             compose_compat_many(Rest, Combined, Accents, Prev)
                     end;
                 compose_compat_many([], Base, [], Prev) ->
                     Base;
                 compose_compat_many([], Base, Accents, Prev) ->
                     [Base|lists:reverse(Accents)].


                 """),

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
                 " maybe_improper_list() | {error, unicode:chardata()}.\n"),
    io:put_chars(Fd, "cp([C|_]=L) when ?IS_CP(C) -> L;\n"),
    io:put_chars(Fd, "cp([List]) -> cp(List);\n"),
    io:put_chars(Fd, "cp([List|R]) -> cpl(List, R);\n"),
    io:put_chars(Fd, "cp([]) -> [];\n"),
    io:put_chars(Fd, "cp(<<C/utf8, R/binary>>) -> [C|R];\n"),
    io:put_chars(Fd, "cp(<<>>) -> [];\n"),
    io:put_chars(Fd, "cp(<<R/binary>>) -> {error,R}.\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl([C], R) when ?IS_CP(C) -> [C|cpl_1_cont(R)];\n"),
    io:put_chars(Fd, "cpl([C|T], R) when ?IS_CP(C) -> [C|cpl_cont(T, R)];\n"),
    io:put_chars(Fd, "cpl([List], R) -> cpl(List, R);\n"),
    io:put_chars(Fd, "cpl([List|T], R) -> cpl(List, [T|R]);\n"),
    io:put_chars(Fd, "cpl([], R) -> cp(R);\n"),
    io:put_chars(Fd, "cpl(<<C/utf8, T/binary>>, R) -> [C,T|R];\n"),
    io:put_chars(Fd, "cpl(<<>>, R) -> cp(R);\n"),
    io:put_chars(Fd, "cpl(<<B/binary>>, R) -> {error,[B|R]}.\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "%%%\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl_cont([C|T], R) when is_integer(C) -> [C|cpl_cont2(T, R)];\n"),
    io:put_chars(Fd, "cpl_cont([L], R) -> cpl_cont(L, R);\n"),
    io:put_chars(Fd, "cpl_cont([L|T], R) -> cpl_cont(L, [T|R]);\n"),
    io:put_chars(Fd, "cpl_cont([], R) -> cpl_1_cont(R);\n"),
    io:put_chars(Fd, "cpl_cont(T, R) -> [T|R].\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl_cont2([C|T], R) when is_integer(C) -> [C|cpl_cont3(T, R)];\n"),
    io:put_chars(Fd, "cpl_cont2([L], R) -> cpl_cont2(L, R);\n"),
    io:put_chars(Fd, "cpl_cont2([L|T], R) -> cpl_cont2(L, [T|R]);\n"),
    io:put_chars(Fd, "cpl_cont2([], R) -> cpl_1_cont2(R);\n"),
    io:put_chars(Fd, "cpl_cont2(T, R) -> [T|R].\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl_cont3([C], R) when is_integer(C) -> [C|R];\n"),
    io:put_chars(Fd, "cpl_cont3([C|T], R) when is_integer(C) -> [C,T|R];\n"),
    io:put_chars(Fd, "cpl_cont3([L], R) -> cpl_cont3(L, R);\n"),
    io:put_chars(Fd, "cpl_cont3([L|T], R) -> cpl_cont3(L, [T|R]);\n"),
    io:put_chars(Fd, "cpl_cont3([], R) -> cpl_1_cont3(R);\n"),
    io:put_chars(Fd, "cpl_cont3(T, R) -> [T|R].\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "%%%\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl_1_cont([C|T]) when is_integer(C) -> [C|cpl_1_cont2(T)];\n"),
    io:put_chars(Fd, "cpl_1_cont([L]) -> cpl_1_cont(L);\n"),
    io:put_chars(Fd, "cpl_1_cont([L|T]) -> cpl_cont(L, T);\n"),
    io:put_chars(Fd, "cpl_1_cont(T) -> T.\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl_1_cont2([C|T]) when is_integer(C) -> [C|cpl_1_cont3(T)];\n"),
    io:put_chars(Fd, "cpl_1_cont2([L]) -> cpl_1_cont2(L);\n"),
    io:put_chars(Fd, "cpl_1_cont2([L|T]) -> cpl_cont2(L, T);\n"),
    io:put_chars(Fd, "cpl_1_cont2(T) -> T.\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cpl_1_cont3([C|_]=T) when is_integer(C) -> T;\n"),
    io:put_chars(Fd, "cpl_1_cont3([L]) -> cpl_1_cont3(L);\n"),
    io:put_chars(Fd, "cpl_1_cont3([L|T]) -> cpl_cont3(L, T);\n"),
    io:put_chars(Fd, "cpl_1_cont3(T) -> T.\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "%%%\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cp_no_bin([C|_]=L) when is_integer(C) -> L;\n"),
    io:put_chars(Fd, "cp_no_bin([List]) -> cp_no_bin(List);\n"),
    io:put_chars(Fd, "cp_no_bin([List|R]) -> cp_no_binl(List, R);\n"),
    io:put_chars(Fd, "cp_no_bin([]) -> [];\n"),
    io:put_chars(Fd, "cp_no_bin(_) -> binary_found.\n"),
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, "cp_no_binl([C], R) when is_integer(C) -> [C|cpl_1_cont(R)];\n"),
    io:put_chars(Fd, "cp_no_binl([C|T], R) when is_integer(C) -> [C|cpl_cont(T, R)];\n"),
    io:put_chars(Fd, "cp_no_binl([List], R) -> cp_no_binl(List, R);\n"),
    io:put_chars(Fd, "cp_no_binl([List|T], R) -> cp_no_binl(List, [T|R]);\n"),
    io:put_chars(Fd, "cp_no_binl([], R) -> cp_no_bin(R);\n"),
    io:put_chars(Fd, "cp_no_binl(_, _) -> binary_found.\n\n"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_gc(Fd, GBP) ->
    %% see http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
    io:put_chars(Fd,
                 "-spec gc(String::unicode:chardata()) ->"
                 " maybe_improper_list() | {error, unicode:chardata()}.\n"),
    io:put_chars(Fd,
                 "gc([]=R) -> R;\n"
                 "gc([CP]=R) when ?IS_CP(CP) -> R;\n"
                 "gc([$\\r=CP|R0]) ->\n"
                 "    case cp(R0) of % Don't break CRLF\n"
                 "        [$\\n|R1] -> [[$\\r,$\\n]|R1];\n"
                 "        T -> [CP|T]\n"
                 "    end;\n"
                 "gc([CP1|T1]=T) when ?IS_CP(CP1), CP1 < 256 ->\n"
                 "    case T1 of\n"
                 "        [CP2|_] when is_integer(CP2), 0 =< CP2, CP2 < 256 -> T; %% Ascii Fast path\n"
                 "        _ -> %% Keep the tail binary.\n"
                 "            case cp_no_bin(T1) of\n"
                 "                [CP2|_]=T3 when is_integer(CP2), 0 =< CP2, CP2 < 256 -> [CP1|T3]; %% Asciii Fast path\n"
                 "                binary_found -> gc_1(T);\n"
                 "                T4 -> gc_1([CP1|T4])\n"
                 "            end\n"
                 "    end;\n"
                 "gc(<<>>) -> [];\n"
                 "gc(<<CP1/utf8, Rest/binary>>) ->\n"
                 "    if CP1 < 256, CP1 =/= $\\r ->\n"
                 "           case Rest of\n"
                 "               <<CP2/utf8, _/binary>> when CP2 < 256 -> %% Ascii Fast path\n"
                 "                   [CP1|Rest];\n"
                 "               _ -> gc_1([CP1|Rest])\n"
                 "           end;\n"
                 "      true -> gc_1([CP1|Rest])\n"
                 "    end;\n"
                 "gc([CP|_]=T) when ?IS_CP(CP) -> gc_1(T);\n"
                 "gc(Str) ->\n"
                 "    case cp(Str) of\n"
                 "        {error,_}=Error -> Error;\n"
                 "        CPs -> gc(CPs)\n"
                 "    end.\n"
                ),

    GenExtP = fun(Range) -> io:format(Fd, "gc_1~s gc_ext_pict(R1,[CP]);\n", [gen_clause(Range)]) end,
    ExtendedPictographic0 = merge_ranges(maps:get(extended_pictographic,GBP)),
    %% Pick codepoints below 256 (some data knowledge here)
    {ExtendedPictographicLow,ExtendedPictographicHigh} =
        lists:splitwith(fun({Start,undefined}) -> Start < 256 end,ExtendedPictographic0),
    io:put_chars(Fd,
                 "\ngc_1([$\\r|R0] = R) ->\n"
                 "    case cp(R0) of % Don't break CRLF\n"
                 "        [$\\n|R1] -> [[$\\r,$\\n]|R1];\n"
                 "        _ -> R\n"
                 "    end;\n"),
    io:put_chars(Fd, "\n%% Handle control\n"),
    GenControl = fun(Range) -> io:format(Fd, "gc_1~s R0;\n", [gen_clause(Range)]) end,
    CRs0 = merge_ranges(maps:get(cr, GBP) ++ maps:get(lf, GBP) ++ maps:get(control, GBP), false),
    [R1,R2,R3|Crs] = CRs0,
    [GenControl(CP) || CP <- merge_ranges([R1,R2,R3], split), CP =/= {$\r, undefined}],
    %%GenControl(R1),GenControl(R2),GenControl(R3),
    io:put_chars(Fd, "\n%% Optimize Latin-1\n"),
    [GenExtP(CP) || CP <- merge_ranges(ExtendedPictographicLow)],

    io:put_chars(Fd,
                 "gc_1([CP|R]=R0) when is_integer(CP), 0 =< CP, CP < 256 ->\n"
                 "    case R of\n"
                 "        [CP2|_] when is_integer(CP2), 0 =< CP2, CP2 < 256 -> R0;\n"
                 "        _ -> gc_extend(cp(R), R, CP)\n"
                 "    end;\n"
                 "gc_1([CP|_]) when not ?IS_CP(CP) ->\n"
                 "    error({badarg,CP});\n"),
    io:put_chars(Fd, "\n%% Continue control\n"),
    [GenControl(CP) || CP <- Crs],
    %% One clause per CP
    %% CRs0 = merge_ranges(maps:get(cr, GBP) ++ maps:get(lf, GBP) ++ maps:get(control, GBP)),
    %% [GenControl(CP) || CP <- CRs0, CP =/= {$\r, undefined}],

    io:put_chars(Fd, "\n%% Handle prepend\n"),
    GenPrepend = fun(Range) -> io:format(Fd, "gc_1~s gc_prepend(R1, CP);\n", [gen_clause(Range)]) end,
    [GenPrepend(CP) || CP <- merge_ranges(maps:get(prepend,GBP))],

    io:put_chars(Fd, "\n%% Handle Hangul L\n"),
    GenHangulL = fun(Range) -> io:format(Fd, "gc_1~s gc_h_L(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenHangulL(CP) || CP <- merge_ranges(maps:get(l,GBP))],
    io:put_chars(Fd, "%% Handle Hangul V\n"),
    GenHangulV = fun(Range) -> io:format(Fd, "gc_1~s gc_h_V(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenHangulV(CP) || CP <- merge_ranges(maps:get(v,GBP))],
    io:put_chars(Fd, "%% Handle Hangul T\n"),
    GenHangulT = fun(Range) -> io:format(Fd, "gc_1~s gc_h_T(R1,[CP]);\n", [gen_clause(Range)]) end,
    [GenHangulT(CP) || CP <- merge_ranges(maps:get(t,GBP))],
    io:put_chars(Fd, "%% Handle Hangul LV and LVT special, since they are large\n"),
    io:put_chars(Fd, "gc_1([CP|_]=R0) when is_integer(CP), 44000 < CP, CP < 56000 -> gc_h_lv_lvt(R0, R0, []);\n"),

    io:put_chars(Fd, "\n%% Handle Regional\n"),
    GenRegional = fun(Range) -> io:format(Fd, "gc_1~s gc_regional(R1,CP);\n", [gen_clause(Range)]) end,
    [GenRegional(CP) || CP <- merge_ranges(maps:get(regional_indicator,GBP))],
    %% io:put_chars(Fd, "%% Handle E_Base\n"),
    %% GenEBase = fun(Range) -> io:format(Fd, "gc_1~s gc_e_cont(R1,[CP]);\n", [gen_clause(Range)]) end,
    %% [GenEBase(CP) || CP <- merge_ranges(maps:get(e_base,GBP))],
    %% io:put_chars(Fd, "%% Handle EBG\n"),
    %% GenEBG = fun(Range) -> io:format(Fd, "gc_1~s gc_e_cont(R1,[CP]);\n", [gen_clause(Range)]) end,
    %% [GenEBG(CP) || CP <- merge_ranges(maps:get(e_base_gaz,GBP))],

    io:put_chars(Fd, "\n%% Handle extended_pictographic\n"),
    [GenExtP(CP) || CP <- merge_ranges(ExtendedPictographicHigh)],

    io:put_chars(Fd, "\n%% default clauses\n"),
    io:put_chars(Fd,
                 """
                 gc_1([CP|R]) ->
                     case is_indic_consonant(CP) of
                         true ->
                             gc_indic(cp(R), R, false, [CP]);
                         false ->
                             gc_extend(cp(R), R, CP)
                     end.

                 """),

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
                 "      [] -> [CP0];\n"
                 "      {error,R} -> [CP0|R]\n"
                 "    end.\n\n"),

    IsCtrl = fun(Range) -> io:format(Fd, "is_control~s true;\n", [gen_single_clause(Range)]) end,
    [IsCtrl(CP) || CP <- merge_ranges(maps:get(cr, GBP) ++ maps:get(lf, GBP) ++ maps:get(control, GBP))],
    io:put_chars(Fd, "is_control(_) -> false.\n\n"),

    io:put_chars(Fd, "%% Handle Extend\n"
                 "%% To simplify binary handling in libraries the tail should be kept binary\n"
                 "%% and not a lookahead CP\n"
                ),
    io:put_chars(Fd,
                 "gc_extend([CP|T], T0, CP0) ->\n"
                 "    case is_extend(CP) of\n"
                 "        false -> [CP0|T0]; % losing work done on T\n"
                 "        _TrueOrZWJ -> gc_extend2(cp(T), T, [CP,CP0])\n"
                 "    end;\n"
                 "gc_extend([], T0, CP) -> [CP|T0];\n"
                 "gc_extend({error,R}, _, CP) -> [CP|R].\n\n"),
    io:put_chars(Fd,
                 "gc_extend2([CP|T], T0, Acc) ->\n"
                 "    case is_extend(CP) of\n"
                 "        false -> [lists:reverse(Acc)|T0]; % losing work done on T\n"
                 "        _TrueOrZWJ -> gc_extend2(cp(T), T, [CP|Acc])\n"
                 "    end;\n"
                 "gc_extend2([], T0, Acc) ->\n"
                 "    [lists:reverse(Acc)|T0];\n"
                 "gc_extend2({error,R}, _, Acc) ->\n"
                 "    [lists:reverse(Acc)] ++ [R].\n\n"
                 ),
    [{ZWJ, undefined}=ZWJRange] = maps:get(zwj, GBP),
    GenExtend = fun(R) when R =:= ZWJRange -> ok;
                   (Range) -> io:format(Fd, "is_extend~s true;\n", [gen_single_clause(Range)])
                end,
    io:format(Fd, "is_extend(~w) -> zwj;\n", [ZWJ]),
    Extends = merge_ranges(maps:get(extend,GBP)++maps:get(spacingmark, GBP), true),
    [GenExtend(CP) || CP <- Extends],
    io:put_chars(Fd, "is_extend(_) -> false.\n\n"),

    io:put_chars(Fd,
                 "gc_ext_pict(T, Acc) ->\n"
                 "    gc_ext_pict(cp(T), T, Acc).\n\n"
                 "gc_ext_pict([CP|R1], T0, Acc) ->\n"
                 "    case is_extend(CP) of\n"
                 "        zwj -> gc_ext_pict_zwj(cp(R1), R1, [CP|Acc]);\n"
                 "        true -> gc_ext_pict(R1, [CP|Acc]);\n"
                 "        false -> add_acc(Acc, T0)\n"
                 "    end;\n"
                 "gc_ext_pict([], T0, Acc) ->\n"
                 "    add_acc(Acc, T0);\n"
                 "gc_ext_pict({error,R}, T, Acc) ->\n"
                 "    gc_ext_pict([], T, Acc) ++ [R].\n\n"),
    io:put_chars(Fd,
                 "gc_ext_pict_zwj([CP|R1], T0, Acc) ->\n"
                 "    case is_ext_pict(CP) of\n"
                 "        true -> gc_ext_pict(R1, [CP|Acc]);\n"
                 "        false -> add_acc(Acc, T0)\n"
                 "    end;\n"
                 "gc_ext_pict_zwj([], T0, Acc) ->\n"
                 "    add_acc(Acc, T0);\n"
                 "gc_ext_pict_zwj({error,R}, T, Acc) ->\n"
                 "    gc_ext_pict_zwj([], T, Acc) ++ [R].\n\n"),

    GenExtPict = fun(Range) -> io:format(Fd, "is_ext_pict~s true;\n", [gen_single_clause(Range)]) end,
    [GenExtPict(CP) || CP <- ExtendedPictographic0],
    io:put_chars(Fd, "is_ext_pict(_) -> false.\n\n"),

    %% --------------------
    io:put_chars(Fd, "%% Handle Regional\n"),
    [{RLess,RLarge}] = merge_ranges(maps:get(regional_indicator,GBP)),
    io:put_chars(Fd,"gc_regional(R0, CP0) ->\n"
                 "    case cp(R0) of\n"),
    io:format(Fd,   "        [CP|R1] when is_integer(CP), ~w =< CP, CP =< ~w ->\n"
              "             gc_extend2(cp(R1),R1,[CP,CP0]);~n",
              [RLess, RLarge]),
    io:put_chars(Fd,"        R1 -> gc_extend(R1, R0, CP0)\n"
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
    io:put_chars(Fd, "        R1 -> gc_h_lv_lvt(R1, R0, Acc)\n    end.\n\n"),

    io:put_chars(Fd, "%% Handle Hangul V\n"),
    io:put_chars(Fd, "gc_h_V(R0, Acc) ->\n    case cp(R0) of\n"),
    GenHangulV_1 = fun(Range) -> io:format(Fd, "~8c~s gc_h_V(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulV_1(CP) || CP <- merge_ranges(maps:get(v,GBP))],
    GenHangulV_2 = fun(Range) -> io:format(Fd, "~8c~s gc_h_T(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulV_2(CP) || CP <- merge_ranges(maps:get(t,GBP))],
    io:put_chars(Fd,
                 "        R1 ->\n"
                 "            case Acc of\n"
                 "                [CP] -> gc_extend(R1, R0, CP);\n"
                 "                _ -> gc_extend2(R1, R0, Acc)\n"
                 "            end\n    end.\n\n"),
    io:put_chars(Fd, "%% Handle Hangul T\n"),
    io:put_chars(Fd, "gc_h_T(R0, Acc) ->\n    case cp(R0) of\n"),
    GenHangulT_1 = fun(Range) -> io:format(Fd, "~8c~s gc_h_T(R1,[CP|Acc]);\n",
                                           [$\s,gen_case_clause(Range)]) end,
    [GenHangulT_1(CP) || CP <- merge_ranges(maps:get(t,GBP))],
    io:put_chars(Fd,
                 "        R1 ->\n"
                 "            case Acc of\n"
                 "                [CP] -> gc_extend(R1, R0, CP);\n"
                 "                _ -> gc_extend2(R1, R0, Acc)\n"
                 "            end\n    end.\n\n"),
    io:put_chars(Fd, "%% Handle Hangul LV\n"),
    io:put_chars(Fd, "gc_h_lv_lvt([CP|_], _R0, _Acc) when not ?IS_CP(CP) -> error(badarg);\n"),
    GenHangulLV = fun(Range) -> io:format(Fd, "gc_h_lv_lvt~s gc_h_V(R1,[CP|Acc]);\n",
                                          [gen_clause2(Range)]) end,
    [GenHangulLV(CP) || CP <- merge_ranges(maps:get(lv,GBP))],
    io:put_chars(Fd, "%% Handle Hangul LVT\n"),
    GenHangulLVT = fun(Range) -> io:format(Fd, "gc_h_lv_lvt~s gc_h_T(R1,[CP|Acc]);\n",
                                           [gen_clause2(Range)]) end,
    [GenHangulLVT(CP) || CP <- merge_ranges(maps:get(lvt,GBP))],
    io:put_chars(Fd, "gc_h_lv_lvt([CP|R1], _, []) -> gc_extend(cp(R1), R1, CP);\n"), %% From gc_1/1
    io:put_chars(Fd, "%% Also handles error tuples\n"),
    io:put_chars(Fd, "gc_h_lv_lvt(R1, R0, [CP]) -> gc_extend(R1, R0, CP);\n"),
    io:put_chars(Fd, "gc_h_lv_lvt(R1, R0, Acc) -> gc_extend2(R1, R0, Acc).\n\n"),

    %% Indic
    io:put_chars(Fd, "\n%% Handle Indic Conjunt Break\n"),
    GenIndicC = fun(Range) -> io:format(Fd, "is_indic_consonant~s true;\n", [gen_single_clause(Range)]) end,
    [GenIndicC(CP) || CP <- merge_ranges(maps:get(consonant, GBP))],
    io:format(Fd, "is_indic_consonant(_) -> false.\n\n", []),

    GenIndicL = fun(Range) -> io:format(Fd, "is_indic_linker~s true;\n", [gen_single_clause(Range)]) end,
    [GenIndicL(CP) || CP <- merge_ranges(maps:get(virama, GBP))],
    io:format(Fd, "is_indic_linker(_) -> false.\n\n", []),
    %% io:format("Consonants: ~p~n", [merge_ranges(maps:get(consonant, GBP))]),

    io:put_chars(Fd,
                 """
                 gc_indic([CP|R1], R0, FetchedLinker, CPs) ->
                     case is_indic_linker(CP) of
                         true ->
                             gc_indic(cp(R1), R1, true, [CP|CPs]);
                         false ->
                             case is_extend(CP) of
                                 false when FetchedLinker ->
                                     case is_indic_consonant(CP) of
                                         true -> gc_indic(cp(R1), R1, false, [CP|CPs]);
                                         false -> add_acc(CPs, R0)
                                     end;
                                 false ->
                                     add_acc(CPs, R0);
                                 _ ->
                                     gc_indic(cp(R1), R1, FetchedLinker, [CP|CPs])
                             end
                     end;
                 gc_indic([], R0, _, CPs) ->
                     add_acc(CPs, R0);
                 gc_indic({error, R0}, _, _, CPs) ->
                     add_acc(CPs, R0).

                 add_acc([CP], R) -> [CP|R];
                 add_acc(CPs, R) -> [lists:reverse(CPs)|R].


                 """),

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

    io:put_chars(Fd, "nolist(CP, []) when ?IS_CP(CP) -> CP;\n"
                 "nolist(CP, L) when ?IS_CP(CP) -> [CP|L].\n\n"),
    ok.

gen_case_table(Fd, Data) ->
    HC = fun(CP, #cp{cs=Cs}, Acc) ->
                 case case_data(CP, Cs) of
                     default -> Acc;
                     CaseData -> [{CP,CaseData}|Acc]
                 end
         end,
    Case = array:sparse_foldr(HC, [], Data),
    [io:format(Fd, "case_table(~w) -> ~w;\n", [CP, Map])|| {CP,Map} <- Case],
    io:format(Fd, "case_table(CP) -> {CP, CP}.\n\n",[]),
    ok.

case_data(CP, {U0,L0,T0,F0}) ->
    U = def_cp(U0,CP),
    L = def_cp(L0,CP),
    T = def_cp(T0,CP),
    F = def_cp(F0,CP),
    case T =:= U andalso F =:= L of
        true  -> {U,L};
        false -> {U,L,T,F}
    end;
case_data(_, _) ->
    default.

def_cp([], CP) -> CP;
def_cp(CP, _) -> CP.

gen_unicode_table(Fd, Data, UpdateTests) ->
    FixCanon = fun(_, #cp{class=CCC, dec=Dec, comp=Comp, cat=Cat}) ->
                       Canon  = decompose(Dec,Data),
                       #{ccc=>CCC, canonical=>Canon, compat=>Comp, cat=>Cat}
               end,
    AofMaps0 = array:sparse_map(FixCanon, Data),
    FixCompat = fun(_, #{ccc:=CCC, canonical:=Canon, compat:=Comp, cat:=Cat}) ->
                        Compat = decompose_compat(Canon, Comp, AofMaps0),
                        {CCC, Canon, Compat, category(Cat)}
                end,
    AofMaps1 = array:sparse_map(FixCompat, AofMaps0),

    Dict0 = array:sparse_to_orddict(AofMaps1),
    Def = {0, [], [], lookup_category},
    {NonDef, CatTable} = lists:partition(fun({_, {0,[],[],_Cat}}) -> false;
                                            (_) -> true
                                         end, Dict0),

    %% Export testfile
    case UpdateTests of
        true ->
            Dict1 = lists:map(fun({Id,{CCC, Canon, Compat, Cat}}) ->
                                      {_, ECat} = lists:keyfind(Cat, 1, category_translate()),
                                      {Id, {CCC, Canon, Compat, ECat}}
                              end, Dict0),
            TestFile = "../test/unicode_util_SUITE_data/unicode_table.bin",
            io:format("Updating: ~s~n", [TestFile]),
            file:write_file(TestFile, term_to_binary(Dict1, [compressed]));
        false ->
            ignore
    end,

    [io:format(Fd, "unicode_table(~w) -> ~w;~n", [CP, Map]) || {CP,Map} <- NonDef],
    io:format(Fd, "unicode_table(_) -> ~w.~n~n",[Def]),

    [io:format(Fd, "cat_translate(~w) -> ~w;~n", [Cat, EC]) || {Cat,EC} <- category_translate()],
    io:format(Fd, "cat_translate(Cat) -> error({internal_error, Cat}).~n~n",[]),
    gen_category(Fd, CatTable, Data),
    ok.

category([C,Sub]) ->
    list_to_atom([C-$A+$a, Sub]).

category_translate() ->
    [{lu, {letter, uppercase}},       % Letter, Uppercase
     {ll, {letter, lowercase}},       % Letter, Lowercase
     {lt, {letter, titlecase}},       % Letter, Titlecase
     {mn, {mark, non_spacing}},       % Mark, Non-Spacing
     {mc, {mark, spacing_combining}}, % Mark, Spacing Combining
     {me, {mark, enclosing}},         % Mark, Enclosing
     {nd, {number, decimal}},         % Number, Decimal Digit
     {nl, {number, letter}},          % Number, Letter
     {no, {number, other}},           % Number, Other
     {zs, {separator, space}},        % Separator, Space
     {zl, {separator, line}},         % Separator, Line
     {zp, {separator, paragraph}},    % Separator, Paragraph
     {cc, {other, control}},          % Other, Control
     {cf, {other, format}},           % Other, Format
     {cs, {other, surrogate}},        % Other, Surrogate
     {co, {other, private}},          % Other, Private Use
     {cn, {other, not_assigned}},     % Other, Not Assigned (no characters in the file have this property)
     {lm, {letter, modifier}},        % Letter, Modifier
     {lo, {letter, other}},           % Letter, Other
     {pc, {punctuation, connector}},  % Punctuation, Connector
     {pd, {punctuation, dash}},       % Punctuation, Dash
     {ps, {punctuation, open}},       % Punctuation, Open
     {pe, {punctuation, close}},      % Punctuation, Close
     {pi, {punctuation, initial}},    % Punctuation, Initial quote (may behave like Ps or Pe depending on usage)
     {pf, {punctuation, final}},      % Punctuation, Final quote (may behave like Ps or Pe depending on usage)
     {po, {punctuation, other}},      % Punctuation, Other
     {sm, {symbol, math}},            % Symbol, Math
     {sc, {symbol, currency}},        % Symbol, Currency
     {sk, {symbol, modifier}},        % Symbol, Modifier
     {so, {symbol, other}}].          % Symbol, Other

gen_category(Fd, [{CP, {_, _, _, Cat}}|Rest], All) ->
    gen_category(Fd, Rest, Cat, CP, CP, All, []).

gen_category(Fd, [{CP, {_, _, _, NextCat}}|Rest], Cat, Start, End, All, Acc)
  when End+1 =:= CP ->
    IsLetterCat = letter_cat(NextCat, Cat),
    if NextCat =:= Cat ->
            gen_category(Fd, Rest, Cat, Start, CP, All, Acc);
       IsLetterCat ->
            gen_category(Fd, Rest, letter, Start, CP, All, Acc);
       Start =:= End ->
            io:format(Fd, "lookup_category(~w) -> ~w;~n", [Start, Cat]),
            gen_category(Fd, Rest, NextCat, CP, CP, All, Acc);
       true ->
            case Cat of
                letter ->
                    io:format(Fd, "lookup_category(CP) when ~w =< CP, CP =< ~w -> subcat_letter(CP);~n",
                              [Start, End]),
                    gen_category(Fd, Rest, NextCat, CP, CP, All,
                                 lists:reverse(lists:seq(Start, End)) ++ Acc);
                _ ->
                    io:format(Fd, "lookup_category(CP) when ~w =< CP, CP =< ~w -> ~w;~n", [Start, End, Cat]),
                    gen_category(Fd, Rest, NextCat, CP, CP, All, Acc)
            end
    end;
gen_category(Fd, [{CP, {_, _, _, NewCat}}|Rest]=Cont, Cat, Start, End, All, Acc) ->
    case array:get(End+1, All) of
        undefined ->
            if Start =:= End ->
                    io:format(Fd, "lookup_category(~w) -> ~w;~n", [Start, Cat]),
                    gen_category(Fd, Rest, NewCat, CP, CP, All, Acc);
               true ->
                    case Cat of
                        letter ->
                            io:format(Fd, "lookup_category(CP) when ~w =< CP, CP =< ~w -> subcat_letter(CP);~n",
                                      [Start, End]),
                            gen_category(Fd, Rest, NewCat, CP, CP, All,
                                         lists:reverse(lists:seq(Start, End)) ++ Acc);
                        _ ->
                            io:format(Fd, "lookup_category(CP) when ~w =< CP, CP =< ~w -> ~w;~n",
                                      [Start, End, Cat]),
                            gen_category(Fd, Rest, NewCat, CP, CP, All, Acc)
                    end
            end;
        _ ->  %% We can make ranges larger by setting already assigned category
            gen_category(Fd, Cont, Cat, Start, End+1, All, Acc)
    end;
gen_category(Fd, [], Cat, Start, End, All, Acc) ->
    case Start =:= End of
        true ->
            io:format(Fd, "lookup_category(~w) -> ~w;~n", [Start, Cat]);
        false ->
            io:format(Fd, "lookup_category(CP) when ~w =< CP, CP =< ~w -> ~w;~n", [Start, End, Cat])
    end,
    io:put_chars(Fd, "lookup_category(Cp) -> cn.\n\n"),
    gen_letter(Fd, lists:reverse(Acc), All),
    ok.

letter_cat(lm, _) ->
    false;
letter_cat(_, lm) ->
    false;
letter_cat(L1, L2) ->
    is_letter(L1) andalso (L2 =:= letter orelse is_letter(L2)).

is_letter(LC) ->
    lists:member(LC, [lu,ll,lt,lo,lm]).

gen_letter(Fd, Letters, All) ->
    gen_letter(Fd, Letters, All, []).
gen_letter(Fd, [CP|Rest], All, Acc) ->
    case array:get(CP, All) of
        undefined ->
            gen_letter(Fd, Rest, All, Acc);
        #cp{cat=Cat0, cs=Cs} ->
            case {category(Cat0), case_table(CP,case_data(CP, Cs))} of
                {Sub,Sub} ->
                    gen_letter(Fd, Rest, All, Acc);
                {lm,_} ->
                    gen_letter(Fd, Rest, All, Acc);
                {Cat, _Dbg} ->
                    case is_letter(Cat) of
                        true ->
                            gen_letter(Fd, Rest, All, [{CP, Cat}|Acc]);
                        false ->
                            gen_letter(Fd, Rest, All, Acc)
                    end
            end
    end;
gen_letter(Fd, [], _, Acc) ->
    [{Start, Cat}|SCletters] = lists:reverse(Acc),
    subcat_letter(Fd, SCletters, Start, Start, Cat),
    io:put_chars(Fd,
                 "subcat_letter(CP) ->\n"
                 "    case case_table(CP) of\n"
                 "        {CP, CP} -> lo;      %{letter,other};\n"
                 "        {CP, _}  -> lu;      %{letter,uppercase};\n"
                 "        {_, CP}  -> ll;      %{letter,lowercase};\n"
                 "        {_, _, CP, _} -> lt; %{letter,titlecase};\n"
                 "        {CP, _, _, _} -> lu; %{letter,uppercase};\n"
                 "        {_,CP,_,_} -> ll     %{letter,lowercase}\n"
                 "    end.\n\n").

subcat_letter(Fd, [{CP, Cat}|R], Start, End, Cat) when End+1 =:= CP ->
    subcat_letter(Fd, R, Start, CP, Cat);
subcat_letter(Fd, Rest, Start, Start, Cat) ->
    io:format(Fd, "subcat_letter(~w) -> ~w;\n",[Start,Cat]),
    case Rest of
        [] -> ok;
        [{CP, NewCat}|R] -> subcat_letter(Fd, R, CP, CP, NewCat)
    end;
subcat_letter(Fd, Rest, Start, End, Cat) ->
    io:format(Fd, "subcat_letter(CP) when ~w =< CP, CP =< ~w -> ~w;\n",[Start,End,Cat]),
    case Rest of
        [] -> ok;
        [{CP, NewCat}|R] -> subcat_letter(Fd, R, CP, CP, NewCat)
    end.

case_table(CP, CaseData) ->
    case CaseData of
        {CP, CP} -> lo;
        {CP, _}  -> lu;
        {_, CP}  -> ll;
        {_, _, CP, _} -> lt;
        {CP, _, _, _} -> lu;
        {_,CP,_,_} -> ll;
        default -> lo
    end.

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


gen_width_table(Fd, WideChars) ->
    MergedWCs = merge_ranges(WideChars),
    Write = fun(Range) -> io:format(Fd, "is_wide_cp~s true;~n", [gen_single_clause(Range)]) end,
    [Write(Range) || Range <- MergedWCs],
    io:format(Fd, "is_wide_cp(_) -> false.~n", []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_clause({R0, undefined}) ->
    io_lib:format("([~w=CP|R1]=R0) ->", [R0]);
gen_clause({R0, R1}) ->
    io_lib:format("([CP|R1]=R0) when is_integer(CP), ~w =< CP, CP =< ~w ->", [R0,R1]).

gen_clause2({R0, undefined}) ->
    io_lib:format("([~w=CP|R1], R0, Acc) ->", [R0]);
gen_clause2({R0, R1}) ->
    io_lib:format("([CP|R1], R0, Acc) when is_integer(CP), ~w =< CP, CP =< ~w ->", [R0,R1]).

gen_case_clause({R0, undefined}) ->
    io_lib:format("[~w=CP|R1] ->", [R0]);
gen_case_clause({R0, R1}) ->
    io_lib:format("[CP|R1] when is_integer(CP), ~w =< CP, CP =< ~w ->", [R0,R1]).


gen_single_clause({R0, undefined}) ->
    io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
    io_lib:format("(CP) when is_integer(CP), ~w =< CP, CP =< ~w ->", [R0,R1]).


merge_ranges(List) ->
    merge_ranges(List, true).

merge_ranges(List, Opt) ->
    Res0 = merge_ranges_1(lists:usort(List), []),
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
    list_to_integer(string:trim(HexStr, both), 16).

to_atom(Str) ->
    list_to_atom(string:lowercase(string:trim(Str, both))).

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



%% Differs from string:lexemes, it returns empty string as token between two delimiters
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
