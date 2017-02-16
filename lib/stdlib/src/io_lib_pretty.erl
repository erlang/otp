%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(io_lib_pretty).

%%% Pretty printing Erlang terms
%%%
%%% In this module "print" means the formatted printing while "write"
%%% means just writing out onto one line.

-export([print/1,print/2,print/3,print/4,print/5,print/6]).

%%%
%%% Exported functions
%%%

%% print(Term) -> [Chars]
%% print(Term, Column, LineLength, Depth) -> [Chars]
%% Depth = -1 gives unlimited print depth. Use io_lib:write for atomic terms.

-spec print(term()) -> io_lib:chars().

print(Term) ->
    print(Term, 1, 80, -1).

%% print(Term, RecDefFun) -> [Chars]
%% print(Term, Depth, RecDefFun) -> [Chars]
%% RecDefFun = fun(Tag, NoFields) -> [FieldTag] | no
%% Used by the shell for printing records and for Unicode.

-type rec_print_fun() :: fun((Tag :: atom(), NFields :: non_neg_integer()) ->
                                  no | [FieldName :: atom()]).
-type column() :: integer().
-type line_length() :: pos_integer().
-type depth() :: integer().
-type max_chars() :: integer().

-type chars() :: io_lib:chars().
-type option() :: {column, column()}
                | {line_length, line_length()}
                | {depth, depth()}
                | {max_chars, max_chars()}
                | {record_print_fun, rec_print_fun()}
                | {strings, boolean()}
                | {encoding, latin1 | utf8 | unicode}.
-type options() :: [option()].

-spec print(term(), rec_print_fun()) -> chars();
           (term(), options()) -> chars().

print(Term, Options) when is_list(Options) ->
    Col = get_option(column, Options, 1),
    Ll = get_option(line_length, Options, 80),
    D = get_option(depth, Options, -1),
    M = get_option(max_chars, Options, -1),
    RecDefFun = get_option(record_print_fun, Options, no_fun),
    Encoding = get_option(encoding, Options, epp:default_encoding()),
    Strings = get_option(strings, Options, true),
    print(Term, Col, Ll, D, M, RecDefFun, Encoding, Strings);
print(Term, RecDefFun) ->
    print(Term, -1, RecDefFun).

-spec print(term(), depth(), rec_print_fun()) -> chars().

print(Term, Depth, RecDefFun) ->
    print(Term, 1, 80, Depth, RecDefFun).

-spec print(term(), column(), line_length(), depth()) -> chars().

print(Term, Col, Ll, D) ->
    print(Term, Col, Ll, D, _M=-1, no_fun, latin1, true).

-spec print(term(), column(), line_length(), depth(), rec_print_fun()) ->
                   chars().
print(Term, Col, Ll, D, RecDefFun) ->
    print(Term, Col, Ll, D, _M=-1, RecDefFun).

-spec print(term(), column(), line_length(), depth(), max_chars(),
            rec_print_fun()) -> chars().

print(Term, Col, Ll, D, M, RecDefFun) ->
    print(Term, Col, Ll, D, M, RecDefFun, latin1, true).

print(_, _, _, 0, _M, _RF, _Enc, _Str) -> "...";
print(Term, Col, Ll, D, M, RecDefFun, Enc, Str) when Col =< 0 ->
    print(Term, 1, Ll, D, M, RecDefFun, Enc, Str);
print(Term, Col, Ll, D, M0, RecDefFun, Enc, Str) when is_tuple(Term);
                                                      is_list(Term);
                                                      is_map(Term);
                                                      is_bitstring(Term) ->
    If = {_S, Len} = print_length(Term, D, RecDefFun, Enc, Str),
    M = max_cs(M0, Len),
    if
        Len < Ll - Col, Len =< M ->
            write(If);
        true ->
            TInd = while_fail([-1, 4], 
                              fun(I) -> cind(If, Col, Ll, M, I, 0, 0) end, 
                              1),
            pp(If, Col, Ll, M, TInd, indent(Col), 0, 0)
    end;
print(Term, _Col, _Ll, _D, _M, _RF, _Enc, _Str) ->
    io_lib:write(Term).

%%%
%%% Local functions
%%%

max_cs(M, Len) when M < 0 ->
    Len;
max_cs(M, _Len) ->
    M.

-define(ATM(T), is_list(element(1, T))).
-define(ATM_PAIR(Pair),
        ?ATM(element(2, element(1, Pair))) % Key
        andalso
        ?ATM(element(3, element(1, Pair)))). % Value
-define(ATM_FLD(Field), ?ATM(element(4, element(1, Field)))).

pp({_S, Len} = If, Col, Ll, M, _TInd, _Ind, LD, W) 
                      when Len < Ll - Col - LD, Len + W + LD =< M ->
    write(If);
pp({{list,L}, _Len}, Col, Ll, M, TInd, Ind, LD, W) ->
    [$[, pp_list(L, Col + 1, Ll, M, TInd, indent(1, Ind), LD, $|, W + 1), $]];
pp({{tuple,true,L}, _Len}, Col, Ll, M, TInd, Ind, LD, W) ->
    [${, pp_tag_tuple(L, Col, Ll, M, TInd, Ind, LD, W + 1), $}];
pp({{tuple,false,L}, _Len}, Col, Ll, M, TInd, Ind, LD, W) ->
    [${, pp_list(L, Col + 1, Ll, M, TInd, indent(1, Ind), LD, $,, W + 1), $}];
pp({{map,Pairs},_Len}, Col, Ll, M, TInd, Ind, LD, W) ->
    [$#, ${, pp_map(Pairs, Col + 2, Ll, M, TInd, indent(2, Ind), LD, W + 1),
     $}];
pp({{record,[{Name,NLen} | L]}, _Len}, Col, Ll, M, TInd, Ind, LD, W) ->
    [Name, ${, pp_record(L, NLen, Col, Ll, M, TInd, Ind, LD, W + NLen+1), $}];
pp({{bin,S}, _Len}, Col, Ll, M, _TInd, Ind, LD, W) ->
    pp_binary(S, Col + 2, Ll, M, indent(2, Ind), LD, W);
pp({S, _Len}, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    S.

%%  Print a tagged tuple by indenting the rest of the elements
%%  differently to the tag. Tuple has size >= 2.
pp_tag_tuple([{Tag,Tlen} | L], Col, Ll, M, TInd, Ind, LD, W) ->
    TagInd = Tlen + 2,
    Tcol = Col + TagInd,
    S = $,,
    if
        TInd > 0, TagInd > TInd ->
            Col1 = Col + TInd,
            Indent = indent(TInd, Ind),
            [Tag|pp_tail(L, Col1, Tcol, Ll, M, TInd, Indent, LD, S, W+Tlen)];
        true ->
            Indent = indent(TagInd, Ind),
            [Tag, S | pp_list(L, Tcol, Ll, M, TInd, Indent, LD, S, W+Tlen+1)]
    end.

pp_map([], _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    "";
pp_map({dots, _}, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    "...";
pp_map([P | Ps], Col, Ll, M, TInd, Ind, LD, W) ->
    {PS, PW} = pp_pair(P, Col, Ll, M, TInd, Ind, last_depth(Ps, LD), W),
    [PS | pp_pairs_tail(Ps, Col, Col + PW, Ll, M, TInd, Ind, LD, PW)].

pp_pairs_tail([], _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    "";
pp_pairs_tail({dots, _}, _Col0, _Col, _M, _Ll, _TInd, _Ind, _LD, _W) ->
    ",...";
pp_pairs_tail([{_, Len}=P | Ps], Col0, Col, Ll, M, TInd, Ind, LD, W) ->
    LD1 = last_depth(Ps, LD),
    ELen = 1 + Len,
    if
        LD1 =:= 0, ELen + 1 < Ll - Col, W + ELen + 1 =< M, ?ATM_PAIR(P);
        LD1 > 0, ELen < Ll - Col - LD1, W + ELen + LD1 =< M, ?ATM_PAIR(P) ->
            [$,, write_pair(P) |
             pp_pairs_tail(Ps, Col0, Col+ELen, Ll, M, TInd, Ind, LD, W+ELen)];
        true ->
            {PS, PW} = pp_pair(P, Col0, Ll, M, TInd, Ind, LD1, 0),
            [$,, $\n, Ind, PS |
             pp_pairs_tail(Ps, Col0, Col0 + PW, Ll, M, TInd, Ind, LD, PW)]
    end.

pp_pair({_, Len}=Pair, Col, Ll, M, _TInd, _Ind, LD, W)
         when Len < Ll - Col - LD, Len + W + LD =< M ->
    {write_pair(Pair), if
                          ?ATM_PAIR(Pair) ->
                              Len;
                          true ->
                              Ll % force nl
                      end};
pp_pair({{map_pair, K, V}, _Len}, Col0, Ll, M, TInd, Ind0, LD, W) ->
    I = map_value_indent(TInd),
    Ind = indent(I, Ind0),
    {[pp(K, Col0, Ll, M, TInd, Ind0, LD, W), " =>\n",
      Ind | pp(V, Col0 + I, Ll, M, TInd, Ind, LD, 0)], Ll}. % force nl

pp_record([], _Nlen, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    "";
pp_record({dots, _}, _Nlen, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    "...";
pp_record([F | Fs], Nlen, Col0, Ll, M, TInd, Ind0, LD, W0) ->
    Nind = Nlen + 1,
    {Col, Ind, S, W} = rec_indent(Nind, TInd, Col0, Ind0, W0),
    {FS, FW} = pp_field(F, Col, Ll, M, TInd, Ind, last_depth(Fs, LD), W),
    [S, FS | pp_fields_tail(Fs, Col, Col + FW, Ll, M, TInd, Ind, LD, W + FW)].

pp_fields_tail([], _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) ->
    "";
pp_fields_tail({dots, _}, _Col0, _Col, _M, _Ll, _TInd, _Ind, _LD, _W) ->
    ",...";
pp_fields_tail([{_, Len}=F | Fs], Col0, Col, Ll, M, TInd, Ind, LD, W) ->
    LD1 = last_depth(Fs, LD),
    ELen = 1 + Len,
    if
        LD1 =:= 0, ELen + 1 < Ll - Col, W + ELen + 1 =< M, ?ATM_FLD(F);
        LD1 > 0, ELen < Ll - Col - LD1, W + ELen + LD1 =< M, ?ATM_FLD(F) ->
            [$,, write_field(F) |
             pp_fields_tail(Fs, Col0, Col+ELen, Ll, M, TInd, Ind, LD, W+ELen)];
        true ->
            {FS, FW} = pp_field(F, Col0, Ll, M, TInd, Ind, LD1, 0),
            [$,, $\n, Ind, FS |
             pp_fields_tail(Fs, Col0, Col0 + FW, Ll, M, TInd, Ind, LD, FW)]
    end.

pp_field({_, Len}=Fl, Col, Ll, M, _TInd, _Ind, LD, W) 
         when Len < Ll - Col - LD, Len + W + LD =< M ->
    {write_field(Fl), if
                          ?ATM_FLD(Fl) -> 
                              Len;
                          true -> 
                              Ll % force nl
                      end};
pp_field({{field, Name, NameL, F}, _Len}, Col0, Ll, M, TInd, Ind0, LD, W0) ->
    {Col, Ind, S, W} = rec_indent(NameL, TInd, Col0, Ind0, W0 + NameL),
    Sep = case S of
              [$\n | _] -> " =";
              _ -> " = "
          end,
    {[Name, Sep, S | pp(F, Col, Ll, M, TInd, Ind, LD, W)], Ll}. % force nl

rec_indent(RInd, TInd, Col0, Ind0, W0) ->
    Nl = (TInd > 0) and (RInd > TInd),
    DCol = case Nl of
               true -> TInd;
               false -> RInd
           end,
    Col = Col0 + DCol,
    Ind = indent(DCol, Ind0),
    S = case Nl of
            true -> [$\n | Ind];
            false -> ""
        end,
    W = case Nl of
            true -> 0;
            false -> W0
        end,
    {Col, Ind, S, W}.

pp_list({dots, _}, _Col0, _Ll, _M, _TInd, _Ind, _LD, _S, _W) ->
    "...";
pp_list([E | Es], Col0, Ll, M, TInd, Ind, LD, S, W) ->
    {ES, WE} = pp_element(E, Col0, Ll, M, TInd, Ind, last_depth(Es, LD), W),
    [ES | pp_tail(Es, Col0, Col0 + WE, Ll, M, TInd, Ind, LD, S, W + WE)].

pp_tail([], _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, _S, _W) ->
    "";
pp_tail([{_, Len}=E | Es], Col0, Col, Ll, M, TInd, Ind, LD, S, W) ->
    LD1 = last_depth(Es, LD),
    ELen = 1 + Len,
    if 
        LD1 =:= 0, ELen + 1 < Ll - Col, W + ELen + 1 =< M, ?ATM(E);
        LD1 > 0, ELen < Ll - Col - LD1, W + ELen + LD1 =< M, ?ATM(E) ->
            [$,, write(E) | 
             pp_tail(Es, Col0, Col + ELen, Ll, M, TInd, Ind, LD, S, W+ELen)];
        true ->
            {ES, WE} = pp_element(E, Col0, Ll, M, TInd, Ind, LD1, 0),
            [$,, $\n, Ind, ES | 
             pp_tail(Es, Col0, Col0 + WE, Ll, M, TInd, Ind, LD, S, WE)]
    end;
pp_tail({dots, _}, _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, S, _W) ->
    [S | "..."];
pp_tail({_, Len}=E, _Col0, Col, Ll, M, _TInd, _Ind, LD, S, W) 
                  when Len + 1 < Ll - Col - (LD + 1), 
                       Len + 1 + W + (LD + 1) =< M, 
                       ?ATM(E) ->
    [S | write(E)];
pp_tail(E, Col0, _Col, Ll, M, TInd, Ind, LD, S, _W) ->
    [S, $\n, Ind | pp(E, Col0, Ll, M, TInd, Ind, LD + 1, 0)].

pp_element({_, Len}=E, Col, Ll, M, _TInd, _Ind, LD, W) 
           when Len < Ll - Col - LD, Len + W + LD =< M, ?ATM(E) ->
    {write(E), Len};
pp_element(E, Col, Ll, M, TInd, Ind, LD, W) ->
    {pp(E, Col, Ll, M, TInd, Ind, LD, W), Ll}. % force nl

%% Reuse the list created by io_lib:write_binary()...
pp_binary([LT,LT,S,GT,GT], Col, Ll, M, Ind, LD, W) ->
    N = erlang:max(8, erlang:min(Ll - Col, M - 4 - W) - LD),
    [LT,LT,pp_binary(S, N, N, Ind),GT,GT].

pp_binary([BS, $, | S], N, N0, Ind) ->
    Len = length(BS) + 1,
    case N - Len of
        N1 when N1 < 0 ->
            [$\n, Ind, BS, $, | pp_binary(S, N0 - Len, N0, Ind)];
        N1 ->
            [BS, $, | pp_binary(S, N1, N0, Ind)]
    end;
pp_binary([BS1, $:, BS2]=S, N, _N0, Ind) 
         when length(BS1) + length(BS2) + 1 > N ->
    [$\n, Ind, S];
pp_binary(S, N, _N0, Ind) ->
    case iolist_size(S) > N of
        true ->
            [$\n, Ind, S];
        false ->
            S
    end.

write({{tuple, _IsTagged, L}, _}) ->
    [${, write_list(L, $,), $}];
write({{list, L}, _}) ->
    [$[, write_list(L, $|), $]];
write({{map, Pairs}, _}) ->
    [$#,${, write_list(Pairs, $,), $}];
write({{map_pair, _K, _V}, _}=Pair) ->
    write_pair(Pair);
write({{record, [{Name,_} | L]}, _}) ->
    [Name, ${, write_fields(L), $}];
write({{bin, S}, _}) ->
    S;
write({S, _}) ->
    S.

write_pair({{map_pair, K, V}, _}) ->
    [write(K), " => ", write(V)].

write_fields([]) ->
    "";
write_fields({dots, _}) ->
    "...";
write_fields([F | Fs]) ->
    [write_field(F) | write_fields_tail(Fs)].

write_fields_tail([]) ->
    "";
write_fields_tail({dots, _}) ->
    ",...";
write_fields_tail([F | Fs]) ->
    [$,, write_field(F) | write_fields_tail(Fs)].

write_field({{field, Name, _NameL, F}, _}) ->
    [Name, " = " | write(F)].

write_list({dots, _}, _S) ->
    "...";
write_list([E | Es], S) ->
    [write(E) | write_tail(Es, S)].

write_tail([], _S) ->
    [];
write_tail([E | Es], S) ->
    [$,, write(E) | write_tail(Es, S)];
write_tail({dots, _}, S) ->
    [S | "..."];
write_tail(E, S) ->
    [S | write(E)].

%% The depth (D) is used for extracting and counting the characters to
%% print. The structure is kept so that the returned intermediate
%% format can be formatted. The separators (list, tuple, record, map) are
%% counted but need to be added later.

%% D =/= 0
print_length([], _D, _RF, _Enc, _Str) ->
    {"[]", 2};
print_length({}, _D, _RF, _Enc, _Str) ->
    {"{}", 2};
print_length(#{}=M, _D, _RF, _Enc, _Str) when map_size(M) =:= 0 ->
    {"#{}", 3};
print_length(List, D, RF, Enc, Str) when is_list(List) ->
    case Str andalso printable_list(List, D, Enc) of
        true ->
            S = write_string(List, Enc),
            {S, length(S)};
        %% Truncated lists could break some existing code.
        % {true, Prefix} ->
        %    S = write_string(Prefix, Enc),
        %    {[S | "..."], 3 + length(S)};
        false ->
            print_length_list(List, D, RF, Enc, Str)
    end;
print_length(Fun, _D, _RF, _Enc, _Str) when is_function(Fun) ->
    S = io_lib:write(Fun),
    {S, iolist_size(S)};
print_length(R, D, RF, Enc, Str) when is_atom(element(1, R)),
                                      is_function(RF) ->
    case RF(element(1, R), tuple_size(R) - 1) of
        no -> 
            print_length_tuple(R, D, RF, Enc, Str);
        RDefs ->
            print_length_record(R, D, RF, RDefs, Enc, Str)
    end;
print_length(Tuple, D, RF, Enc, Str) when is_tuple(Tuple) ->
    print_length_tuple(Tuple, D, RF, Enc, Str);
print_length(Map, D, RF, Enc, Str) when is_map(Map) ->
    print_length_map(Map, D, RF, Enc, Str);
print_length(<<>>, _D, _RF, _Enc, _Str) ->
    {"<<>>", 4};
print_length(<<_/bitstring>>, 1, _RF, _Enc, _Str) ->
    {"<<...>>", 7};
print_length(<<_/bitstring>>=Bin, D, _RF, Enc, Str) ->
    case bit_size(Bin) rem 8 of
        0 ->
	    D1 = D - 1, 
	    case Str andalso printable_bin(Bin, D1, Enc) of
                {true, List} when is_list(List) ->
                    S = io_lib:write_string(List, $"), %"
	            {[$<,$<,S,$>,$>], 4 + length(S)};
                {false, List} when is_list(List) ->
                    S = io_lib:write_string(List, $"), %"
	            {[$<,$<,S,"/utf8>>"], 9 + length(S)};
	        {true, true, Prefix} ->
	            S = io_lib:write_string(Prefix, $"), %"
	            {[$<,$<, S | "...>>"], 7 + length(S)};
	        {false, true, Prefix} ->
                    S = io_lib:write_string(Prefix, $"), %"
	            {[$<,$<, S | "/utf8...>>"], 12 + length(S)};
	        false ->
	            S = io_lib:write(Bin, D),
	            {{bin,S}, iolist_size(S)}
	    end;
        _ ->
           S = io_lib:write(Bin, D),
	   {{bin,S}, iolist_size(S)}
    end;    
print_length(Term, _D, _RF, _Enc, _Str) ->
    S = io_lib:write(Term),
    {S, lists:flatlength(S)}.

print_length_map(_Map, 1, _RF, _Enc, _Str) ->
    {"#{...}", 6};
print_length_map(Map, D, RF, Enc, Str) when is_map(Map) ->
    Pairs = print_length_map_pairs(maps_to_list(Map, D), D, RF, Enc, Str),
    {{map, Pairs}, list_length(Pairs, 3)}.

maps_to_list(Map, D) when D < 0; map_size(Map) =< D ->
    maps:to_list(Map);
maps_to_list(Map, D) ->
    F = fun(_K, _V, {N, L}) when N =:= D ->
                throw(L);
           (K, V, {N, L}) ->
                {N+1, [{K, V} | L]}
        end,
    lists:reverse(catch maps:fold(F, {0, []}, Map)).

print_length_map_pairs([], _D, _RF, _Enc, _Str) ->
    [];
print_length_map_pairs(_Pairs, 1, _RF, _Enc, _Str) ->
    {dots, 3};
print_length_map_pairs([{K, V} | Pairs], D, RF, Enc, Str) ->
    [print_length_map_pair(K, V, D - 1, RF, Enc, Str) |
     print_length_map_pairs(Pairs, D - 1, RF, Enc, Str)].

print_length_map_pair(K, V, D, RF, Enc, Str) ->
    {KS, KL} = print_length(K, D, RF, Enc, Str),
    {VS, VL} = print_length(V, D, RF, Enc, Str),
    KL1 = KL + 4,
    {{map_pair, {KS, KL1}, {VS, VL}}, KL1 + VL}.

print_length_tuple(_Tuple, 1, _RF, _Enc, _Str) ->
    {"{...}", 5};
print_length_tuple(Tuple, D, RF, Enc, Str) ->
    L = print_length_list1(tuple_to_list(Tuple), D, RF, Enc, Str),
    IsTagged = is_atom(element(1, Tuple)) and (tuple_size(Tuple) > 1),
    {{tuple,IsTagged,L}, list_length(L, 2)}.

print_length_record(_Tuple, 1, _RF, _RDefs, _Enc, _Str) ->
    {"{...}", 5};
print_length_record(Tuple, D, RF, RDefs, Enc, Str) ->
    Name = [$# | io_lib:write_atom(element(1, Tuple))],
    NameL = length(Name),
    Elements = tl(tuple_to_list(Tuple)),
    L = print_length_fields(RDefs, D - 1, Elements, RF, Enc, Str),
    {{record, [{Name,NameL} | L]}, list_length(L, NameL + 2)}.

print_length_fields([], _D, [], _RF, _Enc, _Str) ->
    [];
print_length_fields(_, 1, _, _RF, _Enc, _Str) ->
    {dots, 3};
print_length_fields([Def | Defs], D, [E | Es], RF, Enc, Str) ->
    [print_length_field(Def, D - 1, E, RF, Enc, Str) |
     print_length_fields(Defs, D - 1, Es, RF, Enc, Str)].

print_length_field(Def, D, E, RF, Enc, Str) ->
    Name = io_lib:write_atom(Def),
    {S, L} = print_length(E, D, RF, Enc, Str),
    NameL = length(Name) + 3,
    {{field, Name, NameL, {S, L}}, NameL + L}.

print_length_list(List, D, RF, Enc, Str) ->
    L = print_length_list1(List, D, RF, Enc, Str),
    {{list, L}, list_length(L, 2)}.

print_length_list1([], _D, _RF, _Enc, _Str) ->
    [];
print_length_list1(_, 1, _RF, _Enc, _Str) ->
    {dots, 3};
print_length_list1([E | Es], D, RF, Enc, Str) ->
    [print_length(E, D - 1, RF, Enc, Str) |
     print_length_list1(Es, D - 1, RF, Enc, Str)];
print_length_list1(E, D, RF, Enc, Str) ->
    print_length(E, D - 1, RF, Enc, Str).

list_length([], Acc) ->
    Acc;
list_length([{_, Len} | Es], Acc) ->
    list_length_tail(Es, Acc + Len);
list_length({_, Len}, Acc) ->
    Acc + Len.

list_length_tail([], Acc) ->
    Acc;
list_length_tail([{_,Len} | Es], Acc) ->
    list_length_tail(Es, Acc + 1 + Len);
list_length_tail({_, Len}, Acc) ->
    Acc + 1 + Len.

%% ?CHARS printable characters has depth 1.
-define(CHARS, 4).

printable_list(_L, 1, _Enc) ->
    false;
printable_list(L, _D, latin1) ->
    io_lib:printable_latin1_list(L);
printable_list(L, _D, _Uni) ->
    io_lib:printable_list(L).

printable_bin(Bin, D, Enc) when D >= 0, ?CHARS * D =< byte_size(Bin) ->
    printable_bin(Bin, erlang:min(?CHARS * D, byte_size(Bin)), D, Enc);
printable_bin(Bin, D, Enc) ->
    printable_bin(Bin, byte_size(Bin), D, Enc).

printable_bin(Bin, Len, D, latin1) ->
    N = erlang:min(20, Len),
    L = binary_to_list(Bin, 1, N),
    case printable_latin1_list(L, N) of
        all when N =:= byte_size(Bin)  ->
            {true, L};
        all when N =:= Len -> % N < byte_size(Bin)
            {true, true, L};
        all ->
            case printable_bin1(Bin, 1 + N, Len - N) of
                0 when byte_size(Bin) =:= Len ->
                    {true, binary_to_list(Bin)};
                NC when D > 0, Len - NC >= D ->
                    {true, true, binary_to_list(Bin, 1, Len - NC)};
                NC when is_integer(NC) ->
                    false
            end;
        NC when is_integer(NC), D > 0, N - NC >= D ->
            {true, true, binary_to_list(Bin, 1, N - NC)};
        NC when is_integer(NC) ->
            false
    end;
printable_bin(Bin, Len, D, _Uni) ->
    case valid_utf8(Bin,Len) of
	true ->
	    case printable_unicode(Bin, Len, [], io:printable_range()) of
		{_, <<>>, L} ->
		    {byte_size(Bin) =:= length(L), L};
		{NC, Bin1, L} when D > 0, Len - NC >= D ->
		    {byte_size(Bin)-byte_size(Bin1) =:= length(L), true, L};
		{_NC, _Bin, _L} ->
		    false
	    end;
	false ->
	    printable_bin(Bin, Len, D, latin1)
    end.

printable_bin1(_Bin, _Start, 0) ->
    0;
printable_bin1(Bin, Start, Len) ->
    N = erlang:min(10000, Len),
    L = binary_to_list(Bin, Start, Start + N - 1),
    case printable_latin1_list(L, N) of
        all ->
            printable_bin1(Bin, Start + N, Len - N);
        NC when is_integer(NC) ->
            Len - (N - NC)
    end.

%% -> all | integer() >=0. Adopted from io_lib.erl.
% printable_latin1_list([_ | _], 0) -> 0;
printable_latin1_list([C | Cs], N) when C >= $\s, C =< $~ ->
    printable_latin1_list(Cs, N - 1);
printable_latin1_list([C | Cs], N) when C >= $\240, C =< $\377 ->
    printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\n | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\r | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\t | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\v | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\b | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\f | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([$\e | Cs], N) -> printable_latin1_list(Cs, N - 1);
printable_latin1_list([], _) -> all;
printable_latin1_list(_, N) -> N.

valid_utf8(<<>>,_) ->
    true;
valid_utf8(_,0) ->
    true;
valid_utf8(<<_/utf8, R/binary>>,N) ->
    valid_utf8(R,N-1);
valid_utf8(_,_) ->
    false.

printable_unicode(<<C/utf8, R/binary>>=Bin, I, L, Range) when I > 0 ->
    case printable_char(C,Range) of
        true ->
            printable_unicode(R, I - 1, [C | L],Range);
        false ->
            {I, Bin, lists:reverse(L)}
    end;
printable_unicode(Bin, I, L,_) ->
    {I, Bin, lists:reverse(L)}.

printable_char($\n,_) -> true;
printable_char($\r,_) -> true;
printable_char($\t,_) -> true;
printable_char($\v,_) -> true;
printable_char($\b,_) -> true;
printable_char($\f,_) -> true;
printable_char($\e,_) -> true;
printable_char(C,latin1) ->
    C >= $\s andalso C =< $~ orelse
    C >= 16#A0 andalso C =< 16#FF;
printable_char(C,unicode) ->
    C >= $\s andalso C =< $~ orelse
    C >= 16#A0 andalso C < 16#D800 orelse
    C > 16#DFFF andalso C < 16#FFFE orelse
    C > 16#FFFF andalso C =< 16#10FFFF.

write_string(S, latin1) ->
    io_lib:write_latin1_string(S, $"); %"
write_string(S, _Uni) ->
    io_lib:write_string(S, $"). %"

%% Throw 'no_good' if the indentation exceeds half the line length
%% unless there is room for M characters on the line.

cind({_S, Len}, Col, Ll, M, Ind, LD, W) when Len < Ll - Col - LD,
                                             Len + W + LD =< M ->
    Ind;
cind({{list,L}, _Len}, Col, Ll, M, Ind, LD, W) ->
    cind_list(L, Col + 1, Ll, M, Ind, LD, W + 1);
cind({{tuple,true,L}, _Len}, Col, Ll, M, Ind, LD, W) ->
    cind_tag_tuple(L, Col, Ll, M, Ind, LD, W + 1);
cind({{tuple,false,L}, _Len}, Col, Ll, M, Ind, LD, W) ->
    cind_list(L, Col + 1, Ll, M, Ind, LD, W + 1);
cind({{map,Pairs},_Len}, Col, Ll, M, Ind, LD, W) ->
    cind_map(Pairs, Col + 2, Ll, M, Ind, LD, W + 2);
cind({{record,[{_Name,NLen} | L]}, _Len}, Col, Ll, M, Ind, LD, W) ->
    cind_record(L, NLen, Col, Ll, M, Ind, LD, W + NLen + 1);
cind({{bin,_S}, _Len}, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind;
cind({_S, _Len}, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind.

cind_tag_tuple([{_Tag,Tlen} | L], Col, Ll, M, Ind, LD, W) ->
    TagInd = Tlen + 2,
    Tcol = Col + TagInd,
    if
        Ind > 0, TagInd > Ind ->
            Col1 = Col + Ind,
            if
                M + Col1 =< Ll; Col1 =< Ll div 2 ->
                    cind_tail(L, Col1, Tcol, Ll, M, Ind, LD, W + Tlen);
                true ->
                    throw(no_good)
            end;
        M + Tcol < Ll; Tcol < Ll div 2 ->
            cind_list(L, Tcol, Ll, M, Ind, LD, W + Tlen + 1);
        true ->
            throw(no_good)
    end.

cind_map([P | Ps], Col, Ll, M, Ind, LD, W) ->
    PW = cind_pair(P, Col, Ll, M, Ind, last_depth(Ps, LD), W),
    cind_pairs_tail(Ps, Col, Col + PW, Ll, M, Ind, LD, W + PW);
cind_map(_, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind.

cind_pairs_tail([{_, Len}=P | Ps], Col0, Col, Ll, M, Ind, LD, W) ->
    LD1 = last_depth(Ps, LD),
    ELen = 1 + Len,
    if
        LD1 =:= 0, ELen + 1 < Ll - Col, W + ELen + 1 =< M, ?ATM_PAIR(P);
        LD1 > 0, ELen < Ll - Col - LD1, W + ELen + LD1 =< M, ?ATM_PAIR(P) ->
            cind_pairs_tail(Ps, Col0, Col + ELen, Ll, M, Ind, LD, W + ELen);
        true ->
            PW = cind_pair(P, Col0, Ll, M, Ind, LD1, 0),
            cind_pairs_tail(Ps, Col0, Col0 + PW, Ll, M, Ind, LD, PW)
    end;
cind_pairs_tail(_, _Col0, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind.

cind_pair({{map_pair, _Key, _Value}, Len}=Pair, Col, Ll, M, _Ind, LD, W)
         when Len < Ll - Col - LD, Len + W + LD =< M ->
    if
        ?ATM_PAIR(Pair) ->
            Len;
        true ->
            Ll
    end;
cind_pair({{map_pair, K, V}, _Len}, Col0, Ll, M, Ind, LD, W0) ->
    cind(K, Col0, Ll, M, Ind, LD, W0),
    I = map_value_indent(Ind),
    cind(V, Col0 + I, Ll, M, Ind, LD, 0),
    Ll.

map_value_indent(TInd) ->
    case TInd > 0 of
        true ->
            TInd;
        false ->
            4
    end.

cind_record([F | Fs], Nlen, Col0, Ll, M, Ind, LD, W0) ->
    Nind = Nlen + 1,
    {Col, W} = cind_rec(Nind, Col0, Ll, M, Ind, W0),
    FW = cind_field(F, Col, Ll, M, Ind, last_depth(Fs, LD), W),
    cind_fields_tail(Fs, Col, Col + FW, Ll, M, Ind, LD, W + FW);
cind_record(_, _Nlen, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind.

cind_fields_tail([{_, Len}=F | Fs], Col0, Col, Ll, M, Ind, LD, W) ->
    LD1 = last_depth(Fs, LD),
    ELen = 1 + Len,
    if
        LD1 =:= 0, ELen + 1 < Ll - Col, W + ELen + 1 =< M, ?ATM_FLD(F);
        LD1 > 0, ELen < Ll - Col - LD1, W + ELen + LD1 =< M, ?ATM_FLD(F) ->
            cind_fields_tail(Fs, Col0, Col + ELen, Ll, M, Ind, LD, W + ELen);
        true ->
            FW = cind_field(F, Col0, Ll, M, Ind, LD1, 0),
            cind_fields_tail(Fs, Col0, Col + FW, Ll, M, Ind, LD, FW)
    end;
cind_fields_tail(_, _Col0, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind.

cind_field({{field, _N, _NL, _F}, Len}=Fl, Col, Ll, M, _Ind, LD, W) 
         when Len < Ll - Col - LD, Len + W + LD =< M ->
    if
        ?ATM_FLD(Fl) ->
            Len;
        true ->
            Ll
    end;
cind_field({{field, _Name, NameL, F}, _Len}, Col0, Ll, M, Ind, LD, W0) ->
    {Col, W} = cind_rec(NameL, Col0, Ll, M, Ind, W0 + NameL),
    cind(F, Col, Ll, M, Ind, LD, W),
    Ll.

cind_rec(RInd, Col0, Ll, M, Ind, W0) ->
    Nl = (Ind > 0) and (RInd > Ind),
    DCol = case Nl of
               true -> Ind;
               false -> RInd
           end,
    Col = Col0 + DCol,
    if
        M + Col =< Ll; Col =< Ll div 2 ->        
            W = case Nl of
                    true -> 0;
                    false -> W0
                end,
            {Col, W};
        true ->
            throw(no_good)
    end.

cind_list({dots, _}, _Col0, _Ll, _M, Ind, _LD, _W) ->
    Ind;
cind_list([E | Es], Col0, Ll, M, Ind, LD, W) ->
    WE = cind_element(E, Col0, Ll, M, Ind, last_depth(Es, LD), W),
    cind_tail(Es, Col0, Col0 + WE, Ll, M, Ind, LD, W + WE).

cind_tail([], _Col0, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind;
cind_tail([{_, Len}=E | Es], Col0, Col, Ll, M, Ind, LD, W) ->
    LD1 = last_depth(Es, LD),
    ELen = 1 + Len,
    if 
        LD1 =:= 0, ELen + 1 < Ll - Col, W + ELen + 1 =< M, ?ATM(E);
        LD1 > 0, ELen < Ll - Col - LD1, W + ELen + LD1 =< M, ?ATM(E) ->
            cind_tail(Es, Col0, Col + ELen, Ll, M, Ind, LD, W + ELen);
        true -> 
            WE = cind_element(E, Col0, Ll, M, Ind, LD1, 0),
            cind_tail(Es, Col0, Col0 + WE, Ll, M, Ind, LD, WE)
    end;
cind_tail({dots, _}, _Col0, _Col, _Ll, _M, Ind, _LD, _W) ->
    Ind;
cind_tail({_, Len}=E, _Col0, Col, Ll, M, Ind, LD, W)
                  when Len + 1 < Ll - Col - (LD + 1), 
                       Len + 1 + W + (LD + 1) =< M, 
                       ?ATM(E) ->
    Ind;
cind_tail(E, _Col0, Col, Ll, M, Ind, LD, _W) ->
    cind(E, Col, Ll, M, Ind, LD + 1, 0).

cind_element({_, Len}=E, Col, Ll, M, _Ind, LD, W)
           when Len < Ll - Col - LD, Len + W + LD =< M, ?ATM(E) ->
    Len;
cind_element(E, Col, Ll, M, Ind, LD, W) ->
    cind(E, Col, Ll, M, Ind, LD, W),
    Ll.

last_depth([_ | _], _LD) ->
    0;
last_depth(_, LD) ->
    LD + 1.

while_fail([], _F, V) ->
    V;
while_fail([A | As], F, V) ->
    try F(A) catch _ -> while_fail(As, F, V) end.

indent(N) when is_integer(N), N > 0 ->
    chars($\s, N-1).

indent(1, Ind) -> % Optimization of common case
    [$\s | Ind];
indent(4, Ind) -> % Optimization of common case
    S2 = [$\s, $\s],
    [S2, S2 | Ind];
indent(N, Ind) when is_integer(N), N > 0 ->
    [chars($\s, N) | Ind].

%% A deep version of string:chars/2
chars(_C, 0) ->
    [];
chars(C, 2) ->
    [C, C];
chars(C, 3) ->
    [C, C, C];
chars(C, N) when (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S | S];
chars(C, N) ->
    S = chars(C, N bsr 1),
    [C, S | S].

get_option(Key, TupleList, Default) ->
    case lists:keyfind(Key, 1, TupleList) of
	false -> Default;
	{Key, Value} -> Value;
	_ -> Default
    end.
