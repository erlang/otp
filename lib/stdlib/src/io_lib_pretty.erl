%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

print(Term) ->
    print(Term, 1, 80, -1).

%% print(Term, RecDefFun) -> [Chars]
%% print(Term, Depth, RecDefFun) -> [Chars]
%% RecDefFun = fun(Tag, NoFields) -> [FieldTag] | no
%% Used by the shell for printing records.
print(Term, RecDefFun) ->
    print(Term, -1, RecDefFun).

print(Term, Depth, RecDefFun) ->
    print(Term, 1, 80, Depth, RecDefFun).

print(Term, Col, Ll, D) ->
    print(Term, Col, Ll, D, _M=-1, no_fun).

print(Term, Col, Ll, D, RecDefFun) ->
    print(Term, Col, Ll, D, _M=-1, RecDefFun).

print(_, _, _, 0, _M, _RF) -> "...";
print(Term, Col, Ll, D, M, RecDefFun) when Col =< 0 ->
    print(Term, 1, Ll, D, M, RecDefFun);
print(Term, Col, Ll, D, M0, RecDefFun) when is_tuple(Term); 
                                            is_list(Term) ->
    If = {_S, Len} = print_length(Term, D, RecDefFun),
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
print(<<_/bitstring>>=Term, Col, Ll, D, M0, RecDefFun) ->
    If = {_S, Len} = print_length(Term, D, RecDefFun),
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
print(Term, _Col, _Ll, _D, _M, _RF) ->
    io_lib:write(Term).

%%%
%%% Local functions
%%%

max_cs(M, Len) when M < 0 ->
    Len;
max_cs(M, _Len) ->
    M.

-define(ATM(T), is_list(element(1, T))).
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
    {[Name, " = ", S | pp(F, Col, Ll, M, TInd, Ind, LD, W)], Ll}. % force nl

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
write({{record, [{Name,_} | L]}, _}) ->
    [Name, ${, write_fields(L), $}];
write({{bin, S}, _}) ->
    S;
write({S, _}) ->
    S.

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
%% format can be formatted. The separators (list, tuple, record) are
%% counted but need to be added later.

%% D =/= 0
print_length([], _D, _RF) ->
    {"[]", 2};
print_length({}, _D, _RF) ->
    {"{}", 2};
print_length(List, D, RF) when is_list(List) ->
    case printable_list(List, D) of
        true ->
            S = io_lib:write_string(List, $"), %"
            {S, length(S)};
        %% Truncated lists could break some existing code.
        % {true, Prefix} ->
        %    S = io_lib:write_string(Prefix, $"), %"
        %    {[S | "..."], 3 + length(S)};
        false ->
            print_length_list(List, D, RF)
    end;
print_length(Fun, _D, _RF) when is_function(Fun) ->
    S = io_lib:write(Fun),
    {S, iolist_size(S)};
print_length(R, D, RF) when is_atom(element(1, R)), 
                            is_function(RF) ->
    case RF(element(1, R), tuple_size(R) - 1) of
        no -> 
            print_length_tuple(R, D, RF);
        RDefs ->
            print_length_record(R, D, RF, RDefs)
    end;
print_length(Tuple, D, RF) when is_tuple(Tuple) ->
    print_length_tuple(Tuple, D, RF);
print_length(<<>>, _D, _RF) ->
    {"<<>>", 4};
print_length(<<_/bitstring>>, 1, _RF) ->
    {"<<...>>", 7};
print_length(<<_/bitstring>>=Bin, D, _RF) ->
    case bit_size(Bin) rem 8 of
        0 ->
	    D1 = D - 1, 
	    case printable_bin(Bin, D1) of
	        List when is_list(List) ->
                    S = io_lib:write_string(List, $"),
	            {[$<,$<,S,$>,$>], 4 + length(S)};
	        {true, Prefix} -> 
	            S = io_lib:write_string(Prefix, $"), 
	            {[$<,$<, S | "...>>"], 4 + length(S)};
	        false ->
	            S = io_lib:write(Bin, D),
	            {{bin,S}, iolist_size(S)}
	    end;
        _ ->
           S = io_lib:write(Bin, D),
	   {{bin,S}, iolist_size(S)}
    end;    
print_length(Term, _D, _RF) ->
    S = io_lib:write(Term),
    {S, iolist_size(S)}.

print_length_tuple(_Tuple, 1, _RF) ->
    {"{...}", 5};
print_length_tuple(Tuple, D, RF) ->
    L = print_length_list1(tuple_to_list(Tuple), D, RF),
    IsTagged = is_atom(element(1, Tuple)) and (tuple_size(Tuple) > 1),
    {{tuple,IsTagged,L}, list_length(L, 2)}.

print_length_record(_Tuple, 1, _RF, _RDefs) ->
    {"{...}", 5};
print_length_record(Tuple, D, RF, RDefs) ->
    Name = [$# | io_lib:write_atom(element(1, Tuple))],
    NameL = length(Name),
    L = print_length_fields(RDefs, D - 1, tl(tuple_to_list(Tuple)), RF),
    {{record, [{Name,NameL} | L]}, list_length(L, NameL + 2)}.

print_length_fields([], _D, [], _RF) ->
    [];
print_length_fields(_, 1, _, _RF) -> 
    {dots, 3};
print_length_fields([Def | Defs], D, [E | Es], RF) ->
    [print_length_field(Def, D - 1, E, RF) | 
     print_length_fields(Defs, D - 1, Es, RF)].

print_length_field(Def, D, E, RF) ->
    Name = io_lib:write_atom(Def),
    {S, L} = print_length(E, D, RF),
    NameL = length(Name) + 3,
    {{field, Name, NameL, {S, L}}, NameL + L}.

print_length_list(List, D, RF) -> 
    L = print_length_list1(List, D, RF),
    {{list, L}, list_length(L, 2)}.

print_length_list1([], _D, _RF) ->
    [];
print_length_list1(_, 1, _RF) ->
    {dots, 3};
print_length_list1([E | Es], D, RF) ->
    [print_length(E, D - 1, RF) | print_length_list1(Es, D - 1, RF)];
print_length_list1(E, D, RF) ->
    print_length(E, D - 1, RF).

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

printable_list(L, D) when D < 0 ->
    io_lib:printable_list(L);
printable_list(_L, 1) ->
    false;
printable_list(L, _D) ->
    io_lib:printable_list(L).
%% Truncated lists could break some existing code.
% printable_list(L, D) ->
%     Len = ?CHARS * (D - 1),
%     case printable_list1(L, Len) of
%         all ->
%             true;
%         N when is_integer(N), Len - N >= D - 1 ->
%             {L1, _} = lists:split(Len - N, L),
%             {true, L1};
%         N when is_integer(N) ->
%             false
%     end.

printable_bin(Bin, D) when D >= 0, ?CHARS * D =< byte_size(Bin) ->
    printable_bin(Bin, erlang:min(?CHARS * D, byte_size(Bin)), D);
printable_bin(Bin, D) ->
    printable_bin(Bin, byte_size(Bin), D).

printable_bin(Bin, Len, D) ->
    N = erlang:min(20, Len),
    L = binary_to_list(Bin, 1, N),
    case printable_list1(L, N) of
        all when N =:= byte_size(Bin)  ->
            L;
        all when N =:= Len -> % N < byte_size(Bin)
            {true, L};
        all ->
            case printable_bin1(Bin, 1 + N, Len - N) of
                0 when byte_size(Bin) =:= Len ->
                    binary_to_list(Bin);
                NC when D > 0, Len - NC >= D ->
                    {true, binary_to_list(Bin, 1, Len - NC)};
                NC when is_integer(NC) ->
                    false
            end;
        NC when is_integer(NC), D > 0, N - NC >= D ->
            {true, binary_to_list(Bin, 1, N - NC)};
        NC when is_integer(NC) ->
            false
    end.

printable_bin1(_Bin, _Start, 0) ->
    0;
printable_bin1(Bin, Start, Len) ->
    N = erlang:min(10000, Len),
    L = binary_to_list(Bin, Start, Start + N - 1),
    case printable_list1(L, N) of
        all ->
            printable_bin1(Bin, Start + N, Len - N);
        NC when is_integer(NC) ->
            Len - (N - NC)
    end.

%% -> all | integer() >=0. Adopted from io_lib.erl.
% printable_list1([_ | _], 0) -> 0;
printable_list1([C | Cs], N) when is_integer(C), C >= $\s, C =< $~ ->
    printable_list1(Cs, N - 1);
printable_list1([C | Cs], N) when is_integer(C), C >= $\240, C =< $\377 ->
    printable_list1(Cs, N - 1);
printable_list1([$\n | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\r | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\t | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\v | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\b | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\f | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\e | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([], _) -> all;
printable_list1(_, N) -> N.

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
