%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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
-module(beam_debug_info_SUITE).

%% Make sure that we test running code compiled using the
%% `beam_debug_info` option. This will ensure that we test
%% `beam_disasm` on a module with debug information.
-compile([beam_debug_info]).

-include("beam_opcodes.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2,
         smoke/1,
         fixed_bugs/1,
         empty_module/1,
         call_in_call_args/1,
         missing_vars/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [smoke,
     {group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [fixed_bugs,
       empty_module,
       call_in_call_args,
       missing_vars]}].

init_per_suite(Config) ->
    id(Config),
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

smoke(_Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{}),

    TestBeams0 = get_unique_beam_files(),
    TestBeams = compiler_beams() ++ TestBeams0,

    S = ~"""
         Below, for each module, there is a list of functions with
         variables missing in the BEAM debug info. Note that there
         will probably never be possible to have all variables
         present in the debug info, because some variables die before
         a `debug_line` instruction is reached.

         ** means that at least one of the missing variables is
            significant (does not start with an underscore).

         """,
    io:put_chars(S),

    HasDbgSupport = erlang:system_info(emu_flavor) =:= jit,

    test_lib:p_run(fun(Beam) ->
                           do_smoke(Beam, Node, HasDbgSupport)
                   end, TestBeams),

    peer:stop(Peer),

    ok.


compiler_beams() ->
    filelib:wildcard(filename:join([code:lib_dir(compiler), "ebin", "*.beam"])).

do_smoke(Beam, Node, HasDbgSupport) ->
    try
	{ok,{Mod,[{abstract_code,{raw_abstract_v1,Abstr0}}]}} =
	    beam_lib:chunks(Beam, [abstract_code]),

        %% beam_validator will check for each `debug_line` instruction
        %% that the frame size is correct and that all referenced BEAM
        %% registers are valid.
        {ok,Mod,Code} = compile:forms(Abstr0,
                                      [beam_debug_info,binary,report_errors]),
        {ok,_,Abstr} = compile:forms(Abstr0,
                                     [beam_debug_info,dexp,binary,report_errors]),
        SrcVars = source_variables(Abstr),
        IndexToFunctionMap = abstr_debug_lines(Abstr),

        %% Retrieve the debug information in two different ways.
        {DebugInfo,CookedDebugInfo} = get_debug_info(Mod, Code),
        CookedDebugInfoSorted = lists:sort(CookedDebugInfo),
        DebugInfoBif = case HasDbgSupport of
                           true ->
                               lists:sort(load_get_debug_info(Node, Mod, Code));
                           false ->
                               %% No runtime support for debug info.
                               CookedDebugInfoSorted
                       end,
        if
            CookedDebugInfoSorted =:= DebugInfoBif ->
                ok;
            true ->
                Z0 = lists:zip(CookedDebugInfoSorted, DebugInfoBif,
                               {pad, {short,short}}),
                Z = lists:dropwhile(fun({A,B}) -> A =:= B end, Z0),
                io:format("~p\n", [Z]),
                io:format("~p\n", [CookedDebugInfoSorted]),
                io:format("~p\n", [DebugInfoBif]),

                error(inconsistent_debug_info)
        end,

        case Mod of
            ?MODULE when HasDbgSupport ->
                %% This module has been compiled with `beam_debug_info`.
                CookedDebugInfoSorted = lists:sort(code:get_debug_info(Mod));
            _ ->
                ok
        end,

        {DbgVars,DbgLiterals} = debug_info_vars(DebugInfo, IndexToFunctionMap),

        %% The debug information must only contain variables that are
        %% present in the source code. If the sanity check below
        %% fails, it could be for one of the following reasons:
        %%
        %%   * A compiler pass has introduced a new temporary variable
        %%     whose name is a legal Erlang variable name. (Such
        %%     temporary variables are supposed to have invalid names,
        %%     such as `rec1`.)
        %%
        %%   * Something is wrong in the mapping from `debug_line`
        %%     instruction to function names, causing a variable to be
        %%     collected into the wrong function. (See
        %%     abstr_debug_lines/1.)
        %%
        %%   * A heuristic in source_variables/1 is wrong, causing variables
        %%     that actually are present in the debug information to be
        %%     removed from the list of variables from the source code.

        [] = family_difference(DbgVars, SrcVars),

        %% Now figure out which variables are missing from the debug info
        %% and print them.
        AllDbg = family_union(DbgVars, DbgLiterals),
        Diff0 = family_difference(SrcVars, AllDbg),
        Diff = [begin
                    {Vars,B} = format_vars(Vars0),
                    S = io_lib:format("~p/~p: ~ts", [F,A,Vars]),
                    ["  ",case B of
                              true -> "   " ++ S;
                              false -> "** " ++ S
                          end,"\n"]
                end || {{F,A},Vars0} <:- Diff0],
        io:format("~p:\n~ts", [Mod,Diff])
    catch
	throw:{error,Error} ->
	    io:format("*** compilation failure '~p' for file ~s\n",
		      [Error,Beam]),
	    error;
	Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n", [Beam,Class,Error,Stk]),
	    error
    end.

format_vars(Vs) ->
    Str = lists:join(", ", [io_lib:format("~ts", [V]) || V <- Vs]),
    B = lists:all(fun(V) ->
                          case atom_to_binary(V) of
                              <<"_",_/binary>> -> true;
                              _ -> false
                          end
                  end, Vs),
    {Str,B}.

debug_info_vars(DebugInfo, IndexToFunctionMap) ->
    {Vars0,Literals0} = debug_info_vars_1(DebugInfo, IndexToFunctionMap, [], []),
    Vars = family_union(Vars0),
    Literals = family_union(Literals0),
    {Vars,Literals}.

debug_info_vars_1([{I,{_FrameSize,List}}|T], IndexToFunctionMap, VarAcc, LitAcc) ->
    case debug_info_vars_2(List, [], []) of
        {[],[]} ->
            debug_info_vars_1(T, IndexToFunctionMap, VarAcc, LitAcc);
        {Vars,Literals} ->
            F = map_get(I, IndexToFunctionMap),
            debug_info_vars_1(T, IndexToFunctionMap,
                              [{F,Vars}|VarAcc], [{F,Literals}|LitAcc])
    end;
debug_info_vars_1([], _, VarAcc, LitAcc) ->
    {VarAcc,LitAcc}.

debug_info_vars_2([{Name,_Value}|T], VarAcc, LitAcc) when is_integer(Name) ->
    debug_info_vars_2(T, VarAcc, LitAcc);
debug_info_vars_2([{Name0,Value}|T], VarAcc, LitAcc) when is_binary(Name0) ->
    Name = binary_to_atom(Name0),
    case Value of
        {x,_} -> debug_info_vars_2(T, [Name|VarAcc], LitAcc);
        {y,_} -> debug_info_vars_2(T, [Name|VarAcc], LitAcc);
        {value,_} -> debug_info_vars_2(T, VarAcc, [Name|LitAcc])
    end;
debug_info_vars_2([], VarAcc, LitAcc) ->
    {VarAcc,LitAcc}.

family_union(S0) ->
    S1 = sofs:relation(S0, [{function,[variable]}]),
    S2 = sofs:relation_to_family(S1),
    S3 = sofs:family_union(S2),
    sofs:to_external(S3).

family_union(F0, F1) ->
    S0 = sofs:relation(F0, [{function,[variable]}]),
    S1 = sofs:relation(F1, [{function,[variable]}]),
    S2 = sofs:family_union(S0, S1),
    sofs:to_external(S2).

family_difference(F0, F1) ->
    S0 = sofs:family(F0, [{function,[variable]}]),
    S1 = sofs:family(F1, [{function,[variable]}]),
    S2 = sofs:family_difference(S0, S1),
    SpecFun = fun(S) -> sofs:no_elements(S) =/= 0 end,
    S3 = sofs:family_specification(SpecFun, S2),
    sofs:to_external(S3).

%% Load a module on a remote node and retrieve debug information.
load_get_debug_info(Node, Mod, Beam) ->
    erpc:call(Node,
              fun() ->
                      {module,Mod} = code:load_binary(Mod, "", Beam),
                      DebugInfo = code:get_debug_info(Mod),

                      case Mod of
                          ?MODULE ->
                              %% Don't purge the module that this fun
                              %% is located in.
                              ok;
                          _ ->
                              %% Smoke test of purging a module with
                              %% debug information.
                              _ = code:delete(Mod),
                              _ = code:purge(Mod)
                      end,
                      DebugInfo
              end).

%%
%% Extract variables mentioned in the source code. Try to remove
%% variables that will never show up in the debug information; for
%% examples, definitions of variables that are not followed by any
%% `debug_line` instructions can be ignored.
%%
source_variables(Abstr) ->
    [{{Name,Arity},extract_src_vars(F)} ||
        {function,_,Name,Arity,_}=F <- Abstr].

extract_src_vars(F) ->
    L1 = extract_src_vars(F, true, #{}),
    L2 = [V || V := true <- L1],
    lists:sort(L2).

extract_src_vars({var,_,'_'}, _Lc, Acc) ->
    Acc;
extract_src_vars({var,_,Name}, _Lc, Acc0) ->
    case atom_to_binary(Name) of
        <<"cov",_/binary>> ->
            %% Ignore variable added by the sys_coverage pass.
            Acc0;
        <<"rec",_/binary>> ->
            %% Ignore variable added by the erl_expand_pass.
            Acc0;
        _ ->
            true = beam_ssa_codegen:is_original_variable(Name),
            Acc0#{Name => true}
    end;
extract_src_vars({atom,_,_}, _Lc, Acc) -> Acc;
extract_src_vars({bin,_,Es}, _Lc, Acc) ->
    extract_args(Es, Acc);
extract_src_vars({bin_element,_,Val,Size,_}, _Lc, Acc0) ->
    Acc1 = extract_src_vars(Val, false, Acc0),
    case Size of
        default -> Acc1;
        _ -> extract_src_vars(Size, false, Acc1)
    end;
extract_src_vars({char,_,_}, _Lc, Acc) -> Acc;
extract_src_vars({float,_,_}, _Lc, Acc) -> Acc;
extract_src_vars({integer,_,_}, _Lc, Acc) -> Acc;
extract_src_vars({nil,_}, _Lc, Acc) -> Acc;
extract_src_vars({string,_,_}, _Lc, Acc) -> Acc;
extract_src_vars({cons,_,Hd,Tl}, Lc, Acc0) ->
    Acc1 = extract_src_vars(Hd, Lc, Acc0),
    extract_src_vars(Tl, Lc, Acc1);
extract_src_vars({map,_,Fs}, _Lc, Acc0) ->
    extract_args(Fs, Acc0);
extract_src_vars({map,_,M,Fs}, Lc, Acc0) ->
    Acc1 = extract_src_vars(M, Lc, Acc0),
    extract_args(Fs, Acc1);
extract_src_vars({map_field_assoc,_,K,V}, _Lc, Acc0) ->
    Acc1 = extract_src_vars(K, false, Acc0),
    extract_src_vars(V, false, Acc1);
extract_src_vars({map_field_exact,_,K,V}, _Lc, Acc0) ->
    Acc1 = extract_src_vars(K, false, Acc0),
    extract_src_vars(V, false, Acc1);
extract_src_vars({tuple,_,Es}, _Lc, Acc) ->
    extract_args(Es, Acc);
extract_src_vars({call,_,F,As}, Lc, Acc0) ->
    Acc1 = extract_src_vars(F, Lc, Acc0),
    extract_args(As, Acc1);
extract_src_vars({remote,_,Mod,Name}, Lc, Acc0) ->
    Acc1 = extract_src_vars(Mod, Lc, Acc0),
    extract_src_vars(Name, Lc, Acc1);
extract_src_vars({match,_,P,E}, Lc, Acc0) ->
    Acc1 = extract_src_vars(P, false, Acc0),
    extract_src_vars(E, Lc, Acc1);
extract_src_vars({op,_,_Name,Arg}, Lc, Acc0) ->
    extract_src_vars(Arg, Lc, Acc0);
extract_src_vars({op,_,_Name,Lhs,Rhs}, Lc, Acc0) ->
    Acc1 = extract_src_vars(Lhs, false, Acc0),
    extract_src_vars(Rhs, Lc, Acc1);
extract_src_vars({debug_line,_,_}, _Lc, Acc) ->
    Acc;
extract_src_vars({executable_line,_,_}, _Lc, Acc) ->
    Acc;
extract_src_vars({named_fun,_,Name,Cs}, Lc, Acc0) ->
    case any_debug_line_instrs(Cs) of
        false ->
            %% Since there are no `debug_line` instructions within this fun,
            %% none of the variables defined in the fun should ever
            %% show up in the debug info.
            Acc0;
        true ->
            Acc = case Name of
                      '_' -> Acc0;
                      _ -> extract_src_vars({var,anno,Name}, Lc, Acc0)
                  end,
            extract_cs(Cs, true, Acc)
    end;
extract_src_vars({function,_Anno,_,_,Cs}, _Lc, Acc0) ->
    case any_debug_line_instrs(Cs) of
        true ->
            extract_cs(Cs, true, Acc0);
        false ->
            %% There are no `debug_line` instructions in this
            %% function. This happens if code has been placed in a
            %% header filer, or if the `-file()` attribute has been
            %% used to change the name of the source file.
            Acc0
    end;
extract_src_vars({'fun',_Anno,{clauses,Cs}}, _Lc, Acc0) ->
    case any_debug_line_instrs(Cs) of
        true ->
            extract_cs(Cs, true, Acc0);
        false ->
            Acc0
    end;
extract_src_vars({'fun',_Anno,_}, _Lc, Acc0) -> Acc0;
extract_src_vars({block,_Anno,Es}, Lc, Acc0) ->
    extract_body(Es, Lc, Acc0);
extract_src_vars({'receive',_Anno,Cs}, Lc, Acc0) ->
    extract_cs(Cs, Lc, Acc0);
extract_src_vars({'receive',_Anno,Cs,_To,ToE}, Lc, Acc0) ->
    Acc1 = extract_cs(Cs, Lc, Acc0),
    extract_body(ToE, Lc, Acc1);
extract_src_vars({'maybe',_Anno,Body}, Lc, Acc0) ->
    extract_body(Body, Lc, Acc0);
extract_src_vars({'maybe',_Anno,Body,{'else',_,ElseClauses}}, Lc, Acc0) ->
    Acc1 = extract_body(Body, Lc, Acc0),
    extract_cs(ElseClauses, Lc, Acc1);
extract_src_vars({'maybe_match',_Anno,P,E}, Lc, Acc0) ->
    Acc1 = extract_src_vars(P, false, Acc0),
    extract_src_vars(E, Lc, Acc1);
extract_src_vars({'case',_Anno,E,Cs}, Lc, Acc0) ->
    Acc1 = extract_src_vars(E, false, Acc0),
    extract_cs(Cs, Lc, Acc1);
extract_src_vars({'if',_Anno,Cs}, Lc, Acc0) ->
    extract_cs(Cs, Lc, Acc0);
extract_src_vars({'try',_Anno,Es,Scs,Ccs,As}, Lc, Acc0) ->
    Acc1 = extract_body(Es, false, Acc0),
    Acc2 = extract_cs(Scs, Lc, Acc1),
    Acc3 = extract_cs(Ccs, Lc, Acc2),
    extract_body(As, Lc, Acc3);
extract_src_vars({'catch',_Anno,E}, Lc, Acc0) ->
    extract_src_vars(E, Lc, Acc0);
extract_src_vars({zip,_,Qs0}, _Lc, Acc0) ->
    Qs = extract_sv_qs(Qs0),
    extract_args(Qs, Acc0);
extract_src_vars({C,_,Build,Qs0}, Lc, Acc0)
  when C =:= lc; C =:= bc; C =:= mc ->
    case any_debug_line_instrs(Build) of
        false ->
            Qs = extract_sv_qs(Qs0),
            case any_debug_line_instrs(Qs) of
                false ->
                    Acc0;
                true ->
                    extract_args(Qs, Acc0)
            end;
        true ->
            Acc1 = extract_src_vars(Build, Lc, Acc0),
            extract_args(Qs0, Acc1)
    end;
extract_src_vars({G,_,P,E}, _Lc, Acc0) ->
    true = is_generator(G),                     %Assertion.
    Acc1 = extract_src_vars(P, false, Acc0),
    extract_src_vars(E, false, Acc1).

is_generator(generate) -> true;
is_generator(b_generate) -> true;
is_generator(m_generate) -> true;
is_generator(generate_strict) -> true;
is_generator(b_generate_strict) -> true;
is_generator(m_generate_strict) -> true;
is_generator(_) -> false.

extract_cs([{clause,_,Pats,Gs,Body}|Cs], Lc, Acc0) ->
    case Lc andalso not any_debug_line_instrs(Body)  of
        true ->
            extract_cs(Cs, Lc, Acc0);
        false ->
            Acc1 = extract_args(Pats, Acc0),
            Acc2 = extract_guards(Gs, Acc1),
            Acc3 = extract_body(Body, Lc, Acc2),
            extract_cs(Cs, Lc, Acc3)
    end;
extract_cs([], _, Acc) ->
    Acc.

extract_body([I], Lc, Acc) ->
    case Lc andalso not any_debug_line_instrs(I) of
        true ->
            Acc;
        false ->
            extract_src_vars(I, Lc, Acc)
    end;
extract_body([I|Is], Lc, Acc0) ->
    Acc = extract_src_vars(I, false, Acc0),
    extract_body(Is, Lc, Acc);
extract_body([], _Lc, Acc) -> Acc.

extract_args([A|As], Acc) ->
    extract_args(As, extract_src_vars(A, false, Acc));
extract_args([], Acc) -> Acc.

extract_guards([A|As], Acc) ->
    extract_guards(As, extract_args(A, Acc));
extract_guards([], Acc) -> Acc.

extract_sv_qs([{block,BlkL,[{debug_line,_,_}|Bs]}|Qs1]) ->
    [{block,BlkL,Bs}|extract_sv_qs_1(Qs1)];
extract_sv_qs(Qs) -> Qs.

extract_sv_qs_1([Q|Qs]) ->
    case abstr_extract_debug_lines(Qs, []) of
        [] ->
            [Q];
        [_|_] ->
            [Q|extract_sv_qs_1(Qs)]
    end;
extract_sv_qs_1([]) -> [].

any_debug_line_instrs(Abstr) ->
    abstr_extract_debug_lines(Abstr, []) =/= [].

%%
%% Return a mapping from `debug_line` instruction index to function.
%%
abstr_debug_lines(Abstr) ->
    S0 = [{{Name,Arity},abstr_extract_debug_lines(Body)} ||
             {function,_,Name,Arity,Body} <- Abstr],
    S1 = sofs:family(S0, [{function,[line]}]),
    S2 = sofs:family_to_relation(S1),
    S3 = sofs:converse(S2),
    S4 = sofs:to_external(S3),
    maps:from_list(S4).

abstr_extract_debug_lines(Abstr) ->
    abstr_extract_debug_lines(Abstr, []).

abstr_extract_debug_lines({debug_line,_,Index}, Acc) ->
    [Index|Acc];
abstr_extract_debug_lines([H|T], Acc0) ->
    Acc1 = abstr_extract_debug_lines(H, Acc0),
    abstr_extract_debug_lines(T, Acc1);
abstr_extract_debug_lines(Tuple, Acc0) when is_tuple(Tuple) ->
    abstr_extract_debug_lines(tuple_to_list(Tuple), Acc0);
abstr_extract_debug_lines(_, Acc) -> Acc.

%%%
%%% Read and disassemble the BEAM debug information from the "DbgB"
%%% chunk of a BEAM file.
%%%
get_debug_info(Mod, Beam) ->
    {ok,{Mod,[{"DbgB",DebugInfo0},
              {atoms,Atoms0},
              {"Line",Lines0}]}} = beam_lib:chunks(Beam, ["DbgB",atoms,"Line"]),
    Atoms = maps:from_list(Atoms0),
    Literals = case beam_lib:chunks(Beam, ["LitT"]) of
                   {ok,{Mod,[{"LitT",Literals0}]}} ->
                       decode_literal_table(Literals0);
                   {error,_,_} ->
                       []
               end,
    Op = beam_opcodes:opcode(call, 2),
    <<Version:32,
      _NumItems:32,
      _NumVars:32,
      DebugInfo1/binary>> = DebugInfo0,
    0 = Version,
    RawDebugInfo0 = decode_debug_info(DebugInfo1, Literals, Atoms, Op),
    RawDebugInfo = lists:zip(lists:seq(1, length(RawDebugInfo0)), RawDebugInfo0),

    %% The cooked debug info has line numbers instead of indices.
    Lines = decode_line_table(Lines0, Literals, Atoms),
    {beam_file,Mod,_Exp,_Attr,_Opts,Fs} = beam_disasm:file(Beam),
    DebugMap = #{Index => LocationIndex ||
                   {function,_Name,_Arity,_Entry,Is} <:- Fs,
                   {debug_line,LocationIndex,Index,_Live} <- Is},
    CookedDebugInfo =
        [{map_get(map_get(Index, DebugMap), Lines),Info} ||
            {Index,Info} <:- RawDebugInfo,
            is_map_key(Index, DebugMap)],

    {RawDebugInfo,CookedDebugInfo}.

decode_line_table(<<0:32,_Bits:32,_NumIs:32,NumLines:32,
                    _NumFnames:32, Lines0/binary>>,
                  Literals, Atoms) ->
    Lines = decode_line_tab_1(Lines0, Literals, Atoms, NumLines),
    #{K => V || {K,V} <:- lists:zip(lists:seq(1, length(Lines)), Lines)}.

decode_line_tab_1(_Lines, _Literals, _Atoms, 0) ->
    [];
decode_line_tab_1(Lines0, Literals, Atoms, N) ->
    case decode_arg(Lines0, Literals, Atoms) of
        {{atom,_},Lines1} ->
            decode_line_tab_1(Lines1, Literals, Atoms, N);
        {{integer,Line},Lines1} ->
            [Line|decode_line_tab_1(Lines1, Literals, Atoms, N - 1)];
        {nil,Lines1} ->
            decode_line_tab_1(Lines1, Literals, Atoms, N)
    end.

decode_literal_table(<<0:32,N:32,Tab/binary>>) ->
    #{Index => binary_to_term(Literal) ||
        Index <- lists:seq(0, N - 1) &&
            <<Size:32,Literal:Size/binary>> <:= Tab}.

decode_debug_info(Code0, Literals, Atoms, Op) ->
    case Code0 of
        <<Op,Code1/binary>> ->
            {FrameSize0,Code2} = decode_arg(Code1, Literals, Atoms),
            FrameSize = case FrameSize0 of
                            nil -> none;
                            {atom,entry} -> entry;
                            _ -> FrameSize0
                        end,
            {{list,List0},Code3} = decode_arg(Code2, Literals, Atoms),
            List = decode_list(List0),
            [{FrameSize,List}|decode_debug_info(Code3, Literals, Atoms, Op)];
        <<>> ->
            []
    end.

decode_list([{integer,Var}|T]) when is_integer(Var) ->
    decode_list([{literal,Var}|T]);
decode_list([{literal,Var},Where0|T]) ->
    Where = case Where0 of
                {literal,Lit} -> {value,Lit};
                {atom,A} -> {value,A};
                {integer,I} -> {value,I};
                nil -> {value,[]};
                {x,_} -> Where0;
                {y,_} -> Where0
            end,
    [{Var,Where}|decode_list(T)];
decode_list([]) -> [].

decode_args(0, Code, _Literals, _Atoms) ->
    {[],Code};
decode_args(N, Code0, Literals, Atoms) when is_integer(N), N > 0 ->
    {Arg,Code1} = decode_arg(Code0, Literals, Atoms),
    {Args,Code2} = decode_args(N - 1, Code1, Literals, Atoms),
    {[Arg|Args],Code2}.

decode_arg(Code0, Literals, Atoms) ->
    case decode_raw_arg(Code0) of
        {nil,_}=Res -> Res;
        {{u,N},Code1} ->
            {N,Code1};
        {{atom,Index},Code1} ->
            {{atom,map_get(Index, Atoms)},Code1};
        {{integer,_},_}=Res -> Res;
        {{x,_},_}=Res -> Res;
        {{y,_},_}=Res -> Res;
        {{z,1},Code1} ->
            {{u,N},Code2} = decode_raw_arg(Code1),
            {List,Code3} = decode_args(N, Code2, Literals, Atoms),
            {{list,List},Code3};
        {{z,4},Code1} ->
            {{u,N},Code2} = decode_raw_arg(Code1),
            {{literal,map_get(N, Literals)},Code2}
    end.

decode_raw_arg(<<0:4,0:1,?tag_a:3,Code/binary>>) ->
    {nil,Code};
decode_raw_arg(<<N:4,0:1,Tag:3,Code/binary>>) ->
    {{decode_tag(Tag),N},Code};
decode_raw_arg(<<2#111:3,1:1,1:1,Tag:3,Code0/binary>>) ->
    {{u,W0},Code1} = decode_raw_arg(Code0),
    W = W0 + 9,
    <<N:W/signed-unit:8,Code2/binary>> = Code1,
    {{decode_tag(Tag),N},Code2};
decode_raw_arg(<<W0:3,1:1,1:1,Tag:3,Code0/binary>>) ->
    W = W0 + 2,
    <<N:W/signed-unit:8,Code1/binary>> = Code0,
    {{decode_tag(Tag),N},Code1};
decode_raw_arg(<<High:3,0:1,1:1,Tag:3,Low,Code0/binary>>) ->
    N = (High bsl 8) bor Low,
    {{decode_tag(Tag),N},Code0}.

decode_tag(?tag_u) -> u;
decode_tag(?tag_i) -> integer;
decode_tag(?tag_a) -> atom;
decode_tag(?tag_x) -> x;
decode_tag(?tag_y) -> y;
decode_tag(?tag_z) -> z.

%%%
%%% Other test cases.
%%%

fixed_bugs(_Config) ->
    ok = unassigned_yreg(ok),
    {'EXIT',_} = catch unassigned_yreg(not_ok),

    ~"xyz" = wrong_frame_size(id(~"xyz")),
    boom = catch wrong_frame_size(id(42)),

    {ok,error} = no_function(ok),

    ok.

unassigned_yreg(V) ->
    case id(V) of
        _ ->
            case V of ok -> ok end,
            case catch id(whatever) of
                Y ->
                    case id(true) of
                        true ->
                            id(Y),
                            ok;
                        false ->
                            ok
                    end
            end
    end.

wrong_frame_size(X) ->
    id(X),
    case id(X) of
        Y when is_binary(Y) -> Y;
        _Err -> throw(boom)
    end.

no_function(X) ->
    case catch id(X) of
        ok ->
            case catch id(error) of
                Err ->
                    id(0),
                    id({X, Err})
            end;
        Err ->
            id(0),
            id({X, Err})
    end.


empty_module(_Config) ->
    Mod = list_to_atom(?MODULE_STRING ++ "_" ++
                           atom_to_list(?FUNCTION_NAME)),
    Empty = [{attribute,{1,1},file,{atom_to_list(Mod),1}},
             {attribute,{1,2},module,Mod},
             {eof,{3,1}}],
    {ok,Mod,_Code} = compile:forms(Empty, [beam_debug_info,report]),

    ok.

call_in_call_args(Config) ->
    M = ?FUNCTION_NAME,
    PrivDir = proplists:get_value(priv_dir, Config),
    SrcName = filename:join(PrivDir, atom_to_list(M) ++ ".erl"),

    S = ~"""
         -module(call_in_call_args).
         -export([f/1]).

         f(X) ->
             bar:g(
               bar:h(X),
               id(X)
              ).
         id(I) -> I.
         """,

    ok = file:write_file(SrcName, S),
    {ok,M,Asm} = compile:file(SrcName, [report,beam_debug_info,binary,to_asm]),
    {M,_,_,[{function,f,1,_,Is}|_],_} = Asm,
    DebugLines = [I || I <- Is, element(1, I) =:= debug_line],
    io:format("~p\n", [DebugLines]),
    4 = length(DebugLines),

    ok.

missing_vars(Config) ->
    M = ?FUNCTION_NAME,
    PrivDir = proplists:get_value(priv_dir, Config),
    SrcName = filename:join(PrivDir, atom_to_list(M) ++ ".erl"),

    S = ~"""
         -module(missing_vars).              %%L01
         -export([f/3]).                     %%L02
         f(X, Y, Z0) ->                      %%L03
             case X of                       %%L04
                 false ->                    %%L05
                     Z1 = Z0#{k := Y},       %%L06
                     foo:go(X),              %%L07
                     Z1;                     %%L08
                 _ ->                        %%L09
                     Z1 = Z0#{k := X},       %%L10
                     Z1                      %%L11
             end.                            %%L12
         """,

    ok = file:write_file(SrcName, S),
    {ok,M,Asm} = compile:file(SrcName, [report,beam_debug_info,binary,to_asm]),
    {M,_,_,[{function,f,3,_,Is}|_],_} = Asm,
    DebugLines0 = [begin
                       {location,_File,Line} = lists:keyfind(location, 1, Anno),
                       {Line,FrameSz,[Name || {Name,_} <- Vars]}
                   end || {debug_line,Anno,_,_,{FrameSz,Vars}} <- Is],
    DebugLines = lists:sort(DebugLines0),
    io:format("~p\n", [DebugLines]),
    Expected = [{3,entry,[1,2,3]},
                {4,none,['X','Y','Z0']},
                {6,none,['X','Y','Z0']},
                {7,none,['X','Z0','Z1']},
                {8,1,['Z1']},
                {10,none,['X','Y','Z0']},
                {11,none,['Y','Z0','Z1']}],

    ?assertEqual(Expected, DebugLines),

    ok.


%%%
%%% Common utility functions.
%%%

get_unique_beam_files() ->
    F = fun IsCloned(ModString) ->
                case ModString of
                    "_dialyzer_SUITE" -> true;
                    "_r25_SUITE" -> true;
                    [_|T] -> IsCloned(T);
                    _ -> false
                end
        end,
    test_lib:get_unique_files(".beam", F).

id(I) -> I.
