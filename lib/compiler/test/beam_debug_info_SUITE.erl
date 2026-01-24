%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
         smoke_default/1,
         smoke_save_vars/1,
         calls_reported_correctly/1,
         calls_cornercase_reg_in_call/1,
         fixed_bugs/1,
         empty_module/1,
         call_in_call_args/1,
         missing_vars/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [smoke_default,
     smoke_save_vars,
     calls_reported_correctly,
     calls_cornercase_reg_in_call,
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

smoke_default(_Config) ->
    smoke([]).

smoke_save_vars(_Config) ->
    smoke([beam_debug_stack]).

smoke(ExtraOpts) ->
    {ok, Peer, Node} = ?CT_PEER(#{args => ["+D"]}),

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

    HasDbgSupport = erl_debugger:supported() andalso erlang:system_info(emu_flavor) =:= jit,

    test_lib:p_run(fun(Beam) ->
                           do_smoke(Beam, Node, HasDbgSupport, ExtraOpts)
                   end, TestBeams),

    peer:stop(Peer),

    ok.


compiler_beams() ->
    filelib:wildcard(filename:join([code:lib_dir(compiler), "ebin", "*.beam"])).

do_smoke(Beam, Node, HasDbgSupport, ExtraOpts) ->
    try
	{ok,{Mod,[{abstract_code,{raw_abstract_v1,Abstr0}}]}} =
	    beam_lib:chunks(Beam, [abstract_code]),

        %% beam_validator will check for each `debug_line` instruction
        %% that the frame size is correct and that all referenced BEAM
        %% registers are valid.
        Feat = {features,erl_features:configurable()},
        {ok,Mod,Code} = compile:forms(Abstr0,
                                      [beam_debug_info,binary,
                                       report_errors,Feat|ExtraOpts]),
        {ok,_,Abstr} = compile:forms(Abstr0,
                                     [beam_debug_info,dexp,binary,
                                      report_errors,Feat|ExtraOpts]),
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

debug_info_vars_1([{I,Info}|T], IndexToFunctionMap, VarAcc, LitAcc) ->
    List = maps:get(vars, Info, []),
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
    <<Version:32,
      _NumItems:32,
      _NumTerms:32,
      DebugInfo1/binary>> = DebugInfo0,
    1 = Version,
    RawDebugInfo0 = decode_debug_info(DebugInfo1, Literals, Atoms),
    RawDebugInfo = lists:zip(lists:seq(1, length(RawDebugInfo0)), RawDebugInfo0),

    %% The cooked debug info has line numbers instead of indices.
    Lines = decode_line_table(Lines0, Literals, Atoms),
    {beam_file,Mod,_Exp,_Attr,_Opts,Fs} = beam_disasm:file(Beam),
    DebugMap = #{Index => LocationIndex ||
                   {function,_Name,_Arity,_Entry,Is} <:- Fs,
                   {debug_line,{atom,line},LocationIndex,Index,_Live} <- Is},
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

decode_debug_info(Code0, Literals, Atoms) ->
    Entries = decode_entries(Code0, Literals, Atoms),
    {Infos, NextEntry} = lists:foldr(
        fun
            ({K, V}, {InfosN, NextN}) ->
                Next = case NextN of
                    #{K := _} -> error({duplicated, K});
                    _ -> NextN#{K => V}
                end,
                case K of
                    frame_size -> {[Next|InfosN], #{}};
                    _ -> {InfosN, Next}
                end

        end,
        {[], #{}},
        Entries
    ),
    0 = map_size(NextEntry),
    Infos.

decode_entries(<<>>, _Literals, _Atoms) ->
    [];
decode_entries(Code0, Literals, Atoms) ->
    {Entry, Code1} = decode_entry(Code0, Literals, Atoms),
    [Entry|decode_entries(Code1, Literals, Atoms)].

decode_entry(Code0, Literals, Atoms) ->
    Op = beam_opcodes:opcode(call, 2),
    case Code0 of
        <<Op,Code1/binary>> ->
            {EntryType, Code2} = decode_arg(Code1, Literals, Atoms),
            {Value, Code3} = decode_arg(Code2, Literals, Atoms),
            Entry = case EntryType of
                0 ->
                    case Value of
                        nil ->
                            {frame_size, none};
                        {atom,entry} ->
                            {frame_size, entry};
                        _ when is_integer(Value), Value >= 0 ->
                            {frame_size, Value}
                    end;

                1 ->
                    case Value of
                        {list,List} -> {vars, decode_var_mappings(List)}
                    end;

                2 ->
                    case Value of
                        {list,List} -> {calls, decode_calls(List)}
                    end;

                _ ->
                    error({unknown_entry_type, EntryType})
            end,
            {Entry, Code3}
    end.

decode_var_mappings([{integer,Var}|T]) when is_integer(Var) ->
    decode_var_mappings([{literal,Var}|T]);
decode_var_mappings([{literal,Var},Where0|T]) ->
    Where = case Where0 of
                {literal,Lit} -> {value,Lit};
                {atom,A} -> {value,A};
                {integer,I} -> {value,I};
                nil -> {value,[]};
                {x,_} -> Where0;
                {y,_} -> Where0
            end,
    [{Var,Where}|decode_var_mappings(T)];
decode_var_mappings([]) -> [].

decode_calls([{literal, V}|Rest]) when is_binary(V) ->
    [V|decode_calls(Rest)];
decode_calls([A,M0,F0|Rest]) when A>=0, A=<255 ->
    M = decode_var_or_atom(M0),
    F = decode_var_or_atom(F0),
    [{M,F,A}|decode_calls(Rest)];
decode_calls([A0,F0|Rest]) when A0>=256, A0=<511->
    F = decode_var_or_atom(F0),
    A = A0-256,
    [{F,A}|decode_calls(Rest)];
decode_calls([]) ->
    [].

decode_var_or_atom({literal, V}) when is_binary(V) ->
    V;
decode_var_or_atom({atom, A}) when is_atom(A) ->
    A.

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
                       {Kind,Line,FrameSz,[Name || {Name,_} <- Vars]}
                   end || {debug_line,{atom,Kind},Anno,_,_,#{frame_size:=FrameSz,vars:=Vars}} <- Is],
    DebugLines = lists:sort(DebugLines0),
    Expected = [{entry,3,entry,[1,2,3]},
                {line,4,none,['X','Y','Z0']},
                {line,6,none,['X','Y','Z0']},
                {line,7,none,['X','Z0','Z1']},
                {line,8,1,['Z1']},
                {line,10,none,['X','Y','Z0']},
                {line,11,none,['Y','Z0','Z1']}],

    ?assertEqual(Expected, DebugLines),

    ok.

calls_reported_correctly(Config) ->
    M = ?FUNCTION_NAME,
    S = ~"""
        -module(calls_reported_correctly).                         %L01
        -export([fixtures/1]).                                     %L02
        -record(my_rec, {fld1 :: atom(), fld2 :: integer()}).      %L03
        local() -> ok.                                             %L04                                                          %L04
        fixtures(F) ->                                             %L05
            Y = 42, 'not':toplevel(a, b),                          %L06
            foo:bar(13, Y), Z = 43,                                %L07
            local(),                                               %L08
            X = catch local(),                                     %L09
            ok = foo:bar(Y),                                       %L10
            case foo:blah() of ok -> local();                      %L11
                _ -> foo:bar()                                     %L12
            end,                                                   %L13
            try X = hey:ho(42), local() of                         %L14
                _ -> foo:bar()                                     %L15
            catch                                                  %L16
                _:_ -> foo:blah()                                  %L17
            end,                                                   %L18
            hey:ho(X) + foo:bar(Y),                                %L19
            self() ! foo:bar(Y),                                   %L20
            {hey:ho(X), foo:bar(Y)},                               %L21
            [hey:ho(X), foo:bar(Y) | pim:pam()],                   %L22
            #{hey:ho(X) => foo:bar(Y), blah => pim:pam()},         %L23
            #my_rec{fld1 = hey:ho(X), fld2 = foo:bar(Y)},          %L24
            X:handle_call(1,2,3),                                  %L25
            foo:F(1,2,3),                                          %L26
            X:F(),                                                 %L27
            F(),                                                   %L28
            G = bam, foo:G(42),                                    %L29
            X:G(),                                                 %L30
            G(),  % invalid call                                   %L31
            (fun foo:bar/1)(Z),                                    %L32
            Ref=fun foo:blah/2, Ref(X,Y),                          %L33
            (fun local/0)(),                                       %L34
            erlang:apply(foo, bar, [true, 42]),                    %L35
            erlang:apply(foo, bar, [X, Y]),                        %L36
            [pim:pum(E) || L <- foo:bar(), E <- hey:ho(L)],        %L37
            [ pim:pum(E) ||                                        %L38
                L <- foo:bar(),                                    %L39
                E <- hey:ho(L)],                                   %L40
            [                                                      %L41
                pim:pum(E) ||                                      %L42
                L <- foo:bar(),                                    %L43
                E <- hey:ho(L)                                     %L44
            ],                                                     %L45
            H = fun(X) -> foo:bar(X) + 1 end,                      %L46,
            H(42),                                                 %L47
            K = fun(X) ->                                          %L48
                foo:bar(X) + 1                                     %L49
            end,                                                   %L50,
            K(42).                                                 %L51
    """,
    Expected =  [
        {04, #{calls => []}},
        {06, #{calls => [{'not',toplevel,2}]}},
        {07, #{calls => [{foo, bar, 2}]}},
        {08, #{calls => [{local, 0}]}},
        {09, #{calls => [{local, 0}]}},
        {10, #{calls => [{foo, bar, 1}]}},
        {11, #{calls => [{foo, blah, 0}, {local, 0}]}},
        {12, #{calls => [{foo, bar, 0}]}},
        {14, #{calls => [{hey, ho, 1}, {local, 0}]}},
        {15, #{calls => [{foo, bar, 0}]}},
        {17, #{calls => [{foo, blah, 0}]}},
        {19, #{calls => [{hey, ho, 1}, {foo, bar, 1}]}},
        {20, #{calls => [{foo, bar, 1}, {erlang, '!', 2}]}},
        {21, #{calls => [{hey, ho, 1}, {foo, bar, 1}]}},
        {22, #{calls => [{hey, ho, 1}, {foo, bar, 1}, {pim, pam, 0}]}},
        {23, #{calls => [{hey, ho, 1}, {foo, bar, 1}, {pim, pam, 0}]}},
        {24, #{calls => [{hey, ho, 1}, {foo, bar, 1}]}},
        {25, #{calls => [{~"X", handle_call, 3}]}},
        {26, #{calls => [{foo, ~"F", 3}]}},
        {27, #{calls => [{~"X", ~"F", 0}]}},
        {28, #{calls => [~"F"]}},
        {29, #{calls => [{foo, bam, 1}]}},
        {30, #{calls => [{~"X", bam, 0}]}},
        {31, #{calls => []}},
        {32, #{calls => [{erlang, make_fun, 3}, {foo, bar, 1}]}},
        {33, #{calls => [{erlang, make_fun, 3}, {foo, blah, 2}]}},
        {34, #{calls => [{local, 0}]}},
        {35, #{calls => [{foo, bar, 2}]}},
        {36, #{calls => [{foo, bar, 2}]}},

        % We currently can have a single entry for a debug_line,
        % so the the other function calls, that end up
        % inside the comprehension function, get no entry and no
        % annotation
        {37, #{calls => [{foo, bar, 0}, {'-fixtures/1-lc$^0/1-0-',1}]}},

        % One call missing, still due to debug_line overlapping with
        % main function
        {38, #{calls => []}},
        {39, #{calls => [{foo, bar, 0}, {'-fixtures/1-lc$^2/1-2-',1}]}},
        {40, #{calls => [{hey, ho, 1}, {'-fixtures/1-lc$^3/1-3-',2}]}},

        {41, #{calls => []}},
        {42, #{calls => [{pim, pum, 1}]}},
        {43, #{calls => [{foo, bar, 0}, {'-fixtures/1-lc$^4/1-4-',1}]}},
        {44, #{calls => [{hey, ho, 1}, {'-fixtures/1-lc$^5/1-5-',2}]}},

        % Call inside closure missed due to debug_line conflict
        {46, #{calls => []}},
        {47, #{calls => [{'-fixtures/1-fun-6-',1}]}},

        {48, #{calls => []}},
        {49, #{calls => [{foo, bar, 1}]}},
        {51, #{calls => [{'-fixtures/1-fun-7-',1}]}}
        ],
    check_expected_calls(Config, M, S, Expected).


calls_cornercase_reg_in_call(Config) ->
    M = ?FUNCTION_NAME,
    S = ~"""
    -module(calls_cornercase_reg_in_call).  %L01
    -export([go/1]).                        %L02
    go(X) ->                                %L03
        try                                 %L04
            Y = foo:bar(),                  %L05
            Z = Y:go(),                     %L06
            hey:ho(Y, X, Z)                 %L07
        catch _ -> ok                       %L08
        end.                                %L09
    """,
    Expected = [
        {04, #{calls => []}},
        {05, #{calls => [{foo,bar,0}]}},

        % The result from foo:bar() is in x0, which is
        % passed directly in the `call` instruction as
        % first argument, so we currently lose the
        % connection with Z
        % {06, #{calls => [{~"Y",go,0}]}},
        {06, #{calls => []}},

        {07, #{calls => [{hey,ho,3}]}},
        {08, #{calls => []}}
    ],
    check_expected_calls(Config, M, S, Expected).


%%%
%%% Common utility functions.
%%%

get_unique_beam_files() ->
    F = fun IsCloned(ModString) ->
                case ModString of
                    "_dialyzer_SUITE" -> true;
                    "_r26_SUITE" -> true;
                    [_|T] -> IsCloned(T);
                    _ -> false
                end
        end,
    test_lib:get_unique_files(".beam", F).

id(I) -> I.

check_expected_calls(Config, Mod, ModSrc, Expected) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    SrcName = filename:join(PrivDir, atom_to_list(Mod) ++ ".erl"),
    BeamName = filename:join(PrivDir, atom_to_list(Mod) ++ ".beam"),

    ok = file:write_file(SrcName, ModSrc),
    {ok,M,Beam} = compile:file(SrcName, [return_errors, beam_debug_info,binary]),

    {ok, Peer, Node} = ?CT_PEER(#{args => ["+D"]}),

    ok = erpc:call(Node, fun() ->
        code:load_binary(Mod, BeamName, Beam),

        Actual = [{L, maps:with([calls], Item)} ||
                    {L, Item} <:- code:get_debug_info(M)],

        [?assertEqual(ExpectedL, ActualL) ||
            ExpectedL <- Expected && ActualL <- Actual],

        ok
    end),

    peer:stop(Peer),
    ok.
