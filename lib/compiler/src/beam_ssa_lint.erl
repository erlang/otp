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
%% Purpose: Internal consistency checks for the beam_ssa format.

-module(beam_ssa_lint).
-moduledoc false.

-export([module/2, format_error/1]).

-import(lists, [append/1, foldl/3, foreach/2]).

-include("beam_ssa.hrl").

-spec module(#b_module{}, [compile:option()]) ->
                    {'ok',#b_module{}} | {'error',list()}.
module(#b_module{body=Fs,name=Name}=Mod0, _Options) ->
    Es0 = append([validate_function(F) || F <- Fs]),
    case [{none,?MODULE,E} || E <- Es0] of
        [] ->
            {ok, Mod0};
        [_|_]=Es ->
            {error,[{atom_to_list(Name), Es}]}
    end.

-spec format_error(term()) -> iolist().
format_error({{_M,F,A},Error}) ->
    [io_lib:format("~p/~p: ", [F,A]),format_error_1(Error)].

format_instr(I) ->
    [$',beam_ssa_pp:format_instr(I),$'].

format_vars(Vs) ->
    [$[, format_vars_1(Vs), $]].

format_vars_1([V]) ->
    [beam_ssa_pp:format_var(V)];
format_vars_1([V | Vs]) ->
    [beam_ssa_pp:format_var(V), $, | format_vars_1(Vs)];
format_vars_1([]) ->
    [].

format_var(V) ->
    beam_ssa_pp:format_var(V).

validate_function(F) ->
    try
        validate_variables(F),
        []
    catch
        throw:Reason ->
            #{func_info:=MFA} = F#b_function.anno,
            [{MFA,Reason}];
        Class:Error:Stack ->
            io:fwrite("Function: ~p\n", [F#b_function.anno]),
            erlang:raise(Class, Error, Stack)
    end.

-type variables() :: gb_sets:set(beam_ssa:argument()).
-type try_tags() :: gb_sets:set(beam_ssa:b_var()).
-type branch() :: {variables(), try_tags()}.

-record(vvars,
        { blocks :: #{ beam_ssa:label() => beam_ssa:b_blk() },
          branches :: #{ %% Describes the variable state at the time of
                         %% this exact branch (phi node validation).
                         {From :: beam_ssa:label(),
                          To :: beam_ssa:label()} => branch(),
                         %% Describes the variable state common to all
                         %% branches leading to this label (un/redefined
                         %% variable validation).
                         beam_ssa:label() => branch() },
          variables :: variables(),
          try_tags :: try_tags() }).

-spec validate_variables(beam_ssa:b_function()) -> ok.
validate_variables(#b_function{ args = Args, bs = Blocks }) ->
    %% Prefill the mapping with function arguments.
    Args = vvars_get_variables(Args),
    Vars = gb_sets:from_list(Args),
    TryTags = gb_sets:new(),
    Entry = 0,

    State = #vvars{blocks = Blocks,
                   branches = #{ Entry => {Vars, TryTags} },
                   variables = Vars,
                   try_tags = TryTags },
    ok = vvars_assert_unique(Blocks, Args),
    vvars_phi_nodes(vvars_block(Entry, State)).

%% Checks the uniqueness of all variables across all blocks.
-spec vvars_assert_unique(Blocks, [beam_ssa:b_var()]) -> ok when
      Blocks :: #{ beam_ssa:label() => beam_ssa:b_blk() }.
vvars_assert_unique(Blocks, Args) ->
    BlockIs = [Is || #b_blk{is=Is} <:- maps:values(Blocks)],
    Defined0 = #{V => argument || V <- Args},
    _ = foldl(fun(Is, Defined) ->
                      vvars_assert_unique_1(Is, Defined)
              end, Defined0, BlockIs),
    ok.

-spec vvars_assert_unique_1(Is, Defined) -> ok when
      Is :: list(beam_ssa:b_set()),
      Defined :: #{ beam_ssa:b_var() => beam_ssa:b_set() }.
vvars_assert_unique_1([#b_set{dst=Dst,anno=Anno}=I|Is], Defined) ->
    check_anno(Anno),
    case Defined of
        #{Dst:=Old} -> throw({redefined_variable, Dst, Old, I});
        _ -> vvars_assert_unique_1(Is, Defined#{Dst=>I})
    end;
vvars_assert_unique_1([], Defined) ->
    Defined.

-spec vvars_phi_nodes(State :: #vvars{}) -> ok.
vvars_phi_nodes(#vvars{ blocks = Blocks }=State) ->
    _ = [vvars_phi_nodes_1(Is, Id, State) ||
            Id := #b_blk{ is = Is } <:- Blocks],
    ok.

-spec vvars_phi_nodes_1(Is, Id, State) -> ok when
      Is :: list(beam_ssa:b_set()),
      Id :: beam_ssa:label(),
      State :: #vvars{}.
vvars_phi_nodes_1([#b_set{ op = phi, args = Phis }=I | Is], Id, State) ->
    ok = vvars_assert_phi_paths(Phis, I, Id, State),
    ok = vvars_assert_phi_vars(Phis, I, Id, State),
    vvars_phi_nodes_1(Is, Id, State);
vvars_phi_nodes_1([_ | Is], Id, _State) ->
    case [Dst || #b_set{op=phi,dst=Dst} <- Is] of
        [Var|_] ->
            throw({phi_inside_block, Var, Id});
        [] ->
            ok
    end;
vvars_phi_nodes_1([], _Id, _State) ->
    ok.

%% Checks whether all paths leading to this phi node are represented, and that
%% it doesn't reference any non-existent paths.
-spec vvars_assert_phi_paths(Phis, I, Id, State) -> ok when
      Phis :: list({beam_ssa:argument(), beam_ssa:label()}),
      Id :: beam_ssa:label(),
      I :: beam_ssa:b_set(),
      State :: #vvars{}.
vvars_assert_phi_paths(Phis, I, Id, State) ->
    BranchKeys = maps:keys(State#vvars.branches),
    RequiredPaths = ordsets:from_list([From || {From, To} <- BranchKeys, To =:= Id]),
    ProvidedPaths = ordsets:from_list([From || {_Value, From} <:- Phis]),
    case ordsets:subtract(RequiredPaths, ProvidedPaths) of
        [_|_]=MissingPaths -> throw({missing_phi_paths, MissingPaths, I});
        [] -> ok
    end.
%% %% The following test is sometimes useful to find missing optimizations.
%% %% It is commented out, though, because it can be triggered by
%% %% by weird but legal code.
%% case ordsets:subtract(ProvidedPaths, RequiredPaths) of
%%     [_|_]=GarbagePaths -> throw({garbage_phi_paths, GarbagePaths, I});
%%     [] -> ok
%% end.

%% Checks whether all variables used in this phi node are defined in the branch
%% they arrived on.
-spec vvars_assert_phi_vars(Phis, I, Id, State) -> ok when
      Phis :: list({beam_ssa:argument(), beam_ssa:label()}),
      Id :: beam_ssa:label(),
      I :: beam_ssa:b_set(),
      State :: #vvars{}.
vvars_assert_phi_vars(Phis, I, Id, #vvars{blocks=Blocks,branches=Branches}) ->
    PhiVars = [{Var, From} || {#b_var{}=Var, From} <- Phis],
    foreach(fun({Var, From}) ->
                    case Branches of
                        #{ {From, Id} := {Vars, _TryTags} } ->
                            case gb_sets:is_member(Var, Vars) of
                                true -> ok;
                                false -> throw({unknown_variable, Var, I})
                            end;
                        #{} ->
                            throw({unknown_phi_variable, Var, {From, Id}, I})
                    end
            end, PhiVars),
    Labels = [From || {#b_literal{},From} <- Phis],
    foreach(fun(Label) ->
                    case Blocks of
                        #{Label:=_} ->
                            ok;
                        #{} ->
                            throw({undefined_label_in_phi, Label, I})
                    end
            end, Labels).

-spec vvars_block(Id, State) -> #vvars{} when
      Id :: beam_ssa:label(),
      State :: #vvars{}.
vvars_block(Id, State0) ->
    #{ Id := #b_blk{ is = Is, last = Terminator} } = State0#vvars.blocks,
    #{ Id := {Vars, TryTags} } = State0#vvars.branches,
    validate_normalized(Terminator),
    State = State0#vvars{ variables = Vars, try_tags = TryTags },
    vvars_terminator(Terminator, Id, vvars_block_1(Is, Terminator, State)).

validate_normalized(I) ->
    case beam_ssa:normalize(I) of
        I ->
            ok;
        _ ->
            %% Some denormalized forms of #b_br{} can confuse
            %% beam_ssa_pre_codegen and/or beam_ssa_codegen and cause
            %% the compiler to crash. Here is an example of a denormalized
            %% br that may cause the compiler to crash:
            %%
            %%     br bool, ^99, ^99
            throw({not_normalized, I})
    end.

-spec vvars_block_1(Is, Terminator, State) -> #vvars{} when
      Is :: list(#b_set{}),
      Terminator :: beam_ssa:terminator(),
      State :: #vvars{}.
vvars_block_1([#b_set{dst=OpVar,args=OpArgs}=I,
               #b_set{op={succeeded,Kind},args=[OpVar],dst=SuccVar}],
              Terminator, State) ->
    true = Kind =:= guard orelse Kind =:= body, %Assertion.
    case Terminator of
        #b_br{bool=#b_var{}} ->
            ok = vvars_assert_args(OpArgs, I, State),
            vvars_save_var(SuccVar, vvars_save_var(OpVar, State));
        _ when Kind =:= body ->
            ok = vvars_assert_args(OpArgs, I, State),
            vvars_save_var(SuccVar, vvars_save_var(OpVar, State));
        _ ->
            %% A succeeded:guard instruction must be followed by a
            %% two-way branch; otherwise beam_ssa_codegen will crash.
            throw({succeeded_not_followed_by_two_way_br, I})
    end;
vvars_block_1([#b_set{op={succeeded,guard},args=Args}=I | [_|_]],
              _Terminator, State) ->
    ok = vvars_assert_args(Args, I, State),
    %% 'succeeded' must be the last instruction in its block.
    throw({succeeded_not_last, I});
vvars_block_1([#b_set{op={succeeded,_},args=Args}=I], _Terminator, State) ->
    ok = vvars_assert_args(Args, I, State),
    %% 'succeeded' must be directly preceded by the operation it checks.
    throw({succeeded_not_preceded, I});
vvars_block_1([#b_set{ dst = Dst, op = phi } | Is], Terminator, State) ->
    %% We don't check phi node arguments at this point since we may not have
    %% visited their definition yet. They'll be handled later on in
    %% vvars_phi_nodes/1 after all blocks are processed.
    vvars_block_1(Is, Terminator, vvars_save_var(Dst, State));
vvars_block_1([#b_set{ op = new_try_tag, dst = Dst, args = Args }=I | Is],
              Terminator, State) ->
    ok = vvars_assert_args(Args, I, State),
    vvars_block_1(Is, Terminator, vvars_save_try_tag(Dst, State));
vvars_block_1([#b_set{ op = kill_try_tag,
                       dst = Dst,
                       args = [Tag] }=I | Is],
              Terminator, State0) ->
    ok = vvars_assert_args([Tag], I, State0),
    State = vvars_kill_try_tag(Tag, State0),
    vvars_block_1(Is, Terminator, vvars_save_var(Dst, State));
vvars_block_1([#b_set{ op = catch_end,
                       dst = Dst,
                       args = [Tag, Kind] }=I | Is],
              Terminator, State0) ->
    ok = vvars_assert_args([Kind], I, State0),
    State = vvars_kill_try_tag(Tag, State0),
    vvars_block_1(Is, Terminator, vvars_save_var(Dst, State));
vvars_block_1([#b_set{ dst = Dst, args = Args }=I | Is], Terminator, State) ->
    ok = vvars_assert_args(Args, I, State),
    vvars_block_1(Is, Terminator, vvars_save_var(Dst, State));
vvars_block_1([], _Terminator, State) ->
    State.

-spec vvars_terminator(Terminator, From, State) -> #vvars{} when
      Terminator :: beam_ssa:terminator(),
      From :: beam_ssa:label(),
      State :: #vvars{}.
vvars_terminator(#b_ret{arg=Arg,anno=Anno}=I, From, State) ->
    check_anno(Anno),
    ok = vvars_assert_args([Arg], I, State),
    TryTags = State#vvars.try_tags,
    case {gb_sets:is_empty(TryTags),From} of
        {false,?EXCEPTION_BLOCK} ->
            %% Plain guards sometimes branch off to ?EXCEPTION_BLOCK even
            %% though they cannot actually throw exceptions. This ought to be
            %% fixed at the source, but we'll ignore this for now.
            State;
        {false,_} ->
            throw({active_try_tags_on_return, TryTags, I});
        {true,_} ->
            State
    end;
vvars_terminator(#b_switch{arg=Arg,fail=Fail,list=Switch,anno=Anno}=I, From, State) ->
    check_anno(Anno),
    ok = vvars_assert_args([Arg], I, State),
    ok = vvars_assert_args([A || {A,_Lbl} <:- Switch], I, State),
    Labels = [Fail | [Lbl || {_Arg, Lbl} <:- Switch]],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State);
vvars_terminator(#b_br{bool=#b_literal{val=true},succ=Succ,anno=Anno}=I, From, State) ->
    check_anno(Anno),
    Labels = [Succ],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State);
vvars_terminator(#b_br{bool=Arg,succ=Succ,fail=Fail,anno=Anno}=I, From, State) ->
    check_anno(Anno),
    ok = vvars_assert_args([Arg], I, State),
    Labels = [Fail, Succ],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State).

-spec vvars_terminator_1(Labels, From, State) -> #vvars{} when
      Labels :: list(beam_ssa:label()),
      From :: beam_ssa:label(),
      State :: #vvars{}.
vvars_terminator_1(Labels0, From, #vvars{branches=Branches}=State0) ->
    %% Filter out all branches that have already been taken. This should result
    %% in either all of Labels0 or an empty list.
    Labels = [To || To <- Labels0,
                    not maps:is_key({From, To}, Branches)],
    true = Labels =:= Labels0 orelse Labels =:= [], %Assertion
    State1 = foldl(fun(To, State) ->
                           vvars_save_branch(From, To, State)
                   end, State0, Labels),
    foldl(fun(To, State) ->
                  vvars_block(To, State)
          end, State1, Labels).

%% Gets all variable names in args, ignoring literals etc
-spec vvars_get_variables(Args) -> list(beam_ssa:b_var()) when
      Args :: list(beam_ssa:argument()).
vvars_get_variables(Args) ->
    [Var || #b_var{}=Var <- Args].

%% Checks that all variables in Args are defined in all paths leading to the
%% current State.
-spec vvars_assert_args(Args, I, State) -> ok when
      Args :: list(beam_ssa:argument()),
      I :: beam_ssa:terminator() | beam_ssa:b_set(),
      State :: #vvars{}.
vvars_assert_args(Args, I, #vvars{variables=Vars}=State) ->
    foreach(fun(#b_remote{mod=Mod,name=Name}) ->
                    vvars_assert_args([Mod,Name], I, State);
               (#b_var{}=Var) ->
                    case gb_sets:is_member(Var, Vars) of
                        true -> ok;
                        false -> throw({unknown_variable,Var,I})
                    end;
               (_) -> ok
            end, Args).

%% Checks that all given labels are defined in State.
-spec vvars_assert_labels(Labels, I, State) -> ok when
      Labels :: list(beam_ssa:label()),
      I :: beam_ssa:terminator(),
      State :: #vvars{}.
vvars_assert_labels(Labels, I, #vvars{blocks=Blocks}) ->
    foreach(fun(Label) ->
                    case maps:is_key(Label, Blocks) of
                        false -> throw({unknown_block, Label, I});
                        true -> ok
                    end
            end, Labels).

-spec vvars_save_branch(From, To, State) -> #vvars{} when
      From :: beam_ssa:label(),
      To :: beam_ssa:label(),
      State :: #vvars{}.
vvars_save_branch(From, To, State) ->
    Vars = State#vvars.variables,
    TryTags = State#vvars.try_tags,
    Branches0 = State#vvars.branches,
    case Branches0 of
        #{ To := {LblVars, LblTryTags} } ->
            MergedVars = vvars_merge_variables(Vars, LblVars),
            MergedTags = vvars_merge_try_tags(TryTags, LblTryTags),

            Merged = {MergedVars, MergedTags},
            Branch = {Vars, TryTags},

            Branches = Branches0#{ To => Merged, {From, To} => Branch },
            State#vvars { branches = Branches };
        _ ->
            Branch = {Vars, TryTags},
            Branches = Branches0#{ To => Branch, {From, To} => Branch },
            State#vvars { branches = Branches }
    end.

-spec vvars_merge_variables(New, Existing) -> variables() when
      New :: variables(),
      Existing :: variables().
vvars_merge_variables(New, Existing) ->
    gb_sets:intersection(New, Existing).

-spec vvars_merge_try_tags(New, Existing) -> try_tags() when
      New :: try_tags(),
      Existing :: try_tags().
vvars_merge_try_tags(New, Existing) ->
    gb_sets:union(New, Existing).

-spec vvars_save_var(Var, State) -> #vvars{} when
      Var :: #b_var{},
      State :: #vvars{}.
vvars_save_var(Var, State0) ->
    %% vvars_assert_unique guarantees that variables are never set twice.
    Vars = gb_sets:insert(Var, State0#vvars.variables),
    State0#vvars{ variables = Vars }.

-spec vvars_save_try_tag(Var, State) -> #vvars{} when
      Var :: #b_var{},
      State :: #vvars{}.
vvars_save_try_tag(Var, State0) ->
    Vars = gb_sets:insert(Var, State0#vvars.variables),
    TryTags = gb_sets:insert(Var, State0#vvars.try_tags),
    State0#vvars{ variables = Vars, try_tags = TryTags }.

-spec vvars_kill_try_tag(Var, State) -> #vvars{} when
      Var :: #b_var{},
      State :: #vvars{}.
vvars_kill_try_tag(Var, State0) ->
    TryTags = gb_sets:delete(Var, State0#vvars.try_tags),
    State0#vvars{ try_tags = TryTags }.

check_anno(#{}) -> ok;
check_anno(BadAnno) -> throw({bad_annotation,BadAnno}).

format_error_1({redefined_variable, Name, Old, I}) ->
    io_lib:format("Variable ~ts (~ts) redefined by ~ts",
                  [format_var(Name), format_instr(Old), format_instr(I)]);
format_error_1({missing_phi_paths, Paths, I}) ->
    io_lib:format("Phi node ~ts doesn't define a value for these "
                  "branches: ~w",
                  [format_instr(I), Paths]);
format_error_1({garbage_phi_paths, Paths, I}) ->
    io_lib:format("Phi node ~ts defines a value for these unreachable "
                  "or non-existent branches: ~w",
                  [format_instr(I), Paths]);
format_error_1({unknown_phi_variable, Name, {From, _To}, I}) ->
    io_lib:format("Variable ~ts used in phi node ~ts is undefined on "
                  "branch ~w",
                  [format_var(Name), format_instr(I), From]);
format_error_1({unknown_block, Label, I}) ->
    io_lib:format("Unknown block ~p referenced in ~ts",
                  [Label, I]);
format_error_1({unknown_variable, Name, I}) ->
    io_lib:format("Unbound variable ~ts used in ~ts",
                  [format_var(Name), format_instr(I)]);
format_error_1({phi_inside_block, Name, Id}) ->
    io_lib:format("Phi node defining ~ts is not at start of block ~p",
                  [format_var(Name), Id]);
format_error_1({undefined_label_in_phi, Label, I}) ->
    io_lib:format("Unknown block label ~p in phi node ~ts",
                  [Label, format_instr(I)]);
format_error_1({succeeded_not_preceded, I}) ->
    io_lib:format("~ts does not reference the preceding instruction",
                  [format_instr(I)]);
format_error_1({succeeded_not_last, I}) ->
    io_lib:format("~ts is not the last instruction in its block",
                  [format_instr(I)]);
format_error_1({not_normalized, I}) ->
    io_lib:format("~ts is not normalized by beam_ssa:normalize/1",
                  [format_instr(I)]);
format_error_1({succeeded_not_followed_by_two_way_br, I}) ->
    io_lib:format("~ts not followed by a two-way branch",
                  [format_instr(I)]);
format_error_1({active_try_tags_on_return, TryTags0, I}) ->
    TryTags = format_vars(gb_sets:to_list(TryTags0)),
    io_lib:format("Try tags ~ts are still active on ~ts",
                  [TryTags, format_instr(I)]);
format_error_1({bad_annotation, BadAnno}) ->
    io_lib:format("Badly formed annotation: ~p", [BadAnno]).
