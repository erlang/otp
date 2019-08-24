%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

-export([module/2, format_error/1]).

-import(lists, [append/1, foldl/3, foreach/2]).

-include("beam_ssa.hrl").

-spec module(#b_module{}, [compile:option()]) ->
                    {'ok',#b_module{}} | {'error',list()}.
module(#b_module{body=Fs,name=Name}=Mod0, _Options) ->
    Es0 = append([validate_function(F) || F <- Fs]),
    case [{?MODULE,E} || E <- Es0] of
        [] ->
            {ok, Mod0};
        [_|_]=Es ->
            {error,[{atom_to_list(Name), Es}]}
    end.

-spec format_error(term()) -> iolist().
format_error({{_M,F,A},{redefined_variable, Name, Old, I}}) ->
    io_lib:format("~p/~p: Variable ~ts (~ts) redefined by ~ts",
                  [F, A, format_var(Name), format_instr(Old), format_instr(I)]);
format_error({{_M,F,A},{missing_phi_paths, Paths, I}}) ->
    io_lib:format("~p/~p: Phi node ~ts doesn't define a value for these "
                  "branches: ~w",
                  [F, A, format_instr(I), Paths]);
format_error({{_M,F,A},{garbage_phi_paths, Paths, I}}) ->
    io_lib:format("~p/~p: Phi node ~ts defines a value for these unreachable "
                  "or non-existent branches: ~w",
                  [F, A, format_instr(I), Paths]);
format_error({{_M,F,A},{unknown_phi_variable, Name, {From, _To}, I}}) ->
    io_lib:format("~p/~p: Variable ~ts used in phi node ~ts is undefined on "
                  "branch ~w",
                  [F, A, format_var(Name), format_instr(I), From]);
format_error({{_M,F,A},{unknown_block, Label, I}}) ->
    io_lib:format("~p/~p: Unknown block ~p referenced in ~ts",
                  [F, A, Label, I]);
format_error({{_M,F,A},{unknown_variable, Name, I}}) ->
    io_lib:format("~p/~p: Unbound variable ~ts used in ~ts",
                  [F, A, format_var(Name), format_instr(I)]);
format_error({{_M,F,A},{phi_inside_block, Name, Id}}) ->
    io_lib:format("~p/~p: Phi node defining ~ts is not at start of block ~p",
                  [F, A, format_var(Name), Id]);
format_error({{_M,F,A},{undefined_label_in_phi, Label, I}}) ->
    io_lib:format("~p/~p: Unknown block label ~p in phi node ~ts",
                  [F, A, Label, format_instr(I)]).

format_instr(I) ->
    [$',beam_ssa_pp:format_instr(I),$'].

format_var(V) ->
    beam_ssa_pp:format_var(#b_var{name=V}).

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

-type defined_vars() :: gb_sets:set(beam_ssa:var_name()).

-record(vvars,
        {blocks :: #{ beam_ssa:label() => beam_ssa:b_blk() },
         branch_def_vars :: #{
           %% Describes the variable state at the time of this exact branch (phi
           %% node validation).
           {From :: beam_ssa:label(), To :: beam_ssa:label()} => defined_vars(),
           %% Describes the variable state common to all branches leading to this
           %% label (un/redefined variable validation).
           beam_ssa:label() => defined_vars() },
         defined_vars :: defined_vars()}).

-spec validate_variables(beam_ssa:b_function()) -> ok.
validate_variables(#b_function{ args = Args, bs = Blocks }) ->
    %% Prefill the mapping with function arguments.
    ArgNames = vvars_get_varnames(Args),
    DefVars = gb_sets:from_list(ArgNames),
    Entry = 0,

    State = #vvars{blocks = Blocks,
                   branch_def_vars = #{ Entry => DefVars },
                   defined_vars = DefVars},
    ok = vvars_assert_unique(Blocks, ArgNames),
    vvars_phi_nodes(vvars_block(Entry, State)).

%% Checks the uniqueness of all variables across all blocks.
-spec vvars_assert_unique(Blocks, [beam_ssa:var_name()]) -> ok when
      Blocks :: #{ beam_ssa:label() => beam_ssa:b_blk() }.
vvars_assert_unique(Blocks, Args) ->
    BlockIs = [Is || #b_blk{is=Is} <- maps:values(Blocks)],
    Defined0 = maps:from_list([{V,argument} || V <- Args]),
    _ = foldl(fun(Is, Defined) ->
                      vvars_assert_unique_1(Is, Defined)
              end, Defined0, BlockIs),
    ok.

-spec vvars_assert_unique_1(Is, Defined) -> ok when
    Is :: list(beam_ssa:b_set()),
    Defined :: #{ beam_ssa:var_name() => beam_ssa:b_set() }.
vvars_assert_unique_1([#b_set{dst=#b_var{name=DstName}}=I|Is], Defined) ->
    case Defined of
        #{DstName:=Old} -> throw({redefined_variable, DstName, Old, I});
        _ -> vvars_assert_unique_1(Is, Defined#{DstName=>I})
    end;
vvars_assert_unique_1([], Defined) ->
    Defined.

-spec vvars_phi_nodes(State :: #vvars{}) -> ok.
vvars_phi_nodes(#vvars{ blocks = Blocks }=State) ->
    _ = [vvars_phi_nodes_1(Is, Id, State) ||
            {Id, #b_blk{ is = Is }} <- maps:to_list(Blocks)],
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
    case [Dst || #b_set{op=phi,dst=#b_var{name=Dst}} <- Is] of
        [Name|_] ->
            throw({phi_inside_block, Name, Id});
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
    BranchKeys = maps:keys(State#vvars.branch_def_vars),
    RequiredPaths = ordsets:from_list([From || {From, To} <- BranchKeys, To =:= Id]),
    ProvidedPaths = ordsets:from_list([From || {_Value, From} <- Phis]),
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
vvars_assert_phi_vars(Phis, I, Id, #vvars{blocks=Blocks,
                                          branch_def_vars=BranchDefVars}) ->
    Vars = [{Var, From} || {#b_var{}=Var, From} <- Phis],
    foreach(fun({#b_var{name=VarName}, From}) ->
                    BranchKey = {From, Id},
                    case BranchDefVars of
                        #{BranchKey:=DefVars} ->
                            case gb_sets:is_member(VarName, DefVars) of
                                true -> ok;
                                false -> throw({unknown_variable, VarName, I})
                            end;
                        #{} ->
                            throw({unknown_phi_variable, VarName, BranchKey, I})
                    end
            end, Vars),
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
    #{ Id := DefVars } = State0#vvars.branch_def_vars,
    State = State0#vvars{ defined_vars = DefVars },
    vvars_terminator(Terminator, Id, vvars_block_1(Is, State)).

-spec vvars_block_1(Blocks, State) -> #vvars{} when
    Blocks :: list(beam_ssa:b_blk()),
    State :: #vvars{}.
vvars_block_1([], State) ->
    State;
vvars_block_1([#b_set{ dst = #b_var{ name = DstName }, op = phi } | Is], State0) ->
    %% We don't check phi node arguments at this point since we may not have
    %% visited their definition yet. They'll be handled later on in
    %% vvars_phi_nodes/1 after all blocks are processed.
    vvars_block_1(Is, vvars_save_var(DstName, State0));
vvars_block_1([#b_set{ dst = #b_var{ name = DstName }, args = Args }=I | Is], State0) ->
    ok = vvars_assert_args(Args, I, State0),
    vvars_block_1(Is, vvars_save_var(DstName, State0)).

-spec vvars_terminator(Terminator, From, State) -> #vvars{} when
    Terminator :: beam_ssa:terminator(),
    From :: beam_ssa:label(),
    State :: #vvars{}.
vvars_terminator(#b_ret{ arg = Arg }=I, _From, State) ->
    ok = vvars_assert_args([Arg], I, State),
    State;
vvars_terminator(#b_switch{arg=Arg,fail=Fail,list=Switch}=I, From, State) ->
    ok = vvars_assert_args([Arg], I, State),
    ok = vvars_assert_args([A || {A,_Lbl} <- Switch], I, State),
    Labels = [Fail | [Lbl || {_Arg, Lbl} <- Switch]],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State);
vvars_terminator(#b_br{bool=#b_literal{val=true},succ=Succ}=I, From, State) ->
    Labels = [Succ],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State);
vvars_terminator(#b_br{bool=#b_literal{val=false},fail=Fail}=I, From, State) ->
    Labels = [Fail],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State);
vvars_terminator(#b_br{ bool = Arg, succ = Succ, fail = Fail }=I, From, State) ->
    ok = vvars_assert_args([Arg], I, State),
    Labels = [Fail, Succ],
    ok = vvars_assert_labels(Labels, I, State),
    vvars_terminator_1(Labels, From, State).

-spec vvars_terminator_1(Labels, From, State) -> #vvars{} when
    Labels :: list(beam_ssa:label()),
    From :: beam_ssa:label(),
    State :: #vvars{}.
vvars_terminator_1(Labels0, From, State0) ->
    %% Filter out all branches that have already been taken. This should result
    %% in either all of Labels0 or an empty list.
    Labels = [To || To <- Labels0,
              not maps:is_key({From, To}, State0#vvars.branch_def_vars)],
    true = Labels =:= Labels0 orelse Labels =:= [], %Assertion
    State1 = foldl(fun(To, State) ->
                       vvars_save_branch(From, To, State)
                   end, State0, Labels),
    foldl(fun(To, State) ->
              vvars_block(To, State)
          end, State1, Labels).

%% Gets all variable names in args, ignoring literals etc
-spec vvars_get_varnames(Args) -> list(beam_ssa:var_name()) when
    Args :: list(beam_ssa:argument()).
vvars_get_varnames(Args) ->
    [Name || #b_var{ name = Name } <- Args].

%% Checks that all variables in Args are defined in all paths leading to the
%% current State.
-spec vvars_assert_args(Args, I, State) -> ok when
    Args :: list(beam_ssa:argument()),
    I :: beam_ssa:terminator() | beam_ssa:b_set(),
    State :: #vvars{}.
vvars_assert_args(Args, I, #vvars{defined_vars=DefVars}=State) ->
    foreach(fun(#b_remote{mod=Mod,name=Name}) ->
                    vvars_assert_args([Mod,Name], I, State);
               (#b_var{name=Name}) ->
                    case gb_sets:is_member(Name, DefVars) of
                        true -> ok;
                        false -> throw({unknown_variable,Name,I})
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
    DefVars = State#vvars.defined_vars,
    Branches0 = State#vvars.branch_def_vars,
    case Branches0 of
        #{ To := LblDefVars } ->
            MergedVars = vvars_merge_branches(DefVars, LblDefVars),

            Branches = Branches0#{ To => MergedVars, {From, To} => DefVars },
            State#vvars { branch_def_vars = Branches };
        _ ->
            Branches = Branches0#{ To => DefVars, {From, To} => DefVars },
            State#vvars { branch_def_vars = Branches }
    end.

-spec vvars_merge_branches(New, Existing) -> defined_vars() when
    New :: defined_vars(),
    Existing :: defined_vars().
vvars_merge_branches(New, Existing) ->
    gb_sets:intersection(New, Existing).

-spec vvars_save_var(VarName, State) -> #vvars{} when
    VarName :: beam_ssa:var_name(),
    State :: #vvars{}.
vvars_save_var(VarName, State0) ->
    %% vvars_assert_unique guarantees that variables are never set twice.
    DefVars = gb_sets:insert(VarName, State0#vvars.defined_vars),
    State0#vvars{ defined_vars = DefVars }.
