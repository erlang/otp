%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2021. All Rights Reserved.
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
%% This pass infers types from expressions and attempts to simplify or remove
%% subsequent instructions based on that information.
%%
%% This is divided into two subpasses; the first figures out function type
%% signatures for the whole module without optimizing anything, and the second
%% optimizes based on that information, further refining the type signatures as
%% it goes.
%%

-module(beam_ssa_type).
-export([opt_start/2, opt_continue/4, opt_finish/3]).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

-import(lists, [any/2,duplicate/2,foldl/3,member/2,
                keyfind/3,reverse/1,split/2,zip/2]).

%% The maximum number of #b_ret{} terminators a function can have before
%% collapsing success types into a single entry. Consider the following code:
%%
%%    f(0) -> 1;
%%    f(...) -> ...;
%%    f(500000) -> 500000.
%%
%% Since success types are grouped by return type and each clause returns a
%% distinct type (singleton #t_integer{}s), we'll add 500000 entries which
%% makes progress glacial since every call needs to examine them all to
%% determine the return type.
%%
%% The entries are collapsed unconditionally if the number of returns in a
%% function exceeds this threshold. This is necessary because collapsing as we
%% go might widen a type; if we're at (?RETURN_LIMIT - 1) entries and suddenly
%% narrow a type down, it could push us over the edge and collapse all entries,
%% possibly widening the return type and breaking optimizations that were based
%% on the earlier (narrower) types.
-define(RETURN_LIMIT, 30).

%% Constants common to all subpasses.
-record(metadata,
        { func_id :: func_id(),
          limit_return :: boolean(),
          params :: [beam_ssa:b_var()],
          used_once :: #{ beam_ssa:b_var() => _ } }).

-type metadata() :: #metadata{}.
-type meta_cache() :: #{ func_id() => metadata() }.
-type type_db() :: #{ beam_ssa:var_name() := ssa_type() }.

%% The types are the same as in 'beam_types.hrl', with the addition of
%% `(fun(type_db()) -> type())` that defers figuring out the type until it's
%% actually used. Mainly used to coax more type information out of
%% `get_tuple_element` where a test on one element (e.g. record tag) may
%% affect the type of another.
-type ssa_type() :: fun((type_db()) -> type()) | type().

%%

-spec opt_start(term(), term()) -> term().
opt_start(StMap, FuncDb0) when FuncDb0 =/= #{} ->
    {ArgDb, MetaCache, FuncDb} = signatures(StMap, FuncDb0),

    opt_start_1(maps:keys(StMap), ArgDb, StMap, FuncDb, MetaCache);
opt_start(StMap, FuncDb) ->
    %% Module-level analysis is disabled, likely because of a call to
    %% load_nif/2 or similar. opt_continue/4 will assume that all arguments and
    %% return types are 'any'.
    {StMap, FuncDb}.

opt_start_1([Id | Ids], ArgDb, StMap0, FuncDb0, MetaCache) ->
    case ArgDb of
        #{ Id := ArgTypes } ->
            #opt_st{ssa=Linear0,args=Args} = St0 = map_get(Id, StMap0),

            Ts = maps:from_list(zip(Args, ArgTypes)),
            {Linear, FuncDb} = opt_function(Linear0, Args, Id, Ts, FuncDb0, MetaCache),

            St = St0#opt_st{ssa=Linear},
            StMap = StMap0#{ Id := St },

            opt_start_1(Ids, ArgDb, StMap, FuncDb, MetaCache);
        #{} ->
            %% Unreachable functions must be removed so that opt_continue/4
            %% won't process them and potentially taint the argument types of
            %% other functions.
            StMap = maps:remove(Id, StMap0),
            FuncDb = maps:remove(Id, FuncDb0),

            opt_start_1(Ids, ArgDb, StMap, FuncDb, MetaCache)
    end;
opt_start_1([], _CommittedArgs, StMap, FuncDb, _MetaCache) ->
    {StMap, FuncDb}.

%%
%% The initial signature analysis is based on the paper "Practical Type
%% Inference Based on Success Typings" [1] by `Tobias Lindahl` and
%% `Konstantinos Sagonas`, mainly section 6.1 and onwards.
%%
%% The general idea is to start out at the module's entry points and propagate
%% types to the functions we call. The argument types of all exported functions
%% start out a 'any', whereas local functions start at 'none'. Every time a
%% function call widens the argument types, we analyze the callee again and
%% propagate its return types to the callers, analyzing them again, and
%% continuing this process until all arguments and return types have been
%% widened as far as they can be.
%%
%% Note that we do not "jump-start the analysis" by first determining success
%% types as in the paper because we need to know all possible inputs including
%% those that will not return.
%%
%% [1] http://www.it.uu.se/research/group/hipe/papers/succ_types.pdf
%%

-record(sig_st,
        { wl = wl_new() :: worklist(),
          committed = #{} :: #{ func_id() => [type()] },
          updates = #{} :: #{ func_id() => [type()] },
          meta_cache = #{} :: meta_cache()}).

signatures(StMap, FuncDb0) ->
    State0 = init_sig_st(StMap, FuncDb0),
    {State, FuncDb} = signatures_1(StMap, FuncDb0, State0),
    {State#sig_st.committed, State#sig_st.meta_cache, FuncDb}.

signatures_1(StMap, FuncDb0, State0) ->
    case wl_next(State0#sig_st.wl) of
        {ok, FuncId} ->
            {State, FuncDb} = sig_function(FuncId, StMap, State0, FuncDb0),
            signatures_1(StMap, FuncDb, State);
        empty ->
            %% No more work to do, assert that we don't have any outstanding
            %% updates.
            #sig_st{updates=Same,committed=Same} = State0, %Assertion.

            {State0, FuncDb0}
    end.

sig_function(Id, StMap, State, FuncDb) ->
    try
        do_sig_function(Id, StMap, State, FuncDb)
    catch
        Class:Error:Stack ->
            #b_local{name=#b_literal{val=Name},arity=Arity} = Id,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

do_sig_function(Id, StMap, State0, FuncDb0) ->
    case sig_function_1(Id, StMap, State0, FuncDb0) of
        {false, false, State, FuncDb} ->
            %% No added work and the types are identical. Pop ourselves from
            %% the work list and move on to the next function.
            Wl = wl_pop(Id, State#sig_st.wl),
            {State#sig_st{wl=Wl}, FuncDb};
        {false, true, State, FuncDb} ->
            %% We've added some work and our return type is unchanged. Keep
            %% following the work list without popping ourselves; we're very
            %% likely to need to return here later and can avoid a lot of
            %% redundant work by keeping our place in line.
            {State, FuncDb};
        {true, WlChanged, State, FuncDb} ->
            %% Our return type has changed so all of our (previously analyzed)
            %% callers need to be analyzed again.
            %%
            %% If our worklist is unchanged we'll pop ourselves since our
            %% callers will add us back if we need to analyzed again, and
            %% it's wasteful to stay in the worklist when we don't.
            Wl0 = case WlChanged of
                      true -> State#sig_st.wl;
                      false -> wl_pop(Id, State#sig_st.wl)
                  end,

            #func_info{in=Cs0} = map_get(Id, FuncDb0),
            Updates = State#sig_st.updates,
            Callers = [C || C <- Cs0, is_map_key(C, Updates)],
            Wl = wl_defer_list(Callers, Wl0),

            {State#sig_st{wl=Wl}, FuncDb}
    end.

sig_function_1(Id, StMap, State0, FuncDb) ->
    #opt_st{ssa=Linear,args=Args} = map_get(Id, StMap),

    {ArgTypes, State1} = sig_commit_args(Id, State0),
    Ts = maps:from_list(zip(Args, ArgTypes)),

    FakeCall = #b_set{op=call,args=[#b_remote{mod=#b_literal{val=unknown},
                                              name=#b_literal{val=unknown},
                                              arity=0}]},

    Ds = maps:from_list([{Var, FakeCall#b_set{dst=Var}} ||
                            #b_var{}=Var <- Args]),

    Ls = #{ ?EXCEPTION_BLOCK => {incoming, Ts},
            0 => {incoming, Ts} },

    {Meta, State2} = sig_init_metadata(Id, Linear, Args, State1),

    Wl0 = State1#sig_st.wl,

    {State, SuccTypes} = sig_bs(Linear, Ds, Ls, FuncDb, #{}, [], Meta, State2),

    WlChanged = wl_changed(Wl0, State#sig_st.wl),
    #{ Id := #func_info{succ_types=SuccTypes0}=Entry0 } = FuncDb,

    if
        SuccTypes0 =:= SuccTypes ->
            {false, WlChanged, State, FuncDb};
        SuccTypes0 =/= SuccTypes ->
            Entry = Entry0#func_info{succ_types=SuccTypes},
            {true, WlChanged, State, FuncDb#{ Id := Entry }}
    end.

%% Get the metadata for a function. If this function has been analysed
%% previously, retrieve the previously calculated metadata.
sig_init_metadata(Id, Linear, Args, #sig_st{meta_cache=MetaCache} = State) ->
    case MetaCache of
        #{Id := Meta} ->
            {Meta, State};
        #{} ->
            Meta = init_metadata(Id, Linear, Args),
            {Meta, State#sig_st{meta_cache=MetaCache#{Id => Meta}}}
    end.

sig_bs([{L, #b_blk{is=Is,last=Last0}} | Bs],
       Ds0, Ls0, Fdb, Sub0, SuccTypes0, Meta, State0) ->
    case Ls0 of
        #{ L := Incoming } ->
            {incoming, Ts0} = Incoming,         %Assertion.

            {Ts, Ds, Sub, State} =
                sig_is(Is, Ts0, Ds0, Ls0, Fdb, Sub0, State0),

            Last = simplify_terminator(Last0, Ts, Ds, Sub),
            SuccTypes = update_success_types(Last, Ts, Ds, Meta, SuccTypes0),

            UsedOnce = Meta#metadata.used_once,
            {_, Ls1} = update_successors(Last, Ts, Ds, Ls0, UsedOnce),

            %% In the future there may be a point to storing outgoing types on
            %% a per-edge basis as it would give us more precision in phi
            %% nodes, but there's nothing to gain from that at the moment so
            %% we'll store the current Ts to save memory.
            Ls = Ls1#{ L := {outgoing, Ts} },
            sig_bs(Bs, Ds, Ls, Fdb, Sub, SuccTypes, Meta, State);
        #{} ->
            %% This block is never reached. Ignore it.
            sig_bs(Bs, Ds0, Ls0, Fdb, Sub0, SuccTypes0, Meta, State0)
    end;
sig_bs([], _Ds, _Ls, _Fdb, _Sub, SuccTypes, _Meta, State) ->
    {State, SuccTypes}.

sig_is([#b_set{op=call,
               args=[#b_local{}=Callee | _]=Args0,
               dst=Dst}=I0 | Is],
       Ts0, Ds0, Ls, Fdb, Sub, State0) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = I0#b_set{args=Args},

    [_ | CallArgs] = Args,
    {I, State} = sig_local_call(I1, Callee, CallArgs, Ts0, Fdb, State0),

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    sig_is(Is, Ts, Ds, Ls, Fdb, Sub, State);
sig_is([#b_set{op=call,
               args=[#b_var{} | _]=Args0,
               dst=Dst}=I0 | Is],
       Ts0, Ds0, Ls, Fdb, Sub, State) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = I0#b_set{args=Args},

    [Fun | _] = Args,
    I = case normalized_type(Fun, Ts0) of
            #t_fun{type=Type} -> beam_ssa:add_anno(result_type, Type, I1);
            _ -> I1
        end,

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    sig_is(Is, Ts, Ds, Ls, Fdb, Sub, State);
sig_is([#b_set{op=MakeFun,args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Ls, Fdb, Sub0, State0) when MakeFun =:= make_fun;
                                             MakeFun =:= old_make_fun ->
    Args = simplify_args(Args0, Ts0, Sub0),
    I1 = I0#b_set{args=Args},

    {I, State} = sig_make_fun(I1, Ts0, Fdb, State0),

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    sig_is(Is, Ts, Ds, Ls, Fdb, Sub0, State);
sig_is([I0 | Is], Ts0, Ds0, Ls, Fdb, Sub0, State) ->
    case simplify(I0, Ts0, Ds0, Ls, Sub0) of
        {#b_set{}, Ts, Ds} ->
            sig_is(Is, Ts, Ds, Ls, Fdb, Sub0, State);
        Sub when is_map(Sub) ->
            sig_is(Is, Ts0, Ds0, Ls, Fdb, Sub, State)
    end;
sig_is([], Ts, Ds, _Ls, _Fdb, Sub, State) ->
    {Ts, Ds, Sub, State}.

sig_local_call(I0, Callee, Args, Ts, Fdb, State) ->
    ArgTypes = argument_types(Args, Ts),
    I = sig_local_return(I0, Callee, ArgTypes, Fdb),
    {I, sig_update_args(Callee, ArgTypes, State)}.

%% While it's impossible to tell which arguments a fun will be called with
%% (someone could steal it through tracing and call it), we do know its free
%% variables and can update their types as if this were a local call.
sig_make_fun(#b_set{op=MakeFun,
                    args=[#b_local{}=Callee | FreeVars]}=I0,
             Ts, Fdb, State) when MakeFun =:= make_fun;
                                  MakeFun =:= old_make_fun ->
    ArgCount = Callee#b_local.arity - length(FreeVars),

    FVTypes = [concrete_type(FreeVar, Ts) || FreeVar <- FreeVars],
    ArgTypes = duplicate(ArgCount, any) ++ FVTypes,

    I = sig_local_return(I0, Callee, ArgTypes, Fdb),
    {I, sig_update_args(Callee, ArgTypes, State)}.

sig_local_return(I, Callee, ArgTypes, Fdb) ->
    #func_info{succ_types=SuccTypes} = map_get(Callee, Fdb),
    case return_type(SuccTypes, ArgTypes) of
        any -> I;
        Type -> beam_ssa:add_anno(result_type, Type, I)
    end.

init_sig_st(StMap, FuncDb) ->
    %% Start out as if all the roots have been called with 'any' for all
    %% arguments.
    Roots = init_sig_roots(FuncDb),
    #sig_st{ committed=#{},
             updates=init_sig_args(Roots, StMap, #{}),
             wl=wl_defer_list(Roots, wl_new()) }.

init_sig_roots(FuncDb) ->
    maps:fold(fun(Id, #func_info{exported=true}, Acc) ->
                      [Id | Acc];
                 (_, _, Acc) ->
                      Acc
              end, [], FuncDb).

init_sig_args([Root | Roots], StMap, Acc) ->
    #opt_st{args=Args0} = map_get(Root, StMap),
    ArgTypes = lists:duplicate(length(Args0), any),
    init_sig_args(Roots, StMap, Acc#{ Root => ArgTypes });
init_sig_args([], _StMap, Acc) ->
    Acc.

sig_commit_args(Id, #sig_st{updates=Us,committed=Committed0}=State0) ->
    Types = map_get(Id, Us),
    Committed = Committed0#{ Id => Types },
    State = State0#sig_st{committed=Committed},
    {Types, State}.

sig_update_args(Callee, Types, #sig_st{committed=Committed}=State) ->
    case Committed of
        #{ Callee := Current } ->
            case parallel_join(Current, Types) of
                Current ->
                    %% We've already processed this function with these
                    %% arguments, so there's no need to visit it again.
                    State;
                Widened ->
                    sig_update_args_1(Callee, Widened, State)
            end;
        #{} ->
            sig_update_args_1(Callee, Types, State)
    end.

sig_update_args_1(Callee, Types, #sig_st{updates=Us0,wl=Wl0}=State) ->
    Us = case Us0 of
             #{ Callee := Current } ->
                 Us0#{ Callee => parallel_join(Current, Types) };
             #{} ->
                 Us0#{ Callee => Types }
         end,
    State#sig_st{updates=Us,wl=wl_add(Callee, Wl0)}.

-spec opt_continue(Linear, Args, Anno, FuncDb) -> {Linear, FuncDb} when
      Linear :: [{non_neg_integer(), beam_ssa:b_blk()}],
      Args :: [beam_ssa:b_var()],
      Anno :: beam_ssa:anno(),
      FuncDb :: func_info_db().
opt_continue(Linear0, Args, Anno, FuncDb) when FuncDb =/= #{} ->
    Id = get_func_id(Anno),
    case FuncDb of
        #{ Id := #func_info{exported=false,arg_types=ArgTypes} } ->
            %% This is a local function and we're guaranteed to have visited
            %% every call site at least once, so we know that the parameter
            %% types are at least as narrow as the join of all argument types.
            Ts = join_arg_types(Args, ArgTypes, #{}),
            opt_function(Linear0, Args, Id, Ts, FuncDb);
        #{ Id := #func_info{exported=true} } ->
            %% We can't infer the parameter types of exported functions, but
            %% running the pass again could still help other functions.
            Ts = maps:from_list([{V,any} || #b_var{}=V <- Args]),
            opt_function(Linear0, Args, Id, Ts, FuncDb)
    end;
opt_continue(Linear0, Args, Anno, _FuncDb) ->
    %% Module-level optimization is disabled, pass an empty function database
    %% so we only perform local optimizations.
    Id = get_func_id(Anno),
    Ts = maps:from_list([{V,any} || #b_var{}=V <- Args]),
    {Linear, _} = opt_function(Linear0, Args, Id, Ts, #{}),
    {Linear, #{}}.

join_arg_types([Arg | Args], [TypeMap | TMs], Ts) ->
    Type = beam_types:join(maps:values(TypeMap)),
    join_arg_types(Args, TMs, Ts#{ Arg => Type });
join_arg_types([], [], Ts) ->
    Ts.

%%
%% Optimizes a function based on the type information inferred by signatures/2
%% and earlier runs of opt_function/5,6.
%%
%% This is pretty straightforward as it only walks through each function once,
%% and because it only makes types narrower it's safe to optimize the functions
%% in any order or not at all.
%%

opt_function(Linear, Args, Id, Ts, FuncDb) ->
    MetaCache = #{},
    opt_function(Linear, Args, Id, Ts, FuncDb, MetaCache).

-spec opt_function(Linear, Args, Id, Ts, FuncDb, MetaCache) -> Result when
      Linear :: [{non_neg_integer(), beam_ssa:b_blk()}],
      Args :: [beam_ssa:b_var()],
      Id :: func_id(),
      Ts :: type_db(),
      FuncDb :: func_info_db(),
      Result :: {Linear, FuncDb},
      MetaCache :: meta_cache().
opt_function(Linear, Args, Id, Ts, FuncDb, MetaCache) ->
    try
        do_opt_function(Linear, Args, Id, Ts, FuncDb, MetaCache)
    catch
        Class:Error:Stack ->
            #b_local{name=#b_literal{val=Name},arity=Arity} = Id,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

do_opt_function(Linear0, Args, Id, Ts, FuncDb0, MetaCache) ->
    FakeCall = #b_set{op=call,args=[#b_remote{mod=#b_literal{val=unknown},
                                              name=#b_literal{val=unknown},
                                              arity=0}]},

    Ds = maps:from_list([{Var, FakeCall#b_set{dst=Var}} ||
                            #b_var{}=Var <- Args]),

    Ls = #{ ?EXCEPTION_BLOCK => {incoming, Ts},
            0 => {incoming, Ts} },

    Meta = case MetaCache of
               #{Id := Meta0} ->
                   Meta0;
               #{} ->
                   init_metadata(Id, Linear0, Args)
           end,

    {Linear, FuncDb, SuccTypes} =
        opt_bs(Linear0, Ds, Ls, FuncDb0, #{}, [], Meta, []),

    case FuncDb of
        #{ Id := Entry0 } ->
            Entry = Entry0#func_info{succ_types=SuccTypes},
            {Linear, FuncDb#{ Id := Entry }};
        #{} ->
            %% Module-level optimizations have been turned off.
            {Linear, FuncDb}
    end.

get_func_id(Anno) ->
    #{func_info:={_Mod, Name, Arity}} = Anno,
    #b_local{name=#b_literal{val=Name}, arity=Arity}.

opt_bs([{L, #b_blk{is=Is0,last=Last0}=Blk0} | Bs],
       Ds0, Ls0, Fdb0, Sub0, SuccTypes0, Meta, Acc) ->
    case Ls0 of
        #{ L := Incoming } ->
            {incoming, Ts0} = Incoming,         %Assertion.

            {Is, Ts, Ds, Fdb, Sub} =
            opt_is(Is0, Ts0, Ds0, Ls0, Fdb0, Sub0, Meta, []),

            Last1 = simplify_terminator(Last0, Ts, Ds, Sub),
            SuccTypes = update_success_types(Last1, Ts, Ds, Meta, SuccTypes0),

            UsedOnce = Meta#metadata.used_once,
            {Last, Ls1} = update_successors(Last1, Ts, Ds, Ls0, UsedOnce),

            Ls = Ls1#{ L := {outgoing, Ts} },           %Assertion.

            Blk = Blk0#b_blk{is=Is,last=Last},
            opt_bs(Bs, Ds, Ls, Fdb, Sub, SuccTypes, Meta, [{L,Blk} | Acc]);
        #{} ->
            %% This block is never reached. Discard it.
            opt_bs(Bs, Ds0, Ls0, Fdb0, Sub0, SuccTypes0, Meta, Acc)
    end;
opt_bs([], _Ds, _Ls, Fdb, _Sub, SuccTypes, _Meta, Acc) ->
    {reverse(Acc), Fdb, SuccTypes}.

opt_is([#b_set{op=call,
               args=[#b_local{}=Callee | _]=Args0,
               dst=Dst}=I0 | Is],
       Ts0, Ds0, Ls, Fdb0, Sub, Meta, Acc) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = I0#b_set{args=Args},

    [_ | CallArgs] = Args,
    {I, Fdb} = opt_local_call(I1, Callee, CallArgs, Dst, Ts0, Fdb0, Meta),

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    opt_is(Is, Ts, Ds, Ls, Fdb, Sub, Meta, [I | Acc]);
opt_is([#b_set{op=call,
               args=[#b_var{} | _]=Args0,
               dst=Dst}=I0 | Is],
       Ts0, Ds0, Ls, Fdb, Sub, Meta, Acc) ->
    Args = simplify_args(Args0, Ts0, Sub),

    I1 = opt_anno_types(I0#b_set{args=Args}, Ts0),

    [Fun | _] = Args,
    I = case normalized_type(Fun, Ts0) of
            #t_fun{type=Type} when Type =/= any ->
                beam_ssa:add_anno(result_type, Type, I1);
            _ ->
                I1
        end,

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    opt_is(Is, Ts, Ds, Ls, Fdb, Sub, Meta, [I | Acc]);
opt_is([#b_set{op=MakeFun,args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Ls, Fdb0, Sub0, Meta, Acc) when MakeFun =:= make_fun;
                                                 MakeFun =:= old_make_fun ->
    Args = simplify_args(Args0, Ts0, Sub0),
    I1 = I0#b_set{args=Args},

    {I, Fdb} = opt_make_fun(I1, Ts0, Fdb0, Meta),

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    opt_is(Is, Ts, Ds, Ls, Fdb, Sub0, Meta, [I|Acc]);
opt_is([I0 | Is], Ts0, Ds0, Ls, Fdb, Sub0, Meta, Acc) ->
    case simplify(I0, Ts0, Ds0, Ls, Sub0) of
        {#b_set{}=I1, Ts, Ds} ->
            I = opt_anno_types(I1, Ts),
            opt_is(Is, Ts, Ds, Ls, Fdb, Sub0, Meta, [I | Acc]);
        Sub when is_map(Sub) ->
            opt_is(Is, Ts0, Ds0, Ls, Fdb, Sub, Meta, Acc)
    end;
opt_is([], Ts, Ds, _Ls, Fdb, Sub, _Meta, Acc) ->
    {reverse(Acc), Ts, Ds, Fdb, Sub}.

opt_anno_types(#b_set{op=Op,args=Args}=I, Ts) ->
    case benefits_from_type_anno(Op, Args) of
        true -> opt_anno_types_1(I, Args, Ts, 0, #{});
        false -> I
    end.

opt_anno_types_1(I, [#b_var{}=Var | Args], Ts, Index, Acc0) ->
    case concrete_type(Var, Ts) of
        any ->
            opt_anno_types_1(I, Args, Ts, Index + 1, Acc0);
        Type ->
            %% Note that we annotate arguments by their index instead of their
            %% variable name, as they may be renamed by `beam_ssa_pre_codegen`.
            Acc = Acc0#{ Index => Type },
            opt_anno_types_1(I, Args, Ts, Index + 1, Acc)
    end;
opt_anno_types_1(I, [_Arg | Args], Ts, Index, Acc) ->
    opt_anno_types_1(I, Args, Ts, Index + 1, Acc);
opt_anno_types_1(#b_set{}=I, [], _Ts, _Index, Acc) when Acc =:= #{} ->
    I;
opt_anno_types_1(#b_set{anno=Anno0}=I, [], _Ts, _Index, Acc) ->
    case Anno0 of
        #{ arg_types := Acc } ->
            I;
        #{} ->
            Anno = Anno0#{ arg_types => Acc },
            I#b_set{anno=Anno}
    end.

%% Only add type annotations when we know we'll make good use of them.
benefits_from_type_anno({bif,'=:='}, _Args) ->
    true;
benefits_from_type_anno({bif,'=/='}, _Args) ->
    true;
benefits_from_type_anno({bif,Op}, Args) ->
    not erl_internal:bool_op(Op, length(Args));
benefits_from_type_anno(is_tagged_tuple, _Args) ->
    true;
benefits_from_type_anno(call, [#b_var{} | _]) ->
    true;
benefits_from_type_anno({float,convert}, _Args) ->
    true;
benefits_from_type_anno(_Op, _Args) ->
    false.

opt_local_call(I0, Callee, Args, Dst, Ts, Fdb, Meta) ->
    ArgTypes = argument_types(Args, Ts),
    I = opt_local_return(I0, Callee, ArgTypes, Fdb),
    case Fdb of
        #{ Callee := #func_info{exported=false,arg_types=AT0}=Info0 } ->
            %% Update the argument types of *this exact call*, the types
            %% will be joined later when the callee is optimized.
            CallId = {Meta#metadata.func_id, Dst},

            AT = update_arg_types(ArgTypes, AT0, CallId),
            Info = Info0#func_info{arg_types=AT},

            {I, Fdb#{ Callee := Info }};
        #{} ->
            %% We can't narrow the argument types of exported functions as they
            %% can receive anything as part of an external call. We can still
            %% rely on their return types however.
            {I, Fdb}
    end.

%% See sig_make_fun/4
opt_make_fun(#b_set{op=MakeFun,
                    dst=Dst,
                    args=[#b_local{}=Callee | FreeVars]}=I0,
             Ts, Fdb, Meta) when MakeFun =:= make_fun;
                                 MakeFun =:= old_make_fun ->
    ArgCount = Callee#b_local.arity - length(FreeVars),
    FVTypes = [concrete_type(FreeVar, Ts) || FreeVar <- FreeVars],
    ArgTypes = duplicate(ArgCount, any) ++ FVTypes,

    I = opt_local_return(I0, Callee, ArgTypes, Fdb),

    case Fdb of
        #{ Callee := #func_info{exported=false,arg_types=AT0}=Info0 } ->
            CallId = {Meta#metadata.func_id, Dst},

            AT = update_arg_types(ArgTypes, AT0, CallId),
            Info = Info0#func_info{arg_types=AT},

            {I, Fdb#{ Callee := Info }};
        #{} ->
            %% We can't narrow the argument types of exported functions as they
            %% can receive anything as part of an external call.
            {I, Fdb}
    end.

opt_local_return(I, Callee, ArgTypes, Fdb) when Fdb =/= #{} ->
    #func_info{succ_types=SuccTypes} = map_get(Callee, Fdb),
    case return_type(SuccTypes, ArgTypes) of
        any -> I;
        Type -> beam_ssa:add_anno(result_type, Type, I)
    end;
opt_local_return(I, _Callee, _ArgTyps, _Fdb) ->
    %% Module-level optimization is disabled, assume it returns anything.
    I.

update_arg_types([ArgType | ArgTypes], [TypeMap0 | TypeMaps], CallId) ->
    TypeMap = TypeMap0#{ CallId => ArgType },
    [TypeMap | update_arg_types(ArgTypes, TypeMaps, CallId)];
update_arg_types([], [], _CallId) ->
    [].

%%

-spec opt_finish(Args, Anno, FuncDb) -> {Anno, FuncDb} when
      Args :: [beam_ssa:b_var()],
      Anno :: beam_ssa:anno(),
      FuncDb :: func_info_db().
opt_finish(Args, Anno, FuncDb) ->
    Id = get_func_id(Anno),
    case FuncDb of
        #{ Id := #func_info{exported=false,arg_types=ArgTypes} } ->
            ParamInfo0 = maps:get(parameter_info, Anno, #{}),
            ParamInfo = opt_finish_1(Args, ArgTypes, ParamInfo0),
            {Anno#{ parameter_info => ParamInfo }, FuncDb};
        #{} ->
            {Anno, FuncDb}
    end.

opt_finish_1([Arg | Args], [TypeMap | TypeMaps], Acc0) ->
    case beam_types:join(maps:values(TypeMap)) of
        any ->
            opt_finish_1(Args, TypeMaps, Acc0);
        JoinedType ->
            Info = maps:get(Arg, Acc0, []),
            Acc = Acc0#{ Arg => [{type, JoinedType} | Info] },
            opt_finish_1(Args, TypeMaps, Acc)
    end;
opt_finish_1([], [], Acc) ->
    Acc.

%%%
%%% Optimization helpers
%%%

simplify_terminator(#b_br{bool=Bool}=Br0, Ts, Ds, Sub) ->
    Br = beam_ssa:normalize(Br0#b_br{bool=simplify_arg(Bool, Ts, Sub)}),
    simplify_not(Br, Ts, Ds, Sub);
simplify_terminator(#b_switch{arg=Arg0,fail=Fail,list=List0}=Sw0,
                    Ts, Ds, Sub) ->
    Arg = simplify_arg(Arg0, Ts, Sub),
    %% Ensure that no label in the switch list is the same as the
    %% failure label.
    List = [{Val,Lbl} || {Val,Lbl} <- List0, Lbl =/= Fail],
    case beam_ssa:normalize(Sw0#b_switch{arg=Arg,list=List}) of
        #b_switch{}=Sw ->
            case beam_types:is_boolean_type(concrete_type(Arg, Ts)) of
                true -> simplify_switch_bool(Sw, Ts, Ds, Sub);
                false -> Sw
            end;
        #b_br{}=Br ->
            simplify_terminator(Br, Ts, Ds, Sub)
    end;
simplify_terminator(#b_ret{arg=Arg}=Ret, Ts, Ds, Sub) ->
    %% Reducing the result of a call to a literal (fairly common for 'ok')
    %% breaks tail call optimization.
    case Ds of
        #{ Arg := #b_set{op=call}} -> Ret;
        #{} -> Ret#b_ret{arg=simplify_arg(Arg, Ts, Sub)}
    end.

%%
%% Simplifies an instruction, returning either a new instruction (with updated
%% type and definition maps), or an updated substitution map if the instruction
%% was redundant.
%%

simplify(#b_set{op=phi,dst=Dst,args=Args0}=I0, Ts0, Ds0, Ls, Sub) ->
    %% Simplify the phi node by removing all predecessor blocks that no
    %% longer exists or no longer branches to this block.
    {Type, Args} = simplify_phi_args(Args0, Ls, Sub, none, []),
    case phi_all_same(Args) of
        true ->
            %% Eliminate the phi node if there is just one source
            %% value or if the values are identical.
            [{Val, _} | _] = Args,
            Sub#{ Dst => Val };
        false ->
            I = I0#b_set{args=Args},

            Ts = Ts0#{ Dst => Type },
            Ds = Ds0#{ Dst => I },
            {I, Ts, Ds}
    end;
simplify(#b_set{op={succeeded,Kind},args=[Arg],dst=Dst}=I,
         Ts0, Ds0, _Ls, Sub) ->
    Type = case will_succeed(I, Ts0, Ds0, Sub) of
               yes -> beam_types:make_atom(true);
               no -> beam_types:make_atom(false);
               maybe -> beam_types:make_boolean()
           end,
    case Type of
        #t_atom{elements=[true]} ->
            %% The checked operation always succeeds, so it's safe to remove
            %% this instruction regardless of whether we're in a guard or not.
            Lit = #b_literal{val=true},
            Sub#{ Dst => Lit };
        #t_atom{elements=[false]} when Kind =:= guard ->
            %% Failing operations are only safe to remove in guards.
            Lit = #b_literal{val=false},
            Sub#{ Dst => Lit };
        _ ->
            true = is_map_key(Arg, Ds0),        %Assertion.

            %% Note that we never simplify args; this instruction is specific
            %% to the operation being checked, and simplifying could break that
            %% connection.
            Ts = Ts0#{ Dst => Type },
            Ds = Ds0#{ Dst => I },
            {I, Ts, Ds}
    end;
simplify(#b_set{op=bs_match,dst=Dst,args=Args0}=I0, Ts0, Ds0, _Ls, Sub) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = I0#b_set{args=Args},
    I2 = case {Args0,Args} of
             {[_,_,_,#b_var{},_],[Type,Val,Flags,#b_literal{val=all},Unit]} ->
                 %% The size `all` is used for the size of the final binary
                 %% segment in a pattern. Using `all` explicitly is not allowed,
                 %% so we convert it to an obvious invalid size.
                 I1#b_set{args=[Type,Val,Flags,#b_literal{val=bad_size},Unit]};
             {_,_} ->
                 I1
         end,
    %% We KNOW that simplify/2 will return a #b_set{} record when called with
    %% a bs_match instruction.
    #b_set{} = I = simplify(I2, Ts0),
    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    {I, Ts, Ds};
simplify(#b_set{dst=Dst,args=Args0}=I0, Ts0, Ds0, _Ls, Sub) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = beam_ssa:normalize(I0#b_set{args=Args}),
    case simplify(I1, Ts0) of
        #b_set{}=I ->
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{ Dst => I },
            {I, Ts, Ds};
        #b_literal{}=Lit ->
            Sub#{ Dst => Lit };
        #b_var{}=Var ->
            Sub#{ Dst => Var }
    end.

simplify(#b_set{op={bif,'and'},args=Args}=I, Ts) ->
    case is_safe_bool_op(Args, Ts) of
        true ->
            case Args of
                [_,#b_literal{val=false}=Res] -> Res;
                [Res,#b_literal{val=true}] -> Res;
                _ -> eval_bif(I, Ts)
            end;
        false ->
            I
    end;
simplify(#b_set{op={bif,'or'},args=Args}=I, Ts) ->
    case is_safe_bool_op(Args, Ts) of
        true ->
            case Args of
                [Res,#b_literal{val=false}] -> Res;
                [_,#b_literal{val=true}=Res] -> Res;
                _ -> eval_bif(I, Ts)
            end;
        false ->
            I
    end;
simplify(#b_set{op={bif,element},args=[#b_literal{val=Index},Tuple]}=I0, Ts) ->
    case normalized_type(Tuple, Ts) of
        #t_tuple{size=Size} when is_integer(Index),
                                 1 =< Index,
                                 Index =< Size ->
            I = I0#b_set{op=get_tuple_element,
                         args=[Tuple,#b_literal{val=Index-1}]},
            simplify(I, Ts);
        _ ->
            eval_bif(I0, Ts)
    end;
simplify(#b_set{op={bif,hd},args=[List]}=I, Ts) ->
    case normalized_type(List, Ts) of
        #t_cons{} ->
            I#b_set{op=get_hd};
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,tl},args=[List]}=I, Ts) ->
    case normalized_type(List, Ts) of
        #t_cons{} ->
            I#b_set{op=get_tl};
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,size},args=[Term]}=I, Ts) ->
    case normalized_type(Term, Ts) of
        #t_tuple{} ->
            simplify(I#b_set{op={bif,tuple_size}}, Ts);
        #t_bitstring{size_unit=U} when U rem 8 =:= 0 ->
            %% If the bitstring is a binary (the size in bits is
            %% evenly divisibly by 8), byte_size/1 gives
            %% the same result as size/1.
            simplify(I#b_set{op={bif,byte_size}}, Ts);
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,tuple_size},args=[Term]}=I, Ts) ->
    case normalized_type(Term, Ts) of
        #t_tuple{size=Size,exact=true} ->
            #b_literal{val=Size};
        _ ->
            I
    end;
simplify(#b_set{op={bif,is_function},args=[Fun,#b_literal{val=Arity}]}=I, Ts)
  when is_integer(Arity), Arity >= 0 ->
    case normalized_type(Fun, Ts) of
        #t_fun{arity=any} ->
            I;
        #t_fun{arity=Arity} ->
            #b_literal{val=true};
        any ->
            I;
        _ ->
            #b_literal{val=false}
    end;
simplify(#b_set{op={bif,is_map_key},args=[Key,Map]}=I, Ts) ->
    case normalized_type(Map, Ts) of
        #t_map{} ->
            I#b_set{op=has_map_field,args=[Map,Key]};
        _ ->
            I
    end;
simplify(#b_set{op={bif,Op0},args=Args}=I, Ts) when Op0 =:= '==';
                                                    Op0 =:= '/=' ->
    Types = normalized_types(Args, Ts),
    EqEq0 = case {beam_types:meet(Types),beam_types:join(Types)} of
                {none,any} -> true;
                {#t_integer{},#t_integer{}} -> true;
                {#t_float{},#t_float{}} -> true;
                {#t_bitstring{},_} -> true;
                {#t_atom{},_} -> true;
                {_,_} -> false
            end,
    EqEq = EqEq0 orelse any_non_numeric_argument(Args, Ts),
    case EqEq of
        true ->
            Op = case Op0 of
                     '==' -> '=:=';
                     '/=' -> '=/='
                 end,
            simplify(I#b_set{op={bif,Op}}, Ts);
        false ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,'=:='},args=[Same,Same]}, _Ts) ->
    #b_literal{val=true};
simplify(#b_set{op={bif,'=:='},args=[LHS,RHS]}=I, Ts) ->
    LType = concrete_type(LHS, Ts),
    RType = concrete_type(RHS, Ts),
    case beam_types:meet(LType, RType) of
        none ->
            #b_literal{val=false};
        _ ->
            case {beam_types:is_boolean_type(LType),
                  beam_types:normalize(RType)} of
                {true,#t_atom{elements=[true]}} ->
                    %% Bool =:= true  ==>  Bool
                    LHS;
                {true,#t_atom{elements=[false]}} ->
                    %% Bool =:= false ==> not Bool
                    %%
                    %% This will be further optimized to eliminate the
                    %% 'not', swapping the success and failure
                    %% branches in the br instruction. If LHS comes
                    %% from a type test (such as is_atom/1) or a
                    %% comparison operator (such as >=) that can be
                    %% translated to test instruction, this
                    %% optimization will eliminate one instruction.
                    simplify(I#b_set{op={bif,'not'},args=[LHS]}, Ts);
                {_,_} ->
                    eval_bif(I, Ts)
            end
   end;
simplify(#b_set{op={bif,is_list},args=[Src]}=I0, Ts) ->
    case concrete_type(Src, Ts) of
        #t_union{list=#t_cons{}} ->
            I = I0#b_set{op=is_nonempty_list,args=[Src]},
            %% We might need to convert back to is_list/1 if it turns
            %% out that this instruction is followed by a #b_ret{}
            %% terminator.
            beam_ssa:add_anno(was_bif_is_list, true, I);
        #t_union{list=nil} ->
            I0#b_set{op={bif,'=:='},args=[Src,#b_literal{val=[]}]};
        _ ->
            eval_bif(I0, Ts)
    end;
simplify(#b_set{op={bif,Op},args=Args}=I, Ts) ->
    Types = normalized_types(Args, Ts),
    case is_float_op(Op, Types) of
        false ->
            eval_bif(I, Ts);
        true ->
            AnnoArgs = [anno_float_arg(A) || A <- Types],
            eval_bif(beam_ssa:add_anno(float_op, AnnoArgs, I), Ts)
    end;
simplify(#b_set{op=bs_extract,args=[Ctx]}=I, Ts) ->
    case concrete_type(Ctx, Ts) of
        #t_bitstring{} ->
            %% This is a bs_match that has been rewritten as a bs_get_tail;
            %% just return the input as-is.
            Ctx;
        #t_bs_context{} ->
            I
    end;
simplify(#b_set{op=bs_match,
                args=[#b_literal{val=binary}, Ctx, _Flags,
                      #b_literal{val=all},
                      #b_literal{val=OpUnit}]}=I, Ts) ->
    %% <<..., Foo/binary>> can be rewritten as <<..., Foo/bits>> if we know the
    %% unit is correct.
    #t_bs_context{tail_unit=CtxUnit} = concrete_type(Ctx, Ts),
    if
        CtxUnit rem OpUnit =:= 0 ->
            I#b_set{op=bs_get_tail,args=[Ctx]};
        CtxUnit rem OpUnit =/= 0 ->
            I
    end;
simplify(#b_set{op=bs_start_match,args=[#b_literal{val=new}, Src]}=I, Ts) ->
    case concrete_type(Src, Ts) of
        #t_bs_context{} ->
            I#b_set{op=bs_start_match,args=[#b_literal{val=resume}, Src]};
        _ ->
            I
    end;
simplify(#b_set{op=get_tuple_element,args=[Tuple,#b_literal{val=N}]}=I, Ts) ->
    #t_tuple{size=Size,elements=Es} = normalized_type(Tuple, Ts),
    true = Size > N,                            %Assertion.
    ElemType = beam_types:get_tuple_element(N + 1, Es),
    case beam_types:get_singleton_value(ElemType) of
        {ok, Val} -> #b_literal{val=Val};
        error -> I
    end;
simplify(#b_set{op=is_nonempty_list,args=[Src]}=I, Ts) ->
    case normalized_type(Src, Ts) of
        any ->
            I;
        #t_list{} ->
            I;
        #t_cons{} ->
            #b_literal{val=true};
        _ ->
            #b_literal{val=false}
    end;
simplify(#b_set{op=is_tagged_tuple,
                args=[Src,#b_literal{val=Size},#b_literal{}=Tag]}=I, Ts) ->
    case concrete_type(Src, Ts) of
        #t_union{tuple_set=TupleSet}=U ->
            %% A union of different types, one of them (probably)
            %% a tuple. Dig out the tuple type from the union and
            %% find out whether it will match.
            TupleOnlyType = #t_union{tuple_set=TupleSet},
            TT = beam_types:normalize(TupleOnlyType),
            case simplify_is_record(I, TT, Size, Tag, Ts) of
                #b_literal{val=true} ->
                    %% The tuple part of the union will always match.
                    %% A simple is_tuple/1 test will be sufficient to
                    %% distinguish the tuple from the other types in
                    %% the union.
                    I#b_set{op={bif,is_tuple},args=[Src]};
                #b_literal{val=false}=False ->
                    %% Src is never a tuple.
                    False;
                _ ->
                    %% More than one type of tuple can match. Find out
                    %% whether the possible tuples can be
                    %% distinguished by size.
                    TupleArityType = #t_tuple{size=Size,exact=true},
                    TTT = beam_types:meet(TupleArityType, TupleOnlyType),
                    case simplify_is_record(I, TTT, Size, Tag, Ts) of
                        #b_literal{val=true} ->
                            %% The possible tuple types have different sizes.
                            %% Example: {ok, _} | {error, _, _}.
                            case beam_types:normalize(U) of
                                #t_tuple{} ->
                                    %% Src is known to be a tuple, so it will
                                    %% be sufficient to test the arity.
                                    beam_ssa:add_anno(constraints, arity, I);
                                any ->
                                    %% Src might not be a tuple. Must
                                    %% test for a tuple with a given
                                    %% arity.
                                    beam_ssa:add_anno(constraints, tuple_arity, I)
                            end;
                        _ ->
                            I
                    end
            end;
        SimpleType ->
            simplify_is_record(I, SimpleType, Size, Tag, Ts)
    end;
simplify(#b_set{op=put_list,args=[#b_literal{val=H},
                                  #b_literal{val=T}]}, _Ts) ->
    #b_literal{val=[H|T]};
simplify(#b_set{op=put_tuple,args=Args}=I, _Ts) ->
    case make_literal_list(Args) of
        none -> I;
        List -> #b_literal{val=list_to_tuple(List)}
    end;
simplify(#b_set{op=call,args=[#b_remote{}=Rem|Args]}=I, Ts) ->
    case Rem of
        #b_remote{mod=#b_literal{val=Mod},
                  name=#b_literal{val=Name}} ->
            simplify_remote_call(Mod, Name, Args, Ts, I);
        #b_remote{} ->
            I
    end;
simplify(#b_set{op=call,args=[#b_literal{val=Fun}|Args]}=I, _Ts)
  when is_function(Fun, length(Args)) ->
    FI = erlang:fun_info(Fun),
    {module,M} = keyfind(module, 1, FI),
    {name,F} = keyfind(name, 1, FI),
    {arity,A} = keyfind(arity, 1, FI),
    Rem = #b_remote{mod=#b_literal{val=M},
                    name=#b_literal{val=F},
                    arity=A},
    I#b_set{args=[Rem|Args]};
simplify(#b_set{op=peek_message,args=[#b_literal{val=Val}]}=I, _Ts) ->
    case Val of
        none ->
            I;
        _ ->
            %% A potential receive marker has been substituted with a literal,
            %% which means it can't actually be a marker on this path. Replace
            %% it with a normal receive.
            I#b_set{args=[#b_literal{val=none}]}
    end;
simplify(#b_set{op=recv_marker_clear,args=[#b_literal{}]}, _Ts) ->
    %% Not a receive marker: see the 'peek_message' case.
    #b_literal{val=none};
simplify(I, _Ts) ->
    I.

will_succeed(#b_set{args=[Src]}, Ts, Ds, Sub) ->
    case {Ds, Ts} of
        {#{}, #{ Src := none }} ->
            %% Checked operation never returns.
            no;
        {#{ Src := I }, #{}} ->
            %% There can't be any substitution because the instruction
            %% is still there.
            false = is_map_key(Src, Sub),        %Assertion.
            will_succeed_1(I, Src, Ts, Sub);
        {#{}, #{}} ->
            %% The checked instruction has been removed and substituted, so we
            %% can assume it always succeeds.
            true = is_map_key(Src, Sub),        %Assertion.
            yes
    end.

will_succeed_1(#b_set{op=bs_get_tail}, _Src, _Ts, _Sub) ->
    yes;
will_succeed_1(#b_set{op=bs_start_match,args=[_, Arg]}, _Src, Ts, _Sub) ->
    ArgType = concrete_type(Arg, Ts),
    case beam_types:is_bs_matchable_type(ArgType) of
        true ->
            %% In the future we may be able to remove this instruction
            %% altogether when we have a #t_bs_context{}, but for now we need
            %% to keep it for compatibility with older releases of OTP.
            yes;
        false ->
            %% Is it at all possible to match?
            case beam_types:meet(ArgType, #t_bs_matchable{}) of
                none -> no;
                _ -> maybe
            end
    end;

will_succeed_1(#b_set{op={bif,Bif},args=BifArgs}, _Src, Ts, _Sub) ->
    ArgTypes = normalized_types(BifArgs, Ts),
    beam_call_types:will_succeed(erlang, Bif, ArgTypes);
will_succeed_1(#b_set{op=call,
                      args=[#b_remote{mod=#b_literal{val=Mod},
                            name=#b_literal{val=Func}} |
                            CallArgs]},
               _Src, Ts, _Sub) ->
            ArgTypes = normalized_types(CallArgs, Ts),
            beam_call_types:will_succeed(Mod, Func, ArgTypes);

will_succeed_1(#b_set{op=get_hd}, _Src, _Ts, _Sub) ->
    yes;
will_succeed_1(#b_set{op=get_tl}, _Src, _Ts, _Sub) ->
    yes;
will_succeed_1(#b_set{op=has_map_field}, _Src, _Ts, _Sub) ->
    yes;
will_succeed_1(#b_set{op=get_tuple_element}, _Src, _Ts, _Sub) ->
    yes;
will_succeed_1(#b_set{op=put_tuple}, _Src, _Ts, _Sub) ->
    yes;
will_succeed_1(#b_set{op=bs_create_bin}, _Src, _Ts, _Sub) ->
    %% Intentionally don't try to determine whether construction will
    %% fail. Construction is unlikely to fail, and if it fails, the
    %% instruction in the runtime system will generate an exception with
    %% better information of what went wrong.
    maybe;
will_succeed_1(#b_set{op=bs_match,
                      args=[#b_literal{val=Type},_,_,#b_literal{val=Size},_]},
               _Src, _Ts, _Sub) ->
    if
        is_integer(Size), Size >= 0 ->
            maybe;
        Type =:= binary, Size =:= all ->
            %% `all` is a legal size for binary segments at the end of
            %% a binary pattern.
            maybe;
        true ->
            %% Invalid size. Matching will fail.
            no
    end;

%% These operations may fail even though we know their return value on success.
will_succeed_1(#b_set{op=call}, _Src, _Ts, _Sub) ->
    maybe;
will_succeed_1(#b_set{op=get_map_element}, _Src, _Ts, _Sub) ->
    maybe;
will_succeed_1(#b_set{op=wait_timeout}, _Src, _Ts, _Sub) ->
    %% It is essential to keep the {succeeded,body} instruction to
    %% ensure that the failure edge, which potentially leads to a
    %% landingpad, is preserved. If the failure edge is removed, a Y
    %% register holding a `try` tag could be reused prematurely.
    maybe;

will_succeed_1(#b_set{}, _Src, _Ts, _Sub) ->
    maybe.

simplify_is_record(I, #t_tuple{exact=Exact,
                               size=Size,
                               elements=Es},
                   RecSize, #b_literal{val=TagVal}=RecTag, Ts) ->
    TagType = maps:get(1, Es, any),
    TagMatch = case beam_types:get_singleton_value(TagType) of
                   {ok, TagVal} -> yes;
                   {ok, _} -> no;
                   error ->
                       %% Is it at all possible for the tag to match?
                       case beam_types:meet(concrete_type(RecTag, Ts), TagType) of
                           none -> no;
                           _ -> maybe
                       end
               end,
    if
        Size =/= RecSize, Exact; Size > RecSize; TagMatch =:= no ->
            #b_literal{val=false};
        Size =:= RecSize, Exact, TagMatch =:= yes ->
            #b_literal{val=true};
        true ->
            I
    end;
simplify_is_record(I, any, _Size, _Tag, _Ts) ->
    I;
simplify_is_record(_I, _Type, _Size, _Tag, _Ts) ->
    #b_literal{val=false}.

simplify_switch_bool(#b_switch{arg=B,fail=Fail,list=List0}, Ts, Ds, Sub) ->
    FalseVal = #b_literal{val=false},
    TrueVal = #b_literal{val=true},
    List1 = List0 ++ [{FalseVal,Fail},{TrueVal,Fail}],
    {_,FalseLbl} = keyfind(FalseVal, 1, List1),
    {_,TrueLbl} = keyfind(TrueVal, 1, List1),
    Br = #b_br{bool=B,succ=TrueLbl,fail=FalseLbl},
    simplify_terminator(Br, Ts, Ds, Sub).

simplify_not(#b_br{bool=#b_var{}=V,succ=Succ,fail=Fail}=Br0, Ts, Ds, Sub) ->
    case Ds of
        #{V:=#b_set{op={bif,'not'},args=[Bool]}} ->
            case beam_types:is_boolean_type(concrete_type(Bool, Ts)) of
                true ->
                    Br = Br0#b_br{bool=Bool,succ=Fail,fail=Succ},
                    simplify_terminator(Br, Ts, Ds, Sub);
                false ->
                    Br0
            end;
        #{} ->
            Br0
    end;
simplify_not(#b_br{bool=#b_literal{}}=Br, _Sub, _Ts, _Ds) ->
    Br.

simplify_phi_args([{Arg0, From} | Rest], Ls, Sub, Type0, Args) ->
    case Ls of
        #{ From := Outgoing } ->
            {outgoing, Ts} = Outgoing,          %Assertion.

            Arg = simplify_arg(Arg0, Ts, Sub),
            Type = beam_types:join(concrete_type(Arg, Ts), Type0),
            Phi = {Arg, From},

            simplify_phi_args(Rest, Ls, Sub, Type, [Phi | Args]);
        #{} ->
            simplify_phi_args(Rest, Ls, Sub, Type0, Args)
    end;
simplify_phi_args([], _Ls, _Sub, Type, Args) ->
    %% We return the arguments in their incoming order so that they won't
    %% change back and forth and ruin fixpoint iteration in beam_ssa_opt.
    {Type, reverse(Args)}.

phi_all_same([{Arg, _From} | Phis]) ->
    phi_all_same_1(Phis, Arg).

phi_all_same_1([{Arg, _From} | Phis], Arg) ->
    phi_all_same_1(Phis, Arg);
phi_all_same_1([], _Arg) ->
    true;
phi_all_same_1(_Phis, _Arg) ->
    false.

simplify_remote_call(erlang, throw, [Term], Ts, I) ->
    %% Annotate `throw` instructions with the type of their thrown term,
    %% helping `beam_ssa_throw` optimize non-local returns.
    Type = normalized_type(Term, Ts),
    beam_ssa:add_anno(thrown_type, Type, I);
simplify_remote_call(erlang, '++', [#b_literal{val=[]},Tl], _Ts, _I) ->
    Tl;
simplify_remote_call(erlang, setelement,
                     [#b_literal{val=Pos},
                      #b_literal{val=Tuple},
                      #b_var{}=Value], _Ts, I)
  when is_integer(Pos), 1 =< Pos, Pos =< tuple_size(Tuple) ->
    %% Position is a literal integer and the shape of the
    %% tuple is known.
    Els0 = [#b_literal{val=El} || El <- tuple_to_list(Tuple)],
    {Bef,[_|Aft]} = split(Pos - 1, Els0),
    Els = Bef ++ [Value|Aft],
    I#b_set{op=put_tuple,args=Els};
simplify_remote_call(Mod, Name, Args, _Ts, I) ->
    case erl_bifs:is_pure(Mod, Name, length(Args)) of
        true ->
            simplify_pure_call(Mod, Name, Args, I);
        false ->
            I
    end.

%% Simplify a remote call to a pure BIF.
simplify_pure_call(Mod, Name, Args0, I) ->
    case make_literal_list(Args0) of
        none ->
            I;
        Args ->
            %% The arguments are literals. Try to evaluate the BIF.
            try apply(Mod, Name, Args) of
                Val ->
                    case cerl:is_literal_term(Val) of
                        true ->
                            #b_literal{val=Val};
                        false ->
                            %% The value can't be expressed as a literal
                            %% (e.g. a pid).
                            I
                    end
            catch
                _:_ ->
                    %% Failed. Don't bother trying to optimize
                    %% the call.
                    I
            end
    end.

any_non_numeric_argument([#b_literal{val=Lit}|_], _Ts) ->
    is_non_numeric(Lit);
any_non_numeric_argument([#b_var{}=V|T], Ts) ->
    is_non_numeric_type(concrete_type(V, Ts)) orelse
        any_non_numeric_argument(T, Ts);
any_non_numeric_argument([], _Ts) -> false.

is_non_numeric([H|T]) ->
    is_non_numeric(H) andalso is_non_numeric(T);
is_non_numeric(Tuple) when is_tuple(Tuple) ->
    is_non_numeric_tuple(Tuple, tuple_size(Tuple));
is_non_numeric(Map) when is_map(Map) ->
    %% Starting from OTP 18, map keys are compared using `=:=`.
    %% Therefore, we only need to check that the values in the map are
    %% non-numeric. (Support for compiling BEAM files for OTP releases
    %% older than OTP 18 has been dropped.)
    is_non_numeric(maps:values(Map));
is_non_numeric(Num) when is_number(Num) ->
    false;
is_non_numeric(_) -> true.

is_non_numeric_tuple(Tuple, El) when El >= 1 ->
    is_non_numeric(element(El, Tuple)) andalso
        is_non_numeric_tuple(Tuple, El-1);
is_non_numeric_tuple(_Tuple, 0) -> true.

is_non_numeric_type(#t_atom{}) -> true;
is_non_numeric_type(#t_bitstring{}) -> true;
is_non_numeric_type(#t_cons{type=Type,terminator=Terminator}) ->
    is_non_numeric_type(Type) andalso is_non_numeric_type(Terminator);
is_non_numeric_type(#t_list{type=Type,terminator=Terminator}) ->
    is_non_numeric_type(Type) andalso is_non_numeric_type(Terminator);
is_non_numeric_type(#t_map{super_value=Value}) ->
    is_non_numeric_type(Value);
is_non_numeric_type(nil) -> true;
is_non_numeric_type(#t_tuple{size=Size,exact=true,elements=Types})
  when map_size(Types) =:= Size ->
    is_non_numeric_tuple_type(Size, Types);
is_non_numeric_type(_) -> false.

is_non_numeric_tuple_type(0, _Types) ->
    true;
is_non_numeric_tuple_type(Pos, Types) ->
    is_non_numeric_type(map_get(Pos, Types)) andalso
        is_non_numeric_tuple_type(Pos - 1, Types).

make_literal_list(Args) ->
    make_literal_list(Args, []).

make_literal_list([#b_literal{val=H}|T], Acc) ->
    make_literal_list(T, [H|Acc]);
make_literal_list([_|_], _) ->
    none;
make_literal_list([], Acc) ->
    reverse(Acc).

is_safe_bool_op([LHS, RHS], Ts) ->
    LType = concrete_type(LHS, Ts),
    RType = concrete_type(RHS, Ts),
    beam_types:is_boolean_type(LType) andalso
        beam_types:is_boolean_type(RType).

eval_bif(#b_set{op={bif,Bif},args=Args}=I, Ts) ->
    Arity = length(Args),
    case erl_bifs:is_pure(erlang, Bif, Arity) of
        false ->
            I;
        true ->
            case make_literal_list(Args) of
                none ->
                    eval_type_test_bif(I, Bif, concrete_types(Args, Ts));
                LitArgs ->
                    try apply(erlang, Bif, LitArgs) of
                        Val -> #b_literal{val=Val}
                    catch
                        error:_ -> I
                    end

            end
    end.

eval_type_test_bif(I, is_atom, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_atom{});
eval_type_test_bif(I, is_binary, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_bs_matchable{tail_unit=8});
eval_type_test_bif(I, is_bitstring, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_bs_matchable{});
eval_type_test_bif(I, is_boolean, [Type]) ->
    case beam_types:is_boolean_type(Type) of
        true ->
            #b_literal{val=true};
        false ->
            case beam_types:meet(Type, #t_atom{}) of
                #t_atom{elements=[_|_]=Es} ->
                    case any(fun is_boolean/1, Es) of
                        true -> I;
                        false -> #b_literal{val=false}
                    end;
                #t_atom{} ->
                    I;
                none ->
                    #b_literal{val=false}
            end
    end;
eval_type_test_bif(I, is_float, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_float{});
eval_type_test_bif(I, is_function, [Type, #t_integer{elements={Arity,Arity}}])
  when Arity >= 0, Arity =< 255 ->
    eval_type_test_bif_1(I, Type, #t_fun{arity=Arity});
eval_type_test_bif(I, is_function, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_fun{});
eval_type_test_bif(I, is_integer, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_integer{});
eval_type_test_bif(I, is_list, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_list{});
eval_type_test_bif(I, is_map, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_map{});
eval_type_test_bif(I, is_number, [Type]) ->
    eval_type_test_bif_1(I, Type, number);
eval_type_test_bif(I, is_pid, [Type]) ->
    eval_type_test_bif_1(I, Type, pid);
eval_type_test_bif(I, is_port, [Type]) ->
    eval_type_test_bif_1(I, Type, port);
eval_type_test_bif(I, is_reference, [Type]) ->
    eval_type_test_bif_1(I, Type, reference);
eval_type_test_bif(I, is_tuple, [Type]) ->
    eval_type_test_bif_1(I, Type, #t_tuple{});
eval_type_test_bif(I, Op, Types) ->
    case Types of
        [#t_integer{},#t_integer{elements={0,0}}] when Op =:= 'bor'; Op =:= 'bxor' ->
            #b_set{args=[Result,_]} = I,
            Result;
        [#t_integer{},#t_integer{elements={0,0}}] when Op =:= '*'; Op =:= 'band' ->
            #b_literal{val=0};
        [T,#t_integer{elements={0,0}}] when Op =:= '+'; Op =:= '-' ->
            case beam_types:is_numerical_type(T) of
                true ->
                    #b_set{args=[Result,_]} = I,
                    Result;
                false ->
                    I
            end;
        [T,#t_integer{elements={1,1}}] when Op =:= '*'; Op =:= 'div' ->
            case beam_types:is_numerical_type(T) of
                true ->
                    #b_set{args=[Result,_]} = I,
                    Result;
                false ->
                    I
            end;
        [#t_integer{elements={LMin,LMax}},#t_integer{elements={RMin,RMax}}] ->
            case is_inequality_op(Op) of
                true ->
                    case {erlang:Op(LMin, RMin),erlang:Op(LMax, RMin),
                          erlang:Op(LMin, RMax),erlang:Op(LMax, RMax)} of
                        {Bool,Bool,Bool,Bool} ->
                            #b_literal{val=Bool};
                        _ ->
                            I
                    end;
                false ->
                    I
            end;
        _ ->
            I
    end.

is_inequality_op('<') -> true;
is_inequality_op('=<') -> true;
is_inequality_op('>') -> true;
is_inequality_op('>=') -> true;
is_inequality_op(_) -> false.

eval_type_test_bif_1(I, ArgType, Required) ->
    case beam_types:meet(ArgType, Required) of
        ArgType -> #b_literal{val=true};
        none -> #b_literal{val=false};
        _ -> I
    end.

simplify_args(Args, Ts, Sub) ->
    [simplify_arg(Arg, Ts, Sub) || Arg <- Args].

simplify_arg(#b_var{}=Arg0, Ts, Sub) ->
    case sub_arg(Arg0, Sub) of
        #b_literal{}=LitArg ->
            LitArg;
        #b_var{}=Arg ->
            case beam_types:get_singleton_value(concrete_type(Arg, Ts)) of
                {ok, Val} -> #b_literal{val=Val};
                error -> Arg
            end
    end;
simplify_arg(#b_remote{mod=Mod,name=Name}=Rem, Ts, Sub) ->
    Rem#b_remote{mod=simplify_arg(Mod, Ts, Sub),
                 name=simplify_arg(Name, Ts, Sub)};
simplify_arg(Arg, _Ts, _Sub) -> Arg.

sub_arg(#b_var{}=Old, Sub) ->
    case Sub of
        #{Old:=New} -> New;
        #{} -> Old
    end.

is_float_op('-', [#t_float{}]) ->
    true;
is_float_op('/', [_,_]) ->
    true;
is_float_op(Op, [#t_float{},_Other]) ->
    is_float_op_1(Op);
is_float_op(Op, [_Other,#t_float{}]) ->
    is_float_op_1(Op);
is_float_op(_, _) -> false.

is_float_op_1('+') -> true;
is_float_op_1('-') -> true;
is_float_op_1('*') -> true;
is_float_op_1(_) -> false.

anno_float_arg(#t_float{}) -> float;
anno_float_arg(_) -> convert.

%%%
%%% Type helpers
%%%

%% Returns the narrowest possible return type for the given success types and
%% arguments.
return_type(SuccTypes0, CallArgs0) ->
    SuccTypes = st_filter_reachable(SuccTypes0, CallArgs0, [], []),
    st_join_return_types(SuccTypes, none).

st_filter_reachable([{SuccArgs, {call_self, SelfArgs}}=SuccType | Rest],
                    CallArgs0, Deferred, Acc) ->
    case st_is_reachable(SuccArgs, CallArgs0) of
        true ->
            %% If we return a call to ourselves, we need to join our current
            %% argument types with that of the call to ensure all possible
            %% return paths are covered.
            CallArgs = parallel_join(SelfArgs, CallArgs0),
            st_filter_reachable(Rest, CallArgs, Deferred, Acc);
        false ->
            %% This may be reachable after we've joined another self-call, so
            %% we defer it until we've gone through all other self-calls.
            st_filter_reachable(Rest, CallArgs0, [SuccType | Deferred], Acc)
    end;
st_filter_reachable([SuccType | Rest], CallArgs, Deferred, Acc) ->
    st_filter_reachable(Rest, CallArgs, Deferred, [SuccType | Acc]);
st_filter_reachable([], CallArgs, Deferred, Acc) ->
    case st_any_reachable(Deferred, CallArgs) of
        true ->
            %% Handle all deferred self calls that may be reachable now that
            %% we've expanded our argument types.
            st_filter_reachable(Deferred, CallArgs, [], Acc);
        false ->
            %% We have no reachable self calls, so we know our argument types
            %% can't expand any further. Filter out our reachable sites and
            %% return.
            [ST || {SuccArgs, _}=ST <- Acc, st_is_reachable(SuccArgs, CallArgs)]
    end.

st_join_return_types([{_SuccArgs, SuccRet} | Rest], Acc0) ->
    st_join_return_types(Rest, beam_types:join(SuccRet, Acc0));
st_join_return_types([], Acc) ->
    Acc.

st_any_reachable([{SuccArgs, _} | SuccType], CallArgs) ->
    case st_is_reachable(SuccArgs, CallArgs) of
        true -> true;
        false -> st_any_reachable(SuccType, CallArgs)
    end;
st_any_reachable([], _CallArgs) ->
    false.

st_is_reachable([A | SuccArgs], [B | CallArgs]) ->
    case beam_types:meet(A, B) of
        none -> false;
        _Other -> st_is_reachable(SuccArgs, CallArgs)
    end;
st_is_reachable([], []) ->
    true.

update_success_types(#b_ret{arg=Arg}, Ts, Ds, Meta, SuccTypes) ->
    #metadata{ func_id=FuncId,
               limit_return=Limited,
               params=Params } = Meta,

    RetType = case Ds of
                  #{ Arg := #b_set{op=call,args=[FuncId | Args]} } ->
                      {call_self, argument_types(Args, Ts)};
                  #{} ->
                      argument_type(Arg, Ts)
              end,
    ArgTypes = argument_types(Params, Ts),

    case Limited of
        true -> ust_limited(SuccTypes, ArgTypes, RetType);
        false -> ust_unlimited(SuccTypes, ArgTypes, RetType)
    end;
update_success_types(_Last, _Ts, _Ds, _Meta, SuccTypes) ->
    SuccTypes.

%% See ?RETURN_LIMIT for details.
ust_limited(SuccTypes, CallArgs, {call_self, SelfArgs}) ->
    NewArgs = parallel_join(CallArgs, SelfArgs),
    ust_limited_1(SuccTypes, NewArgs, none);
ust_limited(SuccTypes, CallArgs, CallRet) ->
    ust_limited_1(SuccTypes, CallArgs, CallRet).

ust_limited_1([], ArgTypes, RetType) ->
    [{ArgTypes, RetType}];
ust_limited_1([{SuccArgs, SuccRet}], CallArgs, CallRet) ->
    NewTypes = parallel_join(SuccArgs, CallArgs),
    NewType = beam_types:join(SuccRet, CallRet),
    [{NewTypes, NewType}].

%% Adds a new success type. Note that we no longer try to keep the list short
%% by combining entries with the same return type, as that can make effective
%% return types less specific as analysis goes on, which may cause endless
%% loops or render previous optimizations unsafe.
%%
%% See beam_type_SUITE:success_type_oscillation/1 for more details.
ust_unlimited(SuccTypes, _CallArgs, none) ->
    %% 'none' is implied since functions can always fail.
    SuccTypes;
ust_unlimited([{SameArgs, SameType} | _]=SuccTypes, SameArgs, SameType) ->
    %% Already covered, return as-is.
    SuccTypes;
ust_unlimited([SuccType | SuccTypes], CallArgs, CallRet) ->
    [SuccType | ust_unlimited(SuccTypes, CallArgs, CallRet)];
ust_unlimited([], CallArgs, CallRet) ->
    [{CallArgs, CallRet}].

update_successors(#b_br{bool=#b_literal{val=true},succ=Succ}=Last,
                  Ts, _Ds, Ls, _UsedOnce) ->
    {Last, update_successor(Succ, Ts, Ls)};
update_successors(#b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail}=Last0,
                  Ts, Ds, Ls0, UsedOnce) ->
    IsTempVar = is_map_key(Bool, UsedOnce),
    case infer_types_br(Bool, Ts, IsTempVar, Ds) of
        {#{}=SuccTs, #{}=FailTs} ->
            Ls1 = update_successor(Succ, SuccTs, Ls0),
            Ls = update_successor(Fail, FailTs, Ls1),
            {Last0, Ls};
        {#{}=SuccTs, none} ->
            Last = Last0#b_br{bool=#b_literal{val=true},fail=Succ},
            {Last, update_successor(Succ, SuccTs, Ls0)};
        {none, #{}=FailTs} ->
            Last = Last0#b_br{bool=#b_literal{val=true},succ=Fail},
            {Last, update_successor(Fail, FailTs, Ls0)}
    end;
update_successors(#b_switch{arg=#b_var{}=V,fail=Fail0,list=List0}=Last0,
                  Ts, Ds, Ls0, UsedOnce) ->
    IsTempVar = is_map_key(V, UsedOnce),

    {List1, FailTs, Ls1} =
        update_switch(List0, V, concrete_type(V, Ts),
                      Ts, Ds, Ls0, IsTempVar, []),

    case FailTs of
        none ->
            %% The fail block is unreachable; swap it with one of the choices.
            case List1 of
                [{#b_literal{val=0},_}|_] ->
                    %% Swap with the last choice in order to keep the zero the
                    %% first choice. If the loader can substitute a jump table
                    %% instruction, then a shorter version of the jump table
                    %% instruction can be used if the first value is zero.
                    {List, [{_,Fail}]} = split(length(List1)-1, List1),
                    Last = Last0#b_switch{fail=Fail,list=List},
                    {Last, Ls1};
                [{_,Fail}|List] ->
                    %% Swap with the first choice in the list.
                    Last = Last0#b_switch{fail=Fail,list=List},
                    {Last, Ls1}
            end;
        #{} ->
            Ls = update_successor(Fail0, FailTs, Ls1),
            Last = Last0#b_switch{list=List1},
            {Last, Ls}
    end;
update_successors(#b_ret{}=Last, _Ts, _Ds, Ls, _UsedOnce) ->
    {Last, Ls}.

update_switch([{Val, Lbl}=Sw | List],
              V, FailType0, Ts, Ds, Ls0, IsTempVar, Acc) ->
    FailType = beam_types:subtract(FailType0, concrete_type(Val, Ts)),
    case infer_types_switch(V, Val, Ts, IsTempVar, Ds) of
        none ->
            update_switch(List, V, FailType, Ts, Ds, Ls0, IsTempVar, Acc);
        SwTs ->
            Ls = update_successor(Lbl, SwTs, Ls0),
            update_switch(List, V, FailType, Ts, Ds, Ls, IsTempVar, [Sw | Acc])
    end;
update_switch([], _V, none, _Ts, _Ds, Ls, _IsTempVar, Acc) ->
    %% Fail label is unreachable.
    {reverse(Acc), none, Ls};
update_switch([], V, FailType, Ts, Ds, Ls, IsTempVar, Acc) ->
    %% Fail label is reachable, see if we can narrow the type down further.
    FailTs = case beam_types:get_singleton_value(FailType) of
                 {ok, Value} ->
                     %% This is the only possible value at the fail label, so
                     %% we can infer types as if we matched it directly.
                     Lit = #b_literal{val=Value},
                     infer_types_switch(V, Lit, Ts, IsTempVar, Ds);
                 error when IsTempVar ->
                     ts_remove_var(V, Ts);
                 error ->
                     Ts#{ V := FailType }
             end,
    {reverse(Acc), FailTs, Ls}.

update_successor(?EXCEPTION_BLOCK, _Ts, Ls) ->
    %% We KNOW that no variables are used in the ?EXCEPTION_BLOCK,
    %% so there is no need to update the type information. That
    %% can be a huge timesaver for huge functions.
    Ls;
update_successor(S, Ts0, Ls) ->
    case Ls of
        #{ S := {outgoing, _} } ->
            %% We're in a receive loop or similar; the target block will not be
            %% revisited.
            Ls;
        #{ S := {incoming, InTs} } ->
            Ts = join_types(Ts0, InTs),
            Ls#{ S := {incoming, Ts} };
        #{} ->
            Ls#{ S => {incoming, Ts0} }
    end.

update_types(#b_set{op=Op,dst=Dst,anno=Anno,args=Args}, Ts, Ds) ->
    T = type(Op, Args, Anno, Ts, Ds),
    Ts#{ Dst => T }.

type({bif,Bif}, Args, _Anno, Ts, _Ds) ->
    ArgTypes = normalized_types(Args, Ts),
    {RetType, _, _} = beam_call_types:types(erlang, Bif, ArgTypes),
    RetType;
type(bs_create_bin, _Args, _Anno, _Ts, _Ds) ->
    #t_bitstring{};
type(bs_extract, [Ctx], _Anno, _Ts, Ds) ->
    #b_set{op=bs_match,args=Args} = map_get(Ctx, Ds),
    bs_match_type(Args);
type(bs_start_match, [_, Src], _Anno, Ts, _Ds) ->
    case beam_types:meet(#t_bs_matchable{}, concrete_type(Src, Ts)) of
        none ->
            none;
        T ->
            Unit = beam_types:get_bs_matchable_unit(T),
            #t_bs_context{tail_unit=Unit}
    end;
type(bs_match, [#b_literal{val=binary}, Ctx, _Flags,
                #b_literal{val=all}, #b_literal{val=OpUnit}],
     _Anno, Ts, _Ds) ->

    %% This is an explicit tail unit test which does not advance the match
    %% position.
    CtxType = concrete_type(Ctx, Ts),
    OpType = #t_bs_context{tail_unit=OpUnit},

    beam_types:meet(CtxType, OpType);
type(bs_match, Args, _Anno, Ts, _Ds) ->
    [_, Ctx | _] = Args,

    %% Matches advance the current position without testing the tail unit. We
    %% try to retain unit information by taking the GCD of our current unit and
    %% the increments we know the match will advance by.
    #t_bs_context{tail_unit=CtxUnit} = concrete_type(Ctx, Ts),
    OpUnit = bs_match_stride(Args, Ts),

    #t_bs_context{tail_unit=gcd(OpUnit, CtxUnit)};
type(bs_get_tail, [Ctx], _Anno, Ts, _Ds) ->
    #t_bs_context{tail_unit=Unit} = concrete_type(Ctx, Ts),
    #t_bitstring{size_unit=Unit};
type(call, [#b_remote{mod=#b_literal{val=Mod},
                      name=#b_literal{val=Name}}|Args], _Anno, Ts, _Ds) ->
    ArgTypes = normalized_types(Args, Ts),
    {RetType, _, _} = beam_call_types:types(Mod, Name, ArgTypes),
    RetType;
type(call, [#b_remote{} | _Args], _Anno, _Ts, _Ds) ->
    %% Remote call with variable Module and/or Function.
    any;
type(call, [#b_local{} | _Args], Anno, _Ts, _Ds) ->
    case Anno of
        #{ result_type := Type } -> Type;
        #{} -> any
    end;
type(call, [#b_var{} | _Args], Anno, _Ts, _Ds) ->
    case Anno of
        #{ result_type := Type } -> Type;
        #{} -> any
    end;
type(call, [#b_literal{val=Fun} | Args], _Anno, _Ts, _Ds) ->
    case is_function(Fun, length(Args)) of
        true ->
            %% This is an external fun literal (fun M:F/A).
            any;
        false ->
            %% This is either not a fun literal or the number of
            %% arguments is wrong.
            none
    end;
type(extract, [V, #b_literal{val=Idx}], _Anno, _Ts, Ds) ->
    case map_get(V, Ds) of
        #b_set{op=landingpad} when Idx =:= 0 ->
            %% Class
            #t_atom{elements=[error,exit,throw]};
        #b_set{op=landingpad} when Idx =:= 1 ->
            %% Reason
            any;
        #b_set{op=landingpad} when Idx =:= 2 ->
            %% Stack trace
            any
    end;
type(get_hd, [Src], _Anno, Ts, _Ds) ->
    SrcType = #t_cons{} = normalized_type(Src, Ts), %Assertion.
    {RetType, _, _} = beam_call_types:types(erlang, hd, [SrcType]),
    RetType;
type(get_tl, [Src], _Anno, Ts, _Ds) ->
    SrcType = #t_cons{} = normalized_type(Src, Ts), %Assertion.
    {RetType, _, _} = beam_call_types:types(erlang, tl, [SrcType]),
    RetType;
type(get_map_element, [_, _]=Args0, _Anno, Ts, _Ds) ->
    [#t_map{}=Map, Key] = normalized_types(Args0, Ts), %Assertion.
    {RetType, _, _} = beam_call_types:types(erlang, map_get, [Key, Map]),
    RetType;
type(get_tuple_element, [Tuple, Offset], _Anno, _Ts, _Ds) ->
    #b_literal{val=N} = Offset,
    Index = N + 1,
    %% Defer our type until our first use (concrete_type/2), as our type may
    %% depend on another value extracted from the same container.
    fun(Ts) ->
            #t_tuple{size=Size,elements=Es} = normalized_type(Tuple, Ts),
            true = Index =< Size,               %Assertion.
            beam_types:get_tuple_element(N + 1, Es)
    end;
type(has_map_field, [_, _]=Args0, _Anno, Ts, _Ds) ->
    [#t_map{}=Map, Key] = normalized_types(Args0, Ts), %Assertion.
    {RetType, _, _} = beam_call_types:types(erlang, is_map_key, [Key, Map]),
    RetType;
type(is_nonempty_list, [_], _Anno, _Ts, _Ds) ->
    beam_types:make_boolean();
type(is_tagged_tuple, [_,#b_literal{},#b_literal{}], _Anno, _Ts, _Ds) ->
    beam_types:make_boolean();
type(MakeFun, [#b_local{arity=TotalArity} | Env], Anno, _Ts, _Ds)
  when MakeFun =:= make_fun;
       MakeFun =:= old_make_fun ->
    RetType = case Anno of
                  #{ result_type := Type } -> Type;
                  #{} -> any
              end,
    #t_fun{arity=TotalArity - length(Env), type=RetType};
type(put_map, [_Kind, Map | Ss], _Anno, Ts, _Ds) ->
    put_map_type(Map, Ss, Ts);
type(put_list, [Head, Tail], _Anno, Ts, _Ds) ->
    HeadType = concrete_type(Head, Ts),
    TailType = concrete_type(Tail, Ts),
    beam_types:make_cons(HeadType, TailType);
type(put_tuple, Args, _Anno, Ts, _Ds) ->
    {Es, _} = foldl(fun(Arg, {Es0, Index}) ->
                            Type = concrete_type(Arg, Ts),
                            Es = beam_types:set_tuple_element(Index, Type, Es0),
                            {Es, Index + 1}
                    end, {#{}, 1}, Args),
    #t_tuple{exact=true,size=length(Args),elements=Es};
type(resume, [_, _], _Anno, _Ts, _Ds) ->
    none;
type(wait_timeout, [#b_literal{val=infinity}], _Anno, _Ts, _Ds) ->
    %% Waits forever, never reaching the 'after' block.
    beam_types:make_atom(false);
type(_, _, _, _, _) ->
    any.

put_map_type(Map, Ss, Ts) ->
    pmt_1(Ss, Ts, normalized_type(Map, Ts)).

pmt_1([Key0, Value0 | Ss], Ts, Acc0) ->
    Key = normalized_type(Key0, Ts),
    Value = normalized_type(Value0, Ts),
    {Acc, _, _} = beam_call_types:types(maps, put, [Key, Value, Acc0]),
    pmt_1(Ss, Ts, Acc);
pmt_1([], _Ts, Acc) ->
    Acc.

%% We seldom know how far a match operation may advance, but we can often tell
%% which increment it will advance by.
bs_match_stride([#b_literal{val=Type} | Args], Ts) ->
    bs_match_stride(Type, Args, Ts).

bs_match_stride(_, [_,_,Size,#b_literal{val=Unit}], Ts) ->
    case concrete_type(Size, Ts) of
        #t_integer{elements={Sz, Sz}} when is_integer(Sz) ->
            Sz * Unit;
        _ ->
            Unit
    end;
bs_match_stride(string, [_,#b_literal{val=String}], _) ->
    bit_size(String);
bs_match_stride(utf8, _, _) ->
    8;
bs_match_stride(utf16, _, _) ->
    16;
bs_match_stride(utf32, _, _) ->
    32;
bs_match_stride(_, _, _) ->
    1.

-define(UNICODE_MAX, (16#10FFFF)).

bs_match_type([#b_literal{val=Type}|Args]) ->
    bs_match_type(Type, Args).

bs_match_type(binary, Args) ->
    [_,_,_,#b_literal{val=U}] = Args,
    #t_bitstring{size_unit=U};
bs_match_type(float, _) ->
    #t_float{};
bs_match_type(integer, Args) ->
    case Args of
        [_,
         #b_literal{val=Flags},
         #b_literal{val=Size},
         #b_literal{val=Unit}] when Size * Unit < 64 ->
            NumBits = Size * Unit,
            case member(unsigned, Flags) of
                true ->
                    beam_types:make_integer(0, (1 bsl NumBits)-1);
                false ->
                    %% Signed integer. Don't bother.
                    #t_integer{}
            end;
        [_|_] ->
            #t_integer{}
    end;
bs_match_type(skip, _) ->
    any;
bs_match_type(string, _) ->
    any;
bs_match_type(utf8, _) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(utf16, _) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(utf32, _) ->
    beam_types:make_integer(0, ?UNICODE_MAX).

normalized_types(Values, Ts) ->
    [normalized_type(Val, Ts) || Val <- Values].

-spec normalized_type(beam_ssa:value(), type_db()) -> normal_type().

normalized_type(V, Ts) ->
    beam_types:normalize(concrete_type(V, Ts)).

argument_types(Values, Ts) ->
    [argument_type(Val, Ts) || Val <- Values].

-spec argument_type(beam_ssa:value(), type_db()) -> type().

argument_type(V, Ts) ->
    beam_types:limit_depth(concrete_type(V, Ts)).

concrete_types(Values, Ts) ->
    [concrete_type(Val, Ts) || Val <- Values].

-spec concrete_type(beam_ssa:value(), type_db()) -> type().

concrete_type(#b_literal{val=Value}, _Ts) ->
    beam_types:make_type_from_value(Value);
concrete_type(#b_var{}=Var, Ts) ->
    #{ Var := Type } = Ts,
    case is_function(Type) of
        true -> Type(Ts);
        false -> Type
    end.

%% infer_types(Var, Types, #d{}) -> {SuccTypes,FailTypes}
%%  Looking at the expression that defines the variable Var, infer
%%  the types for the variables in the arguments. Return the updated
%%  type database for the case that the expression evaluates to
%%  true, and and for the case that it evaluates to false.
%%
%%  Here is an example. The variable being asked about is
%%  the variable Bool, which is defined like this:
%%
%%     Bool = is_nonempty_list L
%%
%%  If 'is_nonempty_list L' evaluates to 'true', L must
%%  must be cons. The meet of the previously known type of L and 'cons'
%%  will be added to SuccTypes.
%%
%%  On the other hand, if 'is_nonempty_list L' evaluates to false, L
%%  is not cons and cons can be subtracted from the previously known
%%  type for L. For example, if L was known to be 'list', subtracting
%%  'cons' would give 'nil' as the only possible type. The result of the
%%  subtraction for L will be added to FailTypes.

infer_types_br(#b_var{}=V, Ts, IsTempVar, Ds) ->
    #{V:=#b_set{op=Op,args=Args}} = Ds,

    {PosTypes, NegTypes} = infer_type(Op, Args, Ts, Ds),

    SuccTs0 = meet_types(PosTypes, Ts),
    FailTs0 = subtract_types(NegTypes, Ts),

    case IsTempVar of
        true ->
            %% The branch variable is defined in this block and is only
            %% referenced by this terminator. Therefore, there is no need to
            %% include it in the type database passed on to the successors of
            %% of this block.
            SuccTs = ts_remove_var(V, SuccTs0),
            FailTs = ts_remove_var(V, FailTs0),
            {SuccTs, FailTs};
        false ->
            SuccTs = infer_br_value(V, true, SuccTs0),
            FailTs = infer_br_value(V, false, FailTs0),
            {SuccTs, FailTs}
    end.

infer_br_value(_V, _Bool, none) ->
    none;
infer_br_value(V, Bool, NewTs) ->
    #{ V := T } = NewTs,
    case beam_types:is_boolean_type(T) of
        true ->
            NewTs#{ V := beam_types:make_atom(Bool) };
        false ->
            %% V is a try/catch tag or similar, leave it alone.
            NewTs
    end.

infer_types_switch(V, Lit, Ts0, IsTempVar, Ds) ->
    {PosTypes, _} = infer_type({bif,'=:='}, [V, Lit], Ts0, Ds),
    Ts = meet_types(PosTypes, Ts0),
    case IsTempVar of
        true -> ts_remove_var(V, Ts);
        false -> Ts
    end.

ts_remove_var(_V, none) -> none;
ts_remove_var(V, Ts) -> maps:remove(V, Ts).

infer_type({succeeded,_}, [#b_var{}=Src], Ts, Ds) ->
    #b_set{op=Op,args=Args} = maps:get(Src, Ds),
    infer_success_type(Op, Args, Ts, Ds);

%% Type tests are handled separately from other BIFs as we're inferring types
%% based on their result, so we know that subtraction is safe even if we're
%% not branching on 'succeeded'.
infer_type(is_tagged_tuple, [#b_var{}=Src,#b_literal{val=Size},
                             #b_literal{}=Tag], _Ts, _Ds) ->
    Es = beam_types:set_tuple_element(1, concrete_type(Tag, #{}), #{}),
    T = {Src,#t_tuple{exact=true,size=Size,elements=Es}},
    {[T], [T]};
infer_type(is_nonempty_list, [#b_var{}=Src], _Ts, _Ds) ->
    T = {Src,#t_cons{}},
    {[T], [T]};
infer_type({bif,is_atom}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_atom{}},
    {[T], [T]};
infer_type({bif,is_binary}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_bitstring{size_unit=8}},
    {[T], [T]};
infer_type({bif,is_bitstring}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_bitstring{}},
    {[T], [T]};
infer_type({bif,is_boolean}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, beam_types:make_boolean()},
    {[T], [T]};
infer_type({bif,is_float}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_float{}},
    {[T], [T]};
infer_type({bif,is_function}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_fun{}},
    {[T], [T]};
infer_type({bif,is_function}, [#b_var{}=Arg, Arity0], _Ts, _Ds) ->
    Arity = case Arity0 of
                #b_literal{val=V} when is_integer(V), V >= 0, V =< 255 -> V;
                _ -> any
            end,
    T = {Arg, #t_fun{arity=Arity}},
    {[T], [T]};
infer_type({bif,is_integer}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_integer{}},
    {[T], [T]};
infer_type({bif,is_list}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_list{}},
    {[T], [T]};
infer_type({bif,is_map}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_map{}},
    {[T], [T]};
infer_type({bif,is_number}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, number},
    {[T], [T]};
infer_type({bif,is_pid}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, pid},
    {[T], [T]};
infer_type({bif,is_port}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, port},
    {[T], [T]};
infer_type({bif,is_reference}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, reference},
    {[T], [T]};
infer_type({bif,is_tuple}, [#b_var{}=Arg], _Ts, _Ds) ->
    T = {Arg, #t_tuple{}},
    {[T], [T]};
infer_type({bif,'=:='}, [#b_var{}=LHS,#b_var{}=RHS], Ts, _Ds) ->
    %% As an example, assume that L1 is known to be 'list', and L2 is
    %% known to be 'cons'. Then if 'L1 =:= L2' evaluates to 'true', it can
    %% be inferred that L1 is 'cons' (the meet of 'cons' and 'list').
    LType = concrete_type(LHS, Ts),
    RType = concrete_type(RHS, Ts),
    Type = beam_types:meet(LType, RType),

    PosTypes = [{V,Type} || {V, OrigType} <- [{LHS, LType}, {RHS, RType}],
                            OrigType =/= Type],

    %% We must be careful with types inferred from '=:='.
    %%
    %% If we have seen L =:= [a], we know that L is 'cons' if the
    %% comparison succeeds. However, if the comparison fails, L could
    %% still be 'cons'. Therefore, we must not subtract 'cons' from the
    %% previous type of L.
    %%
    %% However, it is safe to subtract a type inferred from '=:=' if
    %% it is single-valued, e.g. if it is [] or the atom 'true'.
    %%
    %% Note that we subtract the left-hand type from the right-hand
    %% value and vice versa. We must not subtract the meet of the two
    %% as it may be too specific. See beam_type_SUITE:type_subtraction/1
    %% for details.
    NegTypes = [T || {_, OtherType}=T <- [{RHS, LType}, {LHS, RType}],
                     beam_types:is_singleton_type(OtherType)],

    {PosTypes, NegTypes};
infer_type({bif,'=:='}, [#b_var{}=Src,#b_literal{}=Lit], Ts, Ds) ->
    Def = maps:get(Src, Ds),
    LitType = concrete_type(Lit, Ts),
    PosTypes = [{Src, LitType} | infer_eq_lit(Def, LitType)],

    %% Subtraction is only safe if LitType is single-valued.
    NegTypes = case beam_types:is_singleton_type(LitType) of
                    true -> PosTypes;
                    false -> []
               end,

    {PosTypes, NegTypes};
infer_type(_Op, _Args, _Ts, _Ds) ->
    {[], []}.

infer_success_type({bif,Op}, Args, Ts, _Ds) ->
    ArgTypes = normalized_types(Args, Ts),

    {_, PosTypes0, CanSubtract} = beam_call_types:types(erlang, Op, ArgTypes),
    PosTypes = [T || {#b_var{},_}=T <- zip(Args, PosTypes0)],

    case CanSubtract of
        true -> {PosTypes, PosTypes};
        false -> {PosTypes, []}
    end;
infer_success_type(call, [#b_var{}=Fun|Args], _Ts, _Ds) ->
    T = {Fun, #t_fun{arity=length(Args)}},
    {[T], []};
infer_success_type(bs_start_match, [_, #b_var{}=Src], _Ts, _Ds) ->
    T = {Src,#t_bs_matchable{}},
    {[T], [T]};
infer_success_type(bs_match, [#b_literal{val=binary},
                              Ctx, _Flags,
                              #b_literal{val=all},
                              #b_literal{val=OpUnit}],
                   _Ts, _Ds) ->
    %% This is an explicit tail unit test which does not advance the match
    %% position, so we know that Ctx has the same unit.
    T = {Ctx, #t_bs_context{tail_unit=OpUnit}},
    {[T], [T]};
infer_success_type(_Op, _Args, _Ts, _Ds) ->
    {[], []}.

infer_eq_lit(#b_set{op={bif,tuple_size},args=[#b_var{}=Tuple]},
             #t_integer{elements={Size,Size}}) ->
    [{Tuple,#t_tuple{exact=true,size=Size}}];
infer_eq_lit(#b_set{op=get_tuple_element,
                    args=[#b_var{}=Tuple,#b_literal{val=N}]},
             LitType) ->
    Index = N + 1,
    case beam_types:set_tuple_element(Index, LitType, #{}) of
        #{ Index := _ }=Es ->
            [{Tuple,#t_tuple{size=Index,elements=Es}}];
        #{} ->
            %% Index was above the element limit; subtraction is not safe.
            []
    end;
infer_eq_lit(_, _) ->
    [].

join_types(Ts, Ts) ->
    Ts;
join_types(LHS, RHS) ->
    if
        map_size(LHS) < map_size(RHS) ->
            join_types_1(maps:keys(LHS), RHS, LHS);
        true ->
            join_types_1(maps:keys(RHS), LHS, RHS)
    end.

%% Joins two type maps, keeping the variables that are common to both maps.
join_types_1([V | Vs], Bigger, Smaller) ->
    case {Bigger, Smaller} of
        {#{ V := Same }, #{ V := Same }} ->
            join_types_1(Vs, Bigger, Smaller);
        {#{ V := LHS0 }, #{ V := RHS0 }} ->
            %% Inlined concrete_type/2 for performance.
            LHS = case is_function(LHS0) of
                      true -> LHS0(Bigger);
                      false -> LHS0
                  end,
            RHS = case is_function(RHS0) of
                      true -> RHS0(Smaller);
                      false -> RHS0
                  end,
            T = beam_types:join(LHS, RHS),
            join_types_1(Vs, Bigger, Smaller#{ V := T });
        {#{}, #{ V := _ }} ->
            join_types_1(Vs, Bigger, maps:remove(V, Smaller))
    end;
join_types_1([], _Bigger, Smaller) ->
    Smaller.

meet_types([{V,T0}|Vs], Ts) ->
    T1 = concrete_type(V, Ts),
    case beam_types:meet(T0, T1) of
        none -> none;
        T1 -> meet_types(Vs, Ts);
        T -> meet_types(Vs, Ts#{ V := T })
    end;
meet_types([], Ts) ->
    Ts.

subtract_types([{V,T0}|Vs], Ts) ->
    T1 = concrete_type(V, Ts),
    case beam_types:subtract(T1, T0) of
        none -> none;
        T1 -> subtract_types(Vs, Ts);
        T -> subtract_types(Vs, Ts#{ V:= T })
    end;
subtract_types([], Ts) ->
    Ts.

parallel_join([A | As], [B | Bs]) ->
    [beam_types:join(A, B) | parallel_join(As, Bs)];
parallel_join([], []) ->
    [].

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

%%%
%%% Helpers
%%%

init_metadata(FuncId, Linear, Params) ->
    {RetCounter, UsedOnce0} = init_metadata_1(reverse(Linear), 0, #{}),
    UsedOnce = maps:without(Params, UsedOnce0),

    #metadata{ func_id = FuncId,
               limit_return = (RetCounter >= ?RETURN_LIMIT),
               params = Params,
               used_once = UsedOnce }.

init_metadata_1([{L,#b_blk{is=Is,last=Last}} | Bs], RetCounter0, Uses0) ->
    %% Track the number of return terminators in use. See ?RETURN_LIMIT for
    %% details.
    RetCounter = case Last of
                     #b_ret{} -> RetCounter0 + 1;
                     _ -> RetCounter0
                 end,

    %% Calculate the set of variables that are only used once in the
    %% terminator of the block that defines them. That will allow us
    %% to discard type information for variables that will never be
    %% referenced by the successor blocks, potentially improving
    %% compilation times.

    Uses1 = used_once_last_uses(beam_ssa:used(Last), L, Uses0),
    Uses = used_once_2(reverse(Is), L, Uses1),
    init_metadata_1(Bs, RetCounter, Uses);
init_metadata_1([], RetCounter, Uses) ->
    {RetCounter, Uses}.

used_once_2([#b_set{dst=Dst}=I|Is], L, Uses0) ->
    Uses = used_once_uses(beam_ssa:used(I), L, Uses0),
    case Uses of
        #{Dst:=L} ->
            used_once_2(Is, L, Uses);
        #{} ->
            %% Used more than once or used once in
            %% in another block.
            used_once_2(Is, L, maps:remove(Dst, Uses))
    end;
used_once_2([], _, Uses) -> Uses.

used_once_uses([V|Vs], L, Uses) ->
    case Uses of
        #{V:=more_than_once} ->
            used_once_uses(Vs, L, Uses);
        #{} ->
            %% Already used or first use is not in
            %% a terminator.
            used_once_uses(Vs, L, Uses#{V=>more_than_once})
    end;
used_once_uses([], _, Uses) -> Uses.

used_once_last_uses([V|Vs], L, Uses) ->
    case Uses of
        #{V:=more_than_once} ->
            %% Used at least twice before.
            used_once_last_uses(Vs, L, Uses);
        #{V:=_} ->
            %% Second time this variable is used.
            used_once_last_uses(Vs, L, Uses#{V:=more_than_once});
        #{} ->
            %% First time this variable is used.
            used_once_last_uses(Vs, L, Uses#{V=>L})
    end;
used_once_last_uses([], _, Uses) -> Uses.

%%
%% Ordered worklist used in signatures/2.
%%
%% This is equivalent to consing (wl_add) and appending (wl_defer_list)
%% to a regular list, but avoids uneccessary work by reordering elements.
%%
%% We can do this since a function only needs to be visited *once* for all
%% prior updates to take effect, so if an element is added to the front, then
%% all earlier instances of the same element are redundant.
%%

-record(worklist,
        { counter = 0 :: integer(),
          elements = gb_trees:empty() :: gb_trees:tree(integer(), term()),
          indexes = #{} :: #{ term() => integer() } }).

-type worklist() :: #worklist{}.

wl_new() -> #worklist{}.

%% Adds an element to the worklist, or moves it to the front if it's already
%% present.
wl_add(Element, #worklist{counter=Counter,elements=Es,indexes=Is}) ->
    case Is of
        #{ Element := Index } ->
            wl_add_1(Element, Counter, gb_trees:delete(Index, Es), Is);
        #{} ->
            wl_add_1(Element, Counter, Es, Is)
    end.

wl_add_1(Element, Counter0, Es0, Is0) ->
    Counter = Counter0 + 1,
    Es = gb_trees:insert(Counter, Element, Es0),
    Is = Is0#{ Element => Counter },
    #worklist{counter=Counter,elements=Es,indexes=Is}.

%% All mutations bump the counter, so we can check for changes without a deep
%% comparison.
wl_changed(#worklist{counter=Same}, #worklist{counter=Same}) -> false;
wl_changed(#worklist{}, #worklist{}) -> true.

%% Adds the given elements to the back of the worklist, skipping the elements
%% that are already present. This lets us append elements arbitrarily after the
%% current front without changing the work order.
wl_defer_list(Elements, #worklist{counter=Counter,elements=Es,indexes=Is}) ->
    wl_defer_list_1(Elements, Counter, Es, Is).

wl_defer_list_1([Element | Elements], Counter0, Es0, Is0) ->
    case Is0 of
        #{ Element := _ } ->
            wl_defer_list_1(Elements, Counter0, Es0, Is0);
        #{} ->
            Counter = Counter0 + 1,
            Es = gb_trees:insert(-Counter, Element, Es0),
            Is = Is0#{ Element => -Counter },
            wl_defer_list_1(Elements, Counter, Es, Is)
    end;
wl_defer_list_1([], Counter, Es, Is) ->
    #worklist{counter=Counter,elements=Es,indexes=Is}.

wl_next(#worklist{indexes=Is}) when Is =:= #{} ->
    empty;
wl_next(#worklist{elements=Es,indexes=Is}) when Is =/= #{} ->
    {_Key, Element} = gb_trees:largest(Es),
    {ok, Element}.

%% Removes the front of the worklist.
wl_pop(Element, #worklist{counter=Counter0,elements=Es0,indexes=Is0}=Wl) ->
    Counter = Counter0 + 1,
    {_Key, Element, Es} = gb_trees:take_largest(Es0), %Assertion.
    Is = maps:remove(Element, Is0),
    Wl#worklist{counter=Counter,elements=Es,indexes=Is}.
