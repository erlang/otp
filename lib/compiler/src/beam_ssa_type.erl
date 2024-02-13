%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
-moduledoc false.
-export([opt_start/2, opt_continue/4, opt_finish/3, opt_ranges/1]).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

-import(lists, [duplicate/2,foldl/3,member/2,
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
-type type_db() :: #{ beam_ssa:b_var() := ssa_type() }.

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

    Ds = #{Var => FakeCall#b_set{dst=Var} ||
             #b_var{}=Var <- Args},

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
       Ts0, Ds0, Ls, Fdb, Sub, State0) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = I0#b_set{args=Args},

    {I, State} = sig_fun_call(I1, Args, Ts0, Ds0, Fdb, Sub, State0),

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    sig_is(Is, Ts, Ds, Ls, Fdb, Sub, State);
sig_is([#b_set{op=make_fun,args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Ls, Fdb, Sub0, State0) ->
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

sig_fun_call(I0, Args, Ts, Ds, Fdb, Sub, State0) ->
    [Fun | CallArgs0] = Args,
    FunType = normalized_type(Fun, Ts),
    Arity = length(CallArgs0),

    case {FunType, Ds} of
        {_, #{ Fun := #b_set{op=make_fun,
                             args=[#b_local{arity=TotalArity}=Callee | Env]} }}
          when TotalArity =:= Arity + length(Env) ->
            %% When a fun is used and defined in the same function, we can make
            %% a direct call since the environment is still available.
            CallArgs = CallArgs0 ++ simplify_args(Env, Ts, Sub),
            I = I0#b_set{args=[Callee | CallArgs]},
            sig_local_call(I, Callee, CallArgs, Ts, Fdb, State0);
        {#t_fun{target={Name,Arity}}, _} ->
            %% When a fun lacks free variables, we can make a direct call even
            %% when we don't know where it was defined.
            Callee = #b_local{name=#b_literal{val=Name},
                              arity=Arity},
            I = I0#b_set{args=[Callee | CallArgs0]},
            sig_local_call(I, Callee, CallArgs0, Ts, Fdb, State0);
        {#t_fun{type=Type}, _} when Type =/= any ->
            {beam_ssa:add_anno(result_type, Type, I0), State0};
        _ ->
            {I0, State0}
    end.

sig_local_call(I0, Callee, Args, Ts, Fdb, State) ->
    ArgTypes = argument_types(Args, Ts),
    I = sig_local_return(I0, Callee, ArgTypes, Fdb),
    {I, sig_update_args(Callee, ArgTypes, State)}.

%% While it's impossible to tell which arguments a fun will be called with
%% (someone could steal it through tracing and call it), we do know its free
%% variables and can update their types as if this were a local call.
sig_make_fun(#b_set{op=make_fun,args=[#b_local{}=Callee | FreeVars]}=I0,
             Ts, Fdb, State) ->
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
    [Id || Id := #func_info{exported=true} <- FuncDb].

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
            Ts = #{V => any || #b_var{}=V <- Args},
            opt_function(Linear0, Args, Id, Ts, FuncDb)
    end;
opt_continue(Linear0, Args, Anno, _FuncDb) ->
    %% Module-level optimization is disabled, pass an empty function database
    %% so we only perform local optimizations.
    Id = get_func_id(Anno),
    Ts = #{V => any || #b_var{}=V <- Args},
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

    Ds = #{Var => FakeCall#b_set{dst=Var} ||
             #b_var{}=Var <- Args},

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
            {Last2, Ls1} = update_successors(Last1, Ts, Ds, Ls0, UsedOnce),

            Last = opt_anno_types(Last2, Ts),

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
       Ts0, Ds0, Ls, Fdb0, Sub, Meta, Acc) ->
    Args = simplify_args(Args0, Ts0, Sub),

    I1 = opt_anno_types(I0#b_set{args=Args}, Ts0),

    {I, Fdb} = opt_fun_call(I1, Args, Ts0, Ds0, Fdb0, Sub, Meta),

    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    opt_is(Is, Ts, Ds, Ls, Fdb, Sub, Meta, [I | Acc]);
opt_is([#b_set{op=make_fun,args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Ls, Fdb0, Sub0, Meta, Acc) ->
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
    end;
opt_anno_types(#b_switch{anno=Anno0,arg=Arg}=I, Ts) ->
    case concrete_type(Arg, Ts) of
        any ->
            I;
        Type ->
            Anno = Anno0#{arg_types => #{0 => Type}},
            I#b_switch{anno=Anno}
    end;
opt_anno_types(I, _Ts) ->
    I.

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
opt_anno_types_1(#b_set{anno=Anno0}=I, [], _Ts, _Index, Acc) ->
    case Anno0 of
        #{ arg_types := Acc } ->
            I;
        #{ arg_types := _ } when Acc =:= #{} ->
            %% One or more arguments have been simplified to literal values.
            Anno = maps:remove(arg_types, Anno0),
            I#b_set{anno=Anno};
        #{} ->
            Anno = Anno0#{ arg_types => Acc },
            I#b_set{anno=Anno}
    end.

%% Only add type annotations when we know we'll make good use of them.
benefits_from_type_anno({bif,_Op}, _Args) ->
    true;
benefits_from_type_anno(bs_create_bin, _Args) ->
    true;
benefits_from_type_anno(bs_match, _Args) ->
    true;
benefits_from_type_anno(is_tagged_tuple, _Args) ->
    true;
benefits_from_type_anno(call, [#b_var{} | _]) ->
    true;
benefits_from_type_anno({float,convert}, _Args) ->
    %% Note: The {float,convert} instruction does not exist when
    %% the main type optimizer pass is run. It is created and
    %% annotated by ssa_opt_float1 in beam_ssa_opt, and can also
    %% be annotated by opt_ranges/1.
    true;
benefits_from_type_anno(get_map_element, _Args) ->
    true;
benefits_from_type_anno(has_map_field, _Args) ->
    true;

%% The types are used to avoid falsely detecting aliasing of
%% non-boxed things.
benefits_from_type_anno(put_list, _Args) ->
    true;
benefits_from_type_anno(put_tuple, _Args) ->
    true;
benefits_from_type_anno(get_tuple_element, _Args) ->
    true;
benefits_from_type_anno(get_hd, _Args) ->
    true;
benefits_from_type_anno(get_tl, _Args) ->
    true;
benefits_from_type_anno(update_record, _Args) ->
    true;
benefits_from_type_anno(_Op, _Args) ->
    false.

opt_fun_call(#b_set{dst=Dst}=I0, [Fun | CallArgs0], Ts, Ds, Fdb, Sub, Meta) ->
    FunType = normalized_type(Fun, Ts),
    Arity = length(CallArgs0),
    case {FunType, Ds} of
        {_, #{ Fun := #b_set{op=make_fun,
                             args=[#b_local{arity=TotalArity}=Callee | Env]} }}
          when TotalArity =:= Arity + length(Env) ->
            %% When a fun is used and defined in the same function, we can make
            %% a direct call since the environment is still available.
            CallArgs = CallArgs0 ++ simplify_args(Env, Ts, Sub),
            I = I0#b_set{args=[Callee | CallArgs]},
            opt_local_call(I, Callee, CallArgs, Dst, Ts, Fdb, Meta);
        {#t_fun{target={Name,Arity}}, _} ->
            %% When a fun lacks free variables, we can make a direct call even
            %% when we don't know where it was defined.
            Callee = #b_local{name=#b_literal{val=Name},
                              arity=Arity},
            I = I0#b_set{args=[Callee | CallArgs0]},
            opt_local_call(I, Callee, CallArgs0, Dst, Ts, Fdb, Meta);
        {#t_fun{type=Type}, _} when Type =/= any ->
            {beam_ssa:add_anno(result_type, Type, I0), Fdb};
        _ ->
            {I0, Fdb}
    end.

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
opt_make_fun(#b_set{op=make_fun,
                    dst=Dst,
                    args=[#b_local{}=Callee | FreeVars]}=I0,
             Ts, Fdb, Meta) ->
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
%%% This sub pass is run once after the main type sub pass
%%% to annotate more instructions with integer ranges.
%%%
%%% The main type sub pass annotates certain instructions with
%%% their types to help the JIT generate better code.
%%%
%%% Example:
%%%
%%%   foo(N0) ->
%%%       N1 = N0 band 3,
%%%       N = N1 + 1,        % N1 is in 0..3
%%%       element(N,
%%%            {zero,one,two,three}).
%%%
%%% The main type pass is able to figure out the range for `N1` but
%%% not for `N`. The reason is that the type pass iterates until it
%%% reaches a fixpoint. To guarantee that it will converge, ranges for
%%% results must only be calculated for operations that retain or
%%% shrink the ranges of their arguments.
%%%
%%% Therefore, to ensure convergence, the main type pass can only
%%% safely calculate ranges for results of operations such as `and`,
%%% `bsr`, and `rem`, but not for operations such as `+`, '-', '*',
%%% and `bsl`.
%%%
%%% This sub pass will start from the types found in the annotations
%%% and propagate them forward through arithmetic instructions within
%%% the same function.
%%%
%%% For the example, this sub pass adds a new annotation for `N`:
%%%
%%%   foo(N0) ->
%%%       N1 = N0 band 3,
%%%       N = N1 + 1,        % N1 is in 0..3
%%%       element(N,         % N is in 1..4
%%%           {zero,one,two,three}).
%%%
%%% With a known range and known tuple size, the JIT is able to remove
%%% all range checks for the `element/2` instruction.
%%%

-spec opt_ranges(Blocks0) -> Blocks when
      Blocks0 :: beam_ssa:block_map(),
      Blocks :: beam_ssa:block_map().

opt_ranges(Blocks) ->
    RPO = beam_ssa:rpo(Blocks),
    Tss = #{0 => #{}, ?EXCEPTION_BLOCK => #{}},
    ranges(RPO, Tss, Blocks).

ranges([L|Ls], Tss0, Blocks0) ->
    #b_blk{is=Is0} = Blk0 = map_get(L, Blocks0),
    Ts0 = map_get(L, Tss0),
    {Is,Ts} = ranges_is(Is0, Ts0, []),
    Blk = Blk0#b_blk{is=Is},
    Blocks = Blocks0#{L := Blk},
    Tss = ranges_successors(beam_ssa:successors(Blk), Ts, Tss0),
    ranges(Ls, Tss, Blocks);
ranges([], _Tss, Blocks) -> Blocks.

ranges_is([#b_set{op=Op,args=Args}=I0|Is], Ts0, Acc) ->
    case benefits_from_type_anno(Op, Args) of
        false ->
            ranges_is(Is, Ts0, [I0|Acc]);
        true ->
            I = update_anno_types(I0, Ts0),
            Ts = ranges_propagate_types(I, Ts0),
            ranges_is(Is, Ts, [I|Acc])
    end;
ranges_is([], Ts, Acc) ->
    {reverse(Acc),Ts}.

ranges_successors([?EXCEPTION_BLOCK|Ls], Ts, Tss) ->
    ranges_successors(Ls, Ts, Tss);
ranges_successors([L|Ls], Ts0, Tss0) ->
    case Tss0 of
        #{L := Ts1} ->
            Ts = join_types(Ts0, Ts1),
            Tss = Tss0#{L := Ts},
            ranges_successors(Ls, Ts0, Tss);
        #{} ->
            Tss = Tss0#{L => Ts0},
            ranges_successors(Ls, Ts0, Tss)
    end;
ranges_successors([], _, Tss) -> Tss.

ranges_propagate_types(#b_set{anno=Anno,op={bif,_}=Op,args=Args,dst=Dst}, Ts) ->
    case Anno of
        #{arg_types := ArgTypes0} ->
            ArgTypes = ranges_get_arg_types(Args, 0, ArgTypes0),
            case beam_call_types:arith_type(Op, ArgTypes) of
                any -> Ts;
                T -> Ts#{Dst => T}
            end;
        #{} ->
            Ts
    end;
ranges_propagate_types(_, Ts) -> Ts.

ranges_get_arg_types([#b_var{}|As], Index, ArgTypes) ->
    case ArgTypes of
        #{Index := Type} ->
            [Type|ranges_get_arg_types(As, Index + 1, ArgTypes)];
        #{} ->
            [any|ranges_get_arg_types(As, Index + 1, ArgTypes)]
    end;
ranges_get_arg_types([#b_literal{val=Value}|As], Index, ArgTypes) ->
    Type = beam_types:make_type_from_value(Value),
    [Type|ranges_get_arg_types(As, Index + 1, ArgTypes)];
ranges_get_arg_types([], _, _) -> [].

update_anno_types(#b_set{anno=Anno,args=Args}=I, Ts) ->
    ArgTypes1 = case Anno of
                    #{arg_types := ArgTypes0} -> ArgTypes0;
                    #{} -> #{}
                end,
    ArgTypes = update_anno_types_1(Args, Ts, 0, ArgTypes1),
    case Anno of
        #{arg_types := ArgTypes} ->
            I;
        #{} when map_size(ArgTypes) =/= 0 ->
            I#b_set{anno=Anno#{arg_types => ArgTypes}};
        #{} ->
            I
    end.

update_anno_types_1([#b_var{}=V|As], Ts, Index, ArgTypes) ->
    T0 = case ArgTypes of
             #{Index := T00} -> T00;
             #{} -> any
         end,
    T1 = case Ts of
             #{V := T11} -> T11;
             #{} -> any
         end,
    case beam_types:meet(T0, T1) of
        any ->
            update_anno_types_1(As, Ts, Index + 1, ArgTypes);
        none ->
            %% This instruction will never be reached. This happens when
            %% compiling code such as the following:
            %%
            %%   f(X) when is_integer(X), 0 =< X, X < 64 ->
            %%        (X = bnot X) + 1.
            %%
            %% The main type optimization sub pass will not find out
            %% that `(X = bnot X)` will never succeed and that the `+`
            %% operator is never executed, but this sub pass will.
            %% This happens very rarely; therefore, don't bother removing
            %% the unreachable instruction.
            update_anno_types_1(As, Ts, Index + 1, ArgTypes);
        T ->
            update_anno_types_1(As, Ts, Index + 1, ArgTypes#{Index => T})
    end;
update_anno_types_1([_|As], Ts, Index, ArgTypes) ->
    update_anno_types_1(As, Ts, Index + 1, ArgTypes);
update_anno_types_1([], _, _, ArgTypes) -> ArgTypes.

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
simplify_terminator(#b_ret{arg=Arg,anno=Anno0}=Ret0, Ts, Ds, Sub) ->
    %% Reducing the result of a call to a literal (fairly common for 'ok')
    %% breaks tail call optimization.
    Ret = case Ds of
              #{ Arg := #b_set{op=call}} -> Ret0;
              #{} -> Ret0#b_ret{arg=simplify_arg(Arg, Ts, Sub)}
          end,
    %% Annotate the terminator with the type it returns, skip the
    %% annotation if nothing is yet known about the variable. The
    %% annotation is used by the alias analysis pass.
    Type = case {Arg, Ts} of
               {#b_literal{},_} -> concrete_type(Arg, Ts);
               {_,#{Arg:=_}} -> concrete_type(Arg, Ts);
               _ -> any
           end,
    case Type of
        any ->
            Ret;
        _ ->
            Anno = Anno0#{ result_type => Type },
            Ret#b_ret{anno=Anno}
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
               'maybe' -> beam_types:make_boolean()
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
simplify(#b_set{op=bs_create_bin=Op,dst=Dst,args=Args0,anno=Anno}=I0,
         Ts0, Ds0, _Ls, Sub) ->
    Args = simplify_args(Args0, Ts0, Sub),

    case Args of
        [#b_literal{val=binary},
         #b_literal{val=[1|_]},
         #b_literal{val=Bitstring}=Lit,
         #b_literal{val=all}] when is_bitstring(Bitstring) ->
            %% If all we're doing is creating a single constant bitstring, we
            %% may as well return it directly.
            Sub#{ Dst => Lit };
        [_|_] ->
            I1 = I0#b_set{args=Args},
            #t_bitstring{size_unit=Unit} = T = type(Op, Args, Anno, Ts0, Ds0),
            I2 = case T of
                    #t_bitstring{appendable=true} ->
                        beam_ssa:add_anno(result_type, T, I1);
                    _ -> I1
                end,
            I = beam_ssa:add_anno(unit, Unit, I2),
            Ts = Ts0#{ Dst => T },
            Ds = Ds0#{ Dst => I },
            {I, Ts, Ds}
    end;
simplify(#b_set{dst=Dst,args=Args0}=I0, Ts0, Ds0, _Ls, Sub) ->
    Args = simplify_args(Args0, Ts0, Sub),
    I1 = beam_ssa:normalize(I0#b_set{args=Args}),
    case simplify(I1, Ts0, Ds0) of
        #b_set{}=I ->
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{ Dst => I },
            {I, Ts, Ds};
        #b_literal{}=Lit ->
            Sub#{ Dst => Lit };
        #b_var{}=Var ->
            Sub#{ Dst => Var }
    end.

simplify(#b_set{op={bif,'band'},args=Args}=I, Ts, Ds) ->
    case normalized_types(Args, Ts) of
        [#t_integer{elements=R},#t_integer{elements={M,M}}] ->
            case beam_bounds:is_masking_redundant(R, M) of
                true ->
                    %% M is mask that will have no effect.
                    hd(Args);
                false ->
                    eval_bif(I, Ts, Ds)
            end;
        [_,_] ->
            eval_bif(I, Ts, Ds)
    end;
simplify(#b_set{op={bif,'and'},args=Args}=I, Ts, Ds) ->
    case is_safe_bool_op(Args, Ts) of
        true ->
            case Args of
                [_,#b_literal{val=false}=Res] -> Res;
                [Res,#b_literal{val=true}] -> Res;
                _ -> eval_bif(I, Ts, Ds)
            end;
        false ->
            I
    end;
simplify(#b_set{op={bif,'or'},args=Args}=I, Ts, Ds) ->
    case is_safe_bool_op(Args, Ts) of
        true ->
            case Args of
                [Res,#b_literal{val=false}] -> Res;
                [_,#b_literal{val=true}=Res] -> Res;
                _ -> eval_bif(I, Ts, Ds)
            end;
        false ->
            I
    end;
simplify(#b_set{op={bif,element},args=[#b_literal{val=Index},Tuple]}=I0, Ts, Ds) ->
    case normalized_type(Tuple, Ts) of
        #t_tuple{size=Size} when is_integer(Index),
                                 1 =< Index,
                                 Index =< Size ->
            I = I0#b_set{op=get_tuple_element,
                         args=[Tuple,#b_literal{val=Index-1}]},
            simplify(I, Ts, Ds);
        _ ->
            eval_bif(I0, Ts, Ds)
    end;
simplify(#b_set{op={bif,hd},args=[List]}=I, Ts, Ds) ->
    case normalized_type(List, Ts) of
        #t_cons{} ->
            I#b_set{op=get_hd};
        _ ->
            eval_bif(I, Ts, Ds)
    end;
simplify(#b_set{op={bif,tl},args=[List]}=I, Ts, Ds) ->
    case normalized_type(List, Ts) of
        #t_cons{} ->
            I#b_set{op=get_tl};
        _ ->
            eval_bif(I, Ts, Ds)
    end;
simplify(#b_set{op={bif,size},args=[Term]}=I, Ts, Ds) ->
    case normalized_type(Term, Ts) of
        #t_tuple{} ->
            simplify(I#b_set{op={bif,tuple_size}}, Ts);
        #t_bitstring{size_unit=U} when U rem 8 =:= 0 ->
            %% If the bitstring is a binary (the size in bits is
            %% evenly divisibly by 8), byte_size/1 gives
            %% the same result as size/1.
            simplify(I#b_set{op={bif,byte_size}}, Ts, Ds);
        _ ->
            eval_bif(I, Ts, Ds)
    end;
simplify(#b_set{op={bif,tuple_size},args=[Term]}=I, Ts, _Ds) ->
    case normalized_type(Term, Ts) of
        #t_tuple{size=Size,exact=true} ->
            #b_literal{val=Size};
        _ ->
            I
    end;
simplify(#b_set{op={bif,is_map_key},args=[Key,Map]}=I, Ts, _Ds) ->
    case normalized_type(Map, Ts) of
        #t_map{} ->
            I#b_set{op=has_map_field,args=[Map,Key]};
        _ ->
            I
    end;
simplify(#b_set{op={bif,Op0},args=[A,B]}=I, Ts, Ds) when Op0 =:= '==';
                                                         Op0 =:= '/=' ->
    EqEq = case {number_type(A, Ts),number_type(B, Ts)} of
               {none,_} ->
                   %% The LHS does not contain any numbers. A strict
                   %% test is always safe.
                   true;
               {_,none} ->
                   %% The RHS does not contain any numbers. A strict
                   %% test is always safe.
                   true;
               {#t_integer{},#t_integer{}} ->
                   %% Both side contain integers but no floats.
                   true;
               {_,_} ->
                   %% Either side can contain a number, substitution is unsafe
                   %% even if both sides are floats as `-0.0 == +0.0`
                   false
           end,
    case EqEq of
        true ->
            Op = case Op0 of
                     '==' -> '=:=';
                     '/=' -> '=/='
                 end,
            simplify(I#b_set{op={bif,Op}}, Ts, Ds);
        false ->
            eval_bif(I, Ts, Ds)
    end;
simplify(#b_set{op={bif,'=:='},args=[Same,Same]}, _Ts, _Ds) ->
    #b_literal{val=true};
simplify(#b_set{op={bif,'=:='},args=[LHS,RHS]}=I, Ts, Ds) ->
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
                    simplify(I#b_set{op={bif,'not'},args=[LHS]}, Ts, Ds);
                {_,_} ->
                    eval_bif(I, Ts, Ds)
            end
    end;
simplify(#b_set{op={bif,is_list},args=[Src]}=I0, Ts, Ds) ->
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
            eval_bif(I0, Ts, Ds)
    end;
simplify(#b_set{op={bif,Op},args=[Term]}=I, Ts, _Ds)
  when Op =:= trunc; Op =:= round; Op =:= ceil; Op =:= floor ->
    case normalized_type(Term, Ts) of
        #t_integer{} -> Term;
        _ -> I
    end;
simplify(#b_set{op={bif,Op},args=Args}=I, Ts, Ds) ->
    Types = normalized_types(Args, Ts),
    case is_float_op(Op, Types) of
        false ->
            eval_bif(I, Ts, Ds);
        true ->
            AnnoArgs = [anno_float_arg(A) || A <- Types],
            eval_bif(beam_ssa:add_anno(float_op, AnnoArgs, I), Ts, Ds)
    end;
simplify(I, Ts, _Ds) ->
    simplify(I, Ts).

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
simplify(#b_set{op=update_tuple,args=[Src | Updates]}=I, Ts) ->
    case {normalized_type(Src, Ts), update_tuple_highest_index(Updates, -1)} of
        {#t_tuple{exact=true,size=Size}, Highest} when Highest =< Size,
                                                       Size < 512 ->
            Args = [#b_literal{val=reuse},
                    #b_literal{val=Size},
                    Src | Updates],
            simplify(I#b_set{op=update_record,args=Args}, Ts);
        {_, _} ->
            I
    end;
simplify(#b_set{op=update_record,args=[_Hint, _Size, #b_literal{val=Tuple0} | Updates]}=I, _Ts)
  when tuple_size(Tuple0) - length(Updates) div 2 < 20 ->
    %% This is an update of a literal tuple. Provided that the number
    %% of elements that are copied from the literal is not
    %% unreasonable, we'll rewrite it to a put_tuple instruction.
    Tuple1 = list_to_tuple([#b_literal{val=E} || E <- tuple_to_list(Tuple0)]),
    Tuple = update_tuple_literal(Updates, Tuple1),
    I#b_set{op=put_tuple,args=tuple_to_list(Tuple)};
simplify(#b_set{op=update_record,args=[Hint0, Size, Src | Updates0]}=I, Ts) ->
    case simplify_update_record(Src, Hint0, Updates0, Ts) of
        {changed, _, []} ->
            Src;
        {changed, Hint, [_|_]=Updates} ->
            I#b_set{op=update_record,args=[Hint, Size, Src | Updates]};
        unchanged ->
            I
    end;
simplify(I, _Ts) ->
    I.

update_tuple_literal([#b_literal{val=Position}, Val | Updates], Tuple0) ->
    Tuple = setelement(Position, Tuple0, Val),
    update_tuple_literal(Updates, Tuple);
update_tuple_literal([], Tuple) ->
    Tuple.

will_succeed(#b_set{args=[Src]}, Ts, Ds, Sub) ->
    case {Ds, Ts} of
        {#{}, #{ Src := none }} ->
            %% Checked operation never returns.
            no;
        {#{ Src := I }, #{}} ->
            %% There can't be any substitution because the instruction
            %% is still there.
            false = is_map_key(Src, Sub),        %Assertion.
            will_succeed_1(I, Src, Ts);
        {#{}, #{}} ->
            %% The checked instruction has been removed and substituted, so we
            %% can assume it always succeeds.
            true = is_map_key(Src, Sub),        %Assertion.
            yes
    end.

will_succeed_1(#b_set{op=bs_get_tail}, _Src, _Ts) ->
    yes;
will_succeed_1(#b_set{op=bs_start_match,args=[_, Arg]}, _Src, Ts) ->
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
                _ -> 'maybe'
            end
    end;

will_succeed_1(#b_set{op={bif,Bif},args=BifArgs}, _Src, Ts) ->
    ArgTypes = concrete_types(BifArgs, Ts),
    beam_call_types:will_succeed(erlang, Bif, ArgTypes);
will_succeed_1(#b_set{op=call,
                      args=[#b_remote{mod=#b_literal{val=Mod},
                                      name=#b_literal{val=Func}} |
                            CallArgs]},
               _Src, Ts) ->
    ArgTypes = concrete_types(CallArgs, Ts),
    beam_call_types:will_succeed(Mod, Func, ArgTypes);

will_succeed_1(#b_set{op=get_hd}, _Src, _Ts) ->
    yes;
will_succeed_1(#b_set{op=get_tl}, _Src, _Ts) ->
    yes;
will_succeed_1(#b_set{op=has_map_field}, _Src, _Ts) ->
    yes;
will_succeed_1(#b_set{op=get_tuple_element}, _Src, _Ts) ->
    yes;
will_succeed_1(#b_set{op=put_tuple}, _Src, _Ts) ->
    yes;
will_succeed_1(#b_set{op=update_tuple,args=[Tuple | Updates]}, _Src, Ts) ->
    TupleType = concrete_type(Tuple, Ts),
    HighestIndex = update_tuple_highest_index(Updates, -1),
    Args = [beam_types:make_integer(HighestIndex), TupleType, any],
    beam_call_types:will_succeed(erlang, setelement, Args);
will_succeed_1(#b_set{op=update_record}, _Src, _Ts) ->
    yes;

will_succeed_1(#b_set{op=bs_create_bin}, _Src, _Ts) ->
    %% Intentionally don't try to determine whether construction will
    %% fail. Construction is unlikely to fail, and if it fails, the
    %% instruction in the runtime system will generate an exception with
    %% better information of what went wrong.
    'maybe';
will_succeed_1(#b_set{op=bs_match,
                      args=[#b_literal{val=Type},_,_,#b_literal{val=Size},_]},
               _Src, _Ts) ->
    if
        is_integer(Size), Size >= 0 ->
            'maybe';
        Type =:= binary, Size =:= all ->
            %% `all` is a legal size for binary segments at the end of
            %% a binary pattern.
            'maybe';
        true ->
            %% Invalid size. Matching will fail.
            no
    end;

%% These operations may fail even though we know their return value on success.
will_succeed_1(#b_set{op=call}, _Src, _Ts) ->
    'maybe';
will_succeed_1(#b_set{op=get_map_element}, _Src, _Ts) ->
    'maybe';
will_succeed_1(#b_set{op=wait_timeout}, _Src, _Ts) ->
    %% It is essential to keep the {succeeded,body} instruction to
    %% ensure that the failure edge, which potentially leads to a
    %% landingpad, is preserved. If the failure edge is removed, a Y
    %% register holding a `try` tag could be reused prematurely.
    'maybe';

will_succeed_1(#b_set{}, _Src, _Ts) ->
    'maybe'.

simplify_update_record(Src, Hint0, Updates, Ts) ->
    case sur_1(Updates, concrete_type(Src, Ts), Ts, Hint0, []) of
        {Hint0, []} ->
            unchanged;
        {Hint, Skipped} ->
            {changed, Hint, sur_skip(Updates, Skipped)}
    end.

sur_1([Index, Arg | Updates], RecordType, Ts, Hint, Skipped) ->
    FieldType = concrete_type(Arg, Ts),
    IndexType = concrete_type(Index, Ts),
    Singleton = beam_types:is_singleton_type(FieldType),
    case beam_call_types:types(erlang, element, [IndexType, RecordType]) of
        {FieldType, _, _} when Singleton ->
            %% We can skip setting fields when we KNOW that they already have
            %% the value we're trying to set.
            sur_1(Updates, RecordType, Ts, Hint, Skipped ++ [Index]);
        {ElementType, _, _} ->
            case beam_types:meet(FieldType, ElementType) of
                none ->
                    %% The element at the index can never have the same value
                    %% as the value we're trying to set. Tell the instruction
                    %% not to bother checking whether it's possible to reuse
                    %% the source.
                    sur_1(Updates, RecordType, Ts,
                          #b_literal{val=copy}, Skipped);
                _ ->
                    sur_1(Updates, RecordType, Ts, Hint, Skipped)
            end
    end;
sur_1([], _RecordType, _Ts, Hint, Skipped) ->
    {Hint, Skipped}.

sur_skip([Index, _Arg | Updates], [Index | Skipped]) ->
    sur_skip(Updates, Skipped);
sur_skip([Index, Arg | Updates], Skipped) ->
    [Index, Arg | sur_skip(Updates, Skipped)];
sur_skip([], []) ->
    [].

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
                           _ -> 'maybe'
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

number_type(V, Ts) ->
    number_type_1(concrete_type(V, Ts)).

number_type_1(any) ->
    #t_number{};
number_type_1(Type) ->
    N = beam_types:meet(Type, #t_number{}),

    List = case beam_types:meet(Type, #t_list{}) of
               #t_cons{type=Head,terminator=Tail} ->
                   beam_types:join(number_type_1(Head), number_type_1(Tail));
               #t_list{type=Head,terminator=Tail} ->
                   beam_types:join(number_type_1(Head), number_type_1(Tail));
               nil ->
                   none;
               none ->
                   none
           end,

    Tup = case beam_types:meet(Type, #t_tuple{}) of
              #t_tuple{size=Size,exact=true,elements=ElemTypes}
                when map_size(ElemTypes) =:= Size ->
                  beam_types:join([number_type_1(ET) ||
                                      ET <- maps:values(ElemTypes)] ++ [none]);
              #t_tuple{} ->
                  #t_number{};
              #t_union{} ->
                  #t_number{};
              none ->
                  none
          end,

    Map = case beam_types:meet(Type, #t_map{}) of
              #t_map{super_value=MapValue} ->
                  %% Starting from OTP 18, map keys are compared using
                  %% `=:=`.  Therefore, we only need to use the values
                  %% in the map.
                  MapValue;
              none ->
                  none
          end,

    Fun = case beam_types:meet(Type, #t_fun{}) of
              #t_fun{} ->
                  %% The environment could contain a number. We don't
                  %% keep track of the environment in #t_fun{}.
                  #t_number{};
              none ->
                  none
          end,

    beam_types:join([N,List,Tup,Map,Fun]).

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

eval_bif(#b_set{op={bif,Bif},args=Args}=I, Ts, Ds) ->
    Arity = length(Args),
    case erl_bifs:is_pure(erlang, Bif, Arity) of
        false ->
            I;
        true ->
            case make_literal_list(Args) of
                none ->
                    eval_bif_1(I, Bif, Ts, Ds);
                LitArgs ->
                    try apply(erlang, Bif, LitArgs) of
                        Val -> #b_literal{val=Val}
                    catch
                        error:_ -> I
                    end

            end
    end.

eval_bif_1(#b_set{args=Args}=I, Op, Ts, Ds) ->
    case concrete_types(Args, Ts) of
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
        [#t_integer{},#t_integer{}] ->
            reassociate(I, Ts, Ds);
        _ ->
            I
    end.

reassociate(#b_set{op={bif,OpB},args=[#b_var{}=V0,#b_literal{val=B0}]}=I, _Ts, Ds)
  when OpB =:= '+'; OpB =:= '-' ->
    case map_get(V0, Ds) of
        #b_set{op={bif,OpA},args=[#b_var{}=V,#b_literal{val=A0}]}
          when OpA =:= '+'; OpA =:= '-' ->
            A = erlang:OpA(A0),
            B = erlang:OpB(B0),
            case A + B of
                Combined when Combined < 0 ->
                    I#b_set{op={bif,'-'},args=[V,#b_literal{val=-Combined}]};
                Combined when Combined >= 0 ->
                    I#b_set{op={bif,'+'},args=[V,#b_literal{val=Combined}]}
            end;
        #b_set{} ->
            I
    end;
reassociate(I, _Ts, _Ds) -> I.

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
                 error ->
                     %% There's more than one possible value, try a weaker
                     %% variant of the inference for case labels.
                     #{ V := Def } = Ds,
                     PosTypes = [{V, FailType} | infer_eq_type(Def, FailType)],

                     case meet_types(PosTypes, Ts) of
                         none -> none;
                         FailTs0 when IsTempVar -> ts_remove_var(V, FailTs0);
                         FailTs0 -> FailTs0
                     end
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
    ArgTypes = concrete_types(Args, Ts),
    case beam_call_types:types(erlang, Bif, ArgTypes) of
        {any, _, _} ->
            case {Bif, Args} of
                {element, [_,#b_literal{val=Tuple}]}
                  when tuple_size(Tuple) > 0 ->
                    join_tuple_elements(Tuple);
                {_, _} ->
                    any
            end;
        {RetType, _, _} ->
            RetType
    end;
type(bs_create_bin, Args, _Anno, Ts, _Ds) ->
    SizeUnit = bs_size_unit(Args, Ts),
    Appendable = case Args of
                     [#b_literal{val=private_append}|_] ->
                         true;
                     [#b_literal{val=append},_,Var|_] ->
                         case argument_type(Var, Ts) of
                             #t_bitstring{appendable=A} -> A;
                             #t_union{other=#t_bitstring{appendable=A}} -> A;
                             _ -> false
                         end;
                     _ ->
                         false
                 end,
    #t_bitstring{size_unit=SizeUnit,appendable=Appendable};
type(bs_extract, [Ctx], _Anno, Ts, Ds) ->
    #b_set{op=bs_match,
           args=[#b_literal{val=Type}, _OrigCtx | Args]} = map_get(Ctx, Ds),
    bs_match_type(Type, Args, Ts);
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
type(bs_match, Args0, _Anno, Ts, _Ds) ->
    [#b_literal{val=Type}, Ctx | Args] = Args0, %Assertion.
    case bs_match_type(Type, Args, Ts) of
        none ->
            none;
        _ ->
            %% Matches advance the current position without testing the tail
            %% unit. We try to retain unit information by taking the GCD of our
            %% current unit and the increments we know the match will advance
            %% by.
            #t_bs_context{tail_unit=CtxUnit} = concrete_type(Ctx, Ts),
            OpUnit = bs_match_stride(Type, Args, Ts),

            #t_bs_context{tail_unit=gcd(OpUnit, CtxUnit)}
    end;
type(bs_get_tail, [Ctx], _Anno, Ts, _Ds) ->
    #t_bs_context{tail_unit=Unit} = concrete_type(Ctx, Ts),
    #t_bitstring{size_unit=Unit};
type(call, [#b_remote{mod=#b_literal{val=Mod},
                      name=#b_literal{val=Name}}|Args], _Anno, Ts, _Ds)
  when is_atom(Mod), is_atom(Name) ->
    ArgTypes = concrete_types(Args, Ts),
    {RetType, _, _} = beam_call_types:types(Mod, Name, ArgTypes),
    RetType;
type(call, [#b_remote{mod=Mod,name=Name} | _Args], _Anno, Ts, _Ds) ->
    %% Remote call with variable Module and/or Function, we can't say much
    %% about it other than that it will crash when either of the two is not an
    %% atom.
    ModType = beam_types:meet(concrete_type(Mod, Ts), #t_atom{}),
    NameType = beam_types:meet(concrete_type(Name, Ts), #t_atom{}),
    case {ModType, NameType} of
        {none, _} -> none;
        {_, none} -> none;
        {_, _} -> any
    end;
type(call, [#b_local{} | _Args], Anno, _Ts, _Ds) ->
    case Anno of
        #{ result_type := Type } -> Type;
        #{} -> any
    end;
type(call, [#b_var{}=Fun | Args], Anno, Ts, _Ds) ->
    FunType = concrete_type(Fun, Ts),
    case {beam_types:meet(FunType, #t_fun{arity=length(Args)}), Anno} of
        {#t_fun{}, #{ result_type := Type }} -> Type;
        {#t_fun{}, #{}} -> any;
        {none, #{}} -> none
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
type(get_map_element, [Map0, Key0], _Anno, Ts, _Ds) ->
    Map = concrete_type(Map0, Ts),
    Key = concrete_type(Key0, Ts),
    %% Note the reversed argument order!
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
            beam_types:get_tuple_element(Index, Es)
    end;
type(has_map_field, [Map0, Key0], _Anno, Ts, _Ds) ->
    Map = concrete_type(Map0, Ts),
    Key = concrete_type(Key0, Ts),
    %% Note the reversed argument order!
    {RetType, _, _} = beam_call_types:types(erlang, is_map_key, [Key, Map]),
    true = none =/= RetType,                    %Assertion.
    RetType;
type(is_nonempty_list, [_], _Anno, _Ts, _Ds) ->
    beam_types:make_boolean();
type(is_tagged_tuple, [_,#b_literal{},#b_literal{}], _Anno, _Ts, _Ds) ->
    beam_types:make_boolean();
type(make_fun, Args, Anno, _Ts, _Ds) ->
    RetType = case Anno of
                  #{ result_type := Type } -> Type;
                  #{} -> any
              end,
    [#b_local{name=#b_literal{val=Name},arity=TotalArity} | Env] = Args,
    Arity = TotalArity - length(Env),
    #t_fun{arity=Arity,target={Name,TotalArity},type=RetType};
type(match_fail, _, _Anno, _Ts, _Ds) ->
    none;
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
type(raw_raise, [Class, _, _], _Anno, Ts, _Ds) ->
    ClassType = concrete_type(Class, Ts),
    case beam_types:meet(ClassType, #t_atom{elements=[error,exit,throw]}) of
        ClassType ->
            %% Unlike erlang:raise/3, the stack argument is always correct as
            %% it's generated by the emulator, so we KNOW that it will raise an
            %% exception when the class is correct.
            none;
        _ ->
            beam_types:make_atom(badarg)
    end;
type(resume, [_, _], _Anno, _Ts, _Ds) ->
    none;
type(update_record, [_Hint, _Size, Src | Updates], _Anno, Ts, _Ds) ->
    update_tuple_type(Updates, Src, Ts);
type(update_tuple, [Src | Updates], _Anno, Ts, _Ds) ->
    update_tuple_type(Updates, Src, Ts);
type(wait_timeout, [#b_literal{val=infinity}], _Anno, _Ts, _Ds) ->
    %% Waits forever, never reaching the 'after' block.
    beam_types:make_atom(false);
type(bs_init_writable, [_Size], _, _, _) ->
    beam_types:make_type_from_value(<<>>);
type(_, _, _, _, _) ->
    any.

update_tuple_type([_|_]=Updates0, Src, Ts) ->
    Updates = update_tuple_type_1(Updates0, Ts),
    beam_types:update_tuple(concrete_type(Src, Ts), Updates).

update_tuple_type_1([#b_literal{val=Index}, Value | Updates], Ts) ->
    [{Index, concrete_type(Value, Ts)} | update_tuple_type_1(Updates, Ts)];
update_tuple_type_1([], _Ts) ->
    [].

update_tuple_highest_index([#b_literal{val=Index}, _Value | Updates], Acc) ->
    update_tuple_highest_index(Updates, max(Index, Acc));
update_tuple_highest_index([], Acc) ->
    true = Acc >= 1,                            %Assertion.
    Acc.

join_tuple_elements(Tuple) ->
    join_tuple_elements(tuple_size(Tuple), Tuple, none).

join_tuple_elements(0, _Tuple, Type) ->
    Type;
join_tuple_elements(I, Tuple, Type0) ->
    Type1 = beam_types:make_type_from_value(element(I, Tuple)),
    Type = beam_types:join(Type0, Type1),
    join_tuple_elements(I - 1, Tuple, Type).

put_map_type(Map, Ss, Ts) ->
    pmt_1(Ss, Ts, concrete_type(Map, Ts)).

pmt_1([Key0, Value0 | Ss], Ts, Acc0) ->
    Key = concrete_type(Key0, Ts),
    Value = concrete_type(Value0, Ts),
    {Acc, _, _} = beam_call_types:types(maps, put, [Key, Value, Acc0]),
    pmt_1(Ss, Ts, Acc);
pmt_1([], _Ts, Acc) ->
    Acc.

bs_size_unit(Args, Ts) ->
    bs_size_unit(Args, Ts, 0, 256).

bs_size_unit([#b_literal{val=Type},#b_literal{val=[U1|_]},Value,SizeTerm|Args],
             Ts, FixedSize, Unit) ->
    case {Type,Value,SizeTerm} of
        {_,#b_literal{val=Bs},#b_literal{val=all}} when is_bitstring(Bs) ->
            %% Add element of known size.
            Size = bit_size(Bs) + FixedSize,
            bs_size_unit(Args, Ts, Size, Unit);
        {_,_,#b_literal{val=all}} ->
            case concrete_type(Value, Ts) of
                #t_bitstring{size_unit=U2} ->
                    %% Adding a bitstring.
                    %% TODO: Can U1 be anything but 1?
                    U = max(U1, U2),
                    bs_size_unit(Args, Ts, FixedSize, gcd(U, Unit));
                _ ->
                    %% Adding something which isn't a bitstring
                    bs_size_unit(Args, Ts, FixedSize, gcd(U1, Unit))
            end;
        {utf8,_,_} ->
            %% Add at least 8 bits, maybe more.
            bs_size_unit(Args, Ts, 8 + FixedSize, gcd(8, Unit));
        {utf16,_,_} ->
            %% Add at least 16 bits, maybe more.
            bs_size_unit(Args, Ts, 16 + FixedSize, gcd(16, Unit));
        {utf32,_,_} ->
            %% Compared to utf8 and utf16 this is a fixed size.
            bs_size_unit(Args, Ts, 32 + FixedSize, Unit);
        {_,_,_} ->
            case concrete_type(SizeTerm, Ts) of
                #t_integer{elements={Size1, Size1}}
                  when is_integer(Size1), is_integer(U1), Size1 >= 0 ->
                    EffectiveSize = Size1 * U1,
                    %% Adding a fixed size element
                    bs_size_unit(Args, Ts, EffectiveSize + FixedSize, Unit);
                _ when is_integer(U1) ->
                    %% Add element with known unit.
                    bs_size_unit(Args, Ts, FixedSize, gcd(U1, Unit));
                _ ->
                    %% Add element without known size or unit.
                    bs_size_unit(Args, Ts, FixedSize, gcd(1, Unit))
            end
    end;
bs_size_unit([], _Ts, FixedSize, Unit) ->
    gcd(FixedSize, Unit).

%% We seldom know how far a match operation may advance, but we can often tell
%% which increment it will advance by.
bs_match_stride(_, [_,Size,#b_literal{val=Unit}], Ts) ->
    case concrete_type(Size, Ts) of
        #t_integer{elements={Sz, Sz}} when is_integer(Sz) ->
            Sz * Unit;
        _ ->
            Unit
    end;
bs_match_stride(string, [#b_literal{val=String}], _) ->
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

bs_match_type(binary, Args, _Ts) ->
    [_,_,#b_literal{val=U}] = Args,
    #t_bitstring{size_unit=U};
bs_match_type(float, _Args, _Ts) ->
    #t_float{};
bs_match_type(integer, Args, Ts) ->
    [#b_literal{val=Flags},Size,#b_literal{val=Unit}] = Args,
    case beam_types:meet(concrete_type(Size, Ts), #t_integer{}) of
        #t_integer{elements=Bounds} ->
            case beam_bounds:bounds('*', Bounds, {Unit, Unit}) of
                {_, MaxBits} when is_integer(MaxBits),
                                  MaxBits >= 1,
                                  MaxBits =< 64 ->
                    case member(unsigned, Flags) of
                        true ->
                            Max = (1 bsl MaxBits) - 1,
                            beam_types:make_integer(0, Max);
                        false ->
                            Max = (1 bsl (MaxBits - 1)) - 1,
                            Min = -(Max + 1),
                            beam_types:make_integer(Min, Max)
                    end;
                {_, 0} ->
                    beam_types:make_integer(0);
                _ ->
                    case member(unsigned, Flags) of
                        true -> #t_integer{elements={0,'+inf'}};
                        false -> #t_integer{}
                    end
            end;
        none ->
            none
    end;

bs_match_type(utf8, _Args, _Ts) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(utf16, _Args, _Ts) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(utf32, _Args, _Ts) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(string, _Args, _Ts) ->
    %% Cannot actually be extracted, but we'll return 'any' to signal that the
    %% associated `bs_match` may succeed.
    any.

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
    {SuccTs0, FailTs0} = infer_types_br_1(V, Ts, Ds),

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

infer_types_br_1(V, Ts, Ds) ->
    #{V:=#b_set{op=Op0,args=Args}} = Ds,

    case inv_relop(Op0) of
        none ->
            %% Not a relational operator.
            {PosTypes, NegTypes} = infer_type(Op0, Args, Ts, Ds),
            SuccTs = meet_types(PosTypes, Ts),
            FailTs = subtract_types(NegTypes, Ts),
            {SuccTs, FailTs};
        InvOp ->
            %% This is a relational operator.
            {bif,Op} = Op0,

            %% Infer the types for both sides of operator succeeding.
            Types = concrete_types(Args, Ts),
            {TruePos, TrueNeg} = infer_relop(Op, Args, Types, Ds),
            TrueTypes = maybe
                           TrueTypes0 = #{} ?= meet_types(TruePos, Ts),
                           subtract_types(TrueNeg, TrueTypes0)
                        else
                            none -> none
                        end,

            %% Infer the types for both sides of operator failing.
            {FalsePos, FalseNeg} = infer_relop(InvOp, Args, Types, Ds),
            FalseTypes = maybe
                            FalseTypes0 = #{} ?= meet_types(FalsePos, Ts),
                            subtract_types(FalseNeg, FalseTypes0)
                         else
                             none -> none
                         end,

            {TrueTypes, FalseTypes}
    end.

infer_relop('=:=', [LHS,RHS],
            [#t_bitstring{appendable=LHSApp}=LType0,
             #t_bitstring{appendable=RHSApp}=RType0], Ds)
  when LHSApp ; RHSApp ->
    %% Bit strings are special in that nothing about their
    %% appendable-status can be deduced from a comparison. The only
    %% information gained is the size_unit. The appendable status is
    %% unchanged by the comparison.
    %%
    %% In order to avoid narrowing the types with regard to
    %% appendable-status, deduce the types for the case when neither
    %% LHS or RHS are appendable, then restore the appendable-status.
    {[{LHS,LType},{RHS,RType} | EqTypes], []} =
        infer_relop('=:=', [LHS,RHS],
                    [LType0#t_bitstring{appendable=false},
                     RType0#t_bitstring{appendable=false}], Ds),
    {[{LHS,LType#t_bitstring{appendable=LHSApp}},
      {RHS,RType#t_bitstring{appendable=RHSApp}} | EqTypes], []};
infer_relop('=:=', [LHS,RHS], [LType,RType], Ds) ->
    EqTypes = infer_eq_type(map_get(LHS, Ds), RType),

    %% As an example, assume that L1 is known to be 'list', and L2 is
    %% known to be 'cons'. Then if 'L1 =:= L2' evaluates to 'true', it
    %% can be inferred that L1 is 'cons' (the meet of 'cons' and
    %% 'list').
    Type = beam_types:meet(LType, RType),
    {[{LHS,Type},{RHS,Type}] ++ EqTypes, []};
infer_relop('=/=', [LHS,RHS], [LType,RType], Ds) ->
    NeTypes = infer_ne_type(map_get(LHS, Ds), RType),

    %% We must be careful with types inferred from '=/='.
    %%
    %% For example, if we have L =/= [a], we must not subtract 'cons'
    %% from the type of L. In general, subtraction is only safe if
    %% the type being subtracted is singled-valued, for example if it
    %% is [] or the the atom 'true'.
    %%
    %% Note that we subtract the left-hand type from the right-hand
    %% value and vice versa. We must not subtract the meet of the two
    %% as it may be too specific. See beam_type_SUITE:type_subtraction/1
    %% for details.
    {[{V,beam_types:subtract(ThisType, OtherType)} ||
         {V, ThisType, OtherType} <- [{RHS, RType, LType}, {LHS, LType, RType}],
         beam_types:is_singleton_type(OtherType)], NeTypes};
infer_relop(Op, Args, Types, _Ds) ->
    {infer_relop(Op, Args, Types), []}.

infer_relop(Op, [Arg1,Arg2], Types0) ->
    case infer_relop(Op, Types0) of
        any ->
            %% Both operands lacked ranges.
            [];
        {NewType1,NewType2} ->
            [{Arg1,NewType1},{Arg2,NewType2}]
    end.

infer_relop(Op, [#t_integer{elements=R1},
                 #t_integer{elements=R2}]) ->
    case beam_bounds:infer_relop_types(Op, R1, R2) of
        {NewR1,NewR2} ->
            {#t_integer{elements=NewR1},
             #t_integer{elements=NewR2}};
        none ->
            {none, none};
        any ->
            any
    end;
infer_relop(Op0, [Type1,Type2]) ->
    Op = case Op0 of
             '<' -> '=<';
             '>' -> '>=';
             _ -> Op0
         end,
    case {infer_get_range(Type1),infer_get_range(Type2)} of
        {unknown,unknown} ->
            any;
        {unknown,_}=R ->
            infer_relop_any(Op, R);
        {_,unknown}=R ->
            infer_relop_any(Op, R);
        {R1,R2} ->
            %% Both operands are numeric types.
            case beam_bounds:infer_relop_types(Op, R1, R2) of
                {NewR1,NewR2} ->
                    {#t_number{elements=NewR1},
                     #t_number{elements=NewR2}};
                none ->
                    {none, none};
                any ->
                    any
            end
    end.

infer_relop_any('=<', {unknown,any}) ->
    %% Since numbers are smaller than any other terms, the LHS must be
    %% a number if operator '=<' evaluates to true.
    {#t_number{}, any};
infer_relop_any('=<', {unknown,{_,Max}}) ->
    {make_number({'-inf',Max}), any};
infer_relop_any('>=', {any,unknown}) ->
    {any, #t_number{}};
infer_relop_any('>=', {{_,Max},unknown}) ->
    {any, make_number({'-inf',Max})};
infer_relop_any('>=', {unknown,{Min,_}}) when is_integer(Min) ->
    %% LHS can be any term, including number, but we can figure out
    %% the minimal possible number.
    N = #t_number{elements={'-inf',Min}},
    {beam_types:subtract(any, N), any};
infer_relop_any('=<', {{Min,_},unknown}) when is_integer(Min) ->
    N = #t_number{elements={'-inf',Min}},
    {any, beam_types:subtract(any, N)};
infer_relop_any(_Op, _R) ->
    any.

make_number({'-inf','+inf'}) ->
    #t_number{};
make_number({_,_}=R) ->
    #t_number{elements=R}.

inv_relop({bif,Op}) -> inv_relop_1(Op);
inv_relop(_) -> none.

inv_relop_1('<') -> '>=';
inv_relop_1('=<') -> '>';
inv_relop_1('>=') -> '<';
inv_relop_1('>') ->  '=<';
inv_relop_1('=:=') -> '=/=';
inv_relop_1('=/=') -> '=:=';
inv_relop_1(_) -> none.

infer_get_range(#t_integer{elements=R}) -> R;
infer_get_range(#t_number{elements=R}) -> R;
infer_get_range(_) -> unknown.

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
    Args = [V,Lit],
    Types = concrete_types(Args, Ts0),
    {PosTypes, NegTypes} = infer_relop('=:=', Args, Types, Ds),
    Ts = maybe
            Ts1 = #{} ?= meet_types(PosTypes, Ts0),
            subtract_types(NegTypes, Ts1)
         else
            none -> none
         end,
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
infer_type({bif,is_function}, [#b_var{}=Arg, Arity], _Ts, _Ds) ->
    case Arity of
        #b_literal{val=V} when is_integer(V), V >= 0, V =< ?MAX_FUNC_ARGS ->
            T = {Arg, #t_fun{arity=V}},
            {[T], [T]};
        _ ->
            %% We cannot subtract the function type when the arity is unknown:
            %% `Arg` may still be a function if the arity is outside the
            %% allowed range.
            T = {Arg, #t_fun{}},
            {[T], []}
    end;
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
    T = {Arg, #t_number{}},
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
infer_type({bif,'and'}, [#b_var{}=LHS,#b_var{}=RHS], Ts, Ds) ->
    %% When this BIF yields true, we know that both `LHS` and `RHS` are 'true'
    %% and should infer accordingly, lest we break later optimizations that
    %% rewrite this BIF to plain control flow.
    %%
    %% Note that we can't do anything for the 'false' case as either (or both)
    %% of the arguments could be false, so we must ignore the negations.
    #{ LHS := #b_set{op=LHSOp,args=LHSArgs},
       RHS := #b_set{op=RHSOp,args=RHSArgs} } = Ds,

    {LHSPos, _} = infer_and_type(LHSOp, LHSArgs, Ts, Ds),
    {RHSPos, _} = infer_and_type(RHSOp, RHSArgs, Ts, Ds),

    True = beam_types:make_atom(true),
    {[{LHS, True}, {RHS, True}] ++ LHSPos ++ RHSPos, []};
infer_type(_Op, _Args, _Ts, _Ds) ->
    {[], []}.

infer_success_type({bif,Op}, Args, Ts, _Ds) ->
    ArgTypes = concrete_types(Args, Ts),

    {_, PosTypes0, CanSubtract} = beam_call_types:types(erlang, Op, ArgTypes),
    PosTypes = zip(Args, PosTypes0),

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

infer_eq_type(#b_set{op={bif,tuple_size},args=[#b_var{}=Tuple]},
              #t_integer{elements={Size,Size}}) ->
    [{Tuple,#t_tuple{exact=true,size=Size}}];
infer_eq_type(#b_set{op=get_tuple_element,
                     args=[#b_var{}=Tuple,#b_literal{val=N}]},
              ElementType) ->
    Index = N + 1,
    case beam_types:set_tuple_element(Index, ElementType, #{}) of
        #{ Index := _ }=Es ->
            [{Tuple,#t_tuple{size=Index,elements=Es}}];
        #{} ->
            %% Index was above the element limit; subtraction is not safe.
            []
    end;
infer_eq_type(_, _) ->
    [].

infer_ne_type(#b_set{op={bif,tuple_size},args=[#b_var{}=Tuple]},
              #t_integer{elements={Size,Size}}) ->
    [{Tuple,#t_tuple{exact=true,size=Size}}];
infer_ne_type(#b_set{op=get_tuple_element,
                     args=[#b_var{}=Tuple,#b_literal{val=N}]},
              ElementType) ->
    Index = N + 1,
    case {beam_types:is_singleton_type(ElementType),
          beam_types:set_tuple_element(Index, ElementType, #{})} of
        {true, #{ Index := _ }=Es} ->
            [{Tuple,#t_tuple{size=Index,elements=Es}}];
        {_, #{}} ->
            %% Subtraction is not safe: either we had a non-singleton element
            %% type (see inference for `=/=`), or the element index was out of
            %% range.
            []
    end;
infer_ne_type(_, _) ->
    [].

infer_and_type(Op, Args, Ts, Ds) ->
    case inv_relop(Op) of
        none ->
            infer_type(Op, Args, Ts, Ds);
        _InvOp ->
            {bif,RelOp} = Op,
            infer_relop(RelOp, Args, concrete_types(Args, Ts), Ds)
    end.

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

meet_types([{#b_literal{}=Lit, T0} | Vs], Ts) ->
    T1 = concrete_type(Lit, Ts),
    case beam_types:meet(T0, T1) of
        none -> none;
        _ -> meet_types(Vs, Ts)
    end;
meet_types([{#b_var{}=V, T0} | Vs], Ts) ->
    T1 = concrete_type(V, Ts),
    case beam_types:meet(T0, T1) of
        none -> none;
        T1 -> meet_types(Vs, Ts);
        T -> meet_types(Vs, Ts#{ V := T })
    end;
meet_types([], Ts) ->
    Ts.

subtract_types([{#b_literal{}=Lit, T0} | Vs], Ts) ->
    T1 = concrete_type(Lit, Ts),
    case beam_types:subtract(T0, T1) of
        none -> none;
        _ -> subtract_types(Vs, Ts)
    end;
subtract_types([{#b_var{}=V, T0}|Vs], Ts) ->
    T1 = concrete_type(V, Ts),
    case beam_types:subtract(T1, T0) of
        none -> none;
        T1 -> subtract_types(Vs, Ts);
        T -> subtract_types(Vs, Ts#{V := T})
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
