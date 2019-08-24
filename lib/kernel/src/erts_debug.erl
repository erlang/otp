%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
-module(erts_debug).

%% Low-level debugging support. EXPERIMENTAL!

-export([size/1,df/1,df/2,df/3,dis_to_file/2,ic/1]).

%% This module contains the following *experimental* BIFs:
%%   disassemble/1
%%   breakpoint/2
%%   same/2
%%   flat_size/1

%%% BIFs

-export([breakpoint/2, disassemble/1, display/1, dist_ext_to_term/2,
         flat_size/1, get_internal_state/1, instructions/0,
         interpreter_size/0,
         map_info/1, same/2, set_internal_state/2,
         size_shared/1, copy_shared/1, dirty_cpu/2, dirty_io/2, dirty/3,
         lcnt_control/1, lcnt_control/2, lcnt_collect/0, lcnt_clear/0,
         lc_graph/0, lc_graph_to_dot/2, lc_graph_merge/2,
         alloc_blocks_size/1]).

-spec breakpoint(MFA, Flag) -> non_neg_integer() when
      MFA :: {Module :: module(),
              Function :: atom(),
              Arity :: arity() | '_'},
      Flag :: boolean().

breakpoint(_, _) ->
    erlang:nif_error(undef).

-spec disassemble(What) -> false | undef | Result when
      What :: MFA | Address,
      Result :: {Address, Code, MFA},
      MFA :: mfa(),
      Address :: non_neg_integer(),
      Code :: binary().

disassemble(_) ->
    erlang:nif_error(undef).

-spec display(Term) -> string() when
      Term :: term().

display(_) ->
    erlang:nif_error(undef).

-spec dist_ext_to_term(Tuple, Binary) -> term() when
      Tuple :: tuple(),
      Binary :: binary().

dist_ext_to_term(_, _) ->
    erlang:nif_error(undef).

-spec flat_size(Term) -> non_neg_integer() when
      Term :: term().

flat_size(_) ->
    erlang:nif_error(undef).

-spec size_shared(Term) -> non_neg_integer() when
      Term :: term().

size_shared(_) ->
    erlang:nif_error(undef).

-spec copy_shared(Term) -> term() when
      Term :: term().

copy_shared(_) ->
    erlang:nif_error(undef).

-spec get_internal_state(W) -> term() when
      W :: reds_left | node_and_dist_references | monitoring_nodes
         | next_pid | 'DbTable_words' | check_io_debug
         | process_info_args | processes | processes_bif_info
         | max_atom_out_cache_index | nbalance | available_internal_state
         | force_heap_frags | memory
         | {process_status, pid()}
         | {link_list, pid() | port() | node()}
         | {monitor_list, pid() | node()}
         | {channel_number, non_neg_integer()}
         | {have_pending_exit, pid() | port() | atom()}
         | {binary_info, binary()}
         | {term_to_binary_no_funs, term()}
         | {dist_port, port()}
         | {atom_out_cache_index, atom()}
         | {fake_scheduler_bindings,
            default_bind | spread | processor_spread | thread_spread
            | thread_no_node_processor_spread | no_node_processor_spread
            | no_node_thread_spread | no_spread | unbound}
         | {reader_groups_map, non_neg_integer()}.

get_internal_state(_) ->
    erlang:nif_error(undef).

-spec instructions() -> [string()].

instructions() ->
    erlang:nif_error(undef).

-spec interpreter_size() -> pos_integer().

interpreter_size() ->
    erlang:nif_error(undef).

-spec ic(F) -> Result when
      F :: function(),
      Result :: term().

ic(F) when is_function(F) ->
    Is0 = erlang:system_info(instruction_counts),
    R   = F(),
    Is1 = erlang:system_info(instruction_counts),
    Is  = lists:keysort(2,[{I,C1 - C0}||{{I,C1},{I,C0}} <- lists:zip(Is1,Is0)]),
    _   = [io:format("~12w ~w~n", [C,I])||{I,C}<-Is],
    io:format("Total: ~w~n",[lists:sum([C||{_I,C}<-Is])]),
    R.

-spec lcnt_control
    (copy_save, boolean()) -> ok;
    (mask, list(atom())) -> ok.

lcnt_control(_Option, _Value) ->
    erlang:nif_error(undef).

-spec lcnt_control
    (copy_save) -> boolean();
    (mask) -> list(atom()).

lcnt_control(_Option) ->
    erlang:nif_error(undef).

-type lcnt_lock_info() :: {atom(), term(), atom(), term()}.

-spec lcnt_collect() ->
    list({duration, {non_neg_integer(), non_neg_integer()}} |
         {locks, list(lcnt_lock_info())}).

lcnt_collect() ->
    erlang:nif_error(undef).

-spec lcnt_clear() -> ok.
lcnt_clear() ->
    erlang:nif_error(undef).

-spec same(Term1, Term2) -> boolean() when
      Term1 :: term(),
      Term2 :: term().

same(_, _) ->
    erlang:nif_error(undef).

-spec set_internal_state(available_internal_state, boolean()) -> boolean();
                           (reds_left, non_neg_integer()) -> true;
                           (block, non_neg_integer()) -> true;
                           (sleep, non_neg_integer()) -> true;
                           (block_scheduler, non_neg_integer()) -> true;
                           (next_pid, non_neg_integer()) -> false | integer();
                           (force_gc, pid() | atom()) -> boolean();
                           (send_fake_exit_signal, {pid() | port(), pid(), term()}) -> dead | message | unaffected | exit;
                           (colliding_names, {atom(), non_neg_integer()}) ->
                                   [atom()];
                           (binary_loop_limit, default) -> -1;
                           (binary_loop_limit, non_neg_integer()) -> non_neg_integer();
                           (re_loop_limit, default) -> -1;
                           (re_loop_limit, non_neg_integer()) -> non_neg_integer();
                           (unicode_loop_limit, default) -> -1;
                           (unicode_loop_limit, non_neg_integer()) -> non_neg_integer();
                           (hipe_test_reschedule_suspend, term()) -> nil();
                           (hipe_test_reschedule_resume, pid() | port()) -> boolean();
                           (test_long_gc_sleep, non_neg_integer()) -> true;
                           (kill_dist_connection, port()) -> boolean();
                           (not_running_optimization, boolean()) -> boolean();
                           (wait, deallocations) -> ok.

set_internal_state(_, _) ->
    erlang:nif_error(undef).

-spec dirty_cpu(Term1, Term2) -> term() when
      Term1 :: term(),
      Term2 :: term().

dirty_cpu(_, _) ->
    erlang:nif_error(undef).

-spec dirty_io(Term1, Term2) -> term() when
      Term1 :: term(),
      Term2 :: term().

dirty_io(_, _) ->
    erlang:nif_error(undef).

-spec dirty(Term1, Term2, Term3) -> term() when
      Term1 :: term(),
      Term2 :: term(),
      Term3 :: term().

dirty(_, _, _) ->
    erlang:nif_error(undef).

%%% End of BIFs

%% size(Term)
%%  Returns the size of Term in actual heap words. Shared subterms are
%%  counted once.  Example: If A = [a,b], B =[A,A] then size(B) returns 8,
%%  while flat_size(B) returns 12.

-spec size(term()) -> non_neg_integer().

-record(s, {seen, maps}).

size(Term) ->
    {Sum,_} = size(Term, #s{seen=gb_trees:empty(),maps=[]}, 0),
    Sum.

size([H|T]=Term, Seen0, Sum0) ->
    case remember_term(Term, Seen0) of
	seen -> {Sum0,Seen0};
	Seen1 ->
	    {Sum,Seen} = size(H, Seen1, Sum0+2),
	    size(T, Seen, Sum)
    end;
size(Tuple, Seen0, Sum0) when is_tuple(Tuple) ->
    case remember_term(Tuple, Seen0) of
	seen -> {Sum0,Seen0};
	Seen ->
	    Sum = Sum0 + 1 + tuple_size(Tuple),
	    tuple_size(1, tuple_size(Tuple), Tuple, Seen, Sum)
    end;
size(Map, Seen0, Sum) when is_map(Map) ->
    case remember_term(Map, Seen0) of
	seen -> {Sum,Seen0};
	Seen -> map_size(Map, Seen, Sum)
    end;
size(Fun, Seen0, Sum) when is_function(Fun) ->
    case remember_term(Fun, Seen0) of
	seen -> {Sum,Seen0};
	Seen -> fun_size(Fun, Seen, Sum)
    end;
size(Term, Seen0, Sum) ->
    case erts_debug:flat_size(Term) of
	0 -> {Sum,Seen0};
	Sz ->
	    case remember_term(Term, Seen0) of
		seen -> {Sum,Seen0};
		Seen -> {Sum+Sz,Seen}
	    end
    end.

tuple_size(I, Sz, _, Seen, Sum) when I > Sz ->
    {Sum,Seen};
tuple_size(I, Sz, Tuple, Seen0, Sum0) ->
    {Sum,Seen} = size(element(I, Tuple), Seen0, Sum0),
    tuple_size(I+1, Sz, Tuple, Seen, Sum).

map_size(Map,Seen0,Sum0) ->
    %% Danger:
    %% The internal nodes from erts_internal:map_hashmap_children/1
    %% is not allowed to leak anywhere. They are only allowed in
    %% containers (cons cells and tuples, not maps), in gc and
    %% in erts_debug:same/2
    case erts_internal:term_type(Map) of
        flatmap ->
            Kt = erts_internal:map_to_tuple_keys(Map),
            Vs = maps:values(Map),
            {Sum1,Seen1} = size(Kt,Seen0,Sum0),
            fold_size(Vs,Seen1,Sum1+length(Vs)+3);
        hashmap ->
            Cs = erts_internal:map_hashmap_children(Map),
            fold_size(Cs,Seen0,Sum0+length(Cs)+2);
        hashmap_node ->
            Cs = erts_internal:map_hashmap_children(Map),
            fold_size(Cs,Seen0,Sum0+length(Cs)+1)
    end.

fun_size(Fun, Seen, Sum) ->
    case erlang:fun_info(Fun, type) of
	{type,external} ->
	    {Sum + erts_debug:flat_size(Fun),Seen};
	{type,local} ->
	    Sz = erts_debug:flat_size(fun() -> ok end),
	    {env,Env} = erlang:fun_info(Fun, env),
	    fold_size(Env, Seen, Sum+Sz+length(Env))
    end.

fold_size([H|T], Seen0, Sum0) ->
    {Sum,Seen} = size(H, Seen0, Sum0),
    fold_size(T, Seen, Sum);
fold_size([], Seen, Sum) -> {Sum,Seen}.

remember_term(Term, #s{maps=Ms}=S) when is_map(Term) ->
    case is_term_seen(Term, Ms) of
        false -> S#s{maps=[Term|Ms]};
        true  -> seen
    end;
remember_term(Term, #s{seen=T}=S) ->
    case gb_trees:lookup(Term,T) of
	none -> S#s{seen=gb_trees:insert(Term,[Term],T)};
	{value,Terms} ->
	    case is_term_seen(Term, Terms) of
		false -> S#s{seen=gb_trees:update(Term,[Term|Terms],T)};
		true  -> seen
	    end
    end.

-spec is_term_seen(term(), [term()]) -> boolean().

is_term_seen(Term, [H|T]) ->
    case erts_debug:same(Term, H) of
	true -> true;
	false -> is_term_seen(Term, T)
    end;
is_term_seen(_, []) -> false.

%% df(Mod)              -- Disassemble Mod to file Mod.dis.
%% df(Mod, Func)        -- Disassemble Mod:Func/Any to file Mod_Func.dis.
%% df(Mod, Func, Arity) -- Disassemble Mod:Func/Arity to file Mod_Func_Arity.dis.

-type df_ret() :: 'ok' | {'error', {'badopen', module()}} | {'undef', module()}.

-spec df(module()) -> df_ret().

df(Mod) when is_atom(Mod) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Name = lists:concat([Mod, ".dis"]),
	    Fs = [{Mod,Func,Arity} || {Func,Arity} <- Fs0],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

-spec df(module(), atom()) -> df_ret().

df(Mod, Func) when is_atom(Mod), is_atom(Func) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Name = lists:concat([Mod, "_", Func, ".dis"]),
	    Fs = [{Mod,Func1,Arity} || {Func1,Arity} <- Fs0, Func1 =:= Func],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

-spec df(module(), atom(), arity()) -> df_ret().

df(Mod, Func, Arity) when is_atom(Mod), is_atom(Func) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Name = lists:concat([Mod, "_", Func, "_", Arity, ".dis"]),
	    Fs = [{Mod,Func1,Arity1} || {Func1,Arity1} <- Fs0,
					Func1 =:= Func, Arity1 =:= Arity],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

-spec dis_to_file(module(), file:filename()) -> df_ret().

dis_to_file(Mod, Name) when is_atom(Mod) ->
    try Mod:module_info(functions) of
	Fs0 when is_list(Fs0) ->
	    Fs = [{Mod,Func,Arity} || {Func,Arity} <- Fs0],
	    dff(Name, Fs)
    catch _:_ -> {undef,Mod}
    end.

dff(Name, Fs) ->
    case file:open(Name, [write,raw,delayed_write]) of
	{ok,F} ->
	    try
		dff_1(F, Fs)
	    after
		_ = file:close(F)
	    end;
	{error,Reason} ->
	    {error,{badopen,Reason}}
    end.

dff_1(File, Fs) ->
    lists:foreach(fun(Mfa) ->
                          disassemble_function(File, Mfa),
                          file:write(File, "\n")
                  end, Fs).

disassemble_function(File, {_,_,_}=MFA) ->
    cont_dis(File, erts_debug:disassemble(MFA), MFA).

cont_dis(_, false, _) -> ok;
cont_dis(File, {Addr,Str,MFA}, MFA) ->
    ok = file:write(File, Str),
    cont_dis(File, erts_debug:disassemble(Addr), MFA);
cont_dis(_, {_,_,_}, _) -> ok.

-spec map_info(Map) -> list() when
      Map :: map().

map_info(_) ->
    erlang:nif_error(undef).

%% Create file "lc_graph.<pid>" with all actual lock dependencies
%% recorded so far by the VM.
%% Needs debug VM or --enable-lock-checking config, returns 'notsup' otherwise.
lc_graph() ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:get_internal_state(lc_graph).

%% Convert "lc_graph.<pid>" file to https://www.graphviz.org dot format.
lc_graph_to_dot(OutFile, InFile) ->
    {ok, [LL0]} = file:consult(InFile),

    [{"NO LOCK",0} | LL] = LL0,
    Map = maps:from_list([{Id, Name} || {Name, Id, _, _} <- LL]),

    case file:open(OutFile, [exclusive]) of
        {ok, Out} ->
            ok = file:write(Out, "digraph G {\n"),

            [dot_print_lock(Out, Lck, Map) || Lck <- LL],

            ok = file:write(Out, "}\n"),
            ok = file:close(Out);

        {error,eexist} ->
            {"File already exists", OutFile}
    end.

dot_print_lock(Out, {_Name, Id, Lst, _}, Map) ->
    [dot_print_edge(Out, From, Id, Map) || From <- Lst],
    ok.

dot_print_edge(_, 0, _, _) ->
    ignore; % "NO LOCK"
dot_print_edge(Out, From, To, Map) ->
    io:format(Out, "~p -> ~p;\n", [maps:get(From,Map), maps:get(To,Map)]).


%% Merge several "lc_graph" files into one file.
lc_graph_merge(OutFile, InFiles) ->
    LLs = lists:map(fun(InFile) ->
                            {ok, [LL]} = file:consult(InFile),
                            LL
                    end,
                    InFiles),

    Res = lists:foldl(fun(A, B) -> lcg_merge(A, B) end,
                      hd(LLs),
                      tl(LLs)),
    case file:open(OutFile, [exclusive]) of
        {ok, Out} ->
            try
                lcg_print(Out, Res)
            after
                file:close(Out)
            end,
            ok;
        {error, eexist} ->
            {"File already exists", OutFile}
    end.

lcg_merge(A, B) ->
    lists:zipwith(fun(LA, LB) -> lcg_merge_locks(LA, LB) end,
                  A, B).

lcg_merge_locks(L, L) ->
    L;
lcg_merge_locks({Name, Id, DA, IA}, {Name, Id, DB, IB}) ->
    Direct = lists:umerge(DA, DB),
    Indirect = lists:umerge(IA, IB),
    {Name, Id, Direct, Indirect -- Direct}.


lcg_print(Out, LL) ->
    io:format(Out, "[", []),
    lcg_print_locks(Out, LL),
    io:format(Out, "].\n", []),
    ok.

lcg_print_locks(Out, [{_,_}=NoLock | Rest]) ->
    io:format(Out, "~p,\n", [NoLock]),
    lcg_print_locks(Out, Rest);
lcg_print_locks(Out, [LastLock]) ->
    io:format(Out, "~w", [LastLock]);
lcg_print_locks(Out, [Lock | Rest]) ->
    io:format(Out, "~w,\n", [Lock]),
    lcg_print_locks(Out, Rest).


%% Returns the amount of memory allocated by the given allocator type.
-spec alloc_blocks_size(Type) -> non_neg_integer() | undefined when
      Type :: atom().

alloc_blocks_size(Type) ->
    Allocs = erlang:system_info(alloc_util_allocators),
    Sizes = erlang:system_info({allocator_sizes, Allocs}),
    alloc_blocks_size_1(Sizes, Type, 0).

alloc_blocks_size_1([], _Type, 0) ->
    undefined;
alloc_blocks_size_1([{_Type, false} | Rest], Type, Acc) ->
    alloc_blocks_size_1(Rest, Type, Acc);
alloc_blocks_size_1([{Type, Instances} | Rest], Type, Acc0) ->
    F = fun ({instance, _, L}, Acc) ->
                MBCSPool = case lists:keyfind(mbcs_pool, 1, L) of
                               {_, Pool} -> Pool;
                               false -> []
                           end,
                {_,MBCS} = lists:keyfind(mbcs, 1, L),
                {_,SBCS} = lists:keyfind(sbcs, 1, L),
                Acc +
                    sum_block_sizes(MBCSPool) +
                    sum_block_sizes(MBCS) +
                    sum_block_sizes(SBCS)
        end,
    alloc_blocks_size_1(Rest, Type, lists:foldl(F, Acc0, Instances));
alloc_blocks_size_1([{_Type, Instances} | Rest], Type, Acc0) ->
    F = fun ({instance, _, L}, Acc) ->
                Acc + sum_foreign_sizes(Type, L)
        end,
    alloc_blocks_size_1(Rest, Type, lists:foldl(F, Acc0, Instances));
alloc_blocks_size_1([], _Type, Acc) ->
    Acc.

sum_foreign_sizes(Type, L) ->
    case lists:keyfind(mbcs_pool, 1, L) of
        {_,Pool} ->
            {_,ForeignBlocks} = lists:keyfind(foreign_blocks, 1, Pool),
            case lists:keyfind(Type, 1, ForeignBlocks) of
                {_,TypeSizes} -> sum_block_sizes(TypeSizes);
                false -> 0
            end;
        _ ->
            0
    end.

sum_block_sizes(Blocks) ->
    lists:foldl(
      fun({blocks_size, Sz,_,_}, Sz0) -> Sz0+Sz;
         ({blocks_size, Sz}, Sz0) -> Sz0+Sz;
         (_, Sz) -> Sz
      end, 0, Blocks).
