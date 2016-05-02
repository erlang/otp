%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-export([size/1,df/1,df/2,df/3,ic/1]).

%% This module contains the following *experimental* BIFs:
%%   disassemble/1
%%   breakpoint/2
%%   same/2
%%   flat_size/1

%%% BIFs

-export([breakpoint/2, disassemble/1, display/1, dist_ext_to_term/2,
         dump_monitors/1, dump_links/1, flat_size/1,
         get_internal_state/1, instructions/0, lock_counters/1,
         map_info/1, same/2, set_internal_state/2,
         size_shared/1, copy_shared/1]).

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

-spec dump_monitors(Id) -> true when
      Id :: pid() | atom().

dump_monitors(_) ->
    erlang:nif_error(undef).

-spec dump_links(Id) -> true when
      Id :: pid() | port() | atom().

dump_links(_) ->
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

-spec lock_counters(info) -> term();
                      (clear) -> ok;
                      ({copy_save, boolean()}) -> boolean();
                      ({process_locks, boolean()}) -> boolean().

lock_counters(_) ->
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

dff(File, Fs) when is_pid(File), is_list(Fs) ->
    lists:foreach(fun(Mfa) ->
			  disassemble_function(File, Mfa),
			  io:nl(File)
		  end, Fs);
dff(Name, Fs) when is_list(Name) ->
    case file:open(Name, [write]) of
	{ok,F} ->
	    try
		dff(F, Fs)
	    after
		_ = file:close(F)
	    end;
	{error,Reason} ->
	    {error,{badopen,Reason}}
    end.

disassemble_function(File, {_,_,_}=MFA) ->
    cont_dis(File, erts_debug:disassemble(MFA), MFA).

cont_dis(_, false, _) -> ok;
cont_dis(File, {Addr,Str,MFA}, MFA) ->
    io:put_chars(File, binary_to_list(Str)),
    cont_dis(File, erts_debug:disassemble(Addr), MFA);
cont_dis(_, {_,_,_}, _) -> ok.

-spec map_info(Map) -> list() when
      Map :: map().

map_info(_) ->
    erlang:nif_error(undef).
