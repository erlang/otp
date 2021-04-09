%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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
-module(queue_prop).

-compile(export_all).

-proptest(eqc).
-proptest([triq, proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC, true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_new() ->
    [] =:= queue:to_list(queue:new()).

prop_is_queue() ->
    ?FORALL(
	{IsQueue, Q},
	oneof([
	    {true, queue()},
	    {false, non_queue()}
	]),
	begin
	    IsQueue =:= queue:is_queue(Q)
	end
    ).

prop_list_conversion() ->
    ?FORALL(
	List,
	list(),
	begin
	    Queue = queue:from_list(List),
	    queue:is_queue(Queue) andalso
	    List =:= queue:to_list(Queue)
	end
    ).

prop_all() ->
    ?FORALL(
	{L, Q},
	oneof([list_queue(atom()), list_queue(term())]),
	begin
	    lists:all(fun is_atom/1, L) =:= queue:all(fun is_atom/1, Q)
	end
    ).

prop_any() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    lists:any(fun is_atom/1, L) =:= queue:any(fun is_atom/1, Q)
	end
    ).

prop_cons() ->
    common_in_r_cons(cons).

prop_daeh() ->
    common_get_r_last_daeh(daeh).

prop_delete() ->
    ?FORALL(
	{X, {L, Q}},
	{term(), list_queue()},
	begin
	    R1 = if
		L =:= [] ->
		    true;
		true ->
		    {_, Y} = hd(lists:sort([{rand:uniform(), I} || I <- L])),
		    equal(lists:delete(Y, L), queue:delete(Y, Q))
	    end,
	    R2 = equal(lists:delete(X, L), queue:delete(X, Q)),

	    R1 andalso R2
	end
    ).

prop_delete_r() ->
    ?FORALL(
	{X, {L, Q}},
	{term(), list_queue()},
	begin
	    R1 = if
		L =:= [] ->
		    true;
		true ->
		    {_, Y} = hd(lists:sort([{rand:uniform(), I} || I <- L])),
		    equal(lists:reverse(lists:delete(Y, lists:reverse(L))), queue:delete_r(Y, Q))
	    end,
	    R2 = equal(lists:reverse(lists:delete(X, lists:reverse(L))), queue:delete_r(X, Q)),

	    R1 andalso R2
	end
    ).

prop_delete_with() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    Q1 = queue:delete_with(fun is_atom/1, Q),
	    L1 = case lists:search(fun is_atom/1, L) of
		false ->
		    L;
		{value, V} ->
		    lists:delete(V, L)
	    end,
	    equal(L1, Q1)
	end
    ).

prop_delete_with_r() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    Q1 = queue:delete_with_r(fun is_atom/1, Q),
	    L1 = lists:reverse(L),
	    L2 = case lists:search(fun is_atom/1, L1) of
		false ->
		    L;
		{value, V} ->
		    lists:reverse(lists:delete(V, L1))
	    end,
	    equal(L2, Q1)
	end
    ).

prop_drop() ->
    common_drop_tail(drop).

prop_drop_r() ->
    common_drop_r_init_liat(drop_r).

prop_filter() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    Q1 = queue:filter(
		fun
		    (I) when is_atom(I) -> true;
		    (I) when is_integer(I) -> [I * 2];
		    (I) when is_float(I) -> [{I, I}];
		    (I) when is_tuple(I) -> [I, I];
		    (_) -> false
		end,
		Q
	    ),
	    L1 = lists:foldr(
		fun
		    (I, Acc) when is_atom(I) -> [I|Acc];
		    (I, Acc) when is_integer(I) -> [I * 2|Acc];
		    (I, Acc) when is_float(I) -> [{I, I}|Acc];
		    (I, Acc) when is_tuple(I) -> [I, I|Acc];
		    (_, Acc) -> Acc
		end,
		[],
		L
	    ),
	    equal(L1, Q1)
	end
    ).

prop_filtermap() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    F = fun
		(I) when is_atom(I) ->
		    true;
		(I) when is_integer(I) ->
		    {true, {I, I}};
		(_) ->
		    false
	    end,
	    Q1 = queue:filtermap(F, Q),
	    L1 = lists:filtermap(F, L),
	    equal(L1, Q1)
	end
    ).

prop_fold() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    % order-independent fold
	    F1 = fun
		(I, Acc) when is_number(I) -> Acc + 2 * I;
		(_, Acc) -> Acc
	    end,
	    RQ1 = queue:fold(F1, 0, Q),
	    RL1 = lists:foldl(F1, 0, L),

	    % order-dependent fold
	    F2 = fun
		(I, Acc) -> [{I, I}|Acc]
	    end,
	    RQ2 = queue:fold(F2, [], Q),
	    RL2 = lists:foldl(F2, [], L),

	    RQ1 =:= RL1 andalso
	    RQ2 =:= RL2
	end
    ).

prop_get() ->
    common_get_head(get).

prop_get_r() ->
    common_get_r_last_daeh(get_r).

prop_head() ->
    common_get_head(head).

prop_in() ->
    ?FORALL(
	L,
	list(),
	begin
	    Q = lists:foldl(
		fun (I, Acc) ->
		    queue:in(I, Acc)
		end,
		queue:new(),
		L
	    ),
	    equal(L, Q)
	end
    ).

prop_in_r() ->
    common_in_r_cons(in_r).

prop_init() ->
    common_drop_r_init_liat(init).

prop_is_empty() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    (length(L) =:= 0) =:= queue:is_empty(Q)
	end
    ).

prop_join() ->
    ?FORALL(
	{{L1, Q1}, {L2, Q2}},
	{list_queue(), list_queue()},
	begin
	    equal(L1 ++ L2, queue:join(Q1, Q2))
	end
    ).

prop_last() ->
    common_get_r_last_daeh(last).

prop_len() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    length(L) =:= queue:len(Q)
	end
    ).

prop_liat() ->
    common_drop_r_init_liat(liat).

prop_member() ->
    ?FORALL(
        {X, {L, Q}},
	{term(), list_queue()},
	begin
	    % all members of L are members of Q
	    lists:all(
		fun (I) ->
		    queue:member(I, Q)
		end,
		L
	    )
	    andalso
	    % all members of Q are members of L
	    lists:all(
		fun (I) ->
		    lists:member(I, L)
		end,
		queue:to_list(Q)
	    )
	    andalso
	    % if X is a member of L, it is also a member of Q,
	    % and if X is not a member of L, it is also not a
	    % member of Q
	    lists:member(X, L) =:= queue:member(X, Q)
	end
    ).

prop_out() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    case queue:out(Q) of
		{{value, I}, Q1} ->
		    I =:= hd(L) andalso
		    equal(tl(L), Q1);
		{empty, Q1} ->
		    L =:= [] andalso
		    equal(L, Q1)
	    end
	end
    ).

prop_out_r() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    case queue:out_r(Q) of
		{{value, I}, Q1} ->
		    L1 = lists:reverse(L),
		    I =:= hd(L1) andalso
		    equal(lists:reverse(tl(L1)), Q1);
		{empty, Q1} ->
		    L =:= [] andalso
		    equal(L, Q1)
	    end
	end
    ).

prop_peek() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    case queue:peek(Q) of
		{value, I} ->
		    I =:= hd(L);
		empty ->
		    L =:= []
	    end
	end
    ).

prop_peek_r() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    case queue:peek_r(Q) of
		{value, I} ->
		    I =:= lists:last(L);
		empty ->
		    L =:= []
	    end
	end
    ).

prop_reverse() ->
    ?FORALL(
	{L, Q},
	list_queue(),
	begin
	    equal(lists:reverse(L), queue:reverse(Q))
	end
    ).

prop_snoc() ->
    ?FORALL(
	L,
	list(),
	begin
	    Q = lists:foldl(
		fun (I, Acc) ->
		    queue:snoc(Acc, I)
		end,
		queue:new(),
		L
	    ),
	    equal(L, Q)
	end
    ).

prop_split() ->
    ?FORALL(
	{N, {L, Q}},
	{non_neg_integer(), list_queue()},
	begin
	    N1 = N rem (length(L) + 1),
	    {Q1, Q2} = queue:split(N1, Q),
	    {L1, L2} = lists:split(N1, L),
	    equal(L1, Q1) andalso
	    equal(L2, Q2)
	end
    ).

prop_tail() ->
    common_drop_tail(tail).

% Test sequences of insert and retrieval operations
prop_ops() ->
    ?FORALL(
	{Ops, {L, Q}},
	{
	    list(
		oneof([{cons, term()},
		       daeh,
		       drop,
		       drop_r,
		       get,
		       get_r,
		       head,
		       {in, term()},
		       {in_r, term()},
		       init,
		       liat,
		       last,
		       out,
		       out_r,
		       peek,
		       peek_r,
		       {snoc, term()},
		       tail])
	    ),
	    list_queue()
	},
	begin
	    {Res, RQ, RL} = lists:foldl(
		fun
		    (_, Acc = {false, _, _}) ->
			Acc;
		    % queue:in/2
		    ({in, I}, {true, Q1, L1}) ->
			{true, queue:in(I, Q1), L1 ++ [I]};
		    % queue:snoc/2
		    ({snoc, I}, {true, Q1, L1}) ->
			{true, queue:snoc(Q1, I), L1 ++ [I]};
		    % queue:in_r/2, queue:cons/2
		    ({Op, I}, {true, Q1, L1}) when Op =:= in_r; Op =:= cons ->
			{true, queue:Op(I, Q1), [I|L1]};
		    % queue:out/1, queue:out_r/1
		    (Op, {true, Q1, L1}) when Op =:= out; Op =:= out_r ->
			case queue:Op(Q1) of
			    {empty, _} ->
				{L1 =:= [], Q1, L1};
			    {{value, I}, Q2} when Op =:= out ->
				{I =:= hd(L1), Q2, tl(L1)};
			    {{value, I}, Q2} when Op =:= out_r ->
				{I =:= lists:last(L1), Q2, lists:droplast(L1)}
			end;
		    % queue:peek/1, queue:peek_r/1
		    (Op, {true, Q1, L1}) when Op =:= peek; Op =:= peek_r ->
			case queue:Op(Q1) of
			    empty ->
				{L1 =:= [], Q1, L1};
			    {value, I} when Op =:= peek ->
				{I =:= hd(L1), Q1, L1};
			    {value, I} when Op =:= peek_r ->
				{I =:= lists:last(L1), Q1, L1}
			end;
		    % queue:get/1, queue:head/1
		    (Op, {true, Q1, L1}) when Op =:= get; Op =:= head ->
			try queue:Op(Q1) of
			    I ->
				{I =:= hd(L1), Q1, L1}
			catch error:empty ->
			    {L1 =:= [], Q1, L1}
			end;
		    % queue:get_r/1, queue:daeh/1, queue:last/1
		    (Op, {true, Q1, L1}) when Op =:= get_r; Op =:= daeh; Op =:= last ->
			try queue:Op(Q1) of
			    I ->
				{I =:= lists:last(L1), Q1, L1}
			catch error:empty ->
			    {L1 =:= [], Q1, L1}
			end;
		    % queue:init/1, queue:drop_r/1, queue:liat/1
		    (Op, {true, Q1, L1}) when Op =:= init; Op =:= drop_r; Op =:= liat ->
			try queue:Op(Q1) of
			    Q2 ->
				L2 = lists:droplast(L1),
				{equal(L2, Q2), Q2, L2}
			catch error:empty ->
			    {L1 =:= [], Q1, L1}
			end;
		    % queue:drop/1, queue:tail/1
		    (Op, {true, Q1, L1}) when Op =:= drop; Op =:= tail ->
			try queue:Op(Q1) of
			    Q2 ->
				L2 = tl(L1),
				{equal(L2, Q2), Q2, L2}
			catch error:empty ->
			    {L1 =:= [], Q1, L1}
			end
		end,
		{true, Q, L},
		Ops
	    ),
	    Res andalso equal(RL, RQ)
	end
    ).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Property helpers %%%
%%%%%%%%%%%%%%%%%%%%%%%%

% get/1 and head/1 have the same semantics
common_get_head(Fn) ->
    ?FORALL(
	{L, Q},
	list_queue(),
	try
	    queue:Fn(Q)
	of E ->
	    E =:= hd(L)
	catch error:empty ->
	    L =:= []
	end
    ).

% get_r/1, last/1 and daeh/1 have the same semantics
common_get_r_last_daeh(Fn) ->
    ?FORALL(
	{L, Q},
	list_queue(),
	try
	    queue:Fn(Q)
	of E ->
	    E =:= lists:last(L)
	catch error:empty ->
	    L =:= []
	end
    ).

% drop_r/1, init/1 and liat/1 have the same semantics 
common_drop_r_init_liat(Fn) ->
    ?FORALL(
	{L, Q},
	list_queue(),
	try
	    queue:Fn(Q)
	of Q1 ->
	    equal(lists:droplast(L), Q1)
	catch error:empty ->
	    L =:= []
	end
    ).

% drop/1 and tail/1 have the same semantics
common_drop_tail(Fn) ->
    ?FORALL(
	{L, Q},
	list_queue(),
	try
	    queue:Fn(Q)
	of Q1 ->
	    equal(tl(L), Q1)
	catch error:empty ->
	    L =:= []
	end
    ).

% in_r/2 and cons/2 have the same semantics
common_in_r_cons(Fn) ->
    ?FORALL(
	L,
	list(),
	begin
	    Q = lists:foldl(
		fun (I, Acc) ->
		    queue:Fn(I, Acc)
		end,
		queue:new(),
		L
	    ),
	    equal(lists:reverse(L), Q)
	end
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

list_queue() ->
    list_queue(term()).

list_queue(Type) ->
    ?LET(List, list(Type), {List, queue:from_list(List)}).

queue() ->
    queue(term()).

queue(Type) ->
    ?LET(List, list(Type), queue:from_list(List)).

non_queue() ->
    ?SUCHTHAT(
	T,
	term(),
	not(
	    is_tuple(T) andalso
	    tuple_size(T) =:= 2 andalso
	    is_list(element(1, T)) andalso
	    is_list(element(2, T))
	)
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

% Check equality of lists and/or queues,
% ie that they contain the same items in
% the same order.
equal(L1, L2) when is_list(L1), is_list(L2) ->
    L1 =:= L2;
equal(Q, L) when is_list(L) ->
    equal(queue:to_list(Q), L);
equal(L, Q) ->
    equal(L, queue:to_list(Q)).
