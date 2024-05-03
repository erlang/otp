%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2024. All Rights Reserved.
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

-include_lib("common_test/include/ct_property_test.hrl").

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
        ct_proper_ext:safe_list(),
        begin
            Queue = queue:from_list(List),
            queue:is_queue(Queue) andalso
            List =:= queue:to_list(Queue)
        end
    ).

prop_from_list_invalid() ->
    ?FORALL(
        NonList,
        ?SUCHTHAT(T, ct_proper_ext:safe_any(), not is_list(T)),
        expect_badarg(fun queue:from_list/1, [NonList])
    ).

prop_to_list_invalid() ->
    common_invalid(fun queue:to_list/1).

prop_all() ->
    ?FORALL(
        {L, Q},
        oneof([list_queue(ct_proper_ext:safe_atom()),
               list_queue(ct_proper_ext:safe_any())]),
        begin
            lists:all(fun is_atom/1, L) =:= queue:all(fun is_atom/1, Q)
        end
    ).

prop_all_invalid() ->
    common_invalid_pred(fun queue:all/2).

prop_any() ->
    ?FORALL(
        {L, Q},
        list_queue(),
        begin
            lists:any(fun is_atom/1, L) =:= queue:any(fun is_atom/1, Q)
        end
    ).

prop_any_invalid() ->
    common_invalid_pred(fun queue:any/2).

prop_cons() ->
    common_in_r_cons(fun queue:cons/2).

prop_cons_invalid() ->
    common_invalid_term(fun queue:cons/2).

prop_daeh() ->
    common_get_r_last_daeh(fun queue:daeh/1).

prop_daeh_invalid() ->
    common_invalid(fun queue:daeh/1).

prop_delete() ->
    ?FORALL(
        {X, {L, Q}},
        {ct_proper_ext:safe_any(), list_queue()},
        begin
            R1 = if
                L =:= [] ->
                    true;
                true ->
                    Y = lists:nth(rand:uniform(length(L)), L),
                    equal(lists:delete(Y, L), queue:delete(Y, Q))
            end,
            R2 = equal(lists:delete(X, L), queue:delete(X, Q)),

            R1 andalso R2
        end
    ).

prop_delete_invalid() ->
    common_invalid_term(fun queue:delete/2).

prop_delete_r() ->
    ?FORALL(
        {X, {L, Q}},
        {ct_proper_ext:safe_any(), list_queue()},
        begin
            R1 = if
                L =:= [] ->
                    true;
                true ->
                    Y = lists:nth(rand:uniform(length(L)), L),
                    equal(lists:reverse(lists:delete(Y, lists:reverse(L))), queue:delete_r(Y, Q))
            end,
            R2 = equal(lists:reverse(lists:delete(X, lists:reverse(L))), queue:delete_r(X, Q)),

            R1 andalso R2
        end
    ).

prop_delete_r_invalid() ->
    common_invalid_term(fun queue:delete_r/2).

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

prop_delete_with_invalid() ->
    common_invalid_pred(fun queue:delete_with/2).

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

prop_delete_with_r_invalid() ->
    common_invalid_pred(fun queue:delete_with_r/2).

prop_drop() ->
    common_drop_tail(fun queue:drop/1).

prop_drop_invalid() ->
    common_invalid(fun queue:drop/1).

prop_drop_r() ->
    common_drop_r_init_liat(fun queue:drop_r/1).

prop_drop_r_invalid() ->
    common_invalid(fun queue:drop_r/1).

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

prop_filter_invalid() ->
    common_invalid_pred(fun queue:filter/2).

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

prop_filtermap_invalid() ->
    common_invalid_pred(fun queue:filtermap/2).

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

prop_fold_invalid() ->
    ?FORALL(
        {Q, Fn},
        oneof([{non_queue(), fun erlang:'+'/2}, {queue(), non_fun(2)}, {non_queue(), non_fun(2)}]),
        expect_badarg(fun queue:fold/3, [Fn, 0, Q])
    ).

prop_get() ->
    common_get_head(fun queue:get/1).

prop_get_invalid() ->
    common_invalid(fun queue:get/1).

prop_get_r() ->
    common_get_r_last_daeh(fun queue:get_r/1).

prop_get_r_invalid() ->
    common_invalid(fun queue:get_r/1).

prop_head() ->
    common_get_head(fun queue:head/1).

prop_head_invalid() ->
    common_invalid(fun queue:head/1).

prop_in() ->
    ?FORALL(
        L,
        ct_proper_ext:safe_list(),
        begin
            Q = lists:foldl(
                fun(I, Acc) ->
                    queue:in(I, Acc)
                end,
                queue:new(),
                L
            ),
            equal(L, Q)
        end
    ).

prop_in_invalid() ->
    common_invalid_term(fun queue:in/2).

prop_in_r() ->
    common_in_r_cons(fun queue:in_r/2).

prop_in_r_invalid() ->
    common_invalid_term(fun queue:in_r/2).

prop_init() ->
    common_drop_r_init_liat(fun queue:init/1).

prop_init_invalid() ->
    common_invalid(fun queue:init/1).

prop_is_empty() ->
    ?FORALL(
        {L, Q},
        list_queue(),
        begin
            (length(L) =:= 0) =:= queue:is_empty(Q)
        end
    ).

prop_is_empty_invalid() ->
    common_invalid(fun queue:is_empty/1).

prop_join() ->
    ?FORALL(
        {{L1, Q1}, {L2, Q2}},
        {list_queue(), list_queue()},
        begin
            equal(L1 ++ L2, queue:join(Q1, Q2))
        end
    ).

prop_join_invalid() ->
    ?FORALL(
        {Q1, Q2},
        oneof([{non_queue(), queue()}, {queue(), non_queue()}, {non_queue(), non_queue()}]),
        expect_badarg(fun queue:join/2, [Q1, Q2])
    ).

prop_last() ->
    common_get_r_last_daeh(fun queue:last/1).

prop_last_invalid() ->
    common_invalid(fun queue:last/1).

prop_len() ->
    ?FORALL(
        {L, Q},
        list_queue(),
        begin
            length(L) =:= queue:len(Q)
        end
    ).

prop_len_invalid() ->
    common_invalid(fun queue:len/1).

prop_liat() ->
    common_drop_r_init_liat(fun queue:liat/1).

prop_liat_invalid() ->
    common_invalid(fun queue:liat/1).

prop_member() ->
    ?FORALL(
        {X, {L, Q}},
        {ct_proper_ext:safe_any(), list_queue()},
        begin
            % all members of L are members of Q
            lists:all(
                fun(I) ->
                    queue:member(I, Q)
                end,
                L
            )
            andalso
            % all members of Q are members of L
            lists:all(
                fun(I) ->
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

prop_member_invalid() ->
    common_invalid_term(fun queue:member/2).

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

prop_out_invalid() ->
    common_invalid(fun queue:out/1).

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

prop_out_r_invalid() ->
    common_invalid(fun queue:out_r/1).

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

prop_peek_invalid() ->
    common_invalid(fun queue:peek/1).

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

prop_peek_r_invalid() ->
    common_invalid(fun queue:peek_r/1).

prop_reverse() ->
    ?FORALL(
        {L, Q},
        list_queue(),
        begin
            equal(lists:reverse(L), queue:reverse(Q))
        end
    ).

prop_reverse_invalid() ->
    common_invalid(fun queue:reverse/1).

prop_snoc() ->
    ?FORALL(
        L,
        ct_proper_ext:safe_list(),
        begin
            Q = lists:foldl(
                fun(I, Acc) ->
                    queue:snoc(Acc, I)
                end,
                queue:new(),
                L
            ),
            equal(L, Q)
        end
    ).

prop_snoc_invalid() ->
    ?FORALL(
        {I, NonQueue},
        {ct_proper_ext:safe_any(), non_queue()},
        expect_badarg(fun queue:snoc/2, [NonQueue, I])
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

prop_split_invalid() ->
    ?FORALL(
        {Q, N},
        oneof(
            [
                {non_queue(), 0},
                ?SUCHTHAT(
                    {Q1, N1},
                    {queue(), ct_proper_ext:safe_any()},
                    not(is_integer(N1) andalso N1>=0 andalso N1=<queue:len(Q1))
                )
            ]
        ),
        expect_badarg(fun queue:split/2, [N, Q])
    ).

prop_tail() ->
    common_drop_tail(fun queue:tail/1).

prop_tail_invalid() ->
    common_invalid(fun queue:tail/1).

% Test sequences of insert and retrieval operations
prop_ops() ->
    ?FORALL(
        {Ops, {L, Q}},
        {
            list(
                oneof([{cons, ct_proper_ext:safe_any()},
                       daeh,
                       drop,
                       drop_r,
                       get,
                       get_r,
                       head,
                       {in, ct_proper_ext:safe_any()},
                       {in_r, ct_proper_ext:safe_any()},
                       init,
                       liat,
                       last,
                       out,
                       out_r,
                       peek,
                       peek_r,
                       {snoc, ct_proper_ext:safe_any()},
                       tail])
            ),
            list_queue()
        },
        do_ops(Ops, L, Q)
    ).

%% Executes the given sequence of queue operations on a model (list)
%% and a queue and compares the returned items (if any) as well as
%% the model and queue.
do_ops([], L, Q) ->
    equal(L, Q);
do_ops([Op|Ops], L0, Q0) ->
    {LItem, L1} = do_op_list(Op, L0),
    {QItem, Q1} = do_op_queue(Op, Q0),

    LItem =:= QItem andalso
    equal(L1, Q1) andalso
    do_ops(Ops, L1, Q1).

list_op_map() ->
    % Inserts item at the rear; affects only the model list, returns no value
    InsertRearOp = fun(I, L) -> {undefined, L ++ [I]} end,
    % Inserts item at the front; affects only the model list, returns no value
    InsertFrontOp = fun(I, L) -> {undefined, [I|L]} end,

    % Helper for functions that should return empty when the model list is empty
    MaybeEmpty = fun(F) -> fun([]) -> {empty, []}; (L) -> F(L) end end,

    % For functions that both affect the model list and return a value
    TakeOp = fun(VF, QF) -> fun(L) -> {{value, VF(L)}, QF(L)} end end,

    % For functions that do not affect the model list but return a value
    ValueOp = fun(F) -> fun(L) -> {{value, F(L)}, L} end end,

    % For functions that affect the model list and do not return a value
    QueueOp = fun(F) -> fun(L) -> {undefined, F(L)} end end,

    #{
        in => InsertRearOp,
        in_r => InsertFrontOp,
        cons => InsertFrontOp,
        snoc => InsertRearOp,
        out => MaybeEmpty(TakeOp(fun erlang:hd/1, fun erlang:tl/1)),
        out_r => MaybeEmpty(TakeOp(fun lists:last/1, fun lists:droplast/1)),
        peek => MaybeEmpty(ValueOp(fun erlang:hd/1)),
        peek_r => MaybeEmpty(ValueOp(fun lists:last/1)),
        get => MaybeEmpty(ValueOp(fun erlang:hd/1)),
        get_r => MaybeEmpty(ValueOp(fun lists:last/1)),
        head => MaybeEmpty(ValueOp(fun erlang:hd/1)),
        daeh => MaybeEmpty(ValueOp(fun lists:last/1)),
        last => MaybeEmpty(ValueOp(fun lists:last/1)),
        init => MaybeEmpty(QueueOp(fun lists:droplast/1)),
        drop => MaybeEmpty(QueueOp(fun erlang:tl/1)),
        drop_r => MaybeEmpty(QueueOp(fun lists:droplast/1)),
        tail => MaybeEmpty(QueueOp(fun erlang:tl/1)),
        liat => MaybeEmpty(QueueOp(fun lists:droplast/1))
    }.

do_op_list({Op, I}, L) ->
    F = maps:get(Op, list_op_map()),
    F(I, L);
do_op_list(Op, L) ->
    F = maps:get(Op, list_op_map()),
    F(L).

queue_op_map() ->
    % Helper for swapping arguments
    SwapArgs = fun(F) -> fun(X, Y) -> F(Y, X) end end,

    % For functions that affect the queue but do not return a value 
    QueueOp1 = fun(F) -> fun(Q) -> {undefined, F(Q)} end end,
    QueueOp2 = fun(F) -> fun(I, Q) -> {undefined, F(I, Q)} end end,

    % For functions that return a value wrapped in a tuple
    WrappedReturn = fun(F) -> fun(Q) -> {F(Q), Q} end end,

    % For functions that return an unwrapped value
    PlainReturn = fun(F) -> fun(Q) -> {{value, F(Q)}, Q} end end,

    % Helper for functions that raise an error when used on
    % an empty queue
    CatchEmpty = fun(F) ->
        fun(Q) ->
            try F(Q) catch error:empty -> {empty, Q} end
        end
    end,

    #{
        in => QueueOp2(fun queue:in/2),
        in_r => QueueOp2(fun queue:in_r/2),
        cons => QueueOp2(fun queue:cons/2),
        snoc => QueueOp2(SwapArgs(fun queue:snoc/2)),
        out => fun queue:out/1,
        out_r => fun queue:out_r/1,
        peek => WrappedReturn(fun queue:peek/1),
        peek_r => WrappedReturn(fun queue:peek_r/1),
        get => CatchEmpty(PlainReturn(fun queue:get/1)),
        get_r => CatchEmpty(PlainReturn(fun queue:get_r/1)),
        head => CatchEmpty(PlainReturn(fun queue:head/1)),
        daeh => CatchEmpty(PlainReturn(fun queue:daeh/1)),
        last => CatchEmpty(PlainReturn(fun queue:last/1)),
        init => CatchEmpty(QueueOp1(fun queue:init/1)),
        drop => CatchEmpty(QueueOp1(fun queue:drop/1)),
        drop_r => CatchEmpty(QueueOp1(fun queue:drop_r/1)),
        tail => CatchEmpty(QueueOp1(fun queue:tail/1)),
        liat => CatchEmpty(QueueOp1(fun queue:liat/1))
    }.

do_op_queue({Op, I}, Q) ->
    F = maps:get(Op, queue_op_map()),
    F(I, Q);
do_op_queue(Op, Q) ->
    F = maps:get(Op, queue_op_map()),
    F(Q).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Property helpers %%%
%%%%%%%%%%%%%%%%%%%%%%%%

% get/1 and head/1 have the same semantics
common_get_head(Fn) ->
    ?FORALL(
        {L, Q},
        list_queue(),
        try
            Fn(Q)
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
            Fn(Q)
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
            Fn(Q)
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
            Fn(Q)
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
        ct_proper_ext:safe_list(),
        begin
            Q = lists:foldl(
                fun(I, Acc) ->
                    Fn(I, Acc)
                end,
                queue:new(),
                L
            ),
            equal(lists:reverse(L), Q)
        end
    ).

common_invalid(Fn) ->
    ?FORALL(
        NonQueue,
        non_queue(),
        expect_badarg(Fn, [NonQueue])
    ).

common_invalid_pred(Fn) ->
    ?FORALL(
        {Q, Pred},
        oneof([{non_queue(), fun is_atom/1}, {queue(), non_fun(1)}, {non_queue(), non_fun(1)}]),
        expect_badarg(Fn, [Pred, Q])
    ).

common_invalid_term(Fn) ->
    ?FORALL(
        {I, NonQueue},
        {ct_proper_ext:safe_any(), non_queue()},
        expect_badarg(Fn, [I, NonQueue])
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

list_queue() ->
    list_queue(ct_proper_ext:safe_any()).

list_queue(Type) ->
    ?LET(
        {List1, List2},
        {list(Type), list(Type)},
        begin
            Queue=lists:foldl(
                fun(X, Acc) -> queue:in(X, Acc) end,
                queue:from_list(List1),
                List2
            ),
            {List1 ++ List2, Queue}
        end
    ).

queue() ->
    queue(ct_proper_ext:safe_any()).

queue(Type) ->
    ?LET(List, list(Type), queue:from_list(List)).

%% This generator produces terms that are not queues.
%%
%% Therefore, it relies on knowledge of the internal representation
%% of queues (at the time of this writing, a tuple of two lists) in
%% order to prevent accidential generation of a queue.
%%
%% If the internal representation of queues ever changes, this
%% generator has to be changed to reflect this.
non_queue() ->
    ?SUCHTHAT(
        T,
        ct_proper_ext:safe_any(),
        not(
            is_tuple(T) andalso
            tuple_size(T) =:= 2 andalso
            is_list(element(1, T)) andalso
            is_list(element(2, T))
        )
    ).

non_fun(Arity) ->
    ?SUCHTHAT(
        T,
        ct_proper_ext:safe_any(),
        not is_function(T, Arity)
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

expect_badarg(Fn, Args) when is_function(Fn, length(Args)) ->
    try
        erlang:apply(Fn, Args)
    of
        _ -> false
    catch
        error:badarg -> true;
        _:_ -> false
    end.
