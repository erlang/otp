-module(pretty).

-export([arithmetic/0,
         binary/0,
         block/0,
         catch_throw/0,
         comparision/0,
         comprehensions/0,
         f/1,
         f/2,
         funs/0,
         handle_call/3,
         is_greater_than/2,
         is_valid_signal/1,
         list_op/0,
         map_ops/2,
         record_ops/2,
         operators/0,
         fold_fun/0,
         short_cut/3,
         termize_file/1,
         test/2,
         timer/0,
         timer/1,
         try_catch/4,
         uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuugly/0,
         uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuugly/1,
         wait_for_onhook/0,
         wait_until_for_onhook/0]).

-define(THRESHOLD, 1).

uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuugly() -> ugh.

uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuugly(Ugh) -> Ugh.

fold_fun() -> fun ({}, Acc) -> Acc end.

f("prefix" ++ Str) -> Str.

f({connect, From, To, Number, Options}, To) ->
    Signal = {connect, From, To, Number, Options},
    Signal;
f(_Signal, _To) -> ignore.

test(Value, Result) ->
    case {Value, Result} of {?THRESHOLD + 1, ok} -> Value end,
    {_A, _B} = {answer, 42},
    Fun1 = fun (X) -> X + 1 end,
    Fun1(3),
    [1, 2, 3, 4] = fun lists:append/2([1, 2], [3, 4]).

is_greater_than(X, Y) ->
    if
        X > Y -> true;
        % works as an 'else' branch
        true ->
            false
    end.

is_valid_signal(Signal) ->
    case Signal of
        {signal, _What, _From, _To} -> true;
        {signal, _What, _To} -> true;
        _Else -> false
    end.

wait_for_onhook() ->
    receive
        onhook ->
            disconnect(),
            idle();
        {connect, B} ->
            B ! {busy, self()},
            wait_for_onhook()
    end.

wait_until_for_onhook() ->
    receive
        onhook ->
            disconnect(),
            idle();
        {connect, B} ->
            B ! {busy, self()},
            wait_for_onhook()
    after 60000 ->
        disconnect(),
        erlang:error()
    end.

disconnect() -> ok.

idle() -> ok.

timer() -> spawn(m, timer, [self()]).

timer(Pid) -> receive after 5000 -> Pid ! timeout end.

comparision() ->
    _ = 1 == 1.0,
    _ = 1 =:= 1.0,
    _ = 1 > a,
    _ = #{c => 3} > #{a => 1,b => 2},
    #{a => 1,b => 2} == #{a => 1.0,b => 2.0}.

arithmetic() ->
    _ = + 1,
    _ = - 1,
    _ = 1 + 1,
    _ = 4 / 2,
    _ = 5 div 2,
    _ = 5 rem 2,
    _ = 2 band 1,
    _ = 2 bor 1,
    _ = not true,
    _ = true and false,
    true xor false.

short_cut(A, B, L) ->
    case A >= - 1.0 andalso math:sqrt(A + 1) > B of
        true -> ok;
        false -> nok
    end,
    OnlyOne = is_atom(L) orelse is_list(L) andalso length(L) == 1,
    OnlyOne.

list_op() ->
    _ = [1, 2, 3] ++ [4, 5],
    [1, 2, 3, 2, 1, 2] -- [2, 1, 2].

map_ops(A, B) ->
    % empty map
    _ = #{},
    % single association with literals
    _ = #{a => <<"hello">>},
    % multiple associations with literals
    _ = #{1 => 2,b => b},
    % single association with variables
    _ = #{k => {A, B}},
    % compound key associated with an evaluated expression
    _ = #{{"w", 1} => fold_fun()},
    _ = #{1.0 => a,1 => b},
    M0 = #{},
    M1 = M0#{a => 0},
    M2 = M1#{a => 1,b => 2},
    M3 = M2#{"function" => fun () -> f("") end},
    % 'a' and 'b' was added in `M1` and `M2`.
    _ = M3#{a := 2,b := 3},
    BM1 = #{1 => a},
    _ = BM1#{1.0 => b},
    _ = BM1#{1 := b},
    M = #{"tuple" => {1, 2}},
    #{"tuple" := {1, _B2}} = M.

-record(pretty_rec, {a, b}).
record_ops(A, B) ->
    _ = #pretty_rec.a,
    % empty map
    _ = #pretty_rec{},
    % single association with literals
    _ = #pretty_rec{a = <<"hello">>},
    % multiple associations with literals
    _ = #pretty_rec{a = 2, b = b},
    % single association with variables
    RAB = #pretty_rec{a = {A, B}},
    #pretty_rec{a = {A, B}} = RAB,
    % wildcard key associated with an evaluated expression
    _ = #pretty_rec{_ = fold_fun()},
    R0 = #pretty_rec{},
    R1 = R0#pretty_rec{a = 0},
    R2 = R1#pretty_rec{a = 1,b = 2},
    R3 = R2#pretty_rec{a = fun () -> f("") end},
    % 'a' and 'b' was added in `R1` and `R2`.
    #pretty_rec{a = 2,b = 3} = R3.

%% only start
%% if not_started
handle_call(start, _From, #{state := not_started} = S) ->
    {reply, ok, S#{state := start}};
%% only change if started
handle_call(change, _From, #{state := start} = S) ->
    {reply, ok, S#{state := changed}}.

binary() ->
    _Bin1 = <<1, 17, 42>>,
    _Bin2 = <<"abc">>,
    _Bin3 = <<1, 17, 42:16>>,
    <<_A, _B, _C:16>> = <<1, 17, 42:16>>,
    <<_D:16, _E, _F>> = <<1, 17, 42:16>>,
    <<_G, _H/binary>> = <<1, 17, 42:16>>,
    <<_G, _J/bitstring>> = <<1, 17, 42:12>>,
    <<1024/utf8>>.

funs() ->
    Fun1 = fun (X) -> X + 1 end,
    3 = Fun1(2),
    Fun2 = fun (X) when X >= 5 -> gt; (_X) -> lt end,
    gt = Fun2(7),
    Fun3 = fun Fact(1) -> 1; Fact(X) when X > 1 -> X * Fact(X - 1) end,
    24 = Fun3(4).

catch_throw() -> catch throw(hello).

try_catch(Exprs, Pattern, Gs, Body) ->
    try Exprs of Pattern when Gs -> Body after Body end,
    try Exprs catch _Error:Pattern -> Body after Body end,
    try Exprs after Body end.

termize_file(Name) ->
    {ok, F} = file:open(Name, [read, binary]),
    try
        {ok, Bin} = file:read(F, 1024 * 1024),
        binary_to_term(Bin)
    after
        file:close(F)
    end.

block() ->
    begin
        (1 + 2) * 3,
        ok
    end.

comprehensions() ->
    [2, 4, 6] = [X * 2 || X <- [1, 2, 3]],
    [2] = [2 || is_integer(2)],
    <<2, 4, 6>> = <<<<(X * 2)>> || <<X>> <= <<1, 2, 3>>>>.

operators() -> 2.45e+1 = 6 + 5 * 4 - 3 / 2 = 6 + 20 - 1.5 = 26 - 1.5.
