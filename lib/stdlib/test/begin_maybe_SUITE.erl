-module(begin_maybe_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([begin1/1, begin2/1, begin3/1]).

all() ->
    [begin1, begin2].

begin1(_Config) ->
    ?assertEqual(maybe1(), case1()).

maybe1() ->
    begin
        {ok, A} <- id({ok,5}),
        B = ok(A),
        {ok, C=[_|_]} <- id({ok, append(A,B)}),
        {ok, {A,B,C}}
    cond
        {error, _Unexpected} -> error;
        {ok, _Unexpected} -> error
    end.

case1() ->
    case begin
        case id({ok,5}) of
            {ok, A} ->
                B = ok(A),
                case id({ok, append(A,B)}) of
                    {ok, C=[_|_]} ->
                        {value, {ok, {A,B,C}}};
                    Other ->
                        {'else', Other}
                end;
            Other ->
                {'else', Other}
        end
    end of
        %% good clause
        {value, X} -> X;
        %% else clause
        {'else', X} ->
            case X of
                {error, _Unexpected} -> error;
                {ok, _Unexpected} -> error
            end
    end.

begin2(_Config) ->
    ?assertEqual(maybe2(), case2()).

maybe2() ->
    begin
        {ok, A} <- id({ok,5}),
        B = ok(A),
        C=[_|_] <- append(A,B),
        {ok, {A,B,C}}
    cond
        {error, _Unexpected} -> error;
        {ok, _Unexpected} -> error
    end.

case2() ->
    case begin
        case id({ok,5}) of
            {ok, A} ->
                B = ok(A),
                case append(A,B) of
                    C=[_|_] ->
                        {value, {ok, {A,B,C}}};
                    Other ->
                        {'else', Other}
                end;
            Other ->
                {'else', Other}
        end
    end of
        %% good clause
        {value, X} -> X;
        %% else clause
        {'else', X} ->
            case X of
                {error, _Unexpected} -> error;
                {ok, _Unexpected} -> error
            end
    end.

begin3(_Config) ->
    %% These don't work yet because of macro processing woes or something?
    %% seems to be the ??EXPR format dying
    % ?assertEqual(x, begin _ <- x end),
    % ?assertEqual(x, begin _ <- x cond _ -> y end),
    % ?assertEqual(y, begin nomatch <- x cond _ -> y end),
    % ?assertEqual(y, begin nomatch <- x cond _ -> x, y end),
    R1 = begin _ <- x end,
    ?assertEqual(x, R1),
    R2 = begin _ <- x cond _ -> y end,
    ?assertEqual(x, R2),
    R3 = begin nomatch <- x cond _ -> y end,
    ?assertEqual(y, R3),
    R4 = begin nomatch <- x cond _ -> x, y end,
    ?assertEqual(y, R4),
    R5 = begin begin nomatch <- x cond _ -> x, y end end,
    ?assertEqual(y, R5),
    R6 = (fun() -> begin nomatch <- x cond _ -> x, y end end)(),
    ?assertEqual(y, R6),
    %% This is equiv to assertequal
    begin
        ((fun () ->
            X__X = (x),
            case (begin _ <- x end) of
                X__X -> ok;
                X__V -> erlang:error({assertEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, skipped_macro},
                                      {expected, X__X},
                                      {value, X__V}]})
            end
          end)())
    end,
    ok.




%%% HELPERS %%%
id(X) -> X.
ok({ok, X}) -> X.
append(A,B) -> [A,B].
