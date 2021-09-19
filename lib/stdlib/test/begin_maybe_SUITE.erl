-module(begin_maybe_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([begin1/1, begin2/1, begin3/1, begin4/1]).

all() ->
    [begin1, begin2, begin3, begin4].

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
    %% These only work if `epp' is modified not to delimit
    %% `cond ... end' blocks the way they were originally intended.
    ?assertEqual(x, begin _ <- x end),
    ?assertEqual(x, begin _ <- x cond _ -> y end),
    ?assertEqual(y, begin nomatch <- x cond _ -> y end),
    ?assertEqual(y, begin nomatch <- x cond _ -> x, y end),
    ok.

begin4(_Config) ->
    {'EXIT', {MaybeReason, _}} = (catch maybe4()),
    {'EXIT', {CaseReason, _}} = (catch case4()),
    ?assertEqual(MaybeReason, CaseReason).

maybe4() ->
    begin
        {ok, A} <- id({ok,4}),
        B = ok({ok, A}),
        {ok, C=[_|_]} <- append(A,B),
        {ok, {A,B,C}}
    cond
        {error, _Unexpected} -> error;
        {ok, _Unexpected} -> error
    end.

case4() ->
    case begin
        case id({ok,4}) of
            {ok, A} ->
                B = ok({ok,A}),
                case append(A,B) of
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
                {ok, _Unexpected} -> error;
                ElseErr -> erlang:error({cond_clause, ElseErr})
            end
    end.



%%% HELPERS %%%
id(X) -> X.
ok({ok, X}) -> X.
append(A,B) -> [A,B].
