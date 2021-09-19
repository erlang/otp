-module(begin_maybe_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([begin1/1, begin2/1, begin3/1, begin4/1, begin5/1, begin6/1, begin7/1]).

all() ->
    [begin1, begin2, begin3, begin4, begin5, begin6, begin7].

begin1(_Config) ->
    ?assertEqual(maybe1(), case1()),
    ok.

maybe1() ->
    begin
        {ok, A} <- id({ok,5}),
        B = ok({ok,A}),
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
                B = ok({ok,A}),
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
    ?assertEqual(maybe2(), case2()),
    ok.

maybe2() ->
    begin
        some_output,
        {ok, A} <- id({ok,5}),
        B = ok({ok,A}),
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
                B = ok({ok,A}),
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

begin5(_Config) ->
    ?assertEqual(maybe5(4), case5(4)),
    ?assertEqual(maybe5(5), case5(5)),
    ?assertEqual(maybe5(a), case5(b)),
    {{maybe5(4), case5(4)},
     {maybe5(5), case5(5)},
     {maybe5(a), case5(a)}}.

maybe5(A) ->
    begin
        {ok, B} <- id({ok,4}),                                      %% B
        {ok, C=[_|_]} <- {ok, append(A,B)},                         %% C
        {error, _} <- begin                                         %% D <- E
                  D = {A,B,C},                                      %% F
                  {error, _} <- err(D)                              %% G
              end,
        ok <- begin                                                 %% H <- I
                  {error, expected} <- good(D) % D still in scope   %% J
              cond
                  ok -> ok                                          %% K
              end,
        {ok, {A,B,C}}                                               %% L
    cond
        %% TODO: the linter complains here, and it's wrong!
        {error, expected} -> saved;                                 %% M
        {error, Unexpected} -> Unexpected;                          %% N
        {ok, _Unexpected} -> error                                  %% O
    end.

case5(A) ->
    case begin
         case id({ok,4}) of
             {ok, B} ->                                                 %% B
                 case {ok,append(A,B)} of
                     {ok, C=[_|_]} ->                                   %% C
                         case                                           %% D
                             case begin                                 %% E
                                    D = {A,B,C},                        %% F
                                    case err(D) of                      %% G
                                        {error, _}=V ->
                                            {value, V};
                                        OtherG ->
                                            {'else', OtherG}
                                    end
                             end of                                     %% E (still)
                                 %% no cond clause, return both flows, no errors possible
                                 {value, Ve} -> Ve;
                                 {'else', Ve} -> Ve
                             end
                         of                                             %% D (still)
                             {error, _} ->
                                 case                                   %% H
                                     case begin                         %% I
                                        case good(D) of                 %% J
                                            {error, expected} = Vj -> {value, Vj};
                                            Other -> {'else', Other}
                                        end
                                     end of
                                         {value, Vk} -> Vk;
                                         {'else', ok} -> ok;            %% K
                                         ElseErr -> erlang:error({cond_clause, ElseErr})
                                     end
                                 of
                                     ok ->
                                         {value, {ok,{A,B,C}}};         %% L
                                     OtherL ->
                                        {'else', OtherL}
                                end;
                            Other ->
                                {'else', Other}
                        end;
                    Other ->
                        {'else', Other}
                end;
            Other ->
                {'else', Other}
        end
end of
    %% good clause
    {value, X} -> X; % L
    %% else clause
    {'else', X} ->
        case X of
            {error, expected} -> saved;        % M
            {error, Unexpected} -> Unexpected; % N
            {ok, _Unexpected} -> error;        % O
            ElseErrN -> erlang:error({else_clause, ElseErrN})
            end
    end.

begin6(_) ->
    ?assertEqual(maybe6(), case6()).

maybe6() ->
    begin
        X = 2+2,
        {ok, 4} <- {error, 5},
        Z = X*2,
        {error, expected} <- {ok, 3},
        id(Z),
        X+Z
    cond
        {error, _} -> a;
        {ok, _} -> b
    end.

case6() ->
    case
        begin
            X = 2+2,
            case {error, 5} of
                {ok, 4} ->
                    Z = X*2,
                    case {ok, 3} of
                        {error, expected} ->
                            id(Z),
                            {successful_begin, X+Z};
                        Other ->
                            {begin_fail_branch, Other}
                    end;
                Other ->
                    {begin_fail_branch, Other}
            end
        end
    of
        {successful_begin, V} -> V;
        {begin_fail_branch, Err} ->
            case Err of
                {error, _} -> a;
                {ok, _} -> b
            end
    end.

begin7(_) ->
    ?assertEqual(maybe7(), case7()).

maybe7() ->
    begin
        3
    cond
        {error, _} -> a;
        {ok, _} -> b
    end.

case7() ->
    case {successful_begin,3} of
        {successful_begin, V} -> V;
        {begin_fail_branch, Err} ->
            case Err of
                {error, _} -> a;
                {ok, _} -> b
            end
    end.

%%% HELPERS %%%
id(X) -> X.

ok({ok, X}) -> X.

append(A,B) -> [A,B].

err({X,_,_}) when is_atom(X) -> {error, bad_type};
err(_) -> {error, unexpected}.

good({X,X,[X,X]}) ->
    ok;
good(_) ->
    {error, expected}.
