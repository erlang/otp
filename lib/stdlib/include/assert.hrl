%%
%% %CopyrightBegin%
%%
%% Copyright (C) 2004-2016 Richard Carlsson, Mickaël Rémond
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

-ifndef(ASSERT_HRL).
-define(ASSERT_HRL, true).

%% Asserts are enabled unless NOASSERT is defined, and ASSERT can be used to
%% override it: if both ASSERT and NOASSERT are defined, then ASSERT takes
%% precedence, and NOASSERT will become undefined.
%%
%% Furthermore, if NODEBUG is defined, it implies NOASSERT, unless DEBUG or
%% ASSERT are defined.
%%
%% If asserts are disabled, all assert macros are defined to be the atom
%% 'ok'. If asserts are enabled, all assert macros are defined to yield 'ok'
%% as the result if the test succeeds, and raise an error exception if the
%% test fails. The error term will then have the form {Name, Info} where
%% Name is the name of the macro and Info is a list of tagged tuples.

%% allow NODEBUG to imply NOASSERT, unless DEBUG
-ifdef(NODEBUG).
-ifndef(DEBUG).
-ifndef(NOASSERT).
-define(NOASSERT, true).
-endif.
-endif.
-endif.

%% allow ASSERT to override NOASSERT
-ifdef(ASSERT).
-undef(NOASSERT).
-endif.

%% Assert macros must not depend on any non-kernel or stdlib libraries.
%%
%% We must use fun-call wrappers ((fun () -> ... end)()) to avoid
%% exporting local variables, and furthermore we only use variable names
%% prefixed with "__", that hopefully will not be bound outside the fun.
%% It is not possible to nest assert macros.

-ifdef(NOASSERT).
-define(assert(BoolExpr),ok).
-else.
%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant or
%% is known to be boolean-only.
-define(assert(BoolExpr),
        begin
        ((fun () ->
            __T = is_process_alive(self()),  % cheap source of truth
            case (BoolExpr) of
                __T -> ok;
                __V -> erlang:error({assert,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??BoolExpr)},
                                      {expected, true},
                                      case not __T of
                                          __V -> {value, false};
                                          _ -> {not_boolean, __V}
                                      end]})
            end
          end)())
        end).
-endif.

%% This is the inverse case of assert, for convenience.
-ifdef(NOASSERT).
-define(assertNot(BoolExpr),ok).
-else.
-define(assertNot(BoolExpr),
        begin
        ((fun () ->
            __F = not is_process_alive(self()),
            case (BoolExpr) of
                __F -> ok;
                __V -> erlang:error({assert,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??BoolExpr)},
                                      {expected, false},
                                      case not __F of
                                          __V -> {value, true};
                                          _ -> {not_boolean, __V}
                                      end]})
            end
          end)())
        end).
-endif.

%% This is mostly a convenience which gives more detailed reports.
%% Note: Guard is a guarded pattern, and can not be used for value.
-ifdef(NOASSERT).
-define(assertMatch(Guard, Expr), ok).
-else.
-define(assertMatch(Guard, Expr),
        begin
        ((fun () ->
            case (Expr) of
                Guard -> ok;
                __V -> erlang:error({assertMatch,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {pattern, (??Guard)},
                                      {value, __V}]})
            end
          end)())
        end).
-endif.

%% This is the inverse case of assertMatch, for convenience.
-ifdef(NOASSERT).
-define(assertNotMatch(Guard, Expr), ok).
-else.
-define(assertNotMatch(Guard, Expr),
        begin
        ((fun () ->
            __V = (Expr),
            case __V of
                Guard -> erlang:error({assertNotMatch,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {expression, (??Expr)},
                                        {pattern, (??Guard)},
                                        {value, __V}]});
                _ -> ok
            end
          end)())
        end).
-endif.

%% This is a convenience macro which gives more detailed reports when
%% the expected LHS value is not a pattern, but a computed value
-ifdef(NOASSERT).
-define(assertEqual(Expect, Expr), ok).
-else.
-define(assertEqual(Expect, Expr),
        begin
        ((fun () ->
            __X = (Expect),
            case (Expr) of
                __X -> ok;
                __V -> erlang:error({assertEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {expected, __X},
                                      {value, __V}]})
            end
          end)())
        end).
-endif.

%% This is the inverse case of assertEqual, for convenience.
-ifdef(NOASSERT).
-define(assertNotEqual(Unexpected, Expr), ok).
-else.
-define(assertNotEqual(Unexpected, Expr),
        begin
        ((fun () ->
            __X = (Unexpected),
            case (Expr) of
                __X -> erlang:error({assertNotEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {value, __X}]});
                _ -> ok
            end
          end)())
        end).
-endif.

%% Note: Class and Term are patterns, and can not be used for value.
%% Term can be a guarded pattern, but Class cannot.
-ifdef(NOASSERT).
-define(assertException(Class, Term, Expr), ok).
-else.
-define(assertException(Class, Term, Expr),
        begin
        ((fun () ->
            try (Expr) of
                __V -> erlang:error({assertException,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??Expr)},
                                       {pattern,
                                        "{ "++(??Class)++" , "++(??Term)
                                        ++" , [...] }"},
                                       {unexpected_success, __V}]})
            catch
                Class:Term -> ok;
                __C:__T ->
                    erlang:error({assertException,
                                  [{module, ?MODULE},
                                   {line, ?LINE},
                                   {expression, (??Expr)},
                                   {pattern,
                                    "{ "++(??Class)++" , "++(??Term)
                                    ++" , [...] }"},
                                   {unexpected_exception,
                                    {__C, __T,
                                     erlang:get_stacktrace()}}]})
            end
          end)())
        end).
-endif.

-define(assertError(Term, Expr), ?assertException(error, Term, Expr)).
-define(assertExit(Term, Expr), ?assertException(exit, Term, Expr)).
-define(assertThrow(Term, Expr), ?assertException(throw, Term, Expr)).

%% This is the inverse case of assertException, for convenience.
%% Note: Class and Term are patterns, and can not be used for value.
%% Both Class and Term can be guarded patterns.
-ifdef(NOASSERT).
-define(assertNotException(Class, Term, Expr), ok).
-else.
-define(assertNotException(Class, Term, Expr),
        begin
        ((fun () ->
            try (Expr) of
                _ -> ok
            catch
                __C:__T ->
                    case __C of
                        Class ->
                            case __T of
                                Term ->
                                    erlang:error({assertNotException,
                                                  [{module, ?MODULE},
                                                   {line, ?LINE},
                                                   {expression, (??Expr)},
                                                   {pattern,
                                                    "{ "++(??Class)++" , "
                                                    ++(??Term)++" , [...] }"},
                                                   {unexpected_exception,
                                                    {__C, __T,
                                                     erlang:get_stacktrace()
                                                    }}]});
                                _ -> ok
                            end;
                        _ -> ok
                    end
            end
          end)())
        end).
-endif.

-endif. % ASSERT_HRL
