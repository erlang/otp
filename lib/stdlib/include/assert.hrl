%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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
-define(assert(BoolExpr), ok).
-define(assert(BoolExpr, Comment), ok).
-else.
%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant or
%% is known to be boolean-only.
-define(assert(BoolExpr),
        begin
        ((fun () ->
            X__T = is_process_alive(self()),  % cheap source of truth
            case (BoolExpr) of
                X__T -> ok;
                X__V -> erlang:error({assert,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??BoolExpr)},
                                      {expected, true},
                                      case not X__T of
                                          X__V -> {value, false};
                                          _ -> {not_boolean, X__V}
                                      end]})
            end
          end)())
        end).
-define(assert(BoolExpr, Comment),
        begin
        ((fun () ->
            X__T = is_process_alive(self()),  % cheap source of truth
            case (BoolExpr) of
                X__T -> ok;
                X__V -> erlang:error({assert,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {comment, (Comment)},
                                      {expression, (??BoolExpr)},
                                      {expected, true},
                                      case not X__T of
                                          X__V -> {value, false};
                                          _ -> {not_boolean, X__V}
                                      end]})
            end
          end)())
        end).
-endif.

%% This is the inverse case of assert, for convenience.
-ifdef(NOASSERT).
-define(assertNot(BoolExpr),ok).
-define(assertNot(BoolExpr, Comment), ok).
-else.
-define(assertNot(BoolExpr),
        begin
        ((fun () ->
            X__F = not is_process_alive(self()),
            case (BoolExpr) of
                X__F -> ok;
                X__V -> erlang:error({assert,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??BoolExpr)},
                                      {expected, false},
                                      case not X__F of
                                          X__V -> {value, true};
                                          _ -> {not_boolean, X__V}
                                      end]})
            end
          end)())
        end).
-define(assertNot(BoolExpr, Comment),
        begin
        ((fun () ->
            X__F = not is_process_alive(self()),
            case (BoolExpr) of
                X__F -> ok;
                X__V -> erlang:error({assert,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {comment, (Comment)},
                                      {expression, (??BoolExpr)},
                                      {expected, false},
                                      case not X__F of
                                          X__V -> {value, true};
                                          _ -> {not_boolean, X__V}
                                      end]})
            end
          end)())
        end).
-endif.

%% This is mostly a convenience which gives more detailed reports.
%% Note: Guard is a guarded pattern, and cannot be used for value.
-ifdef(NOASSERT).
-define(assertMatch(Guard, Expr), ok).
-define(assertMatch(Guard, Expr, Comment), ok).
-else.
-define(assertMatch(Guard, Expr),
        begin
        ((fun () ->
            case (Expr) of
                Guard -> ok;
                X__V -> erlang:error({assertMatch,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {pattern, (??Guard)},
                                      {value, X__V}]})
            end
          end)())
        end).
-define(assertMatch(Guard, Expr, Comment),
        begin
        ((fun () ->
            case (Expr) of
                Guard -> ok;
                X__V -> erlang:error({assertMatch,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {comment, (Comment)},
                                      {expression, (??Expr)},
                                      {pattern, (??Guard)},
                                      {value, X__V}]})
            end
          end)())
        end).
-endif.

%% This is the inverse case of assertMatch, for convenience.
-ifdef(NOASSERT).
-define(assertNotMatch(Guard, Expr), ok).
-define(assertNotMatch(Guard, Expr, Comment), ok).
-else.
-define(assertNotMatch(Guard, Expr),
        begin
        ((fun () ->
            X__V = (Expr),
            case X__V of
                Guard -> erlang:error({assertNotMatch,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {expression, (??Expr)},
                                        {pattern, (??Guard)},
                                        {value, X__V}]});
                _ -> ok
            end
          end)())
        end).
-define(assertNotMatch(Guard, Expr, Comment),
        begin
        ((fun () ->
            X__V = (Expr),
            case X__V of
                Guard -> erlang:error({assertNotMatch,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {comment, (Comment)},
                                        {expression, (??Expr)},
                                        {pattern, (??Guard)},
                                        {value, X__V}]});
                _ -> ok
            end
          end)())
        end).
-endif.

%% This is a convenience macro which gives more detailed reports when
%% the expected LHS value is not a pattern, but a computed value
-ifdef(NOASSERT).
-define(assertEqual(Expect, Expr), ok).
-define(assertEqual(Expect, Expr, Comment), ok).
-else.
-define(assertEqual(Expect, Expr),
        begin
        ((fun () ->
            X__X = (Expect),
            case (Expr) of
                X__X -> ok;
                X__V -> erlang:error({assertEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {expected, X__X},
                                      {value, X__V}]})
            end
          end)())
        end).
-define(assertEqual(Expect, Expr, Comment),
        begin
        ((fun () ->
            X__X = (Expect),
            case (Expr) of
                X__X -> ok;
                X__V -> erlang:error({assertEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {comment, (Comment)},
                                      {expression, (??Expr)},
                                      {expected, X__X},
                                      {value, X__V}]})
            end
          end)())
        end).
-endif.

%% This is the inverse case of assertEqual, for convenience.
-ifdef(NOASSERT).
-define(assertNotEqual(Unexpected, Expr), ok).
-define(assertNotEqual(Unexpected, Expr, Comment), ok).
-else.
-define(assertNotEqual(Unexpected, Expr),
        begin
        ((fun () ->
            X__X = (Unexpected),
            case (Expr) of
                X__X -> erlang:error({assertNotEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {value, X__X}]});
                _ -> ok
            end
          end)())
        end).
-define(assertNotEqual(Unexpected, Expr, Comment),
        begin
        ((fun () ->
            X__X = (Unexpected),
            case (Expr) of
                X__X -> erlang:error({assertNotEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {comment, (Comment)},
                                      {expression, (??Expr)},
                                      {value, X__X}]});
                _ -> ok
            end
          end)())
        end).
-endif.

%% Note: Class and Term are patterns, and cannot be used for value.
%% Term can be a guarded pattern, but Class cannot.
-ifdef(NOASSERT).
-define(assertException(Class, Term, Expr), ok).
-define(assertException(Class, Term, Expr, Comment), ok).
-else.
-define(assertException(Class, Term, Expr),
        begin
        ((fun () ->
            try (Expr) of
                X__V -> erlang:error({assertException,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??Expr)},
                                       {pattern,
                                        "{ "++(??Class)++" , "++(??Term)
                                        ++" , [...] }"},
                                       {unexpected_success, X__V}]})
            catch
                Class:Term -> ok;
                X__C:X__T:X__S ->
                    erlang:error({assertException,
                                  [{module, ?MODULE},
                                   {line, ?LINE},
                                   {expression, (??Expr)},
                                   {pattern,
                                    "{ "++(??Class)++" , "++(??Term)
                                    ++" , [...] }"},
                                   {unexpected_exception,
                                    {X__C, X__T, X__S}}]})
            end
          end)())
        end).
-define(assertException(Class, Term, Expr, Comment),
        begin
        ((fun () ->
            try (Expr) of
                X__V -> erlang:error({assertException,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {comment, (Comment)},
                                       {expression, (??Expr)},
                                       {pattern,
                                        "{ "++(??Class)++" , "++(??Term)
                                        ++" , [...] }"},
                                       {unexpected_success, X__V}]})
            catch
                Class:Term -> ok;
                X__C:X__T:X__S ->
                    erlang:error({assertException,
                                  [{module, ?MODULE},
                                   {line, ?LINE},
                                   {comment, (Comment)},
                                   {expression, (??Expr)},
                                   {pattern,
                                    "{ "++(??Class)++" , "++(??Term)
                                    ++" , [...] }"},
                                   {unexpected_exception,
                                    {X__C, X__T, X__S}}]})
            end
          end)())
        end).
-endif.

-define(assertError(Term, Expr), ?assertException(error, Term, Expr)).
-define(assertError(Term, Expr, Comment),
        ?assertException(error, Term, Expr, Comment)).
-define(assertExit(Term, Expr), ?assertException(exit, Term, Expr)).
-define(assertExit(Term, Expr, Comment),
        ?assertException(exit, Term, Expr, Comment)).
-define(assertThrow(Term, Expr), ?assertException(throw, Term, Expr)).
-define(assertThrow(Term, Expr, Comment),
        ?assertException(throw, Term, Expr, Comment)).

%% This is the inverse case of assertException, for convenience.
%% Note: Class and Term are patterns, and cannot be used for value.
%% Both Class and Term can be guarded patterns.
-ifdef(NOASSERT).
-define(assertNotException(Class, Term, Expr), ok).
-define(assertNotException(Class, Term, Expr, Comment), ok).
-else.
-define(assertNotException(Class, Term, Expr),
        begin
        ((fun () ->
            try (Expr) of
                _ -> ok
            catch
                X__C:X__T:X__S ->
                    case X__C of
                        Class ->
                            case X__T of
                                Term ->
                                    erlang:error({assertNotException,
                                                  [{module, ?MODULE},
                                                   {line, ?LINE},
                                                   {expression, (??Expr)},
                                                   {pattern,
                                                    "{ "++(??Class)++" , "
                                                    ++(??Term)++" , [...] }"},
                                                   {unexpected_exception,
                                                    {X__C, X__T, X__S}}]});
                                _ -> ok
                            end;
                        _ -> ok
                    end
            end
          end)())
        end).
-define(assertNotException(Class, Term, Expr, Comment),
        begin
        ((fun () ->
            try (Expr) of
                _ -> ok
            catch
                X__C:X__T:X__S ->
                    case X__C of
                        Class ->
                            case X__T of
                                Term ->
                                    erlang:error({assertNotException,
                                                  [{module, ?MODULE},
                                                   {line, ?LINE},
                                                   {comment, (Comment)},
                                                   {expression, (??Expr)},
                                                   {pattern,
                                                    "{ "++(??Class)++" , "
                                                    ++(??Term)++" , [...] }"},
                                                   {unexpected_exception,
                                                    {X__C, X__T, X__S}}]});
                                _ -> ok
                            end;
                        _ -> ok
                    end
            end
          end)())
        end).
-endif.

-endif. % ASSERT_HRL
