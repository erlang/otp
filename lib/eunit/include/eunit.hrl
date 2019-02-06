%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% NOTE: An object file that uses the macros in this header file shall
%% never be considered a derived work under the the LGPL; these macros
%% shall be regarded as "small" regardless of the exact line count.
%%
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson

-ifndef(EUNIT_HRL).
-define(EUNIT_HRL, true).

%% Including this file turns on testing and defines TEST, unless NOTEST
%% is defined before the file is included. If both NOTEST and TEST are
%% already defined, then TEST takes precedence, and NOTEST will become
%% undefined.
%%
%% If NODEBUG is defined before this file is included, the debug macros
%% are disabled, unless DEBUG is also defined, in which case NODEBUG
%% will become undefined. NODEBUG also implies NOASSERT, unless testing
%% is enabled.
%%
%% Defining NOASSERT disables asserts. NODEBUG implies NOASSERT unless
%% testing is enabled. If including this file causes TEST to be defined,
%% then NOASSERT will be undefined, even if it was previously defined and
%% even if NODEBUG is defined. If both ASSERT and NOASSERT are defined
%% before the file is included, then ASSERT takes precedence, and NOASSERT
%% will become undefined regardless of TEST.
%%
%% After including this file, EUNIT will be defined if and only if TEST
%% is defined.

%% allow defining TEST to override NOTEST
-ifdef(TEST).
-undef(NOTEST).
-endif.

%% allow defining DEBUG to override NODEBUG
-ifdef(DEBUG).
-undef(NODEBUG).
-endif.

%% note that the main switch used within this file is NOTEST; however,
%% both TEST and EUNIT may be used to check whether testing is enabled
-ifndef(NOTEST).
-ifndef(ASSERT).
-define(ASSERT, true).  % testing requires that assertions are enabled
-endif.
-ifndef(TEST).
-define(TEST, true).
-endif.
-ifndef(EUNIT).
-define(EUNIT, true).
-endif.
-else.
-undef(EUNIT).
-endif.

%% include the assert macros; ASSERT overrides NOASSERT if defined
-include_lib("stdlib/include/assert.hrl").

%% Parse transforms for automatic exporting/stripping of test functions.
%% (Note that although automatic stripping is convenient, it will make
%% the code dependent on this header file and the eunit_striptests
%% module for compilation, even when testing is switched off! Using
%% -ifdef(EUNIT) around all test code makes the program more portable.)

-ifndef(EUNIT_NOAUTO).
-ifndef(NOTEST).
-compile({parse_transform, eunit_autoexport}).
-else.
-compile({parse_transform, eunit_striptests}).
-endif.
-endif.

%% All macros should be available even if testing is turned off, and
%% should preferably not require EUnit to be present at runtime.
%%
%% We must use fun-call wrappers ((fun () -> ... end)()) to avoid
%% exporting local variables, and furthermore we only use variable names
%% prefixed with "__", that hopefully will not be bound outside the fun.

%% A generic let-macro is particularly useful when writing test cases.
%% It is more compact than 'begin X = Y, Z end', and guarantees that
%% X gets a new, local binding.
%% (Note that lowercase 'let' is a reserved word.)
-ifndef(LET).
-define(LET(X,Y,Z), begin ((fun(X)->(Z)end)(Y)) end).
-endif.

%% It is important that testing code is short and readable.
%% An if-then-else macro can make some code much more compact.
%% Compare:  case f(X) of true->g(X); false->h(X) end
%%     and:  ?IF(f(X), g(Y), h(Z))
-ifndef(IF).
-define(IF(B,T,F), begin (case (B) of true->(T); false->(F) end) end).
-endif.

%% This macro yields 'true' if the value of E matches the guarded
%% pattern G, otherwise 'false'.
-ifndef(MATCHES).
-define(MATCHES(G,E), begin (case (E) of G -> true; _ -> false end) end).
-endif.

%% This macro can be used at any time to check whether or not the code
%% is currently running directly under eunit. Note that it does not work
%% in secondary processes if they have been assigned a new group leader.
-ifndef(UNDER_EUNIT).
-define(UNDER_EUNIT,
	(?MATCHES({current_function,{eunit_proc,_,_}},
		  erlang:process_info(erlang:group_leader(),
				      current_function)))).
-endif.

%% General test macros

-define(_test(Expr), {?LINE, fun () -> (Expr) end}).
-define(_assert(BoolExpr), ?_test(?assert(BoolExpr))).
-define(_assertNot(BoolExpr), ?_assert(not (BoolExpr))).
-define(_assertMatch(Guard, Expr), ?_test(?assertMatch(Guard, Expr))).
-define(_assertNotMatch(Guard, Expr), ?_test(?assertNotMatch(Guard, Expr))).
-define(_assertEqual(Expect, Expr), ?_test(?assertEqual(Expect, Expr))).
-define(_assertNotEqual(Unexpected, Expr),
	?_test(?assertNotEqual(Unexpected, Expr))).
-define(_assertException(Class, Term, Expr),
	?_test(?assertException(Class, Term, Expr))).
-define(_assertError(Term, Expr), ?_assertException(error, Term, Expr)).
-define(_assertExit(Term, Expr), ?_assertException(exit, Term, Expr)).
-define(_assertThrow(Term, Expr), ?_assertException(throw, Term, Expr)).
-define(_assertNotException(Class, Term, Expr),
	?_test(?assertNotException(Class, Term, Expr))).

%% Macros for running operating system commands. (Note that these
%% require EUnit to be present at runtime, or at least eunit_lib.)

%% these can be used for simply running commands in a controlled way
-define(_cmd_(Cmd), (eunit_lib:command(Cmd))).
-define(cmdStatus(N, Cmd),
	begin
	((fun () ->
	    case ?_cmd_(Cmd) of
		{(N), __Out} -> __Out;
		{__N, _} -> erlang:error({command_failed,
					  [{module, ?MODULE},
					   {line, ?LINE},
					   {command, (Cmd)},
					   {expected_status,(N)},
					   {status,__N}]})
	    end
	  end)())
	end).
-define(_cmdStatus(N, Cmd), ?_test(?cmdStatus(N, Cmd))).
-define(cmd(Cmd), ?cmdStatus(0, Cmd)).
-define(_cmd(Cmd), ?_test(?cmd(Cmd))).

%% these are only used for testing; they always return 'ok' on success,
%% and have no effect if debugging/testing is turned off
-ifdef(NOASSERT).
-define(assertCmdStatus(N, Cmd), ok).
-else.
-define(assertCmdStatus(N, Cmd),
	begin
            ((fun () ->
                      case ?_cmd_(Cmd) of
                          {(N), _} -> ok;
                          {__N, _} -> erlang:error({assertCmd_failed,
                                                    [{module, ?MODULE},
                                                     {line, ?LINE},
                                                     {command, (Cmd)},
                                                     {expected_status,(N)},
                                                     {status,__N}]})
                      end
              end)())
        end).
-endif.
-define(assertCmd(Cmd), ?assertCmdStatus(0, Cmd)).

-ifdef(NOASSERT).
-define(assertCmdOutput(T, Cmd), ok).
-else.
-define(assertCmdOutput(T, Cmd),
	begin
            ((fun () ->
                      case ?_cmd_(Cmd) of
                          {_, (T)} -> ok;
                          {_, __T} -> erlang:error({assertCmdOutput_failed,
                                                    [{module, ?MODULE},
                                                     {line, ?LINE},
                                                     {command,(Cmd)},
                                                     {expected_output,(T)},
                                                     {output,__T}]})
                      end
              end)())
	end).
-endif.

-define(_assertCmdStatus(N, Cmd), ?_test(?assertCmdStatus(N, Cmd))).
-define(_assertCmd(Cmd), ?_test(?assertCmd(Cmd))).
-define(_assertCmdOutput(T, Cmd), ?_test(?assertCmdOutput(T, Cmd))).

%% Macros to simplify debugging (in particular, they work even when the
%% standard output is being redirected by EUnit while running tests)

-ifdef(NODEBUG).
-define(debugMsg(S), ok).
-define(debugHere, ok).
-define(debugFmt(S, As), ok).
-define(debugVal(E), (E)).
-define(debugValAll(E), (E)).
-define(debugTime(S, E), (E)).
-else.
-define(debugMsg(S),
	begin
	    io:fwrite(user, <<"~ts:~w:~w: ~ts\n">>,
		      [?FILE, ?LINE, self(), S]),
	    ok
	end).
-define(debugHere, (?debugMsg("<-"))).
-define(debugFmt(S, As), (?debugMsg(io_lib:format((S), (As))))).
-define(debugVal(E, D),
	begin
	((fun (__V) ->
		  ?debugFmt(<<"~ts = ~tP">>,
                            [(??E), __V, D]),
		  __V
	  end)(E))
	end).
-ifndef(EUNIT_DEBUG_VAL_DEPTH).
-define(EUNIT_DEBUG_VAL_DEPTH, 15).
-endif.
-define(debugVal(E), ?debugVal(E, ?EUNIT_DEBUG_VAL_DEPTH)).
-define(debugTime(S, E),
	begin
	((fun () ->
		  {__T0, _} = statistics(wall_clock),
		  __V = (E),
		  {__T1, _} = statistics(wall_clock),
		  ?debugFmt(<<"~ts: ~.3f s">>, [(S), (__T1-__T0)/1000]),
		  __V
	  end)())
	end).
-endif.

-endif. % EUNIT_HRL
