%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Test running functionality

-module(eunit_test).

-export([run_testfun/1, mf_wrapper/2, enter_context/4, multi_setup/1]).


-include("eunit.hrl").
-include("eunit_internal.hrl").


%% ---------------------------------------------------------------------
%% Getting a cleaned up stack trace. (We don't want it to include
%% eunit's own internal functions. This complicates self-testing
%% somewhat, but you can't have everything.) Note that we assume that
%% this particular module is the boundary between eunit and user code.

get_stacktrace() ->
    get_stacktrace([]).

get_stacktrace(Ts) ->
    eunit_lib:uniq(prune_trace(erlang:get_stacktrace(), Ts)).

-dialyzer({no_match, prune_trace/2}).
prune_trace([{eunit_data, _, _} | Rest], Tail) ->
    prune_trace(Rest, Tail);
prune_trace([{eunit_data, _, _, _} | Rest], Tail) ->
    prune_trace(Rest, Tail);
prune_trace([{?MODULE, _, _} | _Rest], Tail) ->
    Tail;
prune_trace([{?MODULE, _, _, _} | _Rest], Tail) ->
    Tail;
prune_trace([T | Ts], Tail) ->
    [T | prune_trace(Ts, Tail)];
prune_trace([], Tail) ->
    Tail.


%% ---------------------------------------------------------------------
%% Test runner

%% @spec ((any()) -> any()) -> {ok, Value} | {error, eunit_lib:exception()}
%% @throws wrapperError()

run_testfun(F) ->
    try
	F()
    of Value ->
	    {ok, Value}
    catch
	{eunit_internal, Term} ->
	    %% Internally generated: re-throw Term (lose the trace)
	    throw(Term);
	Class:Reason ->
	    {error, {Class, Reason, get_stacktrace()}}
    end.


-ifdef(TEST).
-dialyzer({[no_match, no_fail_call, no_return], macro_test_/0}).
macro_test_() ->
    {"macro definitions",
     [{?LINE, fun () ->
 		      {?LINE, F} = ?_test(undefined),
 		      {ok, undefined} = run_testfun(F)
 	      end},
      ?_test(begin
 		 {?LINE, F} = ?_assert(true),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assert(false),
 		 {error,{error,{assertion_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {expected,true},
				 {value,false}]},
			 _}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assert([]),
 		 {error,{error,{assertion_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {expected,true},
				 {value,{not_a_boolean,[]}}]},
			 _}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertNot(false),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertNot(true),
 		 {error,{error,{assertion_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {expected,true},
				 {value,false}]},
			 _}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertMatch(ok, ok),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertMatch([_], []),
 		 {error,{error,{assertMatch_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {pattern,"[ _ ]"},
				 {value,[]}]},
			 _}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotMatch(ok, error),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotMatch([_], [42]),
		 {error,{error,{assertNotMatch_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {pattern,"[ _ ]"},
				 {value,[42]}]},
			 _}}
		     = run_testfun(F)
	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertEqual(ok, ok),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertEqual(3, 1+1),
 		 {error,{error,{assertEqual_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {expected,3},
				 {value,2}]},
			 _}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotEqual(1, 0),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotEqual(2, 1+1),
		 {error,{error,{assertNotEqual_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {value,2}]},
			 _}}
		     = run_testfun(F)
	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarith,
 						erlang:error(badarith)),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarith, ok),
 		 {error,{error,{assertException_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {pattern,_},
				 {unexpected_success,ok}]},
			 _}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarg,
 						erlang:error(badarith)),
 		 {error,{error,{assertException_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {pattern,_},
				 {unexpected_exception,
				  {error,badarith,_}}]},
			 _}}
		     = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertError(badarith,
					    erlang:error(badarith)),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertExit(normal, exit(normal)),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertThrow(foo, throw(foo)),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotException(error, badarith, 42),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotException(error, badarith,
						   erlang:error(badarg)),
		 {ok, ok} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertNotException(error, badarith,
						   erlang:error(badarith)),
		 {error,{error,{assertNotException_failed,
				[{module,_},
				 {line,_},
				 {expression,_},
				 {pattern,_},
				 {unexpected_exception,
				  {error,badarith,_}}]},
			 _}}
		     = run_testfun(F)
 	     end)
     ]}.
-endif.


%% ---------------------------------------------------------------------
%% Wrapper for simple "named function" tests ({M,F}), which provides
%% better error reporting when the function is missing at test time.
%%
%% Note that the wrapper fun is usually called by run_testfun/1, and the
%% special exceptions thrown here are expected to be handled there.
%%
%% @throws {eunit_internal, wrapperError()}
%%
%% @type wrapperError() = {no_such_function, mfa()}
%%                      | {module_not_found, moduleName()}

mf_wrapper(M, F) ->
    fun () ->
 	    try M:F()
 	    catch
 		error:undef ->
 		    %% Check if it was M:F/0 that was undefined
 		    case erlang:module_loaded(M) of
 			false ->
 			    fail({module_not_found, M});
 			true ->
 			    case erlang:function_exported(M, F, 0) of
 				false ->
 				    fail({no_such_function, {M,F,0}});
 				true ->
 				    rethrow(error, undef, [{M,F,0}])
 			    end
 		    end
 	    end
    end.

rethrow(Class, Reason, Trace) ->
    erlang:raise(Class, Reason, get_stacktrace(Trace)).

fail(Term) ->
    throw({eunit_internal, Term}).				   


-ifdef(TEST).
wrapper_test_() ->
    {"error handling in function wrapper",
     [?_assertException(throw, {module_not_found, eunit_nonexisting},
  			run_testfun(mf_wrapper(eunit_nonexisting,test))),
      ?_assertException(throw,
  			{no_such_function, {?MODULE,nonexisting_test,0}},
  			run_testfun(mf_wrapper(?MODULE,nonexisting_test))),
      ?_test({error, {error, undef, _T}}
  	     = run_testfun(mf_wrapper(?MODULE,wrapper_test_exported_)))
     ]}.

%% this must be exported (done automatically by the autoexport transform)
-dialyzer({no_missing_calls, wrapper_test_exported_/0}).
wrapper_test_exported_() ->
    {ok, ?MODULE:nonexisting_function()}.
-endif.


%% ---------------------------------------------------------------------
%% Entering a setup-context, with guaranteed cleanup.

%% @spec (Setup, Cleanup, Instantiate, Callback) -> any()
%%    Setup = () -> any()
%%    Cleanup = (any()) -> any()
%%    Instantiate = (any()) -> tests()
%%    Callback = (tests()) -> any()
%% @throws {context_error, Error, eunit_lib:exception()}
%% Error = setup_failed | instantiation_failed | cleanup_failed

enter_context(Setup, Cleanup, Instantiate, Callback) ->
    try Setup() of
	R ->
	    try Instantiate(R) of
		T ->
                    case eunit_lib:is_not_test(T) of
                        true ->
                            catch throw(error),  % generate a stack trace
                            {module,M} = erlang:fun_info(Instantiate, module),
                            {name,N} = erlang:fun_info(Instantiate, name),
                            {arity,A} = erlang:fun_info(Instantiate, arity),
                            context_error({bad_instantiator, {{M,N,A},T}},
                                          error, badarg);
                        false ->
                            ok
                    end,
		    try Callback(T)  %% call back to client code
		    after
			%% Always run cleanup; client may be an idiot
			try Cleanup(R)
			catch
			    Class:Term ->
				context_error(cleanup_failed, Class, Term)
			end
		    end
	    catch
		Class:Term ->
		    context_error(instantiation_failed, Class, Term)
	    end
    catch
	Class:Term ->
	    context_error(setup_failed, Class, Term)
    end.

context_error(Type, Class, Term) ->
    throw({context_error, Type, {Class, Term, get_stacktrace()}}).

%% This generates single setup/cleanup functions from a list of tuples
%% on the form {Tag, Setup, Cleanup}, where the setup function always
%% backs out correctly from partial completion.

multi_setup(List) ->
    {SetupAll, CleanupAll} = multi_setup(List, fun ok/1),
    %% must reverse back and forth here in order to present the list in
    %% "natural" order to the test instantiation function
    {fun () -> lists:reverse(SetupAll([])) end,
     fun (Rs) -> CleanupAll(lists:reverse(Rs)) end}.

multi_setup([{Tag, S, C} | Es], CleanupPrev) ->
    Cleanup = fun ([R | Rs]) ->
		      try C(R) of
			  _ -> CleanupPrev(Rs)
		      catch
			  Class:Term ->
			      throw({Tag, {Class, Term, get_stacktrace()}})
		      end
	      end,
    {SetupRest, CleanupAll} = multi_setup(Es, Cleanup),
    {fun (Rs) ->
	     try S() of
		 R ->
		     SetupRest([R|Rs])
	     catch
		 Class:Term ->
		     CleanupPrev(Rs),
		     throw({Tag, {Class, Term, get_stacktrace()}})
	     end
     end,
     CleanupAll};
multi_setup([{Tag, S} | Es], CleanupPrev) ->
    multi_setup([{Tag, S, fun ok/1} | Es], CleanupPrev);
multi_setup([], CleanupAll) ->
    {fun (Rs) -> Rs end, CleanupAll}.

ok(_) -> ok.
