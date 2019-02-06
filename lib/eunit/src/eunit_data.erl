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
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Interpretation of symbolic test representation

-module(eunit_data).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-include_lib("kernel/include/file.hrl").

-export([iter_init/2, iter_next/1, iter_prev/1, iter_id/1,
	 enter_context/3, get_module_tests/1]).

-import(lists, [foldr/3]).

-define(TICKS_PER_SECOND, 1000).

%% @type tests() =
%%            SimpleTest
%%          | [tests()]
%%          | moduleName()
%%          | {module, moduleName()}
%%          | {application, appName()}
%%          | {application, appName(), [term()]}
%%          | fileName()
%%          | {file, fileName()}
%%          | {string(), tests()}
%%          | {generator, () -> tests()}
%%          | {generator, M::moduleName(), F::functionName()}
%%          | {spawn, tests()}
%%          | {spawn, Node::atom(), tests()}
%%          | {timeout, T::number(), tests()}
%%          | {inorder, tests()}
%%          | {inparallel, tests()}
%%          | {inparallel, N::integer(), tests()}
%%          | {with, X::any(), [AbstractTestFunction]}
%%          | {setup, Where::local | spawn | {spawn, Node::atom()},
%%                    Setup::() -> (R::any()),
%%                    Cleanup::(R::any()) -> any(),
%%                    tests() | Instantiator
%%            }
%%          | {setup, Setup, Cleanup, tests() | Instantiator}
%%          | {setup, Where, Setup, tests() | Instantiator}
%%          | {setup, Setup, tests() | Instantiator}
%%          | {foreach, Where::local | spawn | {spawn, Node::atom()},
%%                      Setup::() -> (R::any()),
%%                      Cleanup::(R::any()) -> any(),
%%                      [tests() | Instantiator]
%%            }
%%          | {foreach, Setup, Cleanup, [tests() | Instantiator]}
%%          | {foreach, Where, Setup, [tests() | Instantiator]}
%%          | {foreach, Setup, [tests() | Instantiator]}
%%          | {foreachx, Where::local | spawn | {spawn, Node::atom()},
%%                       SetupX::(X::any()) -> (R::any()),
%%                       CleanupX::(X::any(), R::any()) -> any(),
%%                       Pairs::[{X::any(),
%%                                (X::any(), R::any()) -> tests()}]
%%            }
%%          | {foreachx, SetupX, CleanupX, Pairs}
%%          | {foreachx, Where, SetupX, Pairs}
%%          | {foreachx, SetupX, Pairs}
%%          | {node, Node::atom(), tests() | Instantiator}
%%          | {node, Node, Args::string(), tests() | Instantiator}
%%
%% SimpleTest = TestFunction | {Line::integer(), SimpleTest}
%%
%% TestFunction = () -> any()
%%              | {test, M::moduleName(), F::functionName()}
%%              | {M::moduleName(), F::functionName()}.
%%
%% AbstractTestFunction = (X::any()) -> any()
%%
%% Instantiator = (R::any()) -> tests()
%%              | {with, [AbstractTestFunction]}
%%
%% Note that `{string(), ...}' is a short-hand for `{string(), {...}}'
%% if the tuple contains more than two elements.
%%
%% @type moduleName() = atom()
%% @type functionName() = atom()
%% @type appName() = atom()
%% @type fileName() = string()

%% TODO: Can we mark up tests as known-failures?
%% TODO: Is it possible to handle known timout/setup failures?
%% TODO: Add diagnostic tests which never fail, but may cause warnings?

%% ---------------------------------------------------------------------
%% Abstract test set iterator

-record(iter,
	{prev = [],
	 next = [],
	 tests = [],
	 pos = 0,
	 parent = []}).

%% @spec (tests(), [integer()]) -> testIterator()
%% @type testIterator()

iter_init(Tests, ParentID) ->
    #iter{tests = Tests, parent = lists:reverse(ParentID)}.

%% @spec (testIterator()) -> [integer()]

iter_id(#iter{pos = N, parent = Ns}) ->
    lists:reverse(Ns, [N]).

%% @spec (testIterator()) -> none | {testItem(), testIterator()}

iter_next(I = #iter{next = []}) ->
    case next(I#iter.tests) of
	{T, Tests} ->
	    {T, I#iter{prev = [T | I#iter.prev],
		       tests = Tests,
		       pos = I#iter.pos + 1}};
	none ->
	    none
    end;
iter_next(I = #iter{next = [T | Ts]}) ->
    {T, I#iter{next = Ts,
	       prev = [T | I#iter.prev],
	       pos = I#iter.pos + 1}}.

%% @spec (testIterator()) -> none | {testItem(), testIterator()}

iter_prev(#iter{prev = []}) ->
    none;
iter_prev(#iter{prev = [T | Ts]} = I) ->
    {T, I#iter{prev = Ts,
	       next = [T | I#iter.next],
		       pos = I#iter.pos - 1}}.


%% ---------------------------------------------------------------------
%% Concrete test set representation iterator

%% @spec (tests()) -> none | {testItem(), tests()}
%% @type testItem() = #test{} | #group{}
%% @throws {bad_test, term()}
%%       | {generator_failed, {{M::atom(),F::atom(),A::integer()},
%%                             exception()}}
%%       | {no_such_function, mfa()}
%%       | {module_not_found, moduleName()}
%%       | {application_not_found, appName()}
%%       | {file_read_error, {Reason::atom(), Message::string(),
%%                            fileName()}}

next(Tests) ->
    case eunit_lib:dlist_next(Tests) of
	[T | Ts] ->
	    case parse(T) of
		{data, T1} ->
		    next([T1 | Ts]);
		T1 ->
		    {T1, Ts}
	    end;
	[] ->
	    none
    end.

%% this returns either a #test{} or #group{} record, or {data, T} to
%% signal that T has been substituted for the given representation

parse({foreach, S, Fs}) when is_function(S), is_list(Fs) ->
    parse({foreach, S, fun ok/1, Fs});
parse({foreach, S, C, Fs})
  when is_function(S), is_function(C), is_list(Fs) ->
    parse({foreach, ?DEFAULT_SETUP_PROCESS, S, C, Fs});
parse({foreach, P, S, Fs})
  when is_function(S), is_list(Fs) ->
    parse({foreach, P, S, fun ok/1, Fs});
parse({foreach, P, S, C, Fs} = T)
  when is_function(S), is_function(C), is_list(Fs) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    case Fs of
	[F | Fs1] ->
	    {data, [{setup, P, S, C, F}, {foreach, P, S, C, Fs1}]};
	[] ->
	    {data, []}
    end;
parse({foreachx, S1, Ps}) when is_function(S1), is_list(Ps) ->
    parse({foreachx, S1, fun ok/2, Ps});
parse({foreachx, S1, C1, Ps})
  when is_function(S1), is_function(C1), is_list(Ps) ->
    parse({foreachx, ?DEFAULT_SETUP_PROCESS, S1, C1, Ps});
parse({foreachx, P, S1, Ps})
  when is_function(S1), is_list(Ps) ->
    parse({foreachx, P, S1, fun ok/2, Ps});
parse({foreachx, P, S1, C1, Ps} = T) 
  when is_function(S1), is_function(C1), is_list(Ps) ->
    check_arity(S1, 1, T),
    check_arity(C1, 2, T),
    case Ps of
	[{X, F1} | Ps1] when is_function(F1) ->
	    check_arity(F1, 2, T),
	    S = fun () -> S1(X) end,
	    C = fun (R) -> C1(X, R) end,
	    F = fun (R) -> F1(X, R) end,
	    {data, [{setup, P, S, C, F}, {foreachx, P, S1, C1, Ps1}]};
	[_|_] ->
	    bad_test(T);
	[] ->
	    {data, []}
    end;
parse({generator, F}) when is_function(F) ->
    {module, M} = erlang:fun_info(F, module),
    {name, N} = erlang:fun_info(F, name),
    {arity, A} = erlang:fun_info(F, arity),
    parse({generator, F, {M,N,A}});
parse({generator, F, {M,N,A}} = T)
  when is_function(F), is_atom(M), is_atom(N), is_integer(A) ->
    check_arity(F, 0, T),
    %% use run_testfun/1 to handle wrapper exceptions
    case eunit_test:run_testfun(F) of
	{ok, T1} ->
            case eunit_lib:is_not_test(T1) of
                true -> throw({bad_generator, {{M,N,A}, T1}});
                false -> ok
            end,
	    {data, T1};
	{error, {Class, Reason, Trace}} ->
	    throw({generator_failed, {{M,N,A}, {Class, Reason, Trace}}})
    end;
parse({generator, M, F}) when is_atom(M), is_atom(F) ->
    parse({generator, eunit_test:mf_wrapper(M, F), {M,F,0}});
parse({inorder, T}) ->
    group(#group{tests = T, order = inorder});
parse({inparallel, T}) ->
    parse({inparallel, 0, T});
parse({inparallel, N, T}) when is_integer(N), N >= 0 ->
    group(#group{tests = T, order = {inparallel, N}});
parse({timeout, N, T}) when is_number(N), N >= 0 ->
    group(#group{tests = T, timeout = round(N * ?TICKS_PER_SECOND)});
parse({spawn, T}) ->
    group(#group{tests = T, spawn = local});
parse({spawn, N, T}) when is_atom(N) ->
    group(#group{tests = T, spawn = {remote, N}});
parse({setup, S, I}) when is_function(S); is_list(S) ->
    parse({setup, ?DEFAULT_SETUP_PROCESS, S, I});
parse({setup, S, C, I}) when is_function(S), is_function(C) ->
    parse({setup, ?DEFAULT_SETUP_PROCESS, S, C, I});
parse({setup, P, S, I}) when is_function(S) ->
    parse({setup, P, S, fun ok/1, I});
parse({setup, P, L, I} = T) when is_list(L) ->
    check_setup_list(L, T),
    {S, C} = eunit_test:multi_setup(L),
    parse({setup, P, S, C, I});
parse({setup, P, S, C, I} = T)
  when is_function(S), is_function(C), is_function(I) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    case erlang:fun_info(I, arity) of
	{arity, 0} ->
	    %% if I is nullary, it is a plain test
	    parse({setup, S, C, fun (_) -> I end});
	_ ->
	    %% otherwise, I must be an instantiator function
	    check_arity(I, 1, T),
	    case P of
		local -> ok;
		spawn -> ok;
		{spawn, N} when is_atom(N) -> ok;
		_ -> bad_test(T)
	    end,
	    group(#group{tests = I,
			 context = #context{setup = S, cleanup = C,
					    process = P}})
    end;
parse({setup, P, S, C, {with, As}}) when is_list(As) ->
    parse({setup, P, S, C, fun (X) -> {with, X, As} end});
parse({setup, P, S, C, T}) when is_function(S), is_function(C) ->
    parse({setup, P, S, C, fun (_) -> T end});
parse({node, N, T}) when is_atom(N) ->
    parse({node, N, "", T});
parse({node, N, A, T1}=T) when is_atom(N) ->
    case eunit_lib:is_string(A) of
	true ->
	    %% TODO: better stack traces for internal funs like these
	    parse({setup,
		   fun () ->
			   %% TODO: auto-start net_kernel if needed
 			   StartedNet = false,
%% The following is commented out because of problems when running
%% eunit as part of the init sequence (from the command line):
%% 			   StartedNet =
%% 			       case whereis(net_kernel) of
%% 				   undefined ->
%% 				       M = list_to_atom(atom_to_list(N)
%% 							++ "_master"),
%% 				       case net_kernel:start([M]) of
%% 					   {ok, _} ->
%% 					       true;
%% 					   {error, E} ->
%% 					       throw({net_kernel_start, E})
%% 				       end;
%% 				   _ -> false
%% 			       end,
%% 			   ?debugVal({started, StartedNet}),
			   {Name, Host} = eunit_lib:split_node(N),
			   {ok, Node} = slave:start_link(Host, Name, A),
			   {Node, StartedNet}
		   end,
		   fun ({Node, StopNet}) ->
%% 			   ?debugVal({stop, StopNet}),
			   slave:stop(Node),
			   case StopNet of
			       true -> net_kernel:stop();
			       false -> ok
			   end
		   end,
		   T1});
	false ->
	    bad_test(T)
    end;
parse({module, M}) when is_atom(M) ->
    {data, {"module '" ++ atom_to_list(M) ++ "'", get_module_tests(M)}};
parse({application, A}) when is_atom(A) ->
    try parse({file, atom_to_list(A)++".app"})
    catch
	{file_read_error,{enoent,_,_}} ->
	    case code:lib_dir(A) of
		Dir when is_list(Dir) ->
		    %% add "ebin" if it exists, like code_server does
		    BinDir = filename:join(Dir, "ebin"),
		    case file:read_file_info(BinDir) of
			{ok, #file_info{type=directory}} ->
			    parse({dir, BinDir});
			_ ->
			    parse({dir, Dir})
		    end;
		_ ->
		    throw({application_not_found, A})
	    end
    end;
parse({application, A, Info}=T) when is_atom(A) ->
    case proplists:get_value(modules, Info) of
	Ms when is_list(Ms) ->
	    case [M || M <- Ms, not is_atom(M)] of
		[] ->
		    {data, {"application '" ++ atom_to_list(A) ++ "'", Ms}};
		_ ->
		    bad_test(T)
	    end;
	_ ->
	    bad_test(T)
    end;
parse({file, F} = T) when is_list(F) ->
    case eunit_lib:is_string(F) of
	true ->
	    {data, {"file \"" ++ F ++ "\"", get_file_tests(F)}};
	false ->
	    bad_test(T)
    end;
parse({dir, D}=T) when is_list(D) ->
    case eunit_lib:is_string(D) of
	true ->
	    {data, {"directory \"" ++ D ++ "\"",
		    get_directory_module_tests(D)}};
	false ->
	    bad_test(T)
    end;
parse({with, X, As}=T) when is_list(As) ->
    case As of
	[A | As1] ->
	    check_arity(A, 1, T),
	    {data, [{eunit_lib:fun_parent(A), fun () -> A(X) end},
		    {with, X, As1}]};
	[] ->
	    {data, []}
    end;
parse({S, T1} = T) when is_list(S) ->
    case eunit_lib:is_string(S) of
	true ->
	    group(#group{tests = T1, desc = unicode:characters_to_binary(S)});
	false ->
	    bad_test(T)
    end;
parse({S, T1}) when is_binary(S) ->
    group(#group{tests = T1, desc = S});
parse(T) when is_tuple(T), size(T) > 2, is_list(element(1, T)) ->
    [S | Es] = tuple_to_list(T),
    parse({S, list_to_tuple(Es)});
parse(T) when is_tuple(T), size(T) > 2, is_binary(element(1, T)) ->
    [S | Es] = tuple_to_list(T),
    parse({S, list_to_tuple(Es)});
parse(M) when is_atom(M) ->
    parse({module, M});
parse(T) when is_list(T) ->
    case eunit_lib:is_string(T) of
	true ->
	    try parse({dir, T})
	    catch
		{file_read_error,{R,_,_}}
		  when R =:= enotdir; R =:= enoent ->
		    parse({file, T})
	    end;
	false ->
	    bad_test(T)
    end;
parse(T) ->
    parse_simple(T).

%% parse_simple always produces a #test{} record

parse_simple({L, F}) when is_integer(L), L >= 0 ->
    (parse_simple(F))#test{line = L};
parse_simple({{M,N,A}=Loc, F}) when is_atom(M), is_atom(N), is_integer(A) ->
    (parse_simple(F))#test{location = Loc};
parse_simple(F) ->
    parse_function(F).

parse_function(F) when is_function(F) ->
    check_arity(F, 0, F),
    #test{f = F, location = eunit_lib:fun_parent(F)};
parse_function({test, M, F}) when is_atom(M), is_atom(F) ->
    #test{f = eunit_test:mf_wrapper(M, F), location = {M, F, 0}};
parse_function({M, F}) when is_atom(M), is_atom(F) ->
    %% {M,F} is now considered obsolete; use {test,M,F} instead
    parse_function({test, M, F});
parse_function(F) ->
    bad_test(F).

check_arity(F, N, _) when is_function(F, N) ->
    ok;
check_arity(_, _, T) ->
    bad_test(T).

check_setup_list([{Tag, S, C} | Es], T)
  when is_atom(Tag), is_function(S), is_function(C) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    check_setup_list(Es, T);
check_setup_list([{Tag, S} | Es], T)
  when is_atom(Tag), is_function(S) ->
    check_arity(S, 0, T),
    check_setup_list(Es, T);
check_setup_list([], _T) ->
    ok;
check_setup_list(_, T) ->
    bad_test(T).

bad_test(T) ->
    throw({bad_test, T}).

ok(_) -> ok.
ok(_, _) -> ok.

%% This does some look-ahead and folds nested groups and tests where
%% possible. E.g., {String, Test} -> Test#test{desc = String}.

group(#group{context = #context{}} = G) ->
    %% leave as it is - the test body is an instantiator, which is not
    %% suitable for lookahead (and anyway, properties of the setup
    %% should not be merged with properties of its body, e.g. spawn)
    G;
group(#group{tests = T0, desc = Desc, order = Order, context = Context,
	     spawn = Spawn, timeout = Timeout} = G) ->
    {T1, Ts} = lookahead(T0),
    {T2, _} = lookahead(Ts),
    case T1 of
	#test{desc = Desc1, timeout = Timeout1}
	when T2 =:= none, Spawn =:= undefined, Context =:= undefined,
	     ((Desc =:= undefined) or (Desc1 =:= undefined)),
	     ((Timeout =:= undefined) or (Timeout1 =:= undefined)) ->
	    %% a single test within a non-spawn/setup group: put the
	    %% information directly on the test; drop the order
	    T1#test{desc = join_properties(Desc, Desc1),
		    timeout = join_properties(Timeout, Timeout1)};

	#test{timeout = undefined}
	when T2 =:= none, Timeout =/= undefined, Context =:= undefined ->
	    %% a single test without timeout, within a non-joinable
	    %% group with a timeout and no fixture: push the timeout to
	    %% the test
	    G#group{tests = {timeout, (Timeout div ?TICKS_PER_SECOND), T0},
		    timeout = undefined};

	#group{desc = Desc1, order = Order1, context = Context1,
	       spawn = Spawn1, timeout = Timeout1}
	when T2 =:= none,
	     ((Desc =:= undefined) or (Desc1 =:= undefined)),
	     ((Order =:= undefined) or (Order1 =:= undefined)),
	     ((Context =:= undefined) or (Context1 =:= undefined)),
	     ((Spawn =:= undefined) or (Spawn1 =:= undefined)),
	     ((Timeout =:= undefined) or (Timeout1 =:= undefined)) ->
	    %% two nested groups with non-conflicting properties
	    group(T1#group{desc = join_properties(Desc, Desc1),
			   order = join_properties(Order, Order1),
			   context = join_properties(Context, Context1),
			   spawn = join_properties(Spawn, Spawn1),
			   timeout = join_properties(Timeout, Timeout1)});

	#group{order = Order1, timeout = Timeout1}
	when T2 =:= none ->
	    %% two nested groups that cannot be joined: try to push the
	    %% timeout and ordering properties to the inner group
	    push_order(Order, Order1, push_timeout(Timeout, Timeout1, G));

	_ ->
	    %% leave the group as it is and discard the lookahead
	    G
    end.

lookahead(T) ->
    case next(T) of
	{T1, Ts} -> {T1, Ts};
	none -> {none, []}
    end.

join_properties(undefined, X) -> X;    
join_properties(X, undefined) -> X.

push_timeout(Timeout, undefined, G=#group{context=undefined})
  when Timeout =/= undefined ->
    %% A timeout on a context (fixture) includes the setup/cleanup time
    %% and must not be propagated into the body
    G#group{tests = {timeout, (Timeout div ?TICKS_PER_SECOND), G#group.tests},
	    timeout = undefined};
push_timeout(_, _, G) ->
    G.

push_order(inorder, undefined, G) ->
    G#group{tests = {inorder, G#group.tests}, order = undefined};
push_order({inparallel, N}, undefined, G) ->
    G#group{tests = {inparallel, N, G#group.tests}, order = undefined};
push_order(_, _, G) ->
    G.

%% ---------------------------------------------------------------------
%% Extracting test funs from a module

%% @throws {module_not_found, moduleName()}

get_module_tests(M) ->
    try M:module_info(exports) of
	Es ->
	    Fs = get_module_tests_1(M, Es),
	    W = ?DEFAULT_MODULE_WRAPPER_NAME,
	    case lists:member({W,1}, Es) of
		false -> Fs;
		true -> {generator, fun () -> M:W(Fs) end}
	    end
    catch
	error:undef -> 
	    throw({module_not_found, M})
    end.

get_module_tests_1(M, Es) ->
    Fs = testfuns(Es, M, ?DEFAULT_TEST_SUFFIX,
		  ?DEFAULT_GENERATOR_SUFFIX),
    Name = atom_to_list(M),
    case lists:suffix(?DEFAULT_TESTMODULE_SUFFIX, Name) of
	false ->
	    Name1 = Name ++ ?DEFAULT_TESTMODULE_SUFFIX,
	    M1 = list_to_atom(Name1),
	    try get_module_tests(M1) of
		Fs1 ->
		    Fs ++ [{"module '" ++ Name1 ++ "'", Fs1}]
	    catch
		{module_not_found, M1} ->
		    Fs
	    end;
	true ->
	    Fs
    end.

testfuns(Es, M, TestSuffix, GeneratorSuffix) ->
    foldr(fun ({F, 0}, Fs) ->
		  N = atom_to_list(F),
		  case lists:suffix(TestSuffix, N) of
		      true ->
			  [{test, M, F} | Fs];
		      false ->
			  case lists:suffix(GeneratorSuffix, N) of
			      true ->
				  [{generator, M, F} | Fs];
			      false ->
				  Fs
			  end
		  end;
	      (_, Fs) ->
		  Fs
	  end,
	  [],
	  Es).    


%% ---------------------------------------------------------------------
%% Getting a test set from a file (text file or object file)

%% @throws {file_read_error, {Reason::atom(), Message::string(),
%%                            fileName()}}

get_file_tests(F) ->
    case is_module_filename(F) of
	true ->
	    %% look relative to current dir first
	    case file:read_file_info(F) of
		{ok, #file_info{type=regular}} ->
		    objfile_test(F);
		_ ->
		    %% (where_is_file/1 does not take a path argument)
		    case code:where_is_file(F) of
			non_existing ->
			    %% this will produce a suitable error message
			    objfile_test(F);
			Path ->
			    objfile_test(Path)
		    end
	    end;
	false ->
	    eunit_lib:consult_file(F)
    end.

is_module_filename(F) ->
    filename:extension(F) =:= code:objfile_extension().

objfile_test({M, File}) ->
    {setup,
     fun () ->
	     %% TODO: better error/stacktrace for this internal fun
	     code:purge(M),
	     {module,M} = code:load_abs(filename:rootname(File)),
	     ok
     end,
     {module, M}};
objfile_test(File) ->
    objfile_test({objfile_module(File), File}).

objfile_module(File) ->
    try
	{value, {module, M}} = lists:keysearch(module, 1,
					       beam_lib:info(File)),
	M
    catch
	_:_ ->
	    throw({file_read_error,
		   {undefined, "extracting module name failed", File}})
    end.


%% ---------------------------------------------------------------------
%% Getting a set of module tests from the object files in a directory

%% @throws {file_read_error,
%%          {Reason::atom(), Message::string(), fileName()}}

get_directory_module_tests(D) ->
    Ms = get_directory_modules(D),
    %% for all 'm' in the set, remove 'm_tests' if present
    F = fun ({M,_}, S) ->
		Name = atom_to_list(M),
		case lists:suffix(?DEFAULT_TESTMODULE_SUFFIX, Name) of
		    false ->
			Name1 = Name ++ ?DEFAULT_TESTMODULE_SUFFIX,
			M1 = list_to_atom(Name1),
			dict:erase(M1, S);
		    true ->
			S
		end
	end,
    [objfile_test(Obj)
     || Obj <- dict:to_list(lists:foldl(F, dict:from_list(Ms), Ms))].

%% TODO: handle packages (recursive search for files)
get_directory_modules(D) ->
    [begin
	 F1 = filename:join(D, F),
	 {objfile_module(F1), F1}
     end
     || F <- eunit_lib:list_dir(D), is_module_filename(F)].



%% ---------------------------------------------------------------------
%% Entering a setup-context, with guaranteed cleanup.

%% @spec (Tests::#context{}, Instantiate, Callback) -> any()
%%    Instantiate = (any()) -> tests()
%%    Callback = (tests()) -> any()
%% @throws {context_error, Error, eunit_lib:exception()}
%% Error = setup_failed | instantiation_failed | cleanup_failed

enter_context(#context{setup = S, cleanup = C, process = P}, I, F) ->
    F1 = case P of
	     local -> F;
	     spawn -> fun (X) -> F({spawn, X}) end;
	     {spawn, N} -> fun (T) -> F({spawn, N, T}) end
	 end,
    eunit_test:enter_context(S, C, I, F1).


-ifdef(TEST).
generator_exported_() ->
    generator().

generator() ->
    T = ?_test(ok),
    [T, T, T].

echo_proc() ->
    receive {P,X} -> P ! X, echo_proc() end.

ping(P) ->
    P ! {self(),ping}, receive ping -> ok end.    

data_test_() ->
    Setup = fun () -> spawn(fun echo_proc/0) end,
    Cleanup = fun (Pid) -> exit(Pid, kill) end,
    Fail = ?_test(throw(eunit)),
    T = ?_test(ok),
    Tests = [T,T,T],
    [?_assertMatch(ok, eunit:test(T)),
     ?_assertMatch(error, eunit:test(Fail)),
     ?_assertMatch(ok, eunit:test({test, ?MODULE, trivial_test})),
     ?_assertMatch(ok, eunit:test({generator, fun () -> Tests end})),
     ?_assertMatch(ok, eunit:test({generator, fun generator/0})),
     ?_assertMatch(ok, eunit:test({generator, ?MODULE, generator_exported_})),
     ?_assertMatch(ok, eunit:test({inorder, Tests})),
     ?_assertMatch(ok, eunit:test({inparallel, Tests})),
     ?_assertMatch(ok, eunit:test({timeout, 10, Tests})),
     ?_assertMatch(ok, eunit:test({spawn, Tests})),
     ?_assertMatch(ok, eunit:test({setup, Setup, Cleanup,
				   fun (P) -> ?_test(ok = ping(P)) end})),
     %%?_assertMatch(ok, eunit:test({node, test@localhost, Tests})),
     ?_assertMatch(ok, eunit:test({module, eunit_lib})),
     ?_assertMatch(ok, eunit:test(eunit_lib)),
     ?_assertMatch(ok, eunit:test("examples/tests.txt"))

     %%?_test({foreach, Setup, [T, T, T]})
    ].

trivial_test() ->
    ok.

trivial_generator_test_() ->
    [?_test(ok)].

lazy_test_() ->
    {spawn, [?_test(undefined = put(count, 0)),
	     lazy_gen(7),
	     ?_assertMatch(7, get(count))]}.

-dialyzer({no_improper_lists, lazy_gen/1}).
lazy_gen(N) ->
    {generator,
     fun () ->
	     if N > 0 ->
		     [?_test(put(count,1+get(count)))
		      | lazy_gen(N-1)];
		true ->
		     []
	     end
     end}.
-endif.
