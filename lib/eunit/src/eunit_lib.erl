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
%% @copyright 2004-2007 Mickaël Rémond, Richard Carlsson
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @private
%% @see eunit
%% @doc Utility functions for eunit

-module(eunit_lib).

-include("eunit.hrl").
-include("eunit_internal.hrl").


-export([dlist_next/1, uniq/1, fun_parent/1, is_string/1, command/1,
	 command/2, command/3, trie_new/0, trie_store/2, trie_match/2,
	 split_node/1, consult_file/1, list_dir/1, format_exit_term/1,
	 format_exception/1, format_exception/2, format_error/1,
         is_not_test/1]).


%% Type definitions for describing exceptions
%%
%% @type exception() = {exceptionClass(), Reason::term(), stackTrace()}
%%
%% @type exceptionClass() = error | exit | throw
%%
%% @type stackTrace() = [{moduleName(), functionName(), arity() | argList()}]
%%
%% @type moduleName() = atom()
%% @type functionName() = atom()
%% @type argList() = [term()]
%% @type fileName() = string()


%% ---------------------------------------------------------------------
%% Formatting of error descriptors
format_exception(Exception) ->
    format_exception(Exception, 20).

format_exception({Class,Term,Trace}, Depth)
  when is_atom(Class), is_list(Trace) ->
    case is_stacktrace(Trace) of
	true ->
	    io_lib:format("~ts**~w:~ts",
			  [format_stacktrace(Trace), Class,
                           format_term(Term, Depth)]);
	false ->
	    format_term(Term, Depth)
    end;
format_exception(Term, Depth) ->
    format_term(Term, Depth).

format_term(Term, Depth) ->
    io_lib:format("~tP\n", [Term, Depth]).

format_exit_term(Term) ->
    {Reason, Trace} = analyze_exit_term(Term),
    io_lib:format("~tP~ts", [Reason, 15, Trace]).

analyze_exit_term({Reason, [_|_]=Trace}=Term) ->
    case is_stacktrace(Trace) of
	true ->
	    {Reason, format_stacktrace(Trace)};
	false ->
	    {Term, ""}
    end;
analyze_exit_term(Term) ->
    {Term, ""}.

is_stacktrace([]) ->
    true;
is_stacktrace([{M,F,A,L}|Fs])
  when is_atom(M), is_atom(F), is_integer(A), is_list(L) ->
    is_stacktrace(Fs);
is_stacktrace([{M,F,As,L}|Fs])
  when is_atom(M), is_atom(F), is_list(As), is_list(L) ->
    is_stacktrace(Fs);
is_stacktrace([{M,F,A}|Fs]) when is_atom(M), is_atom(F), is_integer(A) ->
    is_stacktrace(Fs);
is_stacktrace([{M,F,As}|Fs]) when is_atom(M), is_atom(F), is_list(As) ->
    is_stacktrace(Fs);
is_stacktrace(_) ->
    false.

format_stacktrace(Trace) ->
    format_stacktrace(Trace, "in function", "in call from").

format_stacktrace([{M,F,A,L}|Fs], Pre, Pre1) when is_integer(A) ->
    [io_lib:fwrite("~ts ~w:~w/~w~ts\n",
                   [Pre, M, F, A, format_stacktrace_location(L)])
     | format_stacktrace(Fs, Pre1, Pre1)];
format_stacktrace([{M,F,As,L}|Fs], Pre, Pre1) when is_list(As) ->
    A = length(As),
    C = case is_op(M,F,A) of
	    true when A =:= 1 ->
		[A1] = As,
		io_lib:fwrite("~ts ~ts", [F,format_arg(A1)]);
	    true when A =:= 2 ->
		[A1, A2] = As,
		io_lib:fwrite("~ts ~ts ~ts",
			      [format_arg(A1),F,format_arg(A2)]);
	    false ->
		io_lib:fwrite("~w(~ts)", [F,format_arglist(As)])
	end,
    [io_lib:fwrite("~ts ~w:~w/~w~ts\n  called as ~ts\n",
		   [Pre,M,F,A,format_stacktrace_location(L),C])
     | format_stacktrace(Fs,Pre1,Pre1)];
format_stacktrace([{M,F,As}|Fs], Pre, Pre1) ->
    format_stacktrace([{M,F,As,[]}|Fs], Pre, Pre1);
format_stacktrace([],_Pre,_Pre1) ->
    "".

format_stacktrace_location(Location) ->
    File = proplists:get_value(file, Location),
    Line = proplists:get_value(line, Location),
    if File =/= undefined, Line =/= undefined ->
            io_lib:format(" (~ts, line ~w)", [File, Line]);
       true ->
            ""
    end.

format_arg(A) ->
    io_lib:format("~tP",[A,15]).

format_arglist([A]) ->
    format_arg(A);
format_arglist([A|As]) ->
    [io_lib:format("~tP,",[A,15]) | format_arglist(As)];
format_arglist([]) ->
    "".

is_op(erlang, F, A) ->
    erl_internal:arith_op(F, A)
	orelse erl_internal:bool_op(F, A)
	orelse erl_internal:comp_op(F, A)
	orelse erl_internal:list_op(F, A)
	orelse erl_internal:send_op(F, A);
is_op(_M, _F, _A) ->
    false.

format_error({bad_test, Term}) ->
    error_msg("bad test descriptor", "~tP", [Term, 15]);
format_error({bad_generator, {{M,F,A}, Term}}) ->
    error_msg(io_lib:format("result from generator ~w:~w/~w is not a test",
                            [M,F,A]),
              "~tP", [Term, 15]);
format_error({generator_failed, {{M,F,A}, Exception}}) ->
    error_msg(io_lib:format("test generator ~w:~w/~w failed",[M,F,A]),
              "~ts", [format_exception(Exception)]);
format_error({no_such_function, {M,F,A}})
  when is_atom(M), is_atom(F), is_integer(A) ->
    error_msg(io_lib:format("no such function: ~w:~w/~w", [M,F,A]),
	      "", []);
format_error({module_not_found, M}) ->
    error_msg("test module not found", "~tp", [M]);
format_error({application_not_found, A}) when is_atom(A) ->
    error_msg("application not found", "~w", [A]);
format_error({file_read_error, {_R, Msg, F}}) ->
    error_msg("error reading file", "~ts: ~ts", [Msg, F]);
format_error({setup_failed, Exception}) ->
    error_msg("context setup failed", "~ts",
	      [format_exception(Exception)]);
format_error({cleanup_failed, Exception}) ->
    error_msg("context cleanup failed", "~ts",
	      [format_exception(Exception)]);
format_error({{bad_instantiator, {{M,F,A}, Term}}, _DummyException}) ->
    error_msg(io_lib:format("result from instantiator ~w:~w/~w is not a test",
                            [M,F,A]),
              "~tP", [Term, 15]);
format_error({instantiation_failed, Exception}) ->
    error_msg("instantiation of subtests failed", "~ts",
	      [format_exception(Exception)]).

error_msg(Title, Fmt, Args) ->
    Msg = io_lib:format("**"++Fmt, Args),    % gets indentation right
    io_lib:fwrite("*** ~ts ***\n~ts\n\n", [Title, Msg]).

-ifdef(TEST).
format_exception_test_() ->
    [?_assertMatch(
        "\nymmud:rorre"++_,
        lists:reverse(lists:flatten(
          format_exception(try erlang:error(dummy)
                           catch C:R -> {C, R, erlang:get_stacktrace()}
                           end)))),
     ?_assertMatch(
        "\nymmud:rorre"++_,
        lists:reverse(lists:flatten(
          format_exception(try erlang:error(dummy, [a])
                           catch C:R -> {C, R, erlang:get_stacktrace()}
                           end))))].
-endif.

%% ---------------------------------------------------------------------
%% detect common return values that are definitely not tests

is_not_test(T) ->
    case T of
        ok -> true;
        error -> true;
        true -> true;
        false -> true;
        undefined -> true;
        {ok, _} -> true;
        {error, _} -> true;
        {'EXIT', _} -> true;
        N when is_number(N) -> true;
        [N|_] when is_number(N) -> true;
        X when is_binary(X) -> true;
        X when is_pid(X) -> true;
        X when is_port(X) -> true;
        X when is_reference(X) -> true;
        _ -> false
    end.

%% ---------------------------------------------------------------------
%% Deep list iterator; accepts improper lists/sublists, and also accepts
%% non-lists on the top level. Nonempty strings (not deep strings) are
%% recognized as separate elements, even on the top level. (It is not
%% recommended to include integers in the deep list, since a list of
%% integers is likely to be interpreted as a string.). The result is
%% always presented as a list (which may be improper), which is either
%% empty or otherwise has a non-list head element.

dlist_next([X | Xs] = Xs0) when is_list(X) ->
    case is_nonempty_string(X) of
	true -> Xs0;
	false -> dlist_next(X, Xs)
    end;
dlist_next([_|_] = Xs) ->
    case is_nonempty_string(Xs) of
	true -> [Xs];
	false -> Xs
    end;
dlist_next([]) ->
    [];
dlist_next(X) ->
    [X].

%% the first two clauses avoid pushing empty lists on the stack
dlist_next([X], Ys) when is_list(X) ->
    case is_nonempty_string(X) of
	true -> [X | Ys];
	false -> dlist_next(X, Ys)
    end;
dlist_next([X], Ys) ->
    [X | Ys];
dlist_next([X | Xs], Ys) when is_list(X) ->
    case is_nonempty_string(X) of
	true -> [X | [Xs | Ys]];
	false -> dlist_next(X, [Xs | Ys])
    end;
dlist_next([X | Xs], Ys) ->
    [X | [Xs | Ys]];
dlist_next([], Xs) ->
    dlist_next(Xs).


-ifdef(TEST).
dlist_test_() ->
    {"deep list traversal",
     [{"non-list term -> singleton list",
       ?_test([any] = dlist_next(any))},
      {"empty list -> empty list",
       ?_test([] = dlist_next([]))},
      {"singleton list -> singleton list",
       ?_test([any] = dlist_next([any]))},
      {"taking the head of a flat list",
       ?_test([a,b,c] = dlist_next([a,b,c]))},
      {"skipping an initial empty list",
       ?_test([a,b,c] = dlist_next([[],a,b,c]))},
      {"skipping nested initial empty lists",
       ?_test([a,b,c] = dlist_next([[[[]]],a,b,c]))},
      {"skipping a final empty list",
       ?_test([] = dlist_next([[]]))},
      {"skipping nested final empty lists",
       ?_test([] = dlist_next([[[[]]]]))},
      {"the first element is in a sublist",
       ?_test([a,b,c] = dlist_next([[a],b,c]))},
      {"recognizing a naked string",
       ?_test(["abc"] = dlist_next("abc"))},
      {"recognizing a wrapped string",
       ?_test(["abc"] = dlist_next(["abc"]))},
      {"recognizing a leading string",
       ?_test(["abc",a,b,c] = dlist_next(["abc",a,b,c]))},
      {"recognizing a nested string",
       ?_test(["abc"] = dlist_next([["abc"]]))},
      {"recognizing a leading string in a sublist",
       ?_test(["abc",a,b,c] = dlist_next([["abc"],a,b,c]))},
      {"traversing an empty list",
       ?_test([] = dlist_flatten([]))},
      {"traversing a flat list",
       ?_test([a,b,c] = dlist_flatten([a,b,c]))},
      {"traversing a deep list",
       ?_test([a,b,c] = dlist_flatten([[],[a,[b,[]],c],[]]))},
      {"traversing a deep but empty list",
       ?_test([] = dlist_flatten([[],[[[]]],[]]))}
     ]}.

%% test support
dlist_flatten(Xs) ->
    case dlist_next(Xs) of
	[X | Xs1] -> [X | dlist_flatten(Xs1)];
	[] -> []
    end.
-endif.


%% ---------------------------------------------------------------------
%% Check for proper Unicode-stringness.

is_string([C | Cs]) when is_integer(C), C >= 0, C =< 16#10ffff ->
    is_string(Cs);
is_string([_ | _]) ->
    false;
is_string([]) ->
    true;
is_string(_) ->
    false.

is_nonempty_string([]) -> false;
is_nonempty_string(Cs) -> is_string(Cs).

-ifdef(TEST).
is_string_test_() ->
    {"is_string",
     [{"no non-lists", ?_assert(not is_string($A))},
      {"no non-integer lists", ?_assert(not is_string([true]))},
      {"empty string", ?_assert(is_string(""))},
      {"ascii string", ?_assert(is_string(lists:seq(0, 127)))},
      {"latin-1 string", ?_assert(is_string(lists:seq(0, 255)))},
      {"unicode string",
       ?_assert(is_string([0, $A, 16#10fffe, 16#10ffff]))},
      {"not above unicode range",
       ?_assert(not is_string([0, $A, 16#110000]))},
      {"no negative codepoints", ?_assert(not is_string([$A, -1, 0]))}
     ]}.
-endif.


%% ---------------------------------------------------------------------
%% Splitting a full node name into basename and hostname,
%% using 'localhost' as the default hostname

split_node(N) when is_atom(N) -> split_node(atom_to_list(N));
split_node(Cs) -> split_node_1(Cs, []).

split_node_1([$@ | Cs], As) -> split_node_2(As, Cs);
split_node_1([C | Cs], As) -> split_node_1(Cs, [C | As]);
split_node_1([], As) ->  split_node_2(As, "localhost").

split_node_2(As, Cs) ->
    {list_to_atom(lists:reverse(As)), list_to_atom(Cs)}.

%% ---------------------------------------------------------------------
%% Get the name of the containing function for a fun. (This is encoded
%% in the name of the generated function that implements the fun.)
fun_parent(F) ->
    {module, M} = erlang:fun_info(F, module),
    {name, N} = erlang:fun_info(F, name),
    case erlang:fun_info(F, type) of
	{type, external} ->
	    {arity, A} = erlang:fun_info(F, arity),
	    {M, N, A};
	{type, local} ->
	    [$-|S] = atom_to_list(N),
	    C1 = string:chr(S, $/),
	    C2 = string:chr(S, $-),
	    {M, list_to_atom(string:sub_string(S, 1, C1 - 1)),
	     list_to_integer(string:sub_string(S, C1 + 1, C2 - 1))}
    end.

-ifdef(TEST).
fun_parent_test() ->
    {?MODULE,fun_parent_test,0} = fun_parent(fun () -> ok end).
-endif.

%% ---------------------------------------------------------------------
%% Ye olde uniq function

uniq([X, X | Xs]) -> uniq([X | Xs]);    
uniq([X | Xs]) -> [X | uniq(Xs)];
uniq([]) -> [].

-ifdef(TEST).
-dialyzer({[no_fail_call, no_improper_lists], uniq_test_/0}).
uniq_test_() ->
    {"uniq",
     [?_assertError(function_clause, uniq(ok)),
      ?_assertError(function_clause, uniq([1|2])),
      ?_test([] = uniq([])),
      ?_test([1,2,3] = uniq([1,2,3])),
      ?_test([1,2,3] = uniq([1,2,2,3])),
      ?_test([1,2,3,2,1] = uniq([1,2,2,3,2,2,1])),
      ?_test([1,2,3] = uniq([1,1,1,2,2,2,3,3,3])),
      ?_test(["1","2","3"] = uniq(["1","1","2","2","3","3"]))
     ]}.
-endif.

%% ---------------------------------------------------------------------
%% Replacement for os:cmd

%% TODO: Better cmd support, especially on Windows (not much tested)
%% TODO: Can we capture stderr separately somehow?

command(Cmd) ->
    command(Cmd, "").

command(Cmd, Dir) ->
    command(Cmd, Dir, []).

command(Cmd, Dir, Env) ->
    CD = if Dir =:= "" -> [];
	    true -> [{cd, Dir}]
	 end,
    SetEnv = if Env =:= [] -> []; 
		true -> [{env, Env}]
	     end,
    Opt = CD ++ SetEnv ++ [stream, exit_status, use_stdio,
			   stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, []).

get_data(P, D) ->
    receive
	{P, {data, D1}} ->
	    get_data(P, [D1|D]);
	{P, eof} ->
	    port_close(P),    
	    receive
		{P, {exit_status, N}} ->
		    {N, normalize(lists:flatten(lists:reverse(D)))}
	    end
    end.

normalize([$\r, $\n | Cs]) ->
    [$\n | normalize(Cs)];
normalize([$\r | Cs]) ->
    [$\n | normalize(Cs)];
normalize([C | Cs]) ->
    [C | normalize(Cs)];
normalize([]) ->
    [].

-ifdef(TEST).

-dialyzer({no_match, cmd_test_/0}).
cmd_test_() ->
    ([?_test({0, "hello\n"} = ?_cmd_("echo hello"))]
     ++ case os:type() of
	    {unix, _} ->
		unix_cmd_tests();
	    {win32, _} ->
		win32_cmd_tests();
	    _ ->
		[]
	end).

unix_cmd_tests() ->
    [{"command execution, status, and output",
      [?_cmd("echo hello"),
       ?_assertCmdStatus(0, "true"),
       ?_assertCmdStatus(1, "false"),
       ?_assertCmd("true"),
       ?_assertCmdOutput("hello\n", "echo hello"),
       ?_assertCmdOutput("hello", "echo -n hello")
      ]},
     {"file setup and cleanup",
      setup,
      fun () -> ?cmd("mktemp tmp.XXXXXXXX") end,
      fun (File) -> ?cmd("rm " ++ File) end,
      fun (File) ->
	      [?_assertCmd("echo xyzzy >" ++ File),
	       ?_assertCmdOutput("xyzzy\n", "cat " ++ File)]
      end}
    ].

win32_cmd_tests() ->
    [{"command execution, status, and output",
      [?_cmd("echo hello"),
       ?_assertCmdOutput("hello\n", "echo hello")
      ]}
    ].

-endif. % TEST


%% ---------------------------------------------------------------------
%% Wrapper around file:path_consult

%% @throws {file_read_error, {Reason::atom(), Message::string(),
%%                            fileName()}}

consult_file(File) ->
    case file:path_consult(["."]++code:get_path(), File) of
	{ok, Data, _Path} ->
	    Data;
	{error, Reason} ->
	    Msg = file:format_error(Reason),
	    throw({file_read_error, {Reason, Msg, File}})
    end.

%% ---------------------------------------------------------------------
%% Wrapper around file:list_dir

%% @throws {file_read_error, {Reason::atom(), Message::string(),
%%                            fileName()}}

list_dir(Dir) ->
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    Fs;
	{error, Reason} ->
	    Msg = file:format_error(Reason),
	    throw({file_read_error, {Reason, Msg, Dir}})
    end.

%% ---------------------------------------------------------------------
%% A trie for remembering and checking least specific cancelled events
%% (an empty list `[]' simply represents a stored empty list, i.e., all
%% events will match, while an empty tree means that no events match).

trie_new() ->
    gb_trees:empty().

trie_store([_ | _], []) ->
    [];
trie_store([E | Es], T) ->
    case gb_trees:lookup(E, T) of
	none ->
	    if Es =:= [] ->
		    gb_trees:insert(E, [], T);
	       true ->
		    gb_trees:insert(E, trie_store(Es, gb_trees:empty()),
				    T)
	    end;
	{value, []} ->
	    T;  %% prefix already stored
	{value, T1} ->
	    gb_trees:update(E, trie_store(Es, T1), T)
    end;
trie_store([], _T) ->
    [].

trie_match([_ | _], []) ->
    prefix;
trie_match([E | Es], T) ->
    case gb_trees:lookup(E, T) of
	none ->
	    no;
	{value, []} ->
	    if Es =:= [] -> exact;
	       true -> prefix
	    end;
	{value, T1} ->
	    trie_match(Es, T1)
    end;
trie_match([], []) ->
    exact;
trie_match([], _T) ->
    no.

-ifdef(TEST).

trie_test_() ->
    [{"basic representation",
      [?_assert(trie_new() =:= gb_trees:empty()),
       ?_assert(trie_store([1], trie_new())
		=:= gb_trees:insert(1, [], gb_trees:empty())),
       ?_assert(trie_store([1,2], trie_new())
		=:= gb_trees:insert(1,
				    gb_trees:insert(2, [],
						    gb_trees:empty()),
				    gb_trees:empty())),
       ?_assert([] =:= trie_store([1], [])),
       ?_assert([] =:= trie_store([], gb_trees:empty()))
      ]},
     {"basic storing and matching",
      [?_test(no = trie_match([], trie_new())),
       ?_test(exact = trie_match([], trie_store([], trie_new()))),
       ?_test(no = trie_match([], trie_store([1], trie_new()))),
       ?_test(exact = trie_match([1], trie_store([1], trie_new()))),
       ?_test(prefix = trie_match([1,2], trie_store([1], trie_new()))),
       ?_test(no = trie_match([1], trie_store([1,2], trie_new()))),
       ?_test(no = trie_match([1,3], trie_store([1,2], trie_new()))),
       ?_test(exact = trie_match([1,2,3,4,5],
				 trie_store([1,2,3,4,5], trie_new()))),
       ?_test(prefix = trie_match([1,2,3,4,5],
				  trie_store([1,2,3], trie_new()))),
       ?_test(no = trie_match([1,2,2,4,5],
			       trie_store([1,2,3], trie_new())))
      ]},
     {"matching with partially overlapping patterns",
      setup,
      fun () ->
	      trie_store([1,3,2], trie_store([1,2,3], trie_new()))
      end,
      fun (T) ->
	      [?_test(no = trie_match([], T)),
	       ?_test(no = trie_match([1], T)),
	       ?_test(no = trie_match([1,2], T)),
	       ?_test(no = trie_match([1,3], T)),
	       ?_test(exact = trie_match([1,2,3], T)),
	       ?_test(exact = trie_match([1,3,2], T)),
	       ?_test(no = trie_match([1,2,2], T)),
	       ?_test(no = trie_match([1,3,3], T)),
	       ?_test(prefix = trie_match([1,2,3,4], T)),
	       ?_test(prefix = trie_match([1,3,2,1], T))]
      end},
     {"matching with more general pattern overriding less general",
      setup,
      fun () -> trie_store([1], trie_store([1,2,3], trie_new())) end,
      fun (_) -> ok end,
      fun (T) ->
   	      [?_test(no = trie_match([], T)),
	       ?_test(exact = trie_match([1], T)),
 	       ?_test(prefix = trie_match([1,2], T)),
 	       ?_test(prefix = trie_match([1,2,3], T)),
 	       ?_test(prefix = trie_match([1,2,3,4], T))]
      end}
    ].

-endif.  % TEST
