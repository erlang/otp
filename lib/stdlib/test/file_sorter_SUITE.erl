%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(file_sorter_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t,test_server).
-define(privdir(_), "./file_sorter_SUITE_priv").
-else.
-include_lib("test_server/include/test_server.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), ?config(priv_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, basic/1, badarg/1, 
	 term_sort/1, term_keysort/1,
	 binary_term_sort/1, binary_term_keysort/1,
	 binary_sort/1, 
	 term_merge/1, term_keymerge/1, 
	 binary_term_merge/1, binary_term_keymerge/1,
	 binary_merge/1,
	 term_check/1, term_keycheck/1,
	 binary_term_check/1, binary_term_keycheck/1,
	 binary_check/1,
	 inout/1, misc/1, many/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

init_per_testcase(_Case, Config) ->
    Dog=?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, badarg, term_sort, term_keysort,
     binary_term_sort, binary_term_keysort, binary_sort,
     term_merge, term_keymerge, binary_term_merge,
     binary_term_keymerge, binary_merge, term_check,
     binary_term_keycheck, binary_term_check,
     binary_term_keycheck, binary_check, inout, misc, many].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


basic(doc) ->
    ["Basic test case."];
basic(suite) -> 
    [];
basic(Config) when is_list(Config) ->
    Fmt = binary,
    Arg = {format,Fmt},
    Foo = outfile("foo", Config),
    P0 = pps(),

    ?line F1s = [F1] = to_files([[]], Fmt, Config),
    ?line ok = file_sorter:sort(F1),
    ?line [] = from_files(F1, Fmt),
    ?line ok = file_sorter:keysort(17, F1),
    ?line [] = from_files(F1, Fmt),
    ?line ok = file_sorter:merge(F1s, Foo),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keymerge(17, F1s, Foo),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files([Foo | F1s]),

    ?line [F2] = to_files([[foo,bar]], Fmt, Config),
    ?line ok = file_sorter:sort([F2], F2, Arg),
    ?line [bar,foo] = from_files(F2, Fmt),
    ?line delete_files(F2),

    ?line Fs1 = to_files([[foo],[bar]], Fmt, Config),
    ?line ok = file_sorter:sort(Fs1, Foo, Arg),
    ?line [bar,foo] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:merge(Fs1, Foo, Arg),
    ?line [bar,foo] = from_files(Foo, Fmt),
    ?line delete_files([Foo | Fs1]),

    ?line Fmt2 = binary_term,
    ?line Arg2 = {format, Fmt2},
    ?line [F3] = to_files([[{foo,1},{bar,2}]], Fmt2, Config),
    ?line ok = file_sorter:keysort([2], [F3], F3, Arg2),
    ?line [{foo,1},{bar,2}] = from_files(F3, Fmt2),
    ?line delete_files(F3),

    ?line Fs2 = to_files([[{foo,1}],[{bar,2}]], Fmt2, Config),
    ?line ok = file_sorter:keysort(1, Fs2, Foo, Arg2),
    ?line [{bar,2},{foo,1}] = from_files(Foo, Fmt2),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keymerge(1, Fs2, Foo, Arg2),
    ?line [{bar,2},{foo,1}] = from_files(Foo, Fmt2),
    ?line delete_files([Foo | Fs2]),

    ?line true = P0 =:= pps(),

    ok.

badarg(doc) ->
    ["Call functions with bad arguments."];
badarg(suite) -> 
    [];
badarg(Config) when is_list(Config) ->
    PrivDir = ?privdir(Config),
    BadFile = filename:join(PrivDir, "not_a_file"),
    ABadFile = filename:absname(BadFile),
    ?line file:delete(BadFile),
    ?line {error,{file_error,ABadFile,enoent}} = 
	file_sorter:sort(BadFile),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:sort({flipp})),
    ?line {error,{file_error,ABadFile,enoent}} = 
	file_sorter:keysort(1, BadFile),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:keysort(1, {flipp})),

    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:merge([{flipp}],foo)),
    ?line {error,{file_error,ABadFile,enoent}} = 
	file_sorter:keymerge(1,[BadFile],foo),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:keymerge(1,[{flipp}],foo)),
    ?line {'EXIT', {{badarg, _}, _}} = 
	(catch file_sorter:merge(fun(X) -> X end, foo)),
    ?line {'EXIT', {{badarg, _}, _}} = 
	(catch file_sorter:keymerge(1, fun(X) -> X end, foo)),

    ?line {error,{file_error,ABadFile,enoent}} = 
	file_sorter:check(BadFile),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:check({flipp})),
    ?line {error,{file_error,ABadFile,enoent}} = 
	file_sorter:keycheck(1, BadFile),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:keycheck(1, {flipp})),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:check([{flipp}],foo)),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:keycheck(1,[{flipp}],foo)),
    ?line {'EXIT', {{badarg, _}, _}} = 
	(catch file_sorter:check(fun(X) -> X end, foo)),
    ?line {'EXIT', {{badarg, _}, _}} = 
	(catch file_sorter:keycheck(1, fun(X) -> X end, foo)),

    ?line Fs1 = to_files([[1,2,3]], binary_term, Config),
    ?line {'EXIT', {{badarg, flipp}, _}} = 
	(catch file_sorter:check(Fs1 ++ flipp, [])),
    [F1] = Fs1,
    ?line {error,{file_error,_,_}} = 
        file_sorter:sort(Fs1, foo, [{tmpdir,F1},{size,0}]),
    ?line delete_files(Fs1),
    ?line Fs2 = to_files([[1,2,3]], binary_term, Config),
    {error,{file_error,_,enoent}} = 
	file_sorter:sort(Fs2, foo, [{tmpdir,filename:absname(BadFile)},
                                    {size,0}]),
    ?line delete_files(Fs2),

    ?line {'EXIT', {{badarg, bad}, _}} = 
	(catch file_sorter:check([], [{format,term} | bad])),
    ?line {'EXIT', {{badarg, [{flipp}]}, _}} = 
	(catch file_sorter:check([{flipp}])),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch file_sorter:keycheck(1, {flipp})),
    ?line {'EXIT', {{badarg, [{flipp}]}, _}} = 
	(catch file_sorter:keycheck(2, [{flipp}])),
    ?line {error,{file_error,_,eisdir}} = file_sorter:keycheck(1, []),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch file_sorter:keycheck(kp, [])),
    ?line {'EXIT', {{badarg, kp}, _}} = 
	(catch file_sorter:keycheck([1, kp], [])),
    ?line {'EXIT', {{badarg, kp}, _}} = 
	(catch file_sorter:keycheck([1 | kp], [])),
    ?line {'EXIT', {{badarg, []}, _}} = (catch file_sorter:keycheck([], [])),
    ?line {'EXIT', {{badarg, {format, foo}}, _}} = 
	(catch file_sorter:check([], {format,foo})),
    ?line {'EXIT', {{badarg, not_an_option}, _}} = 
	(catch file_sorter:keycheck(7, [], [not_an_option])),
    ?line {'EXIT', {{badarg, format}, _}} = 
	(catch file_sorter:keycheck(1, [], [{format, binary}])),
    ?line {'EXIT', {{badarg, order}, _}} = 
	(catch file_sorter:keycheck(1, [], [{order, fun compare/2}])),

    ?line do_badarg(fun(I, O) -> file_sorter:sort(I, O) end,
		    fun(Kp, I, O) -> file_sorter:keysort(Kp, I, O) end,
		    BadFile),
    ?line do_badarg_opt(fun(I, O, X) -> file_sorter:sort(I, O, X) end,
			fun(Kp, I, O, X) -> file_sorter:keysort(Kp, I, O, X) 
			end),
    ?line do_badarg(fun(I, O) -> file_sorter:merge(I, O) end,
		    fun(Kp, I, O) -> file_sorter:keymerge(Kp, I, O) end, 
		    BadFile),
    ?line do_badarg_opt(fun(I, O, X) -> file_sorter:merge(I, O, X) end,
			fun(Kp, I, O, X) -> file_sorter:keymerge(Kp, I, O, X) 
			end).

do_badarg(F, KF, BadFile) ->
    [Char | _] = BadFile,
    AFlipp = filename:absname(flipp),
    ?line {error,{file_error,AFlipp,enoent}} = F([flipp | flopp], foo),
    ?line {'EXIT', {{badarg, {foo,bar}}, _}} = (catch F([], {foo,bar})),
    ?line {'EXIT', {{badarg, Char}, _}} = (catch F(BadFile, [])),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = (catch F({flipp}, [])),

    ?line {'EXIT', {{badarg, Char}, _}} = (catch KF(1, BadFile, [])),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = (catch KF(1, {flipp}, [])),
    ?line {error,{file_error,AFlipp,enoent}} = 
	KF(2, [flipp | flopp], foo),
    ?line {'EXIT', {{badarg, {foo,bar}}, _}} = (catch KF(1, [], {foo,bar})),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch KF(kp, [], foo)),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch KF([1, kp], [], foo)),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch KF([1 | kp], [], foo)),
    ?line {'EXIT', {{badarg, []}, _}} = (catch KF([], [], foo)),
    ok.

do_badarg_opt(F, KF) ->
    AFlipp = filename:absname(flipp),
    ?line {error,{file_error,AFlipp,enoent}} = 
	   F([flipp | flopp], foo, []),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = (catch F([{flipp}], foo, [])),
    ?line {'EXIT', {{badarg, {out,put}}, _}} = (catch F([], {out,put}, [])),
    ?line {'EXIT', {{badarg, not_an_option}, _}} = 
	(catch F([], foo, [not_an_option])),
    ?line {'EXIT', {{badarg, {format, foo}}, _}} = 
	   (catch F([], foo, {format,foo})),
    ?line {'EXIT', {{badarg, {size,foo}}, _}} = (catch F([], foo, {size,foo})),

    ?line {'EXIT', {{badarg, {size, -1}}, _}} = (catch F([], foo, {size,-1})),
    ?line {'EXIT', {{badarg, {no_files, foo}}, _}} = 
	(catch F([], foo, {no_files,foo})),
    ?line {'EXIT', {{badarg, {no_files, 1}}, _}} = 
	(catch F([], foo, {no_files,1})),
    ?line {'EXIT', {{badarg, 1}, _}} = (catch F([], foo, {tmpdir,1})),
    ?line {'EXIT', {{badarg, {order,1}}, _}} = (catch F([], foo, {order,1})),
    ?line {'EXIT', {{badarg, {compressed, flopp}}, _}} = 
	   (catch F([], foo, {compressed,flopp})),
    ?line {'EXIT', {{badarg, {unique,flopp}}, _}} = 
	   (catch F([], foo, {unique,flopp})),
    ?line {'EXIT', {{badarg, {header,foo}}, _}} = 
	(catch F([], foo, {header,foo})),
    ?line {'EXIT', {{badarg, {header, 0}}, _}} = 
	(catch F([], foo, {header,0})),
    ?line {'EXIT', {{badarg, {header, 1 bsl 35}}, _}} = 
	(catch F([], foo, {header,1 bsl 35})),
    ?line {'EXIT', {{badarg, header}, _}} = 
	(catch F([], foo, [{header,1},{format,term}])),

    ?line {'EXIT', {{badarg, not_an_option}, _}} = 
	(catch KF(7, [], foo, [not_an_option])),
    ?line {'EXIT', {{badarg,format}, _}} = 
	(catch KF(1, [], foo, [{format, binary}])),
    ?line {'EXIT', {{badarg, order}, _}} = 
	(catch KF(1, [], foo, [{order, fun compare/2}])),
    ?line {'EXIT', {{badarg, {flipp}}, _}} = 
	(catch KF(2, [{flipp}], foo,[])),
    ?line {error,{file_error,AFlipp,enoent}} = 
	KF(2, [flipp | flopp], foo,[]),
    ?line {'EXIT', {{badarg, {out, put}}, _}} = 
	(catch KF(1, [], {out,put}, [])),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch KF(kp, [], foo, [])),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch KF([1, kp], [], foo, [])),
    ?line {'EXIT', {{badarg, kp}, _}} = (catch KF([1 | kp], [], foo, [])),
    ok.

term_sort(doc) ->
    ["Sort terms on files."];
term_sort(suite) ->
    [];
term_sort(Config) when is_list(Config) ->
    ?line sort(term, [{compressed,false}], Config),
    ?line sort(term, [{order, fun compare/2}], Config),
    ?line sort(term, [{order, ascending}, {compressed,true}], Config),
    ?line sort(term, [{order, descending}], Config),
    ok.

term_keysort(doc) ->
    ["Keysort terms on files."];
term_keysort(suite) ->
    [];
term_keysort(Config) when is_list(Config) ->
    ?line keysort(term, [{tmpdir, ""}], Config),
    ?line keysort(term, [{order,descending}], Config),
    ok.

binary_term_sort(doc) ->
    ["Sort binary terms on files."];
binary_term_sort(suite) ->
    [];
binary_term_sort(Config) when is_list(Config) ->
    PrivDir = ?privdir(Config),
    ?line sort({2, binary_term}, [], Config),
    ?line sort(binary_term, [{tmpdir, list_to_atom(PrivDir)}], Config),
    ?line sort(binary_term, [{tmpdir,PrivDir}], Config),
    ?line sort({3,binary_term}, [{order, fun compare/2}], Config),
    ?line sort(binary_term, [{order, fun compare/2}], Config),
    ?line sort(binary_term, [{order,descending}], Config),
    ok.

binary_term_keysort(doc) ->
    ["Keysort binary terms on files."];
binary_term_keysort(suite) ->
    [];
binary_term_keysort(Config) when is_list(Config) ->
    ?line keysort({3, binary_term}, [], Config),
    ?line keysort(binary_term, [], Config),
    ?line keysort(binary_term, [{order,descending}], Config),
    ok.

binary_sort(doc) ->
    ["Sort binaries on files."];
binary_sort(suite) ->
    [];
binary_sort(Config) when is_list(Config) ->
    PrivDir = ?privdir(Config),
    ?line sort({2, binary}, [], Config),
    ?line sort(binary, [{tmpdir, list_to_atom(PrivDir)}], Config),
    ?line sort(binary, [{tmpdir,PrivDir}], Config),
    ?line sort({3,binary}, [{order, fun compare/2}], Config),
    ?line sort(binary, [{order, fun compare/2}], Config),
    ?line sort(binary, [{order,descending}], Config),
    ok.

term_merge(doc) ->
    ["Merge terms on files."];
term_merge(suite) ->
    [];
term_merge(Config) when is_list(Config) ->
    ?line merge(term, [{order, fun compare/2}], Config),
    ?line merge(term, [{order, ascending}, {compressed,true}], Config),
    ?line merge(term, [{order, descending}, {compressed,false}], Config),
    ok.

term_keymerge(doc) ->
    ["Keymerge terms on files."];
term_keymerge(suite) ->
    [];
term_keymerge(Config) when is_list(Config) ->
    ?line keymerge(term, [], Config),
    ?line keymerge(term, [{order, descending}], Config),
    ?line funmerge(term, [], Config),
    ok.

binary_term_merge(doc) ->
    ["Merge binary terms on files."];
binary_term_merge(suite) ->
    [];
binary_term_merge(Config) when is_list(Config) ->
    ?line merge(binary_term, [], Config),
    ?line merge({7, binary_term}, [], Config),
    ?line merge({3, binary_term}, [{order, fun compare/2}], Config),
    ok.

binary_term_keymerge(doc) ->
    ["Keymerge binary terms on files."];
binary_term_keymerge(suite) ->
    [];
binary_term_keymerge(Config) when is_list(Config) ->
    ?line keymerge({3, binary_term}, [], Config),
    ?line keymerge(binary_term, [], Config),
    ?line funmerge({3, binary_term}, [], Config),
    ?line funmerge(binary_term, [], Config),
    ok.

binary_merge(doc) ->
    ["Merge binaries on files."];
binary_merge(suite) ->
    [];
binary_merge(Config) when is_list(Config) ->
    ?line merge(binary, [], Config),
    ?line merge({7, binary}, [], Config),
    ?line merge({3, binary}, [{order, fun compare/2}], Config),
    ok.

term_check(doc) ->
    ["Check terms on files."];
term_check(suite) ->
    [];
term_check(Config) when is_list(Config) ->
    ?line check(term, Config),
    ok.

binary_term_check(doc) ->
    ["Check binary terms on files."];
binary_term_check(suite) ->
    [];
binary_term_check(Config) when is_list(Config) ->
    ?line check(binary_term, Config),
    ok.

term_keycheck(doc) ->
    ["Keycheck terms on files."];
term_keycheck(suite) ->
    [];
term_keycheck(Config) when is_list(Config) ->
    ?line keycheck(term, Config),
    ok.

binary_term_keycheck(doc) ->
    ["Keycheck binary terms on files."];
binary_term_keycheck(suite) ->
    [];
binary_term_keycheck(Config) when is_list(Config) ->
    ?line keycheck(binary_term, Config),
    ok.

binary_check(doc) ->
    ["Check binary terms on files."];
binary_check(suite) ->
    [];
binary_check(Config) when is_list(Config) ->
    ?line check(binary, Config),
    ok.

inout(doc) ->
    ["Funs as input or output."];
inout(suite) ->
    [];
inout(Config) when is_list(Config) ->
    BTF = {format, binary_term},
    Foo = outfile("foo", Config),

    %% Input is fun.
    End = fun(read) -> end_of_input end,

    IF1 = fun(read) -> {[1,7,5], End} end,
    ?line ok = file_sorter:sort(IF1, Foo, [{format, term}]),
    %% 'close' is called, but the return value is caught and ignored.
    IF2 = fun(read) -> {[1,2,3], fun(close) -> throw(ignored) end} end,
    ?line {error, bad_object} = file_sorter:sort(IF2, Foo, BTF),

    IF3 = fun(no_match) -> foo end,
    ?line {'EXIT', {function_clause, _}} = 
	(catch file_sorter:sort(IF3, Foo)),
    IF4 = fun(read) -> throw(my_message) end,
    ?line my_message = (catch file_sorter:sort(IF4, Foo)),
    IF5 = fun(read) -> {error, my_error} end,
    ?line {error, my_error} = file_sorter:sort(IF5, Foo),

    %% Output is fun.
    ?line {error, bad_object} = 
	file_sorter:sort(IF2, fun(close) -> ignored end, BTF),
    Args = [{format, term}],
    ?line {error, bad_object} = 
       file_sorter:keysort(1, IF2, fun(close) -> ignored end, Args),
    OF1 = fun(close) -> fine; (L) when is_list(L) -> fun(close) -> nice end end,
    ?line nice = file_sorter:sort(IF1, OF1, Args),
    OF2 = fun(_) -> my_return end,
    ?line my_return = file_sorter:sort(IF1, OF2, Args),
    OF3 = fun(_) -> throw(my_message) end,
    ?line my_message = (catch file_sorter:sort(IF1, OF3, Args)),
    OF4 = fun(no_match) -> foo end,
    ?line {'EXIT', {function_clause, _}} = 
	(catch file_sorter:sort(IF1, OF4, Args)),

    ?line P0 = pps(),
    ?line Fs1 = to_files([[3,1,2,5,4], [8,3,10]], term, Config),
    ?line error = file_sorter:sort(Fs1, fun(_) -> error end, Args),
    ?line delete_files(Fs1),

    ?line true = P0 =:= pps(),

    %% Passing a value from the input functions to the output functions.
    IFV1 = fun(read) -> {end_of_input, 17} end,
    OFV1 = fun({value, Value}) -> ofv(Value, []) end,
    ?line {17, []} = file_sorter:sort(IFV1, OFV1, Args),

    %% Output is not a fun. The value returned by input funs is ignored.
    %% OTP-5009.
    ?line ok = file_sorter:sort(IFV1, Foo, [{format,term}]),
    ?line [] = from_files(Foo, term),
    ?line delete_files(Foo),

    ok.

ofv(Value, A) ->
    fun(close) -> 
	    {Value, lists:append(lists:reverse(A))};
       (L) when is_list(L) ->
	    ofv(Value, [L | A])
    end.

many(doc) ->
    ["Many temporary files."];
many(suite) ->
    [];
many(Config) when is_list(Config) ->
    Foo = outfile("foo", Config),
    PrivDir = ?privdir(Config),
    P0 = pps(),

    Args = [{format, term}],
    L1 = lists:map(fun(I) -> {one, two, three, I} end, lists:seq(1,1000)),
    L2 = lists:map(fun(I) -> {four, five, six, I} end, lists:seq(1,1000)),
    ?line Fs2 = to_files([L1, L2], term, Config),
    ?line ok = file_sorter:sort(Fs2, Foo, [{size,1000} | Args]),
    ?line R = lists:sort(L1++L2),
    ?line R = from_files(Foo, term),
    ?line 2000 = length(R),
    ?line ok = file_sorter:sort(Fs2, Foo, [{no_files,4},{size,1000} | Args]),
    ?line R = from_files(Foo, term),
    ?line ok = 
	file_sorter:sort(Fs2, Foo, 
			 [{no_files,4},{size,1000},{order,descending} | Args]),
    ?line true = lists:reverse(R) =:= from_files(Foo, term),
    ?line ok = 
	file_sorter:sort(Fs2, Foo, 
			 [{no_files,4},{size,1000},
			  {order,fun compare/2} | Args]),
    ?line R = from_files(Foo, term),
    ?line ok = file_sorter:keysort(4, Fs2, Foo, 
				   [{no_files,4},{size,1000} | Args]),
    ?line RK = lists:keysort(4, L1++L2),
    ?line RK = from_files(Foo, term),
    ?line delete_files(Foo),
    ?line ok = 
	file_sorter:keysort(4, Fs2, Foo, 
		      [{no_files,4},{size,1000},{order,descending} | Args]),
    ?line true = lists:reverse(RK) =:= from_files(Foo, term),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keysort(4, Fs2, Foo, 
				   [{size,500},{order,descending} | Args]),
    ?line true = lists:reverse(RK) =:= from_files(Foo, term),
    ?line delete_files(Foo),
    ?line error = file_sorter:sort(Fs2, fun(_) -> error end, 
				   [{tmpdir, PrivDir}, {no_files,3}, 
				    {size,10000} | Args]),

    TmpDir = filename:join(PrivDir, "tmpdir"),
    file:del_dir(TmpDir),
    ?line ok = file:make_dir(TmpDir),
    ?line case os:type() of
	      {unix, _} ->
		  ?line ok = file:change_mode(TmpDir, 8#0000),
		  ?line {error, {file_error, _,_}} = 
		      file_sorter:sort(Fs2, fun(_M) -> foo end, 
				       [{no_files,3},{size,10000},
					{tmpdir,TmpDir} | Args]);
	      _ ->
		  true
	  end,
    ?line ok = file:del_dir(TmpDir),
    delete_files(Fs2),
    ?line true = P0 =:= pps(),
    ok.

misc(doc) ->
    ["Some other tests."];
misc(suite) ->
    [];
misc(Config) when is_list(Config) ->
    BTF = {format, binary_term},
    Foo = outfile("foo", Config),
    FFoo = filename:absname(Foo),
    P0 = pps(),

    ?line [File] = Fs1 = to_files([[1,3,2]], term, Config),
    ?line ok = file:write_file(Foo,<<>>),
    ?line case os:type() of
	      {unix, _} ->
                  ok = file:change_mode(Foo, 8#0000),
                  {error,{file_error,FFoo,eacces}} = 
                      file_sorter:sort(Fs1, Foo, {format,term});
              _ ->
                  true
          end,
    ?line file:delete(Foo),
    ?line NoBytes = 16, % RAM memory will never get this big, or?
    ?line ALot = (1 bsl (NoBytes*8)) - 1,
    ?line ok = file:write_file(File, <<ALot:NoBytes/unit:8,"foobar">>),
    FFile = filename:absname(File),
    ?line {error, {bad_object,FFile}} = 
        file_sorter:sort(Fs1, Foo, [BTF, {header, 20}]),
    ?line ok = file:write_file(File, <<30:32,"foobar">>),
    ?line {error, {premature_eof, FFile}} = file_sorter:sort(Fs1, Foo, BTF),
    ?line ok = file:write_file(File, <<6:32,"foobar">>),
    ?line {error, {bad_object,FFile}} = file_sorter:sort(Fs1, Foo, BTF),
    ?line case os:type() of
              {unix, _} ->
                  ok = file:change_mode(File, 8#0000),
                  {error, {file_error,FFile,eacces}} = 
                      file_sorter:sort(Fs1, Foo),
                  {error, {file_error,FFile,eacces}} = 
                      file_sorter:sort(Fs1, Foo, {format, binary_term});
              _ ->
                  true
          end,
    ?line delete_files(Fs1),
    ?line true = P0 =:= pps(),

    %% bigger than chunksize
    ?line E1 = <<32000:32, 10:256000>>,
    ?line E2 = <<32000:32, 5:256000>>,
    ?line E3 = <<32000:32, 8:256000>>,
    ?line ok = file:write_file(Foo, [E1, E2, E3]),
    ?line ok = file_sorter:sort([Foo], Foo, [{format,binary},{size,10000}]),
    ?line ok = file_sorter:sort([Foo], Foo, [{format,fun(X) -> X end},
                                             {size,10000}]),
    ?line Es = list_to_binary([E2,E3,E1]),
    ?line {ok, Es} = file:read_file(Foo),
    ?line delete_files(Foo),
    ?line true = P0 =:= pps(),

    %% keysort more than one element
    L = [{c,1,a},{c,2,b},{c,3,c},{b,1,c},{b,2,b},{b,3,a},{a,1,a},{a,2,b},
         {a,3,c}],
    ?line Fs2 = to_files([L], binary_term, Config),
    ?line ok = file_sorter:keysort([2,3], Fs2, Foo, {format, binary_term}),
    ?line KS2_1 = from_files(Foo, binary_term),
    ?line KS2_2 = lists:keysort(2,lists:keysort(3, L)),
    ?line KS2_1 = KS2_2,
    ?line ok = file_sorter:keysort([2,3], Fs2, Foo, 
                                   [{format, binary_term},{size,5}]),
    ?line KS2_3 = from_files(Foo, binary_term),
    ?line KS2_3 = KS2_2,
    ?line ok = file_sorter:keysort([2,3,1], Fs2, Foo, {format, binary_term}),
    ?line KS3_1 = from_files(Foo, binary_term),
    ?line KS3_2 = lists:keysort(2, lists:keysort(3,lists:keysort(1, L))),
    ?line KS3_1 = KS3_2,
    ?line ok = file_sorter:keysort([2,3,1], Fs2, Foo, 
                                   [{format, binary_term},{size,5}]),
    ?line KS3_3 = from_files(Foo, binary_term),
    ?line KS3_3 = KS3_2,
    ?line delete_files([Foo | Fs2]),
    ?line true = P0 =:= pps(),

    %% bigger than chunksize
    %% Assumes that CHUNKSIZE = 16384. Illustrates that the Last argument
    %% of merge_files/5 is necessary. 
    ?line EP1 = erlang:make_tuple(2728,foo),
    ?line EP2 = lists:duplicate(2729,qqq),
    ?line LL = [EP1, EP2, EP1, EP2, EP1, EP2],
    ?line Fs3 = to_files([LL], binary, Config),
    ?line ok = file_sorter:sort(Fs3, Foo, [{format,binary}, {unique,true}]),
    ?line [EP1,EP2] = from_files(Foo, binary),
    ?line delete_files(Foo),
    ?line ok = file_sorter:sort(Fs3, Foo, 
                                [{format,binary_term}, {unique,true}, 
                                 {size,30000}]),
    ?line [EP1,EP2] = from_files(Foo, binary_term),
    ?line delete_files([Foo | Fs3]),

    ?line true = P0 =:= pps(),

    ?line BE1 = <<20000:32, 17:160000>>,
    ?line BE2 = <<20000:32, 1717:160000>>,
    ?line ok = file:write_file(Foo, [BE1,BE2,BE1,BE2]),
    ?line ok = file_sorter:sort([Foo], Foo, [{format,binary},
                                             {size,10000},
                                             {unique,true}]),
    ?line BEs = list_to_binary([BE1, BE2]),
    ?line {ok, BEs} = file:read_file(Foo),
    ?line delete_files(Foo),
    ?line true = P0 =:= pps(),

    ?line Fs4 = to_files([[7,4,1]], binary_term, Config),
    ?line {error, {bad_term, _}} = file_sorter:sort(Fs4, Foo, {format, term}),
    ?line delete_files([Foo | Fs4]),
    ?line true = P0 =:= pps(),

    ok.

%%% 
%%% Utilities.
%%% 

sort(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,5} | XArgs]),
    TmpArgs = [{tmpdir,?privdir(Config)} | Args],
    Foo = outfile("foo", Config),

    %% Input is a fun. Output is a fun.
    ?line [] = file_sorter:sort(input([], 2, Fmt), output([], Fmt), Args),
    ?line L1 = [3,1,2,5,4],
    ?line S1 = file_sorter:sort(input(L1, 2, Fmt), output([], Fmt), TmpArgs),
    ?line S1 = rev(lists:sort(L1), TmpArgs),

    %% Input is a file. Output is a fun.
    ?line [] = file_sorter:sort([], output([], Fmt), Args),
    ?line L2 = [3,1,2,5,4],
    ?line Fs1 = to_files([L2], Fmt, Config),
    ?line S2 = file_sorter:sort(Fs1, output([], Fmt), TmpArgs),
    ?line S2 = rev(lists:sort(L2), TmpArgs),
    ?line delete_files(Fs1),

    %% Input is a file. Output is a file
    ?line ok = file_sorter:sort([], Foo, Args),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:sort([], Foo, [{unique,true} | Args]),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line L3 = [3,1,2,5,4,6],
    ?line Fs2 = to_files([L3], Fmt, Config),
    ?line ok = file_sorter:sort(Fs2, Foo, Args),
    ?line true = rev(lists:sort(L3), Args) =:= from_files(Foo, Fmt),
    ?line delete_files([Foo | Fs2]),
    ?line L4 = [1,3,4,1,2,5,4,5,6],
    ?line Fs3 = to_files([L4], Fmt, Config),
    ?line ok = file_sorter:sort(Fs3, Foo, Args++[{unique,true}, 
						 {size,100000}]),
    ?line true = rev(lists:usort(L4), Args) =:= from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:sort(Fs3, Foo, Args++[{unique,true}]),
    ?line true = rev(lists:usort(L4), Args) =:= from_files(Foo, Fmt),
    ?line delete_files([Foo | Fs3]),

    %% Input is a fun. Output is a file.
    ?line ok = file_sorter:sort(input([], 2, Fmt), Foo, Args),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line L5 = [3,1,2,5,4,7],
    ?line ok = file_sorter:sort(input(L5, 2, Fmt), Foo, Args),
    ?line true = rev(lists:sort(L5), Args) =:= from_files(Foo, Fmt),
    ?line delete_files(Foo),

    %% Removing duplicate keys.
    KFun = key_compare(2),
    L6 = [{5,e},{2,b},{3,c},{1,a},{4,d}] ++ [{2,c},{1,b},{4,a}],
    KUArgs = lists:keydelete(order, 1, Args) ++ 
             [{unique, true}, {order, KFun},{size,100000}],
    ?line ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KUArgs),
    ?line true = rev(lists:ukeysort(2, L6), KUArgs) =:= from_files(Foo, Fmt),
    KArgs = lists:keydelete(unique, 1, KUArgs),
    ?line ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KArgs),
    ?line true = rev(lists:keysort(2, L6), KArgs) =:= from_files(Foo, Fmt),

    %% Removing duplicate keys. Again.
    KUArgs2 = lists:keydelete(order, 1, Args) ++ 
              [{unique, true}, {order, KFun},{size,5}],
    ?line ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KUArgs2),
    ?line true = rev(lists:ukeysort(2, L6), KUArgs2) =:= from_files(Foo, Fmt),
    KArgs2 = lists:keydelete(unique, 1, KUArgs2),
    ?line ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KArgs2),
    ?line true = rev(lists:keysort(2, L6), KArgs2) =:= from_files(Foo, Fmt),
    ?line delete_files(Foo),
    
    ok.

keysort(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,50}, {no_files, 2} | XArgs]),
    TmpArgs = Args ++ [{tmpdir,?privdir(Config)}],
    Foo = outfile("foo", Config),

    %% Input is files. Output is a file.
    ?line ok = file_sorter:keysort(2, [], Foo, Args),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keysort(2, [], Foo, [{unique,true} | Args]),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line L0 = [{a,2},{a,1},{a,2},{a,2},{a,1},{a,2},{a,2},{a,3}],
    ?line Fs0 = to_files([L0], Fmt, Config),
    ?line S = rev(lists:ukeysort(1, L0), Args),
    ?line ok = 
	file_sorter:keysort(1, Fs0, Foo, Args ++ [{unique,true},
						  {size,100000}]),
    ?line S = from_files(Foo, Fmt),
    ?line ok = 
	file_sorter:keysort(1, Fs0, Foo, Args ++ [{unique,true},
						  {size,5}]),
    ?line S = from_files(Foo, Fmt),
    ?line ok = file_sorter:keysort(1, Fs0, Foo, Args ++ [{unique,true}]),
    ?line S = from_files(Foo, Fmt),
    ?line delete_files([Foo | Fs0]),
    ?line L11 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    ?line L21 = [{a,1,x3},{b,2,x3},{c,3,x3}],
    ?line L31 = [{a,1,x2},{b,2,x2},{c,3,x2}],
    ?line L41 = [{a,1,x1},{b,2,x1},{c,3,x1}],
    ?line All = [L11, L21, L31, L41],
    ?line AllFlat = lists:append(All),
    ?line Sorted = rev(lists:keysort(2, AllFlat), Args),
    ?line Fs1 = to_files(All, Fmt, Config),
    ?line ok = file_sorter:keysort(2, Fs1, Foo, Args),
    ?line Sorted = from_files(Foo, Fmt),
    ?line delete_files(Foo),

    %% Input is files. Output is a fun.
    ?line [] = file_sorter:keysort(2, [], output([], Fmt), Args),
    ?line KS1 = file_sorter:keysort(2, Fs1, output([], Fmt), TmpArgs),
    ?line Sorted = KS1,
    ?line delete_files(Fs1),

    %% Input is a fun. Output is a file.
    ?line ok = file_sorter:keysort(2, input([], 2, Fmt), Foo, Args),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keysort(2, input(AllFlat, 4, Fmt), Foo, Args),
    ?line Sorted = from_files(Foo,  Fmt),
    ?line delete_files(Foo),

    %% Input is a fun. Output is a fun.
    ?line [] = file_sorter:keysort(2, input([], 2, Fmt), output([], Fmt),Args),
    ?line KS2 = 
	file_sorter:keysort(2, input(AllFlat, 4, Fmt), output([], Fmt), 
			    TmpArgs),
    ?line Sorted = KS2,
    ok.

merge(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,5} | XArgs]),
    Foo = outfile("foo", Config),

    %% Input is a file. Output is a fun.
    ?line [] = file_sorter:merge([], output([], Fmt), Args),
    ?line L2 = [[1,3,5],[2,4,5]],
    ?line Fs1 = to_files(L2, Fmt, Config),
    ?line S2 = file_sorter:sort(Fs1, output([], Fmt), Args),
    ?line S2 = rev(lists:sort(lists:append(L2)), Args),
    ?line delete_files(Fs1),

    %% Input is a file. Output is a file
    ?line ok = file_sorter:merge([], Foo, Args),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:merge([], Foo, [{unique,true} | Args]),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line L31 = [1,2,3],
    ?line L32 = [2,3,4],
    ?line L33 = [4,5,6],
    ?line L3r = [L31, L32, L33],
    ?line L3 = [rev(L31,Args), rev(L32,Args), rev(L33,Args)],
    ?line Fs2 = to_files(L3, Fmt, Config),
    ?line ok = file_sorter:merge(Fs2, Foo, Args),
    ?line true = rev(lists:merge(L3r), Args) =:= from_files(Foo, Fmt),
    ?line ok = file_sorter:merge(Fs2, Foo, Args++[{unique,true}, 
						  {size,100000}]),
    ?line true = rev(lists:umerge(L3r), Args) =:= from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:merge(Fs2, Foo, Args++[{unique,true}]),
    ?line true = rev(lists:umerge(L3r), Args) =:= from_files(Foo, Fmt),
    ?line delete_files([Foo | Fs2]),

    ok.

keymerge(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,50}, {no_files, 2} | XArgs]),
    Foo = outfile("foo", Config),

    %% Input is files. Output is a file.
    ?line ok = file_sorter:keymerge(2, [], Foo, Args),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keymerge(2, [], Foo, [{unique,true} | Args]),
    ?line [] = from_files(Foo, Fmt),
    ?line delete_files(Foo),
    ?line L0 = [rev([{a,1},{a,2}], Args), rev([{a,2},{a,1},{a,3}], Args)],
    ?line Fs0 = to_files(L0, Fmt, Config),
    ?line delete_files(Foo),
    ?line ok = file_sorter:keymerge(1, Fs0, Foo, Args ++ [{unique,false}]),
    ?line S2 = rev([{a,1},{a,2},{a,2},{a,1},{a,3}], Args),
    ?line S2 = from_files(Foo, Fmt),
    ?line delete_files([Foo | Fs0]),
    ?line L11 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    ?line L21 = [{a,1,x3},{b,2,x3},{c,3,x3}],
    ?line L31 = [{a,1,x2},{b,2,x2},{c,3,x2}],
    ?line L41 = [{a,1,x1},{b,2,x1},{c,3,x1}],
    ?line All = 
	[rev(L11, Args), rev(L21, Args), rev(L31, Args), rev(L41, Args)],
    ?line Merged1 = lists:keymerge(2, L11, L21),
    ?line Merged2 = lists:keymerge(2, L31, L41),
    ?line Merged = rev(lists:keymerge(2, Merged1, Merged2), Args),
    ?line Fs1 = to_files(All, Fmt, Config),
    ?line ok = file_sorter:keymerge(2, Fs1, Foo, Args),
    ?line Merged = from_files(Foo, Fmt),

    fun() -> 
    UArgs = [{unique,true} | Args],
    ?line UMerged1 = lists:ukeymerge(2, L11, L21),
    ?line UMerged2 = lists:ukeymerge(2, L31, L41),
    ?line UMerged = rev(lists:ukeymerge(2, UMerged1, UMerged2), Args),
    ?line ok = file_sorter:keymerge(2, Fs1, Foo, UArgs),
    ?line UMerged = from_files(Foo, Fmt),
    UArgs2 = make_args(Fmt, [{unique,true}, {size,50} | XArgs]),
    ?line ok = file_sorter:keymerge(2, Fs1, Foo, UArgs2),
    ?line UMerged = from_files(Foo, Fmt),
    ?line List = rev([{a,1,x4},{b,2,x4},{c,3,x4}], Args),
    ?line FsL = to_files([List], Fmt, Config),
    ?line ok = file_sorter:keymerge(2, FsL, Foo, UArgs),
    ?line List = from_files(Foo, Fmt),
    ?line List1 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    ?line List2 = [{a,3,x4},{b,4,x4},{c,5,x4}],
    ?line FsLL = to_files([rev(List1, Args), rev(List2, Args)], Fmt, Config),
    ?line ok = file_sorter:keymerge(2, FsLL, Foo, UArgs),
    ?line List1_2 = rev(lists:ukeymerge(2, List1, List2), Args),
    ?line List1_2 = from_files(Foo, Fmt),
    ?line delete_files(Foo)
    end(),

    %% Input is files. Output is a fun.
    ?line Fs3 = to_files(All, Fmt, Config),
    ?line [] = file_sorter:keysort(2, [], output([], Fmt), Args),
    ?line KS1 = file_sorter:keymerge(2, Fs3, output([], Fmt), Args),
    ?line Merged = KS1,
    ?line delete_files([Foo | Fs3]),

    ?line L2 = [[{a,1}],[{a,2}],[{a,3}],[{a,4}],[{a,5}],[{a,6}],[{a,7}]],
    ?line Fs2 = to_files(L2, Fmt, Config),
    ?line M = file_sorter:keymerge(1, Fs2, output([], Fmt), Args),
    ?line M = rev(lists:append(L2), Args),
    ?line delete_files(Fs2),

    ?line LL1 = [{d,4},{e,5},{f,6}],
    ?line LL2 = [{a,1},{b,2},{c,3}],
    ?line LL3 = [{j,10},{k,11},{l,12}],
    ?line LL4 = [{g,7},{h,8},{i,9}],
    ?line LL5 = [{p,16},{q,17},{r,18}],
    ?line LL6 = [{m,13},{n,14},{o,15}],
    ?line LLAll = [rev(LL1, Args),rev(LL2, Args),rev(LL3, Args),
                   rev(LL4, Args),rev(LL5, Args),rev(LL6, Args)],
    ?line FsLL6 = to_files(LLAll, Fmt, Config),
    ?line LL = rev(lists:sort(lists:append(LLAll)), Args),
    ?line ok = file_sorter:keymerge(1, FsLL6, Foo, Args),
    ?line LL = from_files(Foo, Fmt),
    ?line ok = file_sorter:keymerge(1, FsLL6, Foo, [{unique,true} | Args]),
    ?line LL = from_files(Foo, Fmt),
    ?line delete_files([Foo | FsLL6]),

    ok.

funmerge(Fmt, XArgs, Config) ->
    KComp = key_compare(2),
    Args = make_args(Fmt, [{order,KComp},{size,5}, {no_files, 5} | XArgs]),
    UArgs = [{unique,true} | Args],
    Foo = outfile(foo, Config),

    ?line EFs = to_files([[]], Fmt, Config),
    ?line ok = file_sorter:merge(EFs, Foo, UArgs),
    ?line [] = from_files(Foo, Fmt),
    delete_files([Foo | EFs]),

    ?line L11 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    ?line L21 = [{a,1,x3},{b,2,x3},{c,3,x3}],
    ?line L31 = [{a,1,x2},{b,2,x2},{c,3,x2}],
    ?line L41 = [{a,1,x1},{b,2,x1},{c,3,x1}],
    ?line CAll = [L11, L21, L31, L41],
    ?line CMerged1 = lists:merge(KComp, L11, L21),
    ?line CMerged2 = lists:merge(KComp, L31, L41),
    ?line CMerged = lists:merge(KComp, CMerged1, CMerged2),
    ?line CFs1 = to_files(CAll, Fmt, Config),
    ?line ok = file_sorter:merge(CFs1, Foo, Args),
    ?line CMerged = from_files(Foo, Fmt),

    Args4 = make_args(Fmt, [{size,50} | XArgs]),
    ?line ok = file_sorter:merge(CFs1, Foo, [{order,KComp} | Args4]),
    ?line CMerged = from_files(Foo, Fmt),

    ?line UMerged1 = lists:umerge(KComp, L11, L21),
    ?line UMerged2 = lists:umerge(KComp, L31, L41),
    ?line UMerged = lists:umerge(KComp, UMerged1, UMerged2),
    ?line ok = file_sorter:merge(CFs1, Foo, [{order,KComp} | UArgs]),
    ?line UMerged = from_files(Foo, Fmt),
    UArgs2 = 
        lists:keydelete(order, 1,
                        make_args(Fmt, [{unique,true}, {size,50} | XArgs])),
    ?line ok = file_sorter:merge(CFs1, Foo, [{order,KComp} | UArgs2]),
    ?line UMerged = from_files(Foo, Fmt),
    ?line delete_files(Foo),

    ?line List1 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    ?line List2 = [{a,3,x4},{b,4,x4},{c,5,x4}],
    ?line List3 = [{a,5,x4},{b,6,x4},{c,7,x4}],
    ?line FsLL = to_files([List1, List2, List3], Fmt, Config),
    ?line ok = file_sorter:merge(FsLL, Foo, Args),
    ?line List1_2 = lists:merge(KComp,lists:merge(KComp,List1,List2),List3),
    ?line List1_2 = from_files(Foo, Fmt),
    ?line ok = file_sorter:merge(FsLL, Foo, [{order,KComp} | UArgs]),
    ?line UList1_2 = 
        lists:umerge(KComp,lists:umerge(KComp, List1, List2),List3),
    ?line UList1_2 = from_files(Foo, Fmt),
    ?line delete_files([Foo | CFs1]),

    fun() ->
    ?line LL1 = [{d,4},{e,5},{f,6}],
    ?line LL2 = [{a,1},{b,2},{c,3}],
    ?line LL3 = [{j,10},{k,11},{l,12}],
    ?line LL4 = [{g,7},{h,8},{i,9}],
    ?line LL5 = [{p,16},{q,17},{r,18}],
    ?line LL6 = [{m,13},{n,14},{o,15}],
    ?line LLAll = [LL1,LL2,LL3,LL4,LL5,LL6],
    ?line FsLL6 = to_files(LLAll, Fmt, Config),
    ?line LL = lists:sort(lists:append(LLAll)),
    ?line ok = file_sorter:merge(FsLL6, Foo, Args),
    ?line LL = from_files(Foo, Fmt),
    ?line ok = file_sorter:merge(FsLL6, Foo, UArgs),
    ?line LL = from_files(Foo, Fmt),
    ?line delete_files([Foo | FsLL6])
    end(),

    fun() ->
    ?line RLL1 = [{b,2},{h,8},{n,14}],
    ?line RLL2 = [{a,1},{g,7},{m,13}],
    ?line RLL3 = [{d,4},{j,10},{p,16}],
    ?line RLL4 = [{c,3},{i,9},{o,15}],
    ?line RLL5 = [{f,6},{l,12},{r,18}],
    ?line RLL6 = [{e,5},{k,11},{q,17}],
    ?line RLLAll = [RLL1,RLL2,RLL3,RLL4,RLL5,RLL6],
    ?line RFsLL6 = to_files(RLLAll, Fmt, Config),
    ?line RLL = lists:sort(lists:append(RLLAll)),
    ?line ok = file_sorter:merge(RFsLL6, Foo, Args),
    ?line RLL = from_files(Foo, Fmt),
    ?line ok = file_sorter:merge(RFsLL6, Foo, UArgs),
    ?line RLL = from_files(Foo, Fmt),
    ?line delete_files([Foo | RFsLL6])
    end(),

    ok.

check(Fmt, Config) ->
    Args0 = make_args(Fmt, [{size,5}]),
    Args = Args0 ++ [{tmpdir,?privdir(Config)}],

    Fun = fun compare/2,

    L1 = [3,1,2,5,4],
    [F1_0] = Fs1 = to_files([L1], Fmt, Config),
    F1 = filename:absname(F1_0),
    ?line {ok, [{F1,2,1}]} = file_sorter:check(Fs1, Args),
    ?line {ok, [{F1,2,1}]} = file_sorter:check(Fs1, [{order,Fun} | Args]),
    ?line {ok, [{F1,2,1}]} = file_sorter:check(Fs1, [{unique,true} | Args]),
    ?line {ok, [{F1,2,1}]} = 
	file_sorter:check(Fs1, [{order,Fun},{unique,true} | Args]),
    ?line {ok, [{F1,3,2}]} = 
	file_sorter:check(Fs1, [{order,descending} | Args]),
    ?line {ok, [{F1,3,2}]} = 
	file_sorter:check(Fs1, [{unique,true},{order,descending} | Args]),
    ?line delete_files(Fs1),
    
    L2 = [[1,2,2,3,3,4,5,5],[5,5,4,3,3,2,2,1]],
    [F2_0,F3_0] = Fs2 = to_files(L2, Fmt, Config),
    F2 = filename:absname(F2_0),
    F3 = filename:absname(F3_0),
    ?line {ok, [{F3,3,4}]} = file_sorter:check(Fs2, Args),
    ?line {ok, [{F3,3,4}]} = file_sorter:check(Fs2, [{order,Fun} | Args]),
    ?line {ok, [{F2,3,2},{F3,2,5}]} = 
	file_sorter:check(Fs2, [{unique, true} | Args]),
    ?line {ok, [{F2,3,2},{F3,2,5}]} = 
	file_sorter:check(Fs2, [{order,Fun},{unique, true} | Args]),
    ?line {ok, [{F2,2,2}]} = 
	file_sorter:check(Fs2, [{order,descending} | Args]),
    ?line {ok, [{F2,2,2},{F3,2,5}]} = 
	file_sorter:check(Fs2, [{unique,true},{order,descending} | Args]),
    ?line delete_files(Fs2),
    
    L3 = [1,2,3,4],
    ?line Fs3 = to_files([L3], Fmt, Config),
    ?line {ok, []} = file_sorter:check(Fs3, [{unique,true} | Args]),
    ?line {ok, []} = 
	file_sorter:check(Fs3, [{unique,true},{order,Fun} | Args]),
    ?line delete_files(Fs3),

    %% big objects
    ?line T1 = erlang:make_tuple(10000,foo),
    ?line T2 = erlang:make_tuple(10000,bar),
    ?line L4 = [T1,T2],
    ?line [FF_0] = Fs4 = to_files([L4], Fmt, Config),
    FF = filename:absname(FF_0),
    ?line {ok, [{FF,2,T2}]} = file_sorter:check(Fs4, [{unique,true} | Args]),
    ?line delete_files(Fs4),

    CFun = key_compare(2),
    L10 = [[{1,a},{2,b},T10_1={1,b},{3,c}], [{1,b},T10_2={2,a}]],
    [F10_0,F11_0] = Fs10 = to_files(L10, Fmt, Config),
    F10_1 = filename:absname(F10_0),
    F11_1 = filename:absname(F11_0),
    ?line {ok, [{F10_1,3,T10_1},{F11_1,2,T10_2}]} = 
        file_sorter:check(Fs10, [{unique,true},{order,CFun} | Args]),
    ?line delete_files(Fs10),

    ok.

keycheck(Fmt, Config) ->
    Args0 = make_args(Fmt, [{size,5}]),
    Args = Args0 ++ [{tmpdir,?privdir(Config)}],

    ?line L1 = [[{a,1},{b,2}], [{c,2},{b,1},{a,3}]],
    ?line [F1_0,F2_0] = Fs1 = to_files(L1, Fmt, Config),
    F1 = filename:absname(F1_0),
    F2 = filename:absname(F2_0),
    ?line {ok, [{F2,2,{b,1}}]} = file_sorter:keycheck(1, Fs1, Args),
    ?line {ok, [{F2,2,{b,1}}]} = 
	file_sorter:keycheck(1, Fs1, [{unique,true} | Args]),
    ?line {ok, [{F1,2,{b,2}}]} = 
	file_sorter:keycheck(1, Fs1, [{order,descending},{unique,true} | Args]),
    ?line delete_files(Fs1),
    
    L2 = [[{a,1},{a,2},{a,2},{b,2}], [{c,2},{b,1},{b,2},{b,2},{a,3}]],
    ?line [F3_0,F4_0] = Fs2 = to_files(L2, Fmt, Config),
    F3 = filename:absname(F3_0),
    F4 = filename:absname(F4_0),
    ?line {ok, [{F4,2,{b,1}}]} = file_sorter:keycheck(1, Fs2, Args),
    ?line {ok, [{F3,2,{a,2}},{F4,2,{b,1}}]} = 
	file_sorter:keycheck(1, Fs2, [{unique,true} | Args]),
    ?line {ok, [{F3,4,{b,2}}]} = 
	file_sorter:keycheck(1, Fs2, [{order,descending} | Args]),
    ?line {ok, [{F3,2,{a,2}},{F4,3,{b,2}}]} = 
	file_sorter:keycheck(1, Fs2, 
			     [{order,descending},{unique,true} | Args]),
    ?line delete_files(Fs2),
    
    ok.

rev(L, Args) ->
    case lists:member({order, descending}, Args) of
	true ->
	    lists:reverse(L);
	false ->
	    L
    end.

make_args({HL, Fmt}, Args) ->
    make_args(Fmt, [{header, HL} | Args]);
make_args(Fmt, Args) ->
    [{format, Fmt} | Args].

compare(X, Y) ->
    X =< Y.

key_compare(I) ->
    fun(X, Y) -> 
            element(I, bin_to_term(X)) =< element(I, bin_to_term(Y))
    end.

bin_to_term(B) when is_binary(B) -> binary_to_term(B);
bin_to_term(T) -> T.

-define(CHUNKSIZE, 8096).

pps() ->
    erlang:ports().

input(L, N, term) ->
    input(L, N);
input(L, N, {_HL, Format}) when Format =:= binary_term; Format =:= binary ->
    binput(L, N);
input(L, N, Format) when Format =:= binary_term; Format =:= binary ->
    binput(L, N).

binput(L, N) ->
    Bs = lists:map(fun(T) -> term_to_binary(T) end, L),
    input(Bs, N).

input(L, N) ->
    fun(close) ->
	    ok;
       (read) ->
	    case L of
		[] -> end_of_input;
		_ ->
		    R = lists:sublist(L, N),
		    NL = lists:nthtail(length(R), L),
		    {R, input(NL, N)}
	    end
    end.

output(L, term) ->
    output(L);
output(L, {_HL, Format}) when Format =:= binary_term; Format =:= binary ->
    boutput(L);
output(L, Format) when Format =:= binary_term; Format =:= binary ->
    boutput(L).

output(A) ->
    fun(close) -> 
	    lists:append(lists:reverse(A));
       (L) when is_list(L) ->
	    output([L | A])
    end.

boutput(A) ->
    fun(close) -> 
	    Bs = lists:append(lists:reverse(A)),
	    lists:map(fun(B) -> binary_to_term(B) end, Bs);
       (L) when is_list(L) ->
	    boutput([L | A])
    end.

outfile(Name, Config) ->
    list_to_atom(filename:join(?privdir(Config), Name)).

%% [[term()]] -> [filename()]
to_files(Lists, term, Config) ->
    terms_to_files(Lists, Config);
to_files(Lists, Format, Config) when Format =:= binary_term; 
				     Format =:= binary ->
    bins_to_files(Lists, 4, Config);
to_files(Lists, {HL, Format}, Config) when Format =:= binary_term; 
					   Format =:= binary ->
    bins_to_files(Lists, HL, Config).

%% [[term()]] -> [filename()]
terms_to_files(Lists, Config) ->
    PrivDir = ?privdir(Config),
    terms_to_files(Lists, PrivDir, 1).

terms_to_files([L | Ls], PrivDir, N) ->
    F = lists:concat([?MODULE, '_', N]),
    File = filename:join(PrivDir, F),
    {ok, Fd} = file:open(File, [write]),
    write_terms(Fd, L),
    file:close(Fd),
    [list_to_atom(File) | terms_to_files(Ls, PrivDir, N+1)];
terms_to_files([], _PrivDir, _N) ->
    [].

write_terms(Fd, [T | Ts]) ->
    io:format(Fd, "~p.~n", [T]),
    write_terms(Fd, Ts);
write_terms(_Fd, []) ->
    ok.

%% [[term()]] -> [filename()]
bins_to_files(Lists, HL, Config) ->
    PrivDir = ?privdir(Config),
    bins_to_files(Lists, PrivDir, 1, HL).

bins_to_files([L | Fs], PrivDir, N, HL) ->
    F = lists:concat([?MODULE, '_', N]),
    File = filename:join(PrivDir, F),
    {ok, Fd} = file:open(File, [raw,binary,write]),
    write_bins(Fd, L, HL),
    file:close(Fd),
    [list_to_atom(File) | bins_to_files(Fs, PrivDir, N+1, HL)];
bins_to_files([], _PrivDir, _N, _HL) ->
    [].

write_bins(Fd, [T | Ts], HL) ->
    B = term_to_binary(T),
    Sz = byte_size(B),
    ok = file:write(Fd, [<<Sz:HL/unit:8>>, B]),
    write_bins(Fd, Ts, HL);
write_bins(_Fd, [], _HL) ->
    ok.

%% [filename()] -> [[term()]] or filename() -> [term()]
from_files(Files, term) ->
    terms_from_files(Files);
from_files(Files, Format) when Format =:= binary_term; Format =:= binary ->
    bins_from_files(Files, 4);
from_files(Files, {HL, Format}) when Format =:= binary_term; 
                                     Format =:= binary ->
    bins_from_files(Files, HL).

%% [filename()] -> [[term()]] or filename() -> [term()]
terms_from_files(File) when is_atom(File) ->
    [Terms] = terms_from_files([File]),
    Terms;
terms_from_files(Files) ->
    lists:map(fun(F) -> terms_from_file(F) end, Files).

terms_from_file(File) ->
    {ok, Fd} = file:open(File, [read,compressed]),
    terms_from_file(Fd, []).

terms_from_file(Fd, L) ->
    case io:read(Fd, '') of
	{ok, Term} ->
	    terms_from_file(Fd, [Term | L]);
	eof ->
	    file:close(Fd),
	    lists:reverse(L)
    end.

%% [filename()] -> [[term()]]
bins_from_files(File, HL) when is_atom(File) ->
    [Bins] = bins_from_files([File], HL),
    Bins;
bins_from_files(Files, HL) ->
    lists:map(fun(F) -> collect(F, HL) end, Files).

delete_files(File) when is_atom(File) ->
    file:delete(File);
delete_files(Files) ->
    lists:foreach(fun(F) -> file:delete(F) end, Files).

%%%
%%% Collects binaries converted to terms in a list. Not very efficient.
%%%
collect(F, HL) ->
    {ok, Fd} = file:open(F, [read, binary, raw, compressed]),
    R = (catch c(Fd, <<>>, 0, ?CHUNKSIZE, HL, [])),
    file:close(Fd),
    R.

c(Fd, Bin0, Size0, NoBytes, HL, L) ->
    case file:read(Fd, NoBytes) of
	{ok, Bin} ->
	    Size = Size0 + byte_size(Bin),
	    NBin = list_to_binary([Bin0, Bin]),
	    c1(Fd, NBin, Size, HL, L);
	eof when Size0 =:= 0 ->
	    lists:reverse(L);
        eof ->
	    test_server:fail({error, premature_eof});
	Error ->
	    test_server:fail(Error)
    end.

c1(Fd, B, BinSize, HL, L) -> 
    case B of 
	<<Size:HL/unit:8, Bin/binary>> ->
	    if 
		Size > BinSize - HL, Size > ?CHUNKSIZE ->
		    c(Fd, B, BinSize, Size + HL, HL, L);
		Size > BinSize - HL ->
		    c(Fd, B, BinSize, ?CHUNKSIZE, HL, L);
		true ->
		    <<BinTerm:Size/binary, R/binary>> = Bin,	    
		    E = case catch binary_to_term(BinTerm) of
                            {'EXIT', _} ->
				test_server:fail({error, bad_object});
			    Term ->
				Term
			end,
		    NBinSize = BinSize - HL - Size,
		    c1(Fd, R, NBinSize, HL, [E | L])
	    end;
	_ ->
	    c(Fd, B, BinSize, ?CHUNKSIZE, HL, L)
    end.
