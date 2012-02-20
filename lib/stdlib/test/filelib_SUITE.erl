%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

-module(filelib_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 wildcard_one/1,wildcard_two/1,wildcard_errors/1,
	 fold_files/1,otp_5960/1,ensure_dir_eexist/1]).

-import(lists, [foreach/2]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?t:minutes(5)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [wildcard_one, wildcard_two, wildcard_errors,
     fold_files, otp_5960, ensure_dir_eexist].

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


wildcard_one(Config) when is_list(Config) ->
    ?line {ok,OldCwd} = file:get_cwd(),
    ?line Dir = filename:join(?config(priv_dir, Config), "wildcard_one"),
    ?line ok = file:make_dir(Dir),
    ?line file:set_cwd(Dir),
    ?line do_wildcard_1(Dir, fun(Wc) -> filelib:wildcard(Wc) end),
    ?line file:set_cwd(OldCwd),
    ?line ok = file:del_dir(Dir),
    ok.

wildcard_two(Config) when is_list(Config) ->
    ?line Dir = filename:join(?config(priv_dir, Config), "wildcard_two"),
    ?line DirB = unicode:characters_to_binary(Dir, file:native_name_encoding()),
    ?line ok = file:make_dir(Dir),
    ?line do_wildcard_1(Dir, fun(Wc) -> io:format("~p~n",[{Wc,Dir, X = filelib:wildcard(Wc, Dir)}]),X  end),
    ?line do_wildcard_1(Dir, fun(Wc) -> io:format("~p~n",[{Wc,DirB, X = filelib:wildcard(Wc, DirB)}]),
					[unicode:characters_to_list(Y,file:native_name_encoding()) || Y <- X] end),
    ?line do_wildcard_1(Dir, fun(Wc) -> filelib:wildcard(Wc, Dir++"/") end),
    case os:type() of
	{win32,_} ->
	    ok;
	_ ->
	    ?line do_wildcard_1(Dir, fun(Wc) -> filelib:wildcard(Wc, "//"++Dir) end)
    end,
    ?line ok = file:del_dir(Dir),
    ok.

wildcard_errors(Config) when is_list(Config) ->
    ?line wcc("{", missing_delimiter),
    ?line wcc("{a", missing_delimiter),
    ?line wcc("{a,", missing_delimiter),
    ?line wcc("{a,b", missing_delimiter),
    ok.

wcc(Wc, Error) ->
    {'EXIT',{{badpattern,Error},
	     [{filelib,compile_wildcard,1,_}|_]}} =
	(catch filelib:compile_wildcard(Wc)),
    {'EXIT',{{badpattern,Error},
	     [{filelib,wildcard,1,_}|_]}} = (catch filelib:wildcard(Wc)),
    {'EXIT',{{badpattern,Error},
	     [{filelib,wildcard,2,_}|_]}} = (catch filelib:wildcard(Wc, ".")).

do_wildcard_1(Dir, Wcf0) ->
    do_wildcard_2(Dir, Wcf0),
    Wcf = fun(Wc0) ->
		  Wc = filename:join(Dir, Wc0),
		  L = Wcf0(Wc),
		  [subtract_dir(N, Dir) || N <- L]
	  end,
    do_wildcard_2(Dir, Wcf).

subtract_dir([C|Cs], [C|Dir]) -> subtract_dir(Cs, Dir);
subtract_dir("/"++Cs, []) -> Cs.

do_wildcard_2(Dir, Wcf) ->
    %% Basic wildcards.
    All = ["abc","abcdef","glurf"],
    ?line Files = mkfiles(lists:reverse(All), Dir),
    ?line All = Wcf("*"),
    ?line ["abc","abcdef"] = Wcf("a*"),
    ?line ["abc","abcdef"] = Wcf("abc*"),
    ?line ["abcdef"] = Wcf("abc???"),
    ?line ["abcdef"] = Wcf("abcd*"),
    ?line ["abcdef"] = Wcf("*def"),
    ?line ["abcdef","glurf"] = Wcf("{*def,gl*}"),
    ?line ["abc","abcdef"] = Wcf("a*{def,}"),
    ?line ["abc","abcdef"] = Wcf("a*{,def}"),

    %% Negative tests.
    ?line [] = Wcf("b*"),
    ?line [] = Wcf("bufflig"),

    ?line del(Files),
    do_wildcard_3(Dir, Wcf).
    
do_wildcard_3(Dir, Wcf) ->
    %% Some character sets.
    All = ["a01","a02","a03","b00","c02","d19"],
    ?line Files = mkfiles(lists:reverse(All), Dir),
    ?line All = Wcf("[a-z]*"),
    ?line All = Wcf("[a-d]*"),
    ?line All = Wcf("[adbc]*"),
    ?line All = Wcf("?[0-9][0-9]"),
    ?line All = Wcf("?[0-1][0-39]"),
    ?line All = Wcf("[abcdefgh][10][01239]"),
    ?line ["a01","a02","a03","b00","c02"] = Wcf("[a-z]0[0-3]"),
    ?line [] = Wcf("?[a-z][0-39]"),
    ?line del(Files),
    do_wildcard_4(Dir, Wcf).

do_wildcard_4(Dir, Wcf) ->
    %% More character sets: tricky characters.
    All = ["a-","aA","aB","aC","a[","a]"],
    ?line Files = mkfiles(lists:reverse(All), Dir),
    ?line All = Wcf("a[][A-C-]"),
    ?line del(Files),
    do_wildcard_5(Dir, Wcf).

do_wildcard_5(Dir, Wcf) ->
    Dirs = ["xa","blurf","yyy"],
    ?line foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, Dirs),
    All = ["blurf/nisse","xa/arne","xa/kalle","yyy/arne"],
    ?line Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    ?line All = Wcf("*/*"),
    ?line ["blurf/nisse","xa/arne","xa/kalle"] = Wcf("{blurf,xa}/*"),
    ?line ["xa/arne","yyy/arne"] = Wcf("*/arne"),
    ?line ["blurf/nisse"] = Wcf("*/nisse"),
    ?line [] = Wcf("mountain/*"),
    ?line [] = Wcf("xa/gurka"),

    %% Cleanup
    ?line del(Files),
    ?line foreach(fun(D) -> ok = file:del_dir(filename:join(Dir, D)) end, Dirs),
    do_wildcard_6(Dir, Wcf).

do_wildcard_6(Dir, Wcf) ->
    Dirs = ["blurf","xa","yyy"],
    ?line foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, Dirs),
    SubDirs = ["blurf/nisse"],
    ?line foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, SubDirs),
    All = ["blurf/nisse/baz","xa/arne","xa/kalle","yyy/arne"],
    ?line Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    ?line Listing = Wcf("**"),
    ?line ["blurf","blurf/nisse","blurf/nisse/baz","xa","xa/arne","xa/kalle","yyy","yyy/arne"] = Listing,
    ?line Listing = Wcf("**/*"),
    ?line ["xa/arne","yyy/arne"] = Wcf("**/arne"),
    ?line ["blurf/nisse"] = Wcf("**/nisse"),
    ?line [] = Wcf("mountain/**"),

    %% Cleanup
    ?line del(Files),
    ?line foreach(fun(D) -> ok = file:del_dir(filename:join(Dir, D)) end, SubDirs),
    ?line foreach(fun(D) -> ok = file:del_dir(filename:join(Dir, D)) end, Dirs),
    do_wildcard_7(Dir, Wcf).

do_wildcard_7(Dir, Wcf) ->
    Dirs0 = ["blurf"],
    ?line foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, Dirs0),
    Dirs1 = ["blurf/nisse"],
    ?line foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, Dirs1),
    Dirs2 = ["blurf/nisse/a", "blurf/nisse/b"],
    ?line foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, Dirs2),
    All = ["blurf/nisse/a/1.txt", "blurf/nisse/b/2.txt", "blurf/nisse/b/3.txt"],
    ?line Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    ?line All = Wcf("**/blurf/**/*.txt"),

    %% Cleanup
    ?line del(Files),
    ?line foreach(fun(Dirs) ->
      foreach(fun(D) -> ok = file:del_dir(filename:join(Dir, D)) end, Dirs)
    end, [Dirs2, Dirs1, Dirs0]).

fold_files(Config) when is_list(Config) ->
    ?line Dir = filename:join(?config(priv_dir, Config), "fold_files"),
    ?line ok = file:make_dir(Dir),
    ?line Dirs = [filename:join(Dir, D) || D <- ["blurf","blurf/blarf"]],
    ?line foreach(fun(D) -> ok = file:make_dir(D) end, Dirs),
    All = ["fb.txt","ko.txt",
	   "blurf/nisse.text","blurf/blarf/aaa.txt","blurf/blarf/urfa.txt"],
    ?line Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    ?line Files0 = filelib:fold_files(Dir, "^", false,
				      fun(H, T) -> [H|T] end, []),
    ?line same_lists(["fb.txt","ko.txt"], Files0, Dir),

    ?line Files1 = filelib:fold_files(Dir, "^", true,
				      fun(H, T) -> [H|T] end, []),
    ?line same_lists(All, Files1, Dir),

    ?line Files2 = filelib:fold_files(Dir, "[.]text$", true,
				      fun(H, T) -> [H|T] end, []),
    ?line same_lists(["blurf/nisse.text"], Files2, Dir),


    ?line Files3 = filelib:fold_files(Dir, "^..[.]", true,
				      fun(H, T) -> [H|T] end, []),
    ?line same_lists(["fb.txt","ko.txt"], Files3, Dir),

    ?line Files4 = filelib:fold_files(Dir, "^ko[.]txt$", true,
				  fun(H, T) -> [H|T] end, []),
    ?line same_lists(["ko.txt"], Files4, Dir),
    ?line Files4 = filelib:fold_files(Dir, "^ko[.]txt$", false,
				      fun(H, T) -> [H|T] end, []),

    ?line [] = filelib:fold_files(Dir, "^$", true,
				  fun(H, T) -> [H|T] end, []),

    %% Cleanup
    ?line del(Files),
    ?line foreach(fun(D) -> ok = file:del_dir(D) end, lists:reverse(Dirs)),
    ?line ok = file:del_dir(Dir).

same_lists(Expected0, Actual0, BaseDir) ->
    Expected = [filename:absname(N, BaseDir) || N <- lists:sort(Expected0)],
    Actual = lists:sort(Actual0),
    Expected = Actual.

mkfiles([H|T], Dir) ->
    Name = filename:join(Dir, H),
    Garbage = [31+random:uniform(95) || _ <- lists:seq(1, random:uniform(1024))],
    file:write_file(Name, Garbage),
    [Name|mkfiles(T, Dir)];
mkfiles([], _) -> [].

del([H|T]) ->
    ok = file:delete(H),
    del(T);
del([]) -> ok.

otp_5960(suite) ->
    [];
otp_5960(doc) ->
    ["Test that filelib:ensure_dir/1 returns ok or {error,Reason}"];
otp_5960(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Dir = filename:join(PrivDir, "otp_5960_dir"),
    ?line Name1 = filename:join(Dir, name1),
    ?line Name2 = filename:join(Dir, name2),
    ?line ok = filelib:ensure_dir(Name1), % parent is created
    ?line ok = filelib:ensure_dir(Name1), % repeating it should be OK
    ?line ok = filelib:ensure_dir(Name2), % parent already exists
    ?line ok = filelib:ensure_dir(Name2), % repeating it should be OK
    ?line Name3 = filename:join(Name1, name3),
    ?line {ok, FileInfo} = file:read_file_info(Dir),
    case os:type() of
	{win32,_} ->
	    %% Not possibly to write protect directories on Windows
	    %% (at least not using file:write_file_info/2).
	    ok;
	_ ->
	    ?line Mode = FileInfo#file_info.mode,
	    ?line NoWriteMode = Mode - 8#00200 - 8#00020 - 8#00002,
	    ?line ok = file:write_file_info(Dir, #file_info{mode=NoWriteMode}), 
	    ?line {error, _} = filelib:ensure_dir(Name3),
	    ?line ok = file:write_file_info(Dir, #file_info{mode=Mode}),
	    ok
    end.

ensure_dir_eexist(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Dir = filename:join(PrivDir, "ensure_dir_eexist"),
    ?line Name = filename:join(Dir, "same_name_as_file_and_dir"),
    ?line ok = filelib:ensure_dir(Name),
    ?line ok = file:write_file(Name, <<"some string\n">>),

    %% There already is a file with the name of the directory
    %% we want to create.
    ?line NeedFile = filename:join(Name, "file"),
    ?line NeedFileB = filename:join(Name, <<"file">>),
    ?line {error, eexist} = filelib:ensure_dir(NeedFile),
    ?line {error, eexist} = filelib:ensure_dir(NeedFileB),
    ok.
