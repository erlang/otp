%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2022. All Rights Reserved.
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

-module(filelib_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 wildcard_one/1,wildcard_two/1,wildcard_errors/1,
	 fold_files/1,otp_5960/1,ensure_dir_eexist/1,ensure_dir_symlink/1,
         ensure_path_single_dir/1, ensure_path_nested_dirs/1,
         ensure_path_binary_args/1, ensure_path_symlink/1,
         ensure_path_relative_path/1, ensure_path_relative_path_dot_dot/1,
         ensure_path_invalid_path/1,
	 wildcard_symlink/1, is_file_symlink/1, file_props_symlink/1,
         find_source/1, find_source_subdir/1, find_source_otp/1,
         safe_relative_path/1, safe_relative_path_links/1]).

-import(lists, [foreach/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(PRIM_FILE, prim_file).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [wildcard_one, wildcard_two, wildcard_errors,
     fold_files, otp_5960, ensure_dir_eexist, ensure_dir_symlink,
     ensure_path_single_dir, ensure_path_nested_dirs, ensure_path_binary_args,
     ensure_path_symlink, ensure_path_relative_path,
     ensure_path_relative_path_dot_dot,
     ensure_path_invalid_path,
     wildcard_symlink, is_file_symlink, file_props_symlink,
     find_source, find_source_subdir, find_source_otp,
     safe_relative_path, safe_relative_path_links].

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
    {ok,OldCwd} = file:get_cwd(),
    Dir = filename:join(proplists:get_value(priv_dir, Config), "wildcard_one"),
    ok = file:make_dir(Dir),
    do_wildcard_1(Dir,
		  fun(Wc) ->
			  filelib:wildcard(Wc, Dir, erl_prim_loader)
		  end),
    file:set_cwd(Dir),
    do_wildcard_1(Dir,
		  fun(Wc) ->
			  L = filelib:wildcard(Wc),
			  L = filelib:wildcard(disable_prefix_opt(Wc)),
			  L = filelib:wildcard(Wc, erl_prim_loader),
			  L = filelib:wildcard(Wc, "."),
			  L = filelib:wildcard(Wc, Dir),
			  L = filelib:wildcard(disable_prefix_opt(Wc), Dir),
			  L = filelib:wildcard(Wc, Dir++"/.")
		  end),
    file:set_cwd(OldCwd),
    ok = file:del_dir(Dir),
    ok.

wildcard_two(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "wildcard_two"),
    ok = file:make_dir(Dir),
    do_wildcard_1(Dir, fun(Wc) -> io:format("~p~n",[{Wc,Dir, X = filelib:wildcard(Wc, Dir)}]),X  end),
    do_wildcard_1(Dir, fun(Wc) -> filelib:wildcard(Wc, Dir++"/") end),
    do_wildcard_1(Dir, fun(Wc) -> filelib:wildcard(Wc, Dir++"/.") end),
    case os:type() of
	{win32,_} ->
	    ok;
	_ ->
	    do_wildcard_1(Dir, fun(Wc) -> filelib:wildcard(Wc, "//"++Dir) end)
    end,
    ok = file:del_dir(Dir),
    ok.

wildcard_errors(Config) when is_list(Config) ->
    wcc("{", missing_delimiter),
    wcc("{a", missing_delimiter),
    wcc("{a,", missing_delimiter),
    wcc("{a,b", missing_delimiter),
    ok.

wcc(Wc, Error) ->
    {'EXIT',{{badpattern,Error},
	     [{filelib,compile_wildcard,1,_}|_]}} =
	(catch filelib:compile_wildcard(Wc)),
    {'EXIT',{{badpattern,Error},
	     [{filelib,wildcard,1,_}|_]}} = (catch filelib:wildcard(Wc)),
    {'EXIT',{{badpattern,Error},
	     [{filelib,wildcard,2,_}|_]}} = (catch filelib:wildcard(Wc, ".")).

disable_prefix_opt([_,$:|_]=Wc) ->
    Wc;
disable_prefix_opt([C|Wc]) when $a =< C, C =< $z; C =:= $@ ->
    %% There is an optimization for patterns that have a literal prefix
    %% (such as "lib/compiler/ebin/*"). Test that we'll get the same result
    %% if we disable that optimization.
    [$[, C, $] | Wc];
disable_prefix_opt(Wc) ->
    Wc.

do_wildcard_1(Dir, Wcf0) ->
    do_wildcard_2(Dir, Wcf0),
    Wcf = fun(Wc0) ->
		  Wc = Dir ++ "/" ++ Wc0,
		  L = Wcf0(Wc),
		  [subtract_dir(N, Dir) || N <- L]
	  end,
    do_wildcard_2(Dir, Wcf).

subtract_dir([C|Cs], [C|Dir]) -> subtract_dir(Cs, Dir);
subtract_dir("/"++Cs, []) -> Cs.

do_wildcard_2(Dir, Wcf) ->
    %% Basic wildcards.
    All = ["abc","abcdef","glurf"],
    Files = mkfiles(lists:reverse(All), Dir),
    All = Wcf("*"),
    ["abc","abcdef"] = Wcf("a*"),
    ["abc","abcdef"] = Wcf("abc*"),
    ["abcdef"] = Wcf("abc???"),
    ["abcdef"] = Wcf("abcd*"),
    ["abcdef"] = Wcf("*def"),
    ["abcdef","glurf"] = Wcf("{*def,gl*}"),
    ["abc","abcdef"] = Wcf("a*{def,}"),
    ["abc","abcdef"] = Wcf("a*{,def}"),

    %% Constant wildcard.
    ["abcdef"] = Wcf("abcdef"),

    %% Negative tests.
    [] = Wcf("b*"),
    [] = Wcf("bufflig"),

    del(Files),
    do_wildcard_3(Dir, Wcf).

do_wildcard_3(Dir, Wcf) ->
    %% Some character sets.
    All = ["a01","a02","a03","b00","c02","d19"],
    Files = mkfiles(lists:reverse(All), Dir),
    All = Wcf("[a-z]*"),
    All = Wcf("[a-d]*"),
    All = Wcf("[adbc]*"),
    All = Wcf("?[0-9][0-9]"),
    All = Wcf("?[0-1][0-39]"),
    All = Wcf("[abcdefgh][10][01239]"),
    ["a01","a02","a03","b00","c02"] = Wcf("[a-z]0[0-3]"),
    [] = Wcf("?[a-z][0-39]"),
    del(Files),
    do_wildcard_4(Dir, Wcf).

do_wildcard_4(Dir, Wcf) ->
    %% More character sets: tricky characters.
    All = ["a-","aA","aB","aC","a[","a]"],
    Files = mkfiles(lists:reverse(All), Dir),
    All = Wcf("a[][A-C-]"),
    ["a-"] = Wcf("a[-]"),
    ["a["] = Wcf("a["),
    del(Files),
    do_wildcard_5(Dir, Wcf).

do_wildcard_5(Dir, Wcf) ->
    Dirs = ["xa","blurf","yyy"],
    foreach(fun(D) -> ok = file:make_dir(filename:join(Dir, D)) end, Dirs),
    All = ["blurf/nisse","xa/arne","xa/kalle","yyy/arne"],
    Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    All = Wcf("*/*"),
    ["blurf/nisse","xa/arne","xa/kalle"] = Wcf("{blurf,xa}/*"),
    ["xa/arne","yyy/arne"] = Wcf("*/arne"),
    ["blurf/nisse"] = Wcf("*/nisse"),
    [] = Wcf("mountain/*"),
    [] = Wcf("xa/gurka"),
    ["blurf/nisse"] = Wcf("blurf/nisse"),

    %% Cleanup
    del(Files),
    foreach(fun(D) -> ok = file:del_dir(filename:join(Dir, D)) end, Dirs),
    do_wildcard_6(Dir, Wcf).

do_wildcard_6(Dir, Wcf) ->
    ok = file:make_dir(filename:join(Dir, "xbin")),
    All = ["xbin/a.x","xbin/b.x","xbin/c.x"],
    Files = mkfiles(All, Dir),
    All = Wcf("xbin/*.x"),
    All = Wcf("xbin/*"),
    ["xbin"] = Wcf("*"),
    All = Wcf("*/*"),
    del(Files),
    ok = file:del_dir(filename:join(Dir, "xbin")),
    do_wildcard_7(Dir, Wcf).

do_wildcard_7(Dir, Wcf) ->
    Dirs = ["blurf","xa","yyy"],
    SubDirs = ["blurf/nisse"],
    foreach(fun(D) ->
		    ok = file:make_dir(filename:join(Dir, D))
	    end, Dirs ++ SubDirs),
    All = ["blurf/nisse/baz","xa/arne","xa/kalle","yyy/arne"],
    Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    Listing = Wcf("**"),
    ["blurf","blurf/nisse","blurf/nisse/baz",
     "xa","xa/arne","xa/kalle","yyy","yyy/arne"] = Listing,
    Listing = Wcf("**/*"),
    ["xa/arne","yyy/arne"] = Wcf("**/arne"),
    ["blurf/nisse"] = Wcf("**/nisse"),
    [] = Wcf("mountain/**"),

    %% Cleanup
    del(Files),
    foreach(fun(D) ->
		    ok = file:del_dir(filename:join(Dir, D))
	    end, SubDirs ++ Dirs),
    do_wildcard_8(Dir, Wcf).

do_wildcard_8(Dir, Wcf) ->
    Dirs0 = ["blurf"],
    Dirs1 = ["blurf/nisse"],
    Dirs2 = ["blurf/nisse/a", "blurf/nisse/b"],
    foreach(fun(D) ->
		    ok = file:make_dir(filename:join(Dir, D))
	    end, Dirs0 ++ Dirs1 ++ Dirs2),
    All = ["blurf/nisse/a/1.txt", "blurf/nisse/b/2.txt", "blurf/nisse/b/3.txt"],
    Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    All = Wcf("**/blurf/**/*.txt"),

    %% Cleanup
    del(Files),
    foreach(fun(D) ->
		    ok = file:del_dir(filename:join(Dir, D))
	    end, Dirs2 ++ Dirs1 ++ Dirs0),
    do_wildcard_9(Dir, Wcf).

do_wildcard_9(Dir, Wcf) ->
    Dirs0 = ["lib","lib/app","lib/app/ebin"],
    Dirs = [filename:join(Dir, D) || D <- Dirs0],
    [ok = file:make_dir(D) || D <- Dirs],
    Files0 = [filename:join("lib/app/ebin", F++".bar") ||
		 F <- ["abc","foo","foobar"]],
    Files = [filename:join(Dir, F) || F <- Files0],
    [ok = file:write_file(F, <<"some content\n">>) || F <- Files],
    Files0 = Wcf("lib/app/ebin/*.bar"),

    %% Cleanup.
    del(Files),
    [ok = file:del_dir(D) || D <- lists:reverse(Dirs)],
    do_wildcard_10(Dir, Wcf).

%% ERL-451/OTP-14577: Escape characters using \\.
do_wildcard_10(Dir, Wcf) ->
    All0 = ["{abc}","abc","def","---","z--","@a,b","@c"],
    All = case os:type() of
              {unix,_} ->
                  %% '?' is allowed in file names on Unix, but
                  %% not on Windows.
                  ["?q"|All0];
              _ ->
                  All0
          end,
    Files = mkfiles(lists:reverse(All), Dir),

    ["{abc}"] = Wcf("\\{a*"),
    ["{abc}"] = Wcf("\\{abc}"),
    ["abc","def","z--"] = Wcf("[a-z]*"),
    ["---","abc","z--"] = Wcf("[a\\-z]*"),
    ["@a,b","@c"] = Wcf("@{a\\,b,c}"),
    ["@c"] = Wcf("@{a,b,c}"),

    case os:type() of
        {unix,_} ->
            ["?q"] = Wcf("\\?q");
        _ ->
            [] = Wcf("\\?q")
    end,

    del(Files),
    wildcard_11(Dir, Wcf).

%% ERL-ERL-1029/OTP-15987: Fix problems with "@/.." and ".." in general.
wildcard_11(Dir, Wcf) ->
    Dirs0 = ["@","@dir","dir@"],
    Dirs = [filename:join(Dir, D) || D <- Dirs0],
    _ = [ok = file:make_dir(D) || D <- Dirs],
    Files0 = ["@a","b@","x","y","z"],
    Files = mkfiles(Files0, Dir),

    ["@","@a","@dir","b@","dir@","x","y","z"] = Wcf("*"),
    ["@"] = Wcf("@"),
    ["@","@a","@dir"] = Wcf("@*"),
    ["@/..","@dir/.."] = Wcf("@*/.."),
    ["@/../@","@/../@a","@/../@dir",
     "@dir/../@","@dir/../@a","@dir/../@dir"] = Wcf("@*/../@*"),

    %% Non-directories followed by "/.." should not match any files.
    [] = Wcf("@a/.."),
    [] = Wcf("x/.."),

    %% Cleanup.
    del(Files),
    [ok = file:del_dir(D) || D <- Dirs],
    ok.

fold_files(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "fold_files"),
    ok = file:make_dir(Dir),
    Dirs = [filename:join(Dir, D) || D <- ["blurf","blurf/blarf"]],
    foreach(fun(D) -> ok = file:make_dir(D) end, Dirs),
    All = ["fb.txt","ko.txt",
	   "blurf/nisse.text","blurf/blarf/aaa.txt","blurf/blarf/urfa.txt"],
    Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    Files0 = filelib:fold_files(Dir, "^", false,
				fun(H, T) -> [H|T] end, []),
    same_lists(["fb.txt","ko.txt"], Files0, Dir),

    Files1 = filelib:fold_files(Dir, "^", true,
				fun(H, T) -> [H|T] end, []),
    same_lists(All, Files1, Dir),

    Files2 = filelib:fold_files(Dir, "[.]text$", true,
				fun(H, T) -> [H|T] end, []),
    same_lists(["blurf/nisse.text"], Files2, Dir),


    Files3 = filelib:fold_files(Dir, "^..[.]", true,
				fun(H, T) -> [H|T] end, []),
    same_lists(["fb.txt","ko.txt"], Files3, Dir),

    Files4 = filelib:fold_files(Dir, "^ko[.]txt$", true,
				fun(H, T) -> [H|T] end, []),
    same_lists(["ko.txt"], Files4, Dir),
    Files4 = filelib:fold_files(Dir, "^ko[.]txt$", false,
				fun(H, T) -> [H|T] end, []),

    [] = filelib:fold_files(Dir, "^$", true,
			    fun(H, T) -> [H|T] end, []),

    %% Cleanup
    del(Files),
    foreach(fun(D) -> ok = file:del_dir(D) end, lists:reverse(Dirs)),
    ok = file:del_dir(Dir).

same_lists(Expected0, Actual0, BaseDir) ->
    Expected = [filename:absname(N, BaseDir) || N <- lists:sort(Expected0)],
    Actual = lists:sort(Actual0),
    Expected = Actual.

mkfiles([H|T], Dir) ->
    Name = filename:join(Dir, H),
    Garbage = [31+rand:uniform(95) || _ <- lists:seq(1, rand:uniform(1024))],
    file:write_file(Name, Garbage),
    [Name|mkfiles(T, Dir)];
mkfiles([], _) -> [].

del([H|T]) ->
    ok = file:delete(H),
    del(T);
del([]) -> ok.

%% Test that filelib:ensure_dir/1 returns ok or {error,Reason}.
otp_5960(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "otp_5960_dir"),
    Name1 = filename:join(Dir, name1),
    Name2 = filename:join(Dir, name2),
    ok = filelib:ensure_dir(Name1), % parent is created
    ok = filelib:ensure_dir(Name1), % repeating it should be OK
    ok = filelib:ensure_dir(Name2), % parent already exists
    ok = filelib:ensure_dir(Name2), % repeating it should be OK
    Name3 = filename:join(Name1, name3),
    {ok, FileInfo} = file:read_file_info(Dir),
    case os:type() of
	{win32,_} ->
	    %% Not possibly to write protect directories on Windows
	    %% (at least not using file:write_file_info/2).
	    ok;
	_ ->
	    Mode = FileInfo#file_info.mode,
	    NoWriteMode = Mode - 8#00200 - 8#00020 - 8#00002,
	    ok = file:write_file_info(Dir, #file_info{mode=NoWriteMode}),
	    {error, _} = filelib:ensure_dir(Name3),
	    ok = file:write_file_info(Dir, #file_info{mode=Mode}),
	    ok
    end.

ensure_dir_eexist(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "ensure_dir_eexist"),
    Name = filename:join(Dir, "same_name_as_file_and_dir"),
    ok = filelib:ensure_dir(Name),
    ok = file:write_file(Name, <<"some string\n">>),

    %% There already is a file with the name of the directory
    %% we want to create.
    NeedFile = filename:join(Name, "file"),
    NeedFileB = filename:join(Name, <<"file">>),
    {error, eexist} = filelib:ensure_dir(NeedFile),
    {error, eexist} = filelib:ensure_dir(NeedFileB),
    ok.

ensure_dir_symlink(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "ensure_dir_symlink"),
    Name = filename:join(Dir, "same_name_as_file_and_dir"),
    ok = filelib:ensure_dir(Name),
    ok = file:write_file(Name, <<"some string\n">>),
    %% With a symlink to the directory.
    Symlink = filename:join(PrivDir, "ensure_dir_symlink_link"),
    case file:make_symlink(Dir, Symlink) of
        {error,enotsup} ->
            {skip,"Symlinks not supported on this platform"};
        {error,eperm} ->
            {win32,_} = os:type(),
            {skip,"Windows user not privileged to create symlinks"};
        ok ->
            SymlinkedName = filename:join(Symlink, "same_name_as_file_and_dir"),
            ok = filelib:ensure_dir(SymlinkedName)
    end.

ensure_path_single_dir(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "ensure_path_single_dir"),
    ok = filelib:ensure_path(Dir),
    true = filelib:is_dir(Dir).

ensure_path_nested_dirs(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BaseDir = filename:join(PrivDir, "ensure_path_nested_dirs"),
    Path = filename:join(BaseDir, "foo/bar/baz"),
    ok = filelib:ensure_path(Path),
    true = filelib:is_dir(Path).

ensure_path_binary_args(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BaseDir = filename:join(PrivDir, "ensure_path_binary_args"),
    Path = filename:join(BaseDir, "foo/bar/baz"),
    ok = filelib:ensure_path(list_to_binary(Path)),
    true = filelib:is_dir(Path).

ensure_path_invalid_path(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BaseDir = filename:join(PrivDir, "ensure_path_invalid_path"),
    ok = filelib:ensure_path(BaseDir),
    FileName =  filename:join(BaseDir, "foo"),
    ok = file:write_file(FileName, <<"eh?\n">>),
    Path = filename:join(FileName, "foo/bar/baz"),
    case filelib:ensure_path(Path) of
        {error,enotdir} ->
            ok;
        {error,enoent} ->
            %% The documentation has the following to say about the
            %% `enotdir` error reason:
            %%
            %% "On some platforms, enoent is returned instead."
            ok
    end,
    false = filelib:is_dir(Path).

ensure_path_relative_path(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BaseDir = filename:join(PrivDir, "ensure_path_relative_path"),
    ok = filelib:ensure_path(BaseDir),
    ok = file:set_cwd(BaseDir),
    Path = filename:join(BaseDir, "foo/bar/baz"),
    ok = filelib:ensure_path("foo/bar/baz"),
    true = filelib:is_dir(Path).

ensure_path_relative_path_dot_dot(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BaseDir = filename:join(PrivDir, "ensure_path_relative_path"),
    SubDir = filename:join(BaseDir, "dot_dot"),
    ok = filelib:ensure_path(SubDir),
    ok = file:set_cwd(SubDir),
    Path = filename:join(BaseDir, "foo/bar/baz"),
    ok = filelib:ensure_path("../foo/bar/baz"),
    true = filelib:is_dir(Path).

ensure_path_symlink(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "ensure_path_symlink"),
    Name = filename:join(Dir, "same_name_as_file_and_dir"),
    ok = filelib:ensure_path(Dir),
    ok = file:write_file(Name, <<"some string\n">>),
    %% With a symlink to the directory.
    Symlink = filename:join(PrivDir, "ensure_path_symlink_link"),
    case file:make_symlink(Dir, Symlink) of
        {error,enotsup} ->
            {skip,"Symlinks not supported on this platform"};
        {error,eperm} ->
            {win32,_} = os:type(),
            {skip,"Windows user not privileged to create symlinks"};
        ok ->
            SymlinkedName = filename:join(Symlink, "same_name_as_file_and_dir"),
            ok = filelib:ensure_dir(SymlinkedName)
    end.

wildcard_symlink(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?MODULE_STRING++"_wildcard_symlink"),
    SubDir = filename:join(Dir, "sub"),
    AFile = filename:join(SubDir, "a_file"),
    Alias = filename:join(Dir, "symlink"),
    ok = file:make_dir(Dir),
    ok = file:make_dir(SubDir),
    ok = file:write_file(AFile, "not that big\n"),
    case file:make_symlink(AFile, Alias) of
	{error, enotsup} ->
	    {skip, "Links not supported on this platform"};
	{error, eperm} ->
	    {win32,_} = os:type(),
	    {skip, "Windows user not privileged to create symlinks"};
	ok ->
	    ["sub","symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "*"))),
	    ["symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "symlink"))),
	    ["sub","symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "*"),
						erl_prim_loader)),
	    ["symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "symlink"),
						erl_prim_loader)),
	    ["sub","symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "*"),
						?PRIM_FILE)),
	    ["symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "symlink"),
						?PRIM_FILE)),
	    ok = file:delete(AFile),
	    %% The symlink should still be visible even when its target
	    %% has been deleted.
	    ["sub","symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "*"))),
	    ["symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "symlink"))),
	    ["sub","symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "*"),
						erl_prim_loader)),
	    ["symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "symlink"),
						erl_prim_loader)),
	    ["sub","symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "*"),
						?PRIM_FILE)),
	    ["symlink"] =
		basenames(Dir, filelib:wildcard(filename:join(Dir, "symlink"),
						?PRIM_FILE)),
	    ok
    end.

basenames(Dir, Files) ->
    [begin
	 Dir = filename:dirname(F),
	 filename:basename(F)
     end || F <- Files].

is_file_symlink(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?MODULE_STRING++"_is_file_symlink"),
    SubDir = filename:join(Dir, "sub"),
    AFile = filename:join(SubDir, "a_file"),
    DirAlias = filename:join(Dir, "dir_symlink"),
    FileAlias = filename:join(Dir, "file_symlink"),
    ok = file:make_dir(Dir),
    ok = file:make_dir(SubDir),
    ok = file:write_file(AFile, "not that big\n"),
    case file:make_symlink(SubDir, DirAlias) of
	{error, enotsup} ->
	    {skip, "Links not supported on this platform"};
	{error, eperm} ->
	    {win32,_} = os:type(),
	    {skip, "Windows user not privileged to create symlinks"};
	ok ->
	    true = filelib:is_dir(DirAlias),
	    true = filelib:is_dir(DirAlias, erl_prim_loader),
	    true = filelib:is_dir(DirAlias, ?PRIM_FILE),
	    true = filelib:is_file(DirAlias),
	    true = filelib:is_file(DirAlias, erl_prim_loader),
	    true = filelib:is_file(DirAlias, ?PRIM_FILE),
	    ok = file:make_symlink(AFile,FileAlias),
	    true = filelib:is_file(FileAlias),
	    true = filelib:is_file(FileAlias, erl_prim_loader),
	    true = filelib:is_file(FileAlias, ?PRIM_FILE),
	    true = filelib:is_regular(FileAlias),
	    true = filelib:is_regular(FileAlias, erl_prim_loader),
	    true = filelib:is_regular(FileAlias, ?PRIM_FILE),
	    ok
    end.

file_props_symlink(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?MODULE_STRING++"_file_props_symlink"),
    AFile = filename:join(Dir, "a_file"),
    Alias = filename:join(Dir, "symlink"),
    ok = file:make_dir(Dir),
    ok = file:write_file(AFile, "not that big\n"),
    case file:make_symlink(AFile, Alias) of
	{error, enotsup} ->
	    {skip, "Links not supported on this platform"};
	{error, eperm} ->
	    {win32,_} = os:type(),
	    {skip, "Windows user not privileged to create symlinks"};
	ok ->
	    {_,_} = LastMod = filelib:last_modified(AFile),
	    LastMod = filelib:last_modified(Alias),
	    LastMod = filelib:last_modified(Alias, erl_prim_loader),
	    LastMod = filelib:last_modified(Alias, ?PRIM_FILE),
	    FileSize = filelib:file_size(AFile),
	    FileSize = filelib:file_size(Alias),
	    FileSize = filelib:file_size(Alias, erl_prim_loader),
	    FileSize = filelib:file_size(Alias, ?PRIM_FILE)
    end.

find_source(Config) when is_list(Config) ->
    %% filelib:find_{file,source}() does not work if the files are
    %% cover-compiled. To make sure that the test does not fail
    %% when the STDLIB is cover-compiled, search for modules in
    %% the compiler application.

    BeamFile = code:which(compile),
    BeamName = filename:basename(BeamFile),
    BeamDir = filename:dirname(BeamFile),
    SrcName = filename:basename(BeamFile, ".beam") ++ ".erl",

    {ok, BeamFile} = filelib:find_file(BeamName, BeamDir),
    {ok, BeamFile} = filelib:find_file(BeamName, BeamDir, []),
    {ok, BeamFile} = filelib:find_file(BeamName, BeamDir, [{"",""},{"ebin","src"}]),
    {error, not_found} = filelib:find_file(BeamName, BeamDir, [{"ebin","src"}]),

    {ok, SrcFile} = filelib:find_file(SrcName, BeamDir),
    {ok, SrcFile} = filelib:find_file(SrcName, BeamDir, []),
    {ok, SrcFile} = filelib:find_file(SrcName, BeamDir, [{"foo","bar"},{"ebin","src"}]),
    {error, not_found} = filelib:find_file(SrcName, BeamDir, [{"",""}]),

    {ok, SrcFile} = filelib:find_source(BeamFile),
    {ok, SrcFile} = filelib:find_source(BeamName, BeamDir),
    {ok, SrcFile} = filelib:find_source(BeamName, BeamDir,
                                         [{".erl",".yrl",[{"",""}]},
                                          {".beam",".erl",[{"ebin","src"}]}]),
    {error, not_found} = filelib:find_source(BeamName, BeamDir,
                                              [{".erl",".yrl",[{"",""}]}]),

    {ok, ParserErl} = filelib:find_source(code:which(core_parse)),
    ParserErlName = filename:basename(ParserErl),
    ParserErlDir = filename:dirname(ParserErl),
    {ok, ParserYrl} = filelib:find_source(ParserErl),
    "lry." ++ _ = lists:reverse(ParserYrl),
    {ok, ParserYrl} = filelib:find_source(ParserErlName, ParserErlDir,
                                           [{".beam",".erl",[{"ebin","src"}]},
                                            {".erl",".yrl",[{"",""}]}]),

    %% find_source automatically checks the local directory regardless of rules
    {ok, ParserYrl} = filelib:find_source(ParserErl),
    {ok, ParserYrl} = filelib:find_source(ParserErlName, ParserErlDir,
                                          [{".erl",".yrl",[{"ebin","src"}]}]),

    %% find_file does not check the local directory unless in the rules
    ParserYrlName = filename:basename(ParserYrl),
    ParserYrlDir = filename:dirname(ParserYrl),
    {ok, ParserYrl} = filelib:find_file(ParserYrlName, ParserYrlDir,
                                        [{"",""}]),
    {error, not_found} = filelib:find_file(ParserYrlName, ParserYrlDir,
                                           [{"ebin","src"}]),

    %% local directory is in the default list for find_file
    {ok, ParserYrl} = filelib:find_file(ParserYrlName, ParserYrlDir),
    {ok, ParserYrl} = filelib:find_file(ParserYrlName, ParserYrlDir, []),
    ok.

find_source_subdir(Config) when is_list(Config) ->
    BeamFile = code:which(inets), % Located in lib/inets/src/inets_app/
    BeamName = filename:basename(BeamFile),
    BeamDir = filename:dirname(BeamFile),
    SrcName = filename:basename(BeamFile, ".beam") ++ ".erl",

    {ok, SrcFile} = filelib:find_source(BeamName, BeamDir),
    SrcName = filename:basename(SrcFile),

    {error, not_found} =
        filelib:find_source(BeamName, BeamDir,
                            [{".beam",".erl",[{"ebin","src"}]}]),
    {ok, SrcFile} =
        filelib:find_source(BeamName, BeamDir,
                            [{".beam",".erl",
                              [{"ebin",filename:join("src", "*")}]}]),

    {ok, SrcFile} = filelib:find_file(SrcName, BeamDir),

    ok.

%% Test that all available beam files in Erlang/OTP can be
%% tracked to their source files.
find_source_otp(Config) when is_list(Config) ->
    %% We do this in a peer as testcases before this may have
    %% edited the code path and thus more modules show up as
    %% available than should.
    {ok, Peer, Node} = ?CT_PEER(#{ env => [{"ERL_LIBS", false}] }),
    erpc:call(
      Node,
      fun() ->
              lists:map(
                fun F({Module, preloaded, Loaded}) ->
                        F({Module, code:where_is_file(Module ++ ".beam"), Loaded});
		    F({Module, cover_compiled, Loaded}) ->
		        ok;
                    F({Module, Filename, _Loaded}) ->
                        case filelib:find_source(Filename) of
                            {ok, _} -> ok;
                            {error,_} = E ->
                                ct:fail({failed_to_find, Module, Filename, E})
                        end
                end, code:all_available())
      end),
    peer:stop(Peer),
    ok.

safe_relative_path(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Root = filename:join(PrivDir, "filelib_SUITE_safe_relative_path"),
    ok = file:make_dir(Root),
    ok = file:set_cwd(Root),

    ok = file:make_dir("a"),
    ok = file:set_cwd("a"),
    ok = file:make_dir("b"),
    ok = file:set_cwd("b"),
    ok = file:make_dir("c"),

    ok = file:set_cwd(Root),

    "a" = test_srp("a"),
    "a/b" = test_srp("a/b"),
    "a/b" = test_srp("a/./b"),
    "a/b" = test_srp("a/./b/."),

    "" = test_srp("a/.."),
    "" = test_srp("a/./.."),
    "" = test_srp("a/../."),
    "a" = test_srp("a/b/.."),
    "a" = test_srp("a/../a"),
    "a" = test_srp("a/../a/../a"),
    "a/b/c" = test_srp("a/../a/b/c"),

    unsafe = test_srp("a/../.."),
    unsafe = test_srp("a/../../.."),
    unsafe = test_srp("a/./../.."),
    unsafe = test_srp("a/././../../.."),
    unsafe = test_srp("a/b/././../../.."),

    unsafe = test_srp(PrivDir),                 %Absolute path.

    ok.

test_srp(RelPath) ->
    Res = do_test_srp(RelPath),
    Res = case do_test_srp(list_to_binary(RelPath)) of
              Bin when is_binary(Bin) ->
                  binary_to_list(Bin);
              Other ->
                  Other
          end.

do_test_srp(RelPath) ->
    {ok,Root} = file:get_cwd(),
    ok = file:set_cwd(RelPath),
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Root),
    case filelib:safe_relative_path(RelPath, Cwd) of
        unsafe ->
            true = length(Cwd) < length(Root),
            unsafe;
        "" ->
            "";
        SafeRelPath ->
            ok = file:set_cwd(SafeRelPath),
            {ok,Cwd} = file:get_cwd(),
            true = length(Cwd) >= length(Root),
            ok = file:set_cwd(Root),
            SafeRelPath
    end.

safe_relative_path_links(Config) ->
    PrivDir = ?config(priv_dir, Config),
    BaseDir = filename:join(PrivDir, "filelib_SUITE_safe_relative_path_links"),
    ok = file:make_dir(BaseDir),
    try
        case check_symlink_support(BaseDir) of
            true ->
                simple_test(BaseDir),
                inside_directory_test(BaseDir),
                nested_links_test(BaseDir),
                loop_test(BaseDir),
                loop_with_parent_test(BaseDir),
                revist_links_test(BaseDir);
            false ->
                {skipped, "This platform/user can't create symlinks."}
        end
    after
        %% This test leaves some rather nasty links that may screw with
        %% z_SUITE's core file search, so we must make sure everything's
        %% removed regardless of what happens.
        rm_rf(BaseDir)
    end.

check_symlink_support(BaseDir) ->
    Canary = filename:join(BaseDir, "symlink_canary"),
    Link = filename:join(BaseDir, "symlink_canary_link"),
    ok = file:write_file(Canary, <<"chirp">>),
    ok =:= file:make_symlink(Canary, Link).

simple_test(BaseDir) ->
    file:make_dir(filename:join(BaseDir, "simple_test")),
    file:make_symlink("..", filename:join(BaseDir, "simple_test/link")),

    unsafe = filelib:safe_relative_path("link/file", filename:join(BaseDir, "simple_test")),
    "file" = filelib:safe_relative_path("file", filename:join(BaseDir, "simple_test/link")).

inside_directory_test(BaseDir) ->
    file:make_dir(filename:join(BaseDir, "inside_directory_test")),
    file:make_symlink("..", filename:join(BaseDir, "inside_directory_test/link")),

    unsafe = filelib:safe_relative_path("link/file", filename:join(BaseDir, "inside_directory_test")),
    "file" = filelib:safe_relative_path("file", filename:join(BaseDir, "inside_directory_test/link")).

nested_links_test(BaseDir) ->
    file:make_dir(filename:join(BaseDir, "nested_links_test")),
    file:make_dir(filename:join(BaseDir, "nested_links_test/a")),
    file:make_symlink("a/b/c", filename:join(BaseDir, "nested_links_test/link")),
    file:make_symlink("..", filename:join(BaseDir, "nested_links_test/a/b")),

    "c/file" = filelib:safe_relative_path("link/file", filename:join(BaseDir, "nested_links_test")),

    file:delete(filename:join(BaseDir, "nested_links_test/a/b")),
    file:make_symlink("../..", filename:join(BaseDir, "nested_links_test/a/b")),
    unsafe = filelib:safe_relative_path("link/file", filename:join(BaseDir, "nested_links_test")).

loop_test(BaseDir) ->
    file:make_dir(filename:join(BaseDir, "loop_test")),

    file:make_symlink("b", filename:join(BaseDir, "loop_test/c")),
    file:make_symlink("c", filename:join(BaseDir, "loop_test/b")),

    unsafe = filelib:safe_relative_path("c", filename:join(BaseDir, "loop_test")).

loop_with_parent_test(BaseDir) ->
    file:make_dir(filename:join(BaseDir, "loop_with_parent_test")),
    file:make_dir(filename:join(BaseDir, "loop_with_parent_test/bar")),

    file:make_symlink("../bar/foo", filename:join(BaseDir, "loop_with_parent_test/bar/foo")),

    unsafe = filelib:safe_relative_path("bar/foo", filename:join(BaseDir, "loop_with_parent_test")).

revist_links_test(BaseDir) ->
    file:make_dir(filename:join(BaseDir, "revist_links_test")),

    file:make_symlink(".", filename:join(BaseDir, "revist_links_test/x")),
    file:make_symlink("x", filename:join(BaseDir, "revist_links_test/y")),
    file:make_symlink("y", filename:join(BaseDir, "revist_links_test/z")),

    "file" = filelib:safe_relative_path("x/file", filename:join(BaseDir, "revist_links_test")),
    "file" = filelib:safe_relative_path("y/x/file", filename:join(BaseDir, "revist_links_test")),
    "file" = filelib:safe_relative_path("x/x/file", filename:join(BaseDir, "revist_links_test")),
    "file" = filelib:safe_relative_path("x/y/x/y/file", filename:join(BaseDir, "revist_links_test")),
    "file" = filelib:safe_relative_path("x/y/z/x/y/z/file", filename:join(BaseDir, "revist_links_test")),
    "file" = filelib:safe_relative_path("x/x/y/y/file", filename:join(BaseDir, "revist_links_test")),
    "file" = filelib:safe_relative_path("x/z/y/x/./z/foo/../x/./y/file", filename:join(BaseDir, "revist_links_test")).

rm_rf(Dir) ->
    case file:read_link_info(Dir) of
        {ok, #file_info{type = directory}} ->
            {ok, Content} = file:list_dir_all(Dir),
            [ rm_rf(filename:join(Dir,C)) || C <- Content ],
            file:del_dir(Dir),
            ok;
        {ok, #file_info{}} ->
            file:delete(Dir);
        _ ->
            ok
    end.
