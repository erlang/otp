%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
-module(tar_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, borderline/1, atomic/1, long_names/1,
	 create_long_names/1, bad_tar/1, errors/1, extract_from_binary/1,
	 extract_from_binary_compressed/1,
	 extract_from_open_file/1, symlinks/1, open_add_close/1, cooked_compressed/1,
	 memory/1,unicode/1]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [borderline, atomic, long_names, create_long_names,
     bad_tar, errors, extract_from_binary,
     extract_from_binary_compressed, extract_from_open_file,
     symlinks, open_add_close, cooked_compressed, memory, unicode].

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


borderline(doc) ->
    ["Test creating, listing and extracting one file from an archive",
     "multiple times with different file sizes. ",
     "Also check that the file attributes of the extracted file has survived."];
borderline(Config) when is_list(Config) ->

    %% Note: We cannot use absolute paths, because the pathnames will be
    %% too long for the limit allowed in tar files (100 characters).
    %% Therefore, strip off the current working directory from the front
    %% of the private directory path.

    ?line {ok, Cwd} = file:get_cwd(),
    ?line RootDir = ?config(priv_dir, Config),
    ?line TempDir = remove_prefix(Cwd++"/", filename:join(RootDir, "borderline")),
    ?line ok = file:make_dir(TempDir),

    ?line Record = 512,
    ?line Block = 20 * Record,

    ?line lists:foreach(fun(Size) -> borderline_test(Size, TempDir) end,
			[0, 1, 10, 13, 127, 333, Record-1, Record, Record+1,
			 Block-2*Record-1, Block-2*Record, Block-2*Record+1,
			 Block-Record-1, Block-Record, Block-Record+1,
			 Block-1, Block, Block+1,
			 Block+Record-1, Block+Record, Block+Record+1]),

    %% Clean up.
    ?line delete_files([TempDir]),

    ok.

borderline_test(Size, TempDir) ->
    ?line Archive = filename:join(TempDir, "ar_"++integer_to_list(Size)++".tar"),
    ?line Name = filename:join(TempDir, "file_"++integer_to_list(Size)),
    ?line io:format("Testing size ~p", [Size]),

    %% Create a file and archive it.
    ?line {_, _, X0} = erlang:now(),
    ?line file:write_file(Name, random_byte_list(X0, Size)),
    ?line ok = erl_tar:create(Archive, [Name]),
    ?line ok = file:delete(Name),

    %% Verify listing and extracting.
    ?line {ok, [Name]} = erl_tar:table(Archive),
    ?line ok = erl_tar:extract(Archive, [verbose]),

    %% Verify contents of extracted file.
    ?line {ok, Bin} = file:read_file(Name),
    ?line true = match_byte_list(X0, binary_to_list(Bin)),

    %% Verify that Unix tar can read it.
    ?line tar_tf(Archive, Name),

    ok.

tar_tf(Archive, Name) ->
    case os:type() of
	{unix, _} ->
	    tar_tf1(Archive, Name);
	_ ->
	    ok
    end.

tar_tf1(Archive, Name) ->
    ?line Expect = Name ++ "\n",
    ?line cmd_expect("tar tf " ++ Archive, Expect).

%% We can't use os:cmd/1, because Unix 'tar tf Name' on Solaris never
%% terminates when given an archive of a size it doesn't like.

cmd_expect(Cmd, Expect) ->
    ?line Port = open_port({spawn, make_cmd(Cmd)}, [stream, in, eof]),
    ?line get_data(Port, Expect).

get_data(Port, Expect) ->
    receive
	{Port, {data, Bytes}} ->
	    ?line get_data(Port, match_output(Bytes, Expect, Port));
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end, 
	    ?line match_output(eof, Expect, Port)
    end.

match_output([C|Output], [C|Expect], Port) ->
    ?line match_output(Output, Expect, Port);
match_output([_|_], [_|_], Port) ->
    ?line kill_port_and_fail(Port, badmatch);
match_output([X|Output], [], Port) ->
    ?line kill_port_and_fail(Port, {too_much_data, [X|Output]});
match_output([], Expect, _Port) ->
    Expect;
match_output(eof, [], _Port) ->
    [];
match_output(eof, _Expect, Port) ->
    ?line kill_port_and_fail(Port, unexpected_end_of_input).

kill_port_and_fail(Port, Reason) ->
    unlink(Port),
    exit(Port, die),
    test_server:fail(Reason).

make_cmd(Cmd) ->
    case os:type() of
	{win32, _} -> lists:concat(["cmd /c",  Cmd]);
	{unix, _}  -> lists:concat(["sh -c '",  Cmd,  "'"])
    end.

%% Verifies a random byte list.

match_byte_list(X0, [Byte|Rest]) ->
    X = next_random(X0),
    case (X bsr 26) band 16#ff of
	Byte -> match_byte_list(X, Rest);
	_ -> false
    end;
match_byte_list(_, []) ->
    true.

%% Generates a random byte list.

random_byte_list(X0, Count) ->
    random_byte_list(X0, Count, []).

random_byte_list(X0, Count, Result) when Count > 0->
    X = next_random(X0),
    random_byte_list(X, Count-1, [(X bsr 26) band 16#ff|Result]);
random_byte_list(_X, 0, Result) ->
    lists:reverse(Result).

%% This RNG is from line 21 on page 102 in Knuth: The Art of Computer Programming,
%% Volume II, Seminumerical Algorithms.

next_random(X) ->
    (X*17059465+1) band 16#fffffffff.

atomic(doc) ->
    ["Test the 'atomic' operations: create/extract/table, on compressed "
     "and uncompressed archives."
     "Also test the 'cooked' option."];
atomic(suite) -> [];
atomic(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(priv_dir, Config)),
    ?line DataFiles = data_files(),
    ?line Names = [Name || {Name,_,_} <- DataFiles],
    io:format("Names: ~p", [Names]),

    %% Create an uncompressed archive.  The compressed flag should still be
    %% allowed when listing contents or extracting.

    ?line Tar1 = "uncompressed.tar",
    ?line erl_tar:create(Tar1, Names, []),
    ?line {ok, Names} = erl_tar:table(Tar1, []),
    ?line {ok, Names} = erl_tar:table(Tar1, [compressed]),
    ?line {ok, Names} = erl_tar:table(Tar1, [cooked]),
    ?line {ok, Names} = erl_tar:table(Tar1, [compressed,cooked]),
    
    %% Create a compressed archive.

    ?line Tar2 = "compressed.tar",
    ?line erl_tar:create(Tar2, Names, [compressed]),
    ?line {ok, Names} = erl_tar:table(Tar2, [compressed]),
    ?line {error, Reason} = erl_tar:table(Tar2, []),
    ?line {ok, Names} = erl_tar:table(Tar2, [compressed,cooked]),
    ?line {error, Reason} = erl_tar:table(Tar2, [cooked]),
    ?line ok = io:format("No compressed option: ~p, ~s",
			 [Reason, erl_tar:format_error(Reason)]),

    %% Same test again, but this time created with 'cooked'

    ?line Tar3 = "uncompressed_cooked.tar",
    ?line erl_tar:create(Tar3, Names, [cooked]),
    ?line {ok, Names} = erl_tar:table(Tar3, []),
    ?line {ok, Names} = erl_tar:table(Tar3, [compressed]),
    ?line {ok, Names} = erl_tar:table(Tar3, [cooked]),
    ?line {ok, Names} = erl_tar:table(Tar3, [compressed,cooked]),
    
    ?line Tar4 = "compressed_cooked.tar",
    ?line erl_tar:create(Tar4, Names, [compressed,cooked]),
    ?line {ok, Names} = erl_tar:table(Tar4, [compressed]),
    ?line {error, Reason} = erl_tar:table(Tar4, []),
    ?line {ok, Names} = erl_tar:table(Tar4, [compressed,cooked]),
    ?line {error, Reason} = erl_tar:table(Tar4, [cooked]),
    ?line ok = io:format("No compressed option: ~p, ~s",
			 [Reason, erl_tar:format_error(Reason)]),

    %% Clean up.
    ?line delete_files([Tar1,Tar2,Tar3,Tar4|Names]),

    ok.

%% Returns a sequence of characters.

char_seq(N, First) ->
    char_seq(N, First, []).

char_seq(0, _, Result) ->
    Result;
char_seq(N, C, Result) when C < 127 ->
    char_seq(N-1, C+1, [C|Result]);
char_seq(N, _, Result) ->
    char_seq(N, $!, Result).

data_files() ->
    Files = [{"first_file", 1555, $a},
	     {"small_file", 7, $d},
	     {"big_file", 23875, $e},
	     {"last_file", 7500, $g}],
    create_files(Files),
    Files.

create_files([{Name, Size, First}|Rest]) ->
    ok = file:write_file(Name, char_seq(Size, First)),
    create_files(Rest);
create_files([]) ->
    ok.

long_names(doc) ->
    ["Test to extract an Unix tar file containing filenames longer than 100 ",
    "characters and empty directories."];
long_names(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Long = filename:join(DataDir, "long_names.tar"),
    run_in_short_tempdir(Config,
			 fun() -> do_long_names(Long) end).

do_long_names(Long) ->
    %% Try table/2 and extract/2.
    ?line case erl_tar:table(Long, [verbose]) of
	      {ok,List} when is_list(List) ->
		  ?line io:format("~p\n", [List])
	  end,

    ?line {ok,Cwd} = file:get_cwd(),
    ?line ok = erl_tar:extract(Long),
    ?line Base = filename:join([Cwd, "original_software", "written_by",
				"a_bunch_of_hackers",
				"spending_all_their_nights",
				"still", "not_long_enough",
				"but_soon_it_will_be"]),

    %% Verify that the empty directory was created.
    ?line EmptyDir = filename:join(Base, "empty_directory"),
    ?line {ok, #file_info{type=directory}} = file:read_file_info(EmptyDir),

    %% Verify that the files were created.
    ?line {ok,First} = file:read_file(filename:join(Base, "first_file")),
    ?line {ok,Second} = file:read_file(filename:join(Base, "second_file")),
    ?line "Here"++_ = binary_to_list(First),
    ?line "And"++_ = binary_to_list(Second),

    ok.

create_long_names(doc) ->
    ["Creates a tar file from a deep directory structure (filenames are ",
     "longer than 100 characters)."];
create_long_names(Config) when is_list(Config) ->
    run_in_short_tempdir(Config, fun create_long_names/0).
    
create_long_names() ->
    ?line {ok,Dir} = file:get_cwd(),
    Dirs = ["aslfjkshjkhliuf",
	    "asdhjfehnbfsky",
	    "sahajfskdfhsz",
	    "asldfkdlfy4y8rchg",
	    "f7nafhjgffagkhsfkhsjk",
	    "dfjasldkfjsdkfjashbv"],

    ?line DeepDir = make_dirs(Dirs, []),
    ?line AFile = filename:join(DeepDir, "a_file"),
    ?line Hello = "hello, world\n",
    ?line ok = file:write_file(AFile, Hello),
    ?line TarName = filename:join(Dir,  "my_tar_with_long_names.tar"),
    ?line ok = erl_tar:create(TarName, [AFile]),

    %% Print contents.
    ?line ok = erl_tar:tt(TarName),

    %% Extract and verify.
    ?line ExtractDir = "extract_dir",
    ?line ok = file:make_dir(ExtractDir),
    ?line ok = erl_tar:extract(TarName, [{cwd,ExtractDir}]),
    ?line {ok, Bin} = file:read_file(filename:join(ExtractDir, AFile)),
    ?line Hello = binary_to_list(Bin),

    ok.

make_dirs([Dir|Rest], []) ->
    ?line ok = file:make_dir(Dir),
    ?line make_dirs(Rest, Dir);
make_dirs([Dir|Rest], Parent) ->
    ?line Name = filename:join(Parent, Dir),
    ?line ok = file:make_dir(Name),
    ?line make_dirs(Rest, Name);
make_dirs([], Dir) ->
    Dir.

bad_tar(doc) ->
    ["Try erl_tar:table/2 and erl_tar:extract/2 on some corrupted tar files."];
bad_tar(Config) when is_list(Config) ->
    ?line try_bad("bad_checksum", bad_header, Config),
    ?line try_bad("bad_octal",    bad_header, Config),
    ?line try_bad("bad_too_short",    eof, Config),
    ?line try_bad("bad_even_shorter", eof, Config),
    ok.

try_bad(Name0, Reason, Config) ->
    %% Intentionally no ?line macros here.

    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Name = Name0 ++ ".tar",
    io:format("~nTrying ~s", [Name]),
    Full = filename:join(DataDir, Name),
    Opts = [verbose, {cwd, PrivDir}],
    Expected = {error, Reason},
    case {erl_tar:table(Full, Opts), erl_tar:extract(Full, Opts)} of
	{Expected, Expected} ->
	    io:format("Result: ~p", [Expected]),
	    case catch erl_tar:format_error(Reason) of
		{'EXIT', CrashReason} ->
		    test_server:fail({format_error, crashed, CrashReason});
		String when is_list(String) ->
		    io:format("format_error(~p) -> ~s", [Reason, String]);
		Other ->
		    test_server:fail({format_error, returned, Other})
	    end;
	{Other1, Other2} ->
	    io:format("table/2 returned ~p", [Other1]),
	    io:format("extract/2 returned ~p", [Other2]),
	    test_server:fail({bad_return_value, Other1, Other2})
    end.

errors(doc) ->
    ["Tests that some common errors return correct error codes ",
     "and that format_error/1 handles them correctly."];
errors(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),

    %% Give the tar file the same name as a directory.
    ?line BadTar = filename:join(PrivDir, "bad_tarfile.tar"),
    ?line ok = file:make_dir(BadTar),
    ?line try_error(erl_tar, create, [BadTar, []], {BadTar, eisdir}),

    %% Try including non-existent files in the tar file.
    ?line NonExistent = "non_existent_file",
    ?line GoodTar = filename:join(PrivDir, "a_good_tarfile.tar"),
    ?line try_error(erl_tar, create, [GoodTar, [NonExistent]],
		    {NonExistent, enoent}),

    %% Clean up.
    ?line delete_files([GoodTar,BadTar]),
    
    ok.

try_error(M, F, A, Error) ->
    io:format("Trying ~p:~p(~p)", [M, F, A]),
    case catch apply(M, F, A) of
	{'EXIT', Reason} ->
	    exit(Reason);
	ok ->
	    test_server:fail(unexpected_success);
	{error, Error} ->
	    case catch erl_tar:format_error(Error) of
		{'EXIT', FReason} ->
		    test_server:fail({format_error, crashed, FReason});
		String when is_list(String) ->
		    io:format("format_error(~p) -> ~s", [Error, String]);
		Other ->
		    test_server:fail({format_error, returned, Other})
	    end;
	Other ->
	    test_server:fail({expected, {error, Error}, actual, Other})
    end.

%% remove_prefix(Prefix, List) -> ListWithoutPrefix.

remove_prefix([C|Rest1], [C|Rest2]) ->
    remove_prefix(Rest1, Rest2);
remove_prefix(_, Result) ->
    Result.

extract_from_binary(doc) ->
    "Test extracting a tar archive from a binary.";
extract_from_binary(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Long = filename:join(DataDir, "no_fancy_stuff.tar"),
    ?line ExtractDir = filename:join(PrivDir, "extract_from_binary"),
    ?line ok = file:make_dir(ExtractDir),
    
    %% Read a tar file into a binary and extract from the binary.
    ?line {ok, Bin} = file:read_file(Long),
    ?line ok = erl_tar:extract({binary, Bin}, [{cwd,ExtractDir}]),

    %% Verify.
    Dir = filename:join(ExtractDir, "no_fancy_stuff"),
    ?line true = filelib:is_dir(Dir),
    ?line true = filelib:is_file(filename:join(Dir, "a_dir_list")),
    ?line true = filelib:is_file(filename:join(Dir, "EPLICENCE")),

    %% Clean up.
    ?line delete_files([ExtractDir]),

    ok.

extract_from_binary_compressed(Config) when is_list(Config) ->
    %% Test extracting a compressed tar archive from a binary.
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Name = filename:join(DataDir, "cooked_tar_problem.tar.gz"),
    ?line ExtractDir = filename:join(PrivDir, "extract_from_binary_compressed"),
    ?line ok = file:make_dir(ExtractDir),
    ?line {ok,Bin} = file:read_file(Name),

    %% Try taking contents.
    ?line {ok,Files} = erl_tar:table({binary,Bin}, [compressed]),
    ?line io:format("~p\n", [Files]),
    ?line 19 = length(Files),
    
    %% Trying extracting from a binary.
    ?line ok = erl_tar:extract({binary,Bin}, [compressed,{cwd,ExtractDir}]),
    ?line {ok,List} = file:list_dir(filename:join(ExtractDir, "ddll_SUITE_data")),
    ?line io:format("~p\n", [List]),
    ?line 19 = length(List),

    %% Clean up while at the same time testing that all file
    %% were extracted as expected.
    lists:foreach(fun(N) ->
			  File = filename:join(ExtractDir, N),
			  io:format("Deleting: ~p\n", [File]),
			  ?line ok = file:delete(File)
		  end, Files),

    %% Clean up the rest.
    ?line delete_files([ExtractDir]),

    ok.

extract_from_open_file(doc) ->
    "Test extracting a tar archive from an open file.";
extract_from_open_file(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Long = filename:join(DataDir, "no_fancy_stuff.tar"),
    ?line ExtractDir = filename:join(PrivDir, "extract_from_open_file"),
    ?line ok = file:make_dir(ExtractDir),

    ?line {ok, File} = file:open(Long, [read]),
    ?line ok = erl_tar:extract({file, File}, [{cwd,ExtractDir}]),

    %% Verify.
    Dir = filename:join(ExtractDir, "no_fancy_stuff"),
    ?line true = filelib:is_dir(Dir),
    ?line true = filelib:is_file(filename:join(Dir, "a_dir_list")),
    ?line true = filelib:is_file(filename:join(Dir, "EPLICENCE")),

    %% Close open file.
    ?line ok = file:close(File),

    %% Clean up.
    ?line delete_files([ExtractDir]),

    ok.

symlinks(doc) ->
    "Test that archives containing symlinks can be created and extracted.";
symlinks(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Dir = filename:join(PrivDir, "symlinks"),
    ?line ok = file:make_dir(Dir),
    ?line ABadSymlink = filename:join(Dir, "bad_symlink"),
    ?line PointsTo = "/a/definitely/non_existing/path",
    ?line Res = case make_symlink("/a/definitely/non_existing/path", ABadSymlink) of
		    {error, enotsup} ->
			{skip, "Symbolic links not supported on this platform"};
		    ok ->
			symlinks(Dir, "bad_symlink", PointsTo),
			long_symlink(Dir)
		end,

    %% Clean up.
    ?line delete_files([Dir]),
    Res.

make_symlink(Path, Link) ->
    case os:type() of
	{win32,_} ->
	    %% Symlinks on Windows have two problems:
	    %%   1) file:read_link_info/1 cannot read out the target
	    %%      of the symlink if the target does not exist.
	    %%      That is possible (but not easy) to fix in the
	    %%      efile driver.
	    %%
	    %%   2) Symlinks to files and directories are different
	    %%      creatures. If the target is not existing, the
	    %%      symlink will be created to be of the file-pointing
	    %%      type. That can be partially worked around in erl_tar
	    %%      by creating all symlinks when the end of the tar
	    %%      file has been reached.
	    %%
	    %% But for now, pretend that there are no symlinks on
	    %% Windows.
	    {error, enotsup};
	_ ->
	    file:make_symlink(Path, Link)
    end.
	  
symlinks(Dir, BadSymlink, PointsTo) ->
    ?line Tar = filename:join(Dir, "symlink.tar"),
    ?line DerefTar = filename:join(Dir, "dereference.tar"),

    %% Create the archive.

    ?line ok = file:set_cwd(Dir),
    ?line GoodSymlink = "good_symlink",
    ?line AFile = "a_good_file",
    ?line ALine = "A line of text for a file.",
    ?line ok = file:write_file(AFile, ALine),
    ?line ok = file:make_symlink(AFile, GoodSymlink),
    ?line ok = erl_tar:create(Tar, [BadSymlink, GoodSymlink, AFile], [verbose]),

    %% List contents of tar file.

    ?line ok = erl_tar:tt(Tar),

    %% Also create another archive with the dereference flag.

    ?line ok = erl_tar:create(DerefTar, [AFile, GoodSymlink], [dereference, verbose]),

    %% Extract files to a new directory.

    ?line NewDir = filename:join(Dir, "extracted"),
    ?line ok = file:make_dir(NewDir),
    ?line ok = erl_tar:extract(Tar, [{cwd, NewDir}, verbose]),

    %% Verify that the files are there.

    ?line ok = file:set_cwd(NewDir),
    ?line {ok, #file_info{type=symlink}} = file:read_link_info(BadSymlink),
    ?line {ok, PointsTo} = file:read_link(BadSymlink),
    ?line {ok, #file_info{type=symlink}} = file:read_link_info(GoodSymlink),
    ?line {ok, AFile} = file:read_link(GoodSymlink),
    ?line Expected = list_to_binary(ALine),
    ?line {ok, Expected} = file:read_file(GoodSymlink),

    %% Extract the "dereferenced archive"  to a new directory.

    ?line NewDirDeref = filename:join(Dir, "extracted_deref"),
    ?line ok = file:make_dir(NewDirDeref),
    ?line ok = erl_tar:extract(DerefTar, [{cwd, NewDirDeref}, verbose]),

    %% Verify that the files are there.

    ?line ok = file:set_cwd(NewDirDeref),
    ?line {ok, #file_info{type=regular}} = file:read_link_info(GoodSymlink),
    ?line {ok, #file_info{type=regular}} = file:read_link_info(AFile),
    ?line {ok, Expected} = file:read_file(GoodSymlink),
    ?line {ok, Expected} = file:read_file(AFile),

    ok.

long_symlink(Dir) ->
    ?line Tar = filename:join(Dir, "long_symlink.tar"),
    ?line ok = file:set_cwd(Dir),

    ?line AFile = "long_symlink",
    ?line FarTooLong = "/tmp/aarrghh/this/path/is/far/longer/than/one/hundred/characters/which/is/the/maximum/number/of/characters/allowed",
    ?line ok = file:make_symlink(FarTooLong, AFile),
    ?line {error,Error} = erl_tar:create(Tar, [AFile], [verbose]),
    ?line io:format("Error: ~s\n", [erl_tar:format_error(Error)]),
    ?line {FarTooLong,symbolic_link_too_long} = Error,
    ok.

open_add_close(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line ok = file:set_cwd(PrivDir),
    ?line Dir = filename:join(PrivDir, "open_add_close"),
    ?line ok = file:make_dir(Dir),

    ?line [{FileOne,_,_},{FileTwo,_,_},{FileThree,_,_}] = oac_files(),
    ?line ADir = "empty_dir",
    ?line AnotherDir = "another_dir",
    ?line SomeContent = filename:join(AnotherDir, "some_content"),
    ?line ok = file:make_dir(ADir),
    ?line ok = file:make_dir(AnotherDir),
    ?line ok = file:make_dir(SomeContent),

    ?line TarOne = filename:join(Dir, "archive1.tar"),
    ?line {ok,AD} = erl_tar:open(TarOne, [write]),
    ?line ok = erl_tar:add(AD, FileOne, []),
    ?line ok = erl_tar:add(AD, FileTwo, "second file", []),
    ?line ok = erl_tar:add(AD, FileThree, [verbose]),
    ?line ok = erl_tar:add(AD, FileThree, "chunked", [{chunks,11411},verbose]),
    ?line ok = erl_tar:add(AD, ADir, [verbose]),
    ?line ok = erl_tar:add(AD, AnotherDir, [verbose]),
    ?line ok = erl_tar:close(AD),

    ?line ok = erl_tar:t(TarOne),
    ?line ok = erl_tar:tt(TarOne),

    ?line {ok,[FileOne,"second file",FileThree,"chunked",ADir,SomeContent]} = erl_tar:table(TarOne),

    ?line delete_files(["oac_file","oac_small","oac_big",Dir,AnotherDir,ADir]),

    ok.

oac_files() ->
    Files = [{"oac_file", 1459, $x},
	     {"oac_small", 99, $w},
	     {"oac_big", 33896, $A}],
    create_files(Files),
    Files.

cooked_compressed(Config) when is_list(Config) ->
    %% Test that a compressed archive can be read in cooked mode.
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Name = filename:join(DataDir, "cooked_tar_problem.tar.gz"),

    %% Try table/2 and extract/2.
    ?line {ok,List} = erl_tar:table(Name, [cooked,compressed]),
    ?line io:format("~p\n", [List]),
    ?line 19 = length(List),
    ?line ok = erl_tar:extract(Name, [cooked,compressed,{cwd,PrivDir}]),

    %% Clean up while at the same time testing that all file
    %% were extracted as expected.
    lists:foreach(fun(N) ->
			  File = filename:join(PrivDir, N),
			  io:format("Deleting: ~p\n", [File]),
			  ?line ok = file:delete(File)
		  end, List),

    %% Clean up.
    ?line delete_files([filename:join(PrivDir, "ddll_SUITE_data")]),
    ok.

memory(doc) ->
    ["Test that an archive can be created directly from binaries and "
     "that an archive can be extracted into binaries."];
memory(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),

    ?line FileBins = [{"bar/fum", <<"BARFUM">>},{"foo", <<"FOO">>}],
    ?line Name1 = filename:join(DataDir, "memory.tar"),
    ?line ok = erl_tar:create(Name1, FileBins, [write,verbose]),
    ?line {ok,Extracted1} = erl_tar:extract(Name1, [memory,verbose]),
    ?line FileBins1 = lists:sort(Extracted1),

    ?line io:format("FileBins: ~p\n", [FileBins]),
    ?line io:format("FileBins1: ~p\n", [FileBins1]),
    ?line FileBins = FileBins1,

    ?line Name2 = filename:join(DataDir, "memory2.tar"),
    ?line {ok,Fd} = erl_tar:open(Name2, [write]),
    ?line [ok,ok] = [erl_tar:add(Fd, B, N, [write,verbose]) || {N,B} <- FileBins],
    ?line ok = erl_tar:close(Fd),
    ?line {ok,Extracted2} = erl_tar:extract(Name2, [memory,verbose]),
    ?line FileBins2 = lists:sort(Extracted2),
    ?line io:format("FileBins2: ~p\n", [FileBins2]),
    ?line FileBins = FileBins2,

    %% Clean up.
    ?line ok = delete_files([Name1,Name2]),
    ok.

%% Test filenames with characters outside the US ASCII range.
unicode(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    do_unicode(PrivDir),
    case has_transparent_naming() of
	true ->
	    Pa = filename:dirname(code:which(?MODULE)),
	    Node = start_node(unicode, "+fnl -pa "++Pa),
	    ok = rpc:call(Node, erlang, apply,
			  [fun() -> do_unicode(PrivDir) end,[]]),
	    true = test_server:stop_node(Node),
	    ok;
	false ->
	    ok
    end.

has_transparent_naming() ->
    case os:type() of
	{unix,darwin} -> false;
	{unix,_} -> true;
	_ -> false
    end.

do_unicode(PrivDir) ->
    ok = file:set_cwd(PrivDir),
    ok = file:make_dir("unicöde"),

    Names = unicode_create_files(),
    Tar = "unicöde.tar",
    ok = erl_tar:create(Tar, ["unicöde"], []),
    {ok,Names} = erl_tar:table(Tar, []),
    _ = [ok = file:delete(Name) || Name <- Names],
    ok = erl_tar:extract(Tar),
    _ = [{ok,_} = file:read_file(Name) || Name <- Names],
    _ = [ok = file:delete(Name) || Name <- Names],
    ok = file:del_dir("unicöde"),
    ok.

unicode_create_files() ->
    FileA = "unicöde/smörgåsbord",
    ok = file:write_file(FileA, "yum!\n"),
    [FileA|case file:native_name_encoding() of
	       utf8 ->
		   FileB = "unicöde/Хороший файл!",
		   ok = file:write_file(FileB, "But almost empty.\n"),
		   [FileB];
	       latin1 ->
		   []
	   end].

%% Delete the given list of files.
delete_files([]) -> ok;
delete_files([Item|Rest]) ->
    case file:delete(Item) of
	ok ->
	    delete_files(Rest);
	{error,eperm} ->
	    file:change_mode(Item, 8#777),
	    delete_files(filelib:wildcard(filename:join(Item, "*"))),
	    file:del_dir(Item),
	    ok;
	{error,eacces} ->
	    %% We'll see about that!
	    file:change_mode(Item, 8#777),
	    case file:delete(Item) of
		ok -> ok;
		{error,_} ->
		    erlang:yield(),
		    file:change_mode(Item, 8#777),
		    file:delete(Item),
		    ok
	    end;
	{error,_} -> ok
    end,
    delete_files(Rest).

%% Move to a temporary directory with as short name as possible and
%% execute Fun. Remove the directory and any files in it afterwards.
%% This is necessary because pathnames on Windows may be limited to
%% 260 characters.
run_in_short_tempdir(Config, Fun) ->
    {ok,Cwd} = file:get_cwd(),
    PrivDir0 = ?config(priv_dir, Config),

    %% Normalize name to make sure that there is no slash at the end.
    PrivDir = filename:absname(PrivDir0),

    %% We need a base directory with a much shorter pathname than
    %% priv_dir. We KNOW that priv_dir is located four levels below
    %% the directory that common_test puts the ct_run.* directories
    %% in. That fact is not documented, but a usually reliable source
    %% assured me that the directory structure is unlikely to change
    %% in future versions of common_test because of backwards
    %% compatibility (tools developed by users of common_test depend
    %% on the current directory layout).
    Base = lists:foldl(fun(_, D) ->
			       filename:dirname(D)
		       end, PrivDir, [1,2,3,4]),

    Dir = make_temp_dir(Base, 0),
    ok = file:set_cwd(Dir),
    io:format("Running test in ~s\n", [Dir]),
    try
	Fun()
    after
	file:set_cwd(Cwd),
	delete_files([Dir])
    end.

make_temp_dir(Base, I) ->
    Name = filename:join(Base, integer_to_list(I, 36)),
    case file:make_dir(Name) of
	ok -> Name;
	{error,eexist} -> make_temp_dir(Base, I+1)
    end.

start_node(Name, Args) ->
    [_,Host] = string:tokens(atom_to_list(node()), "@"),
    ct:log("Trying to start ~w@~s~n", [Name,Host]),
    case test_server:start_node(Name, peer, [{args,Args}]) of
	{error,Reason} ->
	    test_server:fail(Reason);
	{ok,Node} ->
	    ct:log("Node ~p started~n", [Node]),
	    Node
    end.
