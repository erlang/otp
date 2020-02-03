%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(tar_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2,
         init_per_testcase/2,
         borderline/1, atomic/1, long_names/1,
	 create_long_names/1, bad_tar/1, errors/1, extract_from_binary/1,
	 extract_from_binary_compressed/1, extract_filtered/1,
	 extract_from_open_file/1, symlinks/1, open_add_close/1, cooked_compressed/1,
	 memory/1,unicode/1,read_other_implementations/1,
         sparse/1, init/1, leading_slash/1, dotdot/1,
         roundtrip_metadata/1, apply_file_info_opts/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [borderline, atomic, long_names, create_long_names,
     bad_tar, errors, extract_from_binary,
     extract_from_binary_compressed, extract_from_open_file,
     extract_filtered,
     symlinks, open_add_close, cooked_compressed, memory, unicode,
     read_other_implementations,
     sparse,init,leading_slash,dotdot,roundtrip_metadata,
     apply_file_info_opts].

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

init_per_testcase(_Case, Config) ->
    Ports = ordsets:from_list(erlang:ports()),
    [{ports,Ports}|Config].

%% Test creating, listing and extracting one file from an archive,
%% multiple times with different file sizes.  Also check that the file
%% attributes of the extracted file has survived.
borderline(Config) when is_list(Config) ->

    %% Note: We cannot use absolute paths, because the pathnames will be
    %% too long for the limit allowed in tar files (100 characters).
    %% Therefore, strip off the current working directory from the front
    %% of the private directory path.

    {ok, Cwd} = file:get_cwd(),
    RootDir = proplists:get_value(priv_dir, Config),
    TempDir = remove_prefix(Cwd++"/", filename:join(RootDir, "borderline")),
    ok = file:make_dir(TempDir),

    Record = 512,
    Block = 20 * Record,

    lists:foreach(fun(Size) -> borderline_test(Size, TempDir) end,
		  [0, 1, 10, 13, 127, 333, Record-1, Record, Record+1,
		   Block-2*Record-1, Block-2*Record, Block-2*Record+1,
		   Block-Record-1, Block-Record, Block-Record+1,
		   Block-1, Block, Block+1,
		   Block+Record-1, Block+Record, Block+Record+1]),

    %% Clean up.
    delete_files([TempDir]),

    verify_ports(Config).

borderline_test(Size, TempDir) ->
    io:format("Testing size ~p", [Size]),
    borderline_test(Size, TempDir, true),
    borderline_test(Size, TempDir, false),
    ok.

borderline_test(Size, TempDir, IsUstar) ->
    Prefix = case IsUstar of
                 true ->
                     "file_";
                 false ->
                     lists:duplicate(100, $f) ++ "ile_"
             end,
    SizeList = integer_to_list(Size),
    Archive = filename:join(TempDir, "ar_"++ SizeList ++".tar"),
    Name = filename:join(TempDir, Prefix++SizeList),

    %% Create a file and archive it.
    X0 = erlang:monotonic_time(),
    ok = file:write_file(Name, random_byte_list(X0, Size)),
    ok = erl_tar:create(Archive, [Name]),
    ok = file:delete(Name),

    %% Verify listing and extracting.
    IsUstar = is_ustar(Archive),
    {ok, [Name]} = erl_tar:table(Archive),
    ok = erl_tar:extract(Archive, [verbose]),

    %% Verify contents of extracted file.
    {ok, Bin} = file:read_file(Name),
    true = match_byte_list(X0, binary_to_list(Bin)),

    %% Verify that Unix tar can read it.
    case IsUstar of
        true ->
            tar_tf(Archive, Name);
        false ->
            ok
    end,

    ok.

tar_tf(Archive, Name) ->
    case os:type() of
	{unix, _} ->
	    tar_tf1(Archive, Name);
	_ ->
	    ok
    end.

tar_tf1(Archive, Name) ->
    Expect = Name ++ "\n",
    cmd_expect("tar tf " ++ Archive, Expect).

%% We can't use os:cmd/1, because Unix 'tar tf Name' on Solaris never
%% terminates when given an archive of a size it doesn't like.

cmd_expect(Cmd, Expect) ->
    Port = open_port({spawn, make_cmd(Cmd)}, [stream, in, eof]),
    get_data(Port, Expect).

get_data(Port, Expect) ->
    receive
	{Port, {data, Bytes}} ->
	    get_data(Port, match_output(Bytes, Expect, Port));
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
	    match_output(eof, Expect, Port)
    end.

match_output([C|Output], [C|Expect], Port) ->
    match_output(Output, Expect, Port);
match_output([_|_], [_|_], Port) ->
    kill_port_and_fail(Port, badmatch);
match_output([X|Output], [], Port) ->
    kill_port_and_fail(Port, {too_much_data, [X|Output]});
match_output([], Expect, _Port) ->
    Expect;
match_output(eof, [], _Port) ->
    [];
match_output(eof, _Expect, Port) ->
    kill_port_and_fail(Port, unexpected_end_of_input).

kill_port_and_fail(Port, Reason) ->
    unlink(Port),
    exit(Port, die),
    ct:fail(Reason).

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

%% Test the 'atomic' operations: create/extract/table, on compressed
%% and uncompressed archives.
%% Also test the 'cooked' option.
atomic(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    DataFiles = data_files(),
    Names = [Name || {Name,_,_} <- DataFiles],
    io:format("Names: ~p", [Names]),

    %% Create an uncompressed archive.  The compressed flag should still be
    %% allowed when listing contents or extracting.

    Tar1 = "uncompressed.tar",
    erl_tar:create(Tar1, Names, []),
    {ok, Names} = erl_tar:table(Tar1, []),
    {ok, Names} = erl_tar:table(Tar1, [compressed]),
    {ok, Names} = erl_tar:table(Tar1, [cooked]),
    {ok, Names} = erl_tar:table(Tar1, [compressed,cooked]),

    %% Create a compressed archive.

    Tar2 = "compressed.tar",
    erl_tar:create(Tar2, Names, [compressed]),
    {ok, Names} = erl_tar:table(Tar2, [compressed]),
    {error, Reason} = erl_tar:table(Tar2, []),
    {ok, Names} = erl_tar:table(Tar2, [compressed,cooked]),
    {error, Reason} = erl_tar:table(Tar2, [cooked]),
    ok = io:format("No compressed option: ~p, ~s",
		   [Reason, erl_tar:format_error(Reason)]),

    %% Same test again, but this time created with 'cooked'

    Tar3 = "uncompressed_cooked.tar",
    erl_tar:create(Tar3, Names, [cooked]),
    {ok, Names} = erl_tar:table(Tar3, []),
    {ok, Names} = erl_tar:table(Tar3, [compressed]),
    {ok, Names} = erl_tar:table(Tar3, [cooked]),
    {ok, Names} = erl_tar:table(Tar3, [compressed,cooked]),

    Tar4 = "compressed_cooked.tar",
    erl_tar:create(Tar4, Names, [compressed,cooked]),
    {ok, Names} = erl_tar:table(Tar4, [compressed]),
    {error, Reason} = erl_tar:table(Tar4, []),
    {ok, Names} = erl_tar:table(Tar4, [compressed,cooked]),
    {error, Reason} = erl_tar:table(Tar4, [cooked]),
    ok = io:format("No compressed option: ~p, ~s",
		   [Reason, erl_tar:format_error(Reason)]),

    %% Clean up.
    delete_files([Tar1,Tar2,Tar3,Tar4|Names]),

    verify_ports(Config).

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

%% Test to extract an Unix tar file containing filenames longer than
%% 100 characters and empty directories.
long_names(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Long = filename:join(DataDir, "long_names.tar"),
    run_in_short_tempdir(Config,
			 fun() -> do_long_names(Long) end),
    verify_ports(Config).


do_long_names(Long) ->
    %% Try table/2 and extract/2.
    case erl_tar:table(Long, [verbose]) of
	{ok,List} when is_list(List) ->
	    io:format("~p\n", [List])
    end,

    {ok,Cwd} = file:get_cwd(),
    ok = erl_tar:extract(Long),
    Base = filename:join([Cwd, "original_software", "written_by",
			  "a_bunch_of_hackers",
			  "spending_all_their_nights",
			  "still", "not_long_enough",
			  "but_soon_it_will_be"]),

    %% Verify that the empty directory was created.
    EmptyDir = filename:join(Base, "empty_directory"),
    {ok, #file_info{type=directory}} = file:read_file_info(EmptyDir),

    %% Verify that the files were created.
    {ok,First} = file:read_file(filename:join(Base, "first_file")),
    {ok,Second} = file:read_file(filename:join(Base, "second_file")),
    "Here"++_ = binary_to_list(First),
    "And"++_ = binary_to_list(Second),

    ok.

%% Creates a tar file from a deep directory structure (filenames are
%% longer than 100 characters).
create_long_names(Config) when is_list(Config) ->
    run_in_short_tempdir(Config, fun create_long_names/0),
    verify_ports(Config).

create_long_names() ->
    {ok,Dir} = file:get_cwd(),
    Dirs = ["aslfjkshjkhliuf",
	    "asdhjfehnbfsky",
	    "sahajfskdfhsz",
	    "asldfkdlfy4y8rchg",
	    "f7nafhjgffagkhsfkhsjk",
	    "dfjasldkfjsdkfjashbv"],

    DeepDir = make_dirs(Dirs, []),
    AFile = filename:join(DeepDir, "a_file"),
    Hello = "hello, world\n",
    ok = file:write_file(AFile, Hello),
    TarName = filename:join(Dir,  "my_tar_with_long_names.tar"),
    ok = erl_tar:create(TarName, [AFile]),

    %% Print contents.
    ok = erl_tar:tt(TarName),

    %% Extract and verify.
    true = is_ustar(TarName),
    ExtractDir = "extract_dir",
    ok = file:make_dir(ExtractDir),
    ok = erl_tar:extract(TarName, [{cwd,ExtractDir}]),
    {ok, Bin} = file:read_file(filename:join(ExtractDir, AFile)),
    Hello = binary_to_list(Bin),

    ok.

make_dirs([Dir|Rest], []) ->
    ok = file:make_dir(Dir),
    make_dirs(Rest, Dir);
make_dirs([Dir|Rest], Parent) ->
    Name = filename:join(Parent, Dir),
    ok = file:make_dir(Name),
    make_dirs(Rest, Name);
make_dirs([], Dir) ->
    Dir.

%% Try erl_tar:table/2 and erl_tar:extract/2 on some corrupted tar files.
bad_tar(Config) when is_list(Config) ->
    try_bad("bad_checksum", bad_header, Config),
    try_bad("bad_octal",    invalid_tar_checksum, Config),
    try_bad("bad_too_short",    eof, Config),
    try_bad("bad_even_shorter", eof, Config),
    verify_ports(Config).

try_bad(Name0, Reason, Config) ->
    %% Intentionally no macros here.

    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = Name0 ++ ".tar",
    io:format("~nTrying ~s", [Name]),
    Full = filename:join(DataDir, Name),
    Dest = filename:join(PrivDir, Name0),
    Opts = [verbose, {cwd, Dest}],
    Expected = {error, Reason},
    io:fwrite("Expected: ~p\n", [Expected]),
    case {erl_tar:table(Full, Opts), erl_tar:extract(Full, Opts)} of
	{Expected, Expected} ->
	    io:format("Result: ~p", [Expected]),
	    case catch erl_tar:format_error(Reason) of
		{'EXIT', CrashReason} ->
		    ct:fail({format_error, crashed, CrashReason});
		String when is_list(String) ->
		    io:format("format_error(~p) -> ~s", [Reason, String]);
		Other ->
		    ct:fail({format_error, returned, Other})
	    end;
	{Other1, Other2} ->
	    io:format("table/2 returned ~p", [Other1]),
	    io:format("extract/2 returned ~p", [Other2]),
	    ct:fail({bad_return_value, Other1, Other2})
    end.

%% Tests that some common errors return correct error codes
%% and that format_error/1 handles them correctly.
errors(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    %% Give the tar file the same name as a directory.
    BadTar = filename:join(PrivDir, "bad_tarfile.tar"),
    ok = file:make_dir(BadTar),
    try_error(erl_tar, create, [BadTar, []], {BadTar, eisdir}),

    %% Try including non-existent files in the tar file.
    NonExistent = "non_existent_file",
    GoodTar = filename:join(PrivDir, "a_good_tarfile.tar"),
    try_error(erl_tar, create, [GoodTar, [NonExistent]],
	      {NonExistent, enoent}),

    %% Clean up.
    delete_files([GoodTar,BadTar]),

    verify_ports(Config).

try_error(M, F, A, Error) ->
    io:format("Trying ~p:~p(~p)", [M, F, A]),
    case catch apply(M, F, A) of
	{'EXIT', Reason} ->
	    exit(Reason);
	ok ->
	    ct:fail(unexpected_success);
	{error, Error} ->
	    case catch erl_tar:format_error(Error) of
		{'EXIT', FReason} ->
		    ct:fail({format_error, crashed, FReason});
		String when is_list(String) ->
		    io:format("format_error(~p) -> ~s", [Error, String]);
		Other ->
		    ct:fail({format_error, returned, Other})
	    end;
	Other ->
	    ct:fail({expected, {error, Error}, actual, Other})
    end.

%% remove_prefix(Prefix, List) -> ListWithoutPrefix.

remove_prefix([C|Rest1], [C|Rest2]) ->
    remove_prefix(Rest1, Rest2);
remove_prefix(_, Result) ->
    Result.

%% Test extracting a tar archive from a binary.
extract_from_binary(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Long = filename:join(DataDir, "no_fancy_stuff.tar"),
    ExtractDir = filename:join(PrivDir, "extract_from_binary"),
    ok = file:make_dir(ExtractDir),

    %% Read a tar file into a binary and extract from the binary.
    {ok, Bin} = file:read_file(Long),
    ok = erl_tar:extract({binary, Bin}, [{cwd,ExtractDir}]),

    %% Verify.
    Dir = filename:join(ExtractDir, "no_fancy_stuff"),
    true = filelib:is_dir(Dir),
    true = filelib:is_file(filename:join(Dir, "a_dir_list")),
    true = filelib:is_file(filename:join(Dir, "EPLICENCE")),

    %% Clean up.
    delete_files([ExtractDir]),

    verify_ports(Config).

extract_from_binary_compressed(Config) when is_list(Config) ->
    %% Test extracting a compressed tar archive from a binary.
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(DataDir, "cooked_tar_problem.tar.gz"),
    ExtractDir = filename:join(PrivDir, "extract_from_binary_compressed"),
    ok = file:make_dir(ExtractDir),
    {ok,Bin} = file:read_file(Name),

    %% Try taking contents.
    {ok,Files} = erl_tar:table({binary,Bin}, [compressed]),
    io:format("~p\n", [Files]),
    19 = length(Files),

    %% Trying extracting from a binary.
    ok = erl_tar:extract({binary,Bin}, [compressed,{cwd,ExtractDir}]),
    {ok,List} = file:list_dir(filename:join(ExtractDir, "ddll_SUITE_data")),
    io:format("~p\n", [List]),
    19 = length(List),

    %% Clean up while at the same time testing that all file
    %% were extracted as expected.
    lists:foreach(fun(N) ->
			  File = filename:join(ExtractDir, N),
			  io:format("Deleting: ~p\n", [File]),
			  ok = file:delete(File)
		  end, Files),

    %% Clean up the rest.
    delete_files([ExtractDir]),

    verify_ports(Config).

%% Test extracting a tar archive from a binary.
extract_filtered(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Long = filename:join(DataDir, "no_fancy_stuff.tar"),
    ExtractDir = filename:join(PrivDir, "extract_from_binary"),
    ok = file:make_dir(ExtractDir),

    ok = erl_tar:extract(Long, [{cwd,ExtractDir},{files,["no_fancy_stuff/EPLICENCE"]}]),

    %% Verify.
    Dir = filename:join(ExtractDir, "no_fancy_stuff"),
    true = filelib:is_dir(Dir),
    false = filelib:is_file(filename:join(Dir, "a_dir_list")),
    true = filelib:is_file(filename:join(Dir, "EPLICENCE")),

    %% Clean up.
    delete_files([ExtractDir]),

    verify_ports(Config).

%% Test extracting a tar archive from an open file.
extract_from_open_file(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Long = filename:join(DataDir, "no_fancy_stuff.tar"),
    ExtractDir = filename:join(PrivDir, "extract_from_open_file"),
    ok = file:make_dir(ExtractDir),

    {ok, File} = file:open(Long, [read]),
    ok = erl_tar:extract({file, File}, [{cwd,ExtractDir}]),

    %% Verify.
    Dir = filename:join(ExtractDir, "no_fancy_stuff"),
    true = filelib:is_dir(Dir),
    true = filelib:is_file(filename:join(Dir, "a_dir_list")),
    true = filelib:is_file(filename:join(Dir, "EPLICENCE")),

    %% Close open file.
    ok = file:close(File),

    %% Clean up.
    delete_files([ExtractDir]),

    verify_ports(Config).

%% Test that archives containing symlinks can be created and extracted.
symlinks(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "symlinks"),
    VulnerableDir = filename:join(PrivDir, "vulnerable_symlinks"),
    ok = file:make_dir(Dir),
    ok = file:make_dir(VulnerableDir),
    ABadSymlink = filename:join(Dir, "bad_symlink"),
    PointsTo = "a/definitely/non_existing/path",
    Res = case make_symlink("a/definitely/non_existing/path", ABadSymlink) of
	      {error, enotsup} ->
		  {skip, "Symbolic links not supported on this platform"};
	      ok ->
		  symlinks(Dir, "bad_symlink", PointsTo),
		  long_symlink(Dir),
                  symlink_vulnerability(VulnerableDir)
	  end,

    %% Clean up.
    delete_files([Dir,VulnerableDir]),
    verify_ports(Config),
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
    Tar = filename:join(Dir, "symlink.tar"),
    DerefTar = filename:join(Dir, "dereference.tar"),

    %% Create the archive.

    ok = file:set_cwd(Dir),
    GoodSymlink = "good_symlink",
    AFile = "a_good_file",
    ALine = "A line of text for a file.",
    ok = file:write_file(AFile, ALine),
    ok = file:make_symlink(AFile, GoodSymlink),
    ok = erl_tar:create(Tar, [BadSymlink, GoodSymlink, AFile], [verbose]),
    true = is_ustar(Tar),

    %% List contents of tar file.

    ok = erl_tar:tt(Tar),

    %% Also create another archive with the dereference flag.

    ok = erl_tar:create(DerefTar, [AFile, GoodSymlink], [dereference, verbose]),
    true = is_ustar(DerefTar),

    %% Extract files to a new directory.

    NewDir = filename:join(Dir, "extracted"),
    ok = file:make_dir(NewDir),
    ok = erl_tar:extract(Tar, [{cwd, NewDir}, verbose]),

    %% Verify that the files are there.

    ok = file:set_cwd(NewDir),
    {ok, #file_info{type=symlink}} = file:read_link_info(BadSymlink),
    {ok, PointsTo} = file:read_link(BadSymlink),
    {ok, #file_info{type=symlink}} = file:read_link_info(GoodSymlink),
    {ok, AFile} = file:read_link(GoodSymlink),
    Expected = list_to_binary(ALine),
    {ok, Expected} = file:read_file(GoodSymlink),

    %% Extract the "dereferenced archive"  to a new directory.

    NewDirDeref = filename:join(Dir, "extracted_deref"),
    ok = file:make_dir(NewDirDeref),
    ok = erl_tar:extract(DerefTar, [{cwd, NewDirDeref}, verbose]),

    %% Verify that the files are there.

    ok = file:set_cwd(NewDirDeref),
    {ok, #file_info{type=regular}} = file:read_link_info(GoodSymlink),
    {ok, #file_info{type=regular}} = file:read_link_info(AFile),
    {ok, Expected} = file:read_file(GoodSymlink),
    {ok, Expected} = file:read_file(AFile),

    ok.

long_symlink(Dir) ->
    Tar = filename:join(Dir, "long_symlink.tar"),
    ok = file:set_cwd(Dir),

    AFile = "long_symlink",
    RequiresPAX = "tmp/aarrghh/this/path/is/far/longer/than/one/hundred/characters/which/is/the/maximum/number/of/characters/allowed",
    ok = file:make_symlink(RequiresPAX, AFile),
    ok = erl_tar:create(Tar, [AFile], [verbose]),
    false = is_ustar(Tar),
    NewDir = filename:join(Dir, "extracted"),
    _ = file:make_dir(NewDir),
    ok = erl_tar:extract(Tar, [{cwd, NewDir}, verbose]),
    ok = file:set_cwd(NewDir),
    {ok, #file_info{type=symlink}} = file:read_link_info(AFile),
    {ok, RequiresPAX} = file:read_link(AFile),
    ok.

symlink_vulnerability(Dir) ->
    ok = file:set_cwd(Dir),
    ok = file:make_dir("tar"),
    ok = file:set_cwd("tar"),
    ok = file:make_symlink("..", "link"),
    ok = file:write_file("../file", <<>>),
    ok = erl_tar:create("../my.tar", ["link","link/file"]),
    ok = erl_tar:tt("../my.tar"),

    ok = file:set_cwd(Dir),
    delete_files(["file","tar"]),
    ok = file:make_dir("tar"),
    ok = file:set_cwd("tar"),
    {error,{"..",unsafe_symlink}} = erl_tar:extract("../my.tar"),

    ok.

init(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(PrivDir),
    Dir = filename:join(PrivDir, "init"),
    ok = file:make_dir(Dir),

    [{FileOne,_,_}|_] = oac_files(),
    TarOne = filename:join(Dir, "archive1.tar"),
    {ok,Fd} = file:open(TarOne, [write]),

    %% If the arity of the fun is wrong, badarg should be returned
    {error, badarg} = erl_tar:init(Fd, write, fun file_op_bad/1),

    %% Otherwise we should be good to go
    {ok, Tar} = erl_tar:init(Fd, write, fun file_op/2),
    ok = erl_tar:add(Tar, FileOne, []),
    ok = erl_tar:close(Tar),
    {ok, [FileOne]} = erl_tar:table(TarOne),

    verify_ports(Config).

file_op_bad(_) ->
    throw({error, should_never_be_called}).

file_op(write, {Fd, Data}) ->
    file:write(Fd, Data);
file_op(position, {Fd, Pos}) ->
    file:position(Fd, Pos);
file_op(read2, {Fd, Size}) ->
    file:read(Fd, Size);
file_op(close, Fd) ->
    file:close(Fd).

open_add_close(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(PrivDir),
    Dir = filename:join(PrivDir, "open_add_close"),
    ok = file:make_dir(Dir),

    [{FileOne,_,_},{FileTwo,_,_},{FileThree,_,_}] = oac_files(),
    ADir = "empty_dir",
    AnotherDir = "another_dir",
    SomeContent = filename:join(AnotherDir, "some_content"),
    ok = file:make_dir(ADir),
    ok = file:make_dir(AnotherDir),
    ok = file:make_dir(SomeContent),

    TarOne = filename:join(Dir, "archive1.tar"),
    {ok,AD} = erl_tar:open(TarOne, [write]),
    ok = erl_tar:add(AD, FileOne, []),

    %% Add with {NameInArchive,Name}
    ok = erl_tar:add(AD, {"second file", FileTwo}, []),

    %% Add with {binary, Bin}
    {ok,FileThreeBin} = file:read_file(FileThree),
    ok = erl_tar:add(AD, {FileThree, FileThreeBin}, [verbose]),

    %% Add with Name
    ok = erl_tar:add(AD, FileThree, "chunked", [{chunks,11411},verbose]),
    ok = erl_tar:add(AD, ADir, [verbose]),
    ok = erl_tar:add(AD, AnotherDir, [verbose]),
    ok = erl_tar:close(AD),
    true = is_ustar(TarOne),

    ok = erl_tar:t(TarOne),
    ok = erl_tar:tt(TarOne),

    Expected = {ok,[FileOne,"second file",FileThree,"chunked",ADir,SomeContent]},
    Expected = erl_tar:table(TarOne),

    delete_files(["oac_file","oac_small","oac_big",Dir,AnotherDir,ADir]),

    verify_ports(Config).

oac_files() ->
    Files = [{"oac_file", 1459, $x},
	     {"oac_small", 99, $w},
	     {"oac_big", 33896, $A}],
    create_files(Files),
    Files.

cooked_compressed(Config) when is_list(Config) ->
    %% Test that a compressed archive can be read in cooked mode.
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(DataDir, "cooked_tar_problem.tar.gz"),

    %% Try table/2 and extract/2.
    {ok,List} = erl_tar:table(Name, [cooked,compressed]),
    io:format("~p\n", [List]),
    19 = length(List),
    ok = erl_tar:extract(Name, [cooked,compressed,{cwd,PrivDir}]),

    %% Clean up while at the same time testing that all file
    %% were extracted as expected.
    lists:foreach(fun(N) ->
			  File = filename:join(PrivDir, N),
			  io:format("Deleting: ~p\n", [File]),
			  ok = file:delete(File)
		  end, List),

    %% Clean up.
    delete_files([filename:join(PrivDir, "ddll_SUITE_data")]),

    verify_ports(Config).

%% Test that an archive can be created directly from binaries and
%% that an archive can be extracted into binaries.
memory(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    FileBins = [{"bar/fum", <<"BARFUM">>},{"foo", <<"FOO">>}],
    Name1 = filename:join(DataDir, "memory.tar"),
    ok = erl_tar:create(Name1, FileBins, [write,verbose]),
    {ok,Extracted1} = erl_tar:extract(Name1, [memory,verbose]),
    FileBins1 = lists:sort(Extracted1),

    io:format("FileBins: ~p\n", [FileBins]),
    io:format("FileBins1: ~p\n", [FileBins1]),
    FileBins = FileBins1,

    Name2 = filename:join(DataDir, "memory2.tar"),
    {ok,Fd} = erl_tar:open(Name2, [write]),
    [ok,ok] = [erl_tar:add(Fd, B, N, [write,verbose]) || {N,B} <- FileBins],
    ok = erl_tar:close(Fd),
    {ok,Extracted2} = erl_tar:extract(Name2, [memory,verbose]),
    FileBins2 = lists:sort(Extracted2),
    io:format("FileBins2: ~p\n", [FileBins2]),
    FileBins = FileBins2,

    %% Clean up.
    ok = delete_files([Name1,Name2]),

    verify_ports(Config).

read_other_implementations(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = ["v7.tar", "gnu.tar", "bsd.tar",
             "star.tar", "pax_mtime.tar"],
    do_read_other_implementations(Files, DataDir),
    verify_ports(Config).

do_read_other_implementations([], _DataDir) ->
    ok;
do_read_other_implementations([File|Rest], DataDir) ->
    io:format("~nTrying ~s", [File]),
    Full = filename:join(DataDir, File),
    {ok, _} = erl_tar:table(Full),
    {ok, _} = erl_tar:extract(Full, [memory]),
    do_read_other_implementations(Rest, DataDir).


%% Test handling of sparse files
sparse(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Sparse01Empty = "sparse01_empty.tar",
    Sparse01 = "sparse01.tar",
    Sparse10Empty = "sparse10_empty.tar",
    Sparse10 = "sparse10.tar",
    do_sparse([Sparse01Empty, Sparse01, Sparse10Empty, Sparse10], DataDir, PrivDir),
    verify_ports(Config).

do_sparse([], _DataDir, _PrivDir) ->
    ok;
do_sparse([Name|Rest], DataDir, PrivDir) ->
    io:format("~nTrying sparse file ~s", [Name]),
    Full = filename:join(DataDir, Name),
    {ok, [_]} = erl_tar:table(Full),
    {ok, _} = erl_tar:extract(Full, [memory]),
    do_sparse(Rest, DataDir, PrivDir).

%% Test filenames with characters outside the US ASCII range.
unicode(Config) when is_list(Config) ->
    run_unicode_node(Config, "+fnu"),
    case has_transparent_naming() of
	true ->
	    run_unicode_node(Config, "+fnl");
	false ->
	    ok
    end.

run_unicode_node(Config, Option) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Pa = filename:dirname(code:which(?MODULE)),
    Args = Option ++ " -pa "++Pa,
    io:format("~s\n", [Args]),
    Node = start_node(unicode, Args),
    ok = rpc:call(Node, erlang, apply,
		  [fun() -> do_unicode(PrivDir) end,[]]),
    true = test_server:stop_node(Node),
    ok.

has_transparent_naming() ->
    case os:type() of
	{unix,darwin} -> false;
	{unix,_} -> true;
	_ -> false
    end.

do_unicode(PrivDir) ->
    ok = file:set_cwd(PrivDir),
    ok = file:make_dir("unicöde"),

    Names = lists:sort(unicode_create_files()),
    Tar = "unicöde.tar",
    ok = erl_tar:create(Tar, ["unicöde"], []),

    %% Unicode filenames require PAX format.
    false = is_ustar(Tar),
    {ok,Names0} = erl_tar:table(Tar, []),
    Names = lists:sort(Names0),
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

leading_slash(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?FUNCTION_NAME),
    TarFile = filename:join(Dir, "leading_slash.tar"),
    ok = filelib:ensure_dir(TarFile),
    {ok,Fd} = erl_tar:open(TarFile, [write]),
    TarMemberName = "e/d/c/b/a_member",
    TarMemberNameAbs = "/" ++ TarMemberName,
    Contents = <<"contents\n">>,
    ok = erl_tar:add(Fd, Contents, TarMemberNameAbs, [verbose]),
    ok = erl_tar:close(Fd),

    ok = erl_tar:extract(TarFile, [{cwd,Dir}]),

    {ok,Contents} = file:read_file(filename:join(Dir, TarMemberName)),
    ok.

dotdot(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(Dir),
    Tar = filename:join(Dir, "dotdot.tar"),
    {ok,Fd} = erl_tar:open(Tar, [write]),
    BeamFile = code:which(?MODULE),
    ok = erl_tar:add(Fd, BeamFile, "a/./../../some_file", []),
    ok = erl_tar:close(Fd),

    {error,{_,unsafe_path=Error}} = erl_tar:extract(Tar, [{cwd,Dir}]),
    false = filelib:is_regular(filename:join(PrivDir, "some_file")),
    io:format("~s\n", [erl_tar:format_error(Error)]),

    ok.

roundtrip_metadata(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(Dir),

    do_roundtrip_metadata(Dir, "name-does-not-matter"),
    ok.

do_roundtrip_metadata(Dir, File) ->
    Tar = filename:join(Dir, atom_to_list(?FUNCTION_NAME)++".tar"),
    BeamFile = code:which(compile),
    {ok,Fd} = erl_tar:open(Tar, [write]),
    ok = erl_tar:add(Fd, BeamFile, File, []),
    ok = erl_tar:close(Fd),

    ok = erl_tar:extract(Tar, [{cwd,Dir}]),

    %% Make sure that size and modification times are the same
    %% on all platforms.
    {ok,OrigInfo} = file:read_file_info(BeamFile),
    ExtractedFile = filename:join(Dir, File),
    {ok,ExtractedInfo} = file:read_file_info(ExtractedFile),
    #file_info{size=Size,mtime=Mtime,type=regular} = OrigInfo,
    #file_info{size=Size,mtime=Mtime,type=regular} = ExtractedInfo,

    %% On Unix platforms more fields are expected to be the same.
    case os:type() of
        {unix,_} ->
            #file_info{access=Access,mode=Mode} = OrigInfo,
            #file_info{access=Access,mode=Mode} = ExtractedInfo,
            ok;
        _ ->
            ok
    end.

apply_file_info_opts(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),

    ok = file:make_dir("empty_directory"),
    ok = file:write_file("file", "contents"),

    Opts = [{atime, 0}, {mtime, 0}, {ctime, 0}, {uid, 0}, {gid, 0}],
    TarFile = "reproducible.tar",
    {ok, Tar} = erl_tar:open(TarFile, [write]),
    ok = erl_tar:add(Tar, "file", Opts),
    ok = erl_tar:add(Tar, "empty_directory", Opts),
    ok = erl_tar:add(Tar, <<"contents">>, "memory_file", Opts),
    erl_tar:close(Tar),

    ok = file:make_dir("extracted"),
    erl_tar:extract(TarFile, [{cwd, "extracted"}]),

    {ok, #file_info{mtime=0}} =
        file:read_file_info("extracted/empty_directory", [{time, posix}]),
    {ok, #file_info{mtime=0}} =
        file:read_file_info("extracted/file", [{time, posix}]),
    {ok, #file_info{mtime=0}} =
        file:read_file_info("extracted/memory_file", [{time, posix}]),

    ok.

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
    PrivDir0 = proplists:get_value(priv_dir, Config),

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
	    ct:fail(Reason);
	{ok,Node} ->
	    ct:log("Node ~p started~n", [Node]),
	    Node
    end.

%% Test that the given tar file is a plain USTAR archive,
%% without any PAX extensions.
is_ustar(File) ->
    {ok,Bin} = file:read_file(File),
    <<_:257/binary,"ustar",0,_/binary>> = Bin,
    <<_:156/binary,Type:8,_/binary>> = Bin,
    case Type of
        $x -> false;
        $g -> false;
        _ -> true
    end.


verify_ports(Config) ->
    PortsBefore = proplists:get_value(ports, Config),
    PortsAfter = ordsets:from_list(erlang:ports()),
    case ordsets:subtract(PortsAfter, PortsBefore) of
        [] ->
            ok;
        [_|_]=Rem ->
            error({leaked_ports,Rem})
    end.
