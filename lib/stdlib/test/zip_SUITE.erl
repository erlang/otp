%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2017. All Rights Reserved.
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
-module(zip_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, borderline/1, atomic/1,
         bad_zip/1, unzip_from_binary/1, unzip_to_binary/1,
         zip_to_binary/1,
         unzip_options/1, zip_options/1, list_dir_options/1, aliases/1,
         openzip_api/1, zip_api/1, open_leak/1, unzip_jar/1,
	 unzip_traversal_exploit/1,
         compress_control/1,
	 foldl/1,fd_leak/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/zip.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [borderline, atomic, bad_zip, unzip_from_binary,
     unzip_to_binary, zip_to_binary, unzip_options,
     zip_options, list_dir_options, aliases, openzip_api,
     zip_api, open_leak, unzip_jar, compress_control, foldl,
     unzip_traversal_exploit,fd_leak].

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


%% Test creating, listing and extracting one file from an archive
%% multiple times with different file sizes. Also check that the
%% modification date of the extracted file has survived.
borderline(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    TempDir = filename:join(RootDir, "borderline"),
    ok = file:make_dir(TempDir),

    Record = 512,
    Block = 20 * Record,

    lists:foreach(fun(Size) -> borderline_test(Size, TempDir) end,
                  [0, 1, 10, 13, 127, 333, Record-1, Record, Record+1,
                   Block-Record-1, Block-Record, Block-Record+1,
                   Block-1, Block, Block+1,
                   Block+Record-1, Block+Record, Block+Record+1]),

    %% Clean up.
    delete_files([TempDir]),
    ok.

borderline_test(Size, TempDir) ->
    Archive = filename:join(TempDir, "ar_"++integer_to_list(Size)++".zip"),
    Name = filename:join(TempDir, "file_"++integer_to_list(Size)),
    io:format("Testing size ~p", [Size]),

    %% Create a file and archive it.
    {_, _, X0} = erlang:timestamp(),
    file:write_file(Name, random_byte_list(X0, Size)),
    {ok, Archive} = zip:zip(Archive, [Name]),
    ok = file:delete(Name),

    %% Verify listing and extracting.
    {ok, [#zip_comment{comment = []},
          #zip_file{name = Name,
                    info = Info,
                    offset = 0,
                    comp_size = _}]} = zip:list_dir(Archive),
    Size = Info#file_info.size,
    {ok, [Name]} = zip:extract(Archive, [verbose]),

    %% Verify contents of extracted file.
    {ok, Bin} = file:read_file(Name),
    true = match_byte_list(X0, binary_to_list(Bin)),


    %% Verify that Unix zip can read it. (if we have a unix zip that is!)
    unzip_list(Archive, Name),

    ok.

unzip_list(Archive, Name) ->
    case unix_unzip_exists() of
	true ->
            unzip_list1(Archive, Name);
        _ ->
            ok
    end.

%% Used to do os:find_executable() to check if unzip exists, but on
%% some hosts that would give an unzip program which did not take the
%% "-Z" option.
%% Here we check that "unzip -Z" (which should display usage) and
%% check that it exists with status 0.
unix_unzip_exists() ->
    case os:type() of
	{unix,_} ->
	    Port = open_port({spawn,"unzip -Z > /dev/null"}, [exit_status]),
	    receive
		{Port,{exit_status,0}} ->
		    true;
		{Port,{exit_status,_Fail}} ->
		    false
	    end;
	_ ->
	    false
    end.

unzip_list1(Archive, Name) ->
    Expect = Name ++ "\n",
    cmd_expect("unzip -Z -1 " ++ Archive, Expect).

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
            after 1 ->                          % force context switch
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
match_output(eof, Expect, Port) ->
    kill_port_and_fail(Port, {unexpected_end_of_input, Expect}).

kill_port_and_fail(Port, Reason) ->
    unlink(Port),
    exit(Port, die),
    ct:fail(Reason).

make_cmd(Cmd) ->
    Cmd.
%%     case os:type() of
%%      {win32, _} -> lists:concat(["cmd /c",  Cmd]);
%%      {unix, _}  -> lists:concat(["sh -c '",  Cmd,  "'"])
%%     end.

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

%% Test the 'atomic' operations: zip/unzip/list_dir, on archives.
%% Also test the 'cooked' option.
atomic(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    DataFiles = data_files(),
    Names = [Name || {Name,_,_} <- DataFiles],
    io:format("Names: ~p", [Names]),

    %% Create a zip  archive.

    Zip2 = "zip.zip",
    {ok, Zip2} = zip:zip(Zip2, Names, []),
    Names = names_from_list_dir(zip:list_dir(Zip2)),

    %% Same test again, but this time created with 'cooked'

    Zip3 = "cooked.zip",
    {ok, Zip3} = zip:zip(Zip3, Names, [cooked]),
    Names = names_from_list_dir(zip:list_dir(Zip3)),
    Names = names_from_list_dir(zip:list_dir(Zip3, [cooked])),

    %% Clean up.
    delete_files([Zip2,Zip3|Names]),

    ok.

%% Test the openzip_open/2, openzip_get/1, openzip_get/2, openzip_close/1
%% and openzip_list_dir/1 functions.
openzip_api(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    DataFiles = data_files(),
    Names = [Name || {Name, _, _} <- DataFiles],
    io:format("Names: ~p", [Names]),

    %% Create a zip archive

    Zip = "zip.zip",
    {ok, Zip} = zip:zip(Zip, Names, []),

    %% Open archive
    {ok, OpenZip} = zip:openzip_open(Zip, [memory]),

    %% List dir
    Names = names_from_list_dir(zip:openzip_list_dir(OpenZip)),

    %% Get a file
    Name1 = hd(Names),
    {ok, Data1} = file:read_file(Name1),
    {ok, {Name1, Data1}} = zip:openzip_get(Name1, OpenZip),

    %% Get all files
    FilesDatas = lists:map(fun(Name) -> {ok, B} = file:read_file(Name),
                                        {Name, B} end, Names),
    {ok, FilesDatas} = zip:openzip_get(OpenZip),

    %% Close
    ok = zip:openzip_close(OpenZip),

    %% Clean up.
    delete_files([Names]),

    ok.

%% Test the zip_open/2, zip_get/1, zip_get/2, zip_close/1,
%% and zip_list_dir/1 functions.
zip_api(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    DataFiles = data_files(),
    Names = [Name || {Name, _, _} <- DataFiles],
    io:format("Names: ~p", [Names]),

    %% Create a zip archive
    Zip = "zip.zip",
    {ok, Zip} = zip:zip(Zip, Names, []),

    %% Open archive
    {ok, ZipSrv} = zip:zip_open(Zip, [memory]),

    %% List dir
    Names = names_from_list_dir(zip:zip_list_dir(ZipSrv)),

    %% Get a file
    Name1 = hd(Names),
    {ok, Data1} = file:read_file(Name1),
    {ok, {Name1, Data1}} = zip:zip_get(Name1, ZipSrv),

    %% Get all files
    FilesDatas = lists:map(fun(Name) -> {ok, B} = file:read_file(Name),
                                        {Name, B} end, Names),
    {ok, FilesDatas} = zip:zip_get(ZipSrv),

    %% Close
    ok = zip:zip_close(ZipSrv),

    %% Clean up.
    delete_files([Names]),

    ok.

%% Test that zip doesn't leak processes and ports where the
%% controlling process dies without closing an zip opened with
%% zip:zip_open/1.
open_leak(Config) when is_list(Config) ->
    %% Create a zip archive
    Zip = "zip.zip",
    {ok, Zip} = zip:zip(Zip, [], []),

    %% Open archive in a another process that dies immediately.
    ZipSrv = spawn_zip(Zip, [memory]),

    %% Expect the ZipSrv process to die soon after.
    true = spawned_zip_dead(ZipSrv),

    %% Clean up.
    delete_files([Zip]),

    ok.

spawn_zip(Zip, Options) ->
    Self = self(),
    spawn(fun() -> Self ! zip:zip_open(Zip, Options) end),
    receive
        {ok, ZipSrv} ->
            ZipSrv
    end.

spawned_zip_dead(ZipSrv) ->
    Ref = monitor(process, ZipSrv),
    receive
        {'DOWN', Ref, _, ZipSrv, _} ->
            true
    after 1000 ->
            false
    end.

%% Test options for unzip, only cwd and file_list currently.
unzip_options(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Long = filename:join(DataDir, "abc.zip"),

    %% create a temp directory
    Subdir = filename:join(PrivDir, "t"),
    ok = file:make_dir(Subdir),

    FList = ["quotes/rain.txt","wikipedia.txt"],

    %% Unzip a zip file in Subdir
    {ok, RetList} = zip:unzip(Long, [{cwd, Subdir},
				     {file_list, FList}]),

    %% Verify.
    true = (length(FList) =:= length(RetList)),
    lists:foreach(fun(F)-> {ok,B} = file:read_file(filename:join(DataDir, F)),
			   {ok,B} = file:read_file(filename:join(Subdir, F)) end,
		  FList),
    lists:foreach(fun(F)-> ok = file:delete(F) end,
		  RetList),

    %% Clean up and verify no more files.
    0 = delete_files([Subdir]),
    ok.

%% Test that unzip handles directory traversal exploit (OTP-13633)
unzip_traversal_exploit(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ZipName = filename:join(DataDir, "exploit.zip"),

    %% $ zipinfo -1 test/zip_SUITE_data/exploit.zip 
    %% clash.txt
    %% ../clash.txt
    %% ../above.txt
    %% subdir/../in_root_dir.txt

    %% create a temp directory
    SubDir = filename:join(PrivDir, "exploit_test"),
    ok = file:make_dir(SubDir),
    
    ClashFile = filename:join(SubDir,"clash.txt"),
    AboveFile = filename:join(SubDir,"above.txt"),
    RelativePathFile = filename:join(SubDir,"subdir/../in_root_dir.txt"),

    %% unzip in SubDir
    {ok, [ClashFile, ClashFile, AboveFile, RelativePathFile]} =
	zip:unzip(ZipName, [{cwd,SubDir}]),

    {ok,<<"This file will overwrite other file.\n">>} =
	file:read_file(ClashFile),
    {ok,_} = file:read_file(AboveFile),
    {ok,_} = file:read_file(RelativePathFile),

    %% clean up
    delete_files([SubDir]),
    
     %% create the temp directory again
    ok = file:make_dir(SubDir),

    %% unzip in SubDir
    {ok, [ClashFile, AboveFile, RelativePathFile]} =
	zip:unzip(ZipName, [{cwd,SubDir},keep_old_files]),

    {ok,<<"This is the original file.\n">>} =
	file:read_file(ClashFile),
   
    %% clean up
    delete_files([SubDir]),
    ok.

%% Test unzip a jar file (OTP-7382).
unzip_jar(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    JarFile = filename:join(DataDir, "test.jar"),

    %% create a temp directory
    Subdir = filename:join(PrivDir, "jartest"),
    ok = file:make_dir(Subdir),
    ok = file:set_cwd(Subdir),

    FList = ["META-INF/MANIFEST.MF","test.txt"],

    {ok, RetList} = zip:unzip(JarFile),

    %% Verify.
    lists:foreach(fun(F)-> {ok,B} = file:read_file(filename:join(DataDir, F)),
			   {ok,B} = file:read_file(filename:join(Subdir, F)) end,
		  FList),
    lists:foreach(fun(F)-> ok = file:delete(F) end,
		  RetList),

    %% Clean up and verify no more files.
    0 = delete_files([Subdir]),
    ok.

%% Test the options for unzip, only cwd currently.
zip_options(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(PrivDir),
    DataFiles = data_files(),
    Names = [Name || {Name, _, _} <- DataFiles],

    %% Make sure cwd is not where we get the files
    ok = file:set_cwd(proplists:get_value(data_dir, Config)),

    %% Create a zip archive
    {ok, {_,Zip}} =
        zip:zip("filename_not_used.zip", Names, [memory, {cwd, PrivDir}]),

    %% Open archive
    {ok, ZipSrv} = zip:zip_open(Zip, [memory]),

    %% List dir
    Names = names_from_list_dir(zip:zip_list_dir(ZipSrv)),

    %% Get a file
    Name1 = hd(Names),
    {ok, Data1} = file:read_file(filename:join(PrivDir, Name1)),
    {ok, {Name1, Data1}} = zip:zip_get(Name1, ZipSrv),

    %% Get all files
    FilesDatas = lists:map(fun(Name) -> {ok, B} = file:read_file(filename:join(PrivDir, Name)),
                                        {Name, B} end, Names),
    {ok, FilesDatas} = zip:zip_get(ZipSrv),

    %% Close
    ok = zip:zip_close(ZipSrv),

    %% Clean up.
    delete_files([Names]),

    ok.

%% Test the options for list_dir... one day.
list_dir_options(Config) when is_list(Config) ->
    ok.




%% convert zip_info as returned from list_dir to a list of names
names_from_list_dir({ok, Info}) ->
    names_from_list_dir(Info);
names_from_list_dir(Info) ->
    tl(lists:map(fun(#zip_file{name = Name}) -> Name;
                    (_) -> ok end, Info)).

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

create_files([{Name, dir, _First}|Rest]) ->
    ok = file:make_dir(Name),
    create_files(Rest);
create_files([{Name, Size, First}|Rest]) when is_integer(Size) ->
    ok = file:write_file(Name, char_seq(Size, First)),
    create_files(Rest);
create_files([]) ->
    ok.

%% make_dirs([Dir|Rest], []) ->
%%     ok = file:make_dir(Dir),
%%     make_dirs(Rest, Dir);
%% make_dirs([Dir|Rest], Parent) ->
%%     Name = filename:join(Parent, Dir),
%%     ok = file:make_dir(Name),
%%     make_dirs(Rest, Name);
%% make_dirs([], Dir) ->
%%     Dir.

%% Try zip:unzip/1 on some corrupted zip files.
bad_zip(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    try_bad("bad_crc",    {bad_crc, "abc.txt"}, Config),
    try_bad("bad_central_directory", bad_central_directory, Config),
    try_bad("bad_file_header",    bad_file_header, Config),
    try_bad("bad_eocd",    bad_eocd, Config),
    try_bad("enoent", enoent, Config),
    GetNotFound = fun(A) ->
                          {ok, O} = zip:openzip_open(A, []),
                          zip:openzip_get("not_here", O)
                  end,
    try_bad("abc", file_not_found, GetNotFound, Config),
    ok.

try_bad(N, R, Config) ->
    try_bad(N, R, fun(A) -> io:format("name : ~p\n", [A]),
                            zip:unzip(A, [verbose]) end, Config).

try_bad(Name0, Reason, What, Config) ->
    %% Intentionally no macros here.

    DataDir = proplists:get_value(data_dir, Config),
    Name = Name0 ++ ".zip",
    io:format("~nTrying ~s", [Name]),
    Full = filename:join(DataDir, Name),
    Expected = {error, Reason},
    case What(Full) of
        Expected ->
            io:format("Result: ~p\n", [Expected]);
        Other ->
            io:format("unzip/2 returned ~p (expected ~p)\n", [Other, Expected]),
            ct:fail({bad_return_value, Other})
    end.

%% Test extracting to binary with memory option.
unzip_to_binary(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    WorkDir = filename:join(PrivDir, "unzip_to_binary"),
    _ = file:make_dir(WorkDir),

    ok = file:set_cwd(WorkDir),
    Long = filename:join(DataDir, "abc.zip"),

    %% Unzip a zip file into a binary
    {ok, FBList} = zip:unzip(Long, [memory]),

    %% Verify.
    lists:foreach(fun({F,B}) -> {ok,B}=file:read_file(filename:join(DataDir, F))
                  end, FBList),

    %% Make sure no files created in cwd
    {ok,[]} = file:list_dir(WorkDir),

    ok.

%% Test compressing to binary with memory option.
zip_to_binary(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    WorkDir = filename:join(PrivDir, "zip_to_binary"),
    _ = file:make_dir(WorkDir),

    file:set_cwd(WorkDir),
    FileName = "abc.txt",
    ZipName = "t.zip",
    FilePath = filename:join(DataDir, FileName),
    {ok, _Size} = file:copy(FilePath, FileName),

    %% Zip to a binary archive
    {ok, {ZipName, ZipB}} = zip:zip(ZipName, [FileName], [memory]),

    %% Make sure no files created in cwd
    {ok,[FileName]} = file:list_dir(WorkDir),

    %% Zip to a file
    {ok, ZipName} = zip:zip(ZipName, [FileName]),

    %% Verify.
    {ok, ZipB} = file:read_file(ZipName),
    {ok, FData} = file:read_file(FileName),
    {ok, [{FileName, FData}]} = zip:unzip(ZipB, [memory]),

    %% Clean up.
    delete_files([FileName, ZipName]),

    ok.

%% Test using the aliases, extract/2, table/2 and create/3.
aliases(Config) when is_list(Config) ->
    {_, _, X0} = erlang:timestamp(),
    Size = 100,
    B = list_to_binary(random_byte_list(X0, Size)),
    %% create
    {ok, {"z.zip", ZArchive}} = zip:create("z.zip", [{"b", B}], [memory]),
    %% extract
    {ok, [{"b", B}]} = zip:extract(ZArchive, [memory]),
    %% table
    {ok, [#zip_comment{comment = _}, #zip_file{name = "b",
                                               info = FI,
                                               comp_size = _,
                                               offset = 0}]} =
        zip:table(ZArchive),
    Size = FI#file_info.size,

    ok.



%% Test extracting a zip archive from a binary.
unzip_from_binary(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ExtractDir = filename:join(PrivDir, "extract_from_binary"),
    ok = file:make_dir(ExtractDir),
    Archive = filename:join(ExtractDir, "abc.zip"),
    {ok, _Size} = file:copy(filename:join(DataDir, "abc.zip"), Archive),
    FileName = "abc.txt",
    Quote = "quotes/rain.txt",
    Wikipedia = "wikipedia.txt",
    EmptyFile = "emptyFile",
    file:set_cwd(ExtractDir),

    %% Read a zip file into a binary and extract from the binary.
    {ok, Bin} = file:read_file(Archive),
    {ok, [FileName,Quote,Wikipedia,EmptyFile]} = zip:unzip(Bin),

    %% Verify.
    DestFilename = filename:join(ExtractDir, "abc.txt"),
    {ok, Data} = file:read_file(filename:join(DataDir, FileName)),
    {ok, Data} = file:read_file(DestFilename),

    DestQuote = filename:join([ExtractDir, "quotes", "rain.txt"]),
    {ok, QuoteData} = file:read_file(filename:join(DataDir, Quote)),
    {ok, QuoteData} = file:read_file(DestQuote),

    %% Clean up.
    delete_files([DestFilename, DestQuote, Archive, ExtractDir]),
    ok.

%% oac_files() ->
%%     Files = [{"oac_file", 1459, $x},
%%           {"oac_small", 99, $w},
%%           {"oac_big", 33896, $A}],
%%     create_files(Files),
%%     Files.

%% Delete the given list of files and directories.
%% Return total number of deleted files (not directories)
delete_files(List) ->
    do_delete_files(List, 0).
do_delete_files([],Cnt) ->
    Cnt;
do_delete_files([Item|Rest], Cnt) ->
    case file:delete(Item) of
        ok ->
            DelCnt = 1;
        {error,eperm} ->
            file:change_mode(Item, 8#777),
            DelCnt = delete_files(filelib:wildcard(filename:join(Item, "*"))),
            file:del_dir(Item);
        {error,eacces} ->
            %% We'll see about that!
            file:change_mode(Item, 8#777),
            case file:delete(Item) of
                ok ->
		    DelCnt = 1;
                {error,_} ->
                    erlang:yield(),
                    file:change_mode(Item, 8#777),
                    file:delete(Item),
                    DelCnt = 1
            end;
        {error,_} ->
            DelCnt = 0
    end,
    do_delete_files(Rest, Cnt + DelCnt).

%% Test control of which files that should be compressed.
compress_control(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    Dir = "compress_control",
    Files = [
             {Dir,                                                          dir,   $d},
             {filename:join([Dir, "first_file.txt"]), 10000, $f},
             {filename:join([Dir, "a_dir"]), dir,   $d},
             {filename:join([Dir, "a_dir", "zzz.zip"]), 10000, $z},
             {filename:join([Dir, "a_dir", "lll.lzh"]), 10000, $l},
             {filename:join([Dir, "a_dir", "eee.exe"]), 10000, $e},
             {filename:join([Dir, "a_dir", "ggg.arj"]), 10000, $g},
             {filename:join([Dir, "a_dir", "b_dir"]), dir,   $d},
             {filename:join([Dir, "a_dir", "b_dir", "ggg.arj"]), 10000, $a},
             {filename:join([Dir, "last_file.txt"]), 10000, $l}
            ],

    test_compress_control(Dir,
			  Files,
			  [{compress, []}],
			  []),

    test_compress_control(Dir,
			  Files,
			  [{uncompress, all}],
			  []),

    test_compress_control(Dir,
			  Files,
			  [{uncompress, []}],
			  [".txt", ".exe", ".zip", ".lzh", ".arj"]),

    test_compress_control(Dir,
			  Files,
			  [],
			  [".txt", ".exe"]),

    test_compress_control(Dir,
			  Files,
			  [{uncompress, {add, [".exe"]}},
			   {uncompress, {del, [".zip", "arj"]}}],
			  [".txt", ".zip", "arj"]),

    test_compress_control(Dir,
			  Files,
			  [{uncompress, []},
			   {uncompress, {add, [".exe"]}},
			   {uncompress, {del, [".zip", "arj"]}}],
			  [".txt", ".zip", ".lzh", ".arj"]),

    ok.

test_compress_control(Dir, Files, ZipOptions, Expected) ->
    %% Cleanup
    Zip = "zip.zip",
    Names = [N || {N, _, _} <- Files],
    delete_files([Zip]),
    delete_files(lists:reverse(Names)),

    create_files(Files),
    {ok, Zip} = zip:create(Zip, [Dir], ZipOptions),

    {ok, OpenZip} = zip:openzip_open(Zip, [memory]),
    {ok,[#zip_comment{comment = ""} | ZipList]} = zip:openzip_list_dir(OpenZip),
    io:format("compress_control:  -> ~p  -> ~p\n  -> ~pn", [Expected, ZipOptions, ZipList]),
    verify_compression(Files, ZipList, OpenZip, ZipOptions, Expected),
    ok = zip:openzip_close(OpenZip),

    %% Cleanup
    delete_files([Zip]),
    delete_files(lists:reverse(Names)), % Remove plain files before directories

    ok.

verify_compression([{Name, Kind, _Filler} | Files], ZipList, OpenZip, ZipOptions, Expected) ->
    {Name2, BinSz} =
        case Kind of
            dir ->
                {Name ++ "/", 0};
            _   ->
                {ok, {Name, Bin}} = zip:openzip_get(Name, OpenZip),
                {Name, size(Bin)}
        end,
    {Name2, {value, ZipFile}} = {Name2, lists:keysearch(Name2,  #zip_file.name, ZipList)},
    #zip_file{info = #file_info{size = InfoSz, type = InfoType}, comp_size = InfoCompSz} = ZipFile,

    Ext = filename:extension(Name),
    IsComp = is_compressed(Ext, Kind, ZipOptions),
    ExpComp = lists:member(Ext, Expected),
    case {Name, Kind, InfoType, IsComp, ExpComp, BinSz, InfoSz, InfoCompSz} of
        {_, dir, directory, false, _,     Sz, Sz, Sz}      when Sz =:= BinSz -> ok;
        {_, Sz,  regular,   false, false, Sz, Sz, Sz}      when Sz =:= BinSz -> ok;
        {_, Sz,  regular,   true,  true,  Sz, Sz, OtherSz} when Sz =:= BinSz, OtherSz =/= BinSz -> ok
    end,
    verify_compression(Files, ZipList -- [ZipFile], OpenZip, ZipOptions, Expected);
verify_compression([], [], _OpenZip, _ZipOptions, _Expected) ->
    ok.

is_compressed(_Ext, dir, _Options) ->
    false;
is_compressed(Ext, _Sz, Options) ->
    CompressOpt =
        case [What || {compress, What} <- Options] of
            [] -> all;
            CompressOpts-> extensions(CompressOpts, all)
        end,
    DoCompress = (CompressOpt =:= all) orelse lists:member(Ext, CompressOpt),
    Default = [".Z", ".zip", ".zoo", ".arc", ".lzh", ".arj"],
    UncompressOpt =
        case [What || {uncompress, What} <- Options] of
            [] -> Default;
            UncompressOpts-> extensions(UncompressOpts, Default)
        end,
    DoUncompress = (UncompressOpt =:= all) orelse lists:member(Ext, UncompressOpt),
    DoCompress andalso not DoUncompress.

extensions([H | T], Old) ->
    case H of
        all ->
            extensions(T, H);
        H when is_list(H) ->
            extensions(T, H);
        {add, New} when is_list(New), is_list(Old) ->
            extensions(T, Old ++ New);
        {del, New} when is_list(New), is_list(Old) ->
            extensions(T, Old -- New);
        _ ->
            extensions(T, Old)
    end;
extensions([], Old) ->
    Old.

foldl(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    File = filename:join([PrivDir, "foldl.zip"]),

    FooBin = <<"FOO">>,
    BarBin = <<"BAR">>,
    Files = [{"foo", FooBin}, {"bar", BarBin}],
    {ok, {File, Bin}} = zip:create(File, Files, [memory]),
    ZipFun = fun(N, I, B, Acc) -> [{N, B(), I()} | Acc] end,
    {ok, FileSpec} = zip:foldl(ZipFun, [], {File, Bin}),
    [{"bar", BarBin, #file_info{}}, {"foo", FooBin, #file_info{}}] = FileSpec,
    {ok, {File, Bin}} = zip:create(File, lists:reverse(FileSpec), [memory]),
    {foo_bin, FooBin} =
	try
	    zip:foldl(fun("foo", _, B, _) -> throw(B()); (_, _, _, Acc) -> Acc end, [], {File, Bin})
	catch
	    throw:FooBin ->
		{foo_bin, FooBin}
	end,
    ok = file:write_file(File, Bin),
    {ok, FileSpec} = zip:foldl(ZipFun, [], File),

    {error, einval} = zip:foldl(fun() -> ok end, [], File),
    {error, einval} = zip:foldl(ZipFun, [], 42),
    {error, einval} = zip:foldl(ZipFun, [], {File, 42}),

    ok = file:delete(File),
    {error, enoent} = zip:foldl(ZipFun, [], File),

    ok.

fd_leak(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config)),
    DataDir = proplists:get_value(data_dir, Config),
    Name = filename:join(DataDir, "bad_file_header.zip"),
    BadExtract = fun() ->
                         {error,bad_file_header} = zip:extract(Name),
                         ok
                 end,
    do_fd_leak(BadExtract, 1),

    BadCreate = fun() ->
                        {error,enoent} = zip:zip("failed.zip",
                                                 ["none"]),
                        ok
                end,
    do_fd_leak(BadCreate, 1),

    ok.

do_fd_leak(_Bad, 10000) ->
    ok;
do_fd_leak(Bad, N) ->
    try Bad() of
        ok ->
            do_fd_leak(Bad, N + 1)
    catch
        C:R:Stk ->
            io:format("Bad error after ~p attempts\n", [N]),
            erlang:raise(C, R, Stk)
    end.
