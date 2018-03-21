%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
-module(prim_file_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, init_per_testcase/2, end_per_testcase/2,
	 read_write_file/1, free_memory/0]).
-export([cur_dir_0/1, cur_dir_1/1,
	 make_del_dir/1, pos1/1, pos2/1]).
-export([close/1, delete/1]).
-export([open1/1, modes/1]).
-export([file_info_basic_file/1, file_info_basic_directory/1, file_info_bad/1,
	 file_info_times/1, file_write_file_info/1,
         file_read_file_info_opts/1, file_write_file_info_opts/1,
	 file_write_read_file_info_opts/1]).
-export([rename/1, access/1, truncate/1, datasync/1, sync/1,
	 read_write/1, pread_write/1, append/1, exclusive/1]).
-export([e_delete/1, e_rename/1, e_make_dir/1, e_del_dir/1]).

-export([make_link/1, read_link_info_for_non_link/1,
	 symlinks/1,
	 list_dir_limit/1,
	 list_dir_error/1,
	 list_dir/1]).

-export([advise/1]).
-export([large_write/1]).

%% System probe functions that might be handy to check from the shell
-export([unix_free/1]).

-export([allocate/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(PRIM_FILE, prim_file).

suite() -> [].

all() -> 
    [read_write_file, {group, dirs}, {group, files},
     delete, rename, {group, errors}, {group, links},
     list_dir_limit, list_dir].

groups() -> 
    [{dirs, [],
      [make_del_dir, cur_dir_0, cur_dir_1]},
     {files, [],
      [{group, open}, {group, pos}, {group, file_info},
       truncate, sync, datasync, advise, large_write, allocate]},
     {open, [],
      [open1, modes, close, access, read_write, pread_write,
       append, exclusive]},
     {pos, [], [pos1, pos2]},
     {file_info, [],
      [file_info_basic_file,file_info_basic_directory, file_info_bad,
       file_info_times, file_write_file_info, file_read_file_info_opts,
       file_write_file_info_opts, file_write_read_file_info_opts
      ]},
     {errors, [],
      [e_delete, e_rename, e_make_dir, e_del_dir]},
     {links, [],
      [make_link, read_link_info_for_non_link, symlinks, list_dir_error]}].

init_per_testcase(large_write, Config) ->
    {ok, Started} = application:ensure_all_started(os_mon),
    [{started, Started}|Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(large_write, Config) ->
    [application:stop(App) || App <- lists:reverse(proplists:get_value(started, Config))],
    ok;
end_per_testcase(_, _Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    Priv = proplists:get_value(priv_dir, Config),
	    HasAccessTime =
		case file:read_file_info(Priv) of
		    {ok, #file_info{atime={_, {0, 0, 0}}}} ->
			%% This is a unfortunately a FAT file system.
			[no_access_time];
		    {ok, _} ->
			[]
		end,
	    HasAccessTime++Config;
	_ ->
	    Config
    end.

end_per_suite(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    os:cmd("subst z: /d");
	_ ->
	    ok
    end,
    Config.

%% Matches a term (the last) against alternatives
expect(X, _, X) ->
    X;
expect(_, X, X) ->
    X.

expect(X, _, _, X) ->
    X;
expect(_, X, _, X) ->
    X;
expect(_, _, X, X) ->
    X.

expect(X, _, _, _, X) ->
    X;
expect(_, X, _, _, X) ->
    X;
expect(_, _, X, _, X) ->
    X;
expect(_, _, _, X, X) ->
    X.

%% Calculate the time difference
time_dist({YY, MM, DD, H, M, S}, DT) ->
    time_dist({{YY, MM, DD}, {H, M, S}}, DT);
time_dist(DT, {YY, MM, DD, H, M, S}) ->
    time_dist(DT, {{YY, MM, DD}, {H, M, S}});
time_dist({_D1, _T1} = DT1, {_D2, _T2} = DT2) ->
    calendar:datetime_to_gregorian_seconds(DT2)
	- calendar:datetime_to_gregorian_seconds(DT1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_write_file(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_read_write_file"),

    %% Try writing and reading back some term
    SomeTerm = {"This term",{will,be},[written,$t,$o],1,file,[]},
    ok = ?PRIM_FILE:write_file(Name,term_to_binary(SomeTerm)),
    {ok,Bin1} = ?PRIM_FILE:read_file(Name),
    SomeTerm = binary_to_term(Bin1),

    %% Try a "null" term
    NullTerm = [],
    ok = ?PRIM_FILE:write_file(Name,term_to_binary(NullTerm)),
    {ok,Bin2} = ?PRIM_FILE:read_file(Name),
    NullTerm = binary_to_term(Bin2),

    %% Try some "complicated" types
    BigNum = 123456789012345678901234567890,
    ComplTerm = {self(),make_ref(),BigNum,3.14159},
    ok = ?PRIM_FILE:write_file(Name,term_to_binary(ComplTerm)),
    {ok,Bin3} = ?PRIM_FILE:read_file(Name),
    ComplTerm = binary_to_term(Bin3),

    %% Try reading a nonexistent file
    Name2 = filename:join(RootDir,
			  atom_to_list(?MODULE)
			  ++"_nonexistent_file"),
    {error, enoent} = ?PRIM_FILE:read_file(Name2),
    {error, enoent} = ?PRIM_FILE:read_file(""),

    %% Try writing to a bad filename
    {error, enoent} =
	?PRIM_FILE:write_file("",term_to_binary(NullTerm)),

    %% Try writing something else than a binary
    {error, badarg} = ?PRIM_FILE:write_file(Name,{1,2,3}),
    {error, badarg} = ?PRIM_FILE:write_file(Name,self()),

    %% Some non-term binaries
    ok = ?PRIM_FILE:write_file(Name,[]),
    {ok,Bin4} = ?PRIM_FILE:read_file(Name),
    0 = byte_size(Bin4),

    ok = ?PRIM_FILE:write_file(Name,[Bin1,[],[[Bin2]]]),
    {ok,Bin5} = ?PRIM_FILE:read_file(Name),
    {Bin1,Bin2} = split_binary(Bin5,byte_size(Bin1)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_del_dir(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_mk-dir"),
    ok = ?PRIM_FILE:make_dir(NewDir),
    {error, eexist} = ?PRIM_FILE:make_dir(NewDir),
    ok = ?PRIM_FILE:del_dir(NewDir),
    {error, enoent} = ?PRIM_FILE:del_dir(NewDir),

    %% Make sure we are not in a directory directly under test_server
    %% as that would result in eacces errors when trying to delete '..',
    %% because there are processes having that directory as current.
    ok = ?PRIM_FILE:make_dir(NewDir),
    {ok, CurrentDir} = ?PRIM_FILE:get_cwd(),
    case {os:type(), length(NewDir) >= 260 } of
	{{win32,_}, true} ->
	    io:format("Skip set_cwd for windows path longer than 260 (MAX_PATH)\n", []),
	    io:format("\nNewDir = ~p\n", [NewDir]);
	_ ->
	    ok = ?PRIM_FILE:set_cwd(NewDir)
    end,
    try
	%% Check that we get an error when trying to create...
	%% a deep directory
	NewDir2 = filename:join(RootDir,
				atom_to_list(?MODULE)
				++"_mk-dir-noexist/foo"),
	{error, enoent} = ?PRIM_FILE:make_dir(NewDir2),
	%% a nameless directory
	{error, enoent} = ?PRIM_FILE:make_dir(""),
	%% a directory with illegal name
	{error, badarg} = ?PRIM_FILE:make_dir('mk-dir'),

	%% a directory with illegal name, even if it's a (bad) list
	{error, badarg} = ?PRIM_FILE:make_dir([1,2,3,{}]),

	%% Maybe this isn't an error, exactly, but worth mentioning anyway:
	%% ok = ?PRIM_FILE:make_dir([$f,$o,$o,0,$b,$a,$r])),
	%% The above line works, and created a directory "./foo"
	%% More elegant would maybe have been to fail, or to really create
	%% a directory, but with a name that incorporates the "bar" part of
	%% the list, so that [$f,$o,$o,0,$f,$o,$o] wouldn't refer to the same
	%% dir. But this would slow it down.

	%% Try deleting some bad directories
	%% Deleting the parent directory to the current, sounds dangerous, huh?
	%% Don't worry ;-) the parent directory should never be empty, right?
	case ?PRIM_FILE:del_dir("..") of
	    {error, eexist} -> ok;
	    {error, eacces} -> ok;	%OpenBSD
	    {error, einval} -> ok		%FreeBSD
	end,
	{error, enoent} = ?PRIM_FILE:del_dir(""),
	{error, badarg} = ?PRIM_FILE:del_dir([3,2,1,{}])
    after
	ok = ?PRIM_FILE:set_cwd(CurrentDir)
    end,
    ok.

cur_dir_0(Config) when is_list(Config) ->
    %% Find out the current dir, and cd to it ;-)
    {ok,BaseDir} = ?PRIM_FILE:get_cwd(),
    Dir1 = BaseDir ++ "", %% Check that it's a string
    ok = ?PRIM_FILE:set_cwd(Dir1),
    DirName = atom_to_list(?MODULE) ++ "_curdir",

    %% Make a new dir, and cd to that
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir, DirName),
    ok = ?PRIM_FILE:make_dir(NewDir),
    case {os:type(), length(NewDir) >= 260} of
	{{win32,_}, true} ->
	    io:format("Skip set_cwd for windows path longer than 260 (MAX_PATH):\n"),
	    io:format("\nNewDir = ~p\n", [NewDir]);
	_ ->
	    io:format("cd to ~s",[NewDir]),
	    ok = ?PRIM_FILE:set_cwd(NewDir),

	    %% Create a file in the new current directory, and check that it
	    %% really is created there
	    UncommonName = "uncommon.fil",
	    {ok,Fd} = ?PRIM_FILE:open(UncommonName, [read, write]),
	    ok = ?PRIM_FILE:close(Fd),
	    {ok,NewDirFiles} = ?PRIM_FILE:list_dir("."),
	    true = lists:member(UncommonName,NewDirFiles),

	    %% Delete the directory and return to the old current directory
	    %% and check that the created file isn't there (too!)
	    expect({error, einval}, {error, eacces}, {error, eexist},
		   ?PRIM_FILE:del_dir(NewDir)),
	    ?PRIM_FILE:delete(UncommonName),
	    {ok,[]} = ?PRIM_FILE:list_dir("."),
	    ok = ?PRIM_FILE:set_cwd(Dir1),
	    io:format("cd back to ~s",[Dir1]),
	    ok = ?PRIM_FILE:del_dir(NewDir),
	    {error, enoent} = ?PRIM_FILE:set_cwd(NewDir),
	    ok = ?PRIM_FILE:set_cwd(Dir1),
	    io:format("cd back to ~s",[Dir1]),
	    {ok,OldDirFiles} = ?PRIM_FILE:list_dir("."),
	    false = lists:member(UncommonName,OldDirFiles)
    end,

    %% Try doing some bad things
    {error, badarg} =
	?PRIM_FILE:set_cwd({foo,bar}),
    {error, enoent} =
	?PRIM_FILE:set_cwd(""),
    {error, enoent} =
	?PRIM_FILE:set_cwd(".......a......"),
    {ok,BaseDir} =
	?PRIM_FILE:get_cwd(), %% Still there?

    %% On Windows, there should only be slashes, no backslashes,
    %% in the return value of get_cwd().
    %% (The test is harmless on Unix, because filenames usually
    %% don't contain backslashes.)

    {ok, BaseDir} = ?PRIM_FILE:get_cwd(),
    false = lists:member($\\, BaseDir),

    ok.

%% Tests ?PRIM_FILE:get_cwd/1.

cur_dir_1(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    win_cur_dir_1(Config);
	_ ->
	    {error, enotsup} =
		?PRIM_FILE:get_cwd("d:")
    end,
    ok.

win_cur_dir_1(_Config) ->
    {ok, BaseDir} = ?PRIM_FILE:get_cwd(),

    %% Get the drive letter from the current directory,
    %% and try to get current directory for that drive.

    [Drive, $:|_] = BaseDir,
    {ok, BaseDir} = ?PRIM_FILE:get_cwd([Drive, $:]),
    io:format("BaseDir = ~s\n", [BaseDir]),

    %% Unfortunately, there is no way to move away from the
    %% current drive as we can't use the "subst" command from
    %% a SSH connection. We can't test any more. Too bad.

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



open1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_files"),
    ok = ?PRIM_FILE:make_dir(NewDir),
    Name = filename:join(NewDir, "foo1.fil"),
    {ok,Fd1} = ?PRIM_FILE:open(Name, [read, write]),
    {ok,Fd2} = ?PRIM_FILE:open(Name, [read]),
    Bin = list_to_binary("{a,tuple}.\n"),
    Length = byte_size(Bin),
    ?PRIM_FILE:write(Fd1,Bin),
    {ok,0} = ?PRIM_FILE:position(Fd1,bof),
    {ok, Bin} = ?PRIM_FILE:read(Fd1,Length),
    {ok, Bin} = ?PRIM_FILE:read(Fd2,Length),
    ok = ?PRIM_FILE:close(Fd2),
    {ok,0} = ?PRIM_FILE:position(Fd1,bof),
    ok = ?PRIM_FILE:truncate(Fd1),
    eof = ?PRIM_FILE:read(Fd1,Length),
    ok = ?PRIM_FILE:close(Fd1),
    {ok,Fd3} = ?PRIM_FILE:open(Name, [read]),
    eof = ?PRIM_FILE:read(Fd3,Length),
    ok = ?PRIM_FILE:close(Fd3),
    ok.

%% Tests all open modes.

modes(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_open_modes"),
    ok = ?PRIM_FILE:make_dir(NewDir),
    Name1 = filename:join(NewDir, "foo1.fil"),
    Marker = <<"hello, world">>,
    Length = byte_size(Marker),

    %% write
    {ok, Fd1} = ?PRIM_FILE:open(Name1, [write]),
    ok = ?PRIM_FILE:write(Fd1, Marker),
    ok = ?PRIM_FILE:write(Fd1, <<".\n">>),
    ok = ?PRIM_FILE:close(Fd1),

    %% read
    {ok, Fd2} = ?PRIM_FILE:open(Name1, [read]),
    {ok, Marker} = ?PRIM_FILE:read(Fd2, Length),
    ok = ?PRIM_FILE:close(Fd2),

    %% read and write
    {ok, Fd3} = ?PRIM_FILE:open(Name1, [read, write]),
    {ok, Marker} = ?PRIM_FILE:read(Fd3, Length),
    ok = ?PRIM_FILE:write(Fd3, Marker),
    ok = ?PRIM_FILE:close(Fd3),

    %% read by default
    {ok, Fd4} = ?PRIM_FILE:open(Name1, []),
    {ok, Marker} = ?PRIM_FILE:read(Fd4, Length),
    ok = ?PRIM_FILE:close(Fd4),

    ok.

close(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_close.fil"),
    {ok,Fd1} = ?PRIM_FILE:open(Name, [read, write]),
    %% Just closing it is no fun, we did that a million times already
    %% This is a common error, for code written before Erlang 4.3
    %% bacause then ?PRIM_FILE:open just returned a Pid, and not everyone
    %% really checked what they got.
    {'EXIT',_Msg} = (catch ok = ?PRIM_FILE:close({ok,Fd1})),
    ok = ?PRIM_FILE:close(Fd1),

    %% Try closing one more time
    Val = ?PRIM_FILE:close(Fd1),
    io:format("Second close gave: ~p", [Val]),

    ok.

access(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_access.fil"),
    Bin = <<"ABCDEFGH">>,
    {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?PRIM_FILE:write(Fd1,Bin),
    ok = ?PRIM_FILE:close(Fd1),
    %% Check that we can't write when in read only mode
    {ok,Fd2} = ?PRIM_FILE:open(Name, [read]),
    case catch ?PRIM_FILE:write(Fd2,"XXXX") of
	ok ->
	    ct:fail({access,write});
	_ ->
	    ok
    end,
    ok = ?PRIM_FILE:close(Fd2),
    {ok, Fd3} = ?PRIM_FILE:open(Name, [read]),
    {ok, Bin} = ?PRIM_FILE:read(Fd3,byte_size(Bin)),
    ok = ?PRIM_FILE:close(Fd3),

    ok.

%% Tests ?PRIM_FILE:read/2 and ?PRIM_FILE:write/2.

read_write(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_read_write"),
    ok = ?PRIM_FILE:make_dir(NewDir),

    %% Raw file.
    Name = filename:join(NewDir, "raw.fil"),
    {ok, Fd} = ?PRIM_FILE:open(Name, [read, write]),
    read_write_test(Fd),

    ok.

read_write_test(File) ->
    Marker = <<"hello, world">>,
    ok = ?PRIM_FILE:write(File, Marker),
    {ok, 0} = ?PRIM_FILE:position(File, 0),
    {ok, Marker} = ?PRIM_FILE:read(File, 100),
    eof = ?PRIM_FILE:read(File, 100),
    ok = ?PRIM_FILE:close(File),
    ok.


%% Tests ?PRIM_FILE:pread/2 and ?PRIM_FILE:pwrite/2.

pread_write(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_pread_write"),
    ok = ?PRIM_FILE:make_dir(NewDir),

    %% Raw file.
    Name = filename:join(NewDir, "raw.fil"),
    {ok, Fd} = ?PRIM_FILE:open(Name, [read, write]),
    pread_write_test(Fd),

    ok.

pread_write_test(File) ->
    Marker = <<"hello, world">>,
    Len = byte_size(Marker),
    ok = ?PRIM_FILE:write(File, Marker),
    {ok, Marker} = ?PRIM_FILE:pread(File, 0, 100),
    eof = ?PRIM_FILE:pread(File, 100, 1),
    ok = ?PRIM_FILE:pwrite(File, Len, Marker),
    {ok, Marker} = ?PRIM_FILE:pread(File, Len, 100),
    eof = ?PRIM_FILE:pread(File, 100, 1),
    MM = <<Marker/binary,Marker/binary>>,
    {ok, MM} = ?PRIM_FILE:pread(File, 0, 100),
    ok = ?PRIM_FILE:close(File),
    ok.

%% Test appending to a file.
append(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_append"),
    ok = ?PRIM_FILE:make_dir(NewDir),

    First = "First line\n",
    Second = "Seond lines comes here\n",
    Third = "And here is the third line\n",

    %% Write a small text file.
    Name1 = filename:join(NewDir, "a_file.txt"),
    {ok, Fd1} = ?PRIM_FILE:open(Name1, [write]),
    ok = ?PRIM_FILE:write(Fd1, First),
    ok = ?PRIM_FILE:write(Fd1, Second),
    ok = ?PRIM_FILE:close(Fd1),

    %% Open it a again and a append a line to it.
    {ok, Fd2} = ?PRIM_FILE:open(Name1, [append]),
    ok = ?PRIM_FILE:write(Fd2, Third),
    ok = ?PRIM_FILE:close(Fd2),

    %% Read it back and verify.
    Expected = list_to_binary([First, Second, Third]),
    {ok, Expected} = ?PRIM_FILE:read_file(Name1),

    ok.

%% Test exclusive access to a file.
exclusive(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_exclusive"),
    ok = ?PRIM_FILE:make_dir(NewDir),
    Name = filename:join(NewDir, "ex_file.txt"),
    {ok,Fd} = ?PRIM_FILE:open(Name, [write, exclusive]),
    {error, eexist} = ?PRIM_FILE:open(Name, [write, exclusive]),
    ok = ?PRIM_FILE:close(Fd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pos1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_pos1.fil"),
    {ok, Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?PRIM_FILE:write(Fd1,<<"ABCDEFGH">>),
    ok        = ?PRIM_FILE:close(Fd1),
    {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),

    %% Start pos is first char
    io:format("Relative positions"),
    {ok, <<"A">>} = ?PRIM_FILE:read(Fd2,1),
    {ok, 2}   = ?PRIM_FILE:position(Fd2,{cur,1}),
    {ok, <<"C">>} = ?PRIM_FILE:read(Fd2,1),
    {ok, 0}   = ?PRIM_FILE:position(Fd2,{cur,-3}),
    {ok, <<"A">>} = ?PRIM_FILE:read(Fd2,1),
    %% Backwards from first char should be an error
    {ok,0}    = ?PRIM_FILE:position(Fd2,{cur,-1}),
    {error, einval} = ?PRIM_FILE:position(Fd2,{cur,-1}),
    %% Reset position and move again
    {ok, 0}   = ?PRIM_FILE:position(Fd2,0),
    {ok, 2}   = ?PRIM_FILE:position(Fd2,{cur,2}),
    {ok, <<"C">>} = ?PRIM_FILE:read(Fd2,1),
    %% Go a lot forwards
    {ok, 13}  = ?PRIM_FILE:position(Fd2,{cur,10}),
    eof       = ?PRIM_FILE:read(Fd2,1),

    %% Try some fixed positions
    io:format("Fixed positions"),
    {ok, 8}   = ?PRIM_FILE:position(Fd2,8),
    eof = ?PRIM_FILE:read(Fd2,1),
    {ok, 8}   = ?PRIM_FILE:position(Fd2,cur),
    eof = ?PRIM_FILE:read(Fd2,1),
    {ok, 7}   = ?PRIM_FILE:position(Fd2,7),
    {ok, <<"H">>} = ?PRIM_FILE:read(Fd2,1),
    {ok, 0}   = ?PRIM_FILE:position(Fd2,0),
    {ok, <<"A">>} = ?PRIM_FILE:read(Fd2,1),
    {ok, 3}   = ?PRIM_FILE:position(Fd2,3),
    {ok, <<"D">>} = ?PRIM_FILE:read(Fd2,1),
    {ok, 12}  = ?PRIM_FILE:position(Fd2,12),
    eof       = ?PRIM_FILE:read(Fd2,1),
    {ok, 3}   = ?PRIM_FILE:position(Fd2,3),
    {ok, <<"D">>} = ?PRIM_FILE:read(Fd2,1),
    %% Try the {bof,X} notation
    {ok, 3}   = ?PRIM_FILE:position(Fd2,{bof,3}),
    {ok, <<"D">>} = ?PRIM_FILE:read(Fd2,1),

    %% Try eof positions
    io:format("EOF positions"),
    {ok, 8}   = ?PRIM_FILE:position(Fd2,{eof,0}),
    eof       = ?PRIM_FILE:read(Fd2,1),
    {ok, 7}   = ?PRIM_FILE:position(Fd2,{eof,-1}),
    {ok, <<"H">>} = ?PRIM_FILE:read(Fd2,1),
    {ok, 0}   = ?PRIM_FILE:position(Fd2,{eof,-8}),
    {ok, <<"A">>} = ?PRIM_FILE:read(Fd2,1),
    {error, einval} = ?PRIM_FILE:position(Fd2,{eof,-9}),
    ok.

pos2(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_pos2.fil"),
    {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?PRIM_FILE:write(Fd1,<<"ABCDEFGH">>),
    ok = ?PRIM_FILE:close(Fd1),
    {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),
    {error, einval} = ?PRIM_FILE:position(Fd2,-1),

    %% Make sure that we still can search after an error.
    {ok, 0}   = ?PRIM_FILE:position(Fd2, 0),
    {ok, 3}   = ?PRIM_FILE:position(Fd2, {bof,3}),
    {ok, <<"D">>} = ?PRIM_FILE:read(Fd2,1),

    io:format("DONE"),
    ok.

file_info_basic_file(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),

    %% Create a short file.
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_basic_test"".fil"),
    {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?PRIM_FILE:write(Fd1, "foo bar"),
    ok = ?PRIM_FILE:close(Fd1),

    %% Test that the file has the expected attributes.
    %% The times are tricky, so we will save them to a separate test case.
    {ok, FileInfo} = ?PRIM_FILE:read_file_info(Name),
    #file_info{size = Size, type = Type, access = Access,
	       atime = AccessTime, mtime = ModifyTime} =
	FileInfo,
    io:format("Access ~p, Modify ~p", [AccessTime, ModifyTime]),
    Size = 7,
    Type = regular,
    Access = read_write,
    true = abs(time_dist(filter_atime(AccessTime, Config),
			 filter_atime(ModifyTime,
				      Config))) < 2,
    {AD, AT} = AccessTime,
    all_integers(tuple_to_list(AD) ++ tuple_to_list(AT)),
    {MD, MT} = ModifyTime,
    all_integers(tuple_to_list(MD) ++ tuple_to_list(MT)),

    ok.

file_info_basic_directory(Config) when is_list(Config) ->
    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?PRIM_FILE:read_file_info/1 to work on
    %% platforms such as Windows95.
    RootDir = filename:join([proplists:get_value(priv_dir, Config)]),

    %% Test that the RootDir directory has the expected attributes.
    test_directory(RootDir, read_write),

    %% Note that on Windows file systems, "/" or "c:/" are *NOT* directories.
    %% Therefore, test that ?PRIM_FILE:read_file_info/1 behaves 
    %% as if they were directories.
    case os:type() of
	{win32, _} ->
	    test_directory("/", read_write),
	    test_directory("c:/", read_write),
	    test_directory("c:\\", read_write);
	_ ->
	    test_directory("/", read)
    end,
    ok.

test_directory(Name, ExpectedAccess) ->
    {ok, FileInfo} = ?PRIM_FILE:read_file_info(Name),
    #file_info{size = Size, type = Type, access = Access,
	       atime = AccessTime, mtime = ModifyTime} =
	FileInfo,
    io:format("Testing directory ~s", [Name]),
    io:format("Directory size is ~p", [Size]),
    io:format("Access ~p", [Access]),
    io:format("Access time ~p; Modify time~p",
	      [AccessTime, ModifyTime]),
    Type = directory,
    Access = ExpectedAccess,
    {AD, AT} = AccessTime,
    all_integers(tuple_to_list(AD) ++ tuple_to_list(AT)),
    {MD, MT} = ModifyTime,
    all_integers(tuple_to_list(MD) ++ tuple_to_list(MT)),
    ok.

all_integers([Int|Rest]) when is_integer(Int) ->
    all_integers(Rest);
all_integers([]) ->
    ok.

%% Try something nonexistent.

file_info_bad(Config) when is_list(Config) ->
    RootDir = filename:join([proplists:get_value(priv_dir, Config)]),
    NonExistent = filename:join(RootDir, atom_to_list(?MODULE)++"_nonexistent"),
    {error, enoent} = ?PRIM_FILE:read_file_info(NonExistent),
    ok.

%% Test that the file times behave as they should.

file_info_times(Config) when is_list(Config) ->
    %% We have to try this twice, since if the test runs across the change
    %% of a month the time diff calculations will fail. But it won't happen
    %% if you run it twice in succession.
    test_server:m_out_of_n(
      1,2,
      fun() -> file_info_int(Config) end),
    ok.

file_info_int(Config) ->
    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?PRIM_FILE:read_file_info/1 to work on
    %% platforms such as Windows95.

    RootDir = filename:join([proplists:get_value(priv_dir, Config)]),
    io:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_file_info.fil"),
    {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?PRIM_FILE:write(Fd1,"foo"),

    %% check that the file got a modify date max a few seconds away from now
    {ok, #file_info{type = regular,
		    atime = AccTime1, mtime = ModTime1}} =
	?PRIM_FILE:read_file_info(Name),
    Now = erlang:localtime(),
    io:format("Now ~p",[Now]),
    io:format("Open file Acc ~p Mod ~p",[AccTime1,ModTime1]),
    true = abs(time_dist(filter_atime(Now, Config),
			 filter_atime(AccTime1,
				      Config))) < 8,
    true = abs(time_dist(Now, ModTime1)) < 8,

    %% Sleep until we can be sure the seconds value has changed.
    %% Note: FAT-based filesystem (like on Windows 95) have
    %% a resolution of 2 seconds.
    ct:sleep({seconds,2.2}),

    %% close the file, and watch the modify date change
    ok = ?PRIM_FILE:close(Fd1),
    {ok, #file_info{size = Size, type = regular, access = Access,
		    atime = AccTime2, mtime = ModTime2}} =
	?PRIM_FILE:read_file_info(Name),
    io:format("Closed file Acc ~p Mod ~p",[AccTime2,ModTime2]),
    true = time_dist(ModTime1, ModTime2) >= 0,

    %% this file is supposed to be binary, so it'd better keep it's size
    Size = 3,
    Access = read_write,

    %% Do some directory checking
    {ok, #file_info{size = DSize, type = directory,
		    access = DAccess,
		    atime = AccTime3, mtime = ModTime3}} =
	?PRIM_FILE:read_file_info(RootDir),
    %% this dir was modified only a few secs ago
    io:format("Dir Acc ~p; Mod ~p; Now ~p",
	      [AccTime3, ModTime3, Now]),
    true = abs(time_dist(Now, ModTime3)) < 5,
    DAccess = read_write,
    io:format("Dir size is ~p",[DSize]),
    ok.

%% Filter access times, to cope with a deficiency of FAT file systems
%% (on Windows): The access time is actually only a date.

filter_atime(Atime, Config) ->
    case lists:member(no_access_time, Config) of
	true ->
	    case Atime of
	    	{Date, _} ->
		    {Date, {0, 0, 0}};
		{Y, M, D, _, _, _} ->
		    {Y, M, D, 0, 0, 0}
	    end;
	false ->
	    Atime
    end.

%% Test the write_file_info/2 function.

file_write_file_info(Config) when is_list(Config) ->
    RootDir = get_good_directory(Config),
    io:format("RootDir = ~p", [RootDir]),

    %% Set the file to read only AND update the file times at the same time.
    %% (This used to fail on Windows NT/95 for a local filesystem.)
    %% Note: Seconds must be even; see note in file_info_times/1.

    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_write_file_info_ro"),
    ok = ?PRIM_FILE:write_file(Name, "hello"),
    Time = {{1997, 01, 02}, {12, 35, 42}},
    Info = #file_info{mode=8#400, atime=Time, mtime=Time, ctime=Time},
    ok = ?PRIM_FILE:write_file_info(Name, Info),

    %% Read back the times.

    {ok, ActualInfo} =
	?PRIM_FILE:read_file_info(Name),
    #file_info{mode=_Mode, atime=ActAtime, mtime=Time,
	       ctime=ActCtime} = ActualInfo,
    FilteredAtime = filter_atime(Time, Config),
    FilteredAtime = filter_atime(ActAtime, Config),
    case os:type() of
	{win32, _} ->
	    %% On Windows, "ctime" means creation time and it can
	    %% be set.
	    ActCtime = Time;
	_ ->
	    ok
    end,
    {error, eacces} = ?PRIM_FILE:write_file(Name, "hello again"),

    %% Make the file writable again.
    ?PRIM_FILE:write_file_info(Name, #file_info{mode=8#600}),
    ok = ?PRIM_FILE:write_file(Name, "hello again"),

    %% And unwritable.
    ?PRIM_FILE:write_file_info(Name, #file_info{mode=8#400}),
    {error, eacces} = ?PRIM_FILE:write_file(Name, "hello again"),

    %% Write the times again.
    %% Note: Seconds must be even; see note in file_info_times/1.

    NewTime = {{1997, 02, 15}, {13, 18, 20}},
    NewInfo = #file_info{atime=NewTime, mtime=NewTime, ctime=NewTime},
    ok = ?PRIM_FILE:write_file_info(Name, NewInfo),
    {ok, ActualInfo2} =
	?PRIM_FILE:read_file_info(Name),
    #file_info{atime=NewActAtime, mtime=NewTime,
	       ctime=NewActCtime} = ActualInfo2,
    NewFilteredAtime = filter_atime(NewTime, Config),
    NewFilteredAtime = filter_atime(NewActAtime, Config),
    case os:type() of
	{win32, _} -> NewActCtime = NewTime;
	_ -> ok
    end,

    %% The file should still be unwritable.
    {error, eacces} = ?PRIM_FILE:write_file(Name, "hello again"),

    %% Make the file writeable again, so that we can remove the
    %% test suites ... :-)
    ?PRIM_FILE:write_file_info(Name, #file_info{mode=8#600}),
    ok.

%% Test the write_file_info/3 function.

file_write_file_info_opts(Config) when is_list(Config) ->
    RootDir = get_good_directory(Config),
    io:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir, atom_to_list(?MODULE) ++"_write_file_info_opts"),
    ok   = ?PRIM_FILE:write_file(Name, "hello_opts"),

    lists:foreach(fun
		      ({FI, Opts}) ->
			 ok = ?PRIM_FILE:write_file_info(Name, FI, Opts)
		 end, [
			{#file_info{ mode=8#600, atime = Time, mtime = Time, ctime = Time}, Opts} ||
			  Opts <- [[{time, posix}]],
			  Time <- [ 0,1,-1,100,-100,1000,-1000,10000,-10000 ]
		      ]),

    %% REM: determine date range dependent on time_t = Uint32 | Sint32 | Sint64 | Uint64
    %% Determine time_t on os:type()?
    lists:foreach(fun ({FI, Opts}) ->
			 ok = ?PRIM_FILE:write_file_info(Name, FI, Opts)
		 end, [ {#file_info{ mode=8#400, atime = Time, mtime = Time, ctime = Time}, Opts} ||
			  Opts <- [[{time, universal}],[{time, local}]],
			  Time <- [
				   {{1970,1,1},{0,0,0}},
				   {{1970,1,1},{0,0,1}},
			%	   {{1969,12,31},{23,59,59}},
			%	   {{1908,2,3},{23,59,59}},
				   {{2012,2,3},{23,59,59}},
				   {{2037,2,3},{23,59,59}},
				   erlang:localtime()
				  ]]),
    ok.

file_read_file_info_opts(Config) when is_list(Config) ->
    RootDir = get_good_directory(Config),
    io:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir, atom_to_list(?MODULE) ++"_read_file_info_opts"),
    ok   = ?PRIM_FILE:write_file(Name, "hello_opts"),

    lists:foreach(fun
		      (Opts) ->
			 {ok,_} = ?PRIM_FILE:read_file_info(Name, Opts)
		 end, [[{time, Type}] || Type <- [local, universal, posix]]),
    ok.

%% Test the write and read back *_file_info/3 functions.

file_write_read_file_info_opts(Config) when is_list(Config) ->
    RootDir = get_good_directory(Config),
    io:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir, atom_to_list(?MODULE) ++"_read_write_file_info_opts"),
    ok   = ?PRIM_FILE:write_file(Name, "hello_opts2"),

    ok = file_write_read_file_info_opts(Name, {{1989, 04, 28}, {19,30,22}}, [{time, local}]),
    ok = file_write_read_file_info_opts(Name, {{1989, 04, 28}, {19,30,22}}, [{time, universal}]),
    %% will not work on platforms with unsigned time_t
    %ok = file_write_read_file_info_opts(Name, {{1930, 04, 28}, {19,30,22}}, [{time, local}]),
    %ok = file_write_read_file_info_opts(Name, {{1930, 04, 28}, {19,30,22}}, [{time, universal}]),
    ok = file_write_read_file_info_opts(Name, 1, [{time, posix}]),
    %% will not work on platforms with unsigned time_t
    %ok = file_write_read_file_info_opts(Name, -1, [{time, posix}]),
    %ok = file_write_read_file_info_opts(Name, -300000, [{time, posix}]),
    ok = file_write_read_file_info_opts(Name, 300000, [{time, posix}]),
    ok = file_write_read_file_info_opts(Name, 0, [{time, posix}]),

    ok.

file_write_read_file_info_opts(Name, Mtime, Opts) ->
    {ok, FI} = ?PRIM_FILE:read_file_info(Name, Opts),
    FI2 = FI#file_info{ mtime = Mtime },
    ok = ?PRIM_FILE:write_file_info(Name, FI2, Opts),
    {ok, FI3} = ?PRIM_FILE:read_file_info(Name, Opts),
    io:format("Expecting mtime = ~p, got ~p~n", [FI2#file_info.mtime, FI3#file_info.mtime]),
    FI2 = FI3,
    ok.



%% Returns a directory on a file system that has correct file times.

get_good_directory(Config) ->
    proplists:get_value(priv_dir, Config).

truncate(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_truncate.fil"),

    %% Create a file with some data.
    MyData = "0123456789abcdefghijklmnopqrstuvxyz",
    ok = ?PRIM_FILE:write_file(Name, MyData),

    %% Truncate the file to 10 characters.
    {ok, Fd} = ?PRIM_FILE:open(Name, [read, write]),
    {ok, 10} = ?PRIM_FILE:position(Fd, 10),
    ok = ?PRIM_FILE:truncate(Fd),
    ok = ?PRIM_FILE:close(Fd),

    %% Read back the file and check that it has been truncated.
    Expected = list_to_binary("0123456789"),
    {ok, Expected} = ?PRIM_FILE:read_file(Name),

    %% Open the file read only and verify that it is not possible to
    %% truncate it, OTP-1960
    {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),
    {ok, 5} = ?PRIM_FILE:position(Fd2, 5),
    {error, _} = ?PRIM_FILE:truncate(Fd2),

    ok.


%% Tests that ?PRIM_FILE:datasync/1 at least doesn't crash.
datasync(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Sync = filename:join(PrivDir,
			 atom_to_list(?MODULE)
			 ++"_sync.fil"),

    %% Raw open.
    {ok, Fd} = ?PRIM_FILE:open(Sync, [write]),
    ok = ?PRIM_FILE:datasync(Fd),
    ok = ?PRIM_FILE:close(Fd),

    ok.


%% Tests that ?PRIM_FILE:sync/1 at least doesn't crash.
sync(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Sync = filename:join(PrivDir,
			 atom_to_list(?MODULE)
			 ++"_sync.fil"),

    %% Raw open.
    {ok, Fd} = ?PRIM_FILE:open(Sync, [write]),
    ok = ?PRIM_FILE:sync(Fd),
    ok = ?PRIM_FILE:close(Fd),

    ok.


%% Tests that ?PRIM_FILE:advise/4 at least doesn't crash.
advise(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Advise = filename:join(PrivDir,
			   atom_to_list(?MODULE)
			   ++"_advise.fil"),

    Line1 = <<"Hello\n">>,
    Line2 = <<"World!\n">>,

    {ok, Fd} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:advise(Fd, 0, 0, normal),
    ok = ?PRIM_FILE:write(Fd, Line1),
    ok = ?PRIM_FILE:write(Fd, Line2),
    ok = ?PRIM_FILE:close(Fd),

    {ok, Fd2} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:advise(Fd2, 0, 0, random),
    ok = ?PRIM_FILE:write(Fd2, Line1),
    ok = ?PRIM_FILE:write(Fd2, Line2),
    ok = ?PRIM_FILE:close(Fd2),

    {ok, Fd3} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:advise(Fd3, 0, 0, sequential),
    ok = ?PRIM_FILE:write(Fd3, Line1),
    ok = ?PRIM_FILE:write(Fd3, Line2),
    ok = ?PRIM_FILE:close(Fd3),

    {ok, Fd4} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:advise(Fd4, 0, 0, will_need),
    ok = ?PRIM_FILE:write(Fd4, Line1),
    ok = ?PRIM_FILE:write(Fd4, Line2),
    ok = ?PRIM_FILE:close(Fd4),

    {ok, Fd5} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:advise(Fd5, 0, 0, dont_need),
    ok = ?PRIM_FILE:write(Fd5, Line1),
    ok = ?PRIM_FILE:write(Fd5, Line2),
    ok = ?PRIM_FILE:close(Fd5),

    {ok, Fd6} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:advise(Fd6, 0, 0, no_reuse),
    ok = ?PRIM_FILE:write(Fd6, Line1),
    ok = ?PRIM_FILE:write(Fd6, Line2),
    ok = ?PRIM_FILE:close(Fd6),

    {ok, Fd7} = ?PRIM_FILE:open(Advise, [write]),
    {error, einval} = ?PRIM_FILE:advise(Fd7, 0, 0, bad_advise),
    ok = ?PRIM_FILE:close(Fd7),

    %% test write without advise, then a read after an advise
    {ok, Fd8} = ?PRIM_FILE:open(Advise, [write]),
    ok = ?PRIM_FILE:write(Fd8, Line1),
    ok = ?PRIM_FILE:write(Fd8, Line2),
    ok = ?PRIM_FILE:close(Fd8),
    {ok, Fd9} = ?PRIM_FILE:open(Advise, [read]),
    Offset = 0,
    %% same as a 0 length in some implementations
    Length = byte_size(Line1) + byte_size(Line2),
    ok = ?PRIM_FILE:advise(Fd9, Offset, Length, sequential),
    {ok, Line1} = ?PRIM_FILE:read_line(Fd9),
    {ok, Line2} = ?PRIM_FILE:read_line(Fd9),
    eof = ?PRIM_FILE:read_line(Fd9),
    ok = ?PRIM_FILE:close(Fd9),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

large_write(Config) when is_list(Config) ->
    run_large_file_test(Config,
			fun(Name) -> do_large_write(Name) end,
			"_large_write").

do_large_write(Name) ->
    ChunkSize = (256 bsl 20) + 1,	% 256 M + 1
    Chunks = 16,			% times 16 -> 4 G + 16
    Base = 100,
    Interleave = lists:seq(Base+1, Base+Chunks),
    Chunk = <<0:ChunkSize/unit:8>>,
    Data = zip_data(lists:duplicate(Chunks, Chunk), Interleave),
    Size = Chunks * ChunkSize + Chunks,	% 4 G + 32
    ok = ?PRIM_FILE:write_file(Name, Data),
    {ok,#file_info{size=Size}} = file:read_file_info(Name),
    {ok,Fd} = ?PRIM_FILE:open(Name, [read]),
    check_large_write(Fd, ChunkSize, 0, Interleave).

check_large_write(Fd, ChunkSize, Pos, [X|Interleave]) ->
    Pos1 = Pos + ChunkSize,
    {ok,Pos1} = ?PRIM_FILE:position(Fd, {cur,ChunkSize}),
    {ok,<<X>>} = ?PRIM_FILE:read(Fd, 1),
    check_large_write(Fd, ChunkSize, Pos1+1, Interleave);
check_large_write(Fd, _, _, []) ->
    eof = ?PRIM_FILE:read(Fd, 1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests that ?PRIM_FILE:allocate/3 at least doesn't crash.
allocate(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Allocate = filename:join(PrivDir,
			     atom_to_list(?MODULE)
			     ++"_allocate.fil"),

    Line1 = "Hello\n",
    Line2 = "World!\n",

    {ok, Fd} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd, 1, iolist_size([Line1, Line2])),
    ok = ?PRIM_FILE:write(Fd, Line1),
    ok = ?PRIM_FILE:write(Fd, Line2),
    ok = ?PRIM_FILE:close(Fd),

    {ok, Fd2} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd2, 1, iolist_size(Line1)),
    ok = ?PRIM_FILE:write(Fd2, Line1),
    ok = ?PRIM_FILE:write(Fd2, Line2),
    ok = ?PRIM_FILE:close(Fd2),

    {ok, Fd3} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd3, 1, iolist_size(Line1) + 1),
    ok = ?PRIM_FILE:write(Fd3, Line1),
    ok = ?PRIM_FILE:write(Fd3, Line2),
    ok = ?PRIM_FILE:close(Fd3),

    {ok, Fd4} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd4, 1, 4 * iolist_size([Line1, Line2])),
    ok = ?PRIM_FILE:write(Fd4, Line1),
    ok = ?PRIM_FILE:write(Fd4, Line2),
    ok = ?PRIM_FILE:close(Fd4),

    ok.

allocate_and_assert(Fd, Offset, Length) ->
    %% Just verify that calls to ?PRIM_FILE:allocate/3 don't crash or have
    %% any other negative side effect. We can't really asssert against a
    %% specific return value, because support for file space pre-allocation
    %% depends on the OS, OS version and underlying filesystem.
    %%
    %% The Linux kernel added support for fallocate() in version 2.6.23,
    %% which currently works only for the ext4, ocfs2, xfs and btrfs file
    %% systems. posix_fallocate() is available in glibc as of version
    %% 2.1.94, but it was buggy until glibc version 2.7.
    %%
    %% Mac OS X, as of version 10.3, supports the fcntl operation F_PREALLOCATE.
    %%
    %% Solaris supports posix_fallocate() but only for the UFS file system
    %% apparently (not supported for ZFS).
    %%
    %% FreeBSD 9.0 is the first FreeBSD release supporting posix_fallocate().
    %%
    %% For Windows there's apparently no way to pre-allocate file space, at
    %% least with similar API/semantics as posix_fallocate(), fallocate() or
    %% fcntl F_PREALLOCATE.
    Result = ?PRIM_FILE:allocate(Fd, Offset, Length),
    case os:type() of
        {win32, _} ->
            {error, enotsup} = Result;
        _ ->
            _ = Result
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_delete.fil"),
    {ok, Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?PRIM_FILE:write(Fd1,"ok.\n"),
    ok = ?PRIM_FILE:close(Fd1),
    %% Check that the file is readable
    {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),
    ok = ?PRIM_FILE:close(Fd2),
    ok = ?PRIM_FILE:delete(Name),
    %% Check that the file is not readable anymore
    {error, _} = ?PRIM_FILE:open(Name, [read]),
    %% Try deleting a nonexistent file
    {error, enoent} = ?PRIM_FILE:delete(Name),
    ok.

rename(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    FileName1 = atom_to_list(?MODULE)++"_rename.fil",
    FileName2 = atom_to_list(?MODULE)++"_rename.ful",
    Name1 = filename:join(RootDir, FileName1),
    Name2 = filename:join(RootDir, FileName2),
    {ok,Fd1} = ?PRIM_FILE:open(Name1, [write]),
    ok = ?PRIM_FILE:close(Fd1),
    %% Rename, and check that it really changed name
    ok = ?PRIM_FILE:rename(Name1, Name2),
    {error, _} = ?PRIM_FILE:open(Name1, [read]),
    {ok, Fd2} = ?PRIM_FILE:open(Name2, [read]),
    ok = ?PRIM_FILE:close(Fd2),
    %% Try renaming something to itself
    ok = ?PRIM_FILE:rename(Name2, Name2),
    %% Try renaming something that doesn't exist
    {error, enoent} =
	?PRIM_FILE:rename(Name1, Name2),
    %% Try renaming to something else than a string
    {error, badarg} =
	?PRIM_FILE:rename(Name1, foobar),

    %% Move between directories
    DirName1 = filename:join(RootDir,
			     atom_to_list(?MODULE)
			     ++"_rename_dir"),
    DirName2 = filename:join(RootDir,
			     atom_to_list(?MODULE)
			     ++"_second_rename_dir"),
    Name1foo = filename:join(DirName1, "foo.fil"),
    Name2foo = filename:join(DirName2, "foo.fil"),
    Name2bar = filename:join(DirName2, "bar.dir"),
    ok = ?PRIM_FILE:make_dir(DirName1),
    %% The name has to include the full file name, path is not enough
    expect(
      {error, eexist}, {error, eisdir},
      ?PRIM_FILE:rename(Name2, DirName1)),
    ok =
	?PRIM_FILE:rename(Name2, Name1foo),
    %% Now rename the directory
    ok = ?PRIM_FILE:rename(DirName1, DirName2),
    %% And check that the file is there now
    {ok,Fd3} = ?PRIM_FILE:open(Name2foo, [read]),
    ok = ?PRIM_FILE:close(Fd3),
    %% Try some dirty things now: move the directory into itself
    {error, Msg1} =
	?PRIM_FILE:rename(DirName2, Name2bar),
    io:format("Errmsg1: ~p",[Msg1]),
    %% move dir into a file in itself
    {error, Msg2} =
	?PRIM_FILE:rename(DirName2, Name2foo),
    io:format("Errmsg2: ~p",[Msg2]),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


e_delete(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_e_delete"),
    ok = ?PRIM_FILE:make_dir(Base),

    %% Delete a non-existing file.
    {error, enoent} =
	?PRIM_FILE:delete(filename:join(Base, "non_existing")),

    %% Delete a directory.
    {error, eperm} = ?PRIM_FILE:delete(Base),

    %% Use a path-name with a non-directory component.
    Afile = filename:join(Base, "a_file"),
    ok = ?PRIM_FILE:write_file(Afile, "hello\n"),
    {error, E} =
	expect(
	  {error, enotdir}, {error, enoent}, 
	  ?PRIM_FILE:delete(filename:join(Afile, "another_file"))),
    io:format("Result: ~p~n", [E]),

    %% No permission.
    case os:type() of
	{win32, _} ->
	    %% Remove a character device.
	    {error, eacces} = ?PRIM_FILE:delete("nul");
	_ ->
	    ?PRIM_FILE:write_file_info(
	       Base, #file_info {mode=0}),
	    {error, eacces} = ?PRIM_FILE:delete(Afile),
	    ?PRIM_FILE:write_file_info(
	       Base, #file_info {mode=8#700})
    end,

    ok.

%%% FreeBSD gives EEXIST when renaming a file to an empty dir, although the
%%% manual page can be interpreted as saying that EISDIR should be given.
%%% (What about FreeBSD? We store our nightly build results on a FreeBSD
%%% file system, that's what.)

e_rename(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_e_rename"),
    ok = ?PRIM_FILE:make_dir(Base),

    %% Create an empty directory.
    EmptyDir = filename:join(Base, "empty_dir"),
    ok = ?PRIM_FILE:make_dir(EmptyDir),

    %% Create a non-empty directory.
    NonEmptyDir = filename:join(Base, "non_empty_dir"),
    ok = ?PRIM_FILE:make_dir(NonEmptyDir),
    ok = ?PRIM_FILE:write_file(
	    filename:join(NonEmptyDir, "a_file"),
	    "hello\n"),

    %% Create another non-empty directory.
    ADirectory = filename:join(Base, "a_directory"),
    ok = ?PRIM_FILE:make_dir(ADirectory),
    ok = ?PRIM_FILE:write_file(
	    filename:join(ADirectory, "a_file"),
	    "howdy\n\n"),

    %% Create a data file.
    File = filename:join(Base, "just_a_file"),
    ok = ?PRIM_FILE:write_file(File, "anything goes\n\n"),

    %% Move an existing directory to a non-empty directory.
    {error, eexist} =
	?PRIM_FILE:rename(ADirectory, NonEmptyDir),

    %% Move a root directory.
    {error, einval} = ?PRIM_FILE:rename("/", "arne"),

    %% Move Base into Base/new_name.
    {error, einval} =
	?PRIM_FILE:rename(Base, filename:join(Base, "new_name")),

    %% Overwrite a directory with a file.
    expect({error, eexist}, % FreeBSD (?)
	   {error, eisdir},
	   ?PRIM_FILE:rename(File, EmptyDir)),
    expect({error, eexist}, % FreeBSD (?)
	   {error, eisdir},
	   ?PRIM_FILE:rename(File, NonEmptyDir)),

    %% Move a non-existing file.
    NonExistingFile = filename:join(
			Base, "non_existing_file"),
    {error, enoent} =
	?PRIM_FILE:rename(NonExistingFile, NonEmptyDir),

    %% Overwrite a file with a directory.
    expect({error, eexist}, % FreeBSD (?)
	   {error, enotdir},
	   ?PRIM_FILE:rename(ADirectory, File)),

    %% Move a file to another filesystem.
    %% XXX - This test case is bogus. We cannot be guaranteed that
    %%       the source and destination are on 
    %%       different filesystems.
    %%
    %% XXX - Gross hack!
    Comment =
	case os:type() of
	    {win32, _} ->
		%% At least Windows NT can
		%% successfully move a file to
		%% another drive.
		ok;
	    _ ->
		OtherFs = "/tmp",
		NameOnOtherFs =
		    filename:join(OtherFs,
				  filename:basename(File)),
		{ok, Com} =
		    case ?PRIM_FILE:rename(
			    File, NameOnOtherFs) of
			{error, exdev} ->
			    %% The file could be in
			    %% the same filesystem!
			    {ok, ok};
			ok ->
			    {ok, {comment,
				  "Moving between filesystems "
				  "suceeded, files are probably "
				  "in the same filesystem!"}};
			{error, eperm} ->
			    {ok, {comment, "SBS! You don't "
				  "have the permission to do "
				  "this test!"}};
			Else ->
			    Else
		    end,
		Com
	end,
    Comment.

e_make_dir(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_e_make_dir"),
    ok = ?PRIM_FILE:make_dir(Base),

    %% A component of the path does not exist.
    {error, enoent} =
	?PRIM_FILE:make_dir(filename:join([Base, "a", "b"])),

    %% Use a path-name with a non-directory component.
    Afile = filename:join(Base, "a_directory"),
    ok = ?PRIM_FILE:write_file(Afile, "hello\n"),
    case ?PRIM_FILE:make_dir(
	    filename:join(Afile, "another_directory")) of
	{error, enotdir} -> io:format("Result: enotdir");
	{error, enoent} -> io:format("Result: enoent")
    end,

    %% No permission (on Unix only).
    case os:type() of
	{win32, _} ->
	    ok;
	_ ->
	    ?PRIM_FILE:write_file_info(Base, #file_info {mode=0}),
	    {error, eacces} =
		?PRIM_FILE:make_dir(filename:join(Base, "xxxx")),
	    ?PRIM_FILE:write_file_info(Base, #file_info {mode=8#700})
    end,
    ok.

e_del_dir(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_e_del_dir"),
    io:format("Base: ~p", [Base]),
    ok = ?PRIM_FILE:make_dir(Base),

    %% Delete a non-existent directory.
    {error, enoent} =
	?PRIM_FILE:del_dir(filename:join(Base, "non_existing")),

    %% Use a path-name with a non-directory component.
    Afile = filename:join(Base, "a_directory"),
    ok = ?PRIM_FILE:write_file(Afile, "hello\n"),
    {error, E1} =
	expect({error, enotdir}, {error, enoent},
	       ?PRIM_FILE:del_dir(
		  filename:join(Afile, "another_directory"))),
    io:format("Result: ~p", [E1]),

    %% Delete a non-empty directory.
    %% Delete a non-empty directory.
    {error, E2} =
	expect({error, enotempty}, {error, eexist}, {error, eacces},
	       ?PRIM_FILE:del_dir(Base)),
    io:format("Result: ~p", [E2]),

    %% Remove the current directory.
    {error, E3} =
	expect({error, einval}, 
	       {error, eperm}, % Linux and DUX
	       {error, eacces},
	       {error, ebusy},
	       ?PRIM_FILE:del_dir(".")),
    io:format("Result: ~p", [E3]),

    %% No permission.
    case os:type() of
	{win32, _} ->
	    ok;
	_ ->
	    ADirectory = filename:join(Base, "no_perm"),
	    ok = ?PRIM_FILE:make_dir(ADirectory),
	    ?PRIM_FILE:write_file_info(Base, #file_info {mode=0}),
	    {error, eacces} = ?PRIM_FILE:del_dir(ADirectory),
	    ?PRIM_FILE:write_file_info(
	       Base, #file_info {mode=8#700})
    end,
    ok.


make_link(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_make_link"),
    ok = ?PRIM_FILE:make_dir(NewDir),

    Name = filename:join(NewDir, "a_file"),
    ok = ?PRIM_FILE:write_file(Name, "some contents\n"),

    Alias = filename:join(NewDir, "an_alias"),
    Result =
	case ?PRIM_FILE:make_link(Name, Alias) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    ok ->
		%% Note: We take the opportunity to test 
		%% ?PRIM_FILE:read_link_info/1,
		%% which should in behave exactly as 
		%% ?PRIM_FILE:read_file_info/1
		%% since they are not used on symbolic links.

		{ok, Info} =
		    ?PRIM_FILE:read_link_info(Name),
		{ok, Info} =
		    ?PRIM_FILE:read_link_info(Alias),
		#file_info{links = 2, type = regular} = Info,
		{error, eexist} =
		    ?PRIM_FILE:make_link(Name, Alias),
		ok
	end,

    Result.

%% Test that reading link info for an ordinary file or directory works
%% (on all platforms).
read_link_info_for_non_link(Config) when is_list(Config) ->
    {ok, #file_info{type=directory}} = ?PRIM_FILE:read_link_info("."),
    ok.

symlinks(Config) when is_list(Config)  ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_make_symlink"),
    ok = ?PRIM_FILE:make_dir(NewDir),

    Name = filename:join(NewDir, "a_plain_file"),
    ok = ?PRIM_FILE:write_file(Name, "some stupid content\n"),

    Alias = filename:join(NewDir, "a_symlink_alias"),
    Result =
	case ?PRIM_FILE:make_symlink(Name, Alias) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    {error, eperm} ->
		{win32,_} = os:type(),
		{skipped, "Windows user not privileged to create links"};
	    ok ->
		{ok, Info1} =
		    ?PRIM_FILE:read_file_info(Name),
		{ok, Info1} =
		    ?PRIM_FILE:read_file_info(Alias),
		{ok, Info1} =
		    ?PRIM_FILE:read_link_info(Name),
		#file_info{links = 1, type = regular} = Info1,

		{ok, Info2} =
		    ?PRIM_FILE:read_link_info(Alias),
		#file_info{links=1, type=symlink} = Info2,
		{ok, Name} =
		    ?PRIM_FILE:read_link(Alias),
		{ok, Name} =
		    ?PRIM_FILE:read_link_all(Alias),
		%% If all is good, delete dir again (avoid hanging dir on windows)
		rm_rf(?PRIM_FILE,NewDir),
		ok
	end,

    Result.

%% Creates as many files as possible during a certain time, 
%% periodically calls list_dir/2 to check if it works,
%% then deletes all files.

%% Tests if large directories can be read.
list_dir_limit(Config) when is_list(Config) ->
    MaxTime = 120,
    MaxNumber = 20000,
    ct:timetrap({seconds,2*MaxTime + MaxTime}),
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)++"_list_dir_limit"),
    ok = ?PRIM_FILE:make_dir(NewDir),
    Ref = erlang:start_timer(MaxTime*1000, self(), []),
    Result = list_dir_limit_loop(NewDir, Ref, MaxNumber, 0),
    Time = case erlang:cancel_timer(Ref) of
	       false -> MaxTime;
	       T -> MaxTime - (T div 1000)
	   end,
    Number = case Result of
		 {ok, N} -> N;
		 {error, _Reason, N} -> N;
		 _ -> 0
	     end,
    list_dir_limit_cleanup(NewDir, Number, 0),
    {ok, Number} = Result,
    {comment, 
     "Created " ++ integer_to_list(Number) ++ " files in " 
     ++ integer_to_list(Time) ++ " seconds."}.

list_dir_limit_loop(Dir, _Ref, N, Cnt) when Cnt >= N ->
    list_dir_check(Dir, Cnt);
list_dir_limit_loop(Dir, Ref, N, Cnt) ->
    receive 
	{timeout, Ref, []} -> 
	    list_dir_check(Dir, Cnt)
    after 0 ->
	    Name = integer_to_list(Cnt),
	    case ?PRIM_FILE:write_file(filename:join(Dir, Name), Name) of
		ok ->
		    Next = Cnt + 1,
		    case Cnt rem 100 of
			0 ->
			    case list_dir_check(Dir, Next) of
				{ok, Next} ->
				    list_dir_limit_loop(
				      Dir, Ref, N, Next);
				Other ->
				    Other
			    end;
			_ ->
			    list_dir_limit_loop(Dir, Ref, N, Next)
		    end;
		{error, Reason} ->
		    {error, Reason, Cnt}
	    end
    end.

list_dir_check(Dir, Cnt) ->
    case ?PRIM_FILE:list_dir(Dir) of
	{ok, ListDir} ->
	    case length(ListDir) of
		Cnt ->
		    {ok, Cnt};
		X ->
		    {error, 
		     {wrong_nof_files, X, ?LINE},
		     Cnt}
	    end;
	{error, Reason} ->
	    {error, Reason, Cnt}
    end.

%% Deletes N files while ignoring errors, then continues deleting
%% as long as they exist.

list_dir_limit_cleanup(Dir, N, Cnt) when Cnt >= N ->
    Name = integer_to_list(Cnt),
    case ?PRIM_FILE:delete(filename:join(Dir, Name)) of
	ok ->
	    list_dir_limit_cleanup(Dir, N, Cnt+1);
	_ ->
	    ok
    end;
list_dir_limit_cleanup(Dir, N, Cnt) ->
    Name = integer_to_list(Cnt),
    ?PRIM_FILE:delete(filename:join(Dir, Name)),
    list_dir_limit_cleanup(Dir, N, Cnt+1).

%%%
%%% Test list_dir() on a non-existing pathname.
%%%

list_dir_error(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    NonExisting = filename:join(Priv, "non-existing-dir"),
    {error,enoent} = ?PRIM_FILE:list_dir(NonExisting),
    ok.

%%%
%%% Test list_dir() and list_dir_all().
%%%

list_dir(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    TestDir = filename:join(RootDir, ?MODULE_STRING++"_list_dir"),
    ?PRIM_FILE:make_dir(TestDir),
    list_dir_1(TestDir, 42, []).

list_dir_1(TestDir, 0, Sorted) ->
    [ok = ?PRIM_FILE:delete(filename:join(TestDir, F)) ||
	F <- Sorted],
    ok = ?PRIM_FILE:del_dir(TestDir);
list_dir_1(TestDir, Cnt, Sorted0) ->
    Base = "file" ++ integer_to_list(Cnt),
    Name = filename:join(TestDir, Base),
    ok = ?PRIM_FILE:write_file(Name, Base),
    Sorted = lists:merge([Base], Sorted0),
    {ok,DirList0} = ?PRIM_FILE:list_dir(TestDir),
    {ok,DirList1} = ?PRIM_FILE:list_dir_all(TestDir),
    Sorted = lists:sort(DirList0),
    Sorted = lists:sort(DirList1),
    list_dir_1(TestDir, Cnt-1, Sorted).

%%%
%%% Support for testing large files.
%%%

run_large_file_test(Config, Run, Name) ->
    case {os:type(),os:version()} of
	{{win32,nt},_} ->
	    do_run_large_file_test(Config, Run, Name);
	{{unix,sunos},OsVersion} when OsVersion < {5,5,1} ->
	    {skip,"Only supported on Win32, Unix or SunOS >= 5.5.1"};
	{{unix,_},_} ->
	    DiscFree = unix_free(proplists:get_value(priv_dir, Config)),
            MemFree = free_memory(),
	    io:format("Free disk: ~w KByte~n", [DiscFree]),
	    io:format("Free mem: ~w MByte~n", [MemFree]),
	    if DiscFree < 5 bsl 20; MemFree < 5 bsl 10 ->
		    %% Less than 5 GByte free
		    {skip,"Less than 5 GByte free disk/mem"};
	       true ->
		    do_run_large_file_test(Config, Run, Name)
	    end;
	_ -> 
	    {skip,"Only supported on Win32, Unix or SunOS >= 5.5.1"}
    end.


do_run_large_file_test(Config, Run, Name0) ->
    Name = filename:join(proplists:get_value(priv_dir, Config),
			 ?MODULE_STRING ++ Name0),

    %% Set up a process that will delete this file.
    Tester = self(),
    Deleter = 
	spawn(
	  fun() ->
		  Mref = erlang:monitor(process, Tester),
		  receive
		      {'DOWN',Mref,_,_,_} -> ok;
		      {Tester,done} -> ok
		  end,
		  ?PRIM_FILE:delete(Name)
	  end),

    %% Run the test case.
    Res = Run(Name),

    %% Delete file and finish deleter process.
    Mref = erlang:monitor(process, Deleter),
    Deleter ! {Tester,done},
    receive {'DOWN',Mref,_,_,_} -> ok end,

    Res.

unix_free(Path) ->
    Cmd = ["df -k '",Path,"'"],
    DF0 = os:cmd(Cmd),
    io:format("$ ~s~n~s", [Cmd,DF0]),
    Lines = re:split(DF0, "\n", [trim,{return,list}]),
    Last = lists:last(Lines),
    RE = "^[^\\s]*\\s+\\d+\\s+\\d+\\s+(\\d+)",
    {match,[Avail]} = re:run(Last, RE, [{capture,all_but_first,list}]),
    list_to_integer(Avail).

zip_data([A|As], [B|Bs]) ->
    [[A,B]|zip_data(As, Bs)];
zip_data([], Bs) ->
    Bs;
zip_data(As, []) ->
    As.

%% Stolen from emulator -> alloc_SUITE
free_memory() ->
    %% Free memory in MB.
    try
	SMD = memsup:get_system_memory_data(),
	{value, {free_memory, Free}} = lists:keysearch(free_memory, 1, SMD),
	TotFree = (Free +
		   case lists:keysearch(cached_memory, 1, SMD) of
		       {value, {cached_memory, Cached}} -> Cached;
		       false -> 0
		   end +
		   case lists:keysearch(buffered_memory, 1, SMD) of
		       {value, {buffered_memory, Buffed}} -> Buffed;
		       false -> 0
		   end),
	usable_mem(TotFree) div (1024*1024)
    catch
	error : undef ->
	    ct:fail({"os_mon not built"})
    end.

usable_mem(Memory) ->
    case test_server:is_valgrind() of
        true ->
            %% Valgrind uses extra memory for the V- and A-bits.
            %% http://valgrind.org/docs/manual/mc-manual.html#mc-manual.value
            %% Docs says it uses "compression to represent the V bits compactly"
            %% but let's be conservative and cut usable memory in half.
            Memory div 2;
        false ->
            Memory
    end.


%%%-----------------------------------------------------------------
%%% Utilities
rm_rf(Mod,Dir) ->
    case  Mod:read_link_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok, Content} = Mod:list_dir_all(Dir),
	    [ rm_rf(Mod,filename:join(Dir,C)) || C <- Content ],
	    Mod:del_dir(Dir),
	    ok;
	{ok, #file_info{}} ->
	    Mod:delete(Dir);
	_ ->
	    ok
    end.
