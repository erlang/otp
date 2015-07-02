%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2014. All Rights Reserved.
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
	 init_per_group/2,end_per_group/2,
	 read_write_file/1]).
-export([cur_dir_0a/1, cur_dir_0b/1, 
	 cur_dir_1a/1, cur_dir_1b/1, 
	 make_del_dir_a/1, make_del_dir_b/1,
	 pos1/1, pos2/1]).
-export([close/1, 
	 delete_a/1, delete_b/1]).
-export([ open1/1, modes/1]).
-export([ 
	  file_info_basic_file_a/1, file_info_basic_file_b/1,
	  file_info_basic_directory_a/1, file_info_basic_directory_b/1,
	  file_info_bad_a/1, file_info_bad_b/1, 
	  file_info_times_a/1, file_info_times_b/1, 
	  file_write_file_info_a/1, file_write_file_info_b/1,
	  file_read_file_info_opts/1, file_write_file_info_opts/1,
	  file_write_read_file_info_opts/1
      ]).
-export([rename_a/1, rename_b/1, 
	 access/1, truncate/1, datasync/1, sync/1,
	 read_write/1, pread_write/1, append/1, exclusive/1]).
-export([ e_delete/1, e_rename/1, e_make_dir/1, e_del_dir/1]).

-export([ read_not_really_compressed/1,
	  read_compressed/1, write_compressed/1,
	  compress_errors/1]).

-export([ 
	  make_link_a/1, make_link_b/1,
	  read_link_info_for_non_link/1, 
	  symlinks_a/1, symlinks_b/1,
	  list_dir_limit/1,
	  list_dir_error/1,
	  list_dir/1]).

-export([advise/1]).
-export([large_write/1]).

%% System probe functions that might be handy to check from the shell
-export([unix_free/1]).

-export([allocate/1]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

-define(PRIM_FILE, prim_file).

%% Calls ?PRIM_FILE:F with arguments A and an optional handle H 
%% as first argument, unless the handle is [], i.e no handle.
%% This is a macro to give the compiler and thereby 
%% the cross reference tool the possibility to interprete
%% the call, since M, F, A (or [H | A]) can all be known at 
%% compile time.
-define(PRIM_FILE_call(F, H, A), 
	case H of
	    [] -> apply(?PRIM_FILE, F, A);
	    _ ->  apply(?PRIM_FILE, F, [H | A])
	end).

suite() -> [].

all() -> 
    [read_write_file, {group, dirs}, {group, files},
     delete_a, delete_b, rename_a, rename_b, {group, errors},
     {group, compression}, {group, links}, list_dir_limit, list_dir].

groups() -> 
    [{dirs, [],
      [make_del_dir_a, make_del_dir_b, cur_dir_0a, cur_dir_0b,
       cur_dir_1a, cur_dir_1b]},
     {files, [],
      [{group, open}, {group, pos}, {group, file_info},
       truncate, sync, datasync, advise, large_write, allocate]},
     {open, [],
      [open1, modes, close, access, read_write, pread_write,
       append, exclusive]},
     {pos, [], [pos1, pos2]},
     {file_info, [],
      [file_info_basic_file_a, file_info_basic_file_b,
       file_info_basic_directory_a,
       file_info_basic_directory_b, file_info_bad_a,
       file_info_bad_b, file_info_times_a, file_info_times_b,
       file_write_file_info_a, file_write_file_info_b,
       file_read_file_info_opts, file_write_file_info_opts,
       file_write_read_file_info_opts
   ]},
     {errors, [],
      [e_delete, e_rename, e_make_dir, e_del_dir]},
     {compression, [],
      [read_compressed, read_not_really_compressed,
       write_compressed, compress_errors]},
     {links, [],
      [make_link_a, make_link_b, read_link_info_for_non_link,
       symlinks_a, symlinks_b, list_dir_error]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    Priv = ?config(priv_dir, Config),
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

read_write_file(suite) -> [];
read_write_file(doc) -> [];
read_write_file(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_read_write_file"),

    %% Try writing and reading back some term
    ?line SomeTerm = {"This term",{will,be},[written,$t,$o],1,file,[]},
    ?line ok = ?PRIM_FILE:write_file(Name,term_to_binary(SomeTerm)),
    ?line {ok,Bin1} = ?PRIM_FILE:read_file(Name),
    ?line SomeTerm = binary_to_term(Bin1),
  
    %% Try a "null" term
    ?line NullTerm = [],
    ?line ok = ?PRIM_FILE:write_file(Name,term_to_binary(NullTerm)),
    ?line {ok,Bin2} = ?PRIM_FILE:read_file(Name),
    ?line NullTerm = binary_to_term(Bin2),

    %% Try some "complicated" types
    ?line BigNum = 123456789012345678901234567890,
    ?line ComplTerm = {self(),make_ref(),BigNum,3.14159},
    ?line ok = ?PRIM_FILE:write_file(Name,term_to_binary(ComplTerm)),
    ?line {ok,Bin3} = ?PRIM_FILE:read_file(Name),
    ?line ComplTerm = binary_to_term(Bin3),

    %% Try reading a nonexistent file
    ?line Name2 = filename:join(RootDir, 
				atom_to_list(?MODULE)
				++"_nonexistent_file"),
    ?line {error, enoent} = ?PRIM_FILE:read_file(Name2),
    ?line {error, enoent} = ?PRIM_FILE:read_file(""),

    % Try writing to a bad filename
    ?line {error, enoent} = 
	?PRIM_FILE:write_file("",term_to_binary(NullTerm)),

    % Try writing something else than a binary
    ?line {error, badarg} = ?PRIM_FILE:write_file(Name,{1,2,3}),
    ?line {error, badarg} = ?PRIM_FILE:write_file(Name,self()),

    %% Some non-term binaries
    ?line ok = ?PRIM_FILE:write_file(Name,[]),
    ?line {ok,Bin4} = ?PRIM_FILE:read_file(Name),
    ?line 0 = byte_size(Bin4),

    ?line ok = ?PRIM_FILE:write_file(Name,[Bin1,[],[[Bin2]]]),
    ?line {ok,Bin5} = ?PRIM_FILE:read_file(Name),
    ?line {Bin1,Bin2} = split_binary(Bin5,byte_size(Bin1)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_del_dir_a(suite) -> [];
make_del_dir_a(doc) -> [];
make_del_dir_a(Config) when is_list(Config) ->
    make_del_dir(Config, [], "_a").

make_del_dir_b(suite) -> [];
make_del_dir_b(doc) -> [];
make_del_dir_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = make_del_dir(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    %% Just to make sure the state of the server makes a difference
    ?line {error, einval} = ?PRIM_FILE_call(get_cwd, Handle, []),
    Result.

make_del_dir(Config, Handle, Suffix) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_mk-dir"++Suffix),
    ?line ok = ?PRIM_FILE_call(make_dir, Handle, [NewDir]),
    ?line {error, eexist} = ?PRIM_FILE_call(make_dir, Handle, [NewDir]),
    ?line ok = ?PRIM_FILE_call(del_dir, Handle, [NewDir]),
    ?line {error, enoent} = ?PRIM_FILE_call(del_dir, Handle, [NewDir]),

    % Make sure we are not in a directory directly under test_server
    % as that would result in eacces errors when trying to delete '..',
    % because there are processes having that directory as current.
    ?line ok = ?PRIM_FILE_call(make_dir, Handle, [NewDir]),
    ?line {ok, CurrentDir} = ?PRIM_FILE_call(get_cwd, Handle, []),
    case {os:type(), length(NewDir) >= 260 } of
	{{win32,_}, true} ->
	    io:format("Skip set_cwd for windows path longer than 260 (MAX_PATH)\n", []),
	    io:format("\nNewDir = ~p\n", [NewDir]);
	_ ->
	    ok = ?PRIM_FILE_call(set_cwd, Handle, [NewDir])
    end,
    try
	%% Check that we get an error when trying to create...
	%% a deep directory
	?line NewDir2 = filename:join(RootDir, 
				      atom_to_list(?MODULE)
				      ++"_mk-dir-noexist/foo"),
	?line {error, enoent} = ?PRIM_FILE_call(make_dir, Handle, [NewDir2]),
	%% a nameless directory
	?line {error, enoent} = ?PRIM_FILE_call(make_dir, Handle, [""]),
	%% a directory with illegal name
	?line {error, badarg} = ?PRIM_FILE_call(make_dir, Handle, ['mk-dir']),
	
	%% a directory with illegal name, even if it's a (bad) list
	?line {error, badarg} = ?PRIM_FILE_call(make_dir, Handle, [[1,2,3,{}]]),
	
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
	?line case ?PRIM_FILE_call(del_dir, Handle, [".."]) of
		  {error, eexist} -> ok;
		  {error, eacces} -> ok;	%OpenBSD
		  {error, einval} -> ok		%FreeBSD
	      end,
	?line {error, enoent} = ?PRIM_FILE_call(del_dir, Handle, [""]),
	?line {error, badarg} = ?PRIM_FILE_call(del_dir, Handle, [[3,2,1,{}]])
    after
	?line ok = ?PRIM_FILE_call(set_cwd, Handle, [CurrentDir])
    end,
    ok.

cur_dir_0a(suite) -> [];
cur_dir_0a(doc) -> [];
cur_dir_0a(Config) when is_list(Config) ->
    cur_dir_0(Config, []).

cur_dir_0b(suite) -> [];
cur_dir_0b(doc) -> [];
cur_dir_0b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = cur_dir_0(Config, Handle),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

cur_dir_0(Config, Handle) ->
    %% Find out the current dir, and cd to it ;-)
    ?line {ok,BaseDir} = ?PRIM_FILE_call(get_cwd, Handle, []),
    ?line Dir1 = BaseDir ++ "", %% Check that it's a string
    ?line ok = ?PRIM_FILE_call(set_cwd, Handle, [Dir1]),
    ?line DirName = atom_to_list(?MODULE) ++
	case Handle of
	    [] ->
		"_curdir";
	    _ ->
		"_curdir_h"
	end,

    %% Make a new dir, and cd to that
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, DirName),
    ?line ok = ?PRIM_FILE_call(make_dir, Handle, [NewDir]),
    case {os:type(), length(NewDir) >= 260} of
	{{win32,_}, true} ->
	    io:format("Skip set_cwd for windows path longer than 260 (MAX_PATH):\n"),
	    io:format("\nNewDir = ~p\n", [NewDir]);
	_ ->
	    io:format("cd to ~s",[NewDir]),
	    ok = ?PRIM_FILE_call(set_cwd, Handle, [NewDir]),

	    %% Create a file in the new current directory, and check that it
	    %% really is created there
	    UncommonName = "uncommon.fil",
	    {ok,Fd} = ?PRIM_FILE:open(UncommonName, [read, write]),
	    ok = ?PRIM_FILE:close(Fd),
	    {ok,NewDirFiles} = ?PRIM_FILE_call(list_dir, Handle, ["."]),
	    true = lists:member(UncommonName,NewDirFiles),

	    %% Delete the directory and return to the old current directory
	    %% and check that the created file isn't there (too!)
	    expect({error, einval}, {error, eacces}, {error, eexist},
		 ?PRIM_FILE_call(del_dir, Handle, [NewDir])),
	    ?PRIM_FILE_call(delete, Handle, [UncommonName]),
	    {ok,[]} = ?PRIM_FILE_call(list_dir, Handle, ["."]),
	    ok = ?PRIM_FILE_call(set_cwd, Handle, [Dir1]),
	    io:format("cd back to ~s",[Dir1]),
	    ok = ?PRIM_FILE_call(del_dir, Handle, [NewDir]),
	    {error, enoent} = ?PRIM_FILE_call(set_cwd, Handle, [NewDir]),
	    ok = ?PRIM_FILE_call(set_cwd, Handle, [Dir1]),
	    io:format("cd back to ~s",[Dir1]),
	    {ok,OldDirFiles} = ?PRIM_FILE_call(list_dir, Handle, ["."]),
	    false = lists:member(UncommonName,OldDirFiles)
    end,

    %% Try doing some bad things
    ?line {error, badarg} = 
	?PRIM_FILE_call(set_cwd, Handle, [{foo,bar}]),
    ?line {error, enoent} = 
	?PRIM_FILE_call(set_cwd, Handle, [""]),
    ?line {error, enoent} = 
	?PRIM_FILE_call(set_cwd, Handle, [".......a......"]),
    ?line {ok,BaseDir} = 
	?PRIM_FILE_call(get_cwd, Handle, []), %% Still there?

    %% On Windows, there should only be slashes, no backslashes,
    %% in the return value of get_cwd().
    %% (The test is harmless on Unix, because filenames usually
    %% don't contain backslashes.)

    ?line {ok, BaseDir} = ?PRIM_FILE_call(get_cwd, Handle, []),
    ?line false = lists:member($\\, BaseDir),

    ok.

%% Tests ?PRIM_FILE:get_cwd/1.

cur_dir_1a(suite) -> [];
cur_dir_1a(doc) -> [];
cur_dir_1a(Config) when is_list(Config) ->
    cur_dir_1(Config, []).

cur_dir_1b(suite) -> [];
cur_dir_1b(doc) -> [];
cur_dir_1b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = cur_dir_1(Config, Handle),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

cur_dir_1(Config, Handle) ->
    ?line case os:type() of
	      {win32, _} ->
		  win_cur_dir_1(Config, Handle);
	      _ ->
		  ?line {error, enotsup} =
		      ?PRIM_FILE_call(get_cwd, Handle, ["d:"])
	  end,
    ok.
	
win_cur_dir_1(_Config, Handle) ->
    ?line {ok, BaseDir} = ?PRIM_FILE_call(get_cwd, Handle, []),

    %% Get the drive letter from the current directory,
    %% and try to get current directory for that drive.

    ?line [Drive, $:|_] = BaseDir,
    ?line {ok, BaseDir} = ?PRIM_FILE_call(get_cwd, Handle, [[Drive, $:]]),
    io:format("BaseDir = ~s\n", [BaseDir]),

    %% Unfortunately, there is no way to move away from the
    %% current drive as we can't use the "subst" command from
    %% a SSH connection. We can't test any more. Too bad.

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



open1(suite) -> [];
open1(doc) -> [];
open1(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_files"),
    ?line ok = ?PRIM_FILE:make_dir(NewDir),
    ?line Name = filename:join(NewDir, "foo1.fil"),
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name, [read, write]),
    ?line {ok,Fd2} = ?PRIM_FILE:open(Name, [read]),
    ?line Str = "{a,tuple}.\n",
    ?line Length = length(Str),
    ?line ?PRIM_FILE:write(Fd1,Str),
    ?line {ok,0} = ?PRIM_FILE:position(Fd1,bof),
    ?line {ok, Str} = ?PRIM_FILE:read(Fd1,Length),
    ?line {ok, Str} = ?PRIM_FILE:read(Fd2,Length),
    ?line ok = ?PRIM_FILE:close(Fd2),
    ?line {ok,0} = ?PRIM_FILE:position(Fd1,bof),
    ?line ok = ?PRIM_FILE:truncate(Fd1),
    ?line eof = ?PRIM_FILE:read(Fd1,Length),
    ?line ok = ?PRIM_FILE:close(Fd1),
    ?line {ok,Fd3} = ?PRIM_FILE:open(Name, [read]),
    ?line eof = ?PRIM_FILE:read(Fd3,Length),
    ?line ok = ?PRIM_FILE:close(Fd3),
    ok.

%% Tests all open modes.

modes(suite) -> [];
modes(doc) -> [];
modes(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_open_modes"),
    ?line ok = ?PRIM_FILE:make_dir(NewDir),
    ?line Name1 = filename:join(NewDir, "foo1.fil"),
    ?line Marker = "hello, world",
    ?line Length = length(Marker),

    %% write
    ?line {ok, Fd1} = ?PRIM_FILE:open(Name1, [write]),
    ?line ok = ?PRIM_FILE:write(Fd1, Marker),
    ?line ok = ?PRIM_FILE:write(Fd1, ".\n"),
    ?line ok = ?PRIM_FILE:close(Fd1),

    %% read
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name1, [read]),
    ?line {ok, Marker} = ?PRIM_FILE:read(Fd2, Length),
    ?line ok = ?PRIM_FILE:close(Fd2),

    %% read and write
    ?line {ok, Fd3} = ?PRIM_FILE:open(Name1, [read, write]),
    ?line {ok, Marker} = ?PRIM_FILE:read(Fd3, Length),
    ?line ok = ?PRIM_FILE:write(Fd3, Marker),
    ?line ok = ?PRIM_FILE:close(Fd3),

    %% read by default
    ?line {ok, Fd4} = ?PRIM_FILE:open(Name1, []),
    ?line {ok, Marker} = ?PRIM_FILE:read(Fd4, Length),
    ?line ok = ?PRIM_FILE:close(Fd4),

    %% read and binary
    ?line BinaryMarker = list_to_binary(Marker),
    ?line {ok, Fd5} = ?PRIM_FILE:open(Name1, [read, binary]),
    ?line {ok, BinaryMarker} = ?PRIM_FILE:read(Fd5, Length),
    ?line ok = ?PRIM_FILE:close(Fd5),

    ?line test_server:timetrap_cancel(Dog),
    ok.

close(suite) -> [];
close(doc) -> [];
close(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_close.fil"),
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name, [read, write]),
    %% Just closing it is no fun, we did that a million times already
    %% This is a common error, for code written before Erlang 4.3
    %% bacause then ?PRIM_FILE:open just returned a Pid, and not everyone
    %% really checked what they got.
    ?line {'EXIT',_Msg} = (catch ok = ?PRIM_FILE:close({ok,Fd1})),
    ?line ok = ?PRIM_FILE:close(Fd1),

    %% Try closing one more time
    ?line Val = ?PRIM_FILE:close(Fd1),
    ?line io:format("Second close gave: ~p", [Val]),

    ok.

access(suite) -> [];
access(doc) -> [];
access(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_access.fil"),
    ?line Str = "ABCDEFGH",
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?line ?PRIM_FILE:write(Fd1,Str),
    ?line ok = ?PRIM_FILE:close(Fd1),
    %% Check that we can't write when in read only mode
    ?line {ok,Fd2} = ?PRIM_FILE:open(Name, [read]),
    ?line case catch ?PRIM_FILE:write(Fd2,"XXXX") of
	      ok ->
		  test_server:fail({access,write});
	      _ ->
		  ok
	  end,
    ?line ok = ?PRIM_FILE:close(Fd2),
    ?line {ok, Fd3} = ?PRIM_FILE:open(Name, [read]),
    ?line {ok, Str} = ?PRIM_FILE:read(Fd3,length(Str)),
    ?line ok = ?PRIM_FILE:close(Fd3),

    ok.

%% Tests ?PRIM_FILE:read/2 and ?PRIM_FILE:write/2.

read_write(suite) -> [];
read_write(doc) -> [];
read_write(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_read_write"),
    ?line ok = ?PRIM_FILE:make_dir(NewDir),

    %% Raw file.
    ?line Name = filename:join(NewDir, "raw.fil"),
    ?line {ok, Fd} = ?PRIM_FILE:open(Name, [read, write]),
    ?line read_write_test(Fd),

    ok.

read_write_test(File) ->
    ?line Marker = "hello, world",
    ?line ok = ?PRIM_FILE:write(File, Marker),
    ?line {ok, 0} = ?PRIM_FILE:position(File, 0),
    ?line {ok, Marker} = ?PRIM_FILE:read(File, 100),
    ?line eof = ?PRIM_FILE:read(File, 100),
    ?line ok = ?PRIM_FILE:close(File),
    ok.


%% Tests ?PRIM_FILE:pread/2 and ?PRIM_FILE:pwrite/2.

pread_write(suite) -> [];
pread_write(doc) -> [];
pread_write(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_pread_write"),
    ?line ok = ?PRIM_FILE:make_dir(NewDir),

    %% Raw file.
    ?line Name = filename:join(NewDir, "raw.fil"),
    ?line {ok, Fd} = ?PRIM_FILE:open(Name, [read, write]),
    ?line pread_write_test(Fd),

    ok.

pread_write_test(File) ->
    ?line Marker = "hello, world",
    ?line Len = length(Marker),
    ?line ok = ?PRIM_FILE:write(File, Marker),
    ?line {ok, Marker} = ?PRIM_FILE:pread(File, 0, 100),
    ?line eof = ?PRIM_FILE:pread(File, 100, 1),
    ?line ok = ?PRIM_FILE:pwrite(File, Len, Marker),
    ?line {ok, Marker} = ?PRIM_FILE:pread(File, Len, 100),
    ?line eof = ?PRIM_FILE:pread(File, 100, 1),
    ?line MM = Marker ++ Marker,
    ?line {ok, MM} = ?PRIM_FILE:pread(File, 0, 100),
    ?line ok = ?PRIM_FILE:close(File),
    ok.

append(doc) -> "Test appending to a file.";
append(suite) -> [];
append(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_append"),
    ?line ok = ?PRIM_FILE:make_dir(NewDir),

    ?line First = "First line\n",
    ?line Second = "Seond lines comes here\n",
    ?line Third = "And here is the third line\n",

    %% Write a small text file.
    ?line Name1 = filename:join(NewDir, "a_file.txt"),
    ?line {ok, Fd1} = ?PRIM_FILE:open(Name1, [write]),
    ?line ok = ?PRIM_FILE:write(Fd1, First),
    ?line ok = ?PRIM_FILE:write(Fd1, Second),
    ?line ok = ?PRIM_FILE:close(Fd1),

    %% Open it a again and a append a line to it.
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name1, [append]),
    ?line ok = ?PRIM_FILE:write(Fd2, Third),
    ?line ok = ?PRIM_FILE:close(Fd2),

    %% Read it back and verify.
    ?line Expected = list_to_binary([First, Second, Third]),
    ?line {ok, Expected} = ?PRIM_FILE:read_file(Name1),

    ok.

exclusive(suite) -> [];
exclusive(doc) -> "Test exclusive access to a file.";
exclusive(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir,
				 atom_to_list(?MODULE)
				 ++"_exclusive"),
    ?line ok = ?PRIM_FILE:make_dir(NewDir),
    ?line Name = filename:join(NewDir, "ex_file.txt"),
    ?line {ok,Fd} = ?PRIM_FILE:open(Name, [write, exclusive]),
    ?line {error, eexist} = ?PRIM_FILE:open(Name, [write, exclusive]),
    ?line ok = ?PRIM_FILE:close(Fd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pos1(suite) -> [];
pos1(doc) -> [];
pos1(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_pos1.fil"),
    ?line {ok, Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?line ?PRIM_FILE:write(Fd1,"ABCDEFGH"),
    ?line ok        = ?PRIM_FILE:close(Fd1),
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),

    %% Start pos is first char
    ?line io:format("Relative positions"),
    ?line {ok, "A"} = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 2}   = ?PRIM_FILE:position(Fd2,{cur,1}), 
    ?line {ok, "C"} = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 0}   = ?PRIM_FILE:position(Fd2,{cur,-3}), 
    ?line {ok, "A"} = ?PRIM_FILE:read(Fd2,1),
    %% Backwards from first char should be an error
    ?line {ok,0}    = ?PRIM_FILE:position(Fd2,{cur,-1}),
    ?line {error, einval} = ?PRIM_FILE:position(Fd2,{cur,-1}),
    %% Reset position and move again
    ?line {ok, 0}   = ?PRIM_FILE:position(Fd2,0),
    ?line {ok, 2}   = ?PRIM_FILE:position(Fd2,{cur,2}), 
    ?line {ok, "C"} = ?PRIM_FILE:read(Fd2,1),
    %% Go a lot forwards
    ?line {ok, 13}  = ?PRIM_FILE:position(Fd2,{cur,10}), 
    ?line eof       = ?PRIM_FILE:read(Fd2,1),

    %% Try some fixed positions
    ?line io:format("Fixed positions"),
    ?line {ok, 8}   = ?PRIM_FILE:position(Fd2,8), 
    ?line eof = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 8}   = ?PRIM_FILE:position(Fd2,cur), 
    ?line eof = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 7}   = ?PRIM_FILE:position(Fd2,7), 
    ?line {ok, "H"} = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 0}   = ?PRIM_FILE:position(Fd2,0), 
    ?line {ok, "A"} = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 3}   = ?PRIM_FILE:position(Fd2,3), 
    ?line {ok, "D"} = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 12}  = ?PRIM_FILE:position(Fd2,12), 
    ?line eof       = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 3}   = ?PRIM_FILE:position(Fd2,3), 
    ?line {ok, "D"} = ?PRIM_FILE:read(Fd2,1),
    %% Try the {bof,X} notation
    ?line {ok, 3}   = ?PRIM_FILE:position(Fd2,{bof,3}),
    ?line {ok, "D"} = ?PRIM_FILE:read(Fd2,1),

    %% Try eof positions
    ?line io:format("EOF positions"),
    ?line {ok, 8}   = ?PRIM_FILE:position(Fd2,{eof,0}), 
    ?line eof       = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 7}   = ?PRIM_FILE:position(Fd2,{eof,-1}),
    ?line {ok, "H"} = ?PRIM_FILE:read(Fd2,1),
    ?line {ok, 0}   = ?PRIM_FILE:position(Fd2,{eof,-8}), 
    ?line {ok, "A"} = ?PRIM_FILE:read(Fd2,1),
    ?line {error, einval} = ?PRIM_FILE:position(Fd2,{eof,-9}),
    ok.

pos2(suite) -> [];
pos2(doc) -> [];
pos2(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_pos2.fil"),
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?line ?PRIM_FILE:write(Fd1,"ABCDEFGH"),
    ?line ok = ?PRIM_FILE:close(Fd1),
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),
    ?line {error, einval} = ?PRIM_FILE:position(Fd2,-1),

    %% Make sure that we still can search after an error.
    ?line {ok, 0}   = ?PRIM_FILE:position(Fd2, 0),
    ?line {ok, 3}   = ?PRIM_FILE:position(Fd2, {bof,3}),
    ?line {ok, "D"} = ?PRIM_FILE:read(Fd2,1),

    ?line io:format("DONE"),
    ok.


file_info_basic_file_a(suite) -> [];
file_info_basic_file_a(doc) -> [];
file_info_basic_file_a(Config) when is_list(Config) ->
    file_info_basic_file(Config, [], "_a").

file_info_basic_file_b(suite) -> [];
file_info_basic_file_b(doc) -> [];
file_info_basic_file_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = file_info_basic_file(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

file_info_basic_file(Config, Handle, Suffix) ->
    ?line RootDir = ?config(priv_dir, Config),

    %% Create a short file.
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_basic_test"++Suffix++".fil"),
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?line ?PRIM_FILE:write(Fd1, "foo bar"),
    ?line ok = ?PRIM_FILE:close(Fd1),

    %% Test that the file has the expected attributes.
    %% The times are tricky, so we will save them to a separate test case.
    ?line {ok, FileInfo} = ?PRIM_FILE_call(read_file_info, Handle, [Name]),
    ?line #file_info{size = Size, type = Type, access = Access,
		     atime = AccessTime, mtime = ModifyTime} = 
	FileInfo,
    ?line io:format("Access ~p, Modify ~p", [AccessTime, ModifyTime]),
    ?line Size = 7,
    ?line Type = regular,
    ?line Access = read_write,
    ?line true = abs(time_dist(filter_atime(AccessTime, Config),
			       filter_atime(ModifyTime,
					    Config))) < 2,
    ?line {AD, AT} = AccessTime,
    ?line all_integers(tuple_to_list(AD) ++ tuple_to_list(AT)),
    ?line {MD, MT} = ModifyTime,
    ?line all_integers(tuple_to_list(MD) ++ tuple_to_list(MT)),

    ok.

file_info_basic_directory_a(suite) -> [];
file_info_basic_directory_a(doc) -> [];
file_info_basic_directory_a(Config) when is_list(Config) ->
    file_info_basic_directory(Config, []).

file_info_basic_directory_b(suite) -> [];
file_info_basic_directory_b(doc) -> [];
file_info_basic_directory_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = file_info_basic_directory(Config, Handle),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

file_info_basic_directory(Config, Handle) ->
    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?PRIM_FILE:read_file_info/1 to work on
    %% platforms such as Windows95.
    ?line RootDir = filename:join([?config(priv_dir, Config)]),

    %% Test that the RootDir directory has the expected attributes.
    ?line test_directory(RootDir, read_write, Handle),

    %% Note that on Windows file systems, "/" or "c:/" are *NOT* directories.
    %% Therefore, test that ?PRIM_FILE:read_file_info/1 behaves 
    %% as if they were directories.
    ?line case os:type() of
	      {win32, _} ->
		  ?line test_directory("/", read_write, Handle),
		  ?line test_directory("c:/", read_write, Handle),
		  ?line test_directory("c:\\", read_write, Handle);
	      _ ->
		  ?line test_directory("/", read, Handle)
	  end,
    ok.

test_directory(Name, ExpectedAccess, Handle) ->
    ?line {ok, FileInfo} = ?PRIM_FILE_call(read_file_info, Handle, [Name]),
    ?line #file_info{size = Size, type = Type, access = Access,
		     atime = AccessTime, mtime = ModifyTime} = 
	FileInfo,
    ?line io:format("Testing directory ~s", [Name]),
    ?line io:format("Directory size is ~p", [Size]),
    ?line io:format("Access ~p", [Access]),
    ?line io:format("Access time ~p; Modify time~p", 
		    [AccessTime, ModifyTime]),
    ?line Type = directory,
    ?line Access = ExpectedAccess,
    ?line {AD, AT} = AccessTime,
    ?line all_integers(tuple_to_list(AD) ++ tuple_to_list(AT)),
    ?line {MD, MT} = ModifyTime,
    ?line all_integers(tuple_to_list(MD) ++ tuple_to_list(MT)),
    ok.

all_integers([Int|Rest]) when is_integer(Int) ->
    ?line all_integers(Rest);
all_integers([]) ->
    ok.

%% Try something nonexistent.

file_info_bad_a(suite) -> [];
file_info_bad_a(doc) -> [];
file_info_bad_a(Config) when is_list(Config) ->
    file_info_bad(Config, []).

file_info_bad_b(suite) -> [];
file_info_bad_b(doc) -> [];
file_info_bad_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = file_info_bad(Config, Handle),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

file_info_bad(Config, Handle) ->
    ?line RootDir = filename:join([?config(priv_dir, Config)]),
    ?line {error, enoent} = 
	?PRIM_FILE_call(
	   read_file_info, Handle, 
	   [filename:join(RootDir,
			  atom_to_list(?MODULE)++"_nonexistent")]),
    ok.

%% Test that the file times behave as they should.

file_info_times_a(suite) -> [];
file_info_times_a(doc) -> [];
file_info_times_a(Config) when is_list(Config) ->
    file_info_times(Config, [], "_a").

file_info_times_b(suite) -> [];
file_info_times_b(doc) -> [];
file_info_times_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = file_info_times(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

file_info_times(Config, Handle, Suffix) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    %% We have to try this twice, since if the test runs across the change
    %% of a month the time diff calculations will fail. But it won't happen
    %% if you run it twice in succession.
    ?line test_server:m_out_of_n(
	    1,2,
	    fun() -> ?line file_info_int(Config, Handle, Suffix) end),
    ?line test_server:timetrap_cancel(Dog),
    ok.

file_info_int(Config, Handle, Suffix) ->
    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?PRIM_FILE:read_file_info/1 to work on
    %% platforms such as Windows95.

    ?line RootDir = filename:join([?config(priv_dir, Config)]),
    ?line test_server:format("RootDir = ~p", [RootDir]),

    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_file_info"++Suffix++".fil"),
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?line ?PRIM_FILE:write(Fd1,"foo"),

    %% check that the file got a modify date max a few seconds away from now
    ?line {ok, #file_info{type = regular, 
			  atime = AccTime1, mtime = ModTime1}} = 
	?PRIM_FILE_call(read_file_info, Handle, [Name]),
    ?line Now = erlang:localtime(),
    ?line io:format("Now ~p",[Now]),
    ?line io:format("Open file Acc ~p Mod ~p",[AccTime1,ModTime1]),
    ?line true = abs(time_dist(filter_atime(Now, Config),
			       filter_atime(AccTime1,
					    Config))) < 8,
    ?line true = abs(time_dist(Now, ModTime1)) < 8,
    
    %% Sleep until we can be sure the seconds value has changed.
    %% Note: FAT-based filesystem (like on Windows 95) have
    %% a resolution of 2 seconds.
    ?line test_server:sleep(test_server:seconds(2.2)),

    %% close the file, and watch the modify date change
    ?line ok = ?PRIM_FILE:close(Fd1),
    ?line {ok, #file_info{size = Size, type = regular, access = Access, 
			  atime = AccTime2, mtime = ModTime2}} = 
	?PRIM_FILE_call(read_file_info, Handle, [Name]),
    ?line io:format("Closed file Acc ~p Mod ~p",[AccTime2,ModTime2]),
    ?line true = time_dist(ModTime1, ModTime2) >= 0,

    %% this file is supposed to be binary, so it'd better keep it's size
    ?line Size = 3,
    ?line Access = read_write,

    %% Do some directory checking
    ?line {ok, #file_info{size = DSize, type = directory, 
			  access = DAccess, 
			  atime = AccTime3, mtime = ModTime3}} = 
	?PRIM_FILE_call(read_file_info, Handle, [RootDir]),
    %% this dir was modified only a few secs ago
    ?line io:format("Dir Acc ~p; Mod ~p; Now ~p", 
		    [AccTime3, ModTime3, Now]),
    ?line true = abs(time_dist(Now, ModTime3)) < 5,
    ?line DAccess = read_write,
    ?line io:format("Dir size is ~p",[DSize]),
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

file_write_file_info_a(suite) -> [];
file_write_file_info_a(doc) -> [];
file_write_file_info_a(Config) when is_list(Config) ->
    file_write_file_info(Config, [], "_a").

file_write_file_info_b(suite) -> [];
file_write_file_info_b(doc) -> [];
file_write_file_info_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = file_write_file_info(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

file_write_file_info(Config, Handle, Suffix) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = get_good_directory(Config),
    ?line test_server:format("RootDir = ~p", [RootDir]),

    %% Set the file to read only AND update the file times at the same time.
    %% (This used to fail on Windows NT/95 for a local filesystem.)
    %% Note: Seconds must be even; see note in file_info_times/1.

    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_write_file_info_ro"++Suffix),
    ?line ok = ?PRIM_FILE:write_file(Name, "hello"),
    ?line Time = {{1997, 01, 02}, {12, 35, 42}},
    ?line Info = #file_info{mode=8#400, atime=Time, mtime=Time, ctime=Time},
    ?line ok = ?PRIM_FILE_call(write_file_info, Handle, [Name, Info]),

    %% Read back the times.

    ?line {ok, ActualInfo} = 
	?PRIM_FILE_call(read_file_info, Handle, [Name]),
    ?line #file_info{mode=_Mode, atime=ActAtime, mtime=Time,
		     ctime=ActCtime} = ActualInfo,
    ?line FilteredAtime = filter_atime(Time, Config),
    ?line FilteredAtime = filter_atime(ActAtime, Config),
    ?line case os:type() of
	      {win32, _} ->
		  %% On Windows, "ctime" means creation time and it can
		  %% be set.
		  ActCtime = Time;
	      _ ->
		  ok
	  end,
    ?line {error, eacces} = ?PRIM_FILE:write_file(Name, "hello again"),

    %% Make the file writable again.

    ?line ?PRIM_FILE_call(write_file_info, Handle, 
			  [Name, #file_info{mode=8#600}]),
    ?line ok = ?PRIM_FILE:write_file(Name, "hello again"),

    %% And unwritable.
    ?line ?PRIM_FILE_call(write_file_info, Handle, 
			  [Name, #file_info{mode=8#400}]),
    ?line {error, eacces} = ?PRIM_FILE:write_file(Name, "hello again"),

    %% Write the times again.
    %% Note: Seconds must be even; see note in file_info_times/1.

    ?line NewTime = {{1997, 02, 15}, {13, 18, 20}},
    ?line NewInfo = #file_info{atime=NewTime, mtime=NewTime, ctime=NewTime},
    ?line ok = ?PRIM_FILE_call(write_file_info, Handle, [Name, NewInfo]),
    ?line {ok, ActualInfo2} = 
	?PRIM_FILE_call(read_file_info, Handle, [Name]),
    ?line #file_info{atime=NewActAtime, mtime=NewTime,
		     ctime=NewActCtime} = ActualInfo2,
    ?line NewFilteredAtime = filter_atime(NewTime, Config),
    ?line NewFilteredAtime = filter_atime(NewActAtime, Config),
    ?line case os:type() of
	      {win32, _} -> NewActCtime = NewTime;
	      _ -> ok
	  end,

    %% The file should still be unwritable.
    ?line {error, eacces} = ?PRIM_FILE:write_file(Name, "hello again"),

    %% Make the file writeable again, so that we can remove the
    %% test suites ... :-)
    ?line ?PRIM_FILE_call(write_file_info, Handle, 
			  [Name, #file_info{mode=8#600}]),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Test the write_file_info/3 function.

file_write_file_info_opts(suite) -> [];
file_write_file_info_opts(doc) -> [];
file_write_file_info_opts(Config) when is_list(Config) ->
    {ok, Handle} = ?PRIM_FILE:start(),
    Dog = test_server:timetrap(test_server:seconds(10)),
    RootDir = get_good_directory(Config),
    test_server:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir, atom_to_list(?MODULE) ++"_write_file_info_opts"),
    ok   = ?PRIM_FILE:write_file(Name, "hello_opts"),

    lists:foreach(fun
	    ({FI, Opts}) ->
		ok = ?PRIM_FILE_call(write_file_info, Handle, [Name, FI, Opts])
	end, [ 
	    {#file_info{ mode=8#600, atime = Time, mtime = Time, ctime = Time}, Opts} || 
	    Opts <- [[{time, posix}]],
	    Time <- [ 0,1,-1,100,-100,1000,-1000,10000,-10000 ]
	]),

    % REM: determine date range dependent on time_t = Uint32 | Sint32 | Sint64
    % Determine time_t on os:type()?
    lists:foreach(fun
	    ({FI, Opts}) ->
		ok = ?PRIM_FILE_call(write_file_info, Handle, [Name, FI, Opts])
	end, [ 
	    {#file_info{ mode=8#400, atime = Time, mtime = Time, ctime = Time}, Opts} || 
	    Opts <- [[{time, universal}],[{time, local}]],
	    Time <- [
		{{1970,1,1},{0,0,0}},
		{{1970,1,1},{0,0,1}},
		{{1969,12,31},{23,59,59}},
		{{1908,2,3},{23,59,59}},
		{{2012,2,3},{23,59,59}},
		{{2037,2,3},{23,59,59}},
		erlang:localtime()
	    ]]),
    ok   = ?PRIM_FILE:stop(Handle),
    test_server:timetrap_cancel(Dog),
    ok.

file_read_file_info_opts(suite) -> [];
file_read_file_info_opts(doc) -> [];
file_read_file_info_opts(Config) when is_list(Config) ->
    {ok, Handle} = ?PRIM_FILE:start(),
    Dog = test_server:timetrap(test_server:seconds(10)),
    RootDir = get_good_directory(Config),
    test_server:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir, atom_to_list(?MODULE) ++"_read_file_info_opts"),
    ok   = ?PRIM_FILE:write_file(Name, "hello_opts"),

    lists:foreach(fun
	    (Opts) ->
		{ok,_} = ?PRIM_FILE_call(read_file_info, Handle, [Name, Opts])
    end, [[{time, Type}] || Type <- [local, universal, posix]]),
    ok   = ?PRIM_FILE:stop(Handle),
    test_server:timetrap_cancel(Dog),
    ok.

%% Test the write and read back *_file_info/3 functions.

file_write_read_file_info_opts(suite) -> [];
file_write_read_file_info_opts(doc) -> [];
file_write_read_file_info_opts(Config) when is_list(Config) ->
    {ok, Handle} = ?PRIM_FILE:start(),
    Dog = test_server:timetrap(test_server:seconds(10)),
    RootDir = get_good_directory(Config),
    test_server:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir, atom_to_list(?MODULE) ++"_read_write_file_info_opts"),
    ok   = ?PRIM_FILE:write_file(Name, "hello_opts2"),

    ok = file_write_read_file_info_opts(Handle, Name, {{1989, 04, 28}, {19,30,22}}, [{time, local}]),
    ok = file_write_read_file_info_opts(Handle, Name, {{1989, 04, 28}, {19,30,22}}, [{time, universal}]),
    ok = file_write_read_file_info_opts(Handle, Name, {{1930, 04, 28}, {19,30,22}}, [{time, local}]),
    ok = file_write_read_file_info_opts(Handle, Name, {{1930, 04, 28}, {19,30,22}}, [{time, universal}]),
    ok = file_write_read_file_info_opts(Handle, Name, 1, [{time, posix}]),
    ok = file_write_read_file_info_opts(Handle, Name, -1, [{time, posix}]),
    ok = file_write_read_file_info_opts(Handle, Name, 300000, [{time, posix}]),
    ok = file_write_read_file_info_opts(Handle, Name, -300000, [{time, posix}]),
    ok = file_write_read_file_info_opts(Handle, Name, 0, [{time, posix}]),

    ok = ?PRIM_FILE:stop(Handle),
    test_server:timetrap_cancel(Dog),
    ok.

file_write_read_file_info_opts(Handle, Name, Mtime, Opts) ->
    {ok, FI} = ?PRIM_FILE_call(read_file_info, Handle, [Name, Opts]),
    FI2 = FI#file_info{ mtime = Mtime },
    ok = ?PRIM_FILE_call(write_file_info, Handle, [Name, FI2, Opts]),
    {ok, FI2} = ?PRIM_FILE_call(read_file_info, Handle, [Name, Opts]),
    ok.



%% Returns a directory on a file system that has correct file times.

get_good_directory(Config) ->
    ?line ?config(priv_dir, Config).

truncate(suite) -> [];
truncate(doc) -> [];
truncate(Config) when is_list(Config) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_truncate.fil"),

    %% Create a file with some data.
    ?line MyData = "0123456789abcdefghijklmnopqrstuvxyz",
    ?line ok = ?PRIM_FILE:write_file(Name, MyData),

    %% Truncate the file to 10 characters.
    ?line {ok, Fd} = ?PRIM_FILE:open(Name, [read, write]),
    ?line {ok, 10} = ?PRIM_FILE:position(Fd, 10),
    ?line ok = ?PRIM_FILE:truncate(Fd),
    ?line ok = ?PRIM_FILE:close(Fd),

    %% Read back the file and check that it has been truncated.
    ?line Expected = list_to_binary("0123456789"),
    ?line {ok, Expected} = ?PRIM_FILE:read_file(Name),

    %% Open the file read only and verify that it is not possible to
    %% truncate it, OTP-1960
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),
    ?line {ok, 5} = ?PRIM_FILE:position(Fd2, 5),
    ?line {error, _} = ?PRIM_FILE:truncate(Fd2),

    ok.


datasync(suite) -> [];
datasync(doc) -> "Tests that ?PRIM_FILE:datasync/1 at least doesn't crash.";
datasync(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Sync = filename:join(PrivDir,
			       atom_to_list(?MODULE)
			       ++"_sync.fil"),

    %% Raw open.
    ?line {ok, Fd} = ?PRIM_FILE:open(Sync, [write]),
    ?line ok = ?PRIM_FILE:datasync(Fd),
    ?line ok = ?PRIM_FILE:close(Fd),

    ok.


sync(suite) -> [];
sync(doc) -> "Tests that ?PRIM_FILE:sync/1 at least doesn't crash.";
sync(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Sync = filename:join(PrivDir, 
			       atom_to_list(?MODULE)
			       ++"_sync.fil"),

    %% Raw open.
    ?line {ok, Fd} = ?PRIM_FILE:open(Sync, [write]),
    ?line ok = ?PRIM_FILE:sync(Fd),
    ?line ok = ?PRIM_FILE:close(Fd),

    ok.


advise(suite) -> [];
advise(doc) -> "Tests that ?PRIM_FILE:advise/4 at least doesn't crash.";
advise(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Advise = filename:join(PrivDir,
			       atom_to_list(?MODULE)
			       ++"_advise.fil"),

    Line1 = "Hello\n",
    Line2 = "World!\n",

    ?line {ok, Fd} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:advise(Fd, 0, 0, normal),
    ?line ok = ?PRIM_FILE:write(Fd, Line1),
    ?line ok = ?PRIM_FILE:write(Fd, Line2),
    ?line ok = ?PRIM_FILE:close(Fd),

    ?line {ok, Fd2} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:advise(Fd2, 0, 0, random),
    ?line ok = ?PRIM_FILE:write(Fd2, Line1),
    ?line ok = ?PRIM_FILE:write(Fd2, Line2),
    ?line ok = ?PRIM_FILE:close(Fd2),

    ?line {ok, Fd3} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:advise(Fd3, 0, 0, sequential),
    ?line ok = ?PRIM_FILE:write(Fd3, Line1),
    ?line ok = ?PRIM_FILE:write(Fd3, Line2),
    ?line ok = ?PRIM_FILE:close(Fd3),

    ?line {ok, Fd4} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:advise(Fd4, 0, 0, will_need),
    ?line ok = ?PRIM_FILE:write(Fd4, Line1),
    ?line ok = ?PRIM_FILE:write(Fd4, Line2),
    ?line ok = ?PRIM_FILE:close(Fd4),

    ?line {ok, Fd5} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:advise(Fd5, 0, 0, dont_need),
    ?line ok = ?PRIM_FILE:write(Fd5, Line1),
    ?line ok = ?PRIM_FILE:write(Fd5, Line2),
    ?line ok = ?PRIM_FILE:close(Fd5),

    ?line {ok, Fd6} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:advise(Fd6, 0, 0, no_reuse),
    ?line ok = ?PRIM_FILE:write(Fd6, Line1),
    ?line ok = ?PRIM_FILE:write(Fd6, Line2),
    ?line ok = ?PRIM_FILE:close(Fd6),

    ?line {ok, Fd7} = ?PRIM_FILE:open(Advise, [write]),
    ?line {error, einval} = ?PRIM_FILE:advise(Fd7, 0, 0, bad_advise),
    ?line ok = ?PRIM_FILE:close(Fd7),

    %% test write without advise, then a read after an advise
    ?line {ok, Fd8} = ?PRIM_FILE:open(Advise, [write]),
    ?line ok = ?PRIM_FILE:write(Fd8, Line1),
    ?line ok = ?PRIM_FILE:write(Fd8, Line2),
    ?line ok = ?PRIM_FILE:close(Fd8),
    ?line {ok, Fd9} = ?PRIM_FILE:open(Advise, [read]),
    Offset = 0,
    %% same as a 0 length in some implementations
    Length = length(Line1) + length(Line2),
    ?line ok = ?PRIM_FILE:advise(Fd9, Offset, Length, sequential),
    ?line {ok, Line1} = ?PRIM_FILE:read_line(Fd9),
    ?line {ok, Line2} = ?PRIM_FILE:read_line(Fd9),
    ?line eof = ?PRIM_FILE:read_line(Fd9),
    ?line ok = ?PRIM_FILE:close(Fd9),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

large_write(Config) when is_list(Config) ->
    run_large_file_test(Config,
			fun(Name) -> do_large_write(Name) end,
			"_large_write").

do_large_write(Name) ->
    Dog = test_server:timetrap(test_server:minutes(60)),
    ChunkSize = (256 bsl 20) + 1,	% 256 M + 1
    Chunks = 16,			% times 16 -> 4 G + 16
    Base = 100,
    Interleave = lists:seq(Base+1, Base+Chunks),
    Chunk = <<0:ChunkSize/unit:8>>,
    Data = zip_data(lists:duplicate(Chunks, Chunk), Interleave),
    Size = Chunks * ChunkSize + Chunks,	% 4 G + 32
    Wordsize = erlang:system_info(wordsize),
    case prim_file:write_file(Name, Data) of
	ok when Wordsize =:= 8 ->
	    {ok,#file_info{size=Size}} = file:read_file_info(Name),
	    {ok,Fd} = prim_file:open(Name, [read]),
	    check_large_write(Dog, Fd, ChunkSize, 0, Interleave);
	{error,einval} when Wordsize =:= 4 ->
	    ok
    end.

check_large_write(Dog, Fd, ChunkSize, Pos, [X|Interleave]) ->
    Pos1 = Pos + ChunkSize,
    {ok,Pos1} = prim_file:position(Fd, {cur,ChunkSize}),
    {ok,[X]} = prim_file:read(Fd, 1),
    check_large_write(Dog, Fd, ChunkSize, Pos1+1, Interleave);
check_large_write(Dog, Fd, _, _, []) ->
    eof = prim_file:read(Fd, 1),
    test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allocate(suite) -> [];
allocate(doc) -> "Tests that ?PRIM_FILE:allocate/3 at least doesn't crash.";
allocate(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Allocate = filename:join(PrivDir,
			       atom_to_list(?MODULE)
			       ++"_allocate.fil"),

    Line1 = "Hello\n",
    Line2 = "World!\n",

    ?line {ok, Fd} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd, 1, iolist_size([Line1, Line2])),
    ?line ok = ?PRIM_FILE:write(Fd, Line1),
    ?line ok = ?PRIM_FILE:write(Fd, Line2),
    ?line ok = ?PRIM_FILE:close(Fd),

    ?line {ok, Fd2} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd2, 1, iolist_size(Line1)),
    ?line ok = ?PRIM_FILE:write(Fd2, Line1),
    ?line ok = ?PRIM_FILE:write(Fd2, Line2),
    ?line ok = ?PRIM_FILE:close(Fd2),

    ?line {ok, Fd3} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd3, 1, iolist_size(Line1) + 1),
    ?line ok = ?PRIM_FILE:write(Fd3, Line1),
    ?line ok = ?PRIM_FILE:write(Fd3, Line2),
    ?line ok = ?PRIM_FILE:close(Fd3),

    ?line {ok, Fd4} = ?PRIM_FILE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd4, 1, 4 * iolist_size([Line1, Line2])),
    ?line ok = ?PRIM_FILE:write(Fd4, Line1),
    ?line ok = ?PRIM_FILE:write(Fd4, Line2),
    ?line ok = ?PRIM_FILE:close(Fd4),

    ok.

allocate_and_assert(Fd, Offset, Length) ->
    % Just verify that calls to ?PRIM_FILE:allocate/3 don't crash or have
    % any other negative side effect. We can't really asssert against a
    % specific return value, because support for file space pre-allocation
    % depends on the OS, OS version and underlying filesystem.
    %
    % The Linux kernel added support for fallocate() in version 2.6.23,
    % which currently works only for the ext4, ocfs2, xfs and btrfs file
    % systems. posix_fallocate() is available in glibc as of version
    % 2.1.94, but it was buggy until glibc version 2.7.
    %
    % Mac OS X, as of version 10.3, supports the fcntl operation F_PREALLOCATE.
    %
    % Solaris supports posix_fallocate() but only for the UFS file system
    % apparently (not supported for ZFS).
    %
    % FreeBSD 9.0 is the first FreeBSD release supporting posix_fallocate().
    %
    % For Windows there's apparently no way to pre-allocate file space, at
    % least with similar API/semantics as posix_fallocate(), fallocate() or
    % fcntl F_PREALLOCATE.
    Result = ?PRIM_FILE:allocate(Fd, Offset, Length),
    case os:type() of
        {win32, _} ->
            ?line {error, enotsup} = Result;
        _ ->
            ?line _ = Result
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_a(suite) -> [];
delete_a(doc) -> [];
delete_a(Config) when is_list(Config) ->
    delete(Config, [], "_a").

delete_b(suite) -> [];
delete_b(doc) -> [];
delete_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = delete(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

delete(Config, Handle, Suffix) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_delete"++Suffix++".fil"),
    ?line {ok, Fd1} = ?PRIM_FILE:open(Name, [write]),
    ?line ?PRIM_FILE:write(Fd1,"ok.\n"),
    ?line ok = ?PRIM_FILE:close(Fd1),
    %% Check that the file is readable
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name, [read]),
    ?line ok = ?PRIM_FILE:close(Fd2),
    ?line ok = ?PRIM_FILE_call(delete, Handle, [Name]),
    %% Check that the file is not readable anymore
    ?line {error, _} = ?PRIM_FILE:open(Name, [read]),
    %% Try deleting a nonexistent file
    ?line {error, enoent} = ?PRIM_FILE_call(delete, Handle, [Name]),
    ok.

rename_a(suite) ->[];
rename_a(doc) ->[];
rename_a(Config) when is_list(Config) ->
    rename(Config, [], "_a").

rename_b(suite) ->[];
rename_b(doc) ->[];
rename_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = rename(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

rename(Config, Handle, Suffix) ->
    ?line RootDir = ?config(priv_dir,Config),
    ?line FileName1 = atom_to_list(?MODULE)++"_rename"++Suffix++".fil",
    ?line FileName2 = atom_to_list(?MODULE)++"_rename"++Suffix++".ful",
    ?line Name1 = filename:join(RootDir, FileName1),
    ?line Name2 = filename:join(RootDir, FileName2),
    ?line {ok,Fd1} = ?PRIM_FILE:open(Name1, [write]),
    ?line ok = ?PRIM_FILE:close(Fd1),
    %% Rename, and check that it really changed name
    ?line ok = ?PRIM_FILE_call(rename, Handle, [Name1, Name2]),
    ?line {error, _} = ?PRIM_FILE:open(Name1, [read]),
    ?line {ok, Fd2} = ?PRIM_FILE:open(Name2, [read]),
    ?line ok = ?PRIM_FILE:close(Fd2),
    %% Try renaming something to itself
    ?line ok = ?PRIM_FILE_call(rename, Handle, [Name2, Name2]),
    %% Try renaming something that doesn't exist
    ?line {error, enoent} = 
	?PRIM_FILE_call(rename, Handle, [Name1, Name2]),
    %% Try renaming to something else than a string
    ?line {error, badarg} = 
	?PRIM_FILE_call(rename, Handle, [Name1, foobar]),
    
    %% Move between directories
    ?line DirName1 = filename:join(RootDir,
				   atom_to_list(?MODULE)
				   ++"_rename_dir"++Suffix),
    ?line DirName2 = filename:join(RootDir,
				   atom_to_list(?MODULE)
				   ++"_second_rename_dir"++Suffix),
    ?line Name1foo = filename:join(DirName1, "foo.fil"),
    ?line Name2foo = filename:join(DirName2, "foo.fil"),
    ?line Name2bar = filename:join(DirName2, "bar.dir"),
    ?line ok = ?PRIM_FILE:make_dir(DirName1),
    %% The name has to include the full file name, path is not enough
    ?line expect(
	    {error, eexist}, {error, eisdir},
	    ?PRIM_FILE_call(rename, Handle, [Name2, DirName1])),
    ?line ok = 
	?PRIM_FILE_call(rename, Handle, [Name2, Name1foo]),
    %% Now rename the directory
    ?line ok = ?PRIM_FILE_call(rename, Handle, [DirName1, DirName2]),
    %% And check that the file is there now
    ?line {ok,Fd3} = ?PRIM_FILE:open(Name2foo, [read]),
    ?line ok = ?PRIM_FILE:close(Fd3),
    %% Try some dirty things now: move the directory into itself
    ?line {error, Msg1} = 
	?PRIM_FILE_call(rename, Handle, [DirName2, Name2bar]),
    ?line io:format("Errmsg1: ~p",[Msg1]),
    %% move dir into a file in itself
    ?line {error, Msg2} = 
	?PRIM_FILE_call(rename, Handle, [DirName2, Name2foo]),
    ?line io:format("Errmsg2: ~p",[Msg2]),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


e_delete(suite) -> [];
e_delete(doc) -> [];
e_delete(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_e_delete"),
    ?line ok = ?PRIM_FILE:make_dir(Base),

    %% Delete a non-existing file.
    ?line {error, enoent} = 
	?PRIM_FILE:delete(filename:join(Base, "non_existing")),

    %% Delete a directory.
    ?line {error, eperm} = ?PRIM_FILE:delete(Base),

    %% Use a path-name with a non-directory component.
    ?line Afile = filename:join(Base, "a_file"),
    ?line ok = ?PRIM_FILE:write_file(Afile, "hello\n"),
    ?line {error, E} = 
	expect(
	  {error, enotdir}, {error, enoent}, 
	  ?PRIM_FILE:delete(filename:join(Afile, "another_file"))),
    ?line io:format("Result: ~p~n", [E]),

    %% No permission.
    ?line case os:type() of
	      {win32, _} ->
		  %% Remove a character device.
		  ?line {error, eacces} = ?PRIM_FILE:delete("nul");
	      _ ->
		  ?line ?PRIM_FILE:write_file_info(
			   Base, #file_info {mode=0}),
		  ?line {error, eacces} = ?PRIM_FILE:delete(Afile),
		  ?line ?PRIM_FILE:write_file_info(
			   Base, #file_info {mode=8#600})
	  end,

    ?line test_server:timetrap_cancel(Dog),
    ok.

%%% FreeBSD gives EEXIST when renaming a file to an empty dir, although the
%%% manual page can be interpreted as saying that EISDIR should be given.
%%% (What about FreeBSD? We store our nightly build results on a FreeBSD
%%% file system, that's what.)

e_rename(suite) -> [];
e_rename(doc) -> [];
e_rename(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = filename:join(RootDir,
	atom_to_list(?MODULE)++"_e_rename"),
    ?line ok = ?PRIM_FILE:make_dir(Base),

    %% Create an empty directory.
    ?line EmptyDir = filename:join(Base, "empty_dir"),
    ?line ok = ?PRIM_FILE:make_dir(EmptyDir),

    %% Create a non-empty directory.
    ?line NonEmptyDir = filename:join(Base, "non_empty_dir"),
    ?line ok = ?PRIM_FILE:make_dir(NonEmptyDir),
    ?line ok = ?PRIM_FILE:write_file(
	filename:join(NonEmptyDir, "a_file"),
	"hello\n"),

    %% Create another non-empty directory.
    ?line ADirectory = filename:join(Base, "a_directory"),
    ?line ok = ?PRIM_FILE:make_dir(ADirectory),
    ?line ok = ?PRIM_FILE:write_file(
	filename:join(ADirectory, "a_file"),
	"howdy\n\n"),

    %% Create a data file.
    ?line File = filename:join(Base, "just_a_file"),
    ?line ok = ?PRIM_FILE:write_file(File, "anything goes\n\n"),

    %% Move an existing directory to a non-empty directory.
    ?line {error, eexist} = 
    ?PRIM_FILE:rename(ADirectory, NonEmptyDir),

    %% Move a root directory.
    ?line {error, einval} = ?PRIM_FILE:rename("/", "arne"),

    %% Move Base into Base/new_name.
    ?line {error, einval} = 
    ?PRIM_FILE:rename(Base, filename:join(Base, "new_name")),

    %% Overwrite a directory with a file.
    ?line expect({error, eexist}, % FreeBSD (?)
	{error, eisdir},
	?PRIM_FILE:rename(File, EmptyDir)),
    ?line expect({error, eexist}, % FreeBSD (?)
	{error, eisdir},
	?PRIM_FILE:rename(File, NonEmptyDir)),

    %% Move a non-existing file.
    ?line NonExistingFile = filename:join(
	Base, "non_existing_file"),
    ?line {error, enoent} = 
    ?PRIM_FILE:rename(NonExistingFile, NonEmptyDir),

    %% Overwrite a file with a directory.
    ?line expect({error, eexist}, % FreeBSD (?)
	{error, enotdir},
	?PRIM_FILE:rename(ADirectory, File)),

    %% Move a file to another filesystem.
    %% XXX - This test case is bogus. We cannot be guaranteed that
    %%       the source and destination are on 
    %%       different filesystems.
    %%
    %% XXX - Gross hack!
    ?line Comment = 
    case os:type() of
	{win32, _} ->
	    %% At least Windows NT can
	    %% successfully move a file to
	    %% another drive.
	    ok;
	_ ->
	    OtherFs = "/tmp",
	    ?line NameOnOtherFs =
	    filename:join(OtherFs, 
		filename:basename(File)),
	    ?line {ok, Com} = 
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
    ?line test_server:timetrap_cancel(Dog),
    Comment.

e_make_dir(suite) -> [];
e_make_dir(doc) -> [];
e_make_dir(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_e_make_dir"),
    ?line ok = ?PRIM_FILE:make_dir(Base),

    %% A component of the path does not exist.
    ?line {error, enoent} = 
	?PRIM_FILE:make_dir(filename:join([Base, "a", "b"])),

    %% Use a path-name with a non-directory component.
    ?line Afile = filename:join(Base, "a_directory"),
    ?line ok = ?PRIM_FILE:write_file(Afile, "hello\n"),
    ?line case ?PRIM_FILE:make_dir(
		  filename:join(Afile, "another_directory")) of
	      {error, enotdir} -> io:format("Result: enotdir");
	      {error, enoent} -> io:format("Result: enoent")
	  end,

    %% No permission (on Unix only).
    case os:type() of
	{win32, _} ->
	    ok;
	_ ->
	    ?line ?PRIM_FILE:write_file_info(Base, #file_info {mode=0}),
	    ?line {error, eacces} = 
		?PRIM_FILE:make_dir(filename:join(Base, "xxxx")),
	    ?line 
		?PRIM_FILE:write_file_info(Base, #file_info {mode=8#600})
    end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

e_del_dir(suite) -> [];
e_del_dir(doc) -> [];
e_del_dir(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_e_del_dir"),
    ?line io:format("Base: ~p", [Base]),
    ?line ok = ?PRIM_FILE:make_dir(Base),

    %% Delete a non-existent directory.
    ?line {error, enoent} = 
	?PRIM_FILE:del_dir(filename:join(Base, "non_existing")),

    %% Use a path-name with a non-directory component.
    ?line Afile = filename:join(Base, "a_directory"),
    ?line ok = ?PRIM_FILE:write_file(Afile, "hello\n"),
    ?line {error, E1} = 
	expect({error, enotdir}, {error, enoent},
	       ?PRIM_FILE:del_dir(
		  filename:join(Afile, "another_directory"))),
    ?line io:format("Result: ~p", [E1]),

    %% Delete a non-empty directory.
    %% Delete a non-empty directory.
    ?line {error, E2} = 
	expect({error, enotempty}, {error, eexist}, {error, eacces},
	       ?PRIM_FILE:del_dir(Base)),
    ?line io:format("Result: ~p", [E2]),

    %% Remove the current directory.
    ?line {error, E3} = 
	expect({error, einval}, 
	       {error, eperm}, % Linux and DUX
	       {error, eacces},
	       {error, ebusy},
	       ?PRIM_FILE:del_dir(".")),
    ?line io:format("Result: ~p", [E3]),

    %% No permission.
    case os:type() of
	{win32, _} ->
	    ok;
	_ ->
	    ?line ADirectory = filename:join(Base, "no_perm"),
	    ?line ok = ?PRIM_FILE:make_dir(ADirectory),
	    ?line ?PRIM_FILE:write_file_info(Base, #file_info {mode=0}),
	    ?line {error, eacces} = ?PRIM_FILE:del_dir(ADirectory),
	    ?line ?PRIM_FILE:write_file_info(
		     Base, #file_info {mode=8#600})
    end,
    ?line test_server:timetrap_cancel(Dog),
    ok.


%% Trying reading and positioning from a compressed file.

read_compressed(suite) -> [];
read_compressed(doc) -> [];
read_compressed(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line Real = filename:join(Data, "realmen.html.gz"),
    ?line {ok, Fd} = ?PRIM_FILE:open(Real, [read, compressed]),
    ?line try_read_file(Fd).

%% Trying reading and positioning from an uncompressed file,
%% but with the compressed flag given.

read_not_really_compressed(suite) -> [];
read_not_really_compressed(doc) -> [];
read_not_really_compressed(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line Priv = ?config(priv_dir, Config),

    %% The file realmen.html might have got CRs added (by WinZip).
    %% Remove them, or the file positions will not be correct.

    ?line Real = filename:join(Data, "realmen.html"),
    ?line RealPriv = filename:join(Priv, 
				   atom_to_list(?MODULE)++"_realmen.html"),
    ?line {ok, RealDataBin} = ?PRIM_FILE:read_file(Real),
    ?line RealData = remove_crs(binary_to_list(RealDataBin), []),
    ?line ok = ?PRIM_FILE:write_file(RealPriv, RealData),
    ?line {ok, Fd} = ?PRIM_FILE:open(RealPriv, [read, compressed]),
    ?line try_read_file(Fd).

remove_crs([$\r|Rest], Result) ->
    remove_crs(Rest, Result);
remove_crs([C|Rest], Result) ->
    remove_crs(Rest, [C|Result]);
remove_crs([], Result) ->
    lists:reverse(Result).

try_read_file(Fd) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    %% Seek to the current position (nothing should happen).

    ?line {ok, 0} = ?PRIM_FILE:position(Fd, 0),
    ?line {ok, 0} = ?PRIM_FILE:position(Fd, {cur, 0}),

    %% Read a few lines from a compressed file.

    ?line ShouldBe = "<TITLE>Real Programmers Don't Use PASCAL</TITLE>\n",
    ?line {ok, ShouldBe} = ?PRIM_FILE:read(Fd, length(ShouldBe)),

    %% Now seek forward.

    ?line {ok, 381} = ?PRIM_FILE:position(Fd, 381),
    ?line Back = "Back in the good old days -- the \"Golden Era\" " ++
	"of computers, it was\n",
    ?line {ok, Back} = ?PRIM_FILE:read(Fd, length(Back)),

    %% Try to search forward relative to the current position.

    ?line {ok, CurPos} = ?PRIM_FILE:position(Fd, {cur, 0}),
    ?line RealPos = 4273,
    ?line {ok, RealPos} = ?PRIM_FILE:position(Fd, {cur, RealPos-CurPos}),
    ?line RealProg = "<LI> Real Programmers aren't afraid to use GOTOs.\n",
    ?line {ok, RealProg} = ?PRIM_FILE:read(Fd, length(RealProg)),

    %% Seek backward.

    ?line AfterTitle = length("<TITLE>"),
    ?line {ok, AfterTitle} = ?PRIM_FILE:position(Fd, AfterTitle),
    ?line Title = "Real Programmers Don't Use PASCAL</TITLE>\n",
    ?line {ok, Title} = ?PRIM_FILE:read(Fd, length(Title)),

    %% Done.

    ?line ?PRIM_FILE:close(Fd),
    ?line test_server:timetrap_cancel(Dog),
    ok.

write_compressed(suite) -> [];
write_compressed(doc) -> [];
write_compressed(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Priv = ?config(priv_dir, Config),
    ?line MyFile = filename:join(Priv, 
				 atom_to_list(?MODULE)++"_test.gz"),

    %% Write a file.

    ?line {ok, Fd} = ?PRIM_FILE:open(MyFile, [write, compressed]),
    ?line {ok, 0} = ?PRIM_FILE:position(Fd, 0),
    ?line Prefix = "hello\n",
    ?line End = "end\n",
    ?line ok = ?PRIM_FILE:write(Fd, Prefix),
    ?line {ok, 143} = ?PRIM_FILE:position(Fd, 143),
    ?line ok = ?PRIM_FILE:write(Fd, End),
    ?line ok = ?PRIM_FILE:close(Fd),

    %% Read the file and verify the contents.

    ?line {ok, Fd1} = ?PRIM_FILE:open(MyFile, [read, compressed]),
    ?line {ok, Prefix} = ?PRIM_FILE:read(Fd1, length(Prefix)),
    ?line Second = lists:duplicate(143-length(Prefix), 0) ++ End,
    ?line {ok, Second} = ?PRIM_FILE:read(Fd1, length(Second)),
    ?line ok = ?PRIM_FILE:close(Fd1),

    %% Ensure that the file is compressed.

    TotalSize = 143 + length(End),
    case ?PRIM_FILE:read_file_info(MyFile) of
	{ok, #file_info{size=Size}} when Size < TotalSize ->
	    ok;
	{ok, #file_info{size=Size}} when Size == TotalSize ->
	    test_server:fail(file_not_compressed)
    end,

    %% Write again to ensure that the file is truncated.

    ?line {ok, Fd2} = ?PRIM_FILE:open(MyFile, [write, compressed]),
    ?line NewString = "aaaaaaaaaaa",
    ?line ok = ?PRIM_FILE:write(Fd2, NewString),
    ?line ok = ?PRIM_FILE:close(Fd2),
    ?line {ok, Fd3} = ?PRIM_FILE:open(MyFile, [read, compressed]),
    ?line {ok, NewString} = ?PRIM_FILE:read(Fd3, 1024),
    ?line ok = ?PRIM_FILE:close(Fd3),

    %% Done.

    ?line test_server:timetrap_cancel(Dog),
    ok.

compress_errors(suite) -> [];
compress_errors(doc) -> [];
compress_errors(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Data = ?config(data_dir, Config),
    ?line {error, enoent} = ?PRIM_FILE:open("non_existing__",
				      [compressed, read]),
    ?line {error, einval} = ?PRIM_FILE:open("non_existing__",
				      [compressed, read, write]),

    %% Read a corrupted .gz file.

    ?line Corrupted = filename:join(Data, "corrupted.gz"),
    ?line {ok, Fd} = ?PRIM_FILE:open(Corrupted, [read, compressed]),
    ?line {error, eio} = ?PRIM_FILE:read(Fd, 100),
    ?line ?PRIM_FILE:close(Fd),

    ?line test_server:timetrap_cancel(Dog),
    ok.


make_link_a(doc) -> "Test creating a hard link.";
make_link_a(suite) -> [];
make_link_a(Config) when is_list(Config) ->
    make_link(Config, [], "_a").

make_link_b(doc) -> "Test creating a hard link.";
make_link_b(suite) -> [];
make_link_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = make_link(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

make_link(Config, Handle, Suffix) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_make_link"++Suffix),
    ?line ok = ?PRIM_FILE_call(make_dir, Handle, [NewDir]),
    
    ?line Name = filename:join(NewDir, "a_file"),
    ?line ok = ?PRIM_FILE:write_file(Name, "some contents\n"),
    
    ?line Alias = filename:join(NewDir, "an_alias"),
    ?line Result = 
	case ?PRIM_FILE_call(make_link, Handle, [Name, Alias]) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    ok ->
		%% Note: We take the opportunity to test 
		%% ?PRIM_FILE:read_link_info/1,
		%% which should in behave exactly as 
		%% ?PRIM_FILE:read_file_info/1
		%% since they are not used on symbolic links.
		
		?line {ok, Info} = 
		    ?PRIM_FILE_call(read_link_info, Handle, [Name]),
		?line {ok, Info} = 
		    ?PRIM_FILE_call(read_link_info, Handle, [Alias]),
		?line #file_info{links = 2, type = regular} = Info,
		?line {error, eexist} = 
		    ?PRIM_FILE_call(make_link, Handle, [Name, Alias]),
		ok
	end,
    
    ?line test_server:timetrap_cancel(Dog),
    Result.

read_link_info_for_non_link(doc) ->
    "Test that reading link info for an ordinary file or directory works "
	"(on all platforms).";
read_link_info_for_non_link(suite) -> [];
read_link_info_for_non_link(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    ?line {ok, #file_info{type=directory}} = ?PRIM_FILE:read_link_info("."),
		  
    ?line test_server:timetrap_cancel(Dog),
    ok.
    
symlinks_a(doc) -> "Test operations on symbolic links (for Unix).";
symlinks_a(suite) -> [];
symlinks_a(Config) when is_list(Config) ->
    symlinks(Config, [], "_a").

symlinks_b(doc) -> "Test operations on symbolic links (for Unix).";
symlinks_b(suite) -> [];
symlinks_b(Config) when is_list(Config) ->
    ?line {ok, Handle} = ?PRIM_FILE:start(),
    Result = symlinks(Config, Handle, "_b"),
    ?line ok = ?PRIM_FILE:stop(Handle),
    Result.

symlinks(Config, Handle, Suffix) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_make_symlink"++Suffix),
    ?line ok = ?PRIM_FILE_call(make_dir, Handle, [NewDir]),
    
    ?line Name = filename:join(NewDir, "a_plain_file"),
    ?line ok = ?PRIM_FILE:write_file(Name, "some stupid content\n"),
    
    ?line Alias = filename:join(NewDir, "a_symlink_alias"),
    ?line Result = 
	case ?PRIM_FILE_call(make_symlink, Handle, [Name, Alias]) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    {error, eperm} ->
		{win32,_} = os:type(),
		{skipped, "Windows user not privileged to create links"};
	    ok ->
		?line {ok, Info1} = 
		    ?PRIM_FILE_call(read_file_info, Handle, [Name]),
		?line {ok, Info1} = 
		    ?PRIM_FILE_call(read_file_info, Handle, [Alias]),
		?line {ok, Info1} = 
		    ?PRIM_FILE_call(read_link_info, Handle, [Name]),
		?line #file_info{links = 1, type = regular} = Info1,
		
		?line {ok, Info2} = 
		    ?PRIM_FILE_call(read_link_info, Handle, [Alias]),
		?line #file_info{links=1, type=symlink} = Info2,
		?line {ok, Name} = 
		    ?PRIM_FILE_call(read_link, Handle, [Alias]),
		{ok, Name} =
		    ?PRIM_FILE_call(read_link_all, Handle, [Alias]),
		%% If all is good, delete dir again (avoid hanging dir on windows)
		rm_rf(?PRIM_FILE,NewDir),
		ok
	end,
    
    ?line test_server:timetrap_cancel(Dog),
    Result.

%% Creates as many files as possible during a certain time, 
%% periodically calls list_dir/2 to check if it works,
%% then deletes all files.

list_dir_limit(doc) ->
    "Tests if large directories can be read";
list_dir_limit(suite) ->
    [];
list_dir_limit(Config) when is_list(Config) ->
    ?line MaxTime = 120, 
    ?line MaxNumber = 20000, 
    ?line Dog = test_server:timetrap(
		  test_server:seconds(2*MaxTime + MaxTime)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir,
				 atom_to_list(?MODULE)++"_list_dir_limit"),
    ?line {ok, Handle1} = ?PRIM_FILE:start(),
    ?line ok = ?PRIM_FILE_call(make_dir, Handle1, [NewDir]),
    Ref = erlang:start_timer(MaxTime*1000, self(), []),
    ?line Result = list_dir_limit_loop(NewDir, Handle1, Ref, MaxNumber, 0),
    ?line Time = case erlang:cancel_timer(Ref) of
		     false -> MaxTime;
		     T -> MaxTime - (T div 1000)
		 end,
    ?line Number = case Result of
		       {ok, N} -> N;
		       {error, _Reason, N} -> N;
		       _ -> 0
		   end,
    ?line {ok, Handle2} = ?PRIM_FILE:start(),
    ?line list_dir_limit_cleanup(NewDir, Handle2, Number, 0),
    ?line ok = ?PRIM_FILE:stop(Handle1),
    ?line ok = ?PRIM_FILE:stop(Handle2),
    ?line {ok, Number} = Result,
    ?line test_server:timetrap_cancel(Dog),
    {comment, 
     "Created " ++ integer_to_list(Number) ++ " files in " 
     ++ integer_to_list(Time) ++ " seconds."}.

list_dir_limit_loop(Dir, Handle, _Ref, N, Cnt) when Cnt >= N ->
    list_dir_check(Dir, Handle, Cnt);
list_dir_limit_loop(Dir, Handle, Ref, N, Cnt) ->
    receive 
	{timeout, Ref, []} -> 
	    list_dir_check(Dir, Handle, Cnt)
    after 0 ->
	    Name = integer_to_list(Cnt),
	    case ?PRIM_FILE:write_file(filename:join(Dir, Name), Name) of
		ok ->
		    Next = Cnt + 1,
		    case Cnt rem 100 of
			0 ->
			    case list_dir_check(Dir, Handle, Next) of
				{ok, Next} ->
				    list_dir_limit_loop(
				      Dir, Handle, Ref, N, Next);
				Other ->
				    Other
			    end;
			_ ->
			    list_dir_limit_loop(Dir, Handle, Ref, N, Next)
		    end;
		{error, Reason} ->
		    {error, Reason, Cnt}
	    end
    end.

list_dir_check(Dir, Handle, Cnt) ->
    case ?PRIM_FILE:list_dir(Handle, Dir) of
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

list_dir_limit_cleanup(Dir, Handle, N, Cnt) when Cnt >= N ->
    Name = integer_to_list(Cnt),
    case ?PRIM_FILE:delete(Handle, filename:join(Dir, Name)) of
	ok ->
	    list_dir_limit_cleanup(Dir, Handle, N, Cnt+1);
	_ ->
	    ok
    end;
list_dir_limit_cleanup(Dir, Handle, N, Cnt) ->
    Name = integer_to_list(Cnt),
    ?PRIM_FILE:delete(Handle, filename:join(Dir, Name)),
    list_dir_limit_cleanup(Dir, Handle, N, Cnt+1).

%%%
%%% Test list_dir() on a non-existing pathname.
%%%

list_dir_error(Config) ->
    Priv = ?config(priv_dir, Config),
    NonExisting = filename:join(Priv, "non-existing-dir"),
    {error,enoent} = prim_file:list_dir(NonExisting),
    ok.

%%%
%%% Test list_dir() and list_dir_all().
%%%

list_dir(Config) ->
    RootDir = ?config(priv_dir, Config),
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
	    N = unix_free(?config(priv_dir, Config)),
	    io:format("Free disk: ~w KByte~n", [N]),
	    if N < 5 bsl 20 ->
		    %% Less than 5 GByte free
		    {skip,"Less than 5 GByte free disk"};
	       true ->
		    do_run_large_file_test(Config, Run, Name)
	    end;
	_ -> 
	    {skip,"Only supported on Win32, Unix or SunOS >= 5.5.1"}
    end.


do_run_large_file_test(Config, Run, Name0) ->
    Name = filename:join(?config(priv_dir, Config),
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
		  prim_file:delete(Name)
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
