%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

%% This is a developement feature when developing a new file module,
%% ugly but practical.
-ifndef(FILE_MODULE).
-define(FILE_MODULE, file).
-endif.
-ifndef(FILE_SUITE).
-define(FILE_SUITE, file_SUITE).
-endif.
-ifndef(FILE_INIT).
-define(FILE_INIT(Config), Config).
-endif.
-ifndef(FILE_FINI).
-define(FILE_FINI(Config), Config).
-endif.
-ifndef(FILE_INIT_PER_TESTCASE).
-define(FILE_INIT_PER_TESTCASE(Config), Config).
-endif.
-ifndef(FILE_FIN_PER_TESTCASE).
-define(FILE_FIN_PER_TESTCASE(Config), Config).
-endif.

-module(?FILE_SUITE).

-export([all/1,
	 init/1, fini/1,
	 init_per_testcase/2, fin_per_testcase/2,
	 read_write_file/1, dirs/1, files/1, names/1]).
-export([cur_dir_0/1, cur_dir_1/1, make_del_dir/1,
	 pos/1, pos1/1, pos2/1]).
-export([close/1, consult/1, consult1/1, path_consult/1, delete/1]).
-export([eval/1, eval1/1, path_eval/1, script/1, script1/1, path_script/1,
	 open/1, open1/1,
	 old_modes/1, new_modes/1, path_open/1, open_errors/1]).
-export([file_info/1, file_info_basic_file/1, file_info_basic_directory/1,
	 file_info_bad/1, file_info_times/1, file_write_file_info/1]).
-export([rename/1, access/1, truncate/1, datasync/1, sync/1,
	 read_write/1, pread_write/1, append/1, exclusive/1]).
-export([errors/1, e_delete/1, e_rename/1, e_make_dir/1, e_del_dir/1]).
-export([otp_5814/1]).

-export([compression/1, read_not_really_compressed/1,
	 read_compressed_cooked/1, read_compressed_cooked_binary/1,
	 read_cooked_tar_problem/1,
	 write_compressed/1, compress_errors/1, catenated_gzips/1]).

-export([links/1, make_link/1, read_link_info_for_non_link/1, symlinks/1]).

-export([copy/1]).

-export([new_slave/2, old_slave/2, run_test/2]).

-export([delayed_write/1, read_ahead/1, segment_read/1, segment_write/1]).

-export([ipread/1]).

-export([pid2name/1]).

-export([interleaved_read_write/1]).

-export([altname/1]).

-export([large_file/1]).

-export([read_line_1/1, read_line_2/1, read_line_3/1,read_line_4/1]).

-export([advise/1]).

-export([standard_io/1,mini_server/1]).

%% Debug exports
-export([create_file_slow/2, create_file/2, create_bin/2]).
-export([verify_file/2, verify_bin/3]).
-export([bytes/2, iterate/3]).



-include("test_server.hrl").
-include_lib("kernel/include/file.hrl").



all(suite) ->
    {conf, init,
     [altname, read_write_file, dirs, files, 
      delete, rename, names, errors,
      compression, links, copy,
      delayed_write, read_ahead, segment_read, segment_write,
      ipread, pid2name, interleaved_read_write, 
      otp_5814, large_file, read_line_1, read_line_2, read_line_3, read_line_4,
      standard_io],
     fini}.

init(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    Priv = ?config(priv_dir, Config),
	    HasAccessTime =
		case ?FILE_MODULE:read_file_info(Priv) of
		    {ok, #file_info{atime={_, {0, 0, 0}}}} ->
			%% This is a unfortunately a FAT file system.
			[no_access_time];
		    {ok, _} ->
			[]
		end,
	    ?FILE_INIT(HasAccessTime++Config);
	_ ->
	    ?FILE_INIT(Config)
    end.

fini(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    os:cmd("subst z: /d");
	_ ->
	    ok
    end,
    ?FILE_FINI(Config).

init_per_testcase(_Func, Config) ->
    %%error_logger:info_msg("~p:~p *****~n", [?MODULE, _Func]),
    ?FILE_INIT_PER_TESTCASE(Config).

fin_per_testcase(_Func, Config) ->
    %% error_logger:info_msg("~p:~p END *****~n", [?MODULE, _Func]),
    ?FILE_FIN_PER_TESTCASE(Config).

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
mini_server(Parent) ->
    receive
	die ->
	    ok;
	{io_request,From,To,{put_chars,Data}} ->
	    Parent ! {io_request,From,To,{put_chars,Data}},
	    From ! {io_reply, To, ok},
	    mini_server(Parent);
	{io_request,From,To,{get_chars,'',N}} ->
	    Parent ! {io_request,From,To,{get_chars,'',N}},
	    From ! {io_reply, To, {ok, lists:duplicate(N,$a)}},
	    mini_server(Parent);
	{io_request,From,To,{get_line,''}} ->
	    Parent ! {io_request,From,To,{get_line,''}},
	    From ! {io_reply, To, {ok, "hej\n"}},
	    mini_server(Parent)
    end.

standard_io(suite) ->
    [];
standard_io(doc) ->
    ["Test that standard i/o-servers work with file module"];
standard_io(Config) when is_list(Config) ->
    %% Really just a smoke test
    ?line Pid = spawn(?MODULE,mini_server,[self()]),
    ?line register(mini_server,Pid),
    ?line ok = file:write(mini_server,<<"hej\n">>),
    ?line receive
	     {io_request,_,_,{put_chars,<<"hej\n">>}} ->
		  ok
	  after 1000 ->
		  exit(noreply)
	  end,
    ?line {ok,"aaaaa"} = file:read(mini_server,5),
    ?line receive
	     {io_request,_,_,{get_chars,'',5}} ->
		  ok
	  after 1000 ->
		  exit(noreply)
	  end,
    ?line {ok,"hej\n"} = file:read_line(mini_server),
    ?line receive
	     {io_request,_,_,{get_line,''}} ->
		  ok
	  after 1000 ->
		  exit(noreply)
	  end,
    ?line OldGL = group_leader(),
    ?line group_leader(Pid,self()),
    ?line ok = file:write(standard_io,<<"hej\n">>),
    ?line group_leader(OldGL,self()),
    ?line receive
	     {io_request,_,_,{put_chars,<<"hej\n">>}} ->
		  ok
	  after 1000 ->
		  exit(noreply)
	  end,
    ?line group_leader(Pid,self()),
    ?line {ok,"aaaaa"} = file:read(standard_io,5),
    ?line group_leader(OldGL,self()),
    ?line receive
	     {io_request,_,_,{get_chars,'',5}} ->
		  ok
	  after 1000 ->
		  exit(noreply)
	  end,
    ?line group_leader(Pid,self()),
    ?line {ok,"hej\n"} = file:read_line(standard_io),
    ?line group_leader(OldGL,self()),
    ?line receive
	     {io_request,_,_,{get_line,''}} ->
		  ok
	  after 1000 ->
		  exit(noreply)
	  end,
    Pid ! die,
    receive after 1000 -> ok end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_write_file(suite) -> [];
read_write_file(doc) -> [];
read_write_file(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_read_write_file"),

    %% Try writing and reading back some term
    ?line SomeTerm = {"This term",{will,be},[written,$t,$o],1,file,[]},
    ?line ok = ?FILE_MODULE:write_file(Name,term_to_binary(SomeTerm)),
    ?line {ok,Bin1} = ?FILE_MODULE:read_file(Name),
    ?line SomeTerm = binary_to_term(Bin1),
  
    %% Try a "null" term
    ?line NullTerm = [],
    ?line ok = ?FILE_MODULE:write_file(Name,term_to_binary(NullTerm)),
    ?line {ok,Bin2} = ?FILE_MODULE:read_file(Name),
    ?line NullTerm = binary_to_term(Bin2),

    %% Try some "complicated" types
    ?line BigNum = 123456789012345678901234567890,
    ?line ComplTerm = {self(),make_ref(),BigNum,3.14159},
    ?line ok = ?FILE_MODULE:write_file(Name,term_to_binary(ComplTerm)),
    ?line {ok,Bin3} = ?FILE_MODULE:read_file(Name),
    ?line ComplTerm = binary_to_term(Bin3),

    %% Try reading a nonexistent file
    ?line Name2 = filename:join(RootDir, 
				atom_to_list(?MODULE)
				++"_nonexistent_file"),
    ?line {error, enoent} = ?FILE_MODULE:read_file(Name2),
    ?line {error, enoent} = ?FILE_MODULE:read_file(""),
    ?line {error, enoent} = ?FILE_MODULE:read_file(''),

    % Try writing to a bad filename
    ?line {error, enoent} = 
	?FILE_MODULE:write_file("",term_to_binary(NullTerm)),

    % Try writing something else than a binary
    ?line {error, badarg} = ?FILE_MODULE:write_file(Name,{1,2,3}),
    ?line {error, badarg} = ?FILE_MODULE:write_file(Name,self()),

    %% Some non-term binaries
    ?line ok = ?FILE_MODULE:write_file(Name,[]),
    ?line {ok,Bin4} = ?FILE_MODULE:read_file(Name),
    ?line 0 = byte_size(Bin4),

    ?line ok = ?FILE_MODULE:write_file(Name,[Bin1,[],[[Bin2]]]),
    ?line {ok,Bin5} = ?FILE_MODULE:read_file(Name),
    ?line {Bin1,Bin2} = split_binary(Bin5,byte_size(Bin1)),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirs(suite) -> [make_del_dir, cur_dir_0, cur_dir_1].

make_del_dir(suite) -> [];
make_del_dir(doc) -> [];
make_del_dir(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_mk-dir"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line {error, eexist} = ?FILE_MODULE:make_dir(NewDir),
    ?line ok = ?FILE_MODULE:del_dir(NewDir),
    ?line {error, enoent} = ?FILE_MODULE:del_dir(NewDir),

    %% Check that we get an error when trying to create...
    %% a deep directory
    ?line NewDir2 = filename:join(RootDir, 
				  atom_to_list(?MODULE)
				  ++"_mk-dir/foo"),
    ?line {error, enoent} = ?FILE_MODULE:make_dir(NewDir2),
    %% a nameless directory
    ?line {error, enoent} = ?FILE_MODULE:make_dir(""),
    %% a directory with illegal name
    ?line {error, badarg} = ?FILE_MODULE:make_dir({1,2,3}),

    %% a directory with illegal name, even if it's a (bad) list
    ?line {error, badarg} = ?FILE_MODULE:make_dir([1,2,3,{}]),

    %% Maybe this isn't an error, exactly, but worth mentioning anyway:
    %% ok = ?FILE_MODULE:make_dir([$f,$o,$o,0,$b,$a,$r])),
    %% The above line works, and created a directory "./foo"
    %% More elegant would maybe have been to fail, or to really create
    %% a directory, but with a name that incorporates the "bar" part of
    %% the list, so that [$f,$o,$o,0,$f,$o,$o] wouldn't refer to the same
    %% dir. But this would slow it down.

    %% Try deleting some bad directories
    %% Deleting the parent directory to the current, sounds dangerous, huh?
    %% Don't worry ;-) the parent directory should never be empty, right?
    case ?FILE_MODULE:del_dir('..') of
	{error, eexist} -> ok;
	{error, einval} -> ok			%FreeBSD
    end,
    ?line {error, enoent} = ?FILE_MODULE:del_dir(""),
    ?line {error, badarg} = ?FILE_MODULE:del_dir([3,2,1,{}]),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

cur_dir_0(suite) -> [];
cur_dir_0(doc) -> [];
cur_dir_0(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    %% Find out the current dir, and cd to it ;-)
    ?line {ok,BaseDir} = ?FILE_MODULE:get_cwd(),
    ?line Dir1 = BaseDir ++ "", %% Check that it's a string
    ?line ok = ?FILE_MODULE:set_cwd(Dir1),

    %% Make a new dir, and cd to that
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_curdir"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line io:format("cd to ~s",[NewDir]),
    ?line ok = ?FILE_MODULE:set_cwd(NewDir),

    %% Create a file in the new current directory, and check that it
    %% really is created there
    ?line UncommonName = "uncommon.fil",
    ?line {ok,Fd} = ?FILE_MODULE:open(UncommonName,read_write),
    ?line ok = ?FILE_MODULE:close(Fd),
    ?line {ok,NewDirFiles} = ?FILE_MODULE:list_dir("."),
    ?line true = lists:member(UncommonName,NewDirFiles),

    %% Delete the directory and return to the old current directory
    %% and check that the created file isn't there (too!)
    ?line expect({error, einval}, {error, eacces}, 
		 ?FILE_MODULE:del_dir(NewDir)),
    ?line ?FILE_MODULE:delete(UncommonName),
    ?line {ok,[]} = ?FILE_MODULE:list_dir("."),
    ?line ok = ?FILE_MODULE:set_cwd(Dir1),
    ?line io:format("cd back to ~s",[Dir1]),
    ?line ok = ?FILE_MODULE:del_dir(NewDir),
    ?line {error, enoent} = ?FILE_MODULE:set_cwd(NewDir),
    ?line ok = ?FILE_MODULE:set_cwd(Dir1),
    ?line io:format("cd back to ~s",[Dir1]),
    ?line {ok,OldDirFiles} = ?FILE_MODULE:list_dir("."),
    ?line false = lists:member(UncommonName,OldDirFiles),

    %% Try doing some bad things
    ?line {error, badarg} = ?FILE_MODULE:set_cwd({foo,bar}),
    ?line {error, enoent} = ?FILE_MODULE:set_cwd(""),
    ?line {error, enoent} = ?FILE_MODULE:set_cwd(".......a......"),
    ?line {ok,BaseDir} = ?FILE_MODULE:get_cwd(), %% Still there?

    %% On Windows, there should only be slashes, no backslashes,
    %% in the return value of get_cwd().
    %% (The test is harmless on Unix, because filenames usually
    %% don't contain backslashes.)

    ?line {ok, BaseDir} = ?FILE_MODULE:get_cwd(),
    ?line false = lists:member($\\, BaseDir),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests ?FILE_MODULE:get_cwd/1.

cur_dir_1(suite) -> [];
cur_dir_1(doc) -> [];
cur_dir_1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),

    ?line case os:type() of
	      {unix, _} ->
		  ?line {error, enotsup} = ?FILE_MODULE:get_cwd("d:");
	      vxworks ->
		  ?line {error, enotsup} = ?FILE_MODULE:get_cwd("d:");
	      {win32, _} ->
		  win_cur_dir_1(Config)
	  end,
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.
	
win_cur_dir_1(_Config) ->
    ?line {ok,BaseDir} = ?FILE_MODULE:get_cwd(),

    %% Get the drive letter from the current directory,
    %% and try to get current directory for that drive.

    ?line [Drive,$:|_] = BaseDir,
    ?line {ok,BaseDir} = ?FILE_MODULE:get_cwd([Drive,$:]),
    io:format("BaseDir = ~s\n", [BaseDir]),

    %% Unfortunately, there is no way to move away from the
    %% current drive as we can't use the "subst" command from
    %% a SSH connection. We can't test any more.

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files(suite) ->
    [open,pos,file_info,consult,eval,script,truncate,
     sync,datasync,advise].

open(suite) -> [open1,old_modes,new_modes,path_open,close,access,read_write,
	       pread_write,append,open_errors,exclusive].

open1(suite) -> [];
open1(doc) -> [];
open1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_files"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line Name = filename:join(NewDir, "foo1.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,read_write),
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,read),
    ?line Str = "{a,tuple}.\n",
    ?line io:format(Fd1,Str,[]),
    ?line {ok,0} = ?FILE_MODULE:position(Fd1,bof),
    ?line Str = io:get_line(Fd1,''),
    ?line Str = io:get_line(Fd2,''),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok,0} = ?FILE_MODULE:position(Fd1,bof),
    ?line ok = ?FILE_MODULE:truncate(Fd1),
    ?line eof = io:get_line(Fd1,''),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,Fd3} = ?FILE_MODULE:open(Name,read),
    ?line eof = io:get_line(Fd3,''),
    ?line ok = ?FILE_MODULE:close(Fd3),
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests all open modes.

old_modes(suite) -> [];
old_modes(doc) -> [];
old_modes(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_old_open_modes"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line Name1 = filename:join(NewDir, "foo1.fil"),
    ?line Marker = "hello, world",

    %% write
    ?line {ok, Fd1} = ?FILE_MODULE:open(Name1, write),
    ?line ok = io:write(Fd1, Marker),
    ?line ok = io:put_chars(Fd1, ".\n"),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% read
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name1, read),
    ?line {ok, Marker} = io:read(Fd2, prompt),
    ?line ok = ?FILE_MODULE:close(Fd2),

    %% read_write
    ?line {ok, Fd3} = ?FILE_MODULE:open(Name1, read_write),
    ?line {ok, Marker} = io:read(Fd3, prompt),
    ?line ok = io:write(Fd3, Marker),
    ?line ok = ?FILE_MODULE:close(Fd3),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.


new_modes(suite) -> [];
new_modes(doc) -> [];
new_modes(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_new_open_modes"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line Name1 = filename:join(NewDir, "foo1.fil"),
    ?line Marker = "hello, world",

    %% write
    ?line {ok, Fd1} = ?FILE_MODULE:open(Name1, [write]),
    ?line ok = io:write(Fd1, Marker),
    ?line ok = io:put_chars(Fd1, ".\n"),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% read
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name1, [read]),
    ?line {ok, Marker} = io:read(Fd2, prompt),
    ?line ok = ?FILE_MODULE:close(Fd2),

    %% read and write
    ?line {ok, Fd3} = ?FILE_MODULE:open(Name1, [read, write]),
    ?line {ok, Marker} = io:read(Fd3, prompt),
    ?line ok = io:write(Fd3, Marker),
    ?line ok = ?FILE_MODULE:close(Fd3),

    %% read by default
    ?line {ok, Fd4} = ?FILE_MODULE:open(Name1, []),
    ?line {ok, Marker} = io:read(Fd4, prompt),
    ?line ok = ?FILE_MODULE:close(Fd4),

    %% read and binary
    ?line {ok, Fd5} = ?FILE_MODULE:open(Name1, [read, binary]),
    ?line {ok, Marker} = io:read(Fd5, prompt),
    ?line ok = ?FILE_MODULE:close(Fd5),

    %% read, raw
    ?line {ok, Fd6} = ?FILE_MODULE:open(Name1, [read, raw]),
    ?line {ok, [$\[]} = ?FILE_MODULE:read(Fd6, 1),
    ?line ok = ?FILE_MODULE:close(Fd6),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

path_open(suite) -> [];
path_open(doc) -> [];
path_open(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_path_open"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line FileName = "path_open.fil",
    ?line Name = filename:join(RootDir, FileName),
    ?line {ok,Fd1,_FullName1} =
	?FILE_MODULE:path_open(
	  [RootDir,
	   "nosuch1",
	   NewDir],FileName,write),
    ?line io:format(Fd1,"ABCDEFGH",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% locate it in the last dir
    ?line {ok,Fd2,_FullName2} =
	?FILE_MODULE:path_open(
	  ["nosuch1",
	   NewDir,
	   RootDir],FileName,read),
    ?line {ok,2} = 
	?FILE_MODULE:position(Fd2,2), "C" = io:get_chars(Fd2,'',1),
    ?line ok = ?FILE_MODULE:close(Fd2),
    %% Try a failing path
    ?line {error, enoent} = ?FILE_MODULE:path_open(
			      ["nosuch1",
			       NewDir],FileName,read),
    %% Check that it's found regardless of path, if an absolute name given
    ?line {ok,Fd3,_FullPath3} =
	?FILE_MODULE:path_open(
	  ["nosuch1",
	   NewDir],Name,read),
    ?line {ok,2} = 
	?FILE_MODULE:position(Fd3,2), "C" = io:get_chars(Fd3,'',1),
    ?line ok = ?FILE_MODULE:close(Fd3),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

close(suite) -> [];
close(doc) -> [];
close(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_close.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,read_write),
    %% Just closing it is no fun, we did that a million times already
    %% This is a common error, for code written before Erlang 4.3
    %% bacause then ?FILE_MODULE:open just returned a Pid, and not everyone
    %% really checked what they got.
    ?line {'EXIT',_Msg} = (catch ok = ?FILE_MODULE:close({ok,Fd1})),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% Try closing one more time
    ?line Val = ?FILE_MODULE:close(Fd1),
    ?line io:format("Second close gave: ~p",[Val]),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

access(suite) -> [];
access(doc) -> [];
access(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_access.fil"),
    ?line Str = "ABCDEFGH",
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd1,Str,[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    %% Check that we can't write when in read only mode
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,read),
    ?line case catch io:format(Fd2,"XXXX",[]) of
	      ok ->
		  test_server:fail({format,write});
	      _ ->
		  ok
	  end,
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok,Fd3} = ?FILE_MODULE:open(Name,read),
    ?line Str = io:get_line(Fd3,''),
    ?line ok = ?FILE_MODULE:close(Fd3),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests ?FILE_MODULE:read/2 and ?FILE_MODULE:write/2.

read_write(suite) -> [];
read_write(doc) -> [];
read_write(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_read_write"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line Marker = "hello, world",
    ?line MarkerB = list_to_binary(Marker),

    %% Plain file.
    ?line Name1 = filename:join(NewDir, "plain.fil"),
    ?line {ok, Fd1} = ?FILE_MODULE:open(Name1, [read, write]),
    ?line read_write_test(Fd1, Marker, []),

    %% Raw file.
    ?line Name2 = filename:join(NewDir, "raw.fil"),
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name2, [read, write, raw]),
    ?line read_write_test(Fd2, Marker, []),

    %% Plain binary file.
    ?line Name3 = filename:join(NewDir, "plain-b.fil"),
    ?line {ok, Fd3} = ?FILE_MODULE:open(Name3, [read, write, binary]),
    ?line read_write_test(Fd3, MarkerB, <<>>),

    %% Raw binary file.
    ?line Name4 = filename:join(NewDir, "raw-b.fil"),
    ?line {ok, Fd4} = ?FILE_MODULE:open(Name4, [read, write, raw, binary]),
    ?line read_write_test(Fd4, MarkerB, <<>>),

    ?line test_server:timetrap_cancel(Dog),
    ok.

read_write_test(File, Marker, Empty) ->
    ?line ok = ?FILE_MODULE:write(File, Marker),
    ?line {ok, 0} = ?FILE_MODULE:position(File, 0),
    ?line {ok, Empty} = ?FILE_MODULE:read(File, 0),
    ?line {ok, Marker} = ?FILE_MODULE:read(File, 100),
    ?line eof = ?FILE_MODULE:read(File, 100),
    ?line {ok, Empty} = ?FILE_MODULE:read(File, 0),
    ?line ok = ?FILE_MODULE:close(File),
    ?line [] = flush(),
    ok.


%% Tests ?FILE_MODULE:pread/2 and ?FILE_MODULE:pwrite/2.

pread_write(suite) -> [];
pread_write(doc) -> [];
pread_write(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_pread_write"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line List = "hello, world",
    ?line Bin = list_to_binary(List),

    %% Plain file.
    ?line Name1 = filename:join(NewDir, "plain.fil"),
    ?line {ok, Fd1} = ?FILE_MODULE:open(Name1, [read, write]),
    ?line pread_write_test(Fd1, List),

    %% Raw file.
    ?line Name2 = filename:join(NewDir, "raw.fil"),
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name2, [read, write, raw]),
    ?line pread_write_test(Fd2, List),

    %% Plain file. Binary mode.
    ?line Name3 = filename:join(NewDir, "plain-binary.fil"),
    ?line {ok, Fd3} = ?FILE_MODULE:open(Name3, [binary, read, write]),
    ?line pread_write_test(Fd3, Bin),

    %% Raw file. Binary mode.
    ?line Name4 = filename:join(NewDir, "raw-binary.fil"),
    ?line {ok, Fd4} = ?FILE_MODULE:open(Name4, [binary, read, write, raw]),
    ?line pread_write_test(Fd4, Bin),

    ?line test_server:timetrap_cancel(Dog),
    ok.

pread_write_test(File, Data) ->
    ?line io:format("~p:pread_write_test(~p,~p)~n", [?MODULE, File, Data]),
    ?line Size = if is_binary(Data) -> byte_size(Data);
		    is_list(Data) -> length(Data)
		 end,
    ?line I = Size + 17,
    ?line ok = ?FILE_MODULE:pwrite(File, 0, Data),
    Res = ?FILE_MODULE:pread(File, 0, I),
    ?line {ok, Data} = Res,
    ?line eof = ?FILE_MODULE:pread(File, I, 1),
    ?line ok = ?FILE_MODULE:pwrite(File, [{0, Data}, {I, Data}]),
    ?line {ok, [Data, eof, Data]} = 
	?FILE_MODULE:pread(File, [{0, Size}, {2*I, 1}, {I, Size}]),
    ?line Plist = lists:seq(21*I, 0, -I),
    ?line Pwrite = lists:map(fun(P)->{P,Data}end, Plist),
    ?line Pread = [{22*I,Size} | lists:map(fun(P)->{P,Size}end, Plist)],
    ?line Presult = [eof | lists:map(fun(_)->Data end, Plist)],
    ?line ok = ?FILE_MODULE:pwrite(File, Pwrite),
    ?line {ok, Presult} = ?FILE_MODULE:pread(File, Pread),
    ?line ok = ?FILE_MODULE:close(File),
    ?line [] = flush(),
    ok.

append(doc) -> "Test appending to a file.";
append(suite) -> [];
append(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_append"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),

    ?line First = "First line\n",
    ?line Second = "Seond lines comes here\n",
    ?line Third = "And here is the third line\n",

    %% Write a small text file.
    ?line Name1 = filename:join(NewDir, "a_file.txt"),
    ?line {ok, Fd1} = ?FILE_MODULE:open(Name1, [write]),
    ?line ok = io:format(Fd1, First, []),
    ?line ok = io:format(Fd1, Second, []),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% Open it a again and a append a line to it.
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name1, [append]),
    ?line ok = io:format(Fd2, Third, []),
    ?line ok = ?FILE_MODULE:close(Fd2),

    %% Read it back and verify.
    ?line Expected = list_to_binary([First, Second, Third]),
    ?line {ok, Expected} = ?FILE_MODULE:read_file(Name1),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

open_errors(suite) -> [];
open_errors(doc) -> [];
open_errors(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line DataDir = 
	filename:dirname(
	  filename:join(?config(data_dir, Config), "x")),
    ?line DataDirSlash = DataDir++"/",
    ?line {error, E1} = ?FILE_MODULE:open(DataDir, [read]),
    ?line {error, E2} = ?FILE_MODULE:open(DataDirSlash, [read]),
    ?line {error, E3} = ?FILE_MODULE:open(DataDir, [write]),
    ?line {error, E4} = ?FILE_MODULE:open(DataDirSlash, [write]),
    ?line {eisdir,eisdir,eisdir,eisdir} = {E1,E2,E3,E4},

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

exclusive(suite) -> [];
exclusive(doc) -> "Test exclusive access to a file.";
exclusive(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line NewDir = filename:join(RootDir,
				 atom_to_list(?MODULE)
				 ++"_exclusive"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line Name = filename:join(NewDir, "ex_file.txt"),
    ?line {ok, Fd} = ?FILE_MODULE:open(Name, [write, exclusive]),
    ?line {error, eexist} = ?FILE_MODULE:open(Name, [write, exclusive]),
    ?line ok = ?FILE_MODULE:close(Fd),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pos(suite) -> [pos1,pos2].

pos1(suite) -> [];
pos1(doc) -> [];
pos1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_pos1.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd1,"ABCDEFGH",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,read),

    %% Start pos is first char
    ?line io:format("Relative positions"),
    ?line "A" = io:get_chars(Fd2,'',1),
    ?line {ok,2} = ?FILE_MODULE:position(Fd2,{cur,1}), 
    ?line "C" = io:get_chars(Fd2,'',1),
    ?line {ok,0} = ?FILE_MODULE:position(Fd2,{cur,-3}), 
    ?line "A" = io:get_chars(Fd2,'',1),
    %% Backwards from first char should be an error
    ?line {ok,0} = ?FILE_MODULE:position(Fd2,{cur,-1}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd2,{cur,-1}),
    %% Reset position and move again
    ?line {ok,0} = ?FILE_MODULE:position(Fd2,0),
    ?line {ok,2} = ?FILE_MODULE:position(Fd2,{cur,2}), 
    ?line "C" = io:get_chars(Fd2,'',1),
    %% Go a lot forwards
    ?line {ok,13} = ?FILE_MODULE:position(Fd2,{cur,10}), 
    ?line eof = io:get_chars(Fd2,'',1),

    %% Try some fixed positions
    ?line io:format("Fixed positions"),
    ?line {ok,8} = 
	?FILE_MODULE:position(Fd2,8), eof = io:get_chars(Fd2,'',1),
    ?line {ok,8} = 
	?FILE_MODULE:position(Fd2,cur), eof = io:get_chars(Fd2,'',1),
    ?line {ok,7} = 
	?FILE_MODULE:position(Fd2,7), "H" = io:get_chars(Fd2,'',1),
    ?line {ok,0} = 
	?FILE_MODULE:position(Fd2,0), "A" = io:get_chars(Fd2,'',1),
    ?line {ok,3} = 
	?FILE_MODULE:position(Fd2,3), "D" = io:get_chars(Fd2,'',1),
    ?line {ok,12} = 
	?FILE_MODULE:position(Fd2,12), eof = io:get_chars(Fd2,'',1),
    ?line {ok,3} = 
	?FILE_MODULE:position(Fd2,3), "D" = io:get_chars(Fd2,'',1),
    %% Try the {bof,X} notation
    ?line {ok,3} = ?FILE_MODULE:position(Fd2,{bof,3}),
    ?line "D" = io:get_chars(Fd2,'',1),

    %% Try eof positions
    ?line io:format("EOF positions"),
    ?line {ok,8} = 
	?FILE_MODULE:position(Fd2,{eof,0}), eof=io:get_chars(Fd2,'',1),
    ?line {ok,7} = 
	?FILE_MODULE:position(Fd2,{eof,-1}),
    ?line "H" = io:get_chars(Fd2,'',1),
    ?line {ok,0} = 
	?FILE_MODULE:position(Fd2,{eof,-8}), "A"=io:get_chars(Fd2,'',1),
    ?line {error, einval} = ?FILE_MODULE:position(Fd2,{eof,-9}),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

pos2(suite) -> [];
pos2(doc) -> [];
pos2(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_pos2.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd1,"ABCDEFGH",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,read),
    ?line {error, einval} = ?FILE_MODULE:position(Fd2,-1),

    %% Make sure that we still can search after an error.
    ?line {ok,0} = ?FILE_MODULE:position(Fd2, 0),
    ?line {ok,3} = ?FILE_MODULE:position(Fd2, {bof,3}),
    ?line "D" = io:get_chars(Fd2,'',1),

    ?line [] = flush(),
    ?line io:format("DONE"),
    ?line test_server:timetrap_cancel(Dog),
    ok.

file_info(suite) -> [file_info_basic_file, file_info_basic_directory,
		     file_info_bad, file_info_times, file_write_file_info].

file_info_basic_file(suite) -> [];
file_info_basic_file(doc) -> [];
file_info_basic_file(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir, Config),

    %% Create a short file.
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_basic_test.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name, write),
    ?line io:put_chars(Fd1, "foo bar"),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% Test that the file has the expected attributes.
    %% The times are tricky, so we will save them to a separate test case.
    ?line {ok,#file_info{size=Size,type=Type,access=Access,
			 atime=AccessTime,mtime=ModifyTime}} =
	?FILE_MODULE:read_file_info(Name),
    ?line io:format("Access ~p, Modify ~p", [AccessTime, ModifyTime]),
    ?line Size = 7,
    ?line Type = regular,
    ?line read_write = Access,
    ?line true = abs(time_dist(filter_atime(AccessTime, Config),
			       filter_atime(ModifyTime,
					    Config))) < 2,
    ?line all_integers(tuple_to_list(AccessTime) ++ tuple_to_list(ModifyTime)),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

file_info_basic_directory(suite) -> [];
file_info_basic_directory(doc) -> [];
file_info_basic_directory(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),

    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?FILE_MODULE:file_info/1 to work on
    %% platforms such as Windows95.
    ?line RootDir = filename:join([?config(priv_dir, Config)]),

    %% Test that the RootDir directory has the expected attributes.
    ?line test_directory(RootDir, read_write),

    %% Note that on Windows file systems, 
    %% "/" or "c:/" are *NOT* directories.
    %% Therefore, test that ?FILE_MODULE:file_info/1 behaves as if they were
    %% directories.
    ?line case os:type() of
	      {win32, _} ->
		  ?line test_directory("/", read_write),
		  ?line test_directory("c:/", read_write),
		  ?line test_directory("c:\\", read_write);
	      {unix, _} ->
		  ?line test_directory("/", read);
	      vxworks ->
		  %% Check is just done for owner
		  ?line test_directory("/", read_write)
	  end,
    ?line test_server:timetrap_cancel(Dog).

test_directory(Name, ExpectedAccess) ->
    ?line {ok,#file_info{size=Size,type=Type,access=Access,
			 atime=AccessTime,mtime=ModifyTime}} =
	?FILE_MODULE:read_file_info(Name),
    ?line io:format("Testing directory ~s", [Name]),
    ?line io:format("Directory size is ~p", [Size]),
    ?line io:format("Access ~p", [Access]),
    ?line io:format("Access time ~p; Modify time~p", 
		    [AccessTime, ModifyTime]),
    ?line Type = directory,
    ?line Access = ExpectedAccess,
    ?line all_integers(tuple_to_list(AccessTime) ++ tuple_to_list(ModifyTime)),
    ?line [] = flush(),
    ok.

all_integers([{A,B,C}|T]) ->
    all_integers([A,B,C|T]);
all_integers([Int|Rest]) when is_integer(Int) ->
    ?line all_integers(Rest);
all_integers([]) -> ok.

%% Try something nonexistent.

file_info_bad(suite) -> [];
file_info_bad(doc) -> [];
file_info_bad(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = filename:join([?config(priv_dir, Config)]),
    ?line {error, enoent} = 
	?FILE_MODULE:read_file_info(
	  filename:join(RootDir, 
			atom_to_list(?MODULE)++ "_nonexistent")),
    ?line {error, enoent} = ?FILE_MODULE:read_file_info(""),
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Test that the file times behave as they should.

file_info_times(suite) -> [];
file_info_times(doc) -> [];
file_info_times(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    %% We have to try this twice, since if the test runs across the change
    %% of a month the time diff calculations will fail. But it won't happen
    %% if you run it twice in succession.
    ?line test_server:m_out_of_n(
	    1,2,
	    fun() -> ?line file_info_int(Config) end),
    ?line test_server:timetrap_cancel(Dog),
    ok.

file_info_int(Config) ->
    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?FILE_MODULE:file_info/1 to work on
    %% platforms such as Windows95.

    ?line RootDir = filename:join([?config(priv_dir, Config)]),
    ?line test_server:format("RootDir = ~p", [RootDir]),

    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_file_info.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:put_chars(Fd1,"foo"),

    %% check that the file got a modify date max a few seconds away from now
    ?line {ok,#file_info{type=regular,atime=AccTime1,mtime=ModTime1}} =
	?FILE_MODULE:read_file_info(Name),
    ?line Now = erlang:localtime(), %???
    ?line io:format("Now ~p",[Now]),
    ?line io:format("Open file Acc ~p Mod ~p",[AccTime1,ModTime1]),
    ?line true = abs(time_dist(filter_atime(Now, Config),
			       filter_atime(AccTime1,
					    Config))) < 8,
    ?line true = abs(time_dist(Now,ModTime1)) < 8,

    %% Sleep until we can be sure the seconds value has changed.
    %% Note: FAT-based filesystem (like on Windows 95) have
    %% a resolution of 2 seconds.
    ?line test_server:sleep(test_server:seconds(2.2)),

    %% close the file, and watch the modify date change
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,#file_info{size=Size,type=regular,access=Access,
			 atime=AccTime2,mtime=ModTime2}} =
	?FILE_MODULE:read_file_info(Name),
    ?line io:format("Closed file Acc ~p Mod ~p",[AccTime2,ModTime2]),
    ?line true = time_dist(ModTime1,ModTime2) >= 0,

    %% this file is supposed to be binary, so it'd better keep it's size
    ?line Size = 3,
    ?line Access = read_write,

    %% Do some directory checking
    ?line {ok,#file_info{size=DSize,type=directory,access=DAccess,
			 atime=AccTime3,mtime=ModTime3}} =
	?FILE_MODULE:read_file_info(RootDir),
    %% this dir was modified only a few secs ago
    ?line io:format("Dir Acc ~p; Mod ~p; Now ~p", [AccTime3, ModTime3, Now]),
    ?line true = abs(time_dist(Now,ModTime3)) < 5,
    ?line DAccess = read_write,
    ?line io:format("Dir size is ~p",[DSize]),

    ?line [] = flush(),
    ok.

%% Filter access times, to copy with a deficiency of FAT file systems
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

file_write_file_info(suite) -> [];
file_write_file_info(doc) -> [];
file_write_file_info(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = get_good_directory(Config),
    ?line test_server:format("RootDir = ~p", [RootDir]),

    %% Set the file to read only AND update the file times at the same time.
    %% (This used to fail on Windows NT/95 for a local filesystem.)
    %% Note: Seconds must be even; see note in file_info_times/1.

    ?line Name1 = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_write_file_info_ro"),
    ?line ok = ?FILE_MODULE:write_file(Name1, "hello"),
    ?line Time = {{1997, 01, 02}, {12, 35, 42}},
    ?line Info = #file_info{mode=8#400, atime=Time, mtime=Time, ctime=Time},
    ?line ok = ?FILE_MODULE:write_file_info(Name1, Info),

    %% Read back the times.

    ?line {ok, ActualInfo} = ?FILE_MODULE:read_file_info(Name1),
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
    ?line {error, eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Make the file writable again.

    ?line ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#600}),
    ?line ok = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% And unwritable.
    ?line ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#400}),
    ?line {error, eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Write the times again.
    %% Note: Seconds must be even; see note in file_info_times/1.

    ?line NewTime = {{1997, 02, 15}, {13, 18, 20}},
    ?line NewInfo = #file_info{atime=NewTime, mtime=NewTime, ctime=NewTime},
    ?line ok = ?FILE_MODULE:write_file_info(Name1, NewInfo),
    ?line {ok, ActualInfo2} = ?FILE_MODULE:read_file_info(Name1),
    ?line #file_info{atime=NewActAtime, mtime=NewTime,
		     ctime=NewActCtime} = ActualInfo2,
    ?line NewFilteredAtime = filter_atime(NewTime, Config),
    ?line NewFilteredAtime = filter_atime(NewActAtime, Config),
    ?line case os:type() of
	      {win32, _} -> NewActCtime = NewTime;
	      _ -> ok
	  end,

    %% The file should still be unwritable.
    ?line {error, eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Make the file writeable again, so that we can remove the
    %% test suites ... :-)
    ?line ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#600}),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Returns a directory on a file system that has correct file times.

get_good_directory(Config) ->
    ?line ?config(priv_dir, Config).

consult(suite) -> [consult1, path_consult].

consult1(suite) -> [];
consult1(doc) -> [];
consult1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_consult.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    ?line io:format(Fd1,
		    "{this,[is,1.0],'journey'}.\n\"into\". (sound). ",
		    []),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,[{this,[is,1.0],journey},"into",sound]} = 
	?FILE_MODULE:consult(Name),

    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    %% note the missing double quote
    ?line io:format(
	    Fd2,"{this,[is,1.0],'journey'}.\n \"into. (sound). ",[]),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {error, {_, _, _} = Msg} = ?FILE_MODULE:consult(Name),
    ?line io:format("Errmsg: ~p",[Msg]),

    ?line {error, enoent} = ?FILE_MODULE:consult(Name ++ ".nonexistent"),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

path_consult(suite) -> [];
path_consult(doc) -> [];
path_consult(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line FileName = atom_to_list(?MODULE)++"_path_consult.fil",
    ?line Name = filename:join(RootDir, FileName),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd1,"{this,is,a,journey,into,sound}.\n",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    %% File last in path
    ?line {ok,[{this,is,a,journey,into,sound}],Dir} =
	?FILE_MODULE:path_consult(
	  [filename:join(RootDir, "dir1"),
	   filename:join(RootDir, ".."),
	   filename:join(RootDir, "dir2"),
	   RootDir], FileName),
    ?line true = lists:prefix(RootDir,Dir),

    %% While maybe not an error, it may be worth noting that
    %% when the full path to a file is given, it's always found
    %% regardless of the contents of the path
    ?line {ok,_,_} = ?FILE_MODULE:path_consult(["nosuch1","nosuch2"],Name),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

eval(suite) -> [eval1,path_eval].

eval1(suite) -> [];
eval1(doc) -> [];
eval1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_eval.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    ?line io:format(Fd1,"put(evaluated_ok,\ntrue). ",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line ok = ?FILE_MODULE:eval(Name),
    ?line true = get(evaluated_ok),
    
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    ?line io:format(Fd2,"put(evaluated_ok,\nR). ",[]),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line ok = ?FILE_MODULE:eval(
		  Name, 
		  erl_eval:add_binding('R', true, erl_eval:new_bindings())),
    ?line true = get(evaluated_ok),
    
    ?line {ok,Fd3} = ?FILE_MODULE:open(Name,write),
    %% garbled
    ?line io:format(Fd3,"puGARBLED-GARBLED\ntrue). ",[]),
    ?line ok = ?FILE_MODULE:close(Fd3),
    ?line {error, {_, _, _} = Msg} = ?FILE_MODULE:eval(Name),
    ?line io:format("Errmsg1: ~p",[Msg]),
    
    ?line {error, enoent} = ?FILE_MODULE:eval(Name ++ ".nonexistent"),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

path_eval(suite) -> [];
path_eval(doc) -> [];
path_eval(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line FileName = atom_to_list(?MODULE)++"_path_eval.fil",
    ?line Name = filename:join(RootDir, FileName),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd1,"put(evaluated_ok,true).\n",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    %% File last in path
    ?line {ok,Dir} =
	?FILE_MODULE:path_eval(
	  [filename:join(RootDir, "dir1"),
	   filename:join(RootDir, ".."),
	   filename:join(RootDir, "dir2"),
	   RootDir],FileName),
    ?line true = get(evaluated_ok),
    ?line true = lists:prefix(RootDir,Dir),
    
    %% While maybe not an error, it may be worth noting that
    %% when the full path to a file is given, it's always found
    %% regardless of the contents of the path
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd2,"put(evaluated_ok,R).\n",[]),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok,_} = ?FILE_MODULE:path_eval(
		      ["nosuch1","nosuch2"],
		      Name,
		      erl_eval:add_binding('R', true, erl_eval:new_bindings())),
    
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

script(suite) -> [script1,path_script].

script1(suite) -> [];
script1(doc) -> "";
script1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_script.fil"),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    ?line io:format(Fd1,"A = 11,\nB = 6,\nA+B. ",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,17} = ?FILE_MODULE:script(Name),
    
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    ?line io:format(Fd2,"A = 11,\nA+B. ",[]),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok,17} = ?FILE_MODULE:script(
		       Name, 
		       erl_eval:add_binding('B', 6, erl_eval:new_bindings())),
    
    ?line {ok,Fd3} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd3,"A = 11,\nB = six,\nA+B. ",[]),
    ?line ok = ?FILE_MODULE:close(Fd3),
    ?line {error, {_, _, _} = Msg} = ?FILE_MODULE:script(Name),
    ?line io:format("Errmsg1: ~p",[Msg]),

    ?line {error, enoent} = ?FILE_MODULE:script(Name ++ ".nonexistent"),
    
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    
path_script(suite) -> [];
path_script(doc) -> [];
path_script(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line FileName = atom_to_list(?MODULE)++"_path_script.fil",
    ?line Name = filename:join(RootDir, FileName),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd1,"A = 11,\nB = 6,\nA+B.\n",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    %% File last in path
    ?line {ok, 17, Dir} =
	?FILE_MODULE:path_script(
	   [filename:join(RootDir, "dir1"),
	    filename:join(RootDir, ".."),
	    filename:join(RootDir, "dir2"),
	    RootDir],FileName),
    ?line true = lists:prefix(RootDir,Dir),
    
    %% While maybe not an error, it may be worth noting that
    %% when the full path to a file is given, it's always found
    %% regardless of the contents of the path
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    ?line io:format(Fd2,"A = 11,\nA+B.",[]),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok, 17, Dir} = 
	?FILE_MODULE:path_script(
	   ["nosuch1","nosuch2"],
	   Name,
	   erl_eval:add_binding('B', 6, erl_eval:new_bindings())),
    
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

    

truncate(suite) -> [];
truncate(doc) -> [];
truncate(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_truncate.fil"),

    %% Create a file with some data.
    ?line MyData = "0123456789abcdefghijklmnopqrstuvxyz",
    ?line ok = ?FILE_MODULE:write_file(Name, MyData),

    %% Truncate the file to 10 characters.
    ?line {ok, Fd} = ?FILE_MODULE:open(Name, read_write),
    ?line {ok, 10} = ?FILE_MODULE:position(Fd, 10),
    ?line ok = ?FILE_MODULE:truncate(Fd),
    ?line ok = ?FILE_MODULE:close(Fd),

    %% Read back the file and check that it has been truncated.
    ?line Expected = list_to_binary("0123456789"),
    ?line {ok, Expected} = ?FILE_MODULE:read_file(Name),

    %% Open the file read only and verify that it is not possible to
    %% truncate it, OTP-1960
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name, read),
    ?line {ok, 5} = ?FILE_MODULE:position(Fd2, 5),
    ?line {error, _} = ?FILE_MODULE:truncate(Fd2),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.


datasync(suite) -> [];
datasync(doc) -> "Tests that ?FILE_MODULE:datasync/1 at least doesn't crash.";
datasync(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Sync = filename:join(PrivDir,
			       atom_to_list(?MODULE)
			       ++"_sync.fil"),

    %% Raw open.
    ?line {ok, Fd} = ?FILE_MODULE:open(Sync, [write, raw]),
    ?line ok = ?FILE_MODULE:datasync(Fd),
    ?line ok = ?FILE_MODULE:close(Fd),

    %% Ordinary open.
    ?line {ok, Fd2} = ?FILE_MODULE:open(Sync, [write]),
    ?line ok = ?FILE_MODULE:datasync(Fd2),
    ?line ok = ?FILE_MODULE:close(Fd2),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.


sync(suite) -> [];
sync(doc) -> "Tests that ?FILE_MODULE:sync/1 at least doesn't crash.";
sync(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Sync = filename:join(PrivDir, 
			       atom_to_list(?MODULE)
			       ++"_sync.fil"),

    %% Raw open.
    ?line {ok, Fd} = ?FILE_MODULE:open(Sync, [write, raw]),
    ?line ok = ?FILE_MODULE:sync(Fd),
    ?line ok = ?FILE_MODULE:close(Fd),

    %% Ordinary open.
    ?line {ok, Fd2} = ?FILE_MODULE:open(Sync, [write]),
    ?line ok = ?FILE_MODULE:sync(Fd2),
    ?line ok = ?FILE_MODULE:close(Fd2),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

advise(suite) -> [];
advise(doc) -> "Tests that ?FILE_MODULE:advise/4 at least doesn't crash.";
advise(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Advise = filename:join(PrivDir,
			       atom_to_list(?MODULE)
			       ++"_advise.fil"),

    Line1 = "Hello\n",
    Line2 = "World!\n",

    ?line {ok, Fd} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = ?FILE_MODULE:advise(Fd, 0, 0, normal),
    ?line ok = io:format(Fd, "~s", [Line1]),
    ?line ok = io:format(Fd, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd),

    ?line {ok, Fd2} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = ?FILE_MODULE:advise(Fd2, 0, 0, random),
    ?line ok = io:format(Fd2, "~s", [Line1]),
    ?line ok = io:format(Fd2, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd2),

    ?line {ok, Fd3} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = ?FILE_MODULE:advise(Fd3, 0, 0, sequential),
    ?line ok = io:format(Fd3, "~s", [Line1]),
    ?line ok = io:format(Fd3, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd3),

    ?line {ok, Fd4} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = ?FILE_MODULE:advise(Fd4, 0, 0, will_need),
    ?line ok = io:format(Fd4, "~s", [Line1]),
    ?line ok = io:format(Fd4, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd4),

    ?line {ok, Fd5} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = ?FILE_MODULE:advise(Fd5, 0, 0, dont_need),
    ?line ok = io:format(Fd5, "~s", [Line1]),
    ?line ok = io:format(Fd5, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd5),

    ?line {ok, Fd6} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = ?FILE_MODULE:advise(Fd6, 0, 0, no_reuse),
    ?line ok = io:format(Fd6, "~s", [Line1]),
    ?line ok = io:format(Fd6, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd6),

    ?line {ok, Fd7} = ?FILE_MODULE:open(Advise, [write]),
    ?line {error, einval} = ?FILE_MODULE:advise(Fd7, 0, 0, bad_advise),
    ?line ok = ?FILE_MODULE:close(Fd7),

    %% test write without advise, then a read after an advise
    ?line {ok, Fd8} = ?FILE_MODULE:open(Advise, [write]),
    ?line ok = io:format(Fd8, "~s", [Line1]),
    ?line ok = io:format(Fd8, "~s", [Line2]),
    ?line ok = ?FILE_MODULE:close(Fd8),
    ?line {ok, Fd9} = ?FILE_MODULE:open(Advise, [read]),
    Offset = 0,
    %% same as a 0 length in some implementations
    Length = length(Line1) + length(Line2),
    ?line ok = ?FILE_MODULE:advise(Fd9, Offset, Length, sequential),
    ?line {ok, Line1} = ?FILE_MODULE:read_line(Fd9),
    ?line {ok, Line2} = ?FILE_MODULE:read_line(Fd9),
    ?line eof = ?FILE_MODULE:read_line(Fd9),
    ?line ok = ?FILE_MODULE:close(Fd9),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete(suite) -> [];
delete(doc) -> [];
delete(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line Name = filename:join(RootDir, 
			       atom_to_list(?MODULE)
			       ++"_delete.fil"),
    ?line {ok, Fd1} = ?FILE_MODULE:open(Name, write),
    ?line io:format(Fd1,"ok.\n",[]),
    ?line ok = ?FILE_MODULE:close(Fd1),
    %% Check that the file is readable
    ?line {ok, Fd2} = ?FILE_MODULE:open(Name, read),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line ok = ?FILE_MODULE:delete(Name),
    %% Check that the file is not readable anymore
    ?line {error, _} = ?FILE_MODULE:open(Name, read),
    %% Try deleting a nonexistent file
    ?line {error, enoent} = ?FILE_MODULE:delete(Name),
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

rename(suite) ->[];
rename(doc) ->[];
rename(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line FileName1 = atom_to_list(?MODULE)++"_rename.fil",
    ?line FileName2 = atom_to_list(?MODULE)++"_rename.ful",
    ?line Name1 = filename:join(RootDir, FileName1),
    ?line Name2 = filename:join(RootDir, FileName2),
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name1,write),
    ?line ok = ?FILE_MODULE:close(Fd1),
    %% Rename, and check that id really changed name
    ?line ok = ?FILE_MODULE:rename(Name1,Name2),
    ?line {error, _} = ?FILE_MODULE:open(Name1,read),
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name2,read),
    ?line ok = ?FILE_MODULE:close(Fd2),
    %% Try renaming something to itself
    ?line ok = ?FILE_MODULE:rename(Name2,Name2),
    %% Try renaming something that doesn't exist
    ?line {error, enoent} = ?FILE_MODULE:rename(Name1,Name2),
    %% Try renaming to something else than a string
    ?line {error, badarg} = ?FILE_MODULE:rename(Name1,{foo,bar}),
    
    %% Move between directories
    ?line DirName1 = filename:join(RootDir,
				   atom_to_list(?MODULE)
				   ++"_rename_dir"),
    ?line DirName2 = filename:join(RootDir,
				   atom_to_list(?MODULE)
				   ++"_second_rename_dir"),
    ?line Name1foo = filename:join(DirName1, "foo.fil"),
    ?line Name2foo = filename:join(DirName2, "foo.fil"),
    ?line Name2bar = filename:join(DirName2, "bar.dir"),
    ?line ok = ?FILE_MODULE:make_dir(DirName1),
    %% The name has to include the full file name, path in not enough
    ?line expect({error, eisdir}, {error, eexist}, 
		 ?FILE_MODULE:rename(Name2,DirName1)),
    ?line ok = ?FILE_MODULE:rename(Name2, Name1foo),
    %% Now rename the directory
    ?line ok = ?FILE_MODULE:rename(DirName1,DirName2),
    %% And check that the file is there now
    ?line {ok,Fd3} = ?FILE_MODULE:open(Name2foo, read),
    ?line ok = ?FILE_MODULE:close(Fd3),
    %% Try some dirty things now: move the directory into itself
    ?line {error, Msg1} = ?FILE_MODULE:rename(DirName2, Name2bar),
    ?line io:format("Errmsg1: ~p",[Msg1]),
    %% move dir into a file in itself
    ?line {error, Msg2} = ?FILE_MODULE:rename(DirName2, Name2foo),
    ?line io:format("Errmsg2: ~p",[Msg2]),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

names(suite) -> [];
names(doc) -> [];
names(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line RootDir = ?config(priv_dir,Config),
    ?line FileName = "foo1.fil",
    ?line Name1 = filename:join(RootDir, FileName),
    ?line Name2 = [RootDir,"/","foo1",".","fil"],
    ?line Name3 = [RootDir,"/",foo,$1,[[[],[],'.']],"f",il],
    ?line Name4 = list_to_atom(Name1),
    ?line {ok,Fd0} = ?FILE_MODULE:open(Name1,write),
    ?line ok = ?FILE_MODULE:close(Fd0),

    %% Try some file names
    ?line {ok,Fd1} = ?FILE_MODULE:open(Name1,read),
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line {ok,Fd2f} = ?FILE_MODULE:open(lists:flatten(Name2),read),
    ?line ok = ?FILE_MODULE:close(Fd2f),
    ?line {ok,Fd2} = ?FILE_MODULE:open(Name2,read),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok,Fd3} = ?FILE_MODULE:open(Name3,read),
    ?line ok = ?FILE_MODULE:close(Fd3),
    ?line {ok,Fd4} = ?FILE_MODULE:open(Name4,read),
    ?line ok = ?FILE_MODULE:close(Fd4),

    %% Try some path names
    ?line Path1 = RootDir,
    ?line Path2 = [RootDir],
    ?line Path3 = ['',[],[RootDir,[[]]]],
    ?line Path4 = list_to_atom(Path1),
    ?line {ok,Fd11,_} = ?FILE_MODULE:path_open([Path1],FileName,read),
    ?line ok = ?FILE_MODULE:close(Fd11),
    ?line {ok,Fd12,_} = ?FILE_MODULE:path_open([Path2],FileName,read),
    ?line ok = ?FILE_MODULE:close(Fd12),
    ?line {ok,Fd13,_} = ?FILE_MODULE:path_open([Path3],FileName,read),
    ?line ok = ?FILE_MODULE:close(Fd13),
    ?line {ok,Fd14,_} = ?FILE_MODULE:path_open([Path4],FileName,read),
    ?line ok = ?FILE_MODULE:close(Fd14),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

errors(suite) -> [e_delete, e_rename, e_make_dir, e_del_dir].

e_delete(suite) -> [];
e_delete(doc) -> [];
e_delete(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_e_delete"),
    ?line ok = ?FILE_MODULE:make_dir(Base),

    %% Delete a non-existing file.
    ?line {error, enoent} = 
	?FILE_MODULE:delete(filename:join(Base, "non_existing")),

    %% Delete a directory.
    ?line {error, eperm} = ?FILE_MODULE:delete(Base),

    %% Use a path-name with a non-directory component.
    ?line Afile = filename:join(Base, "a_file"),
    ?line ok = ?FILE_MODULE:write_file(Afile, "hello\n"),
    ?line {error, E} = 
	expect({error, enotdir}, {error, enoent}, 
	       ?FILE_MODULE:delete(filename:join(Afile, "another_file"))),
    ?line io:format("Result: ~p~n", [E]),

    %% No permission.
    ?line case os:type() of
	      {unix, _} ->
		  ?line ?FILE_MODULE:write_file_info(
			   Base, #file_info {mode=0}),
		  ?line {error, eacces} = ?FILE_MODULE:delete(Afile),
		  ?line ?FILE_MODULE:write_file_info(
			   Base, #file_info {mode=8#600});
	      {win32, _} ->
		  %% Remove a character device.
		  ?line {error, eacces} = ?FILE_MODULE:delete("nul");
	      vxworks ->
		  ok
	  end,

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%% FreeBSD gives EEXIST when renaming a file to an empty dir, although the
%%% manual page can be interpreted as saying that EISDIR should be given.
%%% (What about FreeBSD? We store our nightly build results on a FreeBSD
%%% file system, that's what.)

e_rename(suite) -> [];
e_rename(doc) -> [];
e_rename(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {comment, "Windriver: dosFs must be fixed first!"};
	_ ->
	    ?line Dog = test_server:timetrap(test_server:seconds(10)),
	    ?line RootDir = ?config(priv_dir, Config),
	    ?line Base = filename:join(RootDir,
				       atom_to_list(?MODULE)++"_e_rename"),
	    ?line ok = ?FILE_MODULE:make_dir(Base),
	
	    %% Create an empty directory.
	    ?line EmptyDir = filename:join(Base, "empty_dir"),
	    ?line ok = ?FILE_MODULE:make_dir(EmptyDir),

	    %% Create a non-empty directory.
	    ?line NonEmptyDir = filename:join(Base, "non_empty_dir"),
	    ?line ok = ?FILE_MODULE:make_dir(NonEmptyDir),
	    ?line ok = ?FILE_MODULE:write_file(
			  filename:join(NonEmptyDir, "a_file"),
			  "hello\n"),

	    %% Create another non-empty directory.
	    ?line ADirectory = filename:join(Base, "a_directory"),
	    ?line ok = ?FILE_MODULE:make_dir(ADirectory),
	    ?line ok = ?FILE_MODULE:write_file(
			  filename:join(ADirectory, "a_file"),
			  "howdy\n\n"),

	    %% Create a data file.
	    ?line File = filename:join(Base, "just_a_file"),
	    ?line ok = ?FILE_MODULE:write_file(File, "anything goes\n\n"),
	
	    %% Move an existing directory to a non-empty directory.
	    ?line {error, eexist} = 
		?FILE_MODULE:rename(ADirectory, NonEmptyDir),

	    %% Move a root directory.
	    ?line {error, einval} = ?FILE_MODULE:rename("/", "arne"),

	    %% Move Base into Base/new_name.
	    ?line {error, einval} = 
		?FILE_MODULE:rename(Base, filename:join(Base, "new_name")),

	    %% Overwrite a directory with a file.
	    ?line expect({error, eexist}, %FreeBSD (?)
			 {error, eisdir},
			 ?FILE_MODULE:rename(File, EmptyDir)),
	    ?line expect({error, eexist}, %FreeBSD (?)
			 {error, eisdir},
			 ?FILE_MODULE:rename(File, NonEmptyDir)),

	    %% Move a non-existing file.
	    ?line NonExistingFile = 
		filename:join(Base, "non_existing_file"),
	    ?line {error, enoent} = 
		?FILE_MODULE:rename(NonExistingFile, NonEmptyDir),

	    %% Overwrite a file with a directory.
	    ?line expect({error, eexist}, %FreeBSD (?)
			 {error, enotdir},
			 ?FILE_MODULE:rename(ADirectory, File)),
	    
	    %% Move a file to another filesystem.
	    %% XXX - This test case is bogus. We cannot be guaranteed that
	    %%       the source and destination are on 
	    %%       different filesystems.
	    %%
	    %% XXX - Gross hack!
	    ?line Comment = 
		case os:type() of
		    {unix, _} ->
			OtherFs = "/tmp",
			?line NameOnOtherFs =
			    filename:join(OtherFs, filename:basename(File)),
			?line {ok, Com} = 
			    case ?FILE_MODULE:rename(File, NameOnOtherFs) of
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
			Com;
		    {win32, _} ->
			%% At least Windows NT can 
			%% successfully move a file to
			%% another drive.
			ok
		end,
	    ?line [] = flush(),
	    ?line test_server:timetrap_cancel(Dog),
	    Comment
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e_make_dir(suite) -> [];
e_make_dir(doc) -> [];
e_make_dir(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_e_make_dir"),
    ?line ok = ?FILE_MODULE:make_dir(Base),

    %% A component of the path does not exist.
    ?line {error, enoent} = 
	?FILE_MODULE:make_dir(filename:join([Base, "a", "b"])),

    %% Use a path-name with a non-directory component.
    ?line Afile = filename:join(Base, "a_directory"),
    ?line ok = ?FILE_MODULE:write_file(Afile, "hello\n"),
    ?line case ?FILE_MODULE:make_dir(
		  filename:join(Afile, "another_directory")) of
	      {error, enotdir} -> io:format("Result: enotdir");
	      {error, enoent} -> io:format("Result: enoent")
	  end,

    %% No permission (on Unix only).
    case os:type() of
	{unix, _} ->
	    ?line ?FILE_MODULE:write_file_info(Base, #file_info {mode=0}),
	    ?line {error, eacces} = 
		?FILE_MODULE:make_dir(filename:join(Base, "xxxx")),
	    ?line ?FILE_MODULE:write_file_info(
		     Base, #file_info {mode=8#600});
	{win32, _} ->
	    ok;
	vxworks ->
	    ok
    end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e_del_dir(suite) -> [];
e_del_dir(doc) -> [];
e_del_dir(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = test_server:temp_name(filename:join(RootDir, "e_del_dir")),
    ?line io:format("Base: ~p", [Base]),
    ?line ok = ?FILE_MODULE:make_dir(Base),

    %% Delete a non-existent directory.
    ?line {error, enoent} = 
	?FILE_MODULE:del_dir(filename:join(Base, "non_existing")),

    %% Use a path-name with a non-directory component.
    ?line Afile = filename:join(Base, "a_directory"),
    ?line ok = ?FILE_MODULE:write_file(Afile, "hello\n"),
    ?line {error, E1} = 
	expect({error, enotdir}, {error, enoent},
	       ?FILE_MODULE:del_dir(
		 filename:join(Afile, "another_directory"))),
    ?line io:format("Result: ~p", [E1]),

    %% Delete a non-empty directory.
    ?line {error, E2} = 
	expect({error, enotempty}, {error, eexist}, {error, eacces},
	       ?FILE_MODULE:del_dir(Base)),
    ?line io:format("Result: ~p", [E2]),

    %% Remove the current directory.
    ?line {error, E3} = 
	expect({error, einval}, 
	       {error, eperm}, % Linux and DUX
	       {error, eacces},
	       {error, ebusy},
	       ?FILE_MODULE:del_dir(".")),
    ?line io:format("Result: ~p", [E3]),

    %% No permission.
    case os:type() of
	{unix, _} ->
	    ?line ADirectory = filename:join(Base, "no_perm"),
	    ?line ok = ?FILE_MODULE:make_dir(ADirectory),
	    ?line ?FILE_MODULE:write_file_info(
		     Base, #file_info {mode=0}),
	    ?line {error, eacces} = ?FILE_MODULE:del_dir(ADirectory),
	    ?line ?FILE_MODULE:write_file_info(
		     Base, #file_info {mode=8#600});
	{win32, _} ->
	    ok;
	vxworks ->
	    ok
    end,
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compression(suite) ->
    [read_compressed_cooked, read_compressed_cooked_binary,
     read_cooked_tar_problem,
     read_not_really_compressed,
     write_compressed, compress_errors,
     catenated_gzips].

%% Trying reading and positioning from a compressed file.

read_compressed_cooked(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line Real = filename:join(Data, "realmen.html.gz"),
    ?line {ok, Fd} = ?FILE_MODULE:open(Real, [read,compressed]),
    ?line try_read_file_list(Fd).

read_compressed_cooked_binary(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line Real = filename:join(Data, "realmen.html.gz"),
    ?line {ok, Fd} = ?FILE_MODULE:open(Real, [read,compressed,binary]),
    ?line try_read_file_binary(Fd).

%% Trying reading and positioning from an uncompressed file,
%% but with the compressed flag given.

read_not_really_compressed(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line Priv = ?config(priv_dir, Config),

    %% The file realmen.html might have got CRs added (by WinZip).
    %% Remove them, or the file positions will not be correct.

    ?line Real = filename:join(Data, "realmen.html"),
    ?line RealPriv = filename:join(Priv, 
				   atom_to_list(?MODULE)++"_realmen.html"),
    ?line {ok, RealDataBin} = ?FILE_MODULE:read_file(Real),
    ?line RealData = remove_crs(binary_to_list(RealDataBin), []),
    ?line ok = ?FILE_MODULE:write_file(RealPriv, RealData),
    ?line {ok, Fd} = ?FILE_MODULE:open(RealPriv, [read, compressed]),
    ?line try_read_file_list(Fd).

remove_crs([$\r|Rest], Result) ->
    remove_crs(Rest, Result);
remove_crs([C|Rest], Result) ->
    remove_crs(Rest, [C|Result]);
remove_crs([], Result) ->
    lists:reverse(Result).

try_read_file_list(Fd) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    %% Seek to the current position (nothing should happen).

    ?line {ok, 0} = ?FILE_MODULE:position(Fd, 0),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd, {cur, 0}),

    %% Read a few lines from a compressed file.

    ?line ShouldBe = "<TITLE>Real Programmers Don't Use PASCAL</TITLE>\n",
    ?line ShouldBe = io:get_line(Fd, ''),

    %% Now seek forward.

    ?line {ok, 381} = ?FILE_MODULE:position(Fd, 381),
    ?line Back = "Back in the good old days -- the \"Golden Era\" " ++
	"of computers, it was\n",
    ?line Back = io:get_line(Fd, ''),

    %% Try to search forward relative to the current position.

    ?line {ok, CurPos} = ?FILE_MODULE:position(Fd, {cur, 0}),
    ?line RealPos = 4273,
    ?line {ok, RealPos} = ?FILE_MODULE:position(Fd, {cur, RealPos-CurPos}),
    ?line RealProg = "<LI> Real Programmers aren't afraid to use GOTOs.\n",
    ?line RealProg = io:get_line(Fd, ''),

    %% Seek backward.

    ?line AfterTitle = length("<TITLE>"),
    ?line {ok, AfterTitle} = ?FILE_MODULE:position(Fd, AfterTitle),
    ?line Title = "Real Programmers Don't Use PASCAL</TITLE>\n",
    ?line Title = io:get_line(Fd, ''),

    %% Done.

    ?line ?FILE_MODULE:close(Fd),
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

try_read_file_binary(Fd) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    %% Seek to the current position (nothing should happen).

    ?line {ok, 0} = ?FILE_MODULE:position(Fd, 0),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd, {cur, 0}),

    %% Read a few lines from a compressed file.

    ?line ShouldBe = <<"<TITLE>Real Programmers Don't Use PASCAL</TITLE>\n">>,
    ?line ShouldBe = io:get_line(Fd, ''),

    %% Now seek forward.

    ?line {ok, 381} = ?FILE_MODULE:position(Fd, 381),
    ?line Back = <<"Back in the good old days -- the \"Golden Era\" "
		  "of computers, it was\n">>,
    ?line Back = io:get_line(Fd, ''),

    %% Try to search forward relative to the current position.

    ?line {ok, CurPos} = ?FILE_MODULE:position(Fd, {cur, 0}),
    ?line RealPos = 4273,
    ?line {ok, RealPos} = ?FILE_MODULE:position(Fd, {cur, RealPos-CurPos}),
    ?line RealProg = <<"<LI> Real Programmers aren't afraid to use GOTOs.\n">>,
    ?line RealProg = io:get_line(Fd, ''),

    %% Seek backward.

    ?line AfterTitle = length("<TITLE>"),
    ?line {ok, AfterTitle} = ?FILE_MODULE:position(Fd, AfterTitle),
    ?line Title = <<"Real Programmers Don't Use PASCAL</TITLE>\n">>,
    ?line Title = io:get_line(Fd, ''),

    %% Done.

    ?line ?FILE_MODULE:close(Fd),
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

read_cooked_tar_problem(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    ?line Data = ?config(data_dir, Config),
    ?line ProblemFile = filename:join(Data, "cooked_tar_problem.tar.gz"),
    ?line {ok,Fd} = ?FILE_MODULE:open(ProblemFile, [read,compressed,binary]),

    ?line {ok,34304} = file:position(Fd, 34304),
    ?line {ok,Bin} = file:read(Fd, 512),
    ?line 512 = byte_size(Bin),
    
    ?line {ok,34304+512+1024} = file:position(Fd, {cur,1024}),
    
    ?line ok = file:close(Fd),

    ?line test_server:timetrap_cancel(Dog),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_compressed(suite) -> [];
write_compressed(doc) -> [];
write_compressed(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Priv = ?config(priv_dir, Config),
    ?line MyFile = filename:join(Priv, 
				 atom_to_list(?MODULE)++"_test.gz"),

    %% Write a file.

    ?line {ok, Fd} = ?FILE_MODULE:open(MyFile, [write, compressed]),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd, 0),
    ?line Prefix = "hello\n",
    ?line End = "end\n",
    ?line ok = io:put_chars(Fd, Prefix),
    ?line {ok, 143} = ?FILE_MODULE:position(Fd, 143),
    ?line ok = io:put_chars(Fd, End),
    ?line ok = ?FILE_MODULE:close(Fd),

    %% Read the file and verify the contents.

    ?line {ok, Fd1} = ?FILE_MODULE:open(MyFile, [read, compressed]),
    ?line Prefix = io:get_line(Fd1, ''),
    ?line Second = lists:duplicate(143-length(Prefix), 0) ++ End,
    ?line Second = io:get_line(Fd1, ''),
    ?line ok = ?FILE_MODULE:close(Fd1),

    %% Verify succesful compression by uncompressing the file
    %% using zlib:gunzip/1.

    ?line {ok,Contents} = file:read_file(MyFile),
    ?line <<"hello\n",0:137/unit:8,"end\n">> = zlib:gunzip(Contents),

    %% Ensure that the file is compressed.

    TotalSize = 143 + length(End),
    case ?FILE_MODULE:read_file_info(MyFile) of
	{ok, #file_info{size=Size}} when Size < TotalSize ->
	    ok;
	{ok, #file_info{size=Size}} when Size == TotalSize ->
	    test_server:fail(file_not_compressed)
    end,

    %% Write again to ensure that the file is truncated.

    ?line {ok, Fd2} = ?FILE_MODULE:open(MyFile, [write, compressed]),
    ?line NewString = "aaaaaaaaaaa",
    ?line ok = io:put_chars(Fd2, NewString),
    ?line ok = ?FILE_MODULE:close(Fd2),
    ?line {ok, Fd3} = ?FILE_MODULE:open(MyFile, [read, compressed]),
    ?line {ok, NewString} = ?FILE_MODULE:read(Fd3, 1024),
    ?line ok = ?FILE_MODULE:close(Fd3),

    %% Done.

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

catenated_gzips(Config) when is_list(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line MyFile = filename:join(Priv, ?MODULE_STRING++"_test.gz"),

    First = "Hello, all good men going to search parties. ",
    Second = "Now I really need your help.",
    All = iolist_to_binary([First|Second]),
    ?line Cat = [zlib:gzip(First),zlib:gzip(Second)],
    
    ?line ok = file:write_file(MyFile, Cat),

    ?line {ok,Fd} = file:open(MyFile, [read,compressed,binary]),
    ?line {ok,All} = file:read(Fd, 100000),
    ?line ok = file:close(Fd),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compress_errors(suite) -> [];
compress_errors(doc) -> [];
compress_errors(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line DataDir = 
	filename:dirname(
	  filename:join(?config(data_dir, Config), "x")),
    ?line DataDirSlash = DataDir++"/",
    ?line {error, enoent} = ?FILE_MODULE:open("non_existing__",
					      [compressed, read]),
    ?line {error, einval} = ?FILE_MODULE:open("non_existing__",
					      [compressed, read, write]),
    ?line {error, einval} = ?FILE_MODULE:open("non_existing__",
					      [compressed, read, append]),
    ?line {error, einval} = ?FILE_MODULE:open("non_existing__",
					      [compressed, write, append]),
    ?line {error, E1} = ?FILE_MODULE:open(DataDir, [compressed, read]),
    ?line {error, E2} = ?FILE_MODULE:open(DataDirSlash, [compressed, read]),
    ?line {error, E3} = ?FILE_MODULE:open(DataDir, [compressed, write]),
    ?line {error, E4} = ?FILE_MODULE:open(DataDirSlash, [compressed, write]),
    ?line {eisdir,eisdir,eisdir,eisdir} = {E1,E2,E3,E4},

    %% Read a corrupted .gz file.

    ?line Corrupted = filename:join(DataDir, "corrupted.gz"),
    ?line {ok, Fd} = ?FILE_MODULE:open(Corrupted, [read, compressed]),
    ?line {error, eio} = ?FILE_MODULE:read(Fd, 100),
    ?line ?FILE_MODULE:close(Fd),

    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

altname(doc) ->
    "Test the file:altname/1 function";
altname(suite) ->
    [];
altname(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 "long alternative path name with spaces"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    ?line Name = filename:join(NewDir, "a_file_with_long_name"),
    ?line ShortName = filename:join(NewDir, "short"),
    ?line NonexName = filename:join(NewDir, "nonexistent"),
    ?line ok = ?FILE_MODULE:write_file(Name, "some contents\n"),
    ?line ok = ?FILE_MODULE:write_file(ShortName, "some contents\n"),
    ?line Result = 
	case ?FILE_MODULE:altname(NewDir) of
	    {error, enotsup} ->
		{skipped, "Altname not supported on this platform"};
	    {ok, "LONGAL~1"} -> 
		?line {ok, "A_FILE~1"} = ?FILE_MODULE:altname(Name),
		?line {ok, "C:/"} = ?FILE_MODULE:altname("C:/"),
		?line {ok, "C:\\"} = ?FILE_MODULE:altname("C:\\"),
		?line {error,enoent} = ?FILE_MODULE:altname(NonexName),
		?line {ok, "short"} = ?FILE_MODULE:altname(ShortName),
		ok
	end,
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    Result.

links(doc) -> "Test the link functions.";
links(suite) -> [make_link, read_link_info_for_non_link, symlinks].

make_link(doc) -> "Test creating a hard link.";
make_link(suite) -> [];
make_link(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_make_link"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    
    ?line Name = filename:join(NewDir, "a_file"),
    ?line ok = ?FILE_MODULE:write_file(Name, "some contents\n"),
    
    ?line Alias = filename:join(NewDir, "an_alias"),
    ?line Result = 
	case ?FILE_MODULE:make_link(Name, Alias) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    ok ->
		%% Note: We take the opportunity to test 
		%% ?FILE_MODULE:read_link_info/1,
		%% which should in behave exactly as 
		%% ?FILE_MODULE:read_file_info/1
		%% since they are not used on symbolic links.
		
		?line {ok, Info} = ?FILE_MODULE:read_link_info(Name),
		?line {ok, Info} = ?FILE_MODULE:read_link_info(Alias),
		?line #file_info{links = 2, type = regular} = Info,
		?line {error, eexist} = 
		    ?FILE_MODULE:make_link(Name, Alias),
		ok
	end,
    
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    Result.

read_link_info_for_non_link(doc) ->
    "Test that reading link info for an ordinary file or directory works "
	"(on all platforms).";
read_link_info_for_non_link(suite) -> [];
read_link_info_for_non_link(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    ?line {ok, #file_info{type=directory}} = 
	?FILE_MODULE:read_link_info("."),
		  
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

symlinks(doc) -> "Test operations on symbolic links (for Unix).";
symlinks(suite) -> [];
symlinks(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line NewDir = filename:join(RootDir, 
				 atom_to_list(?MODULE)
				 ++"_symlinks"),
    ?line ok = ?FILE_MODULE:make_dir(NewDir),
    
    ?line Name = filename:join(NewDir, "a_plain_file"),
    ?line ok = ?FILE_MODULE:write_file(Name, "some stupid content\n"),
    
    ?line Alias = filename:join(NewDir, "a_symlink_alias"),
    ?line Result = 
	case ?FILE_MODULE:make_symlink(Name, Alias) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    ok ->
		?line {ok, Info1} = ?FILE_MODULE:read_file_info(Name),
		?line {ok, Info1} = ?FILE_MODULE:read_file_info(Alias),
		?line {ok, Info1} = ?FILE_MODULE:read_link_info(Name),
		?line #file_info{links = 1, type = regular} = Info1,
		
		?line {ok, Info2} = ?FILE_MODULE:read_link_info(Alias),
		?line #file_info{links=1, type=symlink} = Info2,
		?line {ok, Name} = ?FILE_MODULE:read_link(Alias),
		ok
	  end,
    
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy(doc) -> [];
copy(suite) -> [];
copy(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    %% Create a text file.
    ?line Name1 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_1.txt"),
    ?line Line = "The quick brown fox jumps over a lazy dog. 0123456789\n",
    ?line Len = length(Line),
    ?line {ok, Handle1} = ?FILE_MODULE:open(Name1, [write]),
    ?line {_, Size1} = 
	iterate({0, 0},
		done,
		fun({_, S}) when S >= 128*1024 ->
			done;
		   ({N, S}) ->
			H = integer_to_list(N),
			ok = ?FILE_MODULE:write(Handle1, [H, " ", Line]),
			{N + 1, S + length(H) + 1 + Len}
		end),
    ?line ?FILE_MODULE:close(Handle1),
    %% Make a copy
    ?line Name2 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_2.txt"),
    ?line {ok, Size1} = ?FILE_MODULE:copy(Name1, Name2),
    %% Concatenate 1
    ?line Name3 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_3.txt"),
    ?line {ok, Handle3} = ?FILE_MODULE:open(Name3, [raw, write, binary]),
    ?line {ok, Size1} = ?FILE_MODULE:copy(Name1, Handle3),
    ?line {ok, Handle2} = ?FILE_MODULE:open(Name2, [read, binary]),
    ?line {ok, Size1} = ?FILE_MODULE:copy(Handle2, Handle3),
    ?line ok = ?FILE_MODULE:close(Handle3),
    ?line ok = ?FILE_MODULE:close(Handle2),
    %% Concatenate 2
    ?line Name4 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_4.txt"),
    ?line {ok, Handle4} = ?FILE_MODULE:open(Name4, [write, binary]),
    ?line {ok, Size1} = ?FILE_MODULE:copy(Name1, Handle4),
    ?line {ok, Handle5} = ?FILE_MODULE:open(Name2, [raw, read, binary]),
    ?line {ok, Size1} = ?FILE_MODULE:copy(Handle5, Handle4),
    ?line ok = ?FILE_MODULE:close(Handle5),
    ?line ok = ?FILE_MODULE:close(Handle4),
    %% %% Just for test of the test
    %% ?line {ok, Handle2q} = ?FILE_MODULE:open(Name2, [write, append]),
    %% ?line ok = ?FILE_MODULE:write(Handle2q, "q"),
    %% ?line ok = ?FILE_MODULE:close(Handle2q),
    %% Compare the files
    ?line {ok, Handle1a} = ?FILE_MODULE:open(Name1, [raw, read]),
    ?line {ok, Handle2a} = ?FILE_MODULE:open(Name2, [raw, read]),
    ?line true = stream_cmp(fd_stream_factory([Handle1a]), 
			    fd_stream_factory([Handle2a])),
    ?line {ok, 0} = ?FILE_MODULE:position(Handle1a, 0),
    ?line {ok, 0} = ?FILE_MODULE:position(Handle2a, 0),
    ?line {ok, Handle3a} = ?FILE_MODULE:open(Name3, [raw, read]),
    ?line true = stream_cmp(fd_stream_factory([Handle1a, Handle2a]), 
			    fd_stream_factory([Handle2a])),
    ?line ok = ?FILE_MODULE:close(Handle1a),
    ?line ok = ?FILE_MODULE:close(Handle2a),
    ?line ok = ?FILE_MODULE:close(Handle3a),
    ?line [] = flush(),
    ?line test_server:timetrap_cancel(Dog),
    ok.



fd_stream_factory([]) ->
    [];
fd_stream_factory([Fd | T] = L) ->
    fun() ->
	    case ?FILE_MODULE:read(Fd, 8192) of
		{ok, Data} when is_binary(Data) ->
		    binary_to_list(Data) ++ fd_stream_factory(L);
		{ok, Data} when is_list(Data) ->
		    Data ++ fd_stream_factory(L);
		eof ->
		    fd_stream_factory(T);
		{error, _} = Error ->
		    Error
	    end
    end.

		    

stream_cmp(F1, F2) when is_function(F1), is_function(F2) ->
    stream_cmp(F1(), F2());
stream_cmp(F, X) when is_function(F) ->
    stream_cmp(F(), X);
stream_cmp(X, F) when is_function(F) ->
    stream_cmp(X, F());
stream_cmp({error, _} = Error, _) ->
    Error;
stream_cmp(_, {error, _} = Error) ->
    Error;
stream_cmp([], []) ->
    true;
stream_cmp([], [_|_]) ->
    false;
stream_cmp([_|_], []) ->
    false;
stream_cmp([H | T1], [H | T2]) ->
    stream_cmp(T1, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test the get_cwd(), open(), and copy() file server calls.
new_slave(_RootDir, Cwd) ->
    ?line L = "qwertyuiopasdfghjklzxcvbnm",
    ?line N = length(L),
    ?line {ok, Cwd}         = ?FILE_MODULE:get_cwd(),
    ?line {error, enotsup}  = ?FILE_MODULE:get_cwd("C:"), % Unix only testcase
    ?line {ok, FD1}         = ?FILE_MODULE:open("file1.txt", write),
    ?line ok                = ?FILE_MODULE:close(FD1),
    ?line {ok, FD2}         = ?FILE_MODULE:open("file1.txt", 
						[write, append, 
						 binary, compressed,
						 delayed_write,
						 {delayed_write, 0, 0},
						 read_ahead,
						 {read_ahead, 0}]),
    ?line ok                = ?FILE_MODULE:write(FD2, L),
    ?line ok                = ?FILE_MODULE:close(FD2),
    ?line {ok, N2}          = ?FILE_MODULE:copy("file1.txt", "file2.txt"),
    ?line io:format("Size ~p, compressed ~p.~n", [N, N2]),
    ?line {ok, FD3}         = ?FILE_MODULE:open("file2.txt", 
						[binary, compressed]),
    %% The file_io_server will translate the binary into a list
    ?line {ok, L}           = ?FILE_MODULE:read(FD3, N+1),
    ?line ok                = ?FILE_MODULE:close(FD3),
    %%
    ?line ok                = ?FILE_MODULE:delete("file1.txt"),
    ?line ok                = ?FILE_MODULE:delete("file2.txt"),
    ?line []                = flush(),
    ok.


%% Test the get_cwd() and open() file server calls.
old_slave(_RootDir, Cwd) ->
    ?line L = "qwertyuiopasdfghjklzxcvbnm",
    ?line N = length(L),
    ?line {ok, Cwd}         = ?FILE_MODULE:get_cwd(),
    ?line {error, enotsup}  = ?FILE_MODULE:get_cwd("C:"), % Unix only testcase
    ?line {ok, FD1}         = ?FILE_MODULE:open("file1.txt", write),
    ?line ok                = ?FILE_MODULE:close(FD1),
    ?line {ok, FD2}         = ?FILE_MODULE:open("file1.txt", 
						[write, binary, compressed]),
    ?line ok                = ?FILE_MODULE:write(FD2, L),
    ?line ok                = ?FILE_MODULE:close(FD2),
    ?line {ok, FD3}         = ?FILE_MODULE:open("file1.txt", [write, append]),
    ?line ok                = ?FILE_MODULE:close(FD3),
    ?line {ok, FD4}         = ?FILE_MODULE:open("file1.txt", 
						[binary, compressed]),
    %% The file_io_server will translate the binary into a list
    ?line {ok, L}           = ?FILE_MODULE:read(FD4, N+1),
    ?line ok                = ?FILE_MODULE:close(FD4),
    %%
    ?line ok                = ?FILE_MODULE:delete("file1.txt"),
    ?line []                = flush(),
    ok.

run_test(Test, Args) ->
    ?line case (catch apply(?MODULE, Test, Args)) of
	      {'EXIT', _} = Exit ->
		  {done, Exit, get(test_server_loc)};
	      Result ->
		  {done, Result}
	  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delayed_write(suite) ->
    [];
delayed_write(doc) ->
    ["Tests the file open option {delayed_write, Size, Delay}"];

delayed_write(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(20)),
    %%
    ?line RootDir = ?config(priv_dir, Config),
    ?line File = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_delayed_write.txt"),
    ?line Data1 = "asdfghjkl",
    ?line Data2 = "qwertyuio",
    ?line Data3 = "zxcvbnm,.",
    ?line Size = length(Data1),
    ?line Size = length(Data2),
    ?line Size = length(Data3),
    ?line Data1Data1 = Data1++Data1,
    ?line Data1Data1Data1 = Data1Data1++Data1,
    ?line Data1Data1Data1Data1 = Data1Data1++Data1Data1,
    %%
    %% Test caching and normal close of non-raw file
    ?line {ok, Fd1} = 
	?FILE_MODULE:open(File, [write, {delayed_write, Size+1, 2000}]),
    ?line ok = ?FILE_MODULE:write(Fd1, Data1),
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Fd2} = ?FILE_MODULE:open(File, [read]),
    ?line case os:type() of
	      vxworks -> 
		  io:format("Line ~p skipped on vxworks", [?LINE]);
	      _ ->
		  ?line eof = ?FILE_MODULE:read(Fd2, 1)
	  end,
    ?line ok = ?FILE_MODULE:write(Fd1, Data1), % Data flush on size
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Data1Data1} = ?FILE_MODULE:pread(Fd2, bof, 2*Size+1),
    ?line ok = ?FILE_MODULE:write(Fd1, Data1),
    ?line ?t:sleep(3000), % Wait until data flush on timeout
    ?line {ok, Data1Data1Data1} = ?FILE_MODULE:pread(Fd2, bof, 3*Size+1),
    ?line ok = ?FILE_MODULE:write(Fd1, Data1),
    ?line ok = ?FILE_MODULE:close(Fd1), % Data flush on close
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Data1Data1Data1Data1} = ?FILE_MODULE:pread(Fd2, bof, 4*Size+1),
    ?line ok = ?FILE_MODULE:close(Fd2),
    %%
    %% Test implicit close through exit by file owning process, 
    %% raw file, default parameters.
    ?line Parent = self(),
    ?line Fun = 
	fun () ->
		Child = self(),
		Test = 
		    fun () ->
			    ?line {ok, Fd} = 
				?FILE_MODULE:open(File, 
						  [raw, write, 
						   delayed_write]),
			    ?line ok = ?FILE_MODULE:write(Fd, Data1),
			    ?line Parent ! {Child, wrote},
			    ?line receive 
				      {Parent, continue, Reason} -> 
					  {ok, Reason}
				  end
		    end,
		case (catch Test()) of
		    {ok, Reason} ->
			exit(Reason);
		    Unknown ->
			exit({Unknown, get(test_server_loc)})
		end
	end,
    ?line Child1 = spawn(Fun),
    ?line Mref1 = erlang:monitor(process, Child1),
    ?line receive 
	      {Child1, wrote} -> 
		  ok;
	      {'DOWN', Mref1, _, _, _} = Down1a ->
		  ?t:fail(Down1a)
	  end,
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Fd3} = ?FILE_MODULE:open(File, [read]),
    ?line case os:type() of
	      vxworks -> 
		  io:format("Line ~p skipped on vxworks", [?LINE]);
	      _ ->
		  ?line eof = ?FILE_MODULE:read(Fd3, 1)
	  end,
    ?line Child1 ! {Parent, continue, normal},
    ?line receive 
	      {'DOWN', Mref1, process, Child1, normal} -> 
		  ok;
	      {'DOWN', Mref1, _, _, _} = Down1b ->
		  ?t:fail(Down1b)
	  end,
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Data1} = ?FILE_MODULE:pread(Fd3, bof, Size+1),
    ?line ok = ?FILE_MODULE:close(Fd3),
    %%
    %% The same again, but this time with reason 'kill'.
    ?line Child2 = spawn(Fun),
    ?line Mref2 = erlang:monitor(process, Child2),
    ?line receive 
	      {Child2, wrote} -> 
		  ok;
	      {'DOWN', Mref2, _, _, _} = Down2a ->
		  ?t:fail(Down2a)
	  end,
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Fd4} = ?FILE_MODULE:open(File, [read]),
    ?line case os:type() of
	      vxworks -> 
		  io:format("Line ~p skipped on vxworks", [?LINE]);
	      _ ->
		  ?line eof = ?FILE_MODULE:read(Fd4, 1)
	  end,
    ?line Child2 ! {Parent, continue, kill},
    ?line receive 
	      {'DOWN', Mref2, process, Child2, kill} -> 
		  ok;
	      {'DOWN', Mref2, _, _, _} = Down2b ->
		  ?t:fail(Down2b)
	  end,
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line eof = ?FILE_MODULE:pread(Fd4, bof, 1),
    ?line ok = ?FILE_MODULE:close(Fd4),
    %%
    %% Test if file position works with delayed_write
    ?line {ok, Fd5} = ?FILE_MODULE:open(File, [raw, read, write, 
					       delayed_write]),
    ?line ok = ?FILE_MODULE:truncate(Fd5),
    ?line ok = ?FILE_MODULE:write(Fd5, [Data1|Data2]),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    ?line ok = ?FILE_MODULE:write(Fd5, [Data3]),
    ?line {ok, Data2} = ?FILE_MODULE:read(Fd5, Size+1),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    ?line Data3Data2 = Data3++Data2,
    ?line {ok, Data3Data2} = ?FILE_MODULE:read(Fd5, 2*Size+1),
    ?line ok = ?FILE_MODULE:close(Fd5),
    %%
    ?line [] = flush(),
    ?line ?t:timetrap_cancel(Dog),
    ?line case os:type() of
	      vxworks -> 
		  {comment, "Some lines skipped on vxworks"};
	      _ ->
		  ok
	  end.


pid2name(doc) -> "Tests file:pid2name/1.";
pid2name(suite) -> [];
pid2name(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line RootDir = ?config(priv_dir, Config),
    ?line Base = test_server:temp_name(
		   filename:join(RootDir, "pid2name_")),
    ?line Name1 = [Base, '.txt'],
    ?line Name2 = Base ++ ".txt",
    %%
    ?line {ok, Pid} = file:open(Name1, [write]),
    ?line {ok, Name2} = file:pid2name(Pid),
    ?line undefined = file:pid2name(self()),
    ?line ok = file:close(Pid),
    ?line test_server:sleep(1000),
    ?line false = is_process_alive(Pid),
    ?line undefined = file:pid2name(Pid),
    %%
    ?line test_server:timetrap_cancel(Dog),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_ahead(suite) ->
    [];
read_ahead(doc) ->
    ["Tests the file open option {read_ahead, Size}"];

read_ahead(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(20)),
    %%
    ?line RootDir = ?config(priv_dir, Config),
    ?line File = filename:join(RootDir, 
			       atom_to_list(?MODULE)++"_read_ahead.txt"),
    ?line Data1 = "asdfghjkl", % Must be
    ?line Data2 = "qwertyuio", % same
    ?line Data3 = "zxcvbnm,.", % length
    ?line Size = length(Data1),
    ?line Size = length(Data2),
    ?line Size = length(Data3),
    %%
    %% Test caching of normal non-raw file
    ?line {ok, Fd1} = ?FILE_MODULE:open(File, [write]),
    ?line ok = ?FILE_MODULE:write(Fd1, [Data1|Data1]),
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Fd2} = ?FILE_MODULE:open(File, [read, {read_ahead, 2*Size}]),
    ?line {ok, Data1} = ?FILE_MODULE:read(Fd2, Size),
    ?line ok = ?FILE_MODULE:pwrite(Fd1, Size, Data2),
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Data1} = ?FILE_MODULE:read(Fd2, Size), % Will read cached data
    ?line Data2Data2Data2 = Data2++Data2++Data2,
    ?line ok = ?FILE_MODULE:pwrite(Fd1, eof, Data2Data2Data2),
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Data2Data2Data2} = 
	?FILE_MODULE:read(Fd2, 3*Size), % Read more than cache buffer
    ?line ok = ?FILE_MODULE:close(Fd1),
    ?line ok = ?FILE_MODULE:close(Fd2),
    %% Test caching of raw file and default parameters
    ?line {ok, Fd3} = ?FILE_MODULE:open(File, [raw, write]),
    ?line ok = ?FILE_MODULE:write(Fd3, [Data1|Data1]),
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Fd4} = ?FILE_MODULE:open(File, [raw, read, read_ahead]),
    ?line {ok, Data1} = ?FILE_MODULE:read(Fd4, Size),
    ?line ok = ?FILE_MODULE:pwrite(Fd3, Size, Data2),
    ?line ?t:sleep(1000), % Just in case the file system is slow
    ?line {ok, Data1} = ?FILE_MODULE:read(Fd4, Size), % Will read cached data
    ?line ok = ?FILE_MODULE:close(Fd3),
    ?line ok = ?FILE_MODULE:close(Fd4),
    %% Test if the file position works in combination with read_ahead
    ?line {ok, Fd5} = ?FILE_MODULE:open(File, [raw, read, write, read_ahead]),
    ?line ok = ?FILE_MODULE:truncate(Fd5),
    ?line ok = ?FILE_MODULE:write(Fd5, [Data1,Data1|Data3]),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    ?line {ok, Data1} = ?FILE_MODULE:read(Fd5, Size),
    ?line ok = ?FILE_MODULE:write(Fd5, Data2),
    ?line {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    ?line Data1Data2Data3 = Data1++Data2++Data3,
    ?line {ok, Data1Data2Data3} = ?FILE_MODULE:read(Fd5, 3*Size+1),
    ?line ok = ?FILE_MODULE:close(Fd5),
    %%
    ?line [] = flush(),
    ?line ?t:timetrap_cancel(Dog),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



segment_read(suite) ->
    [];
segment_read(doc) ->
    ["Tests the segmenting of large reads"];
segment_read(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(60)),
    %%
    ?line Name = filename:join(?config(priv_dir, Config),
			       ?MODULE_STRING ++ "_segment_read"),
    ?line SegSize = 256*1024,
    ?line SegCnt = SegSize div 4,
    ?line Cnt = 4 * SegCnt,
    ?line ok = create_file(Name, Cnt),
    %% 
    %% read_file/1
    %%
    ?line {ok, Bin} = ?FILE_MODULE:read_file(Name),
    ?line true = verify_bin(Bin, 0, Cnt),
    %%
    %% read/2
    %%
    %% Not segmented
    ?line {ok, FD1} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    ?line {ok, B1a} = ?FILE_MODULE:read(FD1, SegSize),
    ?line {ok, B1b} = ?FILE_MODULE:read(FD1, SegSize),
    ?line {ok, B1c} = ?FILE_MODULE:read(FD1, SegSize),
    ?line {ok, B1d} = ?FILE_MODULE:read(FD1, SegSize),
    ?line ok = ?FILE_MODULE:close(FD1),
    ?line true = verify_bin(B1a, 0*SegCnt, SegCnt),
    ?line true = verify_bin(B1b, 1*SegCnt, SegCnt),
    ?line true = verify_bin(B1c, 2*SegCnt, SegCnt),
    ?line true = verify_bin(B1d, 3*SegCnt, SegCnt),
    %%
    %% Segmented
    ?line {ok, FD2} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    ?line {ok, B2a} = ?FILE_MODULE:read(FD2, 1*SegSize),
    ?line {ok, B2b} = ?FILE_MODULE:read(FD2, 2*SegSize),
    ?line {ok, B2c} = ?FILE_MODULE:read(FD2, 2*SegSize),
    ?line ok = ?FILE_MODULE:close(FD2),
    ?line true = verify_bin(B2a, 0*SegCnt, 1*SegCnt),
    ?line true = verify_bin(B2b, 1*SegCnt, 2*SegCnt),
    ?line true = verify_bin(B2c, 3*SegCnt, 1*SegCnt),
    %%
    %% pread/3
    %%
    ?line {ok, FD3} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    %%
    %% Not segmented
    ?line {ok, B3d} = ?FILE_MODULE:pread(FD3, 3*SegSize, SegSize),
    ?line {ok, B3c} = ?FILE_MODULE:pread(FD3, 2*SegSize, SegSize),
    ?line {ok, B3b} = ?FILE_MODULE:pread(FD3, 1*SegSize, SegSize),
    ?line {ok, B3a} = ?FILE_MODULE:pread(FD3, 0*SegSize, SegSize),
    ?line true = verify_bin(B3a, 0*SegCnt, SegCnt),
    ?line true = verify_bin(B3b, 1*SegCnt, SegCnt),
    ?line true = verify_bin(B3c, 2*SegCnt, SegCnt),
    ?line true = verify_bin(B3d, 3*SegCnt, SegCnt),
    %%
    %% Segmented
    ?line {ok, B3g} = ?FILE_MODULE:pread(FD3, 3*SegSize, 2*SegSize),
    ?line {ok, B3f} = ?FILE_MODULE:pread(FD3, 1*SegSize, 2*SegSize),
    ?line {ok, B3e} = ?FILE_MODULE:pread(FD3, 0*SegSize, 1*SegSize),
    ?line true = verify_bin(B3e, 0*SegCnt, 1*SegCnt),
    ?line true = verify_bin(B3f, 1*SegCnt, 2*SegCnt),
    ?line true = verify_bin(B3g, 3*SegCnt, 1*SegCnt),
    %%
    ?line ok = ?FILE_MODULE:close(FD3),
    %%
    %% pread/2
    %%
    ?line {ok, FD5} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    %%
    %% +---+---+---+---+
    %% | 4 | 3 | 2 | 1 |
    %% +---+---+---+---+
    %% <       ^       >
    ?line {ok, [B5d, B5c, B5b, B5a]} = 
	?FILE_MODULE:pread(FD5, [{3*SegSize, SegSize},
				 {2*SegSize, SegSize},
				 {1*SegSize, SegSize},
				 {0*SegSize, SegSize}]),
    ?line true = verify_bin(B5a, 0*SegCnt, SegCnt),
    ?line true = verify_bin(B5b, 1*SegCnt, SegCnt),
    ?line true = verify_bin(B5c, 2*SegCnt, SegCnt),
    ?line true = verify_bin(B5d, 3*SegCnt, SegCnt),
    %%
    %% +---+-------+-------+
    %% | 3 |   2   |   1   |
    %% +---+-------+-------+
    %% <     ^     ^   >
    ?line {ok, [B5g, B5f, B5e]} = 
	?FILE_MODULE:pread(FD5, [{3*SegSize, 2*SegSize},
				 {1*SegSize, 2*SegSize},
				 {0*SegSize, 1*SegSize}]),
    ?line true = verify_bin(B5e, 0*SegCnt, 1*SegCnt),
    ?line true = verify_bin(B5f, 1*SegCnt, 2*SegCnt),
    ?line true = verify_bin(B5g, 3*SegCnt, 1*SegCnt),
    %%
    %%
    %% +-------+-----------+
    %% |   2   |     1     |
    %% +-------+-----------+
    %% <     ^     ^   >
    ?line {ok, [B5i, B5h]} = 
	?FILE_MODULE:pread(FD5, [{2*SegSize, 3*SegSize},
				 {0*SegSize, 2*SegSize}]),
    ?line true = verify_bin(B5h, 0*SegCnt, 2*SegCnt),
    ?line true = verify_bin(B5i, 2*SegCnt, 2*SegCnt),
    %%
    %% +-------+---+---+
    %% |   3   | 2 | 1 |
    %% +-------+---+---+
    %% <     ^     ^   >
    ?line {ok, [B5l, B5k, B5j]} = 
	?FILE_MODULE:pread(FD5, [{3*SegSize, 1*SegSize},
				 {2*SegSize, 1*SegSize},
				 {0*SegSize, 2*SegSize}]),
    ?line true = verify_bin(B5j, 0*SegCnt, 2*SegCnt),
    ?line true = verify_bin(B5k, 2*SegCnt, 1*SegCnt),
    ?line true = verify_bin(B5l, 3*SegCnt, 1*SegCnt),
    %%
    %% Real time response time test.
    %%
    Req = lists:flatten(lists:duplicate(17,
					[{2*SegSize, 2*SegSize},
					 {0*SegSize, 2*SegSize}])),
    ?line {{ok, _}, Comment} = 
	response_analysis(?FILE_MODULE, pread, [FD5, Req]),
    ?line ok = ?FILE_MODULE:close(FD5),
    %%
    ?line [] = flush(),
    ?line ?t:timetrap_cancel(Dog),
    {comment, Comment}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



segment_write(suite) ->
    [];
segment_write(doc) ->
    ["Tests the segmenting of large writes"];
segment_write(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(60)),
    %%
    ?line Name = filename:join(?config(priv_dir, Config),
			       ?MODULE_STRING ++ "_segment_write"),
    ?line SegSize = 256*1024,
    ?line SegCnt = SegSize div 4,
    ?line Cnt = 4 * SegCnt,
    ?line Bin = create_bin(0, Cnt),
    %%
    %% write/2
    %%
    %% Not segmented
    ?line {ok, FD1} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ?line ok = ?FILE_MODULE:write(FD1, subbin(Bin, 0*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:write(FD1, subbin(Bin, 1*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:write(FD1, subbin(Bin, 2*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:write(FD1, subbin(Bin, 3*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:close(FD1),
    ?line true = verify_file(Name, Cnt),
    %%
    %% Segmented
    ?line {ok, FD2} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ?line ok = ?FILE_MODULE:write(FD2, subbin(Bin, 0*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:write(FD2, subbin(Bin, 1*SegSize, 2*SegSize)),
    ?line ok = ?FILE_MODULE:write(FD2, subbin(Bin, 3*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:close(FD2),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +---+---+---+---+
    %% |   |   |   |   |
    %% +---+---+---+---+
    %% <       ^       >
    ?line ok = write_file(Name, [subbin(Bin, 0*SegSize, 1*SegSize),
				 subbin(Bin, 1*SegSize, 1*SegSize),
				 subbin(Bin, 2*SegSize, 1*SegSize),
				 subbin(Bin, 3*SegSize, 1*SegSize)]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +---+-------+---+
    %% |   |       |   |
    %% +---+-------+---+
    %% <     ^     ^   >
    ?line ok = write_file(Name, [subbin(Bin, 0*SegSize, 1*SegSize),
				 subbin(Bin, 1*SegSize, 2*SegSize),
				 subbin(Bin, 3*SegSize, 1*SegSize)]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +-------+-------+
    %% |       |       |
    %% +-------+-------+
    %% <     ^     ^   >
    ?line ok = write_file(Name, [subbin(Bin, 0*SegSize, 2*SegSize),
				 subbin(Bin, 2*SegSize, 2*SegSize)]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +-------+---+---+
    %% |       |   |   |
    %% +-------+---+---+
    %% <     ^     ^   >
    ?line ok = write_file(Name, [subbin(Bin, 0*SegSize, 2*SegSize),
				 subbin(Bin, 2*SegSize, 1*SegSize),
				 subbin(Bin, 3*SegSize, 1*SegSize)]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% pwrite/3
    %%
    %% Not segmented
    ?line {ok, FD3} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ?line ok = ?FILE_MODULE:pwrite(FD3, 3*SegSize, 
				   subbin(Bin, 3*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:pwrite(FD3, 2*SegSize, 
				   subbin(Bin, 2*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:pwrite(FD3, 1*SegSize, 
				   subbin(Bin, 1*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:pwrite(FD3, 0*SegSize, 
				   subbin(Bin, 0*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:close(FD3),
    ?line true = verify_file(Name, Cnt),
    %%
    %% Segmented
    ?line {ok, FD4} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ?line ok = ?FILE_MODULE:pwrite(FD4, 3*SegSize, 
				   subbin(Bin, 3*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:pwrite(FD4, 1*SegSize, 
				   subbin(Bin, 1*SegSize, 2*SegSize)),
    ?line ok = ?FILE_MODULE:pwrite(FD4, 0*SegSize, 
				   subbin(Bin, 0*SegSize, 1*SegSize)),
    ?line ok = ?FILE_MODULE:close(FD4),
    ?line true = verify_file(Name, Cnt),



    %%
    %% pwrite/2
    %%
    %% Not segmented
    ?line {ok, FD5} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ?line ok = ?FILE_MODULE:pwrite(FD5, [{3*SegSize,
					  subbin(Bin, 3*SegSize, 1*SegSize)}]),
    ?line ok = ?FILE_MODULE:pwrite(FD5, [{2*SegSize,
					  subbin(Bin, 2*SegSize, 1*SegSize)}]),
    ?line ok = ?FILE_MODULE:pwrite(FD5, [{1*SegSize,
					  subbin(Bin, 1*SegSize, 1*SegSize)}]),
    ?line ok = ?FILE_MODULE:pwrite(FD5, [{0*SegSize,
					  subbin(Bin, 0*SegSize, 1*SegSize)}]),
    ?line ok = ?FILE_MODULE:close(FD5),
    ?line true = verify_file(Name, Cnt),
    %%
    %% Segmented
    ?line {ok, FD6} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ?line ok = ?FILE_MODULE:pwrite(FD6, [{3*SegSize,
					  subbin(Bin, 3*SegSize, 1*SegSize)}]),
    ?line ok = ?FILE_MODULE:pwrite(FD6, [{1*SegSize,
					  subbin(Bin, 1*SegSize, 2*SegSize)}]),
    ?line ok = ?FILE_MODULE:pwrite(FD6, [{0*SegSize,
					  subbin(Bin, 0*SegSize, 1*SegSize)}]),
    ?line ok = ?FILE_MODULE:close(FD6),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +---+---+---+---+
    %% | 4 | 3 | 2 | 1 |
    %% +---+---+---+---+
    %% <       ^       >
    ?line ok = pwrite_file(Name, [{3*SegSize, 
				   subbin(Bin, 3*SegSize, 1*SegSize)},
				  {2*SegSize, 
				   subbin(Bin, 2*SegSize, 1*SegSize)},
				  {1*SegSize, 
				   subbin(Bin, 1*SegSize, 1*SegSize)},
				  {0*SegSize, 
				   subbin(Bin, 0*SegSize, 1*SegSize)}]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +---+-------+---+
    %% | 3 |   2   | 1 |
    %% +---+-------+---+
    %% <     ^     ^   >
    ?line ok = pwrite_file(Name, [{3*SegSize, 
				   subbin(Bin, 3*SegSize, 1*SegSize)},
				  {1*SegSize, 
				   subbin(Bin, 1*SegSize, 2*SegSize)},
				  {0*SegSize, 
				   subbin(Bin, 0*SegSize, 1*SegSize)}]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +-------+-------+
    %% |   2   |   1   |
    %% +-------+-------+
    %% <     ^     ^   >
    ?line ok = pwrite_file(Name, [{2*SegSize, 
				   subbin(Bin, 2*SegSize, 2*SegSize)},
				  {0*SegSize, 
				   subbin(Bin, 0*SegSize, 2*SegSize)}]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% +-------+---+---+
    %% |   3   | 2 | 1 |
    %% +-------+---+---+
    %% <     ^     ^   >
    ?line ok = pwrite_file(Name, [{3*SegSize, 
				   subbin(Bin, 3*SegSize, 1*SegSize)},
				  {2*SegSize, 
				   subbin(Bin, 2*SegSize, 1*SegSize)},
				  {0*SegSize, 
				   subbin(Bin, 0*SegSize, 2*SegSize)}]),
    ?line true = verify_file(Name, Cnt),
    %%
    %% Real time response time test.
    %%
    ?line {ok, FD7} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    Req = lists:flatten(lists:duplicate(17,
					[{2*SegSize, 
					  subbin(Bin, 2*SegSize, 2*SegSize)},
					{0*SegSize, 
					 subbin(Bin, 0*SegSize, 2*SegSize)}])),
    ?line {ok, Comment} = 
	response_analysis(?FILE_MODULE, pwrite, [FD7, Req]),
    ?line ok = ?FILE_MODULE:close(FD7),
    %%
    ?line [] = flush(),
    ?line ?t:timetrap_cancel(Dog),
    {comment, Comment}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ipread(suite) ->
    [];
ipread(doc) ->
    ["Test Dets special indirect pread"];
ipread(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(30)),
    %%
    ?line Dir = ?config(priv_dir, Config),
    ?line ok = ipread_int(Dir, [raw, binary]),
    ?line ok = ipread_int(Dir, [raw]),
    ?line ok = ipread_int(Dir, [binary]),
    ?line ok = ipread_int(Dir, []),
    ?line ok = ipread_int(Dir, [ram, binary]),
    ?line ok = ipread_int(Dir, [ram]),
    %%
    ?line [] = flush(),
    ?line ?t:timetrap_cancel(Dog),
    ok.

ipread_int(Dir, ModeList) ->
    ?line Name = 
	filename:join(Dir, 
		      lists:flatten([?MODULE_STRING, "_ipread",
		       lists:map(fun (X) ->
					 ["_", atom_to_list(X)]
				 end,
				 ModeList)])),
    ?line io:format("ipread_int<~p, ~p>~n", [Name, ModeList]),
    ?line {Conv, Sizeof} = 
	case lists:member(binary, ModeList) of
	    true ->
		{fun (Bin) when is_binary(Bin) -> Bin;
		     (List) when is_list(List) -> list_to_binary(List)
		 end, 
		 {erlang, size}};
	    false ->
		{fun (Bin) when is_binary(Bin) -> binary_to_list(Bin);
		     (List) when is_list(List) -> List
		 end, 
		 {erlang, length}}
	end,
    ?line Pos = 4711,
    ?line Data = Conv("THE QUICK BROWN FOX JUMPS OVER A LAZY DOG"),
    ?line Size = Sizeof(Data),
    ?line Init = Conv("                 "),
    ?line SizeInit = Sizeof(Init),
    ?line Head = Conv(<<Size:32/big-unsigned, Pos:32/big-unsigned>>),
    ?line Filler = Conv(bytes($ , Pos-SizeInit-Sizeof(Head))),
    ?line Size1 = Size+1,
    ?line SizePos = Size+Pos,
    %%
    ?line {ok, FD} = ?FILE_MODULE:open(Name, [write, read | ModeList]),
    ?line ok = ?FILE_MODULE:truncate(FD),
    ?line ok = ?FILE_MODULE:write(FD, Init),
    ?line ok = ?FILE_MODULE:write(FD, Head),
    ?line ok = ?FILE_MODULE:write(FD, Filler),
    ?line ok = ?FILE_MODULE:write(FD, Data),
    %% Correct read
    ?line {ok, {Size, Pos, Data}} = 
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, infinity),
    %% Invalid header - size > max
    ?line eof = 
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size-1),
    %% Data block protudes over eof
    ?line ok = 
	?FILE_MODULE:pwrite(FD, SizeInit, 
			    <<Size1:32/big-unsigned, 
			     Pos:32/big-unsigned>>),
    ?line {ok, {Size1, Pos, Data}} = 
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size1),
    %% Data block outside file
    ?line ok =
	?FILE_MODULE:pwrite(FD, SizeInit, 
			    <<Size:32/big-unsigned, 
			     SizePos:32/big-unsigned>>),
    ?line {ok, {Size, SizePos, eof}} =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size),
    %% Zero size
    ?line ok = 
	?FILE_MODULE:pwrite(FD, SizeInit, 
			    <<0:32/big-unsigned, 
			     Pos:32/big-unsigned>>),
    ?line {ok, {0, Pos, eof}} = 
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size),
    %% Invalid header - protudes over eof
    ?line eof = 
	?FILE_MODULE:ipread_s32bu_p32bu(FD, 
					Pos+Size-(Sizeof(Head)-1), 
					infinity),
    %% Header not even in file
    ?line eof = 
	?FILE_MODULE:ipread_s32bu_p32bu(FD, Pos+Size, infinity),
    %%
    ?line ok = ?FILE_MODULE:close(FD),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interleaved_read_write(suite) ->
    [];
interleaved_read_write(doc) ->
    ["Tests interleaved read and writes"];
interleaved_read_write(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(30)),
    %%
    ?line Dir = ?config(priv_dir, Config),
    ?line File = 
	filename:join(Dir, ?MODULE_STRING++"interleaved_read_write.txt"),
    ?line {ok,F1} = ?FILE_MODULE:open(File, [write]),
    ?line ok = ?FILE_MODULE:write(F1, "data---r1."), % 10 chars each
    ?line ok = ?FILE_MODULE:write(F1, "data---r2."),
    ?line ok = ?FILE_MODULE:write(F1, "data---r3."),
    ?line ok = ?FILE_MODULE:close(F1),
    ?line {ok,F2} = ?FILE_MODULE:open(File, [read, write]),
    ?line {ok, "data---r1."} = ?FILE_MODULE:read(F2, 10),
    ?line ok = ?FILE_MODULE:write(F2, "data---w2."),
    ?line ok = ?FILE_MODULE:close(F2),
    ?line {ok,F3} = ?FILE_MODULE:open(File, [read]),
    ?line {ok, "data---r1."} = ?FILE_MODULE:read(F3, 10),
    ?line {ok, "data---w2."} = ?FILE_MODULE:read(F3, 10),
    ?line {ok, "data---r3."} = ?FILE_MODULE:read(F3, 10),
    ?line eof = ?FILE_MODULE:read(F3, 1),
    ?line ok = ?FILE_MODULE:close(F2),
    %%
    ?line [] = flush(),
    ?line ?t:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5814(suite) ->
    [];
otp_5814(doc) ->
    ["OTP-5814. eval/consult/script return correct line numbers"];
otp_5814(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(10)),
    PrivDir = ?config(priv_dir, Config),
    File = filename:join(PrivDir, "otp_5814"),
    Path = [PrivDir],
    ?line ok = file:write_file(File, <<"{a,b,c}.
                                        a.
                                        b.
                                        c.
                                        {d,e,
                                        [}.">>),
    ?line {error, {6,erl_parse,_}} = file:eval(File),
    ?line {error, {6,erl_parse,_}} = file:consult(File),
    ?line {error, {6,erl_parse,_}} = file:path_consult(Path, File),
    ?line {error, {6,erl_parse,_}} = file:path_eval(Path, File),
    ?line {error, {6,erl_parse,_}} = file:script(File),
    ?line {error, {6,erl_parse,_}} = file:path_script(Path, File),

    ?line ok = file:write_file(File, <<>>),
    ?line {error, {1,file,undefined_script}} = file:path_script(Path, File),

    %% The error is not propagated...
    ?line ok = file:write_file(File, <<"a.
                                        b.
                                        1/0.">>),
    ?line {error, {3, file, {error, badarith, _}}} = file:eval(File),
    
    ?line ok = file:write_file(File, <<"erlang:raise(throw, apa, []).">>),
    ?line {error, {1, file, {throw, apa, _}}} = file:eval(File),

    file:delete(File),
    ?line ?t:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

large_file(suite) ->
    [];
large_file(doc) ->
    ["Tests positioning in large files (> 4G)"];
large_file(Config) when is_list(Config) ->
    case {os:type(),os:version()} of
	{{win32,nt},_} ->
	    do_large_file(Config);
	{{unix,sunos},{A,B,C}}
	when A == 5, B == 5, C >= 1;   A == 5, B >= 6;   A >= 6 ->
	    do_large_file(Config);
	{{unix,Unix},_} when Unix =:= linux; Unix =:= darwin ->
	    N = unix_free(Config),
	    io:format("Free: ~w KByte~n", [N]),
	    if N < 5 * (1 bsl 20) ->
		    %% Less than 5 GByte free
		    {skipped,"Less than 5 GByte free"};
	       true ->
		    do_large_file(Config)
	    end;
	_ -> 
	    {skipped,"Only supported on Win32, Linux, or SunOS >= 5.5.1"}
    end.

unix_free(Config) ->
    Cmd = ["df -k '",?config(priv_dir, Config),"'"],
    DF0 = os:cmd(Cmd),
    io:format("$ ~s~n~s", [Cmd,DF0]),
    [$\n|DF1] = lists:dropwhile(fun ($\n) -> false; (_) -> true end, DF0),
    {ok,[N],_} = io_lib:fread(" ~*s ~d", DF1),
    N.

do_large_file(Config) ->
    ?line Watchdog = ?t:timetrap(?t:minutes(4)),
    %%
    ?line Name = filename:join(?config(priv_dir, Config),
			       ?MODULE_STRING ++ "_large_file"),
    ?line Tester = self(),
    Deleter = 
	spawn(
	  fun() ->
		  Mref = erlang:monitor(process, Tester),
		  receive
		      {'DOWN',Mref,_,_,_} -> ok;
		      {Tester,done} -> ok
		  end,
		  ?FILE_MODULE:delete(Name)
	  end),
    %%
    ?line S = "1234567890",
    L = length(S),
    R = lists:reverse(S),
    P = 1 bsl 32,
    Ss = lists:sort(S),
    Rs = lists:reverse(Ss),
    ?line {ok,F}  = ?FILE_MODULE:open(Name, [raw,read,write]),
    ?line ok      = ?FILE_MODULE:write(F, S),
    ?line {ok,P}  = ?FILE_MODULE:position(F, P),
    ?line ok      = ?FILE_MODULE:write(F, R),
    ?line {ok,0}  = ?FILE_MODULE:position(F, bof),
    ?line {ok,S}  = ?FILE_MODULE:read(F, L),
    ?line {ok,P}  = ?FILE_MODULE:position(F, {eof,-L}),
    ?line {ok,R}  = ?FILE_MODULE:read(F, L+1),
    ?line {ok,S}  = ?FILE_MODULE:pread(F, 0, L),
    ?line {ok,R}  = ?FILE_MODULE:pread(F, P, L+1),
    ?line ok      = ?FILE_MODULE:pwrite(F, 0, Ss),
    ?line ok      = ?FILE_MODULE:pwrite(F, P, Rs),
    ?line {ok,0}  = ?FILE_MODULE:position(F, bof),
    ?line {ok,Ss} = ?FILE_MODULE:read(F, L),
    ?line {ok,P}  = ?FILE_MODULE:position(F, {eof,-L}),
    ?line {ok,Rs} = ?FILE_MODULE:read(F, L+1),
    ?line ok      = ?FILE_MODULE:close(F),
    %%
    ?line Mref = erlang:monitor(process, Deleter),
    ?line Deleter ! {Tester,done},
    ?line receive {'DOWN',Mref,_,_,_} -> ok end,
    %%
    ?line ?t:timetrap_cancel(Watchdog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



response_analysis(Module, Function, Arguments) ->
    Parent = self(),
    ?line erlang:yield(), % Schedule out before test
    ?line Child = 
	spawn_link(
	  fun () ->
		  receive {Parent, start, Ts} -> ok end,
		  Stat = 
		      iterate(response_stat(response_stat(init, Ts),
					    erlang:now()), 
			      done,
			      fun (S) ->
				      erlang:yield(),
				      receive
					  {Parent, stop} ->
					      done
				      after 0 ->
					      response_stat(S, erlang:now())
				      end
			      end),
		  Parent ! {self(), stopped, response_stat(Stat, erlang:now())}
	  end),
    ?line Child ! {Parent, start, erlang:now()},
    ?line Result = apply(Module, Function, Arguments),
    ?line Child ! {Parent, stop},
    ?line {N, Sum, _, M, Max} = receive {Child, stopped, X} -> X end,
    ?line Mean_ms = (0.001*Sum) / (N-1),
    ?line Max_ms = 0.001 * Max,
    ?line Comment = 
	lists:flatten(
	  io_lib:format(
	    "Scheduling interval: Mean = ~.3f ms, "
	    ++"Max = ~.3f ms for no ~p of ~p.~n",
	    [Mean_ms, Max_ms, M, (N-1)])),
    ?line {Result, Comment}.
    


response_stat(init, Ts) ->
    {0, 0, Ts, 0, 0};
response_stat({N, Sum, {A1, B1, C1}, M, Max}, {A2, B2, C2} = Ts) ->
    D = C2-C1 + 1000000*((B2-B1) + 1000000*(A2-A1)),
    if D > Max ->
	    {N+1, Sum+D, Ts, N, D};
       true ->
	    {N+1, Sum+D, Ts, M, Max}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% This function is kept just for benchmarking reasons.
%% create_file/2 below is some 44 times faster.

create_file_slow(Name, N) when is_integer(N), N >= 0 ->
    ?line {ok, FD} = 
	?FILE_MODULE:open(Name, [raw, write, delayed_write, binary]),
    ?line ok = create_file_slow(FD, 0, N),
    ?line ok = ?FILE_MODULE:close(FD),
    ok.

create_file_slow(_FD, M, M) ->
    ok;
create_file_slow(FD, M, N) ->
    ok = ?FILE_MODULE:write(FD, <<M:32/unsigned>>),
    create_file_slow(FD, M+1, N).



%% Creates a file 'Name' containing 'N' unsigned 32 bit integers 
%% from 0 to N-1.

create_file(Name, N) when is_integer(N), N >= 0 ->
    ?line {ok, FD} = 
	?FILE_MODULE:open(Name, [raw, write, delayed_write, binary]),
    ?line ok = create_file(FD, 0, N),
    ?line ok = ?FILE_MODULE:close(FD),
    ok.

create_file(_FD, M, M) ->
    ok;
create_file(FD, M, N) when M + 1024 =< N ->
    create_file(FD, M, M + 1024, []),
    create_file(FD, M + 1024, N);
create_file(FD, M, N) ->
    create_file(FD, M, N, []).

create_file(FD, M, M, R) ->
    ok = ?FILE_MODULE:write(FD, R);
create_file(FD, M, N0, R) when M + 8 =< N0 ->
    N1  = N0-1,  N2  = N0-2,  N3  = N0-3,  N4  = N0-4, 
    N5  = N0-5,  N6  = N0-6,  N7  = N0-7,  N8  = N0-8, 
    create_file(FD, M, N8, 
		  [<<N8:32/unsigned,  N7:32/unsigned, 
		    N6:32/unsigned,  N5:32/unsigned, 
		    N4:32/unsigned,  N3:32/unsigned, 
		    N2:32/unsigned,  N1:32/unsigned>> | R]);
create_file(FD, M, N0, R) ->
    N1 = N0-1,
    create_file(FD, M, N1, [<<N1:32/unsigned>> | R]).



create_bin(M, N) when is_integer(M), is_integer(N), N >= 0, M >= 0 ->
    create_bin(M, M+N, []).

create_bin(N, N, R) ->
    list_to_binary(R);
create_bin(M, N0, R) when M+8 =< N0 ->
    N1  = N0-1,  N2  = N0-2,  N3  = N0-3,  N4  = N0-4, 
    N5  = N0-5,  N6  = N0-6,  N7  = N0-7,  N8  = N0-8, 
    create_bin(M, N8,
	       [<<N8:32/unsigned,  N7:32/unsigned, 
		 N6:32/unsigned,  N5:32/unsigned, 
		 N4:32/unsigned,  N3:32/unsigned, 
		 N2:32/unsigned,  N1:32/unsigned>> | R]);
create_bin(M, N0, R) ->
    N1 = N0-1,
    create_bin(M, N1, [<<N1:32/unsigned>> | R]).
    
    


verify_bin(<<>>, _, 0) ->
    true;
verify_bin(<<>>, _, _) ->
    false;
verify_bin(Bin, N, Cnt) ->
    N0 = N + 0, N1 = N + 1, N2 = N + 2, N3 = N + 3, 
    N4 = N + 4, N5 = N + 5, N6 = N + 6, N7 = N + 7, 
    case Bin of
	<<N0:32/unsigned, N1:32/unsigned, N2:32/unsigned, N3:32/unsigned, 
	 N4:32/unsigned, N5:32/unsigned, N6:32/unsigned, N7:32/unsigned,
	 B/binary>> ->
	    verify_bin(B, N+8, Cnt-8);
	<<N:32/unsigned, B/binary>> ->
	    verify_bin(B, N+1, Cnt-1);
	_ ->
	    false
    end.



verify_file(Name, N) when is_integer(N), N >= 0 ->
    case ?FILE_MODULE:open(Name, [raw, read, binary]) of
	{ok, FD} ->
	    Result = verify_file(FD, 0, 64*1024, N),
	    ok = ?FILE_MODULE:close(FD),
	    Result;
	Error ->
	    Error
    end.

verify_file(FD, N, _, N) ->
    case ?FILE_MODULE:read(FD, 1) of
	eof ->
	    true;
	{ok, _} ->
	    false
    end;
verify_file(FD, M, Cnt, N) when M+Cnt =< N ->
    case ?FILE_MODULE:read(FD, 4*Cnt) of
	{ok, Bin} ->
	    case verify_bin(Bin, M, Cnt) of
		true ->
		    verify_file(FD, M+Cnt, Cnt, N);
		false ->
		    false
	    end;
	_ ->
	    false
    end;
verify_file(FD, M, _Cnt, N) ->
    verify_file(FD, M, N-M, N).



subbin(Bin, M, N) ->
    <<_:M/binary, B:N/binary, _/binary>> = Bin,
    B.



write_file(Name, Data) ->
    case ?FILE_MODULE:open(Name, [raw, write, binary]) of
	{ok, FD} ->
	    Result = ?FILE_MODULE:write(FD, Data),
	    case {Result, ?FILE_MODULE:close(FD)} of
		{ok, R} -> R;
		_ -> Result
	    end;
	Error ->
	    Error
    end.

pwrite_file(Name, Data) ->
    case ?FILE_MODULE:open(Name, [raw, write, binary]) of
	{ok, FD} ->
	    Result = ?FILE_MODULE:pwrite(FD, Data),
	    case {Result, ?FILE_MODULE:close(FD)} of
		{ok, R} -> R;
		_ -> Result
	    end;
	Error ->
	    Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read_line tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


read_line_testdata(PrivDir) ->
    All0 = [{fun read_line_create0/1,"Testdata1.txt",5,10},
	   {fun read_line_create1/1,"Testdata2.txt",401,802},
	   {fun read_line_create2/1,"Testdata3.txt",1,2},
	   {fun read_line_create3/1,"Testdata4.txt",601,fail},
	   {fun read_line_create4/1,"Testdata5.txt",601,1002},
	   {fun read_line_create5/1,"Testdata6.txt",601,1202},
	   {fun read_line_create6/1,"Testdata7.txt",601,1202},
	   {fun read_line_create7/1,"Testdata8.txt",4001,8002}],
    [ {A,filename:join([PrivDir,B]),C,D} || {A,B,C,D} <- All0 ].

read_line_create_files(TestData) ->
    [ Function(File) || {Function,File,_,_} <- TestData ].

read_line_remove_files(TestData) ->
    [ file:delete(File) || {_Function,File,_,_} <- TestData ].

read_line_1(suite) -> 
    [];
read_line_1(doc) ->
    ["read_line with prim_file"];
read_line_1(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line All = read_line_testdata(PrivDir),
    ?line read_line_create_files(All),
    ?line [ begin 
		io:format("read_line_all: ~s~n",[File]),
		{X,_} = read_line_all(File),
		true
	    end || {_,File,X,_} <- All ],
    ?line [ begin 
		io:format("read_line_all_alternating: ~s~n",[File]),
		{Y,_} = read_line_all_alternating(File),
		true
	    end || {_,File,_,Y} <- All , Y =/= fail],
    ?line [ begin 
		io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
		{'EXIT',_} = (catch read_line_all_alternating(File)),
		true
	    end || {_,File,_,Y} <- All , Y =:= fail],
    ?line read_line_remove_files(All),
    ok.
read_line_2(suite) -> 
    [];
read_line_2(doc) ->
    ["read_line with file"];
read_line_2(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line All = read_line_testdata(PrivDir),
    ?line read_line_create_files(All),
    ?line [ begin 
		io:format("read_line_all: ~s~n",[File]),
		{X,_} = read_line_all2(File),
		true
	    end || {_,File,X,_} <- All ],
    ?line [ begin 
		io:format("read_line_all_alternating: ~s~n",[File]),
		{Y,_} = read_line_all_alternating2(File),
		true
	    end || {_,File,_,Y} <- All , Y =/= fail],
    ?line [ begin 
		io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
		{'EXIT',_} = (catch read_line_all_alternating2(File)),
		true
	    end || {_,File,_,Y} <- All , Y =:= fail],
    ?line read_line_remove_files(All),
    ok.
read_line_3(suite) -> 
    [];
read_line_3(doc) ->
    ["read_line with raw file"];
read_line_3(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line All = read_line_testdata(PrivDir),
    ?line read_line_create_files(All),
    ?line [ begin 
		io:format("read_line_all: ~s~n",[File]),
		{X,_} = read_line_all3(File),
		true
	    end || {_,File,X,_} <- All ],
    ?line [ begin 
		io:format("read_line_all_alternating: ~s~n",[File]),
		{Y,_} = read_line_all_alternating3(File),
		true
	    end || {_,File,_,Y} <- All , Y =/= fail],
    ?line [ begin 
		io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
		{'EXIT',_} = (catch read_line_all_alternating3(File)),
		true
	    end || {_,File,_,Y} <- All , Y =:= fail],
    ?line read_line_remove_files(All),
    ok.
read_line_4(suite) -> 
    [];
read_line_4(doc) ->
    ["read_line with raw buffered file"];
read_line_4(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line All = read_line_testdata(PrivDir),
    ?line read_line_create_files(All),
    ?line [ begin 
		io:format("read_line_all: ~s~n",[File]),
		{X,_} = read_line_all4(File),
		true
	    end || {_,File,X,_} <- All ],
    ?line [ begin 
		io:format("read_line_all_alternating: ~s~n",[File]),
		{Y,_} = read_line_all_alternating4(File),
		true
	    end || {_,File,_,Y} <- All , Y =/= fail],
    ?line [ begin 
		io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
		{'EXIT',_} = (catch read_line_all_alternating4(File)),
		true
	    end || {_,File,_,Y} <- All , Y =:= fail],
    ?line read_line_remove_files(All),
    ok.

rl_lines() ->
    [ <<"hej">>,<<"hopp">>,<<"i">>,<<"lingon\rskogen">>].

read_line_create0(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
    file:write(F,<<"Inget radslut\r">>),
    file:close(F).
read_line_create1(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,100)],
    file:close(F).
read_line_create2(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  [ file:write(F,[R]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,200)],
    file:write(F,<<"\r\n">>),
    file:close(F).

read_line_create3(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  file:write(F,<<"\r\n">>),
	  file:write(F,<<"\r\n">>),
	  [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,100)],
    file:close(F).

read_line_create4(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  file:write(F,<<"\n">>),
	  file:write(F,<<"\n">>),
	  [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,100)],
    file:close(F).

read_line_create5(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  file:write(F,<<"i\n">>),
	  file:write(F,<<"i\n">>),
	  [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,100)],
    file:close(F).

read_line_create6(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  file:write(F,<<"i\r\n">>),
	  file:write(F,<<"i\r\n">>),
	  [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,100)],
    file:close(F).
read_line_create7(Filename) ->
    {ok,F} = file:open(Filename,[write]),
    L = rl_lines(),
    [ begin
	  [ file:write(F,[R,<<"\r\n">>]) || R <- L ], 
	  file:write(F,<<"Inget radslut\r">>)
      end || _ <- lists:seq(1,1000)],
    file:close(F).

read_line_all(Filename) ->
    {ok,F} = prim_file:open(Filename,[read,binary]),
    X=read_rl_lines(F),
    prim_file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.

read_line_all2(Filename) ->
    {ok,F} = file:open(Filename,[read,binary]),
    X=read_rl_lines2(F),
    file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.

read_line_all3(Filename) ->
    {ok,F} = file:open(Filename,[read,binary,raw]),
    X=read_rl_lines2(F),
    file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.
read_line_all4(Filename) ->
    {ok,F} = file:open(Filename,[read,binary,raw,{read_ahead,8192}]),
    X=read_rl_lines2(F),
    file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.

read_rl_lines(F) ->
    case prim_file:read_line(F) of
	eof ->
	    [];
	{error,X} ->
	    {error,X};
	List ->
	    [List | read_rl_lines(F)]
    end.

read_rl_lines2(F) ->
    case file:read_line(F) of
	eof ->
	    [];
	{error,X} ->
	    {error,X};
	List ->
	    [List | read_rl_lines2(F)]
    end.

read_line_all_alternating(Filename) ->
    {ok,F} = prim_file:open(Filename,[read,binary]),
    X=read_rl_lines(F,true),
    prim_file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.

read_line_all_alternating2(Filename) ->
    {ok,F} = file:open(Filename,[read,binary]),
    X=read_rl_lines2(F,true),
    file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.
read_line_all_alternating3(Filename) ->
    {ok,F} = file:open(Filename,[read,binary,raw]),
    X=read_rl_lines2(F,true),
    file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.
read_line_all_alternating4(Filename) ->
    {ok,F} = file:open(Filename,[read,binary,raw,{read_ahead,8192}]),
    X=read_rl_lines2(F,true),
    file:close(F),
    Bin = list_to_binary([B || {ok,B} <- X]),
    Bin = re:replace(list_to_binary([element(2,file:read_file(Filename))]),
		     "\r\n","\n",[global,{return,binary}]),
    {length(X),Bin}.

read_rl_lines(F,Alternate) ->
    case begin
	     case Alternate of
		 true -> prim_file:read(F,1);
		 false -> prim_file:read_line(F)
	     end 
	 end of
	eof ->
	    [];
	{error,X} ->
	    {error,X};
	List ->
	    [List | read_rl_lines(F,not Alternate)]
    end.
read_rl_lines2(F,Alternate) ->
    case begin
	     case Alternate of
		 true -> file:read(F,1);
		 false -> file:read_line(F)
	     end 
	 end of
	eof ->
	    [];
	{error,X} ->
	    {error,X};
	List ->
	    [List | read_rl_lines2(F,not Alternate)]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bytes(B, N)
  when is_integer(B), 0 =< B, B =< 255, is_integer(N), N > 2, N band 1 == 0 ->
    [bytes(B, N bsr 1), bytes(B, N bsr 1)];
bytes(B, 0)
  when is_integer(B), 0 =< B, B =< 255 ->
    [];
bytes(B, 2)
  when is_integer(B), 0 =< B, B =< 255 ->
    [B, B];
bytes(B, N)
  when is_integer(B), 0 =< B, B =< 255, is_integer(N), N > 0 ->
    [B, bytes(B, N-1)].


%% A simple loop construct.
%%
%% Calls 'Fun' with argument 'Start' first and then repeatedly with
%% its returned value (state) until 'Fun' returns 'Stop'. Then
%% the last state value that was not 'Stop' is returned.

iterate(Start, Done, Fun) when is_function(Fun) ->
    iterate(Start, Done, Fun, Start).

iterate(Done, Done, _Fun, I) ->
    I;
iterate(I, Done, Fun, _) ->
    iterate(Fun(I), Done, Fun, I).



flush() ->
    flush([]).

flush(Msgs) ->
    receive
	Msg ->
	    flush([Msg | Msgs])
    after 0 ->
	    lists:reverse(Msgs)
    end.
