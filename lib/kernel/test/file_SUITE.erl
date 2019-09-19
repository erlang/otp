%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

%% This is a development feature when developing a new file module,
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

-define(PRIM_FILE, prim_file).

-module(?FILE_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,
	 read_write_file/1, names/1]).
-export([cur_dir_0/1, cur_dir_1/1, make_del_dir/1,
	 list_dir/1,list_dir_error/1,
	 untranslatable_names/1, untranslatable_names_error/1,
	 pos1/1, pos2/1, pos3/1]).
-export([close/1, consult1/1, path_consult/1, delete/1]).
-export([ eval1/1, path_eval/1, script1/1, path_script/1,
	  open1/1,
	  old_modes/1, new_modes/1, path_open/1, open_errors/1]).
-export([ file_info_basic_file/1, file_info_basic_directory/1,
	  file_info_bad/1, file_info_times/1, file_write_file_info/1,
          file_wfi_helpers/1]).
-export([rename/1, access/1, truncate/1, datasync/1, sync/1,
	 read_write/1, pread_write/1, append/1, exclusive/1]).
-export([ e_delete/1, e_rename/1, e_make_dir/1, e_del_dir/1]).
-export([otp_5814/1, otp_10852/1]).

-export([ read_not_really_compressed/1,
	  read_compressed_cooked/1, read_compressed_cooked_binary/1,
	  read_cooked_tar_problem/1,
	  write_compressed/1, compress_errors/1, catenated_gzips/1,
	  compress_async_crash/1]).

-export([ make_link/1, read_link_info_for_non_link/1, symlinks/1]).

-export([copy/1]).

-export([new_slave/2, old_slave/2, run_test/2]).

-export([delayed_write/1, read_ahead/1, segment_read/1, segment_write/1]).

-export([ipread/1]).

-export([pid2name/1]).

-export([interleaved_read_write/1]).

-export([unicode/1]).
-export([altname/1]).

-export([large_file/0, large_file/1, large_write/0, large_write/1]).

-export([read_line_1/1, read_line_2/1, read_line_3/1,read_line_4/1]).

-export([advise/1]).

-export([allocate/1]).

-export([allocate_file_size/1]).

-export([standard_io/1,mini_server/1]).

-export([old_io_protocol/1]).

-export([unicode_mode/1]).

-export([volume_relative_paths/1,unc_paths/1]).

-export([tiny_writes/1, tiny_writes_delayed/1,
         large_writes/1, large_writes_delayed/1,
         tiny_reads/1, tiny_reads_ahead/1]).

%% Debug exports
-export([create_file_slow/2, create_file/2, create_bin/2]).
-export([verify_file/2, verify_bin/3]).
-export([bytes/2, iterate/3]).


%% System probe functions that might be handy to check from the shell
-export([disc_free/1, memsize/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-include_lib("kernel/include/file.hrl").

-define(THROW_ERROR(RES), throw({fail, ?LINE, RES})).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [unicode, altname, read_write_file, {group, dirs},
     {group, files}, delete, rename, names, volume_relative_paths, unc_paths,
     {group, errors}, {group, compression}, {group, links}, copy,
     delayed_write, read_ahead, segment_read, segment_write,
     ipread, pid2name, interleaved_read_write, otp_5814, otp_10852,
     large_file, large_write, read_line_1, read_line_2, read_line_3,
     read_line_4, standard_io, old_io_protocol,
     unicode_mode, {group, bench}
    ].

groups() -> 
    [{dirs, [], [make_del_dir, cur_dir_0, cur_dir_1,
		 list_dir, list_dir_error, untranslatable_names,
		 untranslatable_names_error]},
     {files, [],
      [{group, open}, {group, pos}, {group, file_info},
       {group, consult}, {group, eval}, {group, script},
       truncate, sync, datasync, advise, allocate, allocate_file_size]},
     {open, [],
      [open1, old_modes, new_modes, path_open, close, access,
       read_write, pread_write, append, open_errors,
       exclusive]},
     {pos, [], [pos1, pos2, pos3]},
     {file_info, [],
      [file_info_basic_file, file_info_basic_directory,
       file_info_bad, file_info_times, file_write_file_info,
       file_wfi_helpers]},
     {consult, [], [consult1, path_consult]},
     {eval, [], [eval1, path_eval]},
     {script, [], [script1, path_script]},
     {errors, [],
      [e_delete, e_rename, e_make_dir, e_del_dir]},
     {compression, [],
      [read_compressed_cooked, read_compressed_cooked_binary,
       read_cooked_tar_problem, read_not_really_compressed,
       write_compressed, compress_errors, catenated_gzips,
       compress_async_crash]},
     {links, [],
      [make_link, read_link_info_for_non_link, symlinks]},
     {bench, [],
      [tiny_writes, tiny_writes_delayed,
       large_writes, large_writes_delayed,
       tiny_reads, tiny_reads_ahead]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(bench, Config) ->
    ScratchDir = proplists:get_value(priv_dir, Config),
    file:delete(filename:join(ScratchDir, "benchmark_scratch_file")),
    Config;
end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->
    SaslConfig = case application:start(sasl) of
		     {error, {already_started, sasl}} ->
			 [];
		     ok ->
			 [{sasl,started}]
		 end,
    application:start(os_mon),

    case os:type() of
	{win32, _} ->
	    Priv = proplists:get_value(priv_dir, Config),
	    HasAccessTime =
		case ?FILE_MODULE:read_file_info(Priv) of
		    {ok, #file_info{atime={_, {0, 0, 0}}}} ->
			%% This is a unfortunately a FAT file system.
			[no_access_time];
		    {ok, _} ->
			[]
		end,
	    ?FILE_INIT(HasAccessTime++Config++SaslConfig);
	_ ->
	    ?FILE_INIT(Config++SaslConfig)
    end.

end_per_suite(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    os:cmd("subst z: /d");
	_ ->
	    ok
    end,

    application:stop(os_mon),
    case proplists:get_value(sasl, Config) of
	started ->
	    application:stop(sasl);
	_Else ->
	    ok
    end,
    ?FILE_FINI(Config).

init_per_testcase(_Func, Config) ->
    %%error_logger:info_msg("~p:~p *****~n", [?MODULE, _Func]),
    ?FILE_INIT_PER_TESTCASE(Config).

end_per_testcase(_Func, Config) ->
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
	{io_request,From,To,{put_chars,_Encoding,Data}} ->
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

%% Test that standard i/o-servers work with file module.
standard_io(Config) when is_list(Config) ->
    %% Really just a smoke test
    Pid = spawn(?MODULE,mini_server,[self()]),
    register(mini_server,Pid),
    ok = file:write(mini_server,<<"hej\n">>),
    receive
	{io_request,_,_,{put_chars,<<"hej\n">>}} ->
	    ok
    after 1000 ->
	    exit(noreply)
    end,
    {ok,"aaaaa"} = file:read(mini_server,5),
    receive
	{io_request,_,_,{get_chars,'',5}} ->
	    ok
    after 1000 ->
	    exit(noreply)
    end,
    {ok,"hej\n"} = file:read_line(mini_server),
    receive
	{io_request,_,_,{get_line,''}} ->
	    ok
    after 1000 ->
	    exit(noreply)
    end,
    OldGL = group_leader(),
    group_leader(Pid,self()),
    ok = file:write(standard_io,<<"hej\n">>),
    group_leader(OldGL,self()),
    receive
	{io_request,_,_,{put_chars,<<"hej\n">>}} ->
	    ok
    after 1000 ->
	    exit(noreply)
    end,
    group_leader(Pid,self()),
    {ok,"aaaaa"} = file:read(standard_io,5),
    group_leader(OldGL,self()),
    receive
	{io_request,_,_,{get_chars,'',5}} ->
	    ok
    after 1000 ->
	    exit(noreply)
    end,
    group_leader(Pid,self()),
    {ok,"hej\n"} = file:read_line(standard_io),
    group_leader(OldGL,self()),
    receive
	{io_request,_,_,{get_line,''}} ->
	    ok
    after 1000 ->
	    exit(noreply)
    end,
    Pid ! die,
    receive after 1000 -> ok end.

%% Test that the old file IO protocol =< R16B still works.
old_io_protocol(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"old_io_protocol.fil"),
    MyData = "0123456789abcdefghijklmnopqrstuvxyz",
    ok = ?FILE_MODULE:write_file(Name, MyData),
    {ok, Fd} = ?FILE_MODULE:open(Name, write),
    Fd ! {file_request,self(),Fd,truncate},
    receive
	{file_reply,Fd,ok} -> ok
    end,
    ok = ?FILE_MODULE:close(Fd),
    {ok, <<>>} = ?FILE_MODULE:read_file(Name),
    [] = flush(),
    ok.

unicode_mode(Config) ->
    Dir = {dir, proplists:get_value(priv_dir,Config)},
    OptVariants = [[Dir],
		   [Dir, {encoding, utf8}],
		   [Dir, binary],
		   [Dir, binary, {encoding, utf8}]
		  ],
    ReadVariants = [{read, fun(Fd) -> um_read(Fd, fun(Fd1) -> file:read(Fd1, 1024) end) end},
		    {read_line, fun(Fd) -> um_read(Fd, fun(Fd1) -> file:read_line(Fd1) end) end}
		    %%{pread, fun(Fd) -> file:pread(Fd, 0, 1024) end},
		    %%{preadl, fun(Fd) -> file:pread(Fd, [{0, 1024}]) end},
		   ],

    _ = [read_write_0("ASCII: list:  Hello World", Read, Opt) ||
	    Opt <- OptVariants, Read <- ReadVariants],
    _ = [read_write_0("LATIN1: list: åäöÅÄÖ", Read, Opt) ||
	    Opt <- OptVariants, Read <- ReadVariants],
    _ = [read_write_0(<<"ASCII: bin: Hello World">>, Read, Opt) ||
	    Opt <- OptVariants, Read <- ReadVariants],
    _ = [read_write_0(<<"LATIN1: bin: åäöÅÄÖ">>, Read, Opt) ||
	    Opt <- OptVariants, Read <- ReadVariants],
    %% These will be double encoded if option is encoding utf-8
    _ = [read_write_0(<<"UTF8: bin: Ωß"/utf8>>, Read, Opt) ||
	    Opt <- OptVariants, Read <- ReadVariants],
    %% These should not work (with encoding set to utf-8)
    %%   according to file's documentation
    _ = [read_write_0("UTF8: list: Ωß", Read, Opt) ||
	    Opt <- OptVariants, Read <- ReadVariants],
    ok.

read_write_0(Str, {Func, ReadFun}, Options) ->
    try
	Res = read_write_1(Str, ReadFun, Options),
	io:format("~p: ~ts ~p '~p'~n", [Func, Str, tl(Options), Res]),
	ok
    catch {fail, Line, ReadBytes = [_|_]} ->
	    io:format("~p:~p: ~p ERROR: ~w vs~n             ~w~n  - ~p~n",
		      [?MODULE, Line, Func, Str, ReadBytes, Options]),
	    exit({error, ?LINE});
	  {fail, Line, ReadBytes} ->
	    io:format("~p:~p: ~p ERROR: ~ts vs~n             ~w~n  - ~p~n",
		      [?MODULE, Line, Func, Str, ReadBytes, Options]),
	    exit({error, ?LINE});
	  error:What:Stacktrace ->
	    io:format("~p:??: ~p ERROR: ~p from~n  ~w~n  ~p~n",
		      [?MODULE, Func, What, Str, Options]),

	    io:format("\t~p~n", [Stacktrace]),
	    exit({error, ?LINE})
    end.

read_write_1(Str0, ReadFun, [{dir,Dir}|Options]) ->
    File = um_filename(Str0, Dir, Options),
    Pre = "line 1\n", Post = "\nlast line\n",
    Str = case is_list(Str0) andalso lists:max(Str0) > 255 of
	      false ->  %% Normal case Use options
		  {ok, FdW} = file:open(File, [write|Options]),
		  IO = [Pre, Str0, Post],
		  ok = file:write(FdW, IO),
		  case is_binary(Str0) of
		      true -> iolist_to_binary(IO);
		      false -> lists:append(IO)
		  end;
	      true -> %% Test unicode lists
		  {ok, FdW} = file:open(File, [write]),
		  Utf8 = unicode:characters_to_binary([Pre, Str0, Post]),
		  file:write(FdW, Utf8),
		  {unicode, Utf8}
	  end,
    file:close(FdW),
    {ok, FdR} = file:open(File, [read|Options]),
    ReadRes = ReadFun(FdR),
    file:close(FdR),
    Res = um_check(Str, ReadRes, Options),
    file:delete(File),
    Res.


um_read(Fd, Fun) ->
    um_read(Fd, Fun, []).

um_read(Fd, Fun, Acc) ->
    case Fun(Fd) of
	eof ->
	    case is_binary(hd(Acc)) of
		true  -> {ok, iolist_to_binary(lists:reverse(Acc))};
		false -> {ok, lists:append(lists:reverse(Acc))}
	    end;
	{ok, Data} ->
	    um_read(Fd, Fun, [Data|Acc]);
	Error ->
	    Error
    end.


um_check(Str, {ok, Str}, _) -> ok;
um_check(Bin, {ok, Res}, _Options) when is_binary(Bin), is_list(Res) ->
    case list_to_binary(Res) of
	Bin -> ok;
	_ -> ?THROW_ERROR(Res)
    end;
um_check(Str, {ok, Res}, _Options) when is_list(Str), is_binary(Res) ->
    case iolist_to_binary(Str) of
	Res -> ok;
	_ -> ?THROW_ERROR(Res)
    end;
um_check({unicode, Utf8Bin}, Res, Options) ->
    um_check_unicode(Utf8Bin, Res,
		     proplists:get_value(binary, Options, false),
		     proplists:get_value(encoding, Options, none));
um_check(_Str, Res, _Options) ->
    ?THROW_ERROR(Res).

um_check_unicode(Utf8Bin, {ok, Utf8Bin}, true, none) ->
    ok;
um_check_unicode(Utf8Bin, {ok, List = [_|_]}, false, none) ->
    case binary_to_list(Utf8Bin) == List of
	true -> ok;
	false -> ?THROW_ERROR(List)
    end;
um_check_unicode(_Utf8Bin, {error, {no_translation, unicode, latin1}}, _, _) ->
    no_translation;
um_check_unicode(_Utf8Bin, Error = {error, _}, _, _Unicode) ->
    ?THROW_ERROR(Error);
um_check_unicode(_Utf8Bin, {ok, _ListOrBin}, _, _UTF8_) ->
    %% List = if is_binary(ListOrBin) -> unicode:characters_to_list(ListOrBin);
    %% 	      true -> ListOrBin
    %% 	   end,
    %% io:format("In: ~w~n", [binary_to_list(Utf8Bin)]),
    %% io:format("Ut: ~w~n", [List]),
    ?THROW_ERROR({shoud_be, no_translation}).

um_filename(Bin, Dir, Options) when is_binary(Bin) ->
    um_filename(binary_to_list(Bin), Dir, Options);
um_filename(Str = [_|_], Dir, Options) ->
    Name = hd(string:lexemes(Str, ":")),
    Enc = atom_to_list(proplists:get_value(encoding, Options, latin1)),
    File = case lists:member(binary, Options) of
	       true ->
		   "test_" ++ Name ++ "_bin_enc_" ++ Enc;
	       false ->
		   "test_" ++ Name ++ "_list_enc_" ++ Enc
	   end,
    filename:join(Dir, File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_write_file(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_read_write_file"),

    %% Try writing and reading back some term
    SomeTerm = {"This term",{will,be},[written,$t,$o],1,file,[]},
    Bin1 = term_to_binary(SomeTerm),
    ok = do_read_write_file(Name, Bin1),

    %% Try a "null" term
    NullTerm = [],
    Bin2 = term_to_binary(NullTerm),
    ok = do_read_write_file(Name, Bin2),

    %% Try reading a nonexistent file
    Name2 = filename:join(RootDir,
			  atom_to_list(?MODULE)
			  ++"_nonexistent_file"),
    {error, enoent} = ?FILE_MODULE:read_file(Name2),
    {error, enoent} = ?FILE_MODULE:read_file(""),
    {error, enoent} = ?FILE_MODULE:read_file(''),

    %% Try writing to a bad filename
    {error, enoent} = do_read_write_file("", Bin2),

    %% Try writing something else than a binary
    {error, badarg} = do_read_write_file(Name, {1,2,3}),
    {error, badarg} = do_read_write_file(Name, self()),

    %% Some non-term binaries
    ok = do_read_write_file(Name, []),

    %% Write some iolists
    ok = do_read_write_file(Name, [Bin1,[],[[Bin2]]]),
    ok = do_read_write_file(Name, ["string",<<"binary">>]),
    ok = do_read_write_file(Name, "pure string"),

    [] = flush(),
    ok.

do_read_write_file(Name, Data) ->
    case ?FILE_MODULE:write_file(Name, Data) of
	ok ->
	    BinData = iolist_to_binary(Data),
	    {ok,BinData} = ?FILE_MODULE:read_file(Name),

	    ok = ?FILE_MODULE:write_file(Name, Data, []),
	    {ok,BinData} = ?FILE_MODULE:read_file(Name),

	    ok = ?FILE_MODULE:write_file(Name, Data, [raw]),
	    {ok,BinData} = ?FILE_MODULE:read_file(Name),

	    ok;
	{error,_}=Res ->
	    Res = ?FILE_MODULE:write_file(Name, Data, []),
	    Res = ?FILE_MODULE:write_file(Name, Data, [raw]),
	    Res
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_del_dir(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_mk-dir"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    {error, eexist} = ?FILE_MODULE:make_dir(NewDir),
    ok = ?FILE_MODULE:del_dir(NewDir),
    {error, enoent} = ?FILE_MODULE:del_dir(NewDir),
    %% Make sure we are not in a directory directly under test_server
    %% as that would result in eacces errors when trying to delete '..',
    %% because there are processes having that directory as current.
    ok = ?FILE_MODULE:make_dir(NewDir),
    {ok,CurrentDir} = file:get_cwd(),
    case {os:type(), length(NewDir) >= 260 } of
	{{win32,_}, true} ->
	    io:format("Skip set_cwd for windows path longer than 260 (MAX_PATH)\n", []),
	    io:format("\nNewDir = ~p\n", [NewDir]);
    	_ ->
	    ok = ?FILE_MODULE:set_cwd(NewDir)
    end,
    try
	%% Check that we get an error when trying to create...
	%% a deep directory
	NewDir2 = filename:join(RootDir,
				atom_to_list(?MODULE)
				++"_mk-dir-noexist/foo"),
	{error, enoent} = ?FILE_MODULE:make_dir(NewDir2),
	%% a nameless directory
	{error, enoent} = ?FILE_MODULE:make_dir(""),
	%% a directory with illegal name
	{error, badarg} = ?FILE_MODULE:make_dir({1,2,3}),

	%% a directory with illegal name, even if it's a (bad) list
	{error, badarg} = ?FILE_MODULE:make_dir([1,2,3,{}]),

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
	    {error, eacces} -> ok;		%OpenBSD
	    {error, einval} -> ok			%FreeBSD
	end,
	{error, enoent} = ?FILE_MODULE:del_dir(""),
	{error, badarg} = ?FILE_MODULE:del_dir([3,2,1,{}]),

	[] = flush()
    after
	?FILE_MODULE:set_cwd(CurrentDir)
    end,
    ok.

cur_dir_0(Config) when is_list(Config) ->
    %% Find out the current dir, and cd to it ;-)
    {ok,BaseDir} = ?FILE_MODULE:get_cwd(),
    Dir1 = BaseDir ++ "", %% Check that it's a string
    ok = ?FILE_MODULE:set_cwd(Dir1),

    %% Make a new dir, and cd to that
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_curdir"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    case {os:type(), length(NewDir) >= 260} of
	{{win32,_}, true} ->
	    io:format("Skip set_cwd for windows path longer than 260 (MAX_PATH):\n"),
	    io:format("\nNewDir = ~p\n", [NewDir]);
	_ ->
	    io:format("cd to ~s",[NewDir]),
    	    ok = ?FILE_MODULE:set_cwd(NewDir),

	    %% Create a file in the new current directory, and check that it
	    %% really is created there
	    UncommonName = "uncommon.fil",
	    {ok,Fd} = ?FILE_MODULE:open(UncommonName,read_write),
	    ok = ?FILE_MODULE:close(Fd),
	    {ok,NewDirFiles} = ?FILE_MODULE:list_dir("."),
	    true = lists:member(UncommonName,NewDirFiles),

	    %% Ensure that we get the same result with a trailing slash; the
	    %% APIs used on Windows will choke on them if passed directly.
	    {ok,NewDirFiles} = ?FILE_MODULE:list_dir("./"),

	    %% Delete the directory and return to the old current directory
	    %% and check that the created file isn't there (too!)
	    expect({error, einval}, {error, eacces}, 
	    	   ?FILE_MODULE:del_dir(NewDir)),
	    ?FILE_MODULE:delete(UncommonName),
	    {ok,[]} = ?FILE_MODULE:list_dir("."),
	    ok = ?FILE_MODULE:set_cwd(Dir1),
	    io:format("cd back to ~s",[Dir1]),

	    ok = ?FILE_MODULE:del_dir(NewDir),
	    {error, enoent} = ?FILE_MODULE:set_cwd(NewDir),
	    ok = ?FILE_MODULE:set_cwd(Dir1),
	    io:format("cd back to ~s",[Dir1]),
	    {ok,OldDirFiles} = ?FILE_MODULE:list_dir("."),
	    false = lists:member(UncommonName,OldDirFiles)
    end,

    %% Try doing some bad things
    {error, badarg} = ?FILE_MODULE:set_cwd({foo,bar}),
    {error, enoent} = ?FILE_MODULE:set_cwd(""),
    {error, enoent} = ?FILE_MODULE:set_cwd(".......a......"),
    {ok,BaseDir} = ?FILE_MODULE:get_cwd(), %% Still there?

    %% On Windows, there should only be slashes, no backslashes,
    %% in the return value of get_cwd().
    %% (The test is harmless on Unix, because filenames usually
    %% don't contain backslashes.)

    {ok, BaseDir} = ?FILE_MODULE:get_cwd(),
    false = lists:member($\\, BaseDir),

    [] = flush(),
    ok.

%% Tests ?FILE_MODULE:get_cwd/1.

cur_dir_1(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    win_cur_dir_1(Config);
	_ ->
	    {error, enotsup} = ?FILE_MODULE:get_cwd("d:")
    end,
    [] = flush(),
    ok.

win_cur_dir_1(_Config) ->
    {ok,BaseDir} = ?FILE_MODULE:get_cwd(),

    %% Get the drive letter from the current directory,
    %% and try to get current directory for that drive.

    [CurDrive,$:|_] = BaseDir,
    {ok,BaseDir} = ?FILE_MODULE:get_cwd([CurDrive,$:]),
    io:format("BaseDir = ~s\n", [BaseDir]),

    %% We should error out on non-existent drives. Any reasonable system will
    %% have at least one.
    CurDirs = [?FILE_MODULE:get_cwd([Drive,$:]) || Drive <- lists:seq($A, $Z)],
    lists:member({error,eaccess}, CurDirs),

    %% Unfortunately, there is no way to move away from the
    %% current drive as we can't use the "subst" command from
    %% a SSH connection. We can't test any more.

    ok.


%%%
%%% Test list_dir() on a non-existing pathname.
%%%

list_dir_error(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    NonExisting = filename:join(Priv, "non-existing-dir"),
    {error,enoent} = ?FILE_MODULE:list_dir(NonExisting),
    ok.

%%%
%%% Test list_dir() and list_dir_all().
%%%

list_dir(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    TestDir = filename:join(RootDir, ?MODULE_STRING++"_list_dir"),
    ?FILE_MODULE:make_dir(TestDir),
    list_dir_1(TestDir, 42, []).

list_dir_1(TestDir, 0, Sorted) ->
    [ok = ?FILE_MODULE:delete(filename:join(TestDir, F)) ||
	F <- Sorted],
    ok = ?FILE_MODULE:del_dir(TestDir);
list_dir_1(TestDir, Cnt, Sorted0) ->
    Base = "file" ++ integer_to_list(Cnt),
    Name = filename:join(TestDir, Base),
    ok = ?FILE_MODULE:write_file(Name, Base),
    Sorted = lists:merge([Base], Sorted0),
    {ok,DirList0} = ?FILE_MODULE:list_dir(TestDir),
    {ok,DirList1} = ?FILE_MODULE:list_dir_all(TestDir),
    Sorted = lists:sort(DirList0),
    Sorted = lists:sort(DirList1),
    list_dir_1(TestDir, Cnt-1, Sorted).

untranslatable_names(Config) ->
    case no_untranslatable_names() of
	true ->
	    {skip,"Not a problem on this OS"};
	false ->
	    untranslatable_names_1(Config)
    end.

untranslatable_names_1(Config) ->
    {ok,OldCwd} = file:get_cwd(),
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "untranslatable_names"),
    ok = file:make_dir(Dir),
    Node = start_node(untranslatable_names, "+fnu"),
    try
	ok = file:set_cwd(Dir),
	[ok = file:write_file(F, F) || {_,F} <- untranslatable_names()],

	ExpectedListDir0 = [unicode:characters_to_list(N, utf8) ||
			       {utf8,N} <- untranslatable_names()],
	ExpectedListDir = lists:sort(ExpectedListDir0),
	io:format("ExpectedListDir: ~p\n", [ExpectedListDir]),
	ExpectedListDir = call_and_sort(Node, file, list_dir, [Dir]),

	ExpectedListDirAll0 = [case Enc of
				   utf8 ->
				       unicode:characters_to_list(N, utf8);
				   latin1 ->
				       N
			       end || {Enc,N} <- untranslatable_names()],
	ExpectedListDirAll = lists:sort(ExpectedListDirAll0),
	io:format("ExpectedListDirAll: ~p\n", [ExpectedListDirAll]),
	ExpectedListDirAll = call_and_sort(Node, file, list_dir_all, [Dir])
    after
	catch test_server:stop_node(Node),
	file:set_cwd(OldCwd),
	[file:delete(F) || {_,F} <- untranslatable_names()],
	file:del_dir(Dir)
    end,
    ok.

untranslatable_names_error(Config) ->
    case no_untranslatable_names() of
	true ->
	    {skip,"Not a problem on this OS"};
	false ->
	    untranslatable_names_error_1(Config)
    end.

untranslatable_names_error_1(Config) ->
    {ok,OldCwd} = file:get_cwd(),
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "untranslatable_names_error"),
    ok = file:make_dir(Dir),
    Node = start_node(untranslatable_names, "+fnue"),
    try
	ok = file:set_cwd(Dir),
	[ok = file:write_file(F, F) || {_,F} <- untranslatable_names()],

	ExpectedListDir0 = [unicode:characters_to_list(N, utf8) ||
			       {utf8,N} <- untranslatable_names()],
	ExpectedListDir = lists:sort(ExpectedListDir0),
	io:format("ExpectedListDir: ~p\n", [ExpectedListDir]),
	{error,{no_translation,BadFile}} =
	    rpc:call(Node, file, list_dir, [Dir]),
	true = lists:keymember(BadFile, 2, untranslatable_names())

    after
	catch test_server:stop_node(Node),
	file:set_cwd(OldCwd),
	[file:delete(F) || {_,F} <- untranslatable_names()],
	file:del_dir(Dir)
    end,
    ok.

untranslatable_names() ->
    [{utf8,<<"abc">>},
     {utf8,<<"def">>},
     {utf8,<<"Lagerl",195,182,"f">>},
     {utf8,<<195,150,"stra Emterwik">>},
     {latin1,<<"M",229,"rbacka">>},
     {latin1,<<"V",228,"rmland">>}].

call_and_sort(Node, M, F, A) ->
    {ok,Res} = rpc:call(Node, M, F, A),
    lists:sort(Res).

no_untranslatable_names() ->
    case os:type() of
	{unix,darwin} -> true;
	{win32,_} -> true;
	_ -> false
    end.

start_node(Name, Args) ->
    [_,Host] = string:lexemes(atom_to_list(node()), "@"),
    ct:log("Trying to start ~w@~s~n", [Name,Host]),
    case test_server:start_node(Name, peer, [{args,Args}]) of
	{error,Reason} ->
	    ct:fail(Reason);
	{ok,Node} ->
	    ct:log("Node ~p started~n", [Node]),
	    Node
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



open1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_files"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    Name = filename:join(NewDir, "foo1.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,read_write),
    {ok,Fd2} = ?FILE_MODULE:open(Name,read),
    Str = "{a,tuple}.\n",
    io:format(Fd1,Str,[]),
    {ok,0} = ?FILE_MODULE:position(Fd1,bof),
    Str = io:get_line(Fd1,''),
    Str = io:get_line(Fd2,''),
    ok = ?FILE_MODULE:close(Fd2),
    {ok,0} = ?FILE_MODULE:position(Fd1,bof),
    ok = ?FILE_MODULE:truncate(Fd1),
    eof = io:get_line(Fd1,''),
    ok = ?FILE_MODULE:close(Fd1),
    {ok,Fd3} = ?FILE_MODULE:open(Name,read),
    eof = io:get_line(Fd3,''),
    ok = ?FILE_MODULE:close(Fd3),
    [] = flush(),
    ok.

%% Tests all open modes.

old_modes(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_old_open_modes"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    Name1 = filename:join(NewDir, "foo1.fil"),
    Marker = "hello, world",

    %% write
    {ok, Fd1} = ?FILE_MODULE:open(Name1, write),
    ok = io:write(Fd1, Marker),
    ok = io:put_chars(Fd1, ".\n"),
    ok = ?FILE_MODULE:close(Fd1),

    %% read
    {ok, Fd2} = ?FILE_MODULE:open(Name1, read),
    {ok, Marker} = io:read(Fd2, prompt),
    ok = ?FILE_MODULE:close(Fd2),

    %% read_write
    {ok, Fd3} = ?FILE_MODULE:open(Name1, read_write),
    {ok, Marker} = io:read(Fd3, prompt),
    ok = io:write(Fd3, Marker),
    ok = ?FILE_MODULE:close(Fd3),

    [] = flush(),
    ok.


new_modes(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_new_open_modes"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    Name1 = filename:join(NewDir, "foo1.fil"),
    Marker = "hello, world",

    %% write
    {ok, Fd1} = ?FILE_MODULE:open(Name1, [write]),
    ok = io:write(Fd1, Marker),
    ok = io:put_chars(Fd1, ".\n"),
    ok = ?FILE_MODULE:close(Fd1),

    %% read
    {ok, Fd2} = ?FILE_MODULE:open(Name1, [read]),
    {ok, Marker} = io:read(Fd2, prompt),
    ok = ?FILE_MODULE:close(Fd2),

    %% read and write
    {ok, Fd3} = ?FILE_MODULE:open(Name1, [read, write]),
    {ok, Marker} = io:read(Fd3, prompt),
    ok = io:write(Fd3, Marker),
    ok = ?FILE_MODULE:close(Fd3),

    %% read by default
    {ok, Fd4} = ?FILE_MODULE:open(Name1, []),
    {ok, Marker} = io:read(Fd4, prompt),
    ok = ?FILE_MODULE:close(Fd4),

    %% read and binary
    {ok, Fd5} = ?FILE_MODULE:open(Name1, [read, binary]),
    {ok, Marker} = io:read(Fd5, prompt),
    ok = ?FILE_MODULE:close(Fd5),

    %% read, raw
    {ok, Fd6} = ?FILE_MODULE:open(Name1, [read, raw]),
    {ok, [$\[]} = ?FILE_MODULE:read(Fd6, 1),
     ok = ?FILE_MODULE:close(Fd6),

     %% write and sync
     case ?FILE_MODULE:open(Name1, [write, sync]) of
	 {ok, Fd7} ->
	     ok = io:write(Fd7, Marker),
	     ok = io:put_chars(Fd7, ".\n"),
	     ok = ?FILE_MODULE:close(Fd7),
	     {ok, Fd8} = ?FILE_MODULE:open(Name1, [read]),
	     {ok, Marker} = io:read(Fd8, prompt),
	     ok = ?FILE_MODULE:close(Fd8);
	 {error, enotsup} ->
	     %% for platforms that don't support the sync option
	     ok
     end,

     [] = flush(),
     ok.

path_open(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_path_open"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    FileName = "path_open.fil",
    Name = filename:join(RootDir, FileName),
    {ok,Fd1,_FullName1} =
	?FILE_MODULE:path_open(
	   [RootDir,
	    "nosuch1",
	    NewDir],FileName,write),
    io:format(Fd1,"ABCDEFGH",[]),
    ok = ?FILE_MODULE:close(Fd1),

    %% locate it in the last dir
    {ok,Fd2,_FullName2} =
	?FILE_MODULE:path_open(
	   ["nosuch1",
	    NewDir,
	    RootDir],FileName,read),
    {ok,2} =
	?FILE_MODULE:position(Fd2,2), "C" = io:get_chars(Fd2,'',1),
    ok = ?FILE_MODULE:close(Fd2),
    %% Try a failing path
    {error, enoent} = ?FILE_MODULE:path_open(
			 ["nosuch1",
			  NewDir],FileName,read),
    %% Check that it's found regardless of path, if an absolute name given
    {ok,Fd3,_FullPath3} =
	?FILE_MODULE:path_open(
	   ["nosuch1",
	    NewDir],Name,read),
    {ok,2} =
	?FILE_MODULE:position(Fd3,2), "C" = io:get_chars(Fd3,'',1),
    ok = ?FILE_MODULE:close(Fd3),

    [] = flush(),
    ok.

close(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_close.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,read_write),
    %% Just closing it is no fun, we did that a million times already
    %% This is a common error, for code written before Erlang 4.3
    %% bacause then ?FILE_MODULE:open just returned a Pid, and not everyone
    %% really checked what they got.
    {'EXIT',_Msg} = (catch ok = ?FILE_MODULE:close({ok,Fd1})),
    ok = ?FILE_MODULE:close(Fd1),

    %% Try closing one more time
    Val = ?FILE_MODULE:close(Fd1),
    io:format("Second close gave: ~p",[Val]),

    %% All operations on a closed raw file should EINVAL, even if they're not
    %% supported on the current platform.
    {ok,Fd2} = ?FILE_MODULE:open(Name, [read, write, raw]),
    ok = ?FILE_MODULE:close(Fd2),

    {error, einval} = ?FILE_MODULE:advise(Fd2, 5, 5, normal),
    {error, einval} = ?FILE_MODULE:allocate(Fd2, 5, 5),
    {error, einval} = ?FILE_MODULE:close(Fd2),
    {error, einval} = ?FILE_MODULE:datasync(Fd2),
    {error, einval} = ?FILE_MODULE:position(Fd2, 5),
    {error, einval} = ?FILE_MODULE:pread(Fd2, 5, 1),
    {error, einval} = ?FILE_MODULE:pwrite(Fd2, 5, "einval please"),
    {error, einval} = ?FILE_MODULE:read(Fd2, 1),
    {error, einval} = ?FILE_MODULE:sync(Fd2),
    {error, einval} = ?FILE_MODULE:truncate(Fd2),
    {error, einval} = ?FILE_MODULE:write(Fd2, "einval please"),

    [] = flush(),
    ok.

access(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_access.fil"),
    Str = "ABCDEFGH",
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:format(Fd1,Str,[]),
    ok = ?FILE_MODULE:close(Fd1),
    %% Check that we can't write when in read only mode
    {ok,Fd2} = ?FILE_MODULE:open(Name,read),
    case catch io:format(Fd2,"XXXX",[]) of
	ok ->
	    ct:fail({format,write});
	_ ->
	    ok
    end,
    ok = ?FILE_MODULE:close(Fd2),
    {ok,Fd3} = ?FILE_MODULE:open(Name,read),
    Str = io:get_line(Fd3,''),
    ok = ?FILE_MODULE:close(Fd3),

    [] = flush(),
    ok.

%% Tests ?FILE_MODULE:read/2 and ?FILE_MODULE:write/2.

read_write(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_read_write"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    Marker = "hello, world",
    MarkerB = list_to_binary(Marker),

    %% Plain file.
    Name1 = filename:join(NewDir, "plain.fil"),
    {ok, Fd1} = ?FILE_MODULE:open(Name1, [read, write]),
    read_write_test(Fd1, Marker, []),

    %% Raw file.
    Name2 = filename:join(NewDir, "raw.fil"),
    {ok, Fd2} = ?FILE_MODULE:open(Name2, [read, write, raw]),
    read_write_test(Fd2, Marker, []),

    %% Plain binary file.
    Name3 = filename:join(NewDir, "plain-b.fil"),
    {ok, Fd3} = ?FILE_MODULE:open(Name3, [read, write, binary]),
    read_write_test(Fd3, MarkerB, <<>>),

    %% Raw binary file.
    Name4 = filename:join(NewDir, "raw-b.fil"),
    {ok, Fd4} = ?FILE_MODULE:open(Name4, [read, write, raw, binary]),
    read_write_test(Fd4, MarkerB, <<>>),

    ok.

read_write_test(File, Marker, Empty) ->
    ok = ?FILE_MODULE:write(File, Marker),
    {ok, 0} = ?FILE_MODULE:position(File, 0),
    {ok, Empty} = ?FILE_MODULE:read(File, 0),
    {ok, Marker} = ?FILE_MODULE:read(File, 100),
    eof = ?FILE_MODULE:read(File, 100),
    {ok, Empty} = ?FILE_MODULE:read(File, 0),
    ok = ?FILE_MODULE:close(File),
    [] = flush(),
    ok.


%% Tests ?FILE_MODULE:pread/2 and ?FILE_MODULE:pwrite/2.

pread_write(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_pread_write"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    List = "hello, world",
    Bin = list_to_binary(List),

    %% Plain file.
    Name1 = filename:join(NewDir, "plain.fil"),
    {ok, Fd1} = ?FILE_MODULE:open(Name1, [read, write]),
    pread_write_test(Fd1, List),

    %% Raw file.
    Name2 = filename:join(NewDir, "raw.fil"),
    {ok, Fd2} = ?FILE_MODULE:open(Name2, [read, write, raw]),
    pread_write_test(Fd2, List),

    %% Plain file. Binary mode.
    Name3 = filename:join(NewDir, "plain-binary.fil"),
    {ok, Fd3} = ?FILE_MODULE:open(Name3, [binary, read, write]),
    pread_write_test(Fd3, Bin),

    %% Raw file. Binary mode.
    Name4 = filename:join(NewDir, "raw-binary.fil"),
    {ok, Fd4} = ?FILE_MODULE:open(Name4, [binary, read, write, raw]),
    pread_write_test(Fd4, Bin),

    ok.

pread_write_test(File, Data) ->
    io:format("~p:pread_write_test(~p,~p)~n", [?MODULE, File, Data]),
    Size = if is_binary(Data) -> byte_size(Data);
	      is_list(Data) -> length(Data)
	   end,
    I = Size + 17,
    ok = ?FILE_MODULE:pwrite(File, 0, Data),
    {ok, Data} = ?FILE_MODULE:pread(File, 0, I),
    {ok, [Data]} = ?FILE_MODULE:pread(File, [{0, I}]),
    eof = ?FILE_MODULE:pread(File, I, 1),
    ok = ?FILE_MODULE:pwrite(File, [{0, Data}, {I, Data}]),
    {ok, [Data, eof, Data]} =
	?FILE_MODULE:pread(File, [{0, Size}, {2*I, 1}, {I, Size}]),
    Plist = lists:seq(21*I, 0, -I),
    Pwrite = lists:map(fun(P)->{P,Data}end, Plist),
    Pread = [{22*I,Size} | lists:map(fun(P)->{P,Size}end, Plist)],
    Presult = [eof | lists:map(fun(_)->Data end, Plist)],
    ok = ?FILE_MODULE:pwrite(File, Pwrite),
    {ok, Presult} = ?FILE_MODULE:pread(File, Pread),
    ok = ?FILE_MODULE:close(File),
    [] = flush(),
    ok.

%% Test appending to a file.
append(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_append"),
    ok = ?FILE_MODULE:make_dir(NewDir),

    First = "First line\n",
    Second = "Seond lines comes here\n",
    Third = "And here is the third line\n",

    %% Write a small text file.
    Name1 = filename:join(NewDir, "a_file.txt"),
    {ok, Fd1} = ?FILE_MODULE:open(Name1, [write]),
    ok = io:format(Fd1, First, []),
    ok = io:format(Fd1, Second, []),
    ok = ?FILE_MODULE:close(Fd1),

    %% Open it a again and a append a line to it.
    {ok, Fd2} = ?FILE_MODULE:open(Name1, [append]),
    ok = io:format(Fd2, Third, []),
    ok = ?FILE_MODULE:close(Fd2),

    %% Read it back and verify.
    Expected = list_to_binary([First, Second, Third]),
    {ok, Expected} = ?FILE_MODULE:read_file(Name1),

    [] = flush(),
    ok.

open_errors(Config) when is_list(Config) ->
    DataDir =
	filename:dirname(
	  filename:join(proplists:get_value(data_dir, Config), "x")),
    DataDirSlash = DataDir++"/",
    {error, E1} = ?FILE_MODULE:open(DataDir, [read]),
    {error, E2} = ?FILE_MODULE:open(DataDirSlash, [read]),
    {error, E3} = ?FILE_MODULE:open(DataDir, [write]),
    {error, E4} = ?FILE_MODULE:open(DataDirSlash, [write]),
    {eisdir,eisdir,eisdir,eisdir} = {E1,E2,E3,E4},

    [] = flush(),
    ok.

%% Test exclusive access to a file.
exclusive(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_exclusive"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    Name = filename:join(NewDir, "ex_file.txt"),
    {ok, Fd} = ?FILE_MODULE:open(Name, [write, exclusive]),
    {error, eexist} = ?FILE_MODULE:open(Name, [write, exclusive]),
    ok = ?FILE_MODULE:close(Fd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pos1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_pos1.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:format(Fd1,"ABCDEFGH",[]),
    ok = ?FILE_MODULE:close(Fd1),
    {ok,Fd2} = ?FILE_MODULE:open(Name,read),

    %% Start pos is first char
    io:format("Relative positions"),
    "A" = io:get_chars(Fd2,'',1),
    {ok,2} = ?FILE_MODULE:position(Fd2,{cur,1}),
    "C" = io:get_chars(Fd2,'',1),
    {ok,0} = ?FILE_MODULE:position(Fd2,{cur,-3}),
    "A" = io:get_chars(Fd2,'',1),
    %% Backwards from first char should be an error
    {ok,0} = ?FILE_MODULE:position(Fd2,{cur,-1}),
    {error, einval} = ?FILE_MODULE:position(Fd2,{cur,-1}),
    %% Reset position and move again
    {ok,0} = ?FILE_MODULE:position(Fd2,0),
    {ok,2} = ?FILE_MODULE:position(Fd2,{cur,2}),
    "C" = io:get_chars(Fd2,'',1),
    %% Go a lot forwards
    {ok,13} = ?FILE_MODULE:position(Fd2,{cur,10}),
    eof = io:get_chars(Fd2,'',1),

    %% Try some fixed positions
    io:format("Fixed positions"),
    {ok,8} =
	?FILE_MODULE:position(Fd2,8), eof = io:get_chars(Fd2,'',1),
    {ok,8} =
	?FILE_MODULE:position(Fd2,cur), eof = io:get_chars(Fd2,'',1),
    {ok,7} =
	?FILE_MODULE:position(Fd2,7), "H" = io:get_chars(Fd2,'',1),
    {ok,0} =
	?FILE_MODULE:position(Fd2,0), "A" = io:get_chars(Fd2,'',1),
    {ok,3} =
	?FILE_MODULE:position(Fd2,3), "D" = io:get_chars(Fd2,'',1),
    {ok,12} =
	?FILE_MODULE:position(Fd2,12), eof = io:get_chars(Fd2,'',1),
    {ok,3} =
	?FILE_MODULE:position(Fd2,3), "D" = io:get_chars(Fd2,'',1),
    %% Try the {bof,X} notation
    {ok,3} = ?FILE_MODULE:position(Fd2,{bof,3}),
    "D" = io:get_chars(Fd2,'',1),

    %% Try eof positions
    io:format("EOF positions"),
    {ok,8} =
	?FILE_MODULE:position(Fd2,{eof,0}), eof=io:get_chars(Fd2,'',1),
    {ok,7} =
	?FILE_MODULE:position(Fd2,{eof,-1}),
    "H" = io:get_chars(Fd2,'',1),
    {ok,0} =
	?FILE_MODULE:position(Fd2,{eof,-8}), "A"=io:get_chars(Fd2,'',1),
    {error, einval} = ?FILE_MODULE:position(Fd2,{eof,-9}),

    [] = flush(),
    ok.

pos2(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_pos2.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:format(Fd1,"ABCDEFGH",[]),
    ok = ?FILE_MODULE:close(Fd1),
    {ok,Fd2} = ?FILE_MODULE:open(Name,read),
    {error, einval} = ?FILE_MODULE:position(Fd2,-1),

    %% Make sure that we still can search after an error.
    {ok,0} = ?FILE_MODULE:position(Fd2, 0),
    {ok,3} = ?FILE_MODULE:position(Fd2, {bof,3}),
    "D" = io:get_chars(Fd2,'',1),

    [] = flush(),
    io:format("DONE"),
    ok.

%% When it does not use raw mode, file:position had a bug.
pos3(Config) when is_list(Config) ->
    RootDir = proplists:get_value(data_dir, Config),
    Name = filename:join(RootDir, "realmen.html.gz"),

    {ok, Fd} = ?FILE_MODULE:open(Name, [read, binary]),
    {ok, _}  = ?FILE_MODULE:read(Fd, 5),
    {error, einval} = ?FILE_MODULE:position(Fd, {bof, -1}),

    %% Here ok had returned =(
    {error, einval} = ?FILE_MODULE:position(Fd, {cur, -10}),
    %% That test is actually questionable since file:position/2
    %% is documented to leave the file position undefined after
    %% it has returned an error.  But on Posix systems the position
    %% is guaranteed to be unchanged after an error return.  On e.g
    %% Windows there is nothing stated about this in the documentation.

    ok.

file_info_basic_file(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),

    %% Create a short file.
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_basic_test.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name, write),
    io:put_chars(Fd1, "foo bar"),
    ok = ?FILE_MODULE:close(Fd1),

    %% Don't crash the file server when passing incorrect arguments.
    {error,badarg} = ?FILE_MODULE:read_file_info(Name, [{time, gurka}]),
    {error,badarg} = ?FILE_MODULE:read_file_info([#{} | gaffel]),

    %% Test that the file has the expected attributes.
    %% The times are tricky, so we will save them to a separate test case.
    {ok,FileInfo} = ?FILE_MODULE:read_file_info(Name),
    {ok,FileInfo} = ?FILE_MODULE:read_file_info(Name, [raw]),
    #file_info{size=Size,type=Type,access=Access,
	       atime=AccessTime,mtime=ModifyTime} = FileInfo,
    io:format("Access ~p, Modify ~p", [AccessTime, ModifyTime]),
    Size = 7,
    Type = regular,
    read_write = Access,
    true = abs(time_dist(filter_atime(AccessTime, Config),
			 filter_atime(ModifyTime,
				      Config))) < 2,
    all_integers(tuple_to_list(AccessTime) ++ tuple_to_list(ModifyTime)),

    [] = flush(),
    ok.

file_info_basic_directory(Config) when is_list(Config) ->
    %% Note: filename:join/1 removes any trailing slash,
    %% which is essential for ?FILE_MODULE:file_info/1 to work on
    %% platforms such as Windows95.
    RootDir = filename:join([proplists:get_value(priv_dir, Config)]),

    %% Test that the RootDir directory has the expected attributes.
    test_directory(RootDir, read_write),

    %% Note that on Windows file systems, 
    %% "/" or "c:/" are *NOT* directories.
    %% Therefore, test that ?FILE_MODULE:file_info/1 behaves as if they were
    %% directories.
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
    {ok,FileInfo} = ?FILE_MODULE:read_file_info(Name),
    {ok,FileInfo} = ?FILE_MODULE:read_file_info(Name, [raw]),
    #file_info{size=Size,type=Type,access=Access,
               atime=AccessTime,mtime=ModifyTime} = FileInfo,
    io:format("Testing directory ~s", [Name]),
    io:format("Directory size is ~p", [Size]),
    io:format("Access ~p", [Access]),
    io:format("Access time ~p; Modify time~p",
	      [AccessTime, ModifyTime]),
    Type = directory,
    Access = ExpectedAccess,
    all_integers(tuple_to_list(AccessTime) ++ tuple_to_list(ModifyTime)),
    [] = flush(),
    ok.

all_integers([{A,B,C}|T]) ->
    all_integers([A,B,C|T]);
all_integers([Int|Rest]) when is_integer(Int) ->
    all_integers(Rest);
all_integers([]) -> ok.

%% Try something nonexistent.

file_info_bad(Config) when is_list(Config) ->
    RootDir = filename:join([proplists:get_value(priv_dir, Config)]),
    FileName = filename:join(RootDir, atom_to_list(?MODULE) ++ "_nonexistent"),
    {error,enoent} = ?FILE_MODULE:read_file_info(FileName),
    {error,enoent} = ?FILE_MODULE:read_file_info(FileName, [raw]),
    {error, enoent} = ?FILE_MODULE:read_file_info(""),
    {error, enoent} = ?FILE_MODULE:read_file_info("", [raw]),
    [] = flush(),
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
    %% which is essential for ?FILE_MODULE:file_info/1 to work on
    %% platforms such as Windows95.

    RootDir = filename:join([proplists:get_value(priv_dir, Config)]),
    io:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_file_info.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:put_chars(Fd1,"foo"),

    %% check that the file got a modify date max a few seconds away from now
    {ok,FileInfo1} = ?FILE_MODULE:read_file_info(Name),
    {ok,FileInfo1Raw} = ?FILE_MODULE:read_file_info(Name, [raw]),

    %% We assert that everything but the size is the same, on some OSs the
    %% size may not have been flushed to disc and we do not want to do a
    %% sync to force it.
    FileInfo1Raw = FileInfo1#file_info{ size = FileInfo1Raw#file_info.size },

    #file_info{type=regular,atime=AccTime1,mtime=ModTime1} = FileInfo1,

    Now = erlang:localtime(), %???
    io:format("Now ~p",[Now]),
    io:format("Open file Acc ~p Mod ~p",[AccTime1,ModTime1]),
    true = abs(time_dist(filter_atime(Now, Config),
			 filter_atime(AccTime1,
				      Config))) < 8,
    true = abs(time_dist(Now,ModTime1)) < 8,

    %% Sleep until we can be sure the seconds value has changed.
    %% Note: FAT-based filesystem (like on Windows 95) have
    %% a resolution of 2 seconds.
    timer:sleep(2200),

    %% close the file, and watch the modify date change
    ok = ?FILE_MODULE:close(Fd1),
    {ok,FileInfo2} = ?FILE_MODULE:read_file_info(Name),
    {ok,FileInfo2} = ?FILE_MODULE:read_file_info(Name, [raw]),
    #file_info{size=Size,type=regular,access=Access,
               atime=AccTime2,mtime=ModTime2} = FileInfo2,
    io:format("Closed file Acc ~p Mod ~p",[AccTime2,ModTime2]),
    true = time_dist(ModTime1,ModTime2) >= 0,

    %% this file is supposed to be binary, so it'd better keep it's size
    Size = 3,
    Access = read_write,

    %% Do some directory checking
    {ok,FileInfo3} = ?FILE_MODULE:read_file_info(RootDir),
    {ok,FileInfo3} = ?FILE_MODULE:read_file_info(RootDir, [raw]),
    #file_info{size=DSize,type=directory,access=DAccess,
               atime=AccTime3,mtime=ModTime3} = FileInfo3,
    %% this dir was modified only a few secs ago
    io:format("Dir Acc ~p; Mod ~p; Now ~p", [AccTime3, ModTime3, Now]),
    true = abs(time_dist(Now,ModTime3)) < 5,
    DAccess = read_write,
    io:format("Dir size is ~p",[DSize]),

    [] = flush(),
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

file_write_file_info(Config) when is_list(Config) ->
    RootDir = get_good_directory(Config),
    io:format("RootDir = ~p", [RootDir]),

    %% Set the file to read only AND update the file times at the same time.
    %% (This used to fail on Windows NT/95 for a local filesystem.)
    %% Note: Seconds must be even; see note in file_info_times/1.

    Name1 = filename:join(RootDir,
			  atom_to_list(?MODULE)
			  ++"_write_file_info_ro"),
    ok = ?FILE_MODULE:write_file(Name1, "hello"),
    Time = {{1997, 01, 02}, {12, 35, 42}},
    Info = #file_info{mode=8#400, atime=Time, mtime=Time, ctime=Time},
    ok = ?FILE_MODULE:write_file_info(Name1, Info),

    %% Read back the times.

    {ok, ActualInfo} = ?FILE_MODULE:read_file_info(Name1),
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
    {error, eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Make the file writable again.

    ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#600}),
    ok = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% And unwritable.
    ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#400}),
    {error, eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Same with raw.
    ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#600}, [raw]),
    ok = ?FILE_MODULE:write_file(Name1, "hello again"),
    ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#400}, [raw]),
    {error,eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Write the times again.
    %% Note: Seconds must be even; see note in file_info_times/1.

    NewTime = {{1997, 02, 15}, {13, 18, 20}},
    NewInfo = #file_info{atime=NewTime, mtime=NewTime, ctime=NewTime},
    ok = ?FILE_MODULE:write_file_info(Name1, NewInfo),
    {ok, ActualInfo2} = ?FILE_MODULE:read_file_info(Name1),
    #file_info{atime=NewActAtime, mtime=NewTime,
	       ctime=NewActCtime} = ActualInfo2,
    NewFilteredAtime = filter_atime(NewTime, Config),
    NewFilteredAtime = filter_atime(NewActAtime, Config),
    case os:type() of
	{win32, _} -> NewActCtime = NewTime;
	_ -> ok
    end,

    %% The file should still be unwritable.
    {error, eacces} = ?FILE_MODULE:write_file(Name1, "hello again"),

    %% Make the file writeable again, so that we can remove the
    %% test suites ... :-)
    ?FILE_MODULE:write_file_info(Name1, #file_info{mode=8#600}),

    [] = flush(),
    ok.

file_wfi_helpers(Config) when is_list(Config) ->
    RootDir = get_good_directory(Config),
    io:format("RootDir = ~p", [RootDir]),

    Name = filename:join(RootDir,
                         atom_to_list(?MODULE) ++ "_wfi_helpers"),

    ok = ?FILE_MODULE:write_file(Name, "hello again"),
    NewTime = {{1997, 02, 15}, {13, 18, 20}},
    ok = ?FILE_MODULE:change_time(Name, NewTime, NewTime),

    {ok, #file_info{atime=NewActAtime, mtime=NewTime}} =
        ?FILE_MODULE:read_file_info(Name),

    NewFilteredAtime = filter_atime(NewTime, Config),
    NewFilteredAtime = filter_atime(NewActAtime, Config),

    %% Make the file unwritable
    ok = ?FILE_MODULE:change_mode(Name, 8#400),
    {error, eacces} = ?FILE_MODULE:write_file(Name, "hello again"),

    %% ... and writable again
    ok = ?FILE_MODULE:change_mode(Name, 8#600),
    ok = ?FILE_MODULE:write_file(Name, "hello again"),

    %% We have no idea which users will work, so all we can do is to check
    %% that it returns enoent instead of crashing.
    {error, enoent} = ?FILE_MODULE:change_group("bogus file name", 0),
    {error, enoent} = ?FILE_MODULE:change_owner("bogus file name", 0),

    [] = flush(),
    ok.

%% Returns a directory on a file system that has correct file times.

get_good_directory(Config) ->
    proplists:get_value(priv_dir, Config).


consult1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_consult.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    io:format(Fd1,
	      "{this,[is,1.0],'journey'}.\n\"into\". (sound). ",
	      []),
    ok = ?FILE_MODULE:close(Fd1),
    {ok,[{this,[is,1.0],journey},"into",sound]} =
	?FILE_MODULE:consult(Name),

    {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    %% note the missing double quote
    io:format(
      Fd2,"{this,[is,1.0],'journey'}.\n \"into. (sound). ",[]),
    ok = ?FILE_MODULE:close(Fd2),
    {error, {_, _, _} = Msg} = ?FILE_MODULE:consult(Name),
    io:format("Errmsg: ~p",[Msg]),

    {error, enoent} = ?FILE_MODULE:consult(Name ++ ".nonexistent"),

    [] = flush(),
    ok.

path_consult(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    FileName = atom_to_list(?MODULE)++"_path_consult.fil",
    Name = filename:join(RootDir, FileName),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:format(Fd1,"{this,is,a,journey,into,sound}.\n",[]),
    ok = ?FILE_MODULE:close(Fd1),
    %% File last in path
    {ok,[{this,is,a,journey,into,sound}],Dir} =
	?FILE_MODULE:path_consult(
	   [filename:join(RootDir, "dir1"),
	    filename:join(RootDir, ".."),
	    filename:join(RootDir, "dir2"),
	    RootDir], FileName),
    true = lists:prefix(RootDir,Dir),

    %% While maybe not an error, it may be worth noting that
    %% when the full path to a file is given, it's always found
    %% regardless of the contents of the path
    {ok,_,_} = ?FILE_MODULE:path_consult(["nosuch1","nosuch2"],Name),

    [] = flush(),
    ok.


eval1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_eval.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    io:format(Fd1,"put(evaluated_ok,\ntrue). ",[]),
    ok = ?FILE_MODULE:close(Fd1),
    ok = ?FILE_MODULE:eval(Name),
    true = get(evaluated_ok),

    {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    io:format(Fd2,"put(evaluated_ok,\nR). ",[]),
    ok = ?FILE_MODULE:close(Fd2),
    ok = ?FILE_MODULE:eval(
	    Name,
	    erl_eval:add_binding('R', true, erl_eval:new_bindings())),
    true = get(evaluated_ok),

    {ok,Fd3} = ?FILE_MODULE:open(Name,write),
    %% garbled
    io:format(Fd3,"puGARBLED-GARBLED\ntrue). ",[]),
    ok = ?FILE_MODULE:close(Fd3),
    {error, {_, _, _} = Msg} = ?FILE_MODULE:eval(Name),
    io:format("Errmsg1: ~p",[Msg]),

    {error, enoent} = ?FILE_MODULE:eval(Name ++ ".nonexistent"),

    [] = flush(),
    ok.

path_eval(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    FileName = atom_to_list(?MODULE)++"_path_eval.fil",
    Name = filename:join(RootDir, FileName),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:format(Fd1,"put(evaluated_ok,true).\n",[]),
    ok = ?FILE_MODULE:close(Fd1),
    %% File last in path
    {ok,Dir} =
	?FILE_MODULE:path_eval(
	   [filename:join(RootDir, "dir1"),
	    filename:join(RootDir, ".."),
	    filename:join(RootDir, "dir2"),
	    RootDir],FileName),
    true = get(evaluated_ok),
    true = lists:prefix(RootDir,Dir),

    %% While maybe not an error, it may be worth noting that
    %% when the full path to a file is given, it's always found
    %% regardless of the contents of the path
    {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    io:format(Fd2,"put(evaluated_ok,R).\n",[]),
    ok = ?FILE_MODULE:close(Fd2),
    {ok,_} = ?FILE_MODULE:path_eval(
		["nosuch1","nosuch2"],
		Name,
		erl_eval:add_binding('R', true, erl_eval:new_bindings())),

    [] = flush(),
    ok.


script1(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_script.fil"),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    io:format(Fd1,"A = 11,\nB = 6,\nA+B. ",[]),
    ok = ?FILE_MODULE:close(Fd1),
    {ok,17} = ?FILE_MODULE:script(Name),

    {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    %% note that there is no final \n (only a space)
    io:format(Fd2,"A = 11,\nA+B. ",[]),
    ok = ?FILE_MODULE:close(Fd2),
    {ok,17} = ?FILE_MODULE:script(
		 Name,
		 erl_eval:add_binding('B', 6, erl_eval:new_bindings())),

    {ok,Fd3} = ?FILE_MODULE:open(Name,write),
    io:format(Fd3,"A = 11,\nB = six,\nA+B. ",[]),
    ok = ?FILE_MODULE:close(Fd3),
    {error, {_, _, _} = Msg} = ?FILE_MODULE:script(Name),
    io:format("Errmsg1: ~p",[Msg]),

    {error, enoent} = ?FILE_MODULE:script(Name ++ ".nonexistent"),

    [] = flush(),
    ok.

path_script(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    FileName = atom_to_list(?MODULE)++"_path_script.fil",
    Name = filename:join(RootDir, FileName),
    {ok,Fd1} = ?FILE_MODULE:open(Name,write),
    io:format(Fd1,"A = 11,\nB = 6,\nA+B.\n",[]),
    ok = ?FILE_MODULE:close(Fd1),
    %% File last in path
    {ok, 17, Dir} =
	?FILE_MODULE:path_script(
	   [filename:join(RootDir, "dir1"),
	    filename:join(RootDir, ".."),
	    filename:join(RootDir, "dir2"),
	    RootDir],FileName),
    true = lists:prefix(RootDir,Dir),

    %% While maybe not an error, it may be worth noting that
    %% when the full path to a file is given, it's always found
    %% regardless of the contents of the path
    {ok,Fd2} = ?FILE_MODULE:open(Name,write),
    io:format(Fd2,"A = 11,\nA+B.",[]),
    ok = ?FILE_MODULE:close(Fd2),
    {ok, 17, Dir} =
	?FILE_MODULE:path_script(
	   ["nosuch1","nosuch2"],
	   Name,
	   erl_eval:add_binding('B', 6, erl_eval:new_bindings())),

    [] = flush(),
    ok.



truncate(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_truncate.fil"),

    %% Create a file with some data.
    MyData = "0123456789abcdefghijklmnopqrstuvxyz",
    ok = ?FILE_MODULE:write_file(Name, MyData),

    %% Truncate the file to 10 characters.
    {ok, Fd} = ?FILE_MODULE:open(Name, read_write),
    {ok, 10} = ?FILE_MODULE:position(Fd, 10),
    ok = ?FILE_MODULE:truncate(Fd),
    ok = ?FILE_MODULE:close(Fd),

    %% Read back the file and check that it has been truncated.
    Expected = list_to_binary("0123456789"),
    {ok, Expected} = ?FILE_MODULE:read_file(Name),

    %% Open the file read only and verify that it is not possible to
    %% truncate it, OTP-1960
    {ok, Fd2} = ?FILE_MODULE:open(Name, read),
    {ok, 5} = ?FILE_MODULE:position(Fd2, 5),
    {error, _} = ?FILE_MODULE:truncate(Fd2),

    [] = flush(),
    ok.


%% Tests that ?FILE_MODULE:datasync/1 at least doesn't crash.
datasync(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Sync = filename:join(PrivDir,
			 atom_to_list(?MODULE)
			 ++"_sync.fil"),

    %% Raw open.
    {ok, Fd} = ?FILE_MODULE:open(Sync, [write, raw]),
    ok = ?FILE_MODULE:datasync(Fd),
    ok = ?FILE_MODULE:close(Fd),

    %% Ordinary open.
    {ok, Fd2} = ?FILE_MODULE:open(Sync, [write]),
    ok = ?FILE_MODULE:datasync(Fd2),
    ok = ?FILE_MODULE:close(Fd2),

    [] = flush(),
    ok.


%% Tests that ?FILE_MODULE:sync/1 at least doesn't crash.
sync(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Sync = filename:join(PrivDir,
			 atom_to_list(?MODULE)
			 ++"_sync.fil"),

    %% Raw open.
    {ok, Fd} = ?FILE_MODULE:open(Sync, [write, raw]),
    ok = ?FILE_MODULE:sync(Fd),
    ok = ?FILE_MODULE:close(Fd),

    %% Ordinary open.
    {ok, Fd2} = ?FILE_MODULE:open(Sync, [write]),
    ok = ?FILE_MODULE:sync(Fd2),
    ok = ?FILE_MODULE:close(Fd2),

    [] = flush(),
    ok.

%% Tests that ?FILE_MODULE:advise/4 at least doesn't crash.
advise(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Advise = filename:join(PrivDir,
			   atom_to_list(?MODULE)
			   ++"_advise.fil"),

    Line1 = "Hello\n",
    Line2 = "World!\n",

    {ok, Fd} = ?FILE_MODULE:open(Advise, [write]),
    ok = ?FILE_MODULE:advise(Fd, 0, 0, normal),
    ok = io:format(Fd, "~s", [Line1]),
    ok = io:format(Fd, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd),

    {ok, Fd2} = ?FILE_MODULE:open(Advise, [write]),
    ok = ?FILE_MODULE:advise(Fd2, 0, 0, random),
    ok = io:format(Fd2, "~s", [Line1]),
    ok = io:format(Fd2, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd2),

    {ok, Fd3} = ?FILE_MODULE:open(Advise, [write]),
    ok = ?FILE_MODULE:advise(Fd3, 0, 0, sequential),
    ok = io:format(Fd3, "~s", [Line1]),
    ok = io:format(Fd3, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd3),

    {ok, Fd4} = ?FILE_MODULE:open(Advise, [write]),
    ok = ?FILE_MODULE:advise(Fd4, 0, 0, will_need),
    ok = io:format(Fd4, "~s", [Line1]),
    ok = io:format(Fd4, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd4),

    {ok, Fd5} = ?FILE_MODULE:open(Advise, [write]),
    ok = ?FILE_MODULE:advise(Fd5, 0, 0, dont_need),
    ok = io:format(Fd5, "~s", [Line1]),
    ok = io:format(Fd5, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd5),

    {ok, Fd6} = ?FILE_MODULE:open(Advise, [write]),
    ok = ?FILE_MODULE:advise(Fd6, 0, 0, no_reuse),
    ok = io:format(Fd6, "~s", [Line1]),
    ok = io:format(Fd6, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd6),

    {ok, Fd7} = ?FILE_MODULE:open(Advise, [write]),
    {error, einval} = ?FILE_MODULE:advise(Fd7, 0, 0, bad_advise),
    ok = ?FILE_MODULE:close(Fd7),

    %% test write without advise, then a read after an advise
    {ok, Fd8} = ?FILE_MODULE:open(Advise, [write]),
    ok = io:format(Fd8, "~s", [Line1]),
    ok = io:format(Fd8, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd8),
    {ok, Fd9} = ?FILE_MODULE:open(Advise, [read]),
    Offset = 0,
    %% same as a 0 length in some implementations
    Length = length(Line1) + length(Line2),
    ok = ?FILE_MODULE:advise(Fd9, Offset, Length, sequential),
    {ok, Line1} = ?FILE_MODULE:read_line(Fd9),
    {ok, Line2} = ?FILE_MODULE:read_line(Fd9),
    eof = ?FILE_MODULE:read_line(Fd9),
    ok = ?FILE_MODULE:close(Fd9),

    [] = flush(),
    ok.

%% Tests that ?FILE_MODULE:allocate/3 at least doesn't crash.
allocate(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Allocate = filename:join(PrivDir,
			     atom_to_list(?MODULE)
			     ++"_allocate.fil"),

    Line1 = "Hello\n",
    Line2 = "World!\n",

    {ok, Fd} = ?FILE_MODULE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd, 1, iolist_size([Line1, Line2])),
    ok = io:format(Fd, "~s", [Line1]),
    ok = io:format(Fd, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd),

    {ok, Fd2} = ?FILE_MODULE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd2, 1, iolist_size(Line1)),
    ok = io:format(Fd2, "~s", [Line1]),
    ok = io:format(Fd2, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd2),

    {ok, Fd3} = ?FILE_MODULE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd3, 1, iolist_size(Line1) + 1),
    ok = io:format(Fd3, "~s", [Line1]),
    ok = io:format(Fd3, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd3),

    {ok, Fd4} = ?FILE_MODULE:open(Allocate, [write, binary]),
    allocate_and_assert(Fd4, 1, 4 * iolist_size([Line1, Line2])),
    ok = io:format(Fd4, "~s", [Line1]),
    ok = io:format(Fd4, "~s", [Line2]),
    ok = ?FILE_MODULE:close(Fd4),

    [] = flush(),
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
    %% least with same semantics as posix_fallocate(), fallocate() and
    %% fcntl F_PREALLOCATE.
    Result = ?FILE_MODULE:allocate(Fd, Offset, Length),
    case os:type() of
        {win32, _} ->
            {error, enotsup} = Result;
        _ ->
            _ = Result
    end.

%% Tests that asserts that file:allocate/3 changes file size
allocate_file_size(Config) when is_list(Config) ->
    case os:type() of
        {unix, darwin} ->
            PrivDir = proplists:get_value(priv_dir, Config),
            Allocate = filename:join(PrivDir, atom_to_list(?MODULE)++"_allocate_file"),

            {ok, Fd} = ?FILE_MODULE:open(Allocate, [write]),
            ok = ?FILE_MODULE:allocate(Fd, 0, 1024),
            {ok, 1024} = ?FILE_MODULE:position(Fd, eof),
            ok = ?FILE_MODULE:close(Fd),

            [] = flush(),
            ok;
        {unix, linux} ->
            {skip, "file:allocate/3 on Linux does not change file size"};
        {win32, _} ->
            {skip, "Windows does not support file:allocate/3"};
        _ ->
            {skip, "Support for allocate/3 is spotty in our test platform at the moment."}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    Name = filename:join(RootDir,
			 atom_to_list(?MODULE)
			 ++"_delete.fil"),
    {ok, Fd1} = ?FILE_MODULE:open(Name, write),
    io:format(Fd1,"ok.\n",[]),
    ok = ?FILE_MODULE:close(Fd1),
    %% Check that the file is readable
    {ok, Fd2} = ?FILE_MODULE:open(Name, read),
    ok = ?FILE_MODULE:close(Fd2),
    ok = ?FILE_MODULE:delete(Name),
    %% Check that the file is not readable anymore
    {error, _} = ?FILE_MODULE:open(Name, read),
    %% Try deleting a nonexistent file
    {error, enoent} = ?FILE_MODULE:delete(Name),
    [] = flush(),
    ok.

rename(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    FileName1 = atom_to_list(?MODULE)++"_rename.fil",
    FileName2 = atom_to_list(?MODULE)++"_rename.ful",
    Name1 = filename:join(RootDir, FileName1),
    Name2 = filename:join(RootDir, FileName2),
    {ok,Fd1} = ?FILE_MODULE:open(Name1,write),
    ok = ?FILE_MODULE:close(Fd1),
    %% Rename, and check that id really changed name
    ok = ?FILE_MODULE:rename(Name1,Name2),
    {error, _} = ?FILE_MODULE:open(Name1,read),
    {ok,Fd2} = ?FILE_MODULE:open(Name2,read),
    ok = ?FILE_MODULE:close(Fd2),
    %% Try renaming something to itself
    ok = ?FILE_MODULE:rename(Name2,Name2),
    %% Try renaming something that doesn't exist
    {error, enoent} = ?FILE_MODULE:rename(Name1,Name2),
    %% Try renaming to something else than a string
    {error, badarg} = ?FILE_MODULE:rename(Name1,{foo,bar}),

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
    ok = ?FILE_MODULE:make_dir(DirName1),
    %% The name has to include the full file name, path in not enough
    expect({error, eisdir}, {error, eexist},
	   ?FILE_MODULE:rename(Name2,DirName1)),
    ok = ?FILE_MODULE:rename(Name2, Name1foo),
    %% Now rename the directory
    ok = ?FILE_MODULE:rename(DirName1,DirName2),
    %% And check that the file is there now
    {ok,Fd3} = ?FILE_MODULE:open(Name2foo, read),
    ok = ?FILE_MODULE:close(Fd3),
    %% Try some dirty things now: move the directory into itself
    {error, Msg1} = ?FILE_MODULE:rename(DirName2, Name2bar),
    io:format("Errmsg1: ~p",[Msg1]),
    %% move dir into a file in itself
    {error, Msg2} = ?FILE_MODULE:rename(DirName2, Name2foo),
    io:format("Errmsg2: ~p",[Msg2]),

    [] = flush(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

names(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    FileName = "foo1.fil",
    Name1 = filename:join(RootDir, FileName),
    Name2 = [RootDir,"/","foo1",".","fil"],
    Name3 = [RootDir,"/",foo,$1,[[[],[],'.']],"f",il],
    {ok,Fd0} = ?FILE_MODULE:open(Name1,write),
    ok = ?FILE_MODULE:close(Fd0),

    %% Try some file names
    {ok,Fd1} = ?FILE_MODULE:open(Name1,read),
    ok = ?FILE_MODULE:close(Fd1),
    {ok,Fd2f} = ?FILE_MODULE:open(lists:flatten(Name2),read),
    ok = ?FILE_MODULE:close(Fd2f),
    {ok,Fd2} = ?FILE_MODULE:open(Name2,read),
    ok = ?FILE_MODULE:close(Fd2),
    {ok,Fd3} = ?FILE_MODULE:open(Name3,read),
    ok = ?FILE_MODULE:close(Fd3),

    %% Now try the same on raw files.
    {ok,Fd4} = ?FILE_MODULE:open(Name2, [read, raw]),
    ok = ?FILE_MODULE:close(Fd4),
    {ok,Fd4f} = ?FILE_MODULE:open(lists:flatten(Name2), [read, raw]),
    ok = ?FILE_MODULE:close(Fd4f),
    {ok,Fd5} = ?FILE_MODULE:open(Name3, [read, raw]),
    ok = ?FILE_MODULE:close(Fd5),

    case length(Name1) > 255 of
	true ->
	    io:format("Path too long for an atom:\n\n~p\n", [Name1]);
	false ->
	    Name4 = list_to_atom(Name1),
	    {ok,Fd6} = ?FILE_MODULE:open(Name4,read),
	    ok = ?FILE_MODULE:close(Fd6)
    end,

    %% Try some path names
    Path1 = RootDir,
    Path2 = [RootDir],
    Path3 = ['',[],[RootDir,[[]]]],
    {ok,Fd11,_} = ?FILE_MODULE:path_open([Path1],FileName,read),
    ok = ?FILE_MODULE:close(Fd11),
    {ok,Fd12,_} = ?FILE_MODULE:path_open([Path2],FileName,read),
    ok = ?FILE_MODULE:close(Fd12),
    {ok,Fd13,_} = ?FILE_MODULE:path_open([Path3],FileName,read),
    ok = ?FILE_MODULE:close(Fd13),
    case length(Path1) > 255 of
	true->
	    io:format("Path too long for an atom:\n\n~p\n", [Path1]);
	false ->
	    Path4 = list_to_atom(Path1),
	    {ok,Fd14,_} = ?FILE_MODULE:path_open([Path4],FileName,read),
	    ok = ?FILE_MODULE:close(Fd14)
    end,
    [] = flush(),
    ok.

volume_relative_paths(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} ->
            {ok, [Drive, $: | _]} = file:get_cwd(),
            %% Relative to current device root.
            {ok, RootInfo} = file:read_file_info([Drive, $:, $/]),
            {ok, RootInfo} = file:read_file_info("/"),
            %% Relative to current device directory.
            {ok, DirContents} = file:list_dir([Drive, $:]),
            {ok, DirContents} = file:list_dir("."),
            [] = flush(),
            ok;
        _ ->
            {skip, "This test is Windows-specific."}
    end.

unc_paths(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} ->
            %% We assume administrative shares are set up and reachable, and we
            %% settle for testing presence as some of the returned data is
            %% different.
            {ok, _} = file:read_file_info("C:\\Windows\\explorer.exe"),
            {ok, _} = file:read_file_info("\\\\localhost\\c$\\Windows\\explorer.exe"),

            {ok, Files} = file:list_dir("C:\\Windows\\"),
            {ok, Files} = file:list_dir("\\\\localhost\\c$\\Windows\\"),

            {ok, Cwd} = file:get_cwd(),

            try
                ok = file:set_cwd("\\\\localhost\\c$\\Windows\\"),
                {ok, _} = file:read_file_info("explorer.exe")
            after
                file:set_cwd(Cwd)
            end,

            [] = flush(),
            ok;
        _ ->
            {skip, "This test is Windows-specific."}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


e_delete(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_e_delete"),
    ok = ?FILE_MODULE:make_dir(Base),

    %% Delete a non-existing file.
    {error, enoent} =
	?FILE_MODULE:delete(filename:join(Base, "non_existing")),

    %% Delete a directory.
    {error, eperm} = ?FILE_MODULE:delete(Base),

    %% Use a path-name with a non-directory component.
    Afile = filename:join(Base, "a_file"),
    ok = ?FILE_MODULE:write_file(Afile, "hello\n"),
    {error, E} =
	expect({error, enotdir}, {error, enoent}, 
	       ?FILE_MODULE:delete(filename:join(Afile, "another_file"))),
    io:format("Result: ~p~n", [E]),

    %% No permission.
    case os:type() of
	{win32, _} ->
	    %% Remove a character device.
	    expect({error, eacces}, {error, einval},
                   ?FILE_MODULE:delete("nul"));
	_ ->
	    ?FILE_MODULE:write_file_info(
	       Base, #file_info {mode=0}),
	    {error, eacces} = ?FILE_MODULE:delete(Afile),
	    ?FILE_MODULE:write_file_info(
	       Base, #file_info {mode=8#700})
    end,

    [] = flush(),
    ok.

%%% FreeBSD gives EEXIST when renaming a file to an empty dir, although the
%%% manual page can be interpreted as saying that EISDIR should be given.
%%% (What about FreeBSD? We store our nightly build results on a FreeBSD
%%% file system, that's what.)

e_rename(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_e_rename"),
    ok = ?FILE_MODULE:make_dir(Base),

    %% Create an empty directory.
    EmptyDir = filename:join(Base, "empty_dir"),
    ok = ?FILE_MODULE:make_dir(EmptyDir),

    %% Create a non-empty directory.
    NonEmptyDir = filename:join(Base, "non_empty_dir"),
    ok = ?FILE_MODULE:make_dir(NonEmptyDir),
    ok = ?FILE_MODULE:write_file(
	    filename:join(NonEmptyDir, "a_file"),
	    "hello\n"),

    %% Create another non-empty directory.
    ADirectory = filename:join(Base, "a_directory"),
    ok = ?FILE_MODULE:make_dir(ADirectory),
    ok = ?FILE_MODULE:write_file(
	    filename:join(ADirectory, "a_file"),
	    "howdy\n\n"),

    %% Create a data file.
    File = filename:join(Base, "just_a_file"),
    ok = ?FILE_MODULE:write_file(File, "anything goes\n\n"),

    %% Move an existing directory to a non-empty directory.
    {error, eexist} = ?FILE_MODULE:rename(ADirectory, NonEmptyDir),

    %% Move a root directory.
    {error, einval} = ?FILE_MODULE:rename("/", "arne"),

    %% Move Base into Base/new_name.
    {error, einval} = 
	?FILE_MODULE:rename(Base, filename:join(Base, "new_name")),

    %% Overwrite a directory with a file.
    expect({error, eexist}, %FreeBSD (?)
	   {error, eisdir},
	   ?FILE_MODULE:rename(File, EmptyDir)),
    expect({error, eexist}, %FreeBSD (?)
	   {error, eisdir},
	   ?FILE_MODULE:rename(File, NonEmptyDir)),

    %% Move a non-existing file.
    NonExistingFile = filename:join(Base, "non_existing_file"),
    {error, enoent} = ?FILE_MODULE:rename(NonExistingFile, NonEmptyDir),

    %% Overwrite a file with a directory.
    expect({error, eexist}, %FreeBSD (?)
	   {error, enotdir},
	   ?FILE_MODULE:rename(ADirectory, File)),

    %% Move a file to another filesystem.
    %% XXX - This test case is bogus. We cannot be guaranteed that
    %%       the source and destination are on 
    %%       different filesystems.
    %%
    %% XXX - Gross hack!
    Comment = case os:type() of
		  {unix, _} ->
		      OtherFs = "/tmp",
		      NameOnOtherFs = filename:join(OtherFs, filename:basename(File)),
		      {ok, Com} = case ?FILE_MODULE:rename(File, NameOnOtherFs) of
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
    [] = flush(),
    Comment.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e_make_dir(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = filename:join(RootDir, 
			 atom_to_list(?MODULE)++"_e_make_dir"),
    ok = ?FILE_MODULE:make_dir(Base),

    %% A component of the path does not exist.
    {error, enoent} = ?FILE_MODULE:make_dir(filename:join([Base, "a", "b"])),

    %% Use a path-name with a non-directory component.
    Afile = filename:join(Base, "a_directory"),
    ok = ?FILE_MODULE:write_file(Afile, "hello\n"),
    case ?FILE_MODULE:make_dir(
            filename:join(Afile, "another_directory")) of
        {error, enotdir} -> io:format("Result: enotdir");
        {error, enoent} -> io:format("Result: enoent")
    end,

    %% No permission (on Unix only).
    case os:type() of
	{win32, _} ->
	    ok;
	_ ->
	    ?FILE_MODULE:write_file_info(Base, #file_info {mode=0}),
	    {error, eacces} = ?FILE_MODULE:make_dir(filename:join(Base, "xxxx")),
	    ?FILE_MODULE:write_file_info(
	       Base, #file_info {mode=8#700})
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e_del_dir(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = test_server:temp_name(filename:join(RootDir, "e_del_dir")),
    io:format("Base: ~p", [Base]),
    ok = ?FILE_MODULE:make_dir(Base),

    %% Delete a non-existent directory.
    {error, enoent} = 
	?FILE_MODULE:del_dir(filename:join(Base, "non_existing")),

    %% Use a path-name with a non-directory component.
    Afile = filename:join(Base, "a_directory"),
    ok = ?FILE_MODULE:write_file(Afile, "hello\n"),
    {error, E1} = expect({error, enotdir}, {error, enoent},
			 ?FILE_MODULE:del_dir(
			    filename:join(Afile, "another_directory"))),
    io:format("Result: ~p", [E1]),

    %% Delete a non-empty directory.
    {error, E2} = expect({error, enotempty}, {error, eexist}, {error, eacces},
			 ?FILE_MODULE:del_dir(Base)),
    io:format("Result: ~p", [E2]),

    %% Remove the current directory.
    {error, E3} = expect({error, einval}, 
			 {error, eperm}, % Linux and DUX
			 {error, eacces},
			 {error, ebusy},
			 ?FILE_MODULE:del_dir(".")),
    io:format("Result: ~p", [E3]),

    %% No permission.
    case os:type() of
	{win32, _} ->
	    ok;
	_ ->
	    ADirectory = filename:join(Base, "no_perm"),
	    ok = ?FILE_MODULE:make_dir(ADirectory),
	    ?FILE_MODULE:write_file_info( Base, #file_info {mode=0}),
	    {error, eacces} = ?FILE_MODULE:del_dir(ADirectory),
	    ?FILE_MODULE:write_file_info( Base, #file_info {mode=8#700})
    end,
    [] = flush(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Trying reading and positioning from a compressed file.

read_compressed_cooked(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Real = filename:join(Data, "realmen.html.gz"),
    {ok, Fd} = ?FILE_MODULE:open(Real, [read,compressed]),
    try_read_file_list(Fd).

read_compressed_cooked_binary(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Real = filename:join(Data, "realmen.html.gz"),
    {ok, Fd} = ?FILE_MODULE:open(Real, [read,compressed,binary]),
    try_read_file_binary(Fd).

%% Trying reading and positioning from an uncompressed file,
%% but with the compressed flag given.

read_not_really_compressed(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Priv = proplists:get_value(priv_dir, Config),

    %% The file realmen.html might have got CRs added (by WinZip).
    %% Remove them, or the file positions will not be correct.

    Real = filename:join(Data, "realmen.html"),
    RealPriv = filename:join(Priv,
			     atom_to_list(?MODULE)++"_realmen.html"),
    {ok, RealDataBin} = ?FILE_MODULE:read_file(Real),
    RealData = remove_crs(binary_to_list(RealDataBin), []),
    ok = ?FILE_MODULE:write_file(RealPriv, RealData),
    {ok, Fd} = ?FILE_MODULE:open(RealPriv, [read, compressed]),
    try_read_file_list(Fd).

remove_crs([$\r|Rest], Result) ->
    remove_crs(Rest, Result);
remove_crs([C|Rest], Result) ->
    remove_crs(Rest, [C|Result]);
remove_crs([], Result) ->
    lists:reverse(Result).

try_read_file_list(Fd) ->
    %% Seek to the current position (nothing should happen).

    {ok, 0} = ?FILE_MODULE:position(Fd, 0),
    {ok, 0} = ?FILE_MODULE:position(Fd, {cur, 0}),

    %% Read a few lines from a compressed file.

    ShouldBe = "<TITLE>Real Programmers Don't Use PASCAL</TITLE>\n",
    ShouldBe = io:get_line(Fd, ''),

    %% Now seek forward.

    {ok, 381} = ?FILE_MODULE:position(Fd, 381),
    Back = "Back in the good old days -- the \"Golden Era\" " ++
	"of computers, it was\n",
    Back = io:get_line(Fd, ''),

    %% Try to search forward relative to the current position.

    {ok, CurPos} = ?FILE_MODULE:position(Fd, {cur, 0}),
    RealPos = 4273,
    {ok, RealPos} = ?FILE_MODULE:position(Fd, {cur, RealPos-CurPos}),
    RealProg = "<LI> Real Programmers aren't afraid to use GOTOs.\n",
    RealProg = io:get_line(Fd, ''),

    %% Seek backward.

    AfterTitle = length("<TITLE>"),
    {ok, AfterTitle} = ?FILE_MODULE:position(Fd, AfterTitle),
    Title = "Real Programmers Don't Use PASCAL</TITLE>\n",
    Title = io:get_line(Fd, ''),

    %% Seek past the end of the file.

    {ok, _} = ?FILE_MODULE:position(Fd, 25000),

    %% Done.

    ?FILE_MODULE:close(Fd),
    [] = flush(),
    ok.

try_read_file_binary(Fd) ->
    %% Seek to the current position (nothing should happen).

    {ok, 0} = ?FILE_MODULE:position(Fd, 0),
    {ok, 0} = ?FILE_MODULE:position(Fd, {cur, 0}),

    %% Read a few lines from a compressed file.

    ShouldBe = <<"<TITLE>Real Programmers Don't Use PASCAL</TITLE>\n">>,
    ShouldBe = io:get_line(Fd, ''),

    %% Now seek forward.

    {ok, 381} = ?FILE_MODULE:position(Fd, 381),
    Back = <<"Back in the good old days -- the \"Golden Era\" "
	     "of computers, it was\n">>,
    Back = io:get_line(Fd, ''),

    %% Try to search forward relative to the current position.

    {ok, CurPos} = ?FILE_MODULE:position(Fd, {cur, 0}),
    RealPos = 4273,
    {ok, RealPos} = ?FILE_MODULE:position(Fd, {cur, RealPos-CurPos}),
    RealProg = <<"<LI> Real Programmers aren't afraid to use GOTOs.\n">>,
    RealProg = io:get_line(Fd, ''),

    %% Seek backward.

    AfterTitle = length("<TITLE>"),
    {ok, AfterTitle} = ?FILE_MODULE:position(Fd, AfterTitle),
    Title = <<"Real Programmers Don't Use PASCAL</TITLE>\n">>,
    Title = io:get_line(Fd, ''),

    %% Done.

    ?FILE_MODULE:close(Fd),
    [] = flush(),
    ok.

read_cooked_tar_problem(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    ProblemFile = filename:join(Data, "cooked_tar_problem.tar.gz"),
    {ok,Fd} = ?FILE_MODULE:open(ProblemFile, [read,compressed,binary]),

    {ok,34304} = file:position(Fd, 34304),
    {ok,Bin} = file:read(Fd, 512),
    512 = byte_size(Bin),

    {ok,34304+512+1024} = file:position(Fd, {cur,1024}),

    ok = file:close(Fd),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_compressed(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    MyFile = filename:join(Priv,
			   atom_to_list(?MODULE)++"_test.gz"),

    %% Write a file.

    {ok, Fd} = ?FILE_MODULE:open(MyFile, [write, compressed]),
    {ok, 0} = ?FILE_MODULE:position(Fd, 0),
    Prefix = "hello\n",
    End = "end\n",
    ok = io:put_chars(Fd, Prefix),
    {ok, 143} = ?FILE_MODULE:position(Fd, 143),
    ok = io:put_chars(Fd, End),
    ok = ?FILE_MODULE:close(Fd),

    %% Read the file and verify the contents.

    {ok, Fd1} = ?FILE_MODULE:open(MyFile, [read, compressed]),
    Prefix = io:get_line(Fd1, ''),
    Second = lists:duplicate(143-length(Prefix), 0) ++ End,
    Second = io:get_line(Fd1, ''),
    ok = ?FILE_MODULE:close(Fd1),

    %% Verify successful compression by uncompressing the file
    %% using zlib:gunzip/1.

    {ok,Contents} = file:read_file(MyFile),
    <<"hello\n",0:137/unit:8,"end\n">> = zlib:gunzip(Contents),

    %% Ensure that the file is compressed.

    TotalSize = 143 + length(End),
    case ?FILE_MODULE:read_file_info(MyFile) of
	{ok, #file_info{size=Size}} when Size < TotalSize ->
	    ok;
	{ok, #file_info{size=Size}} when Size == TotalSize ->
	    ct:fail(file_not_compressed)
    end,

    %% Write again to ensure that the file is truncated.

    {ok, Fd2} = ?FILE_MODULE:open(MyFile, [write, compressed]),
    NewString = "aaaaaaaaaaa",
    ok = io:put_chars(Fd2, NewString),
    ok = ?FILE_MODULE:close(Fd2),
    {ok, Fd3} = ?FILE_MODULE:open(MyFile, [read, compressed]),
    {ok, NewString} = ?FILE_MODULE:read(Fd3, 1024),
    ok = ?FILE_MODULE:close(Fd3),

    %% Done.

    [] = flush(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

catenated_gzips(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    MyFile = filename:join(Priv, ?MODULE_STRING++"_test.gz"),

    First = "Hello, all good men going to search parties. ",
    Second = "Now I really need your help.",
    All = iolist_to_binary([First|Second]),
    Cat = [zlib:gzip(First),zlib:gzip(Second)],

    ok = file:write_file(MyFile, Cat),

    {ok,Fd} = file:open(MyFile, [read,compressed,binary]),
    {ok,All} = file:read(Fd, 100000),
    ok = file:close(Fd),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compress_errors(Config) when is_list(Config) ->
    DataDir =
	filename:dirname(
	  filename:join(proplists:get_value(data_dir, Config), "x")),
    DataDirSlash = DataDir++"/",
    {error, enoent} = ?FILE_MODULE:open("non_existing__",
					[compressed, read]),
    {error, einval} = ?FILE_MODULE:open("non_existing__",
					[compressed, read, write]),
    {error, einval} = ?FILE_MODULE:open("non_existing__",
					[compressed, read, append]),
    {error, einval} = ?FILE_MODULE:open("non_existing__",
					[compressed, write, append]),
    {error, E1} = ?FILE_MODULE:open(DataDir, [compressed, read]),
    {error, E2} = ?FILE_MODULE:open(DataDirSlash, [compressed, read]),
    {error, E3} = ?FILE_MODULE:open(DataDir, [compressed, write]),
    {error, E4} = ?FILE_MODULE:open(DataDirSlash, [compressed, write]),
    {eisdir,eisdir,eisdir,eisdir} = {E1,E2,E3,E4},

    %% Read a corrupted .gz file.

    Corrupted = filename:join(DataDir, "corrupted.gz"),
    {ok, Fd} = ?FILE_MODULE:open(Corrupted, [read, compressed]),
    {error, eio} = ?FILE_MODULE:read(Fd, 100),
    ?FILE_MODULE:close(Fd),

    [] = flush(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compress_async_crash(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Path = filename:join(DataDir, "test.gz"),
    ExpectedData = <<"qwerty">>,

    _ = ?FILE_MODULE:delete(Path),
    {ok, Fd} = ?FILE_MODULE:open(Path, [write, binary, compressed]),
    ok = ?FILE_MODULE:write(Fd, ExpectedData),
    ok = ?FILE_MODULE:close(Fd),

    %% Test that when using async thread pool, the emulator doesn't crash
    %% when the efile port driver is stopped while a compressed file operation
    %% is in progress (being carried by an async thread).
    ok = compress_async_crash_loop(10000, Path, ExpectedData),
    ok = ?FILE_MODULE:delete(Path),
    ok.

compress_async_crash_loop(0, _Path, _ExpectedData) ->
    ok;
compress_async_crash_loop(N, Path, ExpectedData) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(
		   fun() ->
			   {ok, Fd} = ?FILE_MODULE:open(
					 Path, [read, compressed, raw, binary]),
			   Len = byte_size(ExpectedData),
			   Parent ! {self(), continue},
			   {ok, ExpectedData} = ?FILE_MODULE:read(Fd, Len),
			   ok = ?FILE_MODULE:close(Fd),
			   receive foobar -> ok end
		   end),
    receive
        {Pid, continue} ->
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, _, _, Reason} ->
                    shutdown = Reason
            end;
        {'DOWN', Ref, _, _, Reason2} ->
            ct:fail({worker_exited, Reason2})
    after 60000 ->
            exit(Pid, shutdown),
            erlang:demonitor(Ref, [flush]),
            ct:fail(worker_timeout)
    end,
    compress_async_crash_loop(N - 1, Path, ExpectedData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unicode(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    Name = filename:join(Dir, "data-utf8.txt"),
    Txt = lists:seq(128, 255),
    D = unicode:characters_to_binary(Txt, latin1, latin1),
    {ok,Fd1} =
	?FILE_MODULE:open(Name, [write,read,binary,{encoding,unicode}]),
    ok = ?FILE_MODULE:truncate(Fd1),
    ok = ?FILE_MODULE:write(Fd1, Txt),
    {ok,0} = ?FILE_MODULE:position(Fd1, bof),
    {ok,D} = ?FILE_MODULE:read(Fd1, 129),
    {ok,0} = ?FILE_MODULE:position(Fd1, bof),
    {ok,D1} = ?FILE_MODULE:read(Fd1, 64),
    {ok,Pos} = ?FILE_MODULE:position(Fd1, cur),
    {ok,D2} = ?FILE_MODULE:pread(Fd1, {cur,0}, 65),
    D = <<D1/binary, D2/binary>>,
    {ok,D1} = ?FILE_MODULE:pread(Fd1, bof, 64),
    {ok,Pos} = ?FILE_MODULE:position(Fd1, Pos),
    {ok,D2} = ?FILE_MODULE:read(Fd1, 64),
    ok = ?FILE_MODULE:close(Fd1),
    %%
    RawD = unicode:characters_to_binary(Txt, latin1, unicode),
    {ok,RawD} = ?FILE_MODULE:read_file(Name),
    %%
    {ok,Fd2} = ?FILE_MODULE:open(Name, [read,{encoding,unicode}]),
    {ok,Txt} = ?FILE_MODULE:read(Fd2, 129),
    {Txt1,Txt2} = lists:split(64, Txt),
    {ok,Txt2} = ?FILE_MODULE:pread(Fd2, Pos, 65),
    {ok,0} = ?FILE_MODULE:position(Fd2, bof),
    {ok,Txt1} = ?FILE_MODULE:read(Fd2, 64),
    ok = ?FILE_MODULE:close(Fd2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test the file:altname/1 function.
altname(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   "long alternative path name with spaces"),
    ok = ?FILE_MODULE:make_dir(NewDir),
    Name = filename:join(NewDir, "a_file_with_long_name"),
    ShortName = filename:join(NewDir, "short"),
    NonexName = filename:join(NewDir, "nonexistent"),
    ok = ?FILE_MODULE:write_file(Name, "some contents\n"),
    ok = ?FILE_MODULE:write_file(ShortName, "some contents\n"),
    Result =
	case ?FILE_MODULE:altname(NewDir) of
	    {error, enotsup} ->
		{skipped, "Altname not supported on this platform"};
	    {ok, "LONGAL~1"} -> 
		{ok, "A_FILE~1"} = ?FILE_MODULE:altname(Name),
		{ok, "c:/"} = ?FILE_MODULE:altname("C:/"),
		{ok, "c:/"} = ?FILE_MODULE:altname("C:\\"),
		{error,enoent} = ?FILE_MODULE:altname(NonexName),
		{ok, "short"} = ?FILE_MODULE:altname(ShortName),
		ok
	end,
    [] = flush(),
    Result.


%% Test creating a hard link.
make_link(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_make_link"),
    ok = ?FILE_MODULE:make_dir(NewDir),

    Name = filename:join(NewDir, "a_file"),
    ok = ?FILE_MODULE:write_file(Name, "some contents\n"),

    Alias = filename:join(NewDir, "an_alias"),
    Result =
	case ?FILE_MODULE:make_link(Name, Alias) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    ok ->
		%% Note: We take the opportunity to test 
		%% ?FILE_MODULE:read_link_info/1,
		%% which should in behave exactly as 
		%% ?FILE_MODULE:read_file_info/1
		%% since they are not used on symbolic links.

		{ok, Info} = ?FILE_MODULE:read_link_info(Name),
                {ok,Info} = ?FILE_MODULE:read_link_info(Name, [raw]),
		{ok, Info} = ?FILE_MODULE:read_link_info(Alias),
                {ok,Info} = ?FILE_MODULE:read_link_info(Alias, [raw]),
		#file_info{links = 2, type = regular} = Info,
		{error, eexist} =
		    ?FILE_MODULE:make_link(Name, Alias),
		ok
	end,

    [] = flush(),
    Result.

%% Test that reading link info for an ordinary file or directory works
%% (on all platforms).
read_link_info_for_non_link(Config) when is_list(Config) ->
    {ok, #file_info{type=directory}} =
	?FILE_MODULE:read_link_info("."),
    {ok, #file_info{type=directory}} = ?FILE_MODULE:read_link_info(".", [raw]),

    [] = flush(),
    ok.

%% Test operations on symbolic links (for Unix).
symlinks(Config) when is_list(Config) ->
    {error, _} = ?FILE_MODULE:read_link(lists:duplicate(10000,$a)),
    {error, _} = ?FILE_MODULE:read_link_all(lists:duplicate(10000,$a)),
    RootDir = proplists:get_value(priv_dir, Config),
    NewDir = filename:join(RootDir,
			   atom_to_list(?MODULE)
			   ++"_symlinks"),
    ok = ?FILE_MODULE:make_dir(NewDir),

    Name = filename:join(NewDir, "a_plain_file"),
    ok = ?FILE_MODULE:write_file(Name, "some stupid content\n"),

    Alias = filename:join(NewDir, "a_symlink_alias"),
    Result =
	case ?FILE_MODULE:make_symlink(Name, Alias) of
	    {error, enotsup} ->
		{skipped, "Links not supported on this platform"};
	    {error, eperm} ->
		{win32,_} = os:type(),
		{skipped, "Windows user not privileged to create symlinks"};
	    ok ->
		{ok, Info1} = ?FILE_MODULE:read_file_info(Name),
                {ok,Info1} = ?FILE_MODULE:read_file_info(Name, [raw]),
		{ok, Info1} = ?FILE_MODULE:read_file_info(Alias),
                {ok,Info1} = ?FILE_MODULE:read_file_info(Alias, [raw]),
		{ok, Info1} = ?FILE_MODULE:read_link_info(Name),
                {ok,Info1} = ?FILE_MODULE:read_link_info(Name, [raw]),
		#file_info{links = 1, type = regular} = Info1,

		{ok, Info2} = ?FILE_MODULE:read_link_info(Alias),
                {ok,Info2} = ?FILE_MODULE:read_link_info(Alias, [raw]),
		#file_info{links=1, type=symlink} = Info2,
		{ok, Name} = ?FILE_MODULE:read_link(Alias),
		{ok, Name} = ?FILE_MODULE:read_link_all(Alias),
		%% If all is good, delete dir again (avoid hanging dir on windows)
		rm_rf(?FILE_MODULE,NewDir),
		ok
	end,

    [] = flush(),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    %% Create a text file.
    Name1 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_1.txt"),
    Line = "The quick brown fox jumps over a lazy dog. 0123456789\n",
    Len = length(Line),
    {ok, Handle1} = ?FILE_MODULE:open(Name1, [write]),
    {_, Size1} =
	iterate({0, 0},
		done,
		fun({_, S}) when S >= 128*1024 ->
			done;
		   ({N, S}) ->
			H = integer_to_list(N),
			ok = ?FILE_MODULE:write(Handle1, [H, " ", Line]),
			{N + 1, S + length(H) + 1 + Len}
		end),
    ?FILE_MODULE:close(Handle1),
    %% Make a copy
    Name2 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_2.txt"),
    {ok, Size1} = ?FILE_MODULE:copy(Name1, Name2),
    %% Concatenate 1
    Name3 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_3.txt"),
    {ok, Handle3} = ?FILE_MODULE:open(Name3, [raw, write, binary]),
    {ok, Size1} = ?FILE_MODULE:copy(Name1, Handle3),
    {ok, Handle2} = ?FILE_MODULE:open(Name2, [read, binary]),
    {ok, Size1} = ?FILE_MODULE:copy(Handle2, Handle3),
    ok = ?FILE_MODULE:close(Handle3),
    ok = ?FILE_MODULE:close(Handle2),
    %% Concatenate 2
    Name4 = filename:join(RootDir, atom_to_list(?MODULE)++"_copy_4.txt"),
    {ok, Handle4} = ?FILE_MODULE:open(Name4, [write, binary]),
    {ok, Size1} = ?FILE_MODULE:copy(Name1, Handle4),
    {ok, Handle5} = ?FILE_MODULE:open(Name2, [raw, read, binary]),
    {ok, Size1} = ?FILE_MODULE:copy(Handle5, Handle4),
    ok = ?FILE_MODULE:close(Handle5),
    ok = ?FILE_MODULE:close(Handle4),
    %% %% Just for test of the test
    %% {ok, Handle2q} = ?FILE_MODULE:open(Name2, [write, append]),
    %% ok = ?FILE_MODULE:write(Handle2q, "q"),
    %% ok = ?FILE_MODULE:close(Handle2q),
    %% Compare the files
    {ok, Handle1a} = ?FILE_MODULE:open(Name1, [raw, read]),
    {ok, Handle2a} = ?FILE_MODULE:open(Name2, [raw, read]),
    true = stream_cmp(fd_stream_factory([Handle1a]),
		      fd_stream_factory([Handle2a])),
    {ok, 0} = ?FILE_MODULE:position(Handle1a, 0),
    {ok, 0} = ?FILE_MODULE:position(Handle2a, 0),
    {ok, Handle3a} = ?FILE_MODULE:open(Name3, [raw, read]),
    true = stream_cmp(fd_stream_factory([Handle1a, Handle2a]),
		      fd_stream_factory([Handle2a])),
    ok = ?FILE_MODULE:close(Handle1a),
    ok = ?FILE_MODULE:close(Handle2a),
    ok = ?FILE_MODULE:close(Handle3a),
    [] = flush(),
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
    L = "qwertyuiopasdfghjklzxcvbnm",
    N = length(L),
    {ok, Cwd}         = ?FILE_MODULE:get_cwd(),
    {error, enotsup}  = ?FILE_MODULE:get_cwd("C:"), % Unix only testcase
    {ok, FD1}         = ?FILE_MODULE:open("file1.txt", write),
    ok                = ?FILE_MODULE:close(FD1),
    {ok, FD2}         = ?FILE_MODULE:open("file1.txt",
					  [write, append,
					   binary, compressed,
					   delayed_write,
					   {delayed_write, 0, 0},
					   read_ahead,
					   {read_ahead, 0}]),
    ok                = ?FILE_MODULE:write(FD2, L),
    ok                = ?FILE_MODULE:close(FD2),
    {ok, N2}          = ?FILE_MODULE:copy("file1.txt", "file2.txt"),
    io:format("Size ~p, compressed ~p.~n", [N, N2]),
    {ok, FD3}         = ?FILE_MODULE:open("file2.txt",
					  [binary, compressed]),
    %% The file_io_server will translate the binary into a list
    {ok, L}           = ?FILE_MODULE:read(FD3, N+1),
    ok                = ?FILE_MODULE:close(FD3),
    %%
    ok                = ?FILE_MODULE:delete("file1.txt"),
    ok                = ?FILE_MODULE:delete("file2.txt"),
    []                = flush(),
    ok.


%% Test the get_cwd() and open() file server calls.
old_slave(_RootDir, Cwd) ->
    L = "qwertyuiopasdfghjklzxcvbnm",
    N = length(L),
    {ok, Cwd}         = ?FILE_MODULE:get_cwd(),
    {error, enotsup}  = ?FILE_MODULE:get_cwd("C:"), % Unix only testcase
    {ok, FD1}         = ?FILE_MODULE:open("file1.txt", write),
    ok                = ?FILE_MODULE:close(FD1),
    {ok, FD2}         = ?FILE_MODULE:open("file1.txt",
					  [write, binary, compressed]),
    ok                = ?FILE_MODULE:write(FD2, L),
    ok                = ?FILE_MODULE:close(FD2),
    {ok, FD3}         = ?FILE_MODULE:open("file1.txt", [write, append]),
    ok                = ?FILE_MODULE:close(FD3),
    {ok, FD4}         = ?FILE_MODULE:open("file1.txt",
					  [binary, compressed]),
    %% The file_io_server will translate the binary into a list
    {ok, L}           = ?FILE_MODULE:read(FD4, N+1),
    ok                = ?FILE_MODULE:close(FD4),
    %%
    ok                = ?FILE_MODULE:delete("file1.txt"),
    []                = flush(),
    ok.

run_test(Test, Args) ->
    case (catch apply(?MODULE, Test, Args)) of
	{'EXIT', _} = Exit ->
	    {done, Exit, get(test_server_loc)};
	Result ->
	    {done, Result}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the file open option {delayed_write, Size, Delay}.

delayed_write(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    File = filename:join(RootDir, 
			 atom_to_list(?MODULE)++"_delayed_write.txt"),
    Data1 = "asdfghjkl",
    Data2 = "qwertyuio",
    Data3 = "zxcvbnm,.",
    Size = length(Data1),
    Size = length(Data2),
    Size = length(Data3),
    Data1Data1 = Data1++Data1,
    Data1Data1Data1 = Data1Data1++Data1,
    Data1Data1Data1Data1 = Data1Data1++Data1Data1,
    %%
    %% Test caching and normal close of non-raw file
    {ok, Fd1} = 
	?FILE_MODULE:open(File, [write, {delayed_write, Size+1, 400}]),
    ok = ?FILE_MODULE:write(Fd1, Data1),
    %% Wait for a reasonable amount of time to check whether the write was
    %% practically instantaneous or actually delayed.
    timer:sleep(100),
    {ok, Fd2} = ?FILE_MODULE:open(File, [read]),
    eof = ?FILE_MODULE:read(Fd2, 1),
    ok = ?FILE_MODULE:write(Fd1, Data1), % Data flush on size
    timer:sleep(100),
    {ok, Data1Data1} = ?FILE_MODULE:pread(Fd2, bof, 2*Size+1),
    ok = ?FILE_MODULE:write(Fd1, Data1),
    timer:sleep(500), % Wait until data flush on timeout
    {ok, Data1Data1Data1} = ?FILE_MODULE:pread(Fd2, bof, 3*Size+1),
    ok = ?FILE_MODULE:write(Fd1, Data1),
    ok = ?FILE_MODULE:close(Fd1), % Data flush on close
    timer:sleep(100),
    {ok, Data1Data1Data1Data1} = ?FILE_MODULE:pread(Fd2, bof, 4*Size+1),
    ok = ?FILE_MODULE:close(Fd2),
    %%
    %% Test implicit close through exit by file owning process, 
    %% raw file, default parameters.
    Parent = self(),
    Fun = fun() ->
		  Child = self(),
		  Test =
		      fun () ->
			      {ok, Fd} = ?FILE_MODULE:open(File,
							   [raw, write, delayed_write]),
			      ok = ?FILE_MODULE:write(Fd, Data1),
			      Parent ! {Child, wrote},
			      receive
				  {Parent, continue, Reason} ->
				      {ok, Reason}
			      end
		      end,
		  case (catch Test()) of
		      {ok, Reason} -> exit(Reason);
		      Unknown ->
			  exit({Unknown, get(test_server_loc)})
		  end
	  end,
    Child1 = spawn(Fun),
    Mref1 = erlang:monitor(process, Child1),
    receive 
        {Child1, wrote} -> 
            ok;
        {'DOWN', Mref1, _, _, _} = Down1a ->
            ct:fail(Down1a)
    end,
    timer:sleep(100), % Just in case the file system is slow
    {ok, Fd3} = ?FILE_MODULE:open(File, [read]),
    eof = ?FILE_MODULE:read(Fd3, 1),
    Child1 ! {Parent, continue, normal},
    receive 
        {'DOWN', Mref1, process, Child1, normal} -> 
            ok;
        {'DOWN', Mref1, _, _, _} = Down1b ->
            ct:fail(Down1b)
    end,
    timer:sleep(100), % Just in case the file system is slow
    {ok, Data1} = ?FILE_MODULE:pread(Fd3, bof, Size+1),
    ok = ?FILE_MODULE:close(Fd3),
    %%
    %% The same again, but this time with reason 'kill'.
    Child2 = spawn(Fun),
    Mref2 = erlang:monitor(process, Child2),
    receive 
        {Child2, wrote} -> 
            ok;
        {'DOWN', Mref2, _, _, _} = Down2a ->
            ct:fail(Down2a)
    end,
    timer:sleep(100), % Just in case the file system is slow
    {ok, Fd4} = ?FILE_MODULE:open(File, [read]),
    eof = ?FILE_MODULE:read(Fd4, 1),
    Child2 ! {Parent, continue, kill},
    receive 
        {'DOWN', Mref2, process, Child2, kill} -> 
            ok;
        {'DOWN', Mref2, _, _, _} = Down2b ->
            ct:fail(Down2b)
    end,
    timer:sleep(100), % Just in case the file system is slow
    eof = ?FILE_MODULE:pread(Fd4, bof, 1),
    ok  = ?FILE_MODULE:close(Fd4),
    %%
    %% Test if file position works with delayed_write
    {ok, Fd5} = ?FILE_MODULE:open(File, [raw, read, write, 
					 delayed_write]),
    ok = ?FILE_MODULE:truncate(Fd5),
    ok = ?FILE_MODULE:write(Fd5, [Data1|Data2]),
    {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    ok = ?FILE_MODULE:write(Fd5, [Data3]),
    {ok, Data2} = ?FILE_MODULE:read(Fd5, Size+1),
    {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    Data3Data2 = Data3++Data2,
    {ok, Data3Data2} = ?FILE_MODULE:read(Fd5, 2*Size+1),
    ok = ?FILE_MODULE:close(Fd5),
    %%
    [] = flush(),
    ok.


%% Tests file:pid2name/1.
pid2name(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    Base = test_server:temp_name(
	     filename:join(RootDir, "pid2name_")),
    Name1 = [Base, '.txt'],
    Name2 = Base ++ ".txt",
    %%
    {ok, Pid} = file:open(Name1, [write]),
    {ok, Name2} = file:pid2name(Pid),
    undefined = file:pid2name(self()),
    ok = file:close(Pid),
    ct:sleep(1000),
    false = is_process_alive(Pid),
    undefined = file:pid2name(Pid),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the file open option {read_ahead, Size}.

read_ahead(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    File = filename:join(RootDir,
			 atom_to_list(?MODULE)++"_read_ahead.txt"),
    Data1 = "asdfghjkl", % Must be
    Data2 = "qwertyuio", % same
    Data3 = "zxcvbnm,.", % length
    Size = length(Data1),
    Size = length(Data2),
    Size = length(Data3),
    %%
    %% Test caching of normal non-raw file
    {ok, Fd1} = ?FILE_MODULE:open(File, [write]),
    ok = ?FILE_MODULE:write(Fd1, [Data1|Data1]),
    timer:sleep(1000), % Just in case the file system is slow
    {ok, Fd2} = ?FILE_MODULE:open(File, [read, {read_ahead, 2*Size}]),
    {ok, Data1} = ?FILE_MODULE:read(Fd2, Size),
    ok = ?FILE_MODULE:pwrite(Fd1, Size, Data2),
    timer:sleep(1000), % Just in case the file system is slow
    {ok, Data1} = ?FILE_MODULE:read(Fd2, Size), % Will read cached data
    Data2Data2Data2 = Data2++Data2++Data2,
    ok = ?FILE_MODULE:pwrite(Fd1, eof, Data2Data2Data2),
    timer:sleep(1000), % Just in case the file system is slow
    {ok, Data2Data2Data2} =
	?FILE_MODULE:read(Fd2, 3*Size), % Read more than cache buffer
    ok = ?FILE_MODULE:close(Fd1),
    ok = ?FILE_MODULE:close(Fd2),
    %% Test caching of raw file and default parameters
    {ok, Fd3} = ?FILE_MODULE:open(File, [raw, write]),
    ok = ?FILE_MODULE:write(Fd3, [Data1|Data1]),
    timer:sleep(1000), % Just in case the file system is slow
    {ok, Fd4} = ?FILE_MODULE:open(File, [raw, read, read_ahead]),
    {ok, Data1} = ?FILE_MODULE:read(Fd4, Size),
    ok = ?FILE_MODULE:pwrite(Fd3, Size, Data2),
    timer:sleep(1000), % Just in case the file system is slow
    {ok, Data1} = ?FILE_MODULE:read(Fd4, Size), % Will read cached data
    ok = ?FILE_MODULE:close(Fd3),
    ok = ?FILE_MODULE:close(Fd4),
    %% Test if the file position works in combination with read_ahead
    {ok, Fd5} = ?FILE_MODULE:open(File, [raw, read, write, read_ahead]),
    ok = ?FILE_MODULE:truncate(Fd5),
    ok = ?FILE_MODULE:write(Fd5, [Data1,Data1|Data3]),
    {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    {ok, Data1} = ?FILE_MODULE:read(Fd5, Size),
    ok = ?FILE_MODULE:write(Fd5, Data2),
    {ok, 0} = ?FILE_MODULE:position(Fd5, bof),
    Data1Data2Data3 = Data1++Data2++Data3,
    {ok, Data1Data2Data3} = ?FILE_MODULE:read(Fd5, 3*Size+1),
    ok = ?FILE_MODULE:close(Fd5),

    %% Ensure that a read that draws from both the buffer and the file won't
    %% return anything wonky.
    SplitData = << <<(I rem 256)>> || I <- lists:seq(1, 1024) >>,
    file:write_file(File, SplitData),
    {ok, Fd6} = ?FILE_MODULE:open(File, [raw, read, binary, {read_ahead, 256}]),
    {ok, <<1>>} = file:read(Fd6, 1),
    <<1, Shifted:512/binary, _Rest/binary>> = SplitData,
    {ok, Shifted} = file:read(Fd6, 512),

    %%
    [] = flush(),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Tests the segmenting of large reads.
segment_read(Config) when is_list(Config) ->
    Name = filename:join(proplists:get_value(priv_dir, Config),
			 ?MODULE_STRING ++ "_segment_read"),
    SegSize = 256*1024,
    SegCnt = SegSize div 4,
    Cnt = 4 * SegCnt,
    ok = create_file(Name, Cnt),
    %% 
    %% read_file/1
    %%
    {ok, Bin} = ?FILE_MODULE:read_file(Name),
    true = verify_bin(Bin, 0, Cnt),
    %%
    %% read/2
    %%
    %% Not segmented
    {ok, FD1} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    {ok, B1a} = ?FILE_MODULE:read(FD1, SegSize),
    {ok, B1b} = ?FILE_MODULE:read(FD1, SegSize),
    {ok, B1c} = ?FILE_MODULE:read(FD1, SegSize),
    {ok, B1d} = ?FILE_MODULE:read(FD1, SegSize),
    ok = ?FILE_MODULE:close(FD1),
    true = verify_bin(B1a, 0*SegCnt, SegCnt),
    true = verify_bin(B1b, 1*SegCnt, SegCnt),
    true = verify_bin(B1c, 2*SegCnt, SegCnt),
    true = verify_bin(B1d, 3*SegCnt, SegCnt),
    %%
    %% Segmented
    {ok, FD2} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    {ok, B2a} = ?FILE_MODULE:read(FD2, 1*SegSize),
    {ok, B2b} = ?FILE_MODULE:read(FD2, 2*SegSize),
    {ok, B2c} = ?FILE_MODULE:read(FD2, 2*SegSize),
    ok = ?FILE_MODULE:close(FD2),
    true = verify_bin(B2a, 0*SegCnt, 1*SegCnt),
    true = verify_bin(B2b, 1*SegCnt, 2*SegCnt),
    true = verify_bin(B2c, 3*SegCnt, 1*SegCnt),
    %%
    %% pread/3
    %%
    {ok, FD3} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    %%
    %% Not segmented
    {ok, B3d} = ?FILE_MODULE:pread(FD3, 3*SegSize, SegSize),
    {ok, B3c} = ?FILE_MODULE:pread(FD3, 2*SegSize, SegSize),
    {ok, B3b} = ?FILE_MODULE:pread(FD3, 1*SegSize, SegSize),
    {ok, B3a} = ?FILE_MODULE:pread(FD3, 0*SegSize, SegSize),
    true = verify_bin(B3a, 0*SegCnt, SegCnt),
    true = verify_bin(B3b, 1*SegCnt, SegCnt),
    true = verify_bin(B3c, 2*SegCnt, SegCnt),
    true = verify_bin(B3d, 3*SegCnt, SegCnt),
    %%
    %% Segmented
    {ok, B3g} = ?FILE_MODULE:pread(FD3, 3*SegSize, 2*SegSize),
    {ok, B3f} = ?FILE_MODULE:pread(FD3, 1*SegSize, 2*SegSize),
    {ok, B3e} = ?FILE_MODULE:pread(FD3, 0*SegSize, 1*SegSize),
    true = verify_bin(B3e, 0*SegCnt, 1*SegCnt),
    true = verify_bin(B3f, 1*SegCnt, 2*SegCnt),
    true = verify_bin(B3g, 3*SegCnt, 1*SegCnt),
    %%
    ok = ?FILE_MODULE:close(FD3),
    %%
    %% pread/2
    %%
    {ok, FD5} = ?FILE_MODULE:open(Name, [read, raw, binary]),
    %%
    %% +---+---+---+---+
    %% | 4 | 3 | 2 | 1 |
    %% +---+---+---+---+
    %% <       ^       >
    {ok, [B5d, B5c, B5b, B5a]} =
	?FILE_MODULE:pread(FD5, [{3*SegSize, SegSize},
				 {2*SegSize, SegSize},
				 {1*SegSize, SegSize},
				 {0*SegSize, SegSize}]),
    true = verify_bin(B5a, 0*SegCnt, SegCnt),
    true = verify_bin(B5b, 1*SegCnt, SegCnt),
    true = verify_bin(B5c, 2*SegCnt, SegCnt),
    true = verify_bin(B5d, 3*SegCnt, SegCnt),
    %%
    %% +---+-------+-------+
    %% | 3 |   2   |   1   |
    %% +---+-------+-------+
    %% <     ^     ^   >
    {ok, [B5g, B5f, B5e]} =
	?FILE_MODULE:pread(FD5, [{3*SegSize, 2*SegSize},
				 {1*SegSize, 2*SegSize},
				 {0*SegSize, 1*SegSize}]),
    true = verify_bin(B5e, 0*SegCnt, 1*SegCnt),
    true = verify_bin(B5f, 1*SegCnt, 2*SegCnt),
    true = verify_bin(B5g, 3*SegCnt, 1*SegCnt),
    %%
    %%
    %% +-------+-----------+
    %% |   2   |     1     |
    %% +-------+-----------+
    %% <     ^     ^   >
    {ok, [B5i, B5h]} =
	?FILE_MODULE:pread(FD5, [{2*SegSize, 3*SegSize},
				 {0*SegSize, 2*SegSize}]),
    true = verify_bin(B5h, 0*SegCnt, 2*SegCnt),
    true = verify_bin(B5i, 2*SegCnt, 2*SegCnt),
    %%
    %% +-------+---+---+
    %% |   3   | 2 | 1 |
    %% +-------+---+---+
    %% <     ^     ^   >
    {ok, [B5l, B5k, B5j]} =
	?FILE_MODULE:pread(FD5, [{3*SegSize, 1*SegSize},
				 {2*SegSize, 1*SegSize},
				 {0*SegSize, 2*SegSize}]),
    true = verify_bin(B5j, 0*SegCnt, 2*SegCnt),
    true = verify_bin(B5k, 2*SegCnt, 1*SegCnt),
    true = verify_bin(B5l, 3*SegCnt, 1*SegCnt),
    %%
    %% Real time response time test.
    %%
    Req = lists:flatten(lists:duplicate(17,
					[{2*SegSize, 2*SegSize},
					 {0*SegSize, 2*SegSize}])),
    {{ok, _}, Comment} =
	response_analysis(?FILE_MODULE, pread, [FD5, Req]),
    ok = ?FILE_MODULE:close(FD5),
    %%
    [] = flush(),
    {comment, Comment}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Tests the segmenting of large writes.
segment_write(Config) when is_list(Config) ->
    Name = filename:join(proplists:get_value(priv_dir, Config),
			 ?MODULE_STRING ++ "_segment_write"),
    SegSize = 256*1024,
    SegCnt = SegSize div 4,
    Cnt = 4 * SegCnt,
    Bin = create_bin(0, Cnt),
    %%
    %% write/2
    %%
    %% Not segmented
    {ok, FD1} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ok = ?FILE_MODULE:write(FD1, subbin(Bin, 0*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:write(FD1, subbin(Bin, 1*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:write(FD1, subbin(Bin, 2*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:write(FD1, subbin(Bin, 3*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:close(FD1),
    true = verify_file(Name, Cnt),
    %%
    %% Segmented
    {ok, FD2} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ok = ?FILE_MODULE:write(FD2, subbin(Bin, 0*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:write(FD2, subbin(Bin, 1*SegSize, 2*SegSize)),
    ok = ?FILE_MODULE:write(FD2, subbin(Bin, 3*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:close(FD2),
    true = verify_file(Name, Cnt),
    %%
    %% +---+---+---+---+
    %% |   |   |   |   |
    %% +---+---+---+---+
    %% <       ^       >
    ok = write_file(Name, [subbin(Bin, 0*SegSize, 1*SegSize),
			   subbin(Bin, 1*SegSize, 1*SegSize),
			   subbin(Bin, 2*SegSize, 1*SegSize),
			   subbin(Bin, 3*SegSize, 1*SegSize)]),
    true = verify_file(Name, Cnt),
    %%
    %% +---+-------+---+
    %% |   |       |   |
    %% +---+-------+---+
    %% <     ^     ^   >
    ok = write_file(Name, [subbin(Bin, 0*SegSize, 1*SegSize),
			   subbin(Bin, 1*SegSize, 2*SegSize),
			   subbin(Bin, 3*SegSize, 1*SegSize)]),
    true = verify_file(Name, Cnt),
    %%
    %% +-------+-------+
    %% |       |       |
    %% +-------+-------+
    %% <     ^     ^   >
    ok = write_file(Name, [subbin(Bin, 0*SegSize, 2*SegSize),
			   subbin(Bin, 2*SegSize, 2*SegSize)]),
    true = verify_file(Name, Cnt),
    %%
    %% +-------+---+---+
    %% |       |   |   |
    %% +-------+---+---+
    %% <     ^     ^   >
    ok = write_file(Name, [subbin(Bin, 0*SegSize, 2*SegSize),
			   subbin(Bin, 2*SegSize, 1*SegSize),
			   subbin(Bin, 3*SegSize, 1*SegSize)]),
    true = verify_file(Name, Cnt),
    %%
    %% pwrite/3
    %%
    %% Not segmented
    {ok, FD3} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ok = ?FILE_MODULE:pwrite(FD3, 3*SegSize,
			     subbin(Bin, 3*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:pwrite(FD3, 2*SegSize,
			     subbin(Bin, 2*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:pwrite(FD3, 1*SegSize,
			     subbin(Bin, 1*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:pwrite(FD3, 0*SegSize,
			     subbin(Bin, 0*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:close(FD3),
    true = verify_file(Name, Cnt),
    %%
    %% Segmented
    {ok, FD4} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ok = ?FILE_MODULE:pwrite(FD4, 3*SegSize,
			     subbin(Bin, 3*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:pwrite(FD4, 1*SegSize,
			     subbin(Bin, 1*SegSize, 2*SegSize)),
    ok = ?FILE_MODULE:pwrite(FD4, 0*SegSize,
			     subbin(Bin, 0*SegSize, 1*SegSize)),
    ok = ?FILE_MODULE:close(FD4),
    true = verify_file(Name, Cnt),



    %%
    %% pwrite/2
    %%
    %% Not segmented
    {ok, FD5} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ok = ?FILE_MODULE:pwrite(FD5, [{3*SegSize,
				    subbin(Bin, 3*SegSize, 1*SegSize)}]),
    ok = ?FILE_MODULE:pwrite(FD5, [{2*SegSize,
				    subbin(Bin, 2*SegSize, 1*SegSize)}]),
    ok = ?FILE_MODULE:pwrite(FD5, [{1*SegSize,
				    subbin(Bin, 1*SegSize, 1*SegSize)}]),
    ok = ?FILE_MODULE:pwrite(FD5, [{0*SegSize,
				    subbin(Bin, 0*SegSize, 1*SegSize)}]),
    ok = ?FILE_MODULE:close(FD5),
    true = verify_file(Name, Cnt),
    %%
    %% Segmented
    {ok, FD6} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    ok = ?FILE_MODULE:pwrite(FD6, [{3*SegSize,
				    subbin(Bin, 3*SegSize, 1*SegSize)}]),
    ok = ?FILE_MODULE:pwrite(FD6, [{1*SegSize,
				    subbin(Bin, 1*SegSize, 2*SegSize)}]),
    ok = ?FILE_MODULE:pwrite(FD6, [{0*SegSize,
				    subbin(Bin, 0*SegSize, 1*SegSize)}]),
    ok = ?FILE_MODULE:close(FD6),
    true = verify_file(Name, Cnt),
    %%
    %% +---+---+---+---+
    %% | 4 | 3 | 2 | 1 |
    %% +---+---+---+---+
    %% <       ^       >
    ok = pwrite_file(Name, [{3*SegSize,
			     subbin(Bin, 3*SegSize, 1*SegSize)},
			    {2*SegSize,
			     subbin(Bin, 2*SegSize, 1*SegSize)},
			    {1*SegSize,
			     subbin(Bin, 1*SegSize, 1*SegSize)},
			    {0*SegSize,
			     subbin(Bin, 0*SegSize, 1*SegSize)}]),
    true = verify_file(Name, Cnt),
    %%
    %% +---+-------+---+
    %% | 3 |   2   | 1 |
    %% +---+-------+---+
    %% <     ^     ^   >
    ok = pwrite_file(Name, [{3*SegSize,
			     subbin(Bin, 3*SegSize, 1*SegSize)},
			    {1*SegSize,
			     subbin(Bin, 1*SegSize, 2*SegSize)},
			    {0*SegSize,
			     subbin(Bin, 0*SegSize, 1*SegSize)}]),
    true = verify_file(Name, Cnt),
    %%
    %% +-------+-------+
    %% |   2   |   1   |
    %% +-------+-------+
    %% <     ^     ^   >
    ok = pwrite_file(Name, [{2*SegSize,
			     subbin(Bin, 2*SegSize, 2*SegSize)},
			    {0*SegSize,
			     subbin(Bin, 0*SegSize, 2*SegSize)}]),
    true = verify_file(Name, Cnt),
    %%
    %% +-------+---+---+
    %% |   3   | 2 | 1 |
    %% +-------+---+---+
    %% <     ^     ^   >
    ok = pwrite_file(Name, [{3*SegSize,
			     subbin(Bin, 3*SegSize, 1*SegSize)},
			    {2*SegSize,
			     subbin(Bin, 2*SegSize, 1*SegSize)},
			    {0*SegSize,
			     subbin(Bin, 0*SegSize, 2*SegSize)}]),
    true = verify_file(Name, Cnt),
    %%
    %% Real time response time test.
    %%
    {ok, FD7} = ?FILE_MODULE:open(Name, [write, raw, binary]),
    Req = lists:flatten(lists:duplicate(17,
					[{2*SegSize, 
					  subbin(Bin, 2*SegSize, 2*SegSize)},
					 {0*SegSize,
					  subbin(Bin, 0*SegSize, 2*SegSize)}])),
    {ok, Comment} =
	response_analysis(?FILE_MODULE, pwrite, [FD7, Req]),
    ok = ?FILE_MODULE:close(FD7),
    %%
    [] = flush(),
    {comment, Comment}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test Dets special indirect pread.
ipread(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    ok = ipread_int(Dir, [raw, binary]),
    ok = ipread_int(Dir, [raw]),
    ok = ipread_int(Dir, [binary]),
    ok = ipread_int(Dir, []),
    ok = ipread_int(Dir, [ram, binary]),
    ok = ipread_int(Dir, [ram]),
    %%
    [] = flush(),
    ok.

ipread_int(Dir, ModeList) ->
    Name =
	filename:join(Dir, 
		      lists:flatten([?MODULE_STRING, "_ipread",
				     lists:map(fun (X) ->
						       ["_", atom_to_list(X)]
					       end,
					       ModeList)])),
    io:format("ipread_int<~p, ~p>~n", [Name, ModeList]),
    {Conv, Sizeof} =
	case lists:member(binary, ModeList) of
	    true ->
		{fun (Bin) when is_binary(Bin) -> Bin;
		     (List) when is_list(List) -> list_to_binary(List)
		 end, 
		 fun erlang:byte_size/1};
	    false ->
		{fun (Bin) when is_binary(Bin) -> binary_to_list(Bin);
		     (List) when is_list(List) -> List
		 end, 
		 fun erlang:length/1}
	end,
    Pos = 4711,
    Data = Conv("THE QUICK BROWN FOX JUMPS OVER A LAZY DOG"),
    Size = Sizeof(Data),
    Init = Conv("                 "),
    SizeInit = Sizeof(Init),
    Head = Conv(<<Size:32/big-unsigned, Pos:32/big-unsigned>>),
    Filler = Conv(bytes($ , Pos-SizeInit-Sizeof(Head))),
    Size1 = Size+1,
    SizePos = Size+Pos,
    %%
    {ok, FD} = ?FILE_MODULE:open(Name, [write, read | ModeList]),
    ok = ?FILE_MODULE:truncate(FD),
    ok = ?FILE_MODULE:write(FD, Init),
    ok = ?FILE_MODULE:write(FD, Head),
    ok = ?FILE_MODULE:write(FD, Filler),
    ok = ?FILE_MODULE:write(FD, Data),
    %% Correct read
    {ok, {Size, Pos, Data}} =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, infinity),
    %% Invalid header - size > max
    eof =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size-1),
    %% Data block protudes over eof
    ok =
	?FILE_MODULE:pwrite(FD, SizeInit, 
			    <<Size1:32/big-unsigned, 
			      Pos:32/big-unsigned>>),
    {ok, {Size1, Pos, Data}} =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size1),
    %% Data block outside file
    ok =
	?FILE_MODULE:pwrite(FD, SizeInit, 
			    <<Size:32/big-unsigned, 
			      SizePos:32/big-unsigned>>),
    {ok, {Size, SizePos, eof}} =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size),
    %% Zero size
    ok =
	?FILE_MODULE:pwrite(FD, SizeInit, 
			    <<0:32/big-unsigned, 
			      Pos:32/big-unsigned>>),
    {ok, {0, Pos, eof}} =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, SizeInit, Size),
    %% Invalid header - protudes over eof
    eof =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, 
					Pos+Size-(Sizeof(Head)-1), 
					infinity),
    %% Header not even in file
    eof =
	?FILE_MODULE:ipread_s32bu_p32bu(FD, Pos+Size, infinity),
    %%
    ok = ?FILE_MODULE:close(FD),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests interleaved read and writes.
interleaved_read_write(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File =
	filename:join(Dir, ?MODULE_STRING++"interleaved_read_write.txt"),
    {ok,F1} = ?FILE_MODULE:open(File, [write]),
    ok = ?FILE_MODULE:write(F1, "data---r1."), % 10 chars each
    ok = ?FILE_MODULE:write(F1, "data---r2."),
    ok = ?FILE_MODULE:write(F1, "data---r3."),
    ok = ?FILE_MODULE:close(F1),
    {ok,F2} = ?FILE_MODULE:open(File, [read, write]),
    {ok, "data---r1."} = ?FILE_MODULE:read(F2, 10),
    ok = ?FILE_MODULE:write(F2, "data---w2."),
    ok = ?FILE_MODULE:close(F2),
    {ok,F3} = ?FILE_MODULE:open(File, [read]),
    {ok, "data---r1."} = ?FILE_MODULE:read(F3, 10),
    {ok, "data---w2."} = ?FILE_MODULE:read(F3, 10),
    {ok, "data---r3."} = ?FILE_MODULE:read(F3, 10),
    eof = ?FILE_MODULE:read(F3, 1),
    ok = ?FILE_MODULE:close(F2),
    %%
    [] = flush(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OTP-5814. eval/consult/script return correct line numbers.
otp_5814(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    File = filename:join(PrivDir, "otp_5814"),
    Path = [PrivDir],
    ok = file:write_file(File, <<"{a,b,c}.
                                        a.
b.
c.
{d,e,
 [}.">>),
    {error, {6,erl_parse,_}} = file:eval(File),
 {error, {6,erl_parse,_}} = file:consult(File),
 {error, {6,erl_parse,_}} = file:path_consult(Path, File),
 {error, {6,erl_parse,_}} = file:path_eval(Path, File),
 {error, {6,erl_parse,_}} = file:script(File),
 {error, {6,erl_parse,_}} = file:path_script(Path, File),

 ok = file:write_file(File, <<>>),
 {error, {1,file,undefined_script}} = file:path_script(Path, File),

    %% The error is not propagated...
 ok = file:write_file(File, <<"a.
                                        b.
1/0.">>),
    {error, {3, file, {error, badarith, _}}} = file:eval(File),

ok = file:write_file(File, <<"erlang:raise(throw, apa, []).">>),
{error, {1, file, {throw, apa, _}}} = file:eval(File),

file:delete(File),
ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OTP-10852. +fnu and latin1 filenames.
otp_10852(Config) when is_list(Config) ->
    Node = start_node(erl_pp_helper, "+fnu"),
    Dir = proplists:get_value(priv_dir, Config),
    B = filename:join(Dir, <<"\xE4">>),
    ok = rpc_call(Node, get_cwd, [B]),
    {error, no_translation} = rpc_call(Node, set_cwd, [B]),
    ok = rpc_call(Node, delete, [B]),
    ok = rpc_call(Node, rename, [B, B]),
    ok = rpc_call(Node, read_file_info, [B]),
    ok = rpc_call(Node, read_link_info, [B]),
    ok = rpc_call(Node, read_link, [B]),
    ok = rpc_call(Node, write_file_info, [B,#file_info{}]),
    ok = rpc_call(Node, list_dir, [B]),
    ok = rpc_call(Node, list_dir_all, [B]),
    ok = rpc_call(Node, read_file, [B]),
    ok = rpc_call(Node, make_link, [B,B]),
    case rpc_call(Node, make_symlink, [B,B]) of
        {error, eilseq} ->
            %% Some versions of OS X refuse to create files with illegal names.
            {unix,darwin} = os:type();
        {error, eperm} ->
            %% The test user might not have permission to create symlinks.
            {win32,_} = os:type();
        ok ->
            ok
    end,
    ok = rpc_call(Node, delete, [B]),
    case rpc_call(Node, make_dir, [B]) of
        {error, eilseq} ->
            {unix,darwin} = os:type();
        ok ->
            ok
    end,
    ok = rpc_call(Node, del_dir, [B]),
    case rpc_call(Node, write_file, [B,B]) of
        {error, eilseq} ->
            {unix,darwin} = os:type();
        ok ->
            {ok, Fd} = rpc_call(Node, open, [B,[read]]),
            ok = rpc_call(Node, close, [Fd]),
            {ok,0} = rpc_call(Node, copy, [B,B]),
            {ok, Fd2, B} = rpc_call(Node, path_open, [["."], B, [read]]),
            ok = rpc_call(Node, close, [Fd2])
    end,
    true = test_server:stop_node(Node),
    ok.

rpc_call(N, F, As) ->
    case rpc:call(N, ?FILE_MODULE, F, As) of
        {error, enotsup} -> ok;
        {error, enoent} -> ok;
        {error, badarg} -> ok;
        Else -> Else
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

large_file() ->
    [{timetrap,{minutes,20}}].

%% Tests positioning in large files (> 4G).
large_file(Config) when is_list(Config) ->
    run_large_file_test(Config,
			fun(Name) -> do_large_file(Name) end,
			"_large_file").

do_large_file(Name) ->
    S = "1234567890",
    L = length(S),
    R = lists:reverse(S),
    P = 1 bsl 32,
    Ss = lists:sort(S),
    Rs = lists:reverse(Ss),
    {ok,F}  = ?FILE_MODULE:open(Name, [raw,read,write]),
    ok      = ?FILE_MODULE:write(F, S),
    {ok,P}  = ?FILE_MODULE:position(F, P),
    ok      = ?FILE_MODULE:write(F, R),
    {ok,0}  = ?FILE_MODULE:position(F, bof),
    {ok,S}  = ?FILE_MODULE:read(F, L),
    {ok,P}  = ?FILE_MODULE:position(F, {eof,-L}),
    {ok,R}  = ?FILE_MODULE:read(F, L+1),
    {ok,S}  = ?FILE_MODULE:pread(F, 0, L),
    {ok,R}  = ?FILE_MODULE:pread(F, P, L+1),
    ok      = ?FILE_MODULE:pwrite(F, 0, Ss),
    ok      = ?FILE_MODULE:pwrite(F, P, Rs),
    {ok,0}  = ?FILE_MODULE:position(F, bof),
    {ok,Ss} = ?FILE_MODULE:read(F, L),
    {ok,P}  = ?FILE_MODULE:position(F, {eof,-L}),
    {ok,Rs} = ?FILE_MODULE:read(F, L+1),
    ok      = ?FILE_MODULE:close(F),
    %% Reopen the file with 'append'; used to fail on Windows causing
    %% writes to go to the beginning of the file for files > 4GB.
    PL = P + L,
    PLL = PL + L,
    {ok,F1}  = ?FILE_MODULE:open(Name, [raw,read,write,append]),
    ok       = ?FILE_MODULE:write(F1, R),
    {ok,PLL} = ?FILE_MODULE:position(F1, {cur,0}),
    {ok,Rs}  = ?FILE_MODULE:pread(F1, P, L),
    {ok,PL}  = ?FILE_MODULE:position(F1, {eof,-L}),
    {ok,R}   = ?FILE_MODULE:read(F1, L+1),
    ok       = ?FILE_MODULE:close(F1),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

large_write() ->
    [{timetrap,{minutes,20}}].

large_write(Config) when is_list(Config) ->
    run_large_file_test(Config,
			fun(Name) -> do_large_write(Name) end,
			"_large_write").

do_large_write(Name) ->
    Memsize = memsize(),
    io:format("Memsize = ~w Bytes~n", [Memsize]),
    case {erlang:system_info(wordsize),Memsize} of
	{4,_} ->
	    {skip,"Needs a 64-bit emulator"};
	{8,N} when N < 6 bsl 30 ->
	    {skip,
	     "This machine has < 6 GB  memory: "
	     ++integer_to_list(N)};
	{8,_} ->
	    Size = 4*1024*1024*1024+1,
	    Bin = <<0:Size/unit:8>>,
	    ok = file:write_file(Name, Bin),
	    {ok,#file_info{size=Size}} = file:read_file_info(Name),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Benchmarks
%%
%% Note that we only measure the time it takes to run the isolated file
%% operations and that the actual test runtime can differ significantly,
%% especially on the write side as the files need to be truncated before
%% writing.

large_writes(Config) when is_list(Config) ->
    Modes = [raw, binary],
    OpCount = 4096,
    Data = <<0:(64 bsl 10)/unit:8>>,
    run_write_benchmark(Config, Modes, OpCount, Data).

large_writes_delayed(Config) when is_list(Config) ->
    %% Each write is exactly as large as the delay buffer, causing the writes
    %% to pass through each time, giving us a decent idea of how much overhead
    %% delayed_write adds.
    Modes = [raw, binary, {delayed_write, 64 bsl 10, 2000}],
    OpCount = 4096,
    Data = <<0:(64 bsl 10)/unit:8>>,
    run_write_benchmark(Config, Modes, OpCount, Data).

tiny_writes(Config) when is_list(Config) ->
    Modes = [raw, binary],
    OpCount = 512 bsl 10,
    Data = <<0>>,
    run_write_benchmark(Config, Modes, OpCount, Data).

tiny_writes_delayed(Config) when is_list(Config) ->
    Modes = [raw, binary, {delayed_write, 512 bsl 10, 2000}],
    OpCount = 512 bsl 10,
    Data = <<0>>,
    run_write_benchmark(Config, Modes, OpCount, Data).

%% The read benchmarks assume that "benchmark_scratch_file" has been filled by
%% the write benchmarks.

tiny_reads(Config) when is_list(Config) ->
    Modes = [raw, binary],
    OpCount = 512 bsl 10,
    run_read_benchmark(Config, Modes, OpCount, 1).

tiny_reads_ahead(Config) when is_list(Config) ->
    Modes = [raw, binary, {read_ahead, 512 bsl 10}],
    OpCount = 512 bsl 10,
    run_read_benchmark(Config, Modes, OpCount, 1).

run_write_benchmark(Config, Modes, OpCount, Data) ->
    run_benchmark(Config, [write | Modes], OpCount, fun file:write/2, Data).

run_read_benchmark(Config, Modes, OpCount, OpSize) ->
    run_benchmark(Config, [read | Modes], OpCount, fun file:read/2, OpSize).

run_benchmark(Config, Modes, OpCount, Fun, Arg) ->
    ScratchDir = proplists:get_value(priv_dir, Config),
    Path = filename:join(ScratchDir, "benchmark_scratch_file"),
    {ok, Fd} = file:open(Path, Modes),
    submit_throughput_results(Fun, [Fd, Arg], OpCount).

submit_throughput_results(Fun, Args, Times) ->
    MSecs = measure_repeated_file_op(Fun, Args, Times, millisecond),
    IOPS = trunc(Times * (1000 / MSecs)),
    ct_event:notify(#event{ name = benchmark_data, data = [{value,IOPS}] }),
    {comment, io_lib:format("~p IOPS, ~p ms", [IOPS, trunc(MSecs)])}.

measure_repeated_file_op(Fun, Args, Times, Unit) ->
    Start = os:perf_counter(Unit),
    repeated_apply(Fun, Args, Times),
    os:perf_counter(Unit) - Start.

repeated_apply(_F, _Args, Times) when Times =< 0 ->
    ok;
repeated_apply(F, Args, Times) ->
    erlang:apply(F, Args),
    repeated_apply(F, Args, Times - 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


response_analysis(Module, Function, Arguments) ->
    Parent = self(),
    erlang:yield(), % Schedule out before test
    Child =
	spawn_link(
	  fun () ->
		  receive {Parent, start, Ts} -> ok end,
		  Stat = 
		      iterate(response_stat(response_stat(init, Ts),
					    micro_ts()),
			      done,
			      fun (S) ->
				      erlang:yield(),
				      receive
					  {Parent, stop} ->
					      done
				      after 0 ->
					      response_stat(S, micro_ts())
				      end
			      end),
		  Parent ! {self(), stopped, response_stat(Stat, micro_ts())}
	  end),
    Child ! {Parent, start, micro_ts()},
    Result = apply(Module, Function, Arguments),
    Child ! {Parent, stop},
    {N, Sum, _, M, Max} = receive {Child, stopped, X} -> X end,
    Mean_ms = (0.001*Sum) / (N-1),
    Max_ms = 0.001 * Max,
    Comment =
	lists:flatten(
	  io_lib:format(
	    "Scheduling interval: Mean = ~.3f ms, "
	    ++"Max = ~.3f ms for no ~p of ~p.~n",
	    [Mean_ms, Max_ms, M, (N-1)])),
    {Result, Comment}.

micro_ts() ->
    erlang:monotonic_time(microsecond).

response_stat(init, Ts) ->
    {0, 0, Ts, 0, 0};
response_stat({N, Sum, Ts0, M, Max}, Ts) ->
    D = Ts - Ts0,
    if D > Max ->
	    {N+1, Sum+D, Ts, N, D};
       true ->
	    {N+1, Sum+D, Ts, M, Max}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% This function is kept just for benchmarking reasons.
%% create_file/2 below is some 44 times faster.

create_file_slow(Name, N) when is_integer(N), N >= 0 ->
    {ok, FD} =
	?FILE_MODULE:open(Name, [raw, write, delayed_write, binary]),
    ok = create_file_slow(FD, 0, N),
    ok = ?FILE_MODULE:close(FD),
    ok.

create_file_slow(_FD, M, M) ->
    ok;
create_file_slow(FD, M, N) ->
    ok = ?FILE_MODULE:write(FD, <<M:32/unsigned>>),
    create_file_slow(FD, M+1, N).



%% Creates a file 'Name' containing 'N' unsigned 32 bit integers 
%% from 0 to N-1.

create_file(Name, N) when is_integer(N), N >= 0 ->
    {ok, FD} =
	?FILE_MODULE:open(Name, [raw, write, delayed_write, binary]),
    ok = create_file(FD, 0, N),
    ok = ?FILE_MODULE:close(FD),
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

%% read_line with ?PRIM_FILE.
read_line_1(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    All = read_line_testdata(PrivDir),
    read_line_create_files(All),
    [ begin
	  io:format("read_line_all: ~s~n",[File]),
	  {X,_} = read_line_all(File),
	  true
      end || {_,File,X,_} <- All ],
    [ begin
	  io:format("read_line_all_alternating: ~s~n",[File]),
	  {Y,_} = read_line_all_alternating(File),
	  true
      end || {_,File,_,Y} <- All , Y =/= fail],
    [ begin
	  io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
	  {'EXIT',_} = (catch read_line_all_alternating(File)),
	  true
      end || {_,File,_,Y} <- All , Y =:= fail],
    read_line_remove_files(All),
    ok.
%% read_line with file.
read_line_2(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    All = read_line_testdata(PrivDir),
    read_line_create_files(All),
    [ begin
	  io:format("read_line_all: ~s~n",[File]),
	  {X,_} = read_line_all2(File),
	  true
      end || {_,File,X,_} <- All ],
    [ begin
	  io:format("read_line_all_alternating: ~s~n",[File]),
	  {Y,_} = read_line_all_alternating2(File),
	  true
      end || {_,File,_,Y} <- All , Y =/= fail],
    [ begin
	  io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
	  {'EXIT',_} = (catch read_line_all_alternating2(File)),
	  true
      end || {_,File,_,Y} <- All , Y =:= fail],
    read_line_remove_files(All),
    ok.
%% read_line with raw file.
read_line_3(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    All = read_line_testdata(PrivDir),
    read_line_create_files(All),
    [ begin
	  io:format("read_line_all: ~s~n",[File]),
	  {X,_} = read_line_all3(File),
	  true
      end || {_,File,X,_} <- All ],
    [ begin
	  io:format("read_line_all_alternating: ~s~n",[File]),
	  {Y,_} = read_line_all_alternating3(File),
	  true
      end || {_,File,_,Y} <- All , Y =/= fail],
    [ begin
	  io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
	  {'EXIT',_} = (catch read_line_all_alternating3(File)),
	  true
      end || {_,File,_,Y} <- All , Y =:= fail],
    read_line_remove_files(All),
    ok.
%% read_line with raw buffered file.
read_line_4(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    All = read_line_testdata(PrivDir),
    read_line_create_files(All),
    [ begin
	  io:format("read_line_all: ~s~n",[File]),
	  {X,_} = read_line_all4(File),
	  true
      end || {_,File,X,_} <- All ],
    [ begin
	  io:format("read_line_all_alternating: ~s~n",[File]),
	  {Y,_} = read_line_all_alternating4(File),
	  true
      end || {_,File,_,Y} <- All , Y =/= fail],
    [ begin
	  io:format("read_line_all_alternating (failing as should): ~s~n",[File]),
	  {'EXIT',_} = (catch read_line_all_alternating4(File)),
	  true
      end || {_,File,_,Y} <- All , Y =:= fail],
    read_line_remove_files(All),
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
    {ok,F} = ?PRIM_FILE:open(Filename,[read,binary]),
    X=read_rl_lines(F),
    ?PRIM_FILE:close(F),
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
    case ?PRIM_FILE:read_line(F) of
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
    {ok,F} = ?PRIM_FILE:open(Filename,[read,binary]),
    X=read_rl_lines(F,true),
    ?PRIM_FILE:close(F),
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
		 true -> ?PRIM_FILE:read(F,1);
		 false -> ?PRIM_FILE:read_line(F)
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
            case disc_free(proplists:get_value(priv_dir, Config)) of
                error ->
                    {skip, "Failed to query disk space for priv_dir. "
                           "Is it on a remote file system?~n"};
                N when N >= 5 * (1 bsl 20) ->
                    ct:pal("Free disk: ~w KByte~n", [N]),
                    do_run_large_file_test(Config, Run, Name);
                N when N < 5 * (1 bsl 20) ->
                    ct:pal("Free disk: ~w KByte~n", [N]),
                    {skip,"Less than 5 GByte free"}
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
		  ?FILE_MODULE:delete(Name)
	  end),

    %% Run the test case.
    Res = Run(Name),

    %% Delete file and finish deleter process.
    Mref = erlang:monitor(process, Deleter),
    Deleter ! {Tester,done},
    receive {'DOWN',Mref,_,_,_} -> ok end,

    Res.

disc_free(Path) ->
    Data = disksup:get_disk_data(),

    %% What partitions could Data be mounted on?
    Partitions =
        [D || {P, _Tot, _Perc}=D <- Data,
         lists:prefix(filename:nativename(P), filename:nativename(Path))],

    %% Sorting in descending order places the partition with the most specific
    %% path first.
    case lists:sort(fun erlang:'>='/2, Partitions) of
        [{_,Tot, Perc} | _] -> round(Tot * (1-(Perc/100)));
        [] -> error
    end.

memsize() ->
    {Tot,_Used,_}  = memsup:get_memory_data(),
    Tot.

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
