%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
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

-module(ram_file_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 %% init/1, fini/1,
	 init_per_testcase/2, end_per_testcase/2]).
-export([open_modes/1, open_old_modes/1, pread_pwrite/1, position/1,
	 truncate/1, sync/1, get_set_file/1, compress/1, uuencode/1,
	 large_file_errors/1, large_file_light/1, large_file_heavy/1]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

-define(FILE_MODULE, file).         % Name of module to test
-define(RAM_FILE_MODULE, ram_file). % Name of module to test

%%--------------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [open_modes, open_old_modes, pread_pwrite, position,
     truncate, sync, get_set_file, compress, uuencode,
     large_file_errors, large_file_light, large_file_heavy].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Time = 
	case Func of
	    large_file_heavy -> 
		?t:minutes(5);
	    _ ->
		?t:seconds(10)
	end,
    Dog = ?t:timetrap(Time),
    %% error_logger:info_msg("~p:~p *****~n", [?MODULE, Func]),
    [{watchdog, Dog} | Config].

end_per_testcase(_Func, Config) ->
    %% error_logger:info_msg("~p:~p END *****~n", [?MODULE, Func]),
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

%%--------------------------------------------------------------------------
%% Test suites

open_modes(suite) ->
    [];
open_modes(doc) ->
    ["Test that the basic read, write and binary options works for open/2."];
open_modes(Config) when is_list(Config) ->
    ?line Str1 = "The quick brown fox ",
    ?line Str2 = "jumps over a lazy dog ",
    ?line Str  = Str1 ++ Str2,
    ?line Bin1 = list_to_binary(Str1),
    ?line Bin2 = list_to_binary(Str2),
    ?line Bin  = list_to_binary(Str),
    %%
    open_read_write(?FILE_MODULE, Str1, [ram, read, write], Str2),
    open_read(?FILE_MODULE, Str, [ram]),
    open_read_write(?FILE_MODULE, Bin1, [ram, binary, read, write], Bin2),
    open_read(?FILE_MODULE, Bin, [ram, binary, read]),
    %%
    ok.

open_old_modes(suite) ->
    [];
open_old_modes(doc) ->
    ["Test that the old style read, write and binary options ", 
     "works for open/2."];
open_old_modes(Config) when is_list(Config) ->
    ?line Str1 = "The quick brown fox ",
    ?line Str2 = "jumps over a lazy dog ",
    ?line Str  = Str1 ++ Str2,
    ?line Bin1 = list_to_binary(Str1),
    ?line Bin2 = list_to_binary(Str2),
    ?line Bin  = list_to_binary(Str),
    %%
    open_read_write(?RAM_FILE_MODULE, Str1, read_write, Str2),
    open_read(?RAM_FILE_MODULE, Str, read),
    open_read_write(?RAM_FILE_MODULE, Bin1, {binary, read_write}, Bin2),
    open_read(?RAM_FILE_MODULE, Bin, {binary, read}),
    %%
    ok.

open_read_write(Module, Data1, Options, Data2) ->
    ?line io:format("~p:open_read_write(~p, ~p, ~p, ~p)~n", 
		    [?MODULE, Module, Data1, Options, Data2]),
    %%
    ?line Size1 = sizeof(Data1),
    ?line Size2 = sizeof(Data2),
    ?line Data  = append(Data1, Data2),
    ?line Size  = Size1 + Size2,
    %%
    ?line {ok, Fd}    = Module:open(Data1, Options),
    ?line {ok, Data1} = Module:read(Fd, Size1),
    ?line eof         = Module:read(Fd, 1),
    ?line {ok, Zero}  = Module:read(Fd, 0),
    ?line 0           = sizeof(Zero),
    ?line ok          = Module:write(Fd, Data2),
    ?line {ok, 0}     = Module:position(Fd, bof),
    ?line {ok, Data}  = Module:read(Fd, Size),
    ?line eof         = Module:read(Fd, 1),
    ?line {ok, Zero}  = Module:read(Fd, 0),
    ?line ok          = Module:close(Fd),
    %%
    ?line ok.

open_read(Module, Data, Options) ->
    ?line io:format("~p:open_read(~p, ~p, ~p)~n", 
		    [?MODULE, Module, Data, Options]),
    %%
    ?line Size = sizeof(Data),
    %%
    ?line {ok, Fd}         = Module:open(Data, Options),
    ?line {ok, Data}       = Module:read(Fd, Size),
    ?line eof              = Module:read(Fd, 1),
    ?line {ok, Zero}       = Module:read(Fd, 0),
    ?line 0                = sizeof(Zero),
    ?line {error, ebadf}   = Module:write(Fd, Data),
    ?line {ok, 0}          = Module:position(Fd, bof),
    ?line {ok, Data}       = Module:read(Fd, Size),
    ?line eof              = Module:read(Fd, 1),
    ?line {ok, Zero}       = Module:read(Fd, 0),
    ?line ok               = Module:close(Fd),
    %%
    ?line ok.



pread_pwrite(suite) ->
    [];
pread_pwrite(doc) ->
    ["Test that pread/2,3 and pwrite/2,3 works."];
pread_pwrite(Config) when is_list(Config) ->
    ?line Str = "Flygande bäckaziner söka hwila på mjuqa tuvor x",
    ?line Bin = list_to_binary(Str),
    %%
    pread_pwrite_test(?FILE_MODULE, Str, [ram, read, write]),
    pread_pwrite_test(?FILE_MODULE, Bin, [ram, binary, read, write]),
    pread_pwrite_test(?RAM_FILE_MODULE, Str, [read, write]),
    pread_pwrite_test(?RAM_FILE_MODULE, Bin, {binary, read_write}),
    %%
    ok.

pread_pwrite_test(Module, Data, Options) ->
    ?line io:format("~p:pread_pwrite_test(~p, ~p, ~p)~n", 
		    [?MODULE, Module, Data, Options]),
    %%
    ?line Size = sizeof(Data),
    %%
    ?line {ok, Fd}         = Module:open([], Options),
    ?line ok               = Module:pwrite(Fd, 0, Data),
    ?line {ok, Data}       = Module:pread(Fd, 0, Size+1),
    ?line eof              = Module:pread(Fd, Size+1, 1),
    ?line {ok, Zero}       = Module:pread(Fd, Size+1, 0),
    ?line 0                = sizeof(Zero),
    ?line ok               = Module:pwrite(Fd, [{0, Data}, {Size+17, Data}]),
    ?line {ok, [Data, 
		eof, 
		Data,
		Zero]}     = Module:pread(Fd, [{Size+17, Size+1},
					       {2*Size+17+1, 1},
					       {0, Size},
					       {2*Size+17+1, 0}]),
    ?line ok               = Module:close(Fd),
    %%
    ?line ok.

position(suite) ->
    [];
position(doc) ->
    ["Test that position/2 works."];
position(Config) when is_list(Config) ->
    ?line Str = "Att vara eller icke vara, det är frågan. ",
    ?line Bin = list_to_binary(Str),
    %%
    position_test(?FILE_MODULE, Str, [ram, read]),
    position_test(?FILE_MODULE, Bin, [ram, binary]),
    position_test(?RAM_FILE_MODULE, Str, [read]),
    position_test(?RAM_FILE_MODULE, Bin, {binary, read}),
    %%
    ok.

position_test(Module, Data, Options) ->
    ?line io:format("~p:position_test(~p, ~p, ~p)~n", 
		    [?MODULE, Module, Data, Options]),
    %%
    ?line Size = sizeof(Data),
    ?line Size_7 = Size+7,
    %%
    ?line Slice_0_2 = slice(Data, 0, 2),
    ?line Slice_0_3 = slice(Data, 0, 3),
    ?line Slice_2_5 = slice(Data, 2, 5),
    ?line Slice_3_4 = slice(Data, 3, 4),
    ?line Slice_5   = slice(Data, 5, Size),
    %%
    ?line {ok, Fd}          = Module:open(Data, Options),
    %%
    ?line io:format("CUR positions"),
    ?line {ok, Slice_0_2}   = Module:read(Fd, 2),
    ?line {ok, 2}           = Module:position(Fd, cur),
    ?line {ok, Slice_2_5}   = Module:read(Fd, 5),
    ?line {ok, 3}           = Module:position(Fd, {cur, -4}),
    ?line {ok, Slice_3_4}   = Module:read(Fd, 4),
    ?line {ok, 0}           = Module:position(Fd, {cur, -7}),
    ?line {ok, Slice_0_3}   = Module:read(Fd, 3),
    ?line {ok, 0}           = Module:position(Fd, {cur, -3}),
    ?line {error, einval}   = Module:position(Fd, {cur, -1}),
    ?line {ok, 0}           = Module:position(Fd, 0),
    ?line {ok, 2}           = Module:position(Fd, {cur, 2}),
    ?line {ok, Slice_2_5}   = Module:read(Fd, 5),
    ?line {ok, Size_7}      = Module:position(Fd, {cur, Size}),
    ?line {ok, Zero}        = Module:read(Fd, 0),
    ?line 0                 = sizeof(Zero),
    ?line eof               = Module:read(Fd, 1),
    %%
    ?line io:format("Absolute and BOF positions"),
    ?line {ok, Size}        = Module:position(Fd, Size),
    ?line eof               = Module:read(Fd, 1),
    ?line {ok, 5}           = Module:position(Fd, 5),
    ?line {ok, Slice_5}     = Module:read(Fd, Size),
    ?line {ok, 2}           = Module:position(Fd, {bof, 2}),
    ?line {ok, Slice_2_5}   = Module:read(Fd, 5),
    ?line {ok, 3}           = Module:position(Fd, 3),
    ?line {ok, Slice_3_4}   = Module:read(Fd, 4),
    ?line {ok, 0}           = Module:position(Fd, bof),
    ?line {ok, Slice_0_2}   = Module:read(Fd, 2),
    ?line {ok, Size_7}      = Module:position(Fd, {bof, Size_7}),
    ?line {ok, Zero}        = Module:read(Fd, 0),
    %%
    ?line io:format("EOF positions"),
    ?line {ok, Size}        = Module:position(Fd, eof),
    ?line eof               = Module:read(Fd, 1),
    ?line {ok, 5}           = Module:position(Fd, {eof, -Size+5}),
    ?line {ok, Slice_5}     = Module:read(Fd, Size),
    ?line {ok, 2}           = Module:position(Fd, {eof, -Size+2}),
    ?line {ok, Slice_2_5}   = Module:read(Fd, 5),
    ?line {ok, 3}           = Module:position(Fd, {eof, -Size+3}),
    ?line {ok, Slice_3_4}   = Module:read(Fd, 4),
    ?line {ok, 0}           = Module:position(Fd, {eof, -Size}),
    ?line {ok, Slice_0_2}   = Module:read(Fd, 2),
    ?line {ok, Size_7}      = Module:position(Fd, {eof, 7}),
    ?line {ok, Zero}        = Module:read(Fd, 0),
    ?line eof               = Module:read(Fd, 1),
    %%
    ?line ok.



truncate(suite) ->
    [];
truncate(doc) ->
    ["Test that truncate/1 works."];
truncate(Config) when is_list(Config) ->
    ?line Str = "Mån ädlare att lida och fördraga "
	++ "ett bittert ödes stygn av pilar, ",
    ?line Bin = list_to_binary(Str),
    %%
    ok = truncate_test(?FILE_MODULE, Str, [ram, read, write]),
    ok = truncate_test(?FILE_MODULE, Bin, [ram, binary, read, write]),
    ok = truncate_test(?RAM_FILE_MODULE, Str, read_write),
    ok = truncate_test(?RAM_FILE_MODULE, Bin, [binary, read, write]),
    %%
    {error, eacces} = truncate_test(?FILE_MODULE, Str, [ram]),
    {error, eacces} = truncate_test(?FILE_MODULE, Bin, [ram, binary, read]),
    {error, eacces} = truncate_test(?RAM_FILE_MODULE, Str, read),
    {error, eacces} = truncate_test(?RAM_FILE_MODULE, Bin, {binary, read}),
    %%
    ok.

truncate_test(Module, Data, Options) ->
    ?line io:format("~p:truncate_test(~p, ~p, ~p)~n", 
		    [?MODULE, Module, Data, Options]),
    %%
    ?line Size = sizeof(Data),
    ?line Size1 = Size-2,
    ?line Data1 = slice(Data, 0, Size1),
    %%
    ?line {ok, Fd}    = Module:open(Data, Options),
    ?line {ok, Size1} = Module:position(Fd, Size1),
    ?line case Module:truncate(Fd) of
	      ok ->
		  ?line {ok, 0}     = Module:position(Fd, 0),
		  ?line {ok, Data1} = Module:read(Fd, Size),
		  ?line ok          = Module:close(Fd),
		  ?line ok;
	      Error ->
		  ?line ok      = Module:close(Fd),
		  ?line Error
	  end.



sync(suite) ->
    [];
sync(doc) ->
    ["Test that sync/1 at least does not crash."];
sync(Config) when is_list(Config) ->
    ?line Str = "än att ta till vapen mot ett hav av kval. ",
    ?line Bin = list_to_binary(Str),
    %%
    sync_test(?FILE_MODULE, Str, [ram, read, write]),
    sync_test(?FILE_MODULE, Bin, [ram, binary, read, write]),
    sync_test(?RAM_FILE_MODULE, Str, read_write),
    sync_test(?RAM_FILE_MODULE, Bin, [binary, read, write]),
    %%
    sync_test(?FILE_MODULE, Str, [ram]),
    sync_test(?FILE_MODULE, Bin, [ram, binary, read]),
    sync_test(?RAM_FILE_MODULE, Str, read),
    sync_test(?RAM_FILE_MODULE, Bin, {binary, read}),
    %%
    ok.

sync_test(Module, Data, Options) ->
    ?line io:format("~p:sync_test(~p, ~p, ~p)~n", 
		    [?MODULE, Module, Data, Options]),
    %%
    ?line Size = sizeof(Data),
    %%
    ?line {ok, Fd}    = Module:open(Data, Options),
    ?line ok          = Module:sync(Fd),
    ?line {ok, Data}  = Module:read(Fd, Size+1),
    ?line ok.



get_set_file(suite) ->
    [];
get_set_file(doc) ->
    ["Tests get_file/1, set_file/2, get_file_close/1 and get_size/1."];
get_set_file(Config) when is_list(Config) ->
    %% These two strings should not be of equal length.
    ?line Str  = "När högan nord blir snöbetäckt, ",
    ?line Str2 = "får alla harar byta dräkt. ",
    ?line Bin  = list_to_binary(Str),
    ?line Bin2 = list_to_binary(Str2),
    %%
    ok = get_set_file_test(Str, read_write, Str2),
    ok = get_set_file_test(Bin, [binary, read, write], Bin2),
    ok = get_set_file_test(Str, read, Str2),
    ok = get_set_file_test(Bin, [binary, read], Bin2),
    %%
    ok.

get_set_file_test(Data, Options, Data2) ->
    ?line io:format("~p:get_set_file_test(~p, ~p, ~p)~n", 
		    [?MODULE, Data, Options, Data2]),
    %%
    ?line Size  = sizeof(Data),
    ?line Size2 = sizeof(Data2),
    %%
    ?line {ok, Fd}        = ?RAM_FILE_MODULE:open(Data, Options),
    ?line {ok, Size}      = ?RAM_FILE_MODULE:get_size(Fd),
    ?line {ok, Data}      = ?RAM_FILE_MODULE:get_file(Fd),
    ?line {ok, Data}      = ?RAM_FILE_MODULE:get_file_close(Fd),
    ?line {error, einval} = ?RAM_FILE_MODULE:get_size(Fd),
    ?line {ok, Fd2}       = ?RAM_FILE_MODULE:open(Data, Options),
    ?line case ?RAM_FILE_MODULE:set_file(Fd2, Data2) of
	      {ok, Size2} ->
		  ?line {ok, Size2} = ?RAM_FILE_MODULE:get_size(Fd2),
		  ?line {ok, Data2} = ?RAM_FILE_MODULE:get_file(Fd2),
		  ?line {ok, Data2} = ?RAM_FILE_MODULE:get_file_close(Fd2),
		  ?line ok;
	      {error, _} = Error ->
		  ?line {ok, Data}  = ?RAM_FILE_MODULE:get_file_close(Fd2),
		  ?line Error
	  end.
    

    
compress(suite) ->
    [];
compress(doc) ->
    ["Test that compress/1 and uncompress/1 works."];
compress(Config) when is_list(Config) ->
    ?line Data   = ?config(data_dir, Config),
    ?line Real   = filename:join(Data, "realmen.html"),
    ?line RealGz = filename:join(Data, "realmen.html.gz"),
    %%
    %% Uncompress test
    %%
    ?line {ok, FdReal}   = ?FILE_MODULE:open(Real, []),
    ?line {ok, Fd}       = ?FILE_MODULE:open([], [ram, read, write]),
    ?line {ok, FdRealGz} = ?FILE_MODULE:open(RealGz, []),
    %%
    ?line {ok, SzGz}     = ?FILE_MODULE:copy(FdRealGz, Fd),
    ?line {ok, Sz}       = ?RAM_FILE_MODULE:uncompress(Fd),
    ?line {ok, 0}        = ?FILE_MODULE:position(Fd, bof),
    ?line true           = compare(FdReal, Fd),
    %%
    ?line true           = (SzGz =< Sz),
    %%
    %% Compress and uncompress test
    %%
    ?line {ok, 0}        = ?FILE_MODULE:position(FdReal, bof),
    ?line {ok, 0}        = ?FILE_MODULE:position(Fd, bof),
    ?line ok             = ?FILE_MODULE:truncate(Fd),
    ?line {ok, Sz}       = ?FILE_MODULE:copy(FdReal, Fd),
    ?line {ok, SzGz}     = ?RAM_FILE_MODULE:compress(Fd),
    ?line {ok, Sz}       = ?RAM_FILE_MODULE:uncompress(Fd),
    ?line {ok, 0}        = ?FILE_MODULE:position(Fd, bof),
    ?line {ok, 0}        = ?FILE_MODULE:position(FdReal, bof),
    ?line true           = compare(FdReal, Fd),
    %%
    ?line ok             = ?FILE_MODULE:close(FdReal),
    ?line ok             = ?FILE_MODULE:close(Fd),
    ?line ok             = ?FILE_MODULE:close(FdRealGz),


    %% Test uncompressing data that will be expanded many times.
    ?line Huge = iolist_to_binary(mk_42(18)),
    ?line HugeSize = byte_size(Huge),
    ?line HugeGz = zlib:gzip(Huge),

    ?line {ok,HugeFd} = ?FILE_MODULE:open([], [ram,read,write,binary]),
    ?line ok = ?FILE_MODULE:write(HugeFd, HugeGz),
    ?line {ok,HugeSize} = ?RAM_FILE_MODULE:uncompress(HugeFd),
    ?line {ok,0} = ?FILE_MODULE:position(HugeFd, bof),
    ?line {ok,Huge} = ?FILE_MODULE:read(HugeFd, HugeSize),

    %% Uncompressing again should do nothing.
    ?line {ok,HugeSize} = ?RAM_FILE_MODULE:uncompress(HugeFd),
    ?line {ok,0} = ?FILE_MODULE:position(HugeFd, bof),
    ?line {ok,Huge} = ?FILE_MODULE:read(HugeFd, HugeSize),

    ?line ok = ?FILE_MODULE:close(HugeFd),

    ok.

mk_42(0) ->
    [42];
mk_42(N) ->
    B = mk_42(N-1),
    [B|B].

uuencode(suite) ->
    [];
uuencode(doc) ->
    ["Test that uuencode/1 and uudecode/1 works."];
uuencode(Config) when is_list(Config) ->
    ?line Data   = ?config(data_dir, Config),
    ?line Real   = filename:join(Data, "realmen.html"),
    ?line RealUu = filename:join(Data, "realmen.html.uu"),
    %%
    %% Uudecode test
    %%
    ?line {ok, FdReal}   = ?FILE_MODULE:open(Real, []),
    ?line {ok, Fd}       = ?FILE_MODULE:open([], [ram, read, write]),
    ?line {ok, FdRealUu} = ?FILE_MODULE:open(RealUu, []),
    %%
    ?line {ok, SzUu}     = ?FILE_MODULE:copy(FdRealUu, Fd),
    ?line {ok, Sz}       = ?RAM_FILE_MODULE:uudecode(Fd),
    ?line true           = (Sz =< SzUu),
    ?line {ok, 0}        = ?FILE_MODULE:position(Fd, bof),
    ?line true           = compare(FdReal, Fd),
    %%
    %% Uuencode and decode test
    %%
    F = fun(Offs) ->
		Size = Sz - Offs,
		?line {ok, Offs}     = ?FILE_MODULE:position(FdReal, {bof,Offs}),
		?line {ok, 0}        = ?FILE_MODULE:position(Fd, bof),
		?line ok             = ?FILE_MODULE:truncate(Fd),
		?line {ok, Size}     = ?FILE_MODULE:copy(FdReal, Fd),
		?line {ok, SizeUu}   = ?RAM_FILE_MODULE:uuencode(Fd),
		?line true           = (Size =< SizeUu),
		?line {ok, Size}     = ?RAM_FILE_MODULE:uudecode(Fd),
		?line {ok, Offs}     = ?FILE_MODULE:position(FdReal, {bof,Offs}),
		?line {ok, 0}        = ?FILE_MODULE:position(Fd, bof),
		?line true           = compare(FdReal, Fd)
	end,
    lists:foreach(F, lists:seq(0,Sz-1, 43)),

    ?line ok             = ?FILE_MODULE:close(FdReal),
    ?line ok             = ?FILE_MODULE:close(Fd),
    ?line ok             = ?FILE_MODULE:close(FdRealUu),
    %%
    ok.



large_file_errors(suite) ->
    [];
large_file_errors(doc) ->
    ["Test error checking of large file offsets."];
large_file_errors(Config) when is_list(Config) ->
    ?line TwoGig = 1 bsl 31,
    ?line {ok,Fd}         = ?RAM_FILE_MODULE:open("1234567890", [read,write]),
    ?line {error, einval} = ?FILE_MODULE:read(Fd, TwoGig),
    ?line {error, badarg} = ?FILE_MODULE:read(Fd, -1),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {bof,TwoGig}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {bof,-TwoGig-1}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {bof,-1}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {cur,TwoGig}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {cur,-TwoGig-1}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {eof,TwoGig}),
    ?line {error, einval} = ?FILE_MODULE:position(Fd, {eof,-TwoGig-1}),
    ?line {error, einval} = ?FILE_MODULE:pread(Fd, TwoGig, 1),
    ?line {error, einval} = ?FILE_MODULE:pread(Fd, -TwoGig-1, 1),
    ?line {error, einval} = ?FILE_MODULE:pread(Fd, -1, 1),
    ?line {error, einval} = ?FILE_MODULE:pwrite(Fd, TwoGig, "@"),
    ?line {error, einval} = ?FILE_MODULE:pwrite(Fd, -TwoGig-1, "@"),
    ?line {error, einval} = ?FILE_MODULE:pwrite(Fd, -1, "@"),
    ?line {error, einval} = ?FILE_MODULE:pread(Fd, TwoGig, 0),
    ?line {error, einval} = ?FILE_MODULE:pread(Fd, -TwoGig-1, 0),
    ?line {error, einval} = ?FILE_MODULE:pread(Fd, -1, 0),
    ?line ok              = ?FILE_MODULE:close(Fd),
    ok.



large_file_light(suite) ->
    [];
large_file_light(doc) ->
    ["Test light operations on a \"large\" ram_file."];
large_file_light(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    %% Marker for next test case that is to heavy to run in a suite.
    ?line ok = ?FILE_MODULE:write_file(
		  filename:join(PrivDir, "large_file_light"),
		  <<"TAG">>),
    %%
    ?line Data = "abcdefghijklmnopqrstuvwzyz",
    ?line Size = sizeof(Data),
    ?line Max = (1 bsl 31) - 1,
    ?line Max__1 = Max - 1,
    ?line {ok, Fd}        = ?RAM_FILE_MODULE:open(Data, [read]),
    ?line {ok, Data}      = ?FILE_MODULE:read(Fd, Size+1),
    ?line {ok, Max__1}    = ?FILE_MODULE:position(Fd, {eof, Max-Size-1}),
    ?line eof             = ?FILE_MODULE:read(Fd, 1),
    ?line {ok, Max}       = ?FILE_MODULE:position(Fd, {bof, Max}),
    ?line {ok, Zero}      = ?FILE_MODULE:read(Fd, 0),
    ?line 0               = sizeof(Zero),
    ?line eof             = ?FILE_MODULE:read(Fd, 1),
    ?line eof             = ?FILE_MODULE:pread(Fd, Max__1, 1),
    ?line {ok, Zero}      = ?FILE_MODULE:pread(Fd, Max, 0),
    ?line eof             = ?FILE_MODULE:pread(Fd, Max, 1),
    ok.



large_file_heavy(suite) ->
    [];
large_file_heavy(doc) ->
    ["Test operations on a maximum size (2 GByte - 1) ram_file."];
large_file_heavy(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    %% Check previous test case marker.
    case ?FILE_MODULE:read_file_info(
	    filename:join(PrivDir, "large_file_light")) of
	{ok,_} ->
	    {skipped,"Too heavy for casual testing!"};
	_ ->
	    do_large_file_heavy(Config)
    end.

do_large_file_heavy(_Config) ->
    ?line Data = "qwertyuiopasdfghjklzxcvbnm",
    ?line Size = sizeof(Data),
    ?line Max = (1 bsl 31) - 1,
    ?line Max__1 = Max - 1,
    ?line Max__3 = Max - 3,
    ?line {ok, Fd}        = ?RAM_FILE_MODULE:open(Data, [read,write]),
    ?line {ok, Data}      = ?FILE_MODULE:read(Fd, Size+1),
    ?line {ok, Max}       = ?FILE_MODULE:position(Fd, {eof, Max-Size}),
    ?line eof             = ?FILE_MODULE:read(Fd, 1),
    ?line erlang:display({allocating,2,'GByte',please,be,patient,'...'}),
    ?line ok              = ?FILE_MODULE:write(Fd, ""),
    ?line erlang:display({allocating,2,'GByte',succeeded}),
    ?line {ok, Max__1}    = ?FILE_MODULE:position(Fd, {eof, -1}),
    ?line {ok, [0]}       = ?FILE_MODULE:read(Fd, 1),
    ?line {ok, []}        = ?FILE_MODULE:read(Fd, 0),
    ?line eof             = ?FILE_MODULE:read(Fd, 1),
    ?line ok              = ?FILE_MODULE:pwrite(Fd, Max-3, "TAG"),
    ?line {ok, Max}       = ?FILE_MODULE:position(Fd, cur),
    ?line {ok, Max__3}    = ?FILE_MODULE:position(Fd, {eof, -3}),
    ?line {ok, "TAG"}     = ?FILE_MODULE:read(Fd, 3+1),
    ?line {ok, Max__3}    = ?FILE_MODULE:position(Fd, {cur, -3}),
    ?line ok              = ?FILE_MODULE:write(Fd, "tag"),
    ?line {ok, Max}       = ?FILE_MODULE:position(Fd, cur),
    ?line {ok, 0}         = ?FILE_MODULE:position(Fd, bof),
    ?line {ok, "tag"}     = ?FILE_MODULE:pread(Fd, Max__3, 3+1),
    ?line {ok, 0}         = ?FILE_MODULE:position(Fd, cur),
    ?line ok              = ?FILE_MODULE:close(Fd),
    ok.

%%--------------------------------------------------------------------------
%% Utility functions

compare(FdA, FdB) ->
    Size = 65536,
    case {?FILE_MODULE:read(FdA, Size), ?FILE_MODULE:read(FdB, Size)} of
	{{error, _} = Error, _} ->
	    Error;
	{_, {error, _} = Error} ->
	    Error;
	{{ok, A}, {ok, B}} ->
	    case compare_data(A, B) of
		true ->
		    compare(FdA, FdB);
		false ->
		    false
	    end;
	{eof, eof} ->
	    true;
	_ ->
	    false
    end.

compare_data(A, B) when is_list(A), is_list(B) ->
    list_to_binary(A) == list_to_binary(B);
compare_data(A, B) when is_list(A), is_binary(B) ->
    list_to_binary(A) == B;
compare_data(A, B) when is_binary(A), is_list(B) ->
    A == list_to_binary(B);
compare_data(A, B) when is_binary(A), is_binary(B) ->
    A == B.
	    
sizeof(Data) when is_list(Data) ->
    length(Data);
sizeof(Data) when is_binary(Data) ->
    byte_size(Data).

append(Data1, Data2) when is_list(Data1), is_list(Data2) ->    
    Data1 ++ Data2;
append(Data1, Data2) when is_binary(Data1), is_binary(Data2) ->
    list_to_binary([Data1 | Data2]).

slice(Data, Start, Length) when is_list(Data) ->
    lists:sublist(Data, Start+1, Length);
slice(Data, Start, Length) when is_binary(Data) ->
    {_, Bin} = split_binary(Data, Start),
    if
	Length >= byte_size(Bin) ->
	    Bin;
	true ->
	    {B, _} = split_binary(Bin, Length),
	    B
    end.

