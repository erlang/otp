%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(ram_file).

%% Binary RAM file interface

%% Generic file contents operations
-export([open/2, close/1]).
-export([write/2, read/2, copy/3,
	 pread/2, pread/3, pwrite/2, pwrite/3, 
	 position/2, truncate/1, datasync/1, sync/1]).

%% Specialized file operations
-export([get_size/1, get_file/1, set_file/2, get_file_close/1]).
-export([compress/1, uncompress/1, uuencode/1, uudecode/1, advise/4]).
-export([allocate/3]).

-export([open_mode/1]).  %% used by ftp-file

-export([ipread_s32bu_p32bu/3]).



%% Includes and defines

-define(RAM_FILE_DRV, "ram_file_drv").
-define(MAX_I32, (1 bsl 31)).
-define(G_I32(X), is_integer(X), X >= -?MAX_I32, X < ?MAX_I32).

-include("file.hrl").



%% --------------------------------------------------------------------------
%% These operation codes were once identical between efile_drv.c
%% and ram_file_drv.c, but now these drivers are not depeding on each other.
%% So, the codes could be changed to more logical values now, but why indeed?

%% Defined "file" functions 
-define(RAM_FILE_OPEN,            1).
-define(RAM_FILE_READ,            2).
-define(RAM_FILE_LSEEK,           3).
-define(RAM_FILE_WRITE,           4).
-define(RAM_FILE_FSYNC,           9).
-define(RAM_FILE_TRUNCATE,       14).
-define(RAM_FILE_PREAD,          17).
-define(RAM_FILE_PWRITE,         18).
-define(RAM_FILE_FDATASYNC,      19).

%% Other operations
-define(RAM_FILE_GET,            30).
-define(RAM_FILE_SET,            31).
-define(RAM_FILE_GET_CLOSE,      32).
-define(RAM_FILE_COMPRESS,       33).
-define(RAM_FILE_UNCOMPRESS,     34).
-define(RAM_FILE_UUENCODE,       35).
-define(RAM_FILE_UUDECODE,       36).
-define(RAM_FILE_SIZE,           37).
-define(RAM_FILE_ADVISE,         38).
-define(RAM_FILE_ALLOCATE,       39).

%% Open modes for RAM_FILE_OPEN
-define(RAM_FILE_MODE_READ,       1).
-define(RAM_FILE_MODE_WRITE,      2).
-define(RAM_FILE_MODE_READ_WRITE, 3).
%% Use this mask to get just the mode bits to be passed to the driver.
-define(RAM_FILE_MODE_MASK, 3).

%% Seek modes for RAM_FILE_LSEEK
-define(RAM_FILE_SEEK_SET,        0).
-define(RAM_FILE_SEEK_CUR,        1).
-define(RAM_FILE_SEEK_END,        2).

%% Return codes
-define(RAM_FILE_RESP_OK,         0).
-define(RAM_FILE_RESP_ERROR,      1).
-define(RAM_FILE_RESP_DATA,       2).
-define(RAM_FILE_RESP_NUMBER,     3).
-define(RAM_FILE_RESP_INFO,       4).

%% POSIX file advises
-define(POSIX_FADV_NORMAL,     0).
-define(POSIX_FADV_RANDOM,     1).
-define(POSIX_FADV_SEQUENTIAL, 2).
-define(POSIX_FADV_WILLNEED,   3).
-define(POSIX_FADV_DONTNEED,   4).
-define(POSIX_FADV_NOREUSE,    5).

%% --------------------------------------------------------------------------
%% Generic file contents operations.
%%
%% Supposed to be called by applications through module file.

open(Data, ModeList) when is_list(ModeList) ->
    case open_mode(ModeList) of
  	{Mode,Opts} when is_integer(Mode) ->
  	    case ll_open(Data, Mode, Opts) of
  		{ok,Port} ->
  		    {ok,#file_descriptor{module=?MODULE, data=Port}};
  		Error ->
  		    Error
 	    end;
 	{error,_}=Error ->
  	    Error
    end;
%% Old obsolete mode specification
open(Data, Mode) ->
    case mode_list(Mode) of
	ModeList when is_list(ModeList) ->
	    open(Data, ModeList);
	Error ->
	    Error
    end.

close(#file_descriptor{module = ?MODULE, data = Port}) -> 
    ll_close(Port).

read(#file_descriptor{module = ?MODULE, data = Port}, Sz)
  when is_integer(Sz), Sz >= 0 ->
    if
	?G_I32(Sz) ->
	    Cmd = <<?RAM_FILE_READ:8,Sz:32>>,
	    case call_port(Port, Cmd) of
		{ok, {0, _Data}} when Sz =/= 0 ->
		    eof;
		{ok, {_Sz, Data}} ->
		    {ok, Data};
		{error, enomem} ->
		    %% Garbage collecting here might help if
		    %% the current processes has some old binaries left.
		    erlang:garbage_collect(),
		    case call_port(Port, Cmd) of
			{ok, {0, _Data}} when Sz =/= 0 ->
			    eof;
			{ok, {_Sz, Data}} ->
			    {ok, Data};
			Error ->
			    Error
		    end;
		Error ->
		    Error
	    end;
	true ->
	    {error, einval}
    end.

write(#file_descriptor{module = ?MODULE, data = Port}, Bytes) -> 
    case call_port(Port, [?RAM_FILE_WRITE | Bytes]) of
	{ok, _Sz} ->
	    ok;
	Error ->
	    Error
    end.




copy(#file_descriptor{module = ?MODULE} = Source,
     #file_descriptor{module = ?MODULE} = Dest,
     Length)
  when is_integer(Length), Length >= 0;
       is_atom(Length) ->
    %% XXX Should be moved down to the driver for optimization.
    file:copy_opened(Source, Dest, Length).

datasync(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, <<?RAM_FILE_FDATASYNC>>).

sync(#file_descriptor{module = ?MODULE, data = Port}) -> 
    call_port(Port, <<?RAM_FILE_FSYNC>>).

truncate(#file_descriptor{module = ?MODULE, data = Port}) -> 
    call_port(Port, <<?RAM_FILE_TRUNCATE>>).

position(#file_descriptor{module = ?MODULE, data = Port}, Pos) -> 
    case lseek_position(Pos) of
	{ok, Offs, Whence} when ?G_I32(Offs) ->
	    call_port(Port, <<?RAM_FILE_LSEEK:8,Offs:32,Whence:32>>);
	{ok, _, _} ->
	    {error, einval};
	Error ->
	    Error
    end.



pread(#file_descriptor{module = ?MODULE, data = Port}, L) when is_list(L) ->
    pread_1(Port, L, []).

pread_1(Port, [], Cs) ->
    pread_2(Port, lists:reverse(Cs), []);
pread_1(Port, [{At, Sz} | T], Cs)
  when is_integer(At), is_integer(Sz), Sz >= 0 ->
    if 
	?G_I32(At), ?G_I32(Sz) ->
	    pread_1(Port, T, [{Sz,<<?RAM_FILE_PREAD:8,At:32,Sz:32>>}|Cs]);
	true ->
	    {error, einval}
    end;
pread_1(_, _, _243) ->
   {error, badarg}.

pread_2(_Port, [], R) ->
    {ok, lists:reverse(R)};
pread_2(Port, [{Sz,Command}|Commands], R) ->
    case call_port(Port, Command) of
	{ok, {0,_Data}} when Sz =/= 0 -> 
	    pread_2(Port, Commands, [eof | R]);
	{ok, {_Sz,Data}} -> 
	    pread_2(Port, Commands, [Data | R]);
	Error -> 
	    Error
    end.

pread(#file_descriptor{module = ?MODULE, data = Port}, At, Sz) 
  when is_integer(At), is_integer(Sz), Sz >= 0 ->
    if
	?G_I32(At), ?G_I32(Sz) ->
	    case call_port(Port, <<?RAM_FILE_PREAD:8,At:32,Sz:32>>) of
		{ok, {0,_Data}} when Sz =/= 0 -> 
		    eof;
		{ok, {_Sz,Data}} -> 
		    {ok, Data};
		Error -> 
		    Error
	    end;
	true ->
	    {error, einval}
    end;
pread(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, badarg}.



pwrite(#file_descriptor{module = ?MODULE, data = Port}, L) when is_list(L) ->
    pwrite_1(Port, L, 0, []).

pwrite_1(Port, [], _, Cs) ->
    pwrite_2(Port, lists:reverse(Cs), 0);
pwrite_1(Port, [{At, Bytes} | T], R, Cs) when is_integer(At) ->
    if
	?G_I32(At), is_binary(Bytes) ->
	    pwrite_1(Port, T, R+1, 
		     [<<?RAM_FILE_PWRITE:8,At:32,Bytes/binary>> | Cs]);
	?G_I32(At) ->
	    try erlang:iolist_to_binary(Bytes) of
		Bin ->
		    pwrite_1(Port, T, R+1, 
			     [<<?RAM_FILE_PWRITE:8,At:32,Bin/binary>> | Cs])
	    catch
		error:Reason ->
		    {error, Reason}
	    end;
	true ->
	    {error, {R, einval}}
    end;
pwrite_1(_, _, _, _) ->
    {error, badarg}.

pwrite_2(_Port, [], _R) ->
    ok;
pwrite_2(Port, [Command|Commands], R) ->
    case call_port(Port, Command) of
	{ok, _Sz} ->
	    pwrite_2(Port, Commands, R+1);
	{error, badarg} = Error ->
	    Error;
	{error, Reason} ->
	    {error, {R, Reason}}
    end.

pwrite(#file_descriptor{module = ?MODULE, data = Port}, At, Bytes)
  when is_integer(At) ->
    if
	?G_I32(At) ->
	    case call_port(Port, [<<?RAM_FILE_PWRITE:8,At:32>>|Bytes]) of
		{ok, _Sz} ->
		    ok;
		Error ->
		    Error
	    end;
	true ->
	    {error, einval}
    end;
pwrite(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, badarg}.


ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE} = Handle, Pos, MaxSz) ->
    file:ipread_s32bu_p32bu_int(Handle, Pos, MaxSz).



%% --------------------------------------------------------------------------
%% Specialized ram_file API for functions not in file, unique to ram_file.
%%


get_file(#file_descriptor{module = ?MODULE, data = Port}) ->
    case call_port(Port, [?RAM_FILE_GET]) of
	{ok, {_Sz, Data}} -> 
	    {ok, Data};
	Error -> 
	    Error
    end;
get_file(#file_descriptor{}) ->
    {error, enotsup}.

set_file(#file_descriptor{module = ?MODULE, data = Port}, Data) ->
    call_port(Port, [?RAM_FILE_SET | Data]);
set_file(#file_descriptor{}, _) ->
    {error, enotsup}.

get_file_close(#file_descriptor{module = ?MODULE, data = Port}) ->
    case call_port(Port, [?RAM_FILE_GET_CLOSE]) of
	{ok, {_Sz, Data}} -> 
	    {ok, Data};
	Error -> 
	    Error
    end;
get_file_close(#file_descriptor{}) ->
    {error, enotsup}.

get_size(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_SIZE]);
get_size(#file_descriptor{}) ->
    {error, enotsup}.

compress(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_COMPRESS]);
compress(#file_descriptor{}) ->
    {error, enotsup}.

uncompress(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_UNCOMPRESS]);
uncompress(#file_descriptor{}) ->
    {error, enotsup}.


uuencode(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_UUENCODE]);
uuencode(#file_descriptor{}) ->
    {error, enotsup}.

uudecode(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_UUDECODE]);
uudecode(#file_descriptor{}) ->
    {error, enotsup}.

advise(#file_descriptor{module = ?MODULE, data = Port}, Offset,
        Length, Advise) ->
    Cmd0 = <<?RAM_FILE_ADVISE, Offset:64/signed, Length:64/signed>>,
    case Advise of
    normal ->
        call_port(Port, <<Cmd0/binary, ?POSIX_FADV_NORMAL:32/signed>>);
    random ->
        call_port(Port, <<Cmd0/binary, ?POSIX_FADV_RANDOM:32/signed>>);
    sequential ->
        call_port(Port, <<Cmd0/binary, ?POSIX_FADV_SEQUENTIAL:32/signed>>);
    will_need ->
        call_port(Port, <<Cmd0/binary, ?POSIX_FADV_WILLNEED:32/signed>>);
    dont_need ->
        call_port(Port, <<Cmd0/binary, ?POSIX_FADV_DONTNEED:32/signed>>);
    no_reuse ->
        call_port(Port, <<Cmd0/binary, ?POSIX_FADV_NOREUSE:32/signed>>);
    _ ->
        {error, einval}
    end;
advise(#file_descriptor{}, _Offset, _Length, _Advise) ->
    {error, enotsup}.

allocate(#file_descriptor{module = ?MODULE, data = Port}, Offset, Length) ->
    call_port(Port, <<?RAM_FILE_ALLOCATE, Offset:64/signed, Length:64/signed>>);
allocate(#file_descriptor{}, _Offset, _Length) ->
    {error, enotsup}.



%%%-----------------------------------------------------------------
%%% Functions to communicate with the driver

ll_open(Data, Mode, Opts) ->
    try erlang:open_port({spawn, ?RAM_FILE_DRV}, Opts) of
	Port ->
	    case call_port(Port, [<<?RAM_FILE_OPEN:8,Mode:32>>|Data]) of
		{error, _} = Error ->
		    ll_close(Port),
		    Error;
		{ok, _} ->
		    {ok, Port}
	    end
    catch
	error:Reason ->
	    {error, Reason}
    end.

call_port(Port, Command) when is_port(Port), is_binary(Command) ->
    try erlang:port_command(Port, Command) of
	true ->
	    get_response(Port)
    catch
	error:badarg ->
	    {error, einval}; % Since Command is valid, Port must be dead
	error:Reason ->
	    {error, Reason}
    end;
call_port(Port, Command) ->
    try erlang:iolist_to_binary(Command) of
	Bin ->
	    call_port(Port, Bin)
    catch
	error:Reason ->
	    {error, Reason}
    end.

get_response(Port) ->
    receive
	{Port, {data, [Response|Rest]}} ->
	    translate_response(Response, Rest);
	{'EXIT', Port, _Reason} ->
	    {error, port_died}
    end.

ll_close(Port) ->
    try erlang:port_close(Port) catch error:_ -> ok end,
    receive %% In case the caller is the owner and traps exits
	{'EXIT', Port, _} ->
	    ok
    after 0 ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% Utility functions.

mode_list(read) ->
    [read];
mode_list(write) ->
    [write];
mode_list(read_write) ->
    [read, write];
mode_list({binary, Mode}) when is_atom(Mode) ->
    [binary | mode_list(Mode)];
mode_list({character, Mode}) when is_atom(Mode) ->
    mode_list(Mode);
mode_list(_) ->
    {error, badarg}.



%% Converts a list of mode atoms into an mode word for the driver.
%% Returns {Mode, Opts} wher Opts is a list of options for 
%% erlang:open_port/2, or {error, einval} upon failure.

open_mode(List) when is_list(List) ->
    case open_mode(List, {0, []}) of
	{Mode, Opts} when Mode band 
			  (?RAM_FILE_MODE_READ bor ?RAM_FILE_MODE_WRITE) 
			  =:= 0 ->
	    {Mode bor ?RAM_FILE_MODE_READ, Opts};
	Other ->
	    Other
    end.

open_mode([ram|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode, Opts});
open_mode([read|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode bor ?RAM_FILE_MODE_READ, Opts});
open_mode([write|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode bor ?RAM_FILE_MODE_WRITE, Opts});
open_mode([binary|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode, [binary | Opts]});
open_mode([], {Mode, Opts}) ->
    {Mode, Opts};
open_mode(_, _) ->
    {error, badarg}.



%% Converts a position tuple {bof, X} | {cur, X} | {eof, X} into
%% {ok, Offset, OriginCode} for the driver.
%% Returns {error, einval} upon failure.

lseek_position(Pos) when is_integer(Pos) ->
    lseek_position({bof, Pos});
lseek_position(bof) ->
    lseek_position({bof, 0});
lseek_position(cur) ->
    lseek_position({cur, 0});
lseek_position(eof) ->
    lseek_position({eof, 0});
lseek_position({bof, Offset}) when is_integer(Offset) ->
    {ok, Offset, ?RAM_FILE_SEEK_SET};
lseek_position({cur, Offset}) when is_integer(Offset) ->
    {ok, Offset, ?RAM_FILE_SEEK_CUR};
lseek_position({eof, Offset}) when is_integer(Offset) ->
    {ok, Offset, ?RAM_FILE_SEEK_END};
lseek_position(_) ->
    {error, badarg}.



translate_response(?RAM_FILE_RESP_OK, []) ->
    ok;
translate_response(?RAM_FILE_RESP_OK, Data) ->
    {ok, Data};
translate_response(?RAM_FILE_RESP_ERROR, List) when is_list(List) ->
    {error, list_to_atom(List)};
translate_response(?RAM_FILE_RESP_NUMBER, [X1, X2, X3, X4]) ->
    {ok, i32(X1, X2, X3, X4)};
translate_response(?RAM_FILE_RESP_DATA, [X1, X2, X3, X4|Data]) ->
    {ok, {i32(X1, X2, X3, X4), Data}};
translate_response(X, Data) ->
    {error, {bad_response_from_port, X, Data}}.

i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.
