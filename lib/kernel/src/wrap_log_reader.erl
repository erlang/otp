%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

%% Read wrap files with internal format

-module(wrap_log_reader).

%%-define(debug, true).
-ifdef(debug).
-define(FORMAT(P, A), io:format(P, A)).
-else.
-define(FORMAT(P, A), ok).
-endif.

-export([open/1, open/2, chunk/1, chunk/2, close/1]).

-export_type([continuation/0]).

-include("disk_log.hrl").

-record(wrap_reader, 
	{fd       :: file:fd(),
	 cont     :: dlog_cont(), 	% disk_log's continuation record
	 file     :: file:filename(),	% file name without extension
	 file_no  :: non_neg_integer(),	% current file number
	 mod_time :: file:date_time(),	% modification time of current file
	 first_no :: non_neg_integer() | 'one' % first read file number
	}).

-opaque continuation() :: #wrap_reader{}.

%%
%%  Exported functions
%%

%% A special case to be handled when appropriate: if current file
%% number is one greater than number of files then the max file number
%% is not yet reached, we are on the first 'round' of filling the wrap
%% files.

-type open_ret() :: {'ok', Continuation :: continuation()}
                  | {'error', Reason :: tuple()}.

-spec open(Filename) -> open_ret() when
      Filename :: string() | atom().

open(File) when is_atom(File) ->
    open(atom_to_list(File));
open(File) when is_list(File) ->
    case read_index_file(File) of
	%% The special case described above.
	{ok, {CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} 
	             when CurFileNo =:= NoOfFiles + 1 ->
	    FileNo = 1,
	    ?FORMAT("open from ~p Cur = ~p, Sz = ~p, Tot = ~p, NoFiles = ~p~n",
		    [FileNo, CurFileNo, _CurFileSz, _TotSz, NoOfFiles]),
	    open_int(File, FileNo, FileNo);
	{ok, {CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} ->
	    FileNo = case (CurFileNo + 1) rem NoOfFiles of
			 0 -> NoOfFiles;
			 No -> No
		     end,
	    ?FORMAT("open from ~p Cur = ~p, Sz = ~p, Tot = ~p, NoFiles = ~p~n",
		    [FileNo, CurFileNo, _CurFileSz, _TotSz, NoOfFiles]),
	    open_int(File, FileNo, FileNo);
	Error ->
	    Error
    end.

-spec open(Filename, N) -> open_ret() when
      Filename :: string() | atom(),
      N :: integer().

open(File, FileNo) when is_atom(File), is_integer(FileNo) ->
    open(atom_to_list(File), FileNo);
open(File, FileNo) when is_list(File), is_integer(FileNo) ->
    case read_index_file(File) of
	{ok, {_CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} 
	  when NoOfFiles >= FileNo ->
	    ?FORMAT("open file ~p Cur = ~p, Sz = ~p, Tot = ~p, NoFiles = ~p~n",
		    [FileNo, _CurFileNo, _CurFileSz, _TotSz, NoOfFiles]),
	    open_int(File, FileNo, one);
	%% The special case described above.
	{ok, {CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} 
	  when CurFileNo =:= FileNo, CurFileNo =:= NoOfFiles +1 ->
	    ?FORMAT("open file ~p Cur = ~p, Sz = ~p, Tot = ~p, NoFiles = ~p~n",
		    [FileNo, CurFileNo, _CurFileSz, _TotSz, NoOfFiles]),
	    open_int(File, FileNo, one);
	{ok, {_CurFileNo, _CurFileSz, _TotSz, _NoOfFiles}} ->
	    {error, {file_not_found, add_ext(File, FileNo)}};
	Error ->
	    Error
    end.

-spec close(Continuation) -> 'ok' | {'error', Reason} when
      Continuation :: continuation(),
      Reason :: file:posix().

close(#wrap_reader{fd = FD}) ->
    file:close(FD).

-type chunk_ret() :: {Continuation2, Terms :: [term()]}
                   | {Continuation2,
                      Terms :: [term()],
                      Badbytes :: non_neg_integer()}
                   | {Continuation2, 'eof'}
                   | {'error', Reason :: term()}.

-spec chunk(Continuation) -> chunk_ret() when
      Continuation :: continuation().

chunk(WR = #wrap_reader{}) ->
    chunk(WR, ?MAX_CHUNK_SIZE, 0).

-spec chunk(Continuation, N) -> chunk_ret() when
      Continuation :: continuation(),
      N :: infinity | pos_integer().

chunk(WR = #wrap_reader{}, infinity) ->
    chunk(WR, ?MAX_CHUNK_SIZE, 0);
chunk(WR = #wrap_reader{}, N) when is_integer(N), N > 0 ->
    chunk(WR, N, 0).

%%
%%  Local functions
%%

open_int(File, FileNo, FirstFileNo) ->
    FName = add_ext(File, FileNo),
    case file:open(FName, [raw, binary, read]) of
	{ok, Fd} ->  %% File exists
	    case file:read(Fd, ?HEADSZ) of
		{ok, Head} ->
		    case disk_log_1:is_head(Head) of
			no ->
			    _ = file:close(Fd),
			    {error, {not_a_log_file, FName}};
			_ -> % yes or yes_not_closed
			    case last_mod_time(FName) of
				{ok, ModTime} ->
				    WR = #wrap_reader{fd = Fd, cont = start,
						      file = File, 
						      file_no = FileNo,
						      mod_time = ModTime, 
						      first_no = FirstFileNo},
				    {ok, WR};
				{error, E} ->
				    _ = file:close(Fd),
				    {error, {file_error, FName, E}}
			    end
		    end;
		_Other ->
		    _ = file:close(Fd),
		    {error, {not_a_log_file, FName}}
	    end;
	_Other ->
	    {error, {not_a_log_file, FName}}
    end.

chunk(WR, N, Bad) ->
    #wrap_reader{fd = Fd, cont = Continue, file = File, file_no = CurFileNo,
		 first_no = FirstFileNo} = WR,
    case read_a_chunk(Fd, N, Continue, add_ext(File, CurFileNo)) of
	eof ->
	    case FirstFileNo of
		one ->
		    {WR, eof};
		_Else ->
		    chunk_at_eof(WR, N, Bad)
	    end;
	{ContOut, [], BadBytes} ->
	    ?FORMAT("chunk: empty chunk read, ~p bad bytes~n", [BadBytes]),
	    chunk(WR#wrap_reader{cont = ContOut}, N, Bad + BadBytes);
	{ContOut, Chunk, BadBytes} when Bad + BadBytes =:= 0 ->
	    {WR#wrap_reader{cont = ContOut}, Chunk};
	{ContOut, Chunk, BadBytes} ->
	    ?FORMAT("chunk: total of ~p bad bytes~n", [BadBytes]),
	    {WR#wrap_reader{cont = ContOut}, Chunk, Bad + BadBytes};
	Error ->
	    Error
    end.

read_a_chunk(Fd, N, start, FileName) ->
    read_a_chunk(Fd, FileName, 0, [], N);
read_a_chunk(Fd, N, More, FileName) ->
    Pos = More#continuation.pos,
    B = More#continuation.b,
    read_a_chunk(Fd, FileName, Pos, B, N).

read_a_chunk(Fd, FileName, Pos, B, N) ->
    R = disk_log_1:chunk_read_only(Fd, FileName, Pos, B, N),
    %% Create terms from the binaries returned from chunk_read_only/5.
    %% 'foo' will do here since Log is not used in read-only mode.
    Log = foo,
    case disk_log:ichunk_end(R, Log) of
	{C = #continuation{}, S} ->
	    {C, S, 0};
	Else ->
	    Else
    end.

chunk_at_eof(WR, N, Bad) ->
    #wrap_reader{file = File, file_no = CurFileNo,
		 first_no = FirstFileNo} = WR,
    case read_index_file(File) of
	{ok, IndexFile} ->
	    {_, _, _, NoOfFiles} = IndexFile,
	    NewFileNo = case (CurFileNo + 1) rem NoOfFiles of
			    %% The special case described above.
			    _ when CurFileNo > NoOfFiles -> 1;
			    0 when NoOfFiles > 1 -> NoOfFiles;
			    No when CurFileNo =:= NoOfFiles -> 
				FileName = add_ext(File, CurFileNo+1),
				case file:read_file_info(FileName) of
				    {ok, _} -> CurFileNo + 1;
				    _ -> No
				end;
			    No -> No
			end,
	    ?FORMAT("chunk: at eof, index file: ~p, FirstFileNo: ~p, "
		    "CurFileNo: ~p, NoOfFiles: ~p, NewFileNo: ~p~n", 
		    [IndexFile, FirstFileNo, CurFileNo, 
		     NoOfFiles, NewFileNo]),
	    case {FirstFileNo, NewFileNo} of
		{_, 0} -> {WR, eof};
		{_, FirstFileNo} -> {WR, eof};
		_ -> read_next_file(WR, N, NewFileNo, Bad)
	    end;
	Error ->
	    Error
    end.

%% Read the index file for the File
%% -> {ok, {CurFileNo, CurFileSz, TotSz, NoOfFiles}} | {error, Reason}
read_index_file(File) ->
    case catch disk_log_1:read_index_file(File) of
	{1, 0, 0, 0} ->
	    {error, {index_file_not_found, File}};
	{error, _Reason} ->
	    {error, {index_file_not_found, File}};
	FileData ->
	    {ok, FileData}
    end.

%% When reading all the index files, this function closes the previous
%% index file and opens the next one.
read_next_file(WR, N, NewFileNo, Bad) ->
    #wrap_reader{file = File, file_no = CurFileNo, 
		 mod_time = ModTime, first_no = FirstFileNo} = WR,
    %% If current file were closed here, then WR would be in a strange
    %% state should an error occur below.
    case last_mod_time(add_ext(File, NewFileNo)) of
	{ok, NewModTime} ->
	    OldMT = calendar:datetime_to_gregorian_seconds(ModTime),
	    NewMT = calendar:datetime_to_gregorian_seconds(NewModTime),
	    Diff = NewMT - OldMT,
	    ?FORMAT("next: now = ~p~n      last mtime = ~p~n"
		    "      mtime = ~p~n      diff = ~p~n",
		    [calendar:local_time(), ModTime, NewModTime, Diff]),
	    if 
		Diff < 0 ->
		    %% The file to be read is older than the one just finished.
		    {error, {is_wrapped, add_ext(File, CurFileNo)}}; 
		true -> 
		    case open_int(File, NewFileNo, FirstFileNo) of
			{ok, NWR} ->
			    _ = close(WR), %% Now we can safely close the old file.
			    chunk(NWR, N, Bad);
			Error ->
			    Error
		    end
	    end;
	{error, EN} ->
	    {error, {file_error, add_ext(File, NewFileNo), EN}}
    end.

%% Get the last modification time of a file
last_mod_time(File) ->
    case file:read_file_info(File) of
	{ok, FileInfo} ->
	    {ok, FileInfo#file_info.mtime};
	E ->
	    {error, E}
    end.

add_ext(File, Ext) ->
    lists:concat([File, ".", Ext]).
