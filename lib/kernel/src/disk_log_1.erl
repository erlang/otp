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
-module(disk_log_1).

%% Efficient file based log - implementation part

-export([int_open/4, ext_open/4, logl/1, close/3, truncate/3, chunk/5, 
         sync/2, write_cache/2]).
-export([mf_int_open/7, mf_int_log/3, mf_int_close/2, mf_int_inc/2, 
	 mf_ext_inc/2, mf_int_chunk/4, mf_int_chunk_step/3, 
	 mf_sync/1, mf_write_cache/1]).
-export([mf_ext_open/7, mf_ext_log/3, mf_ext_close/2]).

-export([print_index_file/1]).
-export([read_index_file/1]).
-export([read_size_file/1, read_size_file_version/1]).
-export([chunk_read_only/5]).
-export([mf_int_chunk_read_only/4]).
-export([change_size_wrap/3]).
-export([get_wrap_size/1]).
-export([is_head/1]).
-export([position/3, truncate_at/3, fwrite/4, fclose/2]).
-export([set_quiet/1, is_quiet/0]).

-compile({inline,[{scan_f2,7}]}).

-import(lists, [concat/1, reverse/1, sum/1]).

-include("disk_log.hrl").

%%% At the head of a LOG file we have [?LOGMAGIC, ?OPENED | ?CLOSED].
%%% Otherwise it's not a LOG file. Following that, the head, come the
%%% logged items.
%%%
%%% There are four formats of wrap log files (so far). Only the size
%%% file and the index file differ between versions between the first
%%% three version. The fourth version 2(a), has some protection
%%% against damaged item sizes.
%%% Version 0: no "siz" file
%%% Version 1: "siz" file, 4 byte sizes
%%% Version 2: 8 byte sizes (support for large files)
%%% Version 2(a): Change of the format of logged items:
%%%               if the size of a term binary is greater than or equal to
%%%               ?MIN_MD5_TERM, a logged item looks like
%%%               <<Size:32, ?BIGMAGICHEAD:32, MD5:128, Term/binary>>,
%%%               otherwise <<Size:32, ?BIGMAGICHEAD:32, Term/binary>>.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% -> {ok, NoBytes, NewFdC} | {Error, NewFdC}
log(FdC, FileName, X) ->
    {Bs, Size} = logl(X, [], 0),
    case fwrite(FdC, FileName, Bs, Size) of
	{ok, NewFdC} ->
	    {ok, Size, NewFdC};
	Error ->
	    Error
    end.

-spec logl([binary()]) -> {iolist(), non_neg_integer()}.
logl(X) ->
    logl(X, [], 0).

-dialyzer({no_improper_lists, logl/3}).
logl([X | T], Bs, Size) ->
    Sz = byte_size(X),
    BSz = <<Sz:?SIZESZ/unit:8>>,
    NBs = case Sz < ?MIN_MD5_TERM of
              true ->
                  [Bs, BSz, ?BIGMAGICHEAD | X];
              false ->
                  MD5 = erlang:md5(BSz),
                  [Bs, BSz, ?BIGMAGICHEAD, MD5 | X]
              end,
    logl(T, NBs, Size + ?HEADERSZ + Sz);
logl([], Bs, Size) ->
    {Bs, Size}.

%% -> {ok, NewFdC} | {Error, NewFdC}
write_cache(#cache{fd = Fd, c = C}, FName) ->
    erase(write_cache_timer_is_running),
    write_cache(Fd, FName, C).
  
%% -> {Reply, NewFdC}; Reply = ok | Error
sync(FdC, FName) ->
    fsync(FdC, FName).
  
%% -> {Reply, NewFdC}; Reply = ok | Error
truncate(FdC, FileName, Head) ->
    Reply = truncate_at(FdC, FileName, ?HEADSZ),
    case Reply of
	{ok, _} when Head =:= none ->
            Reply;
	{ok, FdC1} ->
	    {ok, B} = Head,
	    case log(FdC1, FileName, [B]) of
		{ok, _NoBytes, NewFdC} ->
		    {ok, NewFdC};
		Reply2 ->
		    Reply2
	    end;
	_ ->
	    Reply
    end.

%% -> {NewFdC, Reply}, Reply = {Cont, Binaries} | {error, Reason} | eof
chunk(FdC, FileName, Pos, B, N) when is_binary(B) ->
    true = byte_size(B) >= ?HEADERSZ,
    do_handle_chunk(FdC, FileName, Pos, B, N);
chunk(FdC, FileName, Pos, NoBytes, N) ->
    MaxNoBytes = case NoBytes of
                     [] -> ?MAX_CHUNK_SIZE;
                     _ -> erlang:max(NoBytes, ?MAX_CHUNK_SIZE)
                 end,
    case read_chunk(FdC, FileName, Pos, MaxNoBytes) of
	{NewFdC, {ok, Bin}} when byte_size(Bin) < ?HEADERSZ ->
	    {NewFdC, {error, {corrupt_log_file, FileName}}};
	{NewFdC, {ok, Bin}} when NoBytes =:= []; byte_size(Bin) >= NoBytes ->
	    NewPos = Pos + byte_size(Bin),
            do_handle_chunk(NewFdC, FileName, NewPos, Bin, N);
	{NewFdC, {ok, _Bin}} ->
	    {NewFdC, {error, {corrupt_log_file, FileName}}};
	{NewFdC, eof} when is_integer(NoBytes) -> % "cannot happen"
	    {NewFdC, {error, {corrupt_log_file, FileName}}};
	Other -> % eof or error
	    Other
    end.

do_handle_chunk(FdC, FileName, Pos, B, N) ->
    case handle_chunk(B, Pos, N, []) of
        corrupt ->
            {FdC, {error, {corrupt_log_file, FileName}}};
        {C, []} ->
            chunk(FdC, FileName, C#continuation.pos, C#continuation.b, N);
        C_Ack ->
            {FdC, C_Ack}
    end.

handle_chunk(B, Pos, 0, Ack) when byte_size(B) >= ?HEADERSZ ->
    {#continuation{pos = Pos, b = B}, Ack};
handle_chunk(B= <<Size:?SIZESZ/unit:8, ?BIGMAGICINT:?MAGICSZ/unit:8, 
             Tail/binary>>, Pos, N, Ack) when Size < ?MIN_MD5_TERM ->
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    %% The client calls binary_to_term/1.
	    handle_chunk(Tail2, Pos, N-1, [BinTerm | Ack]);
	_ ->
	    BytesToRead = Size + ?HEADERSZ,
            {#continuation{pos = Pos - byte_size(B), b = BytesToRead}, Ack}
    end;
handle_chunk(B= <<Size:?SIZESZ/unit:8, ?BIGMAGICINT:?MAGICSZ/unit:8, 
             Tail/binary>>, Pos, _N, Ack) -> % when Size >= ?MIN_MD5_TERM
    MD5 = erlang:md5(<<Size:?SIZESZ/unit:8>>),
    case Tail of
        %% The requested object is always bigger than a chunk.
        <<MD5:16/binary, Bin:Size/binary>> ->
            {#continuation{pos = Pos, b = []}, [Bin | Ack]};
        <<MD5:16/binary, _/binary>> ->
            BytesToRead = Size + ?HEADERSZ + 16,
            {#continuation{pos = Pos - byte_size(B), b = BytesToRead}, Ack};
        _ when byte_size(Tail) >= 16 ->
            corrupt;
        _ ->
            {#continuation{pos = Pos - byte_size(B), b = []}, Ack}
    end;
handle_chunk(B= <<Size:?SIZESZ/unit:8, ?MAGICINT:?MAGICSZ/unit:8, Tail/binary>>,
	     Pos, N, Ack) ->
    %% Version 2, before 2(a).
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    handle_chunk(Tail2, Pos, N-1, [BinTerm | Ack]);
	_ ->
	    %% We read the whole thing into one binary, even if Size is huge.
	    BytesToRead = Size + ?HEADERSZ,
            {#continuation{pos = Pos - byte_size(B), b = BytesToRead}, Ack}
    end;
handle_chunk(B, _Pos, _N, _Ack) when byte_size(B) >= ?HEADERSZ ->
    corrupt;
handle_chunk(B, Pos, _N, Ack) ->
    {#continuation{pos = Pos-byte_size(B), b = []}, Ack}.

read_chunk(FdC, FileName, Pos, MaxBytes) ->
    {FdC1, R} = pread(FdC, FileName, Pos + ?HEADSZ, MaxBytes),
    case position(FdC1, FileName, eof) of
	{ok, NewFdC, _Pos} ->
	    {NewFdC, R};
	{Error, NewFdC} ->
	    {NewFdC, Error}
    end.

%% Used by wrap_log_reader.
%% -> {NewFdC, Reply}, 
%%    Reply = {Cont, Binaries, Bad} (Bad >= 0) | {error, Reason} | eof
chunk_read_only(FdC = #cache{}, FileName, Pos, B, N) ->
    do_chunk_read_only(FdC, FileName, Pos, B, N);
chunk_read_only(Fd, FileName, Pos, B, N) ->
    %% wrap_log_reader calling...
    FdC = #cache{fd = Fd}, 
    {_NFdC, Reply} = do_chunk_read_only(FdC, FileName, Pos, B, N),
    Reply.

do_chunk_read_only(FdC, FileName, Pos, B, N) when is_binary(B) ->
    true = byte_size(B) >= ?HEADERSZ,
    do_handle_chunk_ro(FdC, FileName, Pos, B, N);
do_chunk_read_only(FdC, FileName, Pos, NoBytes, N) ->
    MaxNoBytes = case NoBytes of
                     [] -> ?MAX_CHUNK_SIZE;
                     _ -> erlang:max(NoBytes, ?MAX_CHUNK_SIZE)
                 end,
    case read_chunk_ro(FdC, FileName, Pos, MaxNoBytes) of
	{NewFdC, {ok, Bin}} when byte_size(Bin) < ?HEADERSZ ->
	    NewCont = #continuation{pos = Pos+byte_size(Bin), b = []},
	    {NewFdC, {NewCont, [], byte_size(Bin)}};
	{NewFdC, {ok, Bin}} when NoBytes =:= []; byte_size(Bin) >= NoBytes ->
	    NewPos = Pos + byte_size(Bin),
	    do_handle_chunk_ro(NewFdC, FileName, NewPos, Bin, N);
	{NewFdC, {ok, Bin}} ->
	    NewCont = #continuation{pos = Pos+byte_size(Bin), b = []},
	    {NewFdC, {NewCont, [], byte_size(Bin)-?HEADERSZ}};
	{NewFdC, eof} when is_integer(NoBytes) -> % "cannot happen"
	    {NewFdC, eof}; % what else?
	Other -> 
	    Other
    end.

do_handle_chunk_ro(FdC, FileName, Pos, B, N) ->
    case handle_chunk_ro(B, Pos, N, [], 0) of
        {C, [], 0} ->
            #continuation{pos = NewPos, b = NoBytes} = C,
            do_chunk_read_only(FdC, FileName, NewPos, NoBytes, N);
        C_Ack_Bad ->
            {FdC, C_Ack_Bad}
    end.

handle_chunk_ro(B, Pos, 0, Ack, Bad) when byte_size(B) >= ?HEADERSZ ->
    {#continuation{pos = Pos, b = B}, Ack, Bad};
handle_chunk_ro(B= <<Size:?SIZESZ/unit:8, ?BIGMAGICINT:?MAGICSZ/unit:8,
                Tail/binary>>, Pos, N, Ack, Bad) when Size < ?MIN_MD5_TERM ->
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    handle_chunk_ro(Tail2, Pos, N-1, [BinTerm | Ack], Bad);
	_ ->
	    BytesToRead = Size + ?HEADERSZ,
            {#continuation{pos = Pos - byte_size(B), b = BytesToRead}, Ack, Bad}
    end;
handle_chunk_ro(B= <<Size:?SIZESZ/unit:8, ?BIGMAGICINT:?MAGICSZ/unit:8, 
                Tail/binary>>, Pos, N, Ack, Bad) -> % when Size>=?MIN_MD5_TERM
    MD5 = erlang:md5(<<Size:?SIZESZ/unit:8>>),
    case Tail of
        <<MD5:16/binary, Bin:Size/binary>> ->
            %% The requested object is always bigger than a chunk.
            {#continuation{pos = Pos, b = []}, [Bin | Ack], Bad};
        <<MD5:16/binary, _/binary>> ->
            BytesToRead = Size + ?HEADERSZ + 16,
            {#continuation{pos = Pos - byte_size(B), b = BytesToRead}, Ack, Bad};
        <<_BadMD5:16/binary, _:1/unit:8, Tail2/binary>> ->
            handle_chunk_ro(Tail2, Pos, N-1, Ack, Bad+1);
        _ ->
            {#continuation{pos = Pos - byte_size(B), b = []}, Ack, Bad}
    end;
handle_chunk_ro(B= <<Size:?SIZESZ/unit:8, ?MAGICINT:?MAGICSZ/unit:8,
                Tail/binary>>, Pos, N, Ack, Bad) ->
    %% Version 2, before 2(a).
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    handle_chunk_ro(Tail2, Pos, N-1, [BinTerm | Ack], Bad);
	_ ->
	    %% We read the whole thing into one binary, even if Size is huge.
	    BytesToRead = Size + ?HEADERSZ,
            {#continuation{pos = Pos - byte_size(B), b = BytesToRead}, Ack, Bad}
    end;
handle_chunk_ro(B, Pos, N, Ack, Bad) when byte_size(B) >= ?HEADERSZ ->
    <<_:1/unit:8, B2/binary>> = B,
    handle_chunk_ro(B2, Pos, N-1, Ack, Bad+1);
handle_chunk_ro(B, Pos, _N, Ack, Bad) ->
    {#continuation{pos = Pos-byte_size(B), b = []}, Ack, Bad}.

read_chunk_ro(FdC, FileName, Pos, MaxBytes) ->
    pread(FdC, FileName, Pos + ?HEADSZ, MaxBytes).

%% -> ok | throw(Error)
close(#cache{fd = Fd, c = []}, FileName, read_only) ->
    case file:close(Fd) of
        ok -> ok;
        Error -> file_error(FileName, Error)
    end;
close(#cache{fd = Fd, c = C}, FileName, read_write) ->
    {Reply, _NewFdC} = write_cache(Fd, FileName, C),
    mark(Fd, FileName, ?CLOSED),
    case file:close(Fd) of
        ok -> ok;
        Error -> file_error(FileName, Error)
    end,
    if Reply =:= ok -> ok; true -> throw(Reply) end.

%% Open an internal file. Head is ignored if Mode is read_only.
%% int_open(FileName, Repair, Mode, Head) -> 
%%    {ok, {Alloc, FdC, HeadSize, FileSize}} 
%%  | {repaired, FdC, Terms, BadBytes, FileSize} 
%%  | throw(Error)
%% Alloc = new | existed
%% HeadSize = {NumberOfItemsWritten, NumberOfBytesWritten}
%% (HeadSize is equal {0, 0} if Alloc =:= existed, or no header written.)
int_open(FName, truncate, read_write, Head) ->
    new_int_file(FName, Head);
int_open(FName, Repair, read_write, Head) ->
    case open_read(FName) of
	{ok, Fd} ->  %% File exists
	    case file:read(Fd, ?HEADSZ) of
		{ok, FileHead} ->
		    case is_head(FileHead) of
			yes ->
                            case file:close(Fd) of
                                ok -> ok;
                                Error2 -> file_error(FName, Error2)
                            end,
			    case open_update(FName) of
				{ok, Fd2} ->
				    mark(Fd2, FName, ?OPENED),
                                    FdC1 = #cache{fd = Fd2},
				    {FdC, P} = position_close(FdC1, FName,eof),
				    {ok, {existed, FdC, {0, 0}, P}};
				Error ->
				    file_error(FName, Error)
			    end;
			yes_not_closed when Repair ->
			    repair(Fd, FName);
			yes_not_closed when not Repair ->
			    _ = file:close(Fd),
			    throw({error, {need_repair, FName}});
			no ->
			    _ = file:close(Fd),
			    throw({error, {not_a_log_file, FName}})
		    end;
		eof ->
		    _= file:close(Fd),
		    throw({error, {not_a_log_file, FName}});
		Error ->
		    file_error_close(Fd, FName, Error)
	    end;
	_Other ->
	    new_int_file(FName, Head)
    end;
int_open(FName, _Repair, read_only, _Head) ->
    case open_read(FName) of
	{ok, Fd} ->  %% File exists
	    case file:read(Fd, ?HEADSZ) of
		{ok, Head} ->
		    case is_head(Head) of
			yes ->
			    {ok, P} = position_close2(Fd, FName, eof),
                            FdC = #cache{fd = Fd},
			    {ok, {existed, FdC, {0, 0}, P}};
			yes_not_closed  ->
			    {ok, P} = position_close2(Fd, FName, eof),
                            FdC = #cache{fd = Fd},
			    {ok, {existed, FdC, {0, 0}, P}};
			no ->
			    _= file:close(Fd),
			    throw({error, {not_a_log_file, FName}})
		    end;
		eof ->
		    _ = file:close(Fd),
		    throw({error, {not_a_log_file, FName}});
		Error ->
		    file_error_close(Fd, FName, Error)
	    end;
	Error ->
	    file_error(FName, Error)
    end.

new_int_file(FName, Head) ->
    case open_update(FName) of
	{ok, Fd} ->
            ok = truncate_at_close2(Fd, FName, bof),
            fwrite_close2(Fd, FName, [?LOGMAGIC, ?OPENED]),
            {FdC1, Nh, HeadSz} = int_log_head(Fd, Head),
	    {FdC, FileSize} = position_close(FdC1, FName, cur),
            {ok, {new, FdC, {Nh, ?HEADERSZ + HeadSz}, FileSize}};
	Error ->
	    file_error(FName, Error)
    end.

%% -> {FdC, NoItemsWritten, NoBytesWritten} | throw(Error)
int_log_head(Fd, Head) ->
    case lh(Head, internal) of
	{ok, BinHead} -> 
            {Bs, Size} = logl([BinHead]),
            {ok, FdC} = fwrite_header(Fd, Bs, Size),
            {FdC, 1, Size};
	none ->
	    {#cache{fd = Fd}, 0, 0};
	Error -> 
	    _= file:close(Fd),
	    throw(Error)
    end.
    
%% Open an external file.
%% -> {ok, {Alloc, FdC, HeadSize}, FileSize} | throw(Error)
ext_open(FName, truncate, read_write, Head) ->
    new_ext_file(FName, Head);
ext_open(FName, _Repair, read_write, Head) ->
    case file:read_file_info(FName) of
	{ok, _FileInfo} ->
	    case open_update(FName) of
		{ok, Fd} ->
		    {ok, P} = position_close2(Fd, FName, eof),
                    FdC = #cache{fd = Fd},
		    {ok, {existed, FdC, {0, 0}, P}};
		Error ->
		    file_error(FName, Error)
	    end;
	_Other ->
	    new_ext_file(FName, Head)
    end;
ext_open(FName, _Repair, read_only, _Head) ->
    case open_read(FName) of
	{ok, Fd} ->
	    {ok, P} = position_close2(Fd, FName, eof),
            FdC = #cache{fd = Fd},
	    {ok, {existed, FdC, {0, 0}, P}};
	Error ->
	    file_error(FName, Error)
    end.

new_ext_file(FName, Head) ->
    case open_truncate(FName) of
	{ok, Fd} ->
	    {FdC1, HeadSize} = ext_log_head(Fd, Head),
	    {FdC, FileSize} = position_close(FdC1, FName, cur),
	    {ok, {new, FdC, HeadSize, FileSize}};
	Error ->
	    file_error(FName, Error)
    end.

%% -> {FdC, {NoItemsWritten, NoBytesWritten}} | throw(Error)
ext_log_head(Fd, Head) ->
    case lh(Head, external) of
	{ok, BinHead} -> 
            Size = byte_size(BinHead),
            {ok, FdC} = fwrite_header(Fd, BinHead, Size),
            {FdC, {1, Size}};
	none ->
	    {#cache{fd = Fd}, {0, 0}};
	Error -> 
            _= file:close(Fd),
	    throw(Error)
    end.
    
%% -> _Any | throw()
mark(Fd, FileName, What) ->
    {ok, _} = position_close2(Fd, FileName, 4),
    fwrite_close2(Fd, FileName, What).

%% -> {ok, Bin} | Error
lh({ok, Bin}, _Format) ->
    {ok, Bin};
lh({M, F, A}, Format) when is_list(A) ->
    case catch apply(M, F, A) of
	{ok, Head} when Format =:= internal ->
	    {ok, term_to_binary(Head)};
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Bin};
	{ok, Bytes} ->
	    case catch list_to_binary(Bytes) of
		{'EXIT', _} ->
		    {error, {invalid_header, {{M,F,A}, {ok, Bytes}}}};
		Bin ->
		    {ok, Bin}
	    end;
	{'EXIT', Error} ->
	    {error, {invalid_header, {{M,F,A}, Error}}};
	Error ->
	    {error, {invalid_header, {{M,F,A}, Error}}}
    end;
lh({M, F, A}, _Format) -> % cannot happen
    {error, {invalid_header, {M, F, A}}};
lh(none, _Format) ->
    none;
lh(H, _F) -> % cannot happen
    {error, {invalid_header, H}}.

repair(In, File) ->
    FSz = file_size(File),
    case is_quiet() of
        true -> ok;
        _ -> error_logger:info_msg("disk_log: repairing ~tp ...\n", [File])
    end,
    Tmp = add_ext(File, "TMP"),
    {ok, {_Alloc, Out, {0, _}, _FileSize}} = new_int_file(Tmp, none),
    scan_f_read(<<>>, In, Out, File, FSz, Tmp, ?MAX_CHUNK_SIZE, 0, 0).

scan_f_read(B, In, Out, File, FSz, Tmp, MaxBytes, No, Bad) ->
    case file:read(In, MaxBytes) of
        eof ->
            done_scan(In, Out, Tmp, File, No, Bad+byte_size(B));
        {ok, Bin}  ->
            NewBin = list_to_binary([B, Bin]),
            {NB, NMax, Ack, NNo, NBad} =
                scan_f(NewBin, FSz, [], No, Bad),
            case log(Out, Tmp, lists:reverse(Ack)) of
                {ok, _Size, NewOut} ->
                    scan_f_read(NB, In, NewOut, File, FSz, Tmp, NMax,NNo,NBad);
                {{error, {file_error, _Filename, Error}}, NewOut} ->
                    repair_err(In, NewOut, Tmp, File, {error, Error})
            end;
        Error -> 
            repair_err(In, Out, Tmp, File, Error)
    end.

scan_f(B = <<Size:?SIZESZ/unit:8, ?BIGMAGICINT:?MAGICSZ/unit:8, Tail/binary>>,
       FSz, Ack, No, Bad) when Size < ?MIN_MD5_TERM ->
    scan_f2(B, FSz, Ack, No, Bad, Size, Tail);
scan_f(B = <<Size:?SIZESZ/unit:8, ?BIGMAGICINT:?MAGICSZ/unit:8, Tail/binary>>,
       FSz, Ack, No, Bad) -> % when Size >= ?MIN_MD5_TERM
    MD5 = erlang:md5(<<Size:?SIZESZ/unit:8>>),
    case Tail of
        <<MD5:16/binary, BinTerm:Size/binary, Tail2/binary>> ->
            case catch binary_to_term(BinTerm) of
                {'EXIT', _} ->
                    scan_f(Tail2, FSz, Ack, No, Bad+Size);
                _Term ->
                    scan_f(Tail2, FSz, [BinTerm | Ack], No+1, Bad)
            end;
        <<MD5:16/binary, _/binary>> ->
            {B, Size-byte_size(Tail)+16, Ack, No, Bad};
        _ when byte_size(Tail) < 16 ->
            {B, Size-byte_size(Tail)+16, Ack, No, Bad};
        _ ->
            <<_:8, B2/binary>> = B,
            scan_f(B2, FSz, Ack, No, Bad+1)
    end;
scan_f(B = <<Size:?SIZESZ/unit:8, ?MAGICINT:?MAGICSZ/unit:8, Tail/binary>>, 
       FSz, Ack, No, Bad) when Size =< FSz ->
    %% Since the file is not compressed, the item size cannot exceed
    %% the file size.
    scan_f2(B, FSz, Ack, No, Bad, Size, Tail);
scan_f(B = <<_:?HEADERSZ/unit:8, _/binary>>, FSz, Ack, No, Bad) ->
    <<_:8, B2/binary>> = B,
    scan_f(B2, FSz, Ack, No, Bad + 1);
scan_f(B, _FSz, Ack, No, Bad) ->
    {B, ?MAX_CHUNK_SIZE, Ack, No, Bad}.

scan_f2(B, FSz, Ack, No, Bad, Size, Tail) ->
    case Tail of
        <<BinTerm:Size/binary, Tail2/binary>> ->
            case catch binary_to_term(BinTerm) of
                {'EXIT', _} ->
                    <<_:8, B2/binary>> = B,
                    scan_f(B2, FSz, Ack, No, Bad+1);
                _Term ->
                    scan_f(Tail2, FSz, [BinTerm | Ack], No+1, Bad)
            end;
        _ ->
            {B, Size-byte_size(Tail), Ack, No, Bad}
    end.

done_scan(In, Out, OutName, FName, RecoveredTerms, BadChars) ->
    _ = file:close(In),
    case catch fclose(Out, OutName) of
        ok ->
            case file:rename(OutName, FName) of
                ok ->
                    case open_update(FName) of
                        {ok, New} ->
                            {ok, P} = position_close2(New, FName, eof),
                            FdC = #cache{fd = New},
                            {repaired, FdC, RecoveredTerms, BadChars, P};
                        Error ->
                            file_error(FName, Error)
                    end;
                Error ->
                    _ = file:delete(OutName),
                    file_error(FName, Error)
            end;
        Error ->
            _ = file:delete(OutName),
            throw(Error)
    end.

-spec repair_err(file:io_device(), #cache{}, file:filename(),
		 file:filename(), {'error', file:posix()}) -> no_return().
repair_err(In, Out, OutName, ErrFileName, Error) ->
    _= file:close(In),
    catch fclose(Out, OutName),
    %% OutName is often the culprit, try to remove it anyway...
    _ = file:delete(OutName),
    file_error(ErrFileName, Error).

%% Used by wrap_log_reader.
-spec is_head(binary()) -> 'yes' | 'yes_not_closed' | 'no'.
is_head(<<M:4/binary, S:4/binary>>) when ?LOGMAGIC =:= M, ?CLOSED =:= S ->
    yes;
is_head(<<M:4/binary, S:4/binary>>) when ?LOGMAGIC =:= M, ?OPENED =:= S ->
    yes_not_closed;
is_head(Bin) when is_binary(Bin) ->
    no.

%%-----------------------------------------------------------------
%% Func: mf_int_open/7, mf_ext_open/7
%% Args: FName = file:filename()
%%       MaxB = integer() 
%%       MaxF = integer()
%%       Repair = truncate | true | false
%%       Mode = read_write | read_only
%%       Head = none | {ok, Bin} | {M, F, A}
%%       Version = integer()
%% Purpose: An ADT for wrapping logs.  mf_int_ writes binaries (mf_ext_
%%          writes bytes)
%%          to files called FName.1, FName.2, ..., FName.MaxF.  
%%          Writes MaxB bytes on each file.  
%%          Creates a file called Name.idx in the Dir.  This
%%          file contains the last written FileName as one byte, and
%%          follwing that, the sizes of each file (size 0 number of items).
%%          On startup, this file is read, and the next available
%%          filename is used as first log file.
%%          Reports can be browsed with Report Browser Tool (rb), or
%%          read with disk_log.
%%-----------------------------------------------------------------
-spec mf_int_open(FName   :: file:filename(),
		  MaxB    :: integer(),
		  MaxF    :: integer(),
		  Repair  :: dlog_repair(),
		  Mode    :: dlog_mode(),
		  Head    :: dlog_head(),
		  Version :: integer())
      -> {'ok', #handle{}, integer()}
       | {'repaired', #handle{},
	  non_neg_integer(), non_neg_integer(), non_neg_integer()}.
%%     | throw(FileError)
mf_int_open(FName, MaxB, MaxF, Repair, Mode, Head, Version) -> 
    {First, Sz, TotSz, NFiles} = read_index_file(Repair, FName, MaxF),
    write_size_file(Mode, FName, MaxB, MaxF, Version),
    NewMaxF = if 
		  NFiles > MaxF ->
		      {MaxF, NFiles};
		  true ->
		      MaxF
	      end,
    case int_file_open(FName, First, 0, 0, Head, Repair, Mode) of
	{ok, FdC, FileName, Lost, {NoItems, NoBytes}, FSz} ->
	    % firstPos = NoBytes is not always correct when the file
	    % existed, but it will have to do since we don't know
	    % where the header ends.
	    CurCnt = Sz + NoItems - Lost,
	    {ok, #handle{filename = FName, maxB = MaxB,
			 maxF = NewMaxF, curF = First, cur_fdc = FdC,
			 cur_name = FileName, cur_cnt = CurCnt,
			 acc_cnt = -Sz, curB = FSz, 
			 firstPos = NoBytes, noFull = 0, accFull = 0}, 
	     TotSz + CurCnt};
	{repaired, FdC, FileName, Rec, Bad, FSz} ->
	    {repaired, 
	     #handle{filename = FName, maxB = MaxB, cur_name = FileName, 
		     maxF = NewMaxF, curF = First, cur_fdc = FdC,
		     cur_cnt = Rec, acc_cnt = -Rec, curB = FSz, 
		     firstPos = 0, noFull = 0, accFull = 0}, 
	     Rec, Bad, TotSz + Rec}
    end.

%% -> {ok, handle(), Lost} | {error, Error, handle()}
mf_int_inc(Handle, Head) -> 
    #handle{filename = FName, cur_cnt = CurCnt, acc_cnt = AccCnt, 
	    cur_name = FileName, curF = CurF, maxF = MaxF, 
	    cur_fdc = CurFdC, noFull = NoFull} = Handle,
    case catch wrap_int_log(FName, CurF, MaxF, CurCnt, Head) of
	{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost} ->
	    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF, 
				    cur_name = NewFileName, 
				    cur_cnt = Nh, acc_cnt = AccCnt + CurCnt, 
				    maxF = NewMaxF, firstPos = FirstPos,
				    curB = FirstPos, noFull = NoFull + 1},
	    case catch close(CurFdC, FileName, read_write) of
		ok ->
		    {ok, Handle1, Lost};
		Error -> % Error in the last file, new file opened.
		    {error, Error, Handle1}
	    end;
	Error ->
	    {error, Error, Handle}
    end.

%% -> {ok, handle(), Logged, Lost, NoWraps} | {ok, handle(), Logged} 
%%    | {error, Error, handle(), Logged, Lost}
%% The returned handle is not always valid - something may
%% have been written before things went wrong. 
mf_int_log(Handle, Bins, Head) ->
    mf_int_log(Handle, Bins, Head, 0, []).

mf_int_log(Handle, [], _Head, No, []) ->
    {ok, Handle, No};
mf_int_log(Handle, [], _Head, No, Wraps0) ->
    Wraps = reverse(Wraps0),
    {ok, Handle, No, sum(Wraps), Wraps};
mf_int_log(Handle, Bins, Head, No0, Wraps) ->
    #handle{curB = CurB, maxB = MaxB, cur_name = FileName, cur_fdc = CurFdC, 
            firstPos = FirstPos0, cur_cnt = CurCnt} = Handle,
    {FirstBins, LastBins, NoBytes, N} = 
	int_split_bins(CurB, MaxB, FirstPos0, Bins),
    case FirstBins of
	[] ->
            #handle{filename = FName, curF = CurF, maxF = MaxF, 
                    acc_cnt = AccCnt, noFull = NoFull} = Handle,
	    case catch wrap_int_log(FName, CurF, MaxF, CurCnt, Head) of
		{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost} ->
		    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF,
					    cur_cnt = Nh, 
					    cur_name = NewFileName,
					    acc_cnt = AccCnt + CurCnt, 
					    maxF = NewMaxF, 
					    curB = FirstPos, 
					    firstPos = FirstPos,
					    noFull = NoFull + 1},
		    case catch close(CurFdC, FileName, read_write) of
			ok ->
			    mf_int_log(Handle1, Bins, Head, No0 + Nh, 
				       [Lost | Wraps]);
			Error ->
			    Lost1 = Lost + sum(Wraps),
			    {error, Error, Handle1, No0 + Nh, Lost1}
		    end;
		Error ->
		    {error, Error, Handle, No0, sum(Wraps)}
	    end;
	_ ->
	    case fwrite(CurFdC, FileName, FirstBins, NoBytes) of
                {ok, NewCurFdC} ->
		    Handle1 = Handle#handle{cur_fdc = NewCurFdC, 
                                            curB = CurB + NoBytes, 
					    cur_cnt = CurCnt + N},
		    mf_int_log(Handle1, LastBins, Head, No0 + N, Wraps);
		{Error, NewCurFdC} ->
		    Handle1 = Handle#handle{cur_fdc = NewCurFdC},
		    {error, Error, Handle1, No0, sum(Wraps)}
	    end
    end.

wrap_int_log(FName, CurF, MaxF, CurCnt, Head) ->
    {NewF, NewMaxF} = inc_wrap(FName, CurF, MaxF),
    {ok, NewFdC, NewFileName, Lost, {Nh, FirstPos}, _FileSize} = 
	int_file_open(FName, NewF, CurF, CurCnt, Head),
    {NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost}.

%% -> {NewHandle, Reply}, Reply = {Cont, Binaries} | {error, Reason} | eof
mf_int_chunk(Handle, 0, Bin, N) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk(Handle, {FirstF, 0}, Bin, N);
mf_int_chunk(#handle{curF = FileNo, cur_fdc = FdC, cur_name = FileName} 
             = Handle, {FileNo, Pos}, Bin, N) ->
    {NewFdC, Reply} = chunk(FdC, FileName, Pos, Bin, N),
    {Handle#handle{cur_fdc = NewFdC}, conv(Reply, FileNo)};
mf_int_chunk(Handle, {FileNo, Pos}, Bin, N) ->
    FName = add_ext(Handle#handle.filename, FileNo),
    NFileNo = inc(FileNo, Handle#handle.maxF),
    case catch int_open(FName, true, read_only, any) of
	{error, _Reason} ->
	    case is_quiet() of
		true -> ok;
		_ -> error_logger:info_msg("disk_log: chunk error. File ~tp missing.\n\n",
					   [FName])
	    end,
	    mf_int_chunk(Handle, {NFileNo, 0}, [], N);
	{ok, {_Alloc, FdC, _HeadSize, _FileSize}} ->
	    case chunk(FdC, FName, Pos, Bin, N) of
		{NewFdC, eof} ->
		    _ = file:close(NewFdC#cache.fd),
		    mf_int_chunk(Handle, {NFileNo, 0}, [], N);
		{NewFdC, Other} ->
		    _ = file:close(NewFdC#cache.fd),
		    {Handle, conv(Other, FileNo)}
	    end
    end.

%% -> {NewHandle, Reply}, 
%%    Reply = {Cont, Binaries, Bad} (Bad >= 0) | {error, Reason} | eof
mf_int_chunk_read_only(Handle, 0, Bin, N) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk_read_only(Handle, {FirstF, 0}, Bin, N);
mf_int_chunk_read_only(#handle{curF = FileNo, cur_fdc = FdC, cur_name=FileName}
                       = Handle, {FileNo, Pos}, Bin, N) ->
    {NewFdC, Reply} = do_chunk_read_only(FdC, FileName, Pos, Bin, N),
    {Handle#handle{cur_fdc = NewFdC}, conv(Reply, FileNo)};
mf_int_chunk_read_only(Handle, {FileNo, Pos}, Bin, N) ->
    FName = add_ext(Handle#handle.filename, FileNo),
    NFileNo = inc(FileNo, Handle#handle.maxF),
    case catch int_open(FName, true, read_only, any) of
	{error, _Reason} ->
	    case is_quiet() of
		true -> ok;
		_ -> error_logger:info_msg("disk_log: chunk error. File ~tp missing.\n\n",
					   [FName])
	    end,
	    mf_int_chunk_read_only(Handle, {NFileNo, 0}, [], N);
	{ok, {_Alloc, FdC, _HeadSize, _FileSize}} ->
	    case do_chunk_read_only(FdC, FName, Pos, Bin, N) of
		{NewFdC, eof} ->
		    _ = file:close(NewFdC#cache.fd),
		    mf_int_chunk_read_only(Handle, {NFileNo,0}, [], N);
		{NewFdC, Other} ->
		    _ = file:close(NewFdC#cache.fd),
		    {Handle, conv(Other, FileNo)}
	    end
    end.

%% -> {ok, Cont} | Error
mf_int_chunk_step(Handle, 0, Step) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk_step(Handle, {FirstF, 0}, Step);
mf_int_chunk_step(Handle, {FileNo, _Pos}, Step) ->
    NFileNo = inc(FileNo, Handle#handle.maxF, Step),
    FileName = add_ext(Handle#handle.filename, NFileNo),
    case file:read_file_info(FileName) of
	{ok, _FileInfo} ->	
	    {ok, #continuation{pos = {NFileNo, 0}, b = []}};
	_Error ->
	    {error, end_of_log}
    end.

%% -> {Reply, handle()}; Reply = ok | Error
mf_write_cache(#handle{filename = FName, cur_fdc = FdC} = Handle) ->
    erase(write_cache_timer_is_running),
    #cache{fd = Fd, c = C} = FdC,
    {Reply, NewFdC} = write_cache(Fd, FName, C),
    {Reply, Handle#handle{cur_fdc = NewFdC}}.

%% -> {Reply, handle()}; Reply = ok | Error
mf_sync(#handle{filename = FName, cur_fdc = FdC} = Handle) ->
    {Reply, NewFdC} = fsync(FdC, FName),
    {Reply, Handle#handle{cur_fdc = NewFdC}}.

%% -> ok | throw(FileError)
mf_int_close(#handle{filename = FName, curF = CurF, cur_name = FileName, 
		     cur_fdc = CurFdC, cur_cnt = CurCnt}, Mode) ->
    close(CurFdC, FileName, Mode),
    write_index_file(Mode, FName, CurF, CurF, CurCnt),
    ok.

%% -> {ok, handle(), Cnt} | throw(FileError)
mf_ext_open(FName, MaxB, MaxF, Repair, Mode, Head, Version) -> 
    {First, Sz, TotSz, NFiles} = read_index_file(Repair, FName, MaxF),
    write_size_file(Mode, FName, MaxB, MaxF, Version),
    NewMaxF = if 
		  NFiles > MaxF ->
		      {MaxF, NFiles};
		  true ->
		      MaxF
	      end,
    {ok, FdC, FileName, Lost, {NoItems, NoBytes}, CurB} = 
	ext_file_open(FName, First, 0, 0, Head, Repair, Mode),
    CurCnt = Sz + NoItems - Lost,
    {ok, #handle{filename = FName, maxB = MaxB, cur_name = FileName, 
		 maxF = NewMaxF, cur_cnt = CurCnt, acc_cnt = -Sz,
		 curF = First, cur_fdc = FdC, firstPos = NoBytes,
		 curB = CurB, noFull = 0, accFull = 0},
     TotSz + CurCnt}.

%% -> {ok, handle(), Lost} 
%%   | {error, Error, handle()}
%%   | throw(FatalError)
%% Fatal errors should always terminate the log.
mf_ext_inc(Handle, Head) -> 
    #handle{filename = FName, cur_cnt = CurCnt, cur_name = FileName, 
	    acc_cnt = AccCnt, curF = CurF, maxF = MaxF, cur_fdc = CurFdC, 
	    noFull = NoFull} = Handle,
    case catch wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) of
	{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost} ->
	    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF, 
				    cur_name = NewFileName,
				    cur_cnt = Nh, acc_cnt = AccCnt + CurCnt, 
				    maxF = NewMaxF, firstPos = FirstPos, 
				    curB = FirstPos, noFull = NoFull + 1},
	    case catch fclose(CurFdC, FileName) of
		ok ->
		    {ok, Handle1, Lost};
		Error -> % Error in the last file, new file opened.
		    {error, Error, Handle1}
	    end;
	Error ->
	    {error, Error, Handle}
    end.

%% -> {ok, handle(), Logged, Lost, NoWraps} | {ok, handle(), Logged} 
%%    | {error, Error, handle(), Logged, Lost}

%% The returned handle is not always valid -
%% something may have been written before things went wrong.
mf_ext_log(Handle, Bins, Head) ->
    mf_ext_log(Handle, Bins, Head, 0, []).

mf_ext_log(Handle, [], _Head, No, []) ->
    {ok, Handle, No};
mf_ext_log(Handle, [], _Head, No, Wraps0) ->
    Wraps = reverse(Wraps0),
    {ok, Handle, No, sum(Wraps), Wraps};
mf_ext_log(Handle, Bins, Head, No0, Wraps) ->
    #handle{curB = CurB, maxB = MaxB, cur_name = FileName, cur_fdc = CurFdC, 
            firstPos = FirstPos0, cur_cnt = CurCnt} = Handle,
    {FirstBins, LastBins, NoBytes, N} = 
	ext_split_bins(CurB, MaxB, FirstPos0, Bins),
    case FirstBins of
	[] ->
            #handle{filename = FName, curF = CurF, maxF = MaxF, 
                    acc_cnt = AccCnt, noFull = NoFull} = Handle,
	    case catch wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) of
		{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost} ->
		    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF,
					    cur_cnt = Nh, 
					    cur_name = NewFileName,
					    acc_cnt = AccCnt + CurCnt, 
					    maxF = NewMaxF, 
					    curB = FirstPos, 
					    firstPos = FirstPos,
					    noFull = NoFull + 1},
		    case catch fclose(CurFdC, FileName) of
			ok ->
			    mf_ext_log(Handle1, Bins, Head, No0 + Nh, 
				       [Lost | Wraps]);
			Error ->
			    Lost1 = Lost + sum(Wraps),
			    {error, Error, Handle1, No0 + Nh, Lost1}
		    end;
		Error ->
		    {error, Error, Handle, No0, sum(Wraps)}
	    end;
	_ ->
	    case fwrite(CurFdC, FileName, FirstBins, NoBytes) of
                {ok, NewCurFdC} ->
		    Handle1 = Handle#handle{cur_fdc = NewCurFdC, 
                                            curB = CurB + NoBytes, 
					    cur_cnt = CurCnt + N},
		    mf_ext_log(Handle1, LastBins, Head, No0 + N, Wraps);
		{Error, NewCurFdC} ->
		    Handle1 = Handle#handle{cur_fdc = NewCurFdC},
		    {error, Error, Handle1, No0, sum(Wraps)}
	    end
    end.

wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) ->
    {NewF, NewMaxF} = inc_wrap(FName, CurF, MaxF),
    {ok, NewFdC, NewFileName, Lost, {Nh, FirstPos}, _FileSize} = 
	ext_file_open(FName, NewF, CurF, CurCnt, Head),
    {NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost}.

%% -> ok | throw(FileError)
mf_ext_close(#handle{filename = FName, curF = CurF,
		     cur_fdc = CurFdC, cur_cnt = CurCnt}, Mode) ->
    Res = (catch fclose(CurFdC, FName)),
    write_index_file(Mode, FName, CurF, CurF, CurCnt),
    Res.

%% -> {ok, handle()} | throw(FileError)
change_size_wrap(Handle, {NewMaxB, NewMaxF}, Version) ->
    FName = Handle#handle.filename,
    {_MaxB, MaxF} = get_wrap_size(Handle),
    write_size_file(read_write, FName, NewMaxB, NewMaxF, Version),
    if
	NewMaxF > MaxF ->
	    remove_files(FName, MaxF + 1, NewMaxF),
	    {ok, Handle#handle{maxB = NewMaxB, maxF = NewMaxF}};
	NewMaxF < MaxF ->
	    {ok, Handle#handle{maxB = NewMaxB, maxF = {NewMaxF, MaxF}}};
	true ->
	    {ok, Handle#handle{maxB = NewMaxB, maxF = NewMaxF}}
    end.

%%-----------------------------------------------------------------
%% Misc functions
%%-----------------------------------------------------------------
%% -> {ok, FdC, FileName, Lost, HeadSize, FileSize} | throw(Error)
int_file_open(FName, NewFile, OldFile, OldCnt, Head) ->
    Repair = truncate, Mode = read_write,
    int_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode).

%% -> {ok, FdC, FileName, Lost, HeadSize, FileSize} 
%%  | {repaired, FdC, FileName, Rec, Bad, FileSize} 
%%  | throw(Error)
int_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode) ->
    N = add_ext(FName, NewFile),
    case int_open(N, Repair, Mode, Head) of
	{ok, {_Alloc, FdC, HeadSize, FileSize}} ->
	    Lost = write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {ok, FdC, N, Lost, HeadSize, FileSize};
	{repaired, FdC, Recovered, BadBytes, FileSize} ->
	    write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {repaired, FdC, N, Recovered, BadBytes, FileSize}
    end.

%% -> {ok, FdC, FileName, Lost, HeadSize, FileSize} | throw(Error)
ext_file_open(FName, NewFile, OldFile, OldCnt, Head) ->
    Repair = truncate, Mode = read_write,
    ext_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode).

ext_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode) ->
    FileName = add_ext(FName, NewFile),
    {ok, {_Alloc, FdC, HeadSize, FileSize}} = 
        ext_open(FileName, Repair, Mode, Head),
    Lost = write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
    {ok, FdC, FileName, Lost, HeadSize, FileSize}.

%%-----------------------------------------------------------------
%% The old file format for index file (CurFileNo > 0), Version 0:
%%
%% CurFileNo SizeFile1 SizeFile2  ... SizeFileN
%%   1 byte   4 bytes    4 bytes       4 bytes
%%
%% The new file format for index file (NewFormat = 0), version 1:
%%
%% NewFormat CurFileNo SizeFile1 SizeFile2  ... SizeFileN
%%   1 byte   4 bytes    4 bytes       4 bytes
%%
%% The current file format for index file (sizes in bytes), version 2:
%%
%% 0 (1) 0 (4) FileFormatVersion (1) CurFileNo (4) SizeFile1 (8) ...
%%
%% (SizeFileI refers to number of items on the log file.)
%%-----------------------------------------------------------------

-define(index_file_name(F), add_ext(F, "idx")).

read_index_file(truncate, FName, MaxF) ->
    remove_files(FName, 2, MaxF),
    _ = file:delete(?index_file_name(FName)),
    {1, 0, 0, 0};
read_index_file(_, FName, _MaxF) ->
    read_index_file(FName).

%% Used by wrap_log_reader.
%% -> {CurFileNo, CurFileSz, TotSz, NoFiles} | throw(FileError)
%%  where TotSz does not include CurFileSz.

read_index_file(FName) ->
    FileName = ?index_file_name(FName),
    case open_read(FileName) of
	{ok, Fd} ->
	    R = case file:read(Fd, ?MAX_CHUNK_SIZE) of
		    {ok, <<0, 0:32, Version, CurF:32, Tail/binary>>}
		             when Version =:= ?VERSION, 
				  0 < CurF, CurF < ?MAX_FILES -> 
			parse_index(CurF, Version, 1, Tail, Fd, 0, 0, 0);
		    {ok, <<0, CurF:32, Tail/binary>>} 
		             when 0 < CurF, CurF < ?MAX_FILES -> 
			parse_index(CurF, 1, 1, Tail, Fd, 0, 0, 0);
		    {ok, <<CurF, Tail/binary>>} when 0 < CurF -> 
			parse_index(CurF, 1, 1, Tail, Fd, 0, 0, 0);
		    _ErrorOrEof ->
			{1, 0, 0, 0}
		end,
	    _ = file:close(Fd),
	    R;
	_Error ->
	    {1, 0, 0, 0}
    end.

parse_index(CurF, V, CurF, <<CurSz:64, Tail/binary>>, Fd, _, TotSz, NFiles)
          when V =:= ?VERSION ->
    parse_index(CurF, V, CurF+1, Tail, Fd, CurSz, TotSz, NFiles+1);
parse_index(CurF, V, N, <<Sz:64, Tail/binary>>, Fd, CurSz, TotSz, NFiles)
          when V =:= ?VERSION ->
    parse_index(CurF, V, N+1, Tail, Fd, CurSz, TotSz + Sz, NFiles+1);
parse_index(CurF, V, CurF, <<CurSz:32, Tail/binary>>, Fd, _, TotSz, NFiles)
          when V < ?VERSION ->
    parse_index(CurF, V, CurF+1, Tail, Fd, CurSz, TotSz, NFiles+1);
parse_index(CurF, V, N, <<Sz:32, Tail/binary>>, Fd, CurSz, TotSz, NFiles)
          when V < ?VERSION ->
    parse_index(CurF, V, N+1, Tail, Fd, CurSz, TotSz + Sz, NFiles+1);
parse_index(CurF, V, N, B, Fd, CurSz, TotSz, NFiles) ->
    case file:read(Fd, ?MAX_CHUNK_SIZE) of
	eof when 0 =:= byte_size(B) ->
	    {CurF, CurSz, TotSz, NFiles};	    
	{ok, Bin} ->
            NewB = list_to_binary([B, Bin]),
	    parse_index(CurF, V, N, NewB, Fd, CurSz, TotSz, NFiles);
	_ErrorOrEof ->
	    {1, 0, 0, 0}
    end.

%% Returns: Number of lost items (if an old file was truncated)
%% -> integer() | throw(FileError)
write_index_file(read_only, _FName, _NewFile, _OldFile, _OldCnt) ->
    0;
write_index_file(read_write, FName, NewFile, OldFile, OldCnt) ->
    FileName = ?index_file_name(FName),
    case open_update(FileName) of
	{ok, Fd} ->
	    {Offset, SzSz} = 
		case file:read(Fd, 6) of
		    eof ->
			Bin = <<0, 0:32, ?VERSION, NewFile:32>>,
			fwrite_close2(Fd, FileName, Bin),
			{10, 8};
		    {ok, <<0, 0:32, _Version>>} ->
			pwrite_close2(Fd, FileName, 6, <<NewFile:32>>),
			{10, 8};
		    {ok, <<0, _/binary>>} ->
			pwrite_close2(Fd, FileName, 1, <<NewFile:32>>),
			{5, 4};
		    {ok, <<_,_/binary>>} ->
                        %% Very old format, convert to the latest format!
			case file:read_file(FileName) of
			    {ok, <<_CurF, Tail/binary>>} ->
				{ok, _} = position_close2(Fd, FileName, bof),
				Bin = <<0, 0:32, ?VERSION, NewFile:32>>,
				NewTail = to_8_bytes(Tail, [], FileName, Fd),
				fwrite_close2(Fd, FileName, [Bin | NewTail]),
				{10, 8};
			    Error ->
				file_error_close(Fd, FileName, Error)
			end;
		    Error ->
			file_error_close(Fd, FileName, Error)
		end,

	    NewPos = Offset + (NewFile - 1)*SzSz,
	    OldCntBin = <<OldCnt:SzSz/unit:8>>,
	    if
		OldFile > 0 ->
		    R = file:pread(Fd, NewPos, SzSz),
		    OldPos = Offset + (OldFile - 1)*SzSz,
		    pwrite_close2(Fd, FileName, OldPos, OldCntBin),
		    _ = file:close(Fd),
		    case R of
			{ok, <<Lost:SzSz/unit:8>>} -> Lost;
			{ok, _} -> 
                            throw({error, {invalid_index_file, FileName}});
			eof    -> 0;
			Error2 -> file_error(FileName, Error2)
		    end;
		true -> 	
		    pwrite_close2(Fd, FileName, NewPos, OldCntBin),
		    _ = file:close(Fd),
		    0
	    end;
	E -> 
	    file_error(FileName, E)
    end.

-dialyzer({no_improper_lists, to_8_bytes/4}).
to_8_bytes(<<N:32,T/binary>>, NT, FileName, Fd) ->
    to_8_bytes(T, [NT | <<N:64>>], FileName, Fd);
to_8_bytes(B, NT, _FileName, _Fd) when byte_size(B) =:= 0 ->
    NT;
to_8_bytes(_B, _NT, FileName, Fd) ->
    _ = file:close(Fd),
    throw({error, {invalid_index_file, FileName}}).

%% -> ok | throw(FileError)
index_file_trunc(FName, N) ->
    FileName = ?index_file_name(FName),
    case open_update(FileName) of
	{ok, Fd} ->
	    case file:read(Fd, 6) of
		eof ->
		    _ = file:close(Fd),
		    ok;
		{ok, <<0, 0:32, Version>>} when Version =:= ?VERSION ->
		    truncate_index_file(Fd, FileName, 10, 8, N);
		{ok, <<0, _/binary>>} ->
		    truncate_index_file(Fd, FileName, 5, 4, N);
		{ok, <<_, _/binary>>} -> % cannot happen
		    truncate_index_file(Fd, FileName, 1, 4, N);
		Error ->
		    file_error_close(Fd, FileName, Error)
	    end;
	Error ->
	    file_error(FileName, Error)
    end.

truncate_index_file(Fd, FileName, Offset, N, SzSz) ->
    Pos = Offset + N*SzSz,
    case Pos > file_size(FileName) of
	true ->
	    ok = file:close(Fd);
	false ->
	    truncate_at_close2(Fd, FileName, {bof, Pos}),
	    ok = file:close(Fd)
    end,
    ok.
	    
print_index_file(File) ->
    io:format("-- Index begin --~n"),
    case file:read_file(File) of
	{ok, <<0, 0:32, Version, CurF:32, Tail/binary>>} 
	         when Version =:= ?VERSION, 0 < CurF, CurF < ?MAX_FILES ->
	    io:format("cur file: ~w~n", [CurF]),
	    loop_index(1, Version, Tail);
	{ok, <<0, CurF:32, Tail/binary>>} when 0 < CurF, CurF < ?MAX_FILES ->
	    io:format("cur file: ~w~n", [CurF]),
	    loop_index(1, 1, Tail);
	{ok, <<CurF, Tail/binary>>} when 0 < CurF ->
	    io:format("cur file: ~w~n", [CurF]),
	    loop_index(1, 1, Tail);
	_Else ->
	    ok
    end,
    io:format("-- end --~n").    

loop_index(N, V, <<Sz:64, Tail/binary>>) when V =:= ?VERSION ->
    io:format(" ~p  items: ~w~n", [N, Sz]),
    loop_index(N+1, V, Tail);
loop_index(N, V, <<Sz:32, Tail/binary>>) when V < ?VERSION ->
    io:format(" ~p  items: ~w~n", [N, Sz]),
    loop_index(N+1, V, Tail);
loop_index(_, _, _) ->
    ok.

-define(size_file_name(F), add_ext(F, "siz")).

%% Version 0: no size file
%% Version 1: <<MaxSize:32, MaxFiles:32>>
%% Version 2: <<Version:8, MaxSize:64, MaxFiles:32>>

%% -> ok | throw(FileError)
write_size_file(read_only, _FName, _NewSize, _NewMaxFiles, _Version) ->
    ok;
write_size_file(read_write, FName, NewSize, NewMaxFiles, Version) ->
    FileName = ?size_file_name(FName),
    Bin = if 
	      Version =:=  ?VERSION ->
		  <<Version, NewSize:64, NewMaxFiles:32>>;
	      true ->
		  <<NewSize:32, NewMaxFiles:32>>
	  end,
    case file:write_file(FileName, Bin) of
	ok -> 
	    ok;
	E -> 
	    file_error(FileName, E)
    end.
	    
%% -> {NoBytes, NoFiles}.
read_size_file(FName) ->
    {Size,_Version} = read_size_file_version(FName),
    Size.

%% -> {{NoBytes, NoFiles}, Version}, Version = integer() | undefined
read_size_file_version(FName) ->
    case file:read_file(?size_file_name(FName)) of
	{ok, <<Version, Size:64, MaxFiles:32>>} when Version =:= ?VERSION ->
	    {{Size, MaxFiles}, Version};
	{ok, <<Size:32, MaxFiles:32>>} ->
	    {{Size, MaxFiles}, 1};
	_ ->
	    %% The oldest version too...
	    {{0, 0}, ?VERSION}
    end.

conv({More, Terms}, FileNo) when is_record(More, continuation) ->
    Cont = More#continuation{pos = {FileNo, More#continuation.pos}},
    {Cont, Terms};
conv({More, Terms, Bad}, FileNo) when is_record(More, continuation) ->
    Cont = More#continuation{pos = {FileNo, More#continuation.pos}},
    {Cont, Terms, Bad};
conv(Other, _) ->
    Other.

find_first_file(#handle{filename = FName, curF = CurF, maxF = MaxF}) ->
    fff(FName, inc(CurF, MaxF), CurF, MaxF).

fff(_FName, CurF, CurF, _MaxF) -> CurF;
fff(FName, MaybeFirstF, CurF, MaxF) ->
    N = add_ext(FName, MaybeFirstF),
    case file:read_file_info(N) of
	{ok, _} -> MaybeFirstF;
	_ -> fff(FName, inc(MaybeFirstF, MaxF), CurF, MaxF)
    end.
	    
%% -> {iolist(), LastBins, NoBytes, NoTerms}
ext_split_bins(CurB, MaxB, FirstPos, Bins) ->
    MaxBs = MaxB - CurB, IsFirst = CurB =:= FirstPos,
    ext_split_bins(MaxBs, IsFirst, [], Bins, 0, 0).

-dialyzer({no_improper_lists, ext_split_bins/6}).
ext_split_bins(MaxBs, IsFirst, First, [X | Last], Bs, N) ->
    NBs = Bs + byte_size(X),
    if
        NBs =< MaxBs ->
	    ext_split_bins(MaxBs, IsFirst, [First | X], Last, NBs, N+1);
	IsFirst, First =:= [] ->
            % To avoid infinite loop - we allow the file to be
   	    % too big if it's just one item on the file.
	    {[X], Last, NBs, N+1}; 
	true ->
	    {First, [X | Last], Bs, N}
    end;
ext_split_bins(_, _, First, [], Bs, N) ->
    {First, [], Bs, N}.

%% -> {iolist(), LastBins, NoBytes, NoTerms}
int_split_bins(CurB, MaxB, FirstPos, Bins) ->
    MaxBs = MaxB - CurB, IsFirst = CurB =:= FirstPos,
    int_split_bins(MaxBs, IsFirst, [], Bins, 0, 0).

-dialyzer({no_improper_lists, int_split_bins/6}).
int_split_bins(MaxBs, IsFirst, First, [X | Last], Bs, N) ->
    Sz = byte_size(X),
    NBs = Bs + Sz + ?HEADERSZ,
    BSz = <<Sz:?SIZESZ/unit:8>>,
    XB = case Sz < ?MIN_MD5_TERM of
             true ->
                 [BSz, ?BIGMAGICHEAD | X];
             false ->
                 MD5 = erlang:md5(BSz),
                 [BSz, ?BIGMAGICHEAD, MD5 | X]
         end,
    if
        NBs =< MaxBs ->
	    int_split_bins(MaxBs, IsFirst, [First | XB], Last, NBs, N+1);
	IsFirst, First =:= [] ->
            % To avoid infinite loop - we allow the file to be
   	    % too big if it's just one item on the file.
	    {[XB], Last, NBs, N+1}; 
	true ->
	    {First, [X | Last], Bs, N}
    end;
int_split_bins(_, _, First, [], Bs, N) ->
    {First, [], Bs, N}.

%% -> {NewCurrentFileNo, MaxFilesToBe} | throw(FileError)
inc_wrap(FName, CurF, MaxF) ->
    case MaxF of
	%% Number of max files has changed
	{NewMaxF, OldMaxF} ->
	    if 
		CurF >= NewMaxF ->
		    %% We are at or above the new number of files
		    remove_files(FName, CurF + 1, OldMaxF),
		    if 
			CurF > NewMaxF ->
			    %% The change was done while the current file was 
			    %% greater than the new number of files.
			    %% The index file is not trunctated here, since
			    %% writing the index file while opening the file
			    %% with index 1 will write the value for the file
			    %% with extension CurF as well. Next time the 
			    %% limit is reached, the index file will be
			    %% truncated.
			    {1, {NewMaxF, CurF}};
			true ->
			    %% The change was done while the current file was 
			    %% less than the new number of files.
			    %% Remove the files from the index file too
			    index_file_trunc(FName, NewMaxF), 
			    {1, NewMaxF}
		    end;
		true ->
		    %% We haven't reached the new limit yet
		    NewFt = inc(CurF, NewMaxF),
		    {NewFt, MaxF}
	    end;
	MaxF ->
	    %% Normal case.
	    NewFt = inc(CurF, MaxF),
	    {NewFt, MaxF}
    end.

inc(N, {_NewMax, OldMax}) -> inc(N, OldMax, 1);
inc(N, Max) -> inc(N, Max, 1).

inc(N, Max, Step) ->
    Nx = (N + Step) rem Max,
    if
	Nx > 0 -> Nx;
	true -> Nx + Max
    end.


file_size(Fname) ->
    {ok, Fi} = file:read_file_info(Fname),
    Fi#file_info.size.

%% -> ok | throw(FileError)
%% Tries to remove each file with name FName.I, N<=I<=Max.
remove_files(FName, N, Max) ->
    remove_files(FName, N, Max, ok).

remove_files(_FName, N, Max, ok) when N > Max ->
    ok;
remove_files(_FName, N, Max, {FileName, Error}) when N > Max ->
    file_error(FileName, Error);
remove_files(FName, N, Max, Reply) ->
    FileName = add_ext(FName, N),
    NewReply = case file:delete(FileName) of
		   ok -> Reply;
		   {error, enoent} -> Reply;
		   Error -> {FileName, Error}
	       end,
    remove_files(FName, N + 1, Max, NewReply).

%% -> {MaxBytes, MaxFiles}
get_wrap_size(#handle{maxB = MaxB, maxF = MaxF}) ->
    case MaxF of
	{NewMaxF,_} -> {MaxB, NewMaxF};
	MaxF        -> {MaxB, MaxF}
    end.

add_ext(Name, Ext) ->
    concat([Name, ".", Ext]).

open_read(FileName) ->
    file:open(FileName, [raw, binary, read]).

open_update(FileName) ->
    file:open(FileName, [raw, binary, read, write]).

open_truncate(FileName) ->
    file:open(FileName, [raw, binary, write]).

%%% Functions that access files, and throw on error. 

-define(TIMEOUT, 2000). % ms

%% -> {Reply, cache()}; Reply = ok | Error
fwrite(FdC, _FN, _B, 0) ->
    {ok, FdC};  % avoid starting a timer for empty writes
fwrite(#cache{fd = Fd, c = C, sz = Sz} = FdC, FileName, B, Size) ->
    Sz1 = Sz + Size,
    C1 = cache_append(C, B),
    if Sz1 > ?MAX_FWRITE_CACHE ->
            write_cache(Fd, FileName, C1);
       true ->
            maybe_start_timer(C),
            {ok, FdC#cache{sz = Sz1, c = C1}}
    end.

cache_append([], B) -> B;
cache_append(C, B) -> [C | B].

%% if the cache was empty, start timer (unless it's already running)
maybe_start_timer([]) ->
    case get(write_cache_timer_is_running) of
        true ->
            ok;
        _ ->
            put(write_cache_timer_is_running, true),
            erlang:send_after(?TIMEOUT, self(), {self(), write_cache}),
            ok
    end;
maybe_start_timer(_C) ->
    ok.

fwrite_header(Fd, B, Size) ->
    {ok, #cache{fd = Fd, sz = Size, c = B}}.

%% -> {NewFdC, Reply}; Reply = ok | Error
pread(#cache{fd = Fd, c = C}, FileName, Position, MaxBytes) ->
    Reply = write_cache(Fd, FileName, C),
    case Reply of
	{ok, NewFdC} ->
	    case file:pread(Fd, Position, MaxBytes) of
		{error, Error} -> 
		    {NewFdC, catch file_error(FileName, {error, Error})};
		R -> 
		    {NewFdC, R}
	    end;
	{Error, NewFdC} ->
	    {NewFdC, Error}
    end.

%% -> {ok, cache(), Pos} | {Error, cache()}
position(#cache{fd = Fd, c = C}, FileName, Pos) ->
    Reply = write_cache(Fd, FileName, C),
    case Reply of
	{ok, NewFdC} ->
	    case position2(Fd, FileName, Pos) of
		{ok, Loc} -> 
		    {ok, NewFdC, Loc};
		Error -> 
		    {Error, NewFdC}
	    end;
	_Error ->
	    Reply
    end.
	    
position_close(#cache{fd = Fd, c = C}, FileName, Pos) ->
    NewFdC = write_cache_close(Fd, FileName, C),
    {ok, Loc} = position_close2(Fd, FileName, Pos),
    {NewFdC, Loc}.

fsync(#cache{fd = Fd, c = C}, FileName) ->
    Reply = write_cache(Fd, FileName, C),    
    case Reply of
	{ok, NewFdC} ->
	    case file:sync(Fd) of
		ok -> 
		    Reply;
		Error -> 
		    {catch file_error(FileName, Error), NewFdC}
	    end;
	_Error ->
	    Reply
    end.

%% -> {Reply, NewFdC}; Reply = ok | Error
truncate_at(FdC, FileName, Pos) ->
    case position(FdC, FileName, Pos) of
	{ok, NewFdC, _Pos} ->
	    case file:truncate(NewFdC#cache.fd) of
		ok ->
		    {ok, NewFdC};
		Error ->
		    {catch file_error(FileName, Error), NewFdC}
	    end;
	Reply ->
	    Reply
    end.

fwrite_close2(Fd, FileName, B) ->
    case file:write(Fd, B) of
	ok    -> ok;
	Error -> file_error_close(Fd, FileName, Error)
    end.

pwrite_close2(Fd, FileName, Position, B) ->
    case file:pwrite(Fd, Position, B) of
	ok -> ok;
	{error,Error} -> file_error(FileName, {error, Error})
    end.

position2(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> catch file_error(FileName, {error, Error});
	OK -> OK
    end.

position_close2(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error_close(Fd, FileName, {error, Error});
	OK -> OK
    end.
	    
truncate_at_close2(Fd, FileName, Pos) ->
    {ok, _} = position_close2(Fd, FileName, Pos),
    case file:truncate(Fd) of
	ok    -> ok;
	Error -> file_error_close(Fd, FileName, Error)
    end.

fclose(#cache{fd = Fd, c = C}, FileName) ->
    %% The cache is empty if the file was opened in read_only mode.
    _ = write_cache_close(Fd, FileName, C),
    file:close(Fd).

set_quiet(Bool) ->
    put(quiet, Bool).

is_quiet() ->
    get(quiet) =:= true.

%% -> {Reply, #cache{}}; Reply = ok | Error
write_cache(Fd, _FileName, []) ->
    {ok, #cache{fd = Fd}};
write_cache(Fd, FileName, C) ->
    case file:write(Fd, C) of
        ok    -> {ok, #cache{fd = Fd}};
        Error -> {catch file_error(FileName, Error), #cache{fd = Fd}}
    end.

-spec write_cache_close(file:fd(), file:filename(), iodata()) -> #cache{}. % | throw(Error)

write_cache_close(Fd, _FileName, []) ->
    #cache{fd = Fd};
write_cache_close(Fd, FileName, C) ->
    case file:write(Fd, C) of
        ok    -> #cache{fd = Fd};
        Error -> file_error_close(Fd, FileName, Error)
    end.

-spec file_error(file:filename(), {'error', file:posix()}) -> no_return().

file_error(FileName, {error, Error}) ->
    throw({error, {file_error, FileName, Error}}).

-spec file_error_close(file:fd(), file:filename(), {'error', file:posix()}) -> no_return().

file_error_close(Fd, FileName, {error, Error}) ->
    _ = file:close(Fd),
    throw({error, {file_error, FileName, Error}}).
