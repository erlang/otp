%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(zlib).

-export([open/0,close/1,deflateInit/1,deflateInit/2,deflateInit/6,
	 deflateSetDictionary/2,deflateReset/1,deflateParams/3,
	 deflate/2,deflate/3,deflateEnd/1,
	 inflateInit/1,inflateInit/2,inflateSetDictionary/2,
	 inflateSync/1,inflateReset/1,inflate/2,inflateEnd/1,
	 inflateChunk/1, inflateChunk/2,
	 setBufSize/2,getBufSize/1,
	 crc32/1,crc32/2,crc32/3,adler32/2,adler32/3,getQSize/1,
	 crc32_combine/4,adler32_combine/4,
	 compress/1,uncompress/1,zip/1,unzip/1,
	 gzip/1,gunzip/1]).

-export_type([zstream/0, zlevel/0, zwindowbits/0, zmemlevel/0, zstrategy/0]).

%% flush argument encoding
-define(Z_NO_FLUSH,      0).
-define(Z_SYNC_FLUSH,    2).
-define(Z_FULL_FLUSH,    3).
-define(Z_FINISH,        4).

%% compression level
-define(Z_NO_COMPRESSION,         0).
-define(Z_BEST_SPEED,             1).
-define(Z_BEST_COMPRESSION,       9).
-define(Z_DEFAULT_COMPRESSION,  (-1)).

%% compresssion strategy
-define(Z_FILTERED,            1).
-define(Z_HUFFMAN_ONLY,        2).
-define(Z_RLE,                 3).
-define(Z_DEFAULT_STRATEGY,    0).

%% deflate compression method
-define(Z_DEFLATED,  8).

-define(Z_NULL, 0).

-define(MAX_WBITS, 15).

%% gzip defs (rfc 1952)

-define(ID1, 16#1f).
-define(ID2, 16#8b).

-define(FTEXT,     16#01).
-define(FHCRC,     16#02).
-define(FEXTRA,    16#04).
-define(FNAME,     16#08).
-define(FCOMMENT,  16#10).
-define(RESERVED,  16#E0).

-define(OS_MDDOS,   0).
-define(OS_AMIGA,   1).
-define(OS_OPENVMS, 2).
-define(OS_UNIX,    3).
-define(OS_VMCMS,   4).
-define(OS_ATARI,   5).
-define(OS_OS2,     6).
-define(OS_MAC,     7).
-define(OS_ZSYS,    8).
-define(OS_CPM,     9).
-define(OS_TOP20,  10).
-define(OS_NTFS,   11).
-define(OS_QDOS,   12).
-define(OS_ACORN,  13).
-define(OS_UNKNOWN,255).

-define(DEFLATE_INIT,    1).
-define(DEFLATE_INIT2,   2).
-define(DEFLATE_SETDICT, 3).
-define(DEFLATE_RESET,   4).
-define(DEFLATE_END,     5).
-define(DEFLATE_PARAMS,  6).
-define(DEFLATE,         7).

-define(INFLATE_INIT,    8).
-define(INFLATE_INIT2,   9).
-define(INFLATE_SETDICT, 10).
-define(INFLATE_SYNC,    11).
-define(INFLATE_RESET,   12).
-define(INFLATE_END,     13).
-define(INFLATE,         14).
-define(INFLATE_CHUNK,   25).

-define(CRC32_0,         15).
-define(CRC32_1,         16).
-define(CRC32_2,         17).

-define(SET_BUFSZ,       18).
-define(GET_BUFSZ,       19).
-define(GET_QSIZE,       20).

-define(ADLER32_1,       21).
-define(ADLER32_2,       22).

-define(CRC32_COMBINE,   23).
-define(ADLER32_COMBINE, 24).

%%------------------------------------------------------------------------

%% Main data types of the file
-type zstream()     :: port().

%% Auxiliary data types of the file
-type zlevel()      :: 'none' | 'default' | 'best_compression' | 'best_speed' 
                     | 0..9.
-type zmethod()     :: 'deflated'.
-type zwindowbits() :: -15..-8 | 8..47.
-type zmemlevel()   :: 1..9.
-type zstrategy()   :: 'default' | 'filtered' | 'huffman_only' | 'rle'.

%%------------------------------------------------------------------------

%% open a z_stream
-spec open() -> zstream().
open() ->
    open_port({spawn, "zlib_drv"}, [binary]).

%% close and release z_stream
-spec close(Z) -> 'ok' when
      Z :: zstream().
close(Z) ->
    try
	true = port_close(Z),
	receive	      %In case the caller is the owner and traps exits
	    {'EXIT',Z,_} -> ok
	after 0 -> ok
	end
    catch _:_ -> erlang:error(badarg)
    end.

-spec deflateInit(Z) -> 'ok' when
      Z :: zstream().
deflateInit(Z) ->
    call(Z, ?DEFLATE_INIT, <<?Z_DEFAULT_COMPRESSION:32>>).

-spec deflateInit(Z, Level) -> 'ok' when
      Z :: zstream(),
      Level :: zlevel().
deflateInit(Z, Level) ->
    call(Z, ?DEFLATE_INIT, <<(arg_level(Level)):32>>).

-spec deflateInit(Z, Level, Method,
		  WindowBits, MemLevel, Strategy) -> 'ok' when
      Z :: zstream(),
      Level :: zlevel(),
      Method :: zmethod(),
      WindowBits :: zwindowbits(),
      MemLevel :: zmemlevel(),
      Strategy :: zstrategy().
deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) ->
    call(Z, ?DEFLATE_INIT2, <<(arg_level(Level)):32, 
			     (arg_method(Method)):32,
			     (arg_bitsz(WindowBits)):32, 
			     (arg_mem(MemLevel)):32,
			     (arg_strategy(Strategy)):32>>).

-spec deflateSetDictionary(Z, Dictionary) -> Adler32 when
      Z :: zstream(),
      Dictionary :: iodata(),
      Adler32 :: integer().
deflateSetDictionary(Z, Dictionary) ->
    call(Z, ?DEFLATE_SETDICT, Dictionary).

-spec deflateReset(Z) -> 'ok' when
      Z :: zstream().
deflateReset(Z) ->
    call(Z, ?DEFLATE_RESET, []).

-spec deflateParams(Z, Level, Strategy) -> ok when
      Z :: zstream(),
      Level :: zlevel(),
      Strategy :: zstrategy().
deflateParams(Z, Level, Strategy) ->
    call(Z, ?DEFLATE_PARAMS, <<(arg_level(Level)):32, 
			      (arg_strategy(Strategy)):32>>).

-spec deflate(Z, Data) -> Compressed when
      Z :: zstream(),
      Data :: iodata(),
      Compressed :: iolist().
deflate(Z, Data) ->
    deflate(Z, Data, none).

-spec deflate(Z, Data, Flush) -> Compressed when
      Z :: zstream(),
      Data :: iodata(),
      Flush :: none | sync | full | finish,
      Compressed :: iolist().
deflate(Z, Data, Flush) ->
    try port_command(Z, Data) of
	true ->
	    _ = call(Z, ?DEFLATE, <<(arg_flush(Flush)):32>>),
	    collect(Z)
    catch 
	error:_Err ->
	    flush(Z),
	    erlang:error(badarg) 
    end.

-spec deflateEnd(Z) -> 'ok' when
      Z :: zstream().
deflateEnd(Z) ->
    call(Z, ?DEFLATE_END, []).    

-spec inflateInit(Z) -> 'ok' when
      Z :: zstream().
inflateInit(Z) ->
    call(Z, ?INFLATE_INIT, []).

-spec inflateInit(Z, WindowBits) -> 'ok' when
      Z :: zstream(),
      WindowBits :: zwindowbits().
inflateInit(Z, WindowBits) -> 
    call(Z, ?INFLATE_INIT2, <<(arg_bitsz(WindowBits)):32>>).

-spec inflateSetDictionary(Z, Dictionary) -> 'ok' when
      Z :: zstream(),
      Dictionary :: iodata().
inflateSetDictionary(Z, Dictionary) -> 
    call(Z, ?INFLATE_SETDICT, Dictionary).

-spec inflateSync(zstream()) -> 'ok'.
inflateSync(Z) -> 
    call(Z, ?INFLATE_SYNC, []).

-spec inflateReset(Z) -> 'ok' when
      Z :: zstream().
inflateReset(Z) -> 
    call(Z, ?INFLATE_RESET, []).

-spec inflate(Z, Data) -> Decompressed when
      Z :: zstream(),
      Data :: iodata(),
      Decompressed :: iolist().
inflate(Z, Data) ->
    try port_command(Z, Data) of
	true -> 
	    _ = call(Z, ?INFLATE, <<?Z_NO_FLUSH:32>>),
	    collect(Z)
    catch 
	error:_Err ->
	    flush(Z),
	    erlang:error(badarg) 
    end.

-spec inflateChunk(Z, Data) -> Decompressed | {more, Decompressed} when
      Z :: zstream(),
      Data :: iodata(),
      Decompressed :: iolist().
inflateChunk(Z, Data) ->
    try port_command(Z, Data) of
	true ->
        inflateChunk(Z)
    catch
	error:_Err ->
	    flush(Z),
	    erlang:error(badarg)
    end.

-spec inflateChunk(Z) -> Decompressed | {more, Decompressed} when
      Z :: zstream(),
      Decompressed :: iolist().
inflateChunk(Z) ->
    Status = call(Z, ?INFLATE_CHUNK, []),
    Data = receive
	{Z, {data, Bin}} ->
	    Bin
    after 0 ->
	    []
    end,

    case Status of
        Good when (Good == ok) orelse (Good == stream_end) ->
            Data;
        inflate_has_more ->
            {more, Data}
    end.

-spec inflateEnd(Z) -> 'ok' when
      Z :: zstream().
inflateEnd(Z) ->
    call(Z, ?INFLATE_END, []).

-spec setBufSize(Z, Size) -> 'ok' when
      Z :: zstream(),
      Size :: non_neg_integer().
setBufSize(Z, Size) ->
    call(Z, ?SET_BUFSZ, <<Size:32>>).

-spec getBufSize(Z) -> Size when
      Z :: zstream(),
      Size :: non_neg_integer().
getBufSize(Z) ->
    call(Z, ?GET_BUFSZ, []).

-spec crc32(Z) -> CRC when
      Z :: zstream(),
      CRC :: integer().
crc32(Z) ->
    call(Z, ?CRC32_0, []).

-spec crc32(Z, Data) -> CRC when
      Z :: zstream(),
      Data :: iodata(),
      CRC :: integer().
crc32(Z, Data) ->
    call(Z, ?CRC32_1, Data).

-spec crc32(Z, PrevCRC, Data) -> CRC when
      Z :: zstream(),
      PrevCRC :: integer(),
      Data :: iodata(),
      CRC :: integer().
crc32(Z, CRC, Data) ->
    call(Z, ?CRC32_2, [<<CRC:32>>, Data]).

-spec adler32(Z, Data) -> CheckSum when
      Z :: zstream(),
      Data :: iodata(),
      CheckSum :: integer().
adler32(Z, Data) ->
    call(Z, ?ADLER32_1, Data).

-spec adler32(Z, PrevAdler, Data) -> CheckSum when
      Z :: zstream(),
      PrevAdler :: integer(),
      Data :: iodata(),
      CheckSum :: integer().
adler32(Z, Adler, Data) when is_integer(Adler) ->
    call(Z, ?ADLER32_2, [<<Adler:32>>, Data]);
adler32(_Z, _Adler, _Data)  ->
    erlang:error(badarg).

-spec crc32_combine(Z, CRC1, CRC2, Size2) -> CRC when
      Z :: zstream(),
      CRC :: integer(),
      CRC1 :: integer(),
      CRC2 :: integer(),
      Size2 :: integer().
crc32_combine(Z, CRC1, CRC2, Len2) 
  when is_integer(CRC1), is_integer(CRC2), is_integer(Len2) ->
    call(Z, ?CRC32_COMBINE, <<CRC1:32, CRC2:32, Len2:32>>);
crc32_combine(_Z, _CRC1, _CRC2, _Len2) ->
    erlang:error(badarg).

-spec adler32_combine(Z, Adler1, Adler2, Size2) -> Adler when
      Z :: zstream(),
      Adler :: integer(),
      Adler1 :: integer(),
      Adler2 :: integer(),
      Size2 :: integer().
adler32_combine(Z, Adler1, Adler2, Len2) 
  when is_integer(Adler1), is_integer(Adler2), is_integer(Len2) ->
    call(Z, ?ADLER32_COMBINE, <<Adler1:32, Adler2:32, Len2:32>>);
adler32_combine(_Z, _Adler1, _Adler2, _Len2) ->
    erlang:error(badarg).

-spec getQSize(zstream()) -> non_neg_integer().
getQSize(Z) ->
    call(Z, ?GET_QSIZE, []).

%% compress/uncompress zlib with header
-spec compress(Data) -> Compressed when
      Data :: iodata(),
      Compressed :: binary().
compress(Data) ->
    Z = open(),
    Bs = try
	     deflateInit(Z, default),
	     B = deflate(Z, Data, finish),
	     deflateEnd(Z),
	     B
	 after
	     close(Z)
	 end,
    iolist_to_binary(Bs).

-spec uncompress(Data) -> Decompressed when
      Data  :: iodata(),
      Decompressed :: binary().
uncompress(Data) ->
    try iolist_size(Data) of
        Size ->
            if
                Size >= 8 ->
                    Z = open(),
		    Bs = try
			     inflateInit(Z),
			     B = inflate(Z, Data),
			     inflateEnd(Z),
			     B
			 after
			     close(Z)
			 end,
                    iolist_to_binary(Bs);
                true ->
                    erlang:error(data_error)
            end
    catch
        _:_ ->
            erlang:error(badarg)
    end.

%% unzip/zip zlib without header (zip members)
-spec zip(Data) -> Compressed when
      Data :: iodata(),
      Compressed :: binary().
zip(Data) ->
    Z = open(),
    Bs = try
	     deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
	     B = deflate(Z, Data, finish),
	     deflateEnd(Z),
	     B
	 after
	     close(Z)
	 end,
    iolist_to_binary(Bs).

-spec unzip(Data) -> Decompressed when
      Data :: iodata(),
      Decompressed :: binary().
unzip(Data) ->
    Z = open(),
    Bs = try
	     inflateInit(Z, -?MAX_WBITS),
	     B = inflate(Z, Data),
	     inflateEnd(Z),
	     B
	 after
	     close(Z)
	 end,
    iolist_to_binary(Bs).
    
-spec gzip(Data) -> Compressed when
      Data :: iodata(),
      Compressed :: binary().
gzip(Data) ->
    Z = open(),
    Bs = try
	     deflateInit(Z, default, deflated, 16+?MAX_WBITS, 8, default),
	     B = deflate(Z, Data, finish),
	     deflateEnd(Z),
	     B
	 after
	     close(Z)
	 end,
    iolist_to_binary(Bs).

-spec gunzip(Data) -> Decompressed when
      Data :: iodata(),
      Decompressed :: binary().
gunzip(Data) ->
    Z = open(),
    Bs = try
	     inflateInit(Z, 16+?MAX_WBITS),
	     B = inflate(Z, Data),
	     inflateEnd(Z),
	     B
	 after
	     close(Z)
	 end,
    iolist_to_binary(Bs).

-spec collect(zstream()) -> iolist().
collect(Z) -> 
    collect(Z, []).

-spec collect(zstream(), iolist()) -> iolist().
collect(Z, Acc) ->
    receive 
	{Z, {data, Bin}} ->
	    collect(Z, [Bin|Acc])
    after 0 ->
	    reverse(Acc)
    end.

-spec flush(zstream()) -> 'ok'.
flush(Z) ->
    receive
	{Z, {data,_}} ->
	    flush(Z)
    after 0 ->
	    ok
    end.
    
arg_flush(none)    -> ?Z_NO_FLUSH;
%% ?Z_PARTIAL_FLUSH is deprecated in zlib -- deliberately not included.
arg_flush(sync)    -> ?Z_SYNC_FLUSH;
arg_flush(full)    -> ?Z_FULL_FLUSH;
arg_flush(finish)  -> ?Z_FINISH;
arg_flush(_) -> erlang:error(badarg).

arg_level(none)             -> ?Z_NO_COMPRESSION;
arg_level(best_speed)       -> ?Z_BEST_SPEED;
arg_level(best_compression) -> ?Z_BEST_COMPRESSION;
arg_level(default)          -> ?Z_DEFAULT_COMPRESSION;
arg_level(Level) when is_integer(Level), Level >= 0, Level =< 9 -> Level;
arg_level(_) -> erlang:error(badarg).
     
arg_strategy(filtered) ->     ?Z_FILTERED;
arg_strategy(huffman_only) -> ?Z_HUFFMAN_ONLY;
arg_strategy(rle) -> ?Z_RLE;
arg_strategy(default) ->      ?Z_DEFAULT_STRATEGY;
arg_strategy(_) -> erlang:error(badarg).

arg_method(deflated) -> ?Z_DEFLATED;
arg_method(_) -> erlang:error(badarg).

-spec arg_bitsz(zwindowbits()) -> zwindowbits().
arg_bitsz(Bits) when is_integer(Bits) andalso
		     ((8 =< Bits andalso Bits < 48) orelse
		      (-15 =< Bits andalso Bits =< -8)) ->
    Bits;
arg_bitsz(_) -> erlang:error(badarg).

-spec arg_mem(zmemlevel()) -> zmemlevel().
arg_mem(Level) when is_integer(Level), 1 =< Level, Level =< 9 -> Level;
arg_mem(_) -> erlang:error(badarg).

call(Z, Cmd, Arg) ->
    try port_control(Z, Cmd, Arg) of
	[0|Res] -> list_to_atom(Res);
	[1|Res] ->
	    flush(Z),
	    erlang:error(list_to_atom(Res));
	[2,A,B,C,D] ->
	    (A bsl 24)+(B bsl 16)+(C bsl 8)+D;
	[3,A,B,C,D] ->
	    erlang:error({need_dictionary,(A bsl 24)+(B bsl 16)+(C bsl 8)+D});
	[4, _, _, _, _] ->
	    inflate_has_more
    catch 
	error:badarg -> %% Rethrow loses port_control from stacktrace.
	    erlang:error(badarg)
    end.

reverse(X) ->
    reverse(X, []).

reverse([H|T], Y) ->
    reverse(T, [H|Y]);
reverse([], X) -> 
    X.
