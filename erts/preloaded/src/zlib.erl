%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

-export([open/0,close/1,set_controlling_process/2,
         deflateInit/1,deflateInit/2,deflateInit/6,
         deflateSetDictionary/2,deflateReset/1,deflateParams/3,
         deflate/2,deflate/3,deflateEnd/1,
         inflateInit/1,inflateInit/2,inflateInit/3,
         inflateSetDictionary/2,inflateGetDictionary/1, inflateReset/1,
         inflate/2,inflate/3,inflateEnd/1,
         inflateChunk/2,inflateChunk/1,
         safeInflate/2,
         setBufSize/2,getBufSize/1,
         crc32/1,crc32/2,crc32/3,adler32/2,adler32/3,
         crc32_combine/4,adler32_combine/4,
         compress/1,uncompress/1,zip/1,unzip/1,
         gzip/1,gunzip/1]).

-export([on_load/0]).

%% These are soft-deprecated until OTP 21.
% -deprecated([inflateChunk/1, inflateChunk/2,
%              getBufSize/1, setBufSize/2,
%              crc32/1,crc32/2,crc32/3,adler32/2,adler32/3,
%              crc32_combine/4,adler32_combine/4]).

-export_type([zstream/0, zflush/0, zlevel/0, zwindowbits/0, zmemlevel/0,
              zstrategy/0]).

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

-define(MAX_WBITS, 15).

-define(DEFAULT_MEMLEVEL, 8).
-define(DEFAULT_WBITS, 15).

-define(EOS_BEHAVIOR_ERROR, 0).
-define(EOS_BEHAVIOR_RESET, 1).
-define(EOS_BEHAVIOR_CUT, 2).

%% Chunk sizes are hardcoded on account of them screwing with the
%% predictability of the system. zlib is incapable of trapping so we need to
%% ensure that it never operates on any significant amount of data.
-define(DEFLATE_IN_CHUNKSIZE, 8 bsl 10).
-define(DEFLATE_OUT_CHUNKSIZE, 8 bsl 10).
-define(INFLATE_IN_CHUNKSIZE, 8 bsl 10).
-define(INFLATE_OUT_CHUNKSIZE, 16 bsl 10).

%%------------------------------------------------------------------------

%% Public data types.
-type zstream() :: reference().
-type zflush() :: 'none' | 'sync' | 'full' | 'finish'.

-type zlevel() ::
    'none' | 'default' | 'best_compression' | 'best_speed' | 0..9.
-type zstrategy() :: 'default' | 'filtered' | 'huffman_only' | 'rle'.

-type zmemlevel() :: 1..9.
-type zwindowbits() :: -15..-8 | 8..47.

%% Private data types.

-type zmethod()     :: 'deflated'.

-record(zlib_opts, {
        stream :: zstream() | 'undefined',
        method :: function(),
        input_chunk_size :: pos_integer(),
        output_chunk_size :: pos_integer(),
        flush :: non_neg_integer()
    }).

%%------------------------------------------------------------------------

on_load() ->
    case erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok -> ok
    end.

-spec open() -> zstream().
open() ->
    open_nif().
open_nif() ->
    erlang:nif_error(undef).

-spec close(Z) -> 'ok' when
      Z :: zstream().
close(Z) ->
    close_nif(Z).
close_nif(_Z) ->
    erlang:nif_error(undef).

-spec set_controlling_process(Z, Pid) -> 'ok' when
      Z :: zstream(),
      Pid :: pid().
set_controlling_process(Z, Pid) ->
    set_controller_nif(Z, Pid).
set_controller_nif(_Z, _Pid) ->
    erlang:nif_error(undef).

-spec deflateInit(Z) -> 'ok' when
      Z :: zstream().
deflateInit(Z) ->
    deflateInit(Z, default).

-spec deflateInit(Z, Level) -> 'ok' when
      Z :: zstream(),
      Level :: zlevel().
deflateInit(Z, Level) ->
    deflateInit(Z, Level, deflated, ?DEFAULT_WBITS, ?DEFAULT_MEMLEVEL, default).

-spec deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) -> 'ok' when
      Z :: zstream(),
      Level :: zlevel(),
      Method :: zmethod(),
      WindowBits :: zwindowbits(),
      MemLevel :: zmemlevel(),
      Strategy :: zstrategy().
deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) ->
    deflateInit_nif(Z,
                    arg_level(Level),
                    arg_method(Method),
                    arg_bitsz(WindowBits),
                    arg_mem(MemLevel),
                    arg_strategy(Strategy)).
deflateInit_nif(_Z, _Level, _Method, _WindowBits, _MemLevel, _Strategy) ->
    erlang:nif_error(undef).

-spec deflateSetDictionary(Z, Dictionary) -> Adler32 when
      Z :: zstream(),
      Dictionary :: iodata(),
      Adler32 :: non_neg_integer().
deflateSetDictionary(Z, Dictionary) ->
    deflateSetDictionary_nif(Z, Dictionary).
deflateSetDictionary_nif(_Z, _Dictionary) ->
    erlang:nif_error(undef).

-spec deflateReset(Z) -> 'ok' when
      Z :: zstream().
deflateReset(Z) ->
    deflateReset_nif(Z).
deflateReset_nif(_Z) ->
    erlang:nif_error(undef).

-spec deflateParams(Z, Level, Strategy) -> ok when
      Z :: zstream(),
      Level :: zlevel(),
      Strategy :: zstrategy().
deflateParams(Z, Level0, Strategy0) ->
    Level = arg_level(Level0),
    Strategy = arg_strategy(Strategy0),
    Progress = deflate(Z, <<>>, sync),
    case deflateParams_nif(Z, Level, Strategy) of
        ok ->
            save_progress(Z, deflate, Progress),
            ok;
        Other ->
            Other
    end.
deflateParams_nif(_Z, _Level, _Strategy) ->
    erlang:nif_error(undef).

-spec deflate(Z, Data) -> Compressed when
      Z :: zstream(),
      Data :: iodata(),
      Compressed :: iolist().
deflate(Z, Data) ->
    deflate(Z, Data, none).

-spec deflate(Z, Data, Flush) -> Compressed when
      Z :: zstream(),
      Data :: iodata(),
      Flush :: zflush(),
      Compressed :: iolist().
deflate(Z, Data, Flush) ->
    Progress = restore_progress(Z, deflate),
    enqueue_input(Z, Data),
    append_iolist(Progress, dequeue_all_chunks(Z, deflate_opts(Flush))).

deflate_opts(Flush) ->
    #zlib_opts{
        method = fun deflate_nif/4,
        input_chunk_size = ?DEFLATE_IN_CHUNKSIZE,
        output_chunk_size = ?DEFLATE_OUT_CHUNKSIZE,
        flush = arg_flush(Flush)
    }.

deflate_nif(_Z, _InputChSize, _OutputChSize, _Flush) ->
    erlang:nif_error(undef).

-spec deflateEnd(Z) -> 'ok' when
      Z :: zstream().
deflateEnd(Z) ->
    deflateEnd_nif(Z).
deflateEnd_nif(_Z) ->
    erlang:nif_error(undef).

-spec inflateInit(Z) -> 'ok' when
      Z :: zstream().
inflateInit(Z) ->
    inflateInit(Z, ?DEFAULT_WBITS).

-spec inflateInit(Z, WindowBits) -> 'ok' when
      Z :: zstream(),
      WindowBits :: zwindowbits().
inflateInit(Z, WindowBits) ->
    inflateInit(Z, WindowBits, cut).

-spec inflateInit(Z, WindowBits, EoSBehavior) -> 'ok' when
      Z :: zstream(),
      WindowBits :: zwindowbits(),
      EoSBehavior :: error | reset | cut.
inflateInit(Z, WindowBits, EoSBehavior) ->
    inflateInit_nif(Z, arg_bitsz(WindowBits), arg_eos_behavior(EoSBehavior)).
inflateInit_nif(_Z, _WindowBits, _EoSBehavior) ->
    erlang:nif_error(undef).

-spec inflateSetDictionary(Z, Dictionary) -> 'ok' when
      Z :: zstream(),
      Dictionary :: iodata().
inflateSetDictionary(Z, Dictionary) ->
    inflateSetDictionary_nif(Z, Dictionary).
inflateSetDictionary_nif(_Z, _Dictionary) ->
    erlang:nif_error(undef).

-spec inflateGetDictionary(Z) -> Dictionary when
      Z :: zstream(),
      Dictionary :: binary().
inflateGetDictionary(Z) ->
    case inflateGetDictionary_nif(Z) of
        Dictionary when is_binary(Dictionary) ->
            Dictionary;
        not_supported ->
            erlang:error(enotsup)
    end.
inflateGetDictionary_nif(_Z) ->
    erlang:nif_error(undef).

-spec inflateReset(Z) -> 'ok' when
      Z :: zstream().
inflateReset(Z) ->
    inflateReset_nif(Z).
inflateReset_nif(_Z) ->
    erlang:nif_error(undef).

-spec inflate(Z, Data) -> Decompressed when
      Z :: zstream(),
      Data :: iodata(),
      Decompressed :: iolist().
inflate(Z, Data) ->
    inflate(Z, Data, []).

-spec inflate(Z, Data, Options) -> Decompressed when
      Z :: zstream(),
      Data :: iodata(),
      Options :: list({exception_on_need_dict, boolean()}),
      Decompressed :: iolist() |
                      {need_dictionary,
                       Adler32 :: non_neg_integer(),
                       Output :: iolist()}.
inflate(Z, Data, Options) ->
    enqueue_input(Z, Data),
    Result = dequeue_all_chunks(Z, inflate_opts()),
    case proplist_get_value(Options, exception_on_need_dict, true) of
        true -> exception_on_need_dict(Z, Result);
        false -> Result
    end.

inflate_nif(_Z, _InputChSize, _OutputChSize, _Flush) ->
    erlang:nif_error(undef).

inflate_opts() ->
    #zlib_opts{
        method = fun inflate_nif/4,
        input_chunk_size = ?INFLATE_IN_CHUNKSIZE,
        output_chunk_size = ?INFLATE_OUT_CHUNKSIZE,
        flush = arg_flush(none)
    }.

-spec inflateChunk(Z, Data) -> Decompressed | {more, Decompressed} when
      Z :: zstream(),
      Data :: iodata(),
      Decompressed :: iolist().
inflateChunk(Z, Data) ->
    enqueue_input(Z, Data),
    inflateChunk(Z).

-spec inflateChunk(Z) -> Decompressed | {more, Decompressed} when
      Z :: zstream(),
      Decompressed :: iolist().
inflateChunk(Z) ->
    Opts0 = inflate_opts(),
    Opts = Opts0#zlib_opts { output_chunk_size = getBufSize(Z) },

    Result0 = dequeue_next_chunk(Z, Opts),
    Result1 = exception_on_need_dict(Z, Result0),
    yield_inflateChunk(Z, Result1).

yield_inflateChunk(_Z, {continue, Output}) ->
    {more, lists:flatten(Output)};
yield_inflateChunk(_Z, {finished, Output}) ->
    lists:flatten(Output).

exception_on_need_dict(Z, {need_dictionary, Adler, Output}) ->
    Progress = restore_progress(Z, inflate),
    save_progress(Z, inflate, append_iolist(Progress, Output)),
    erlang:error({need_dictionary, Adler});
exception_on_need_dict(Z, {Mark, Output}) ->
    Progress = restore_progress(Z, inflate),
    {Mark, append_iolist(Progress, Output)};
exception_on_need_dict(Z, Output) when is_list(Output); is_binary(Output) ->
    Progress = restore_progress(Z, inflate),
    append_iolist(Progress, Output).

-spec safeInflate(Z, Data) -> Result when
      Z :: zstream(),
      Data :: iodata(),
      Result :: {continue, Output :: iolist()} |
                {finished, Output :: iolist()} |
                {need_dictionary,
                 Adler32 :: non_neg_integer(),
                 Output :: iolist()}.
safeInflate(Z, Data) ->
    enqueue_input(Z, Data),
    dequeue_next_chunk(Z, inflate_opts()).

-spec inflateEnd(Z) -> 'ok' when
      Z :: zstream().
inflateEnd(Z) ->
    inflateEnd_nif(Z).
inflateEnd_nif(_Z) ->
    erlang:nif_error(undef).

-spec setBufSize(Z, Size) -> 'ok' when
      Z :: zstream(),
      Size :: non_neg_integer().
setBufSize(Z, Size) when is_integer(Size), Size > 16, Size < (1 bsl 24) ->
    setBufSize_nif(Z, Size);
setBufSize(_Z, _Size) ->
    erlang:error(badarg).
setBufSize_nif(_Z, _Size) ->
    erlang:nif_error(undef).

-spec getBufSize(Z) -> non_neg_integer() when
      Z :: zstream().
getBufSize(Z) ->
    getBufSize_nif(Z).
getBufSize_nif(_Z) ->
    erlang:nif_error(undef).

-spec crc32(Z) -> CRC when
      Z :: zstream(),
      CRC :: non_neg_integer().
crc32(Z) ->
    crc32_nif(Z).
crc32_nif(_Z) ->
    erlang:nif_error(undef).

-spec crc32(Z, Data) -> CRC when
      Z :: zstream(),
      Data :: iodata(),
      CRC :: non_neg_integer().
crc32(Z, Data) when is_reference(Z) ->
    erlang:crc32(Data);
crc32(_Z, _Data) ->
    erlang:error(badarg).

-spec crc32(Z, PrevCRC, Data) -> CRC when
      Z :: zstream(),
      PrevCRC :: non_neg_integer(),
      Data :: iodata(),
      CRC :: non_neg_integer().
crc32(Z, CRC, Data) when is_reference(Z) ->
    erlang:crc32(CRC, Data);
crc32(_Z, _CRC, _Data) ->
    erlang:error(badarg).

-spec crc32_combine(Z, CRC1, CRC2, Size2) -> CRC when
      Z :: zstream(),
      CRC :: non_neg_integer(),
      CRC1 :: non_neg_integer(),
      CRC2 :: non_neg_integer(),
      Size2 :: non_neg_integer().
crc32_combine(Z, CRC1, CRC2, Size2) when is_reference(Z) ->
    erlang:crc32_combine(CRC1, CRC2, Size2);
crc32_combine(_Z, _CRC1, _CRC2, _Size2) ->
    erlang:error(badarg).

-spec adler32(Z, Data) -> CheckSum when
      Z :: zstream(),
      Data :: iodata(),
      CheckSum :: non_neg_integer().
adler32(Z, Data) when is_reference(Z) ->
    erlang:adler32(Data);
adler32(_Z, _Data) ->
    erlang:error(badarg).

-spec adler32(Z, PrevAdler, Data) -> CheckSum when
      Z :: zstream(),
      PrevAdler :: non_neg_integer(),
      Data :: iodata(),
      CheckSum :: non_neg_integer().
adler32(Z, Adler, Data) when is_reference(Z) ->
    erlang:adler32(Adler, Data);
adler32(_Z, _Adler, _Data) ->
    erlang:error(badarg).

-spec adler32_combine(Z, Adler1, Adler2, Size2) -> Adler when
      Z :: zstream(),
      Adler :: non_neg_integer(),
      Adler1 :: non_neg_integer(),
      Adler2 :: non_neg_integer(),
      Size2 :: non_neg_integer().
adler32_combine(Z, Adler1, Adler2, Size2) when is_reference(Z) ->
    erlang:adler32_combine(Adler1, Adler2, Size2);
adler32_combine(_Z, _Adler1, _Adler2, _Size2) ->
    erlang:error(badarg).

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
             inflateInit(Z, 16+?MAX_WBITS, reset),
             B = inflate(Z, Data),
             inflateEnd(Z),
             B
         after
             close(Z)
         end,
    iolist_to_binary(Bs).

-spec dequeue_all_chunks(Z, Opts) -> Result when
    Z :: zstream(),
    Opts :: #zlib_opts{},
    Result :: {need_dictionary, integer(), iolist()} |
              iolist().
dequeue_all_chunks(Z, Opts) ->
    dequeue_all_chunks_1(Z, Opts, []).
dequeue_all_chunks_1(Z, Opts, Output) ->
    case dequeue_next_chunk(Z, Opts) of
        {need_dictionary, _, _} = NeedDict ->
            NeedDict;
        {continue, Chunk} ->
            dequeue_all_chunks_1(Z, Opts, append_iolist(Output, Chunk));
        {finished, Chunk} ->
            append_iolist(Output, Chunk)
    end.

-spec dequeue_next_chunk(Z, Opts) -> Result when
    Z :: zstream(),
    Opts :: #zlib_opts{},
    Result :: {need_dictionary, integer(), iolist()} |
              {continue, iolist()} |
              {finished, iolist()}.
dequeue_next_chunk(Z, Opts) ->
    Method = Opts#zlib_opts.method,
    IChSz = Opts#zlib_opts.input_chunk_size,
    OChSz = Opts#zlib_opts.output_chunk_size,
    Flush = Opts#zlib_opts.flush,
    Method(Z, IChSz, OChSz, Flush).

-spec append_iolist(IO, D) -> iolist() when
    IO :: iodata(),
    D :: iodata().
append_iolist([], D) when is_list(D) -> D;
append_iolist([], D) -> [D];
append_iolist(IO, []) -> IO;
append_iolist(IO, [D]) -> [IO, D];
append_iolist(IO, D) -> [IO, D].

%% inflate/2 and friends are documented as throwing an error on Z_NEED_DICT
%% rather than simply returning something to that effect, and deflateParams/3
%% may flush behind the scenes. This requires us to stow away our current
%% progress in the handle and resume from that point on our next call.
%%
%% Generally speaking this is either a refc binary or nothing at all, so it's
%% pretty cheap.

-spec save_progress(Z, Kind, Output) -> ok when
      Z :: zstream(),
      Kind :: inflate | deflate,
      Output :: iolist().
save_progress(Z, Kind, Output) ->
    ok = setStash_nif(Z, {Kind, Output}).

-spec restore_progress(Z, Kind) -> iolist() when
    Z :: zstream(),
    Kind :: inflate | deflate.
restore_progress(Z, Kind) ->
    case getStash_nif(Z) of
        {ok, {Kind, Output}} ->
            ok = clearStash_nif(Z),
            Output;
        empty ->
            []
    end.

-spec clearStash_nif(Z) -> ok when
      Z :: zstream().
clearStash_nif(_Z) ->
    erlang:nif_error(undef).

-spec setStash_nif(Z, Term) -> ok when
      Z :: zstream(),
      Term :: term().
setStash_nif(_Z, _Term) ->
    erlang:nif_error(undef).

-spec getStash_nif(Z) -> {ok, term()} | empty when
      Z :: zstream().
getStash_nif(_Z) ->
    erlang:nif_error(undef).

%% The 'proplists' module isn't preloaded so we can't rely on its existence.
proplist_get_value([], _Name, DefVal) -> DefVal;
proplist_get_value([{Name, Value} | _Opts], Name, _DefVal) -> Value;
proplist_get_value([_Head | Opts], Name, DefVal) ->
    proplist_get_value(Opts, Name, DefVal).

arg_flush(none)   -> ?Z_NO_FLUSH;
%% ?Z_PARTIAL_FLUSH is deprecated in zlib -- deliberately not included.
arg_flush(sync)   -> ?Z_SYNC_FLUSH;
arg_flush(full)   -> ?Z_FULL_FLUSH;
arg_flush(finish) -> ?Z_FINISH;
arg_flush(_) -> erlang:error(bad_flush_mode).

arg_level(none)             -> ?Z_NO_COMPRESSION;
arg_level(best_speed)       -> ?Z_BEST_SPEED;
arg_level(best_compression) -> ?Z_BEST_COMPRESSION;
arg_level(default)          -> ?Z_DEFAULT_COMPRESSION;
arg_level(Level) when is_integer(Level), Level >= 0, Level =< 9 -> Level;
arg_level(_) -> erlang:error(bad_compression_level).

arg_strategy(filtered) ->     ?Z_FILTERED;
arg_strategy(huffman_only) -> ?Z_HUFFMAN_ONLY;
arg_strategy(rle) -> ?Z_RLE;
arg_strategy(default) ->      ?Z_DEFAULT_STRATEGY;
arg_strategy(_) -> erlang:error(bad_compression_strategy).

arg_method(deflated) -> ?Z_DEFLATED;
arg_method(_) -> erlang:error(bad_compression_method).

arg_eos_behavior(error) -> ?EOS_BEHAVIOR_ERROR;
arg_eos_behavior(reset) -> ?EOS_BEHAVIOR_RESET;
arg_eos_behavior(cut) -> ?EOS_BEHAVIOR_CUT;
arg_eos_behavior(_) -> erlang:error(bad_eos_behavior).

-spec arg_bitsz(zwindowbits()) -> zwindowbits().
arg_bitsz(Bits) when is_integer(Bits) andalso
                     ((8 =< Bits andalso Bits < 48) orelse
                      (-15 =< Bits andalso Bits =< -8)) ->
    Bits;
arg_bitsz(_) -> erlang:error(bad_windowbits).

-spec arg_mem(zmemlevel()) -> zmemlevel().
arg_mem(Level) when is_integer(Level), 1 =< Level, Level =< 9 -> Level;
arg_mem(_) -> erlang:error(bad_memlevel).

-spec enqueue_input(Z, IOData) -> ok when
      Z :: zstream(),
      IOData :: iodata().
enqueue_input(Z, IOData) ->
    enqueue_input_1(Z, erlang:iolist_to_iovec(IOData)).

enqueue_input_1(_Z, []) ->
    ok;
enqueue_input_1(Z, IOVec) ->
    case enqueue_nif(Z, IOVec) of
        {continue, Remainder} -> enqueue_input_1(Z, Remainder);
        ok -> ok
    end.

enqueue_nif(_Z, _IOVec) ->
    erlang:nif_error(undef).
