%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
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
-moduledoc """
zlib compression interface.

This module provides an API for the zlib library
([www.zlib.net](http://www.zlib.net)). It is used to compress and decompress
data. The data format is described by
[RFC 1950](https://www.ietf.org/rfc/rfc1950.txt),
[RFC 1951](https://www.ietf.org/rfc/rfc1951.txt), and
[RFC 1952](https://www.ietf.org/rfc/rfc1952.txt).

A typical (compress) usage is as follows:

```erlang
Z = zlib:open(),
ok = zlib:deflateInit(Z,default),

Compress = fun F(end_of_data) ->
                 zlib:deflate(Z, [], finish);
               F(Data) ->
                 [zlib:deflate(Z, Data)|F(Read())]
           end,
Compressed = Compress(Read()),
ok = zlib:deflateEnd(Z),
zlib:close(Z),
list_to_binary(Compressed)
```

In all functions errors, `{'EXIT',{Reason,Backtrace}}`, can be thrown, where
`Reason` describes the error.

Typical `Reason`s:

- **`badarg`** - Bad argument.

- **`not_initialized`** - The stream hasn't been initialized, eg. if
  `inflateInit/1` wasn't called prior to a call to `inflate/2`.

- **`not_on_controlling_process`** - The stream was used by a process that
  doesn't control it. Use `set_controlling_process/2` if you need to transfer a
  stream to a different process.

- **`data_error`** - The data contains errors.

- **`stream_error`** - Inconsistent stream state.

- **`{need_dictionary,Adler32}`** - See `inflate/2`.
""".

-export([open/0,close/1,set_controlling_process/2,
         deflateInit/1,deflateInit/2,deflateInit/6,
         deflateSetDictionary/2,deflateReset/1,deflateParams/3,
         deflate/2,deflate/3,deflateEnd/1,
         inflateInit/1,inflateInit/2,inflateInit/3,
         inflateSetDictionary/2,inflateGetDictionary/1, inflateReset/1,
         inflate/2,inflate/3,inflateEnd/1,
         safeInflate/2,
         compress/1,uncompress/1,zip/1,unzip/1,
         gzip/1,gunzip/1]).

-export([on_load/0]).

-removed([{inflateChunk, 1, "use zlib:safeInflate/2 instead"},
          {inflateChunk, 2, "use zlib:safeInflate/2 instead"},
          {getBufSize, 1, "this function has been removed"},
          {setBufSize, 2, "this function has been removed"},
          {crc32, 1, "use erlang:crc32/1 on the uncompressed data instead"},
          {crc32, 2, "use erlang:crc32/1 instead"},
          {crc32, 3, "use erlang:crc32/2 instead"},
          {adler32, 2, "use erlang:adler32/1 instead"},
          {adler32, 3, "use erlang:adler32/2 instead"},
          {crc32_combine, 4, "use erlang:crc32_combine/3 instead"},
          {adler32_combine, 4, "use erlang:adler_combine/3 instead"}]).

-export_type([zstream/0, zflush/0, zlevel/0, zwindowbits/0, zmemlevel/0,
              zstrategy/0]).

-nifs([close_nif/1, open_nif/0, set_controller_nif/2, deflateInit_nif/6,
       deflateSetDictionary_nif/2, deflateReset_nif/1, deflateEnd_nif/1,
       deflateParams_nif/3, deflate_nif/4, inflateInit_nif/3,
       inflateSetDictionary_nif/2, inflateGetDictionary_nif/1,
       inflateReset_nif/1, inflateEnd_nif/1, inflate_nif/4,
       getStash_nif/1, clearStash_nif/1, setStash_nif/2,
       enqueue_nif/2]).

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

%% compression strategy
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
-doc "A zlib stream, see `open/0`.".
-type zstream() :: reference().
-type zflush() :: 'none' | 'sync' | 'full' | 'finish'.

-type zlevel() ::
    'none' | 'default' | 'best_compression' | 'best_speed' | 0..9.
-type zstrategy() :: 'default' | 'filtered' | 'huffman_only' | 'rle'.

-type zmemlevel() :: 1..9.
-doc "Normally in the range `-15..-8 | 8..15`.".
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

-doc false.
on_load() ->
    case erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok -> ok
    end.

-doc "Opens a zlib stream.".
-spec open() -> zstream().
open() ->
    open_nif().
open_nif() ->
    erlang:nif_error(undef).

-doc "Closes the stream referenced by `Z`.".
-spec close(Z) -> 'ok' when
      Z :: zstream().
close(Z) ->
    close_nif(Z).
close_nif(_Z) ->
    erlang:nif_error(undef).

-doc "Changes the controlling process of `Z` to `Pid`, which must be a local process.".
-doc(#{since => <<"OTP 20.1.3">>}).
-spec set_controlling_process(Z, Pid) -> 'ok' when
      Z :: zstream(),
      Pid :: pid().
set_controlling_process(Z, Pid) ->
    set_controller_nif(Z, Pid).
set_controller_nif(_Z, _Pid) ->
    erlang:nif_error(undef).

-doc "Same as `zlib:deflateInit(Z, default)`.".
-spec deflateInit(Z) -> 'ok' when
      Z :: zstream().
deflateInit(Z) ->
    deflateInit(Z, default).

-doc """
Initializes a zlib stream for compression.

`Level` decides the compression level to be used:

- `default` gives default compromise between speed and compression
- `none` (0) gives no compression
- `best_speed` (1) gives best speed
- `best_compression` (9) gives best compression
""".
-spec deflateInit(Z, Level) -> 'ok' when
      Z :: zstream(),
      Level :: zlevel().
deflateInit(Z, Level) ->
    deflateInit(Z, Level, deflated, ?DEFAULT_WBITS, ?DEFAULT_MEMLEVEL, default).

-doc """
Initiates a zlib stream for compression.

- **`Level`** - Compression level to use:

  - `default` gives default compromise between speed and compression
  - `none` (0) gives no compression
  - `best_speed` (1) gives best speed
  - `best_compression` (9) gives best compression

- **`Method`** - Compression method to use, currently the only supported method
  is `deflated`.

- **`WindowBits`** - The base two logarithm of the window size (the size of the
  history buffer). It is to be in the range 8 through 15. Larger values result
  in better compression at the expense of memory usage. Defaults to 15 if
  `deflateInit/2` is used. A negative `WindowBits` value suppresses the zlib
  header (and checksum) from the stream. Notice that the zlib source mentions
  this only as a undocumented feature.

  > #### Warning {: .warning }
  >
  > Due to a known bug in the underlying zlib library, `WindowBits` values 8 and
  > -8 do not work as expected. In zlib versions before 1.2.9 values 8 and -8
  > are automatically changed to 9 and -9. _From zlib version 1.2.9 value -8 is
  > rejected_ causing `zlib:deflateInit/6` to fail (8 is still changed to 9). It
  > also seem possible that future versions of zlib may fix this bug and start
  > accepting 8 and -8 as is.
  >
  > Conclusion: Avoid values 8 and -8 unless you know your zlib version supports
  > them.

- **`MemLevel`** - Specifies how much memory is to be allocated for the internal
  compression state: `MemLevel`=1 uses minimum memory but is slow and reduces
  compression ratio; `MemLevel`=9 uses maximum memory for optimal speed.
  Defaults to 8.

- **`Strategy`** - Tunes the compression algorithm. Use the following values:

  - `default` for normal data
  - `filtered` for data produced by a filter (or predictor)
  - `huffman_only` to force Huffman encoding only (no string match)
  - `rle` to limit match distances to one (run-length encoding)

  Filtered data consists mostly of small values with a somewhat random
  distribution. In this case, the compression algorithm is tuned to compress
  them better. The effect of `filtered` is to force more Huffman coding and less
  string matching; it is somewhat intermediate between `default` and
  `huffman_only`. `rle` is designed to be almost as fast as `huffman_only`, but
  gives better compression for PNG image data.

  `Strategy` affects only the compression ratio, but not the correctness of the
  compressed output even if it is not set appropriately.
""".
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

-doc """
Initializes the compression dictionary from the specified byte sequence without
producing any compressed output.

This function must be called immediately after
[`deflateInit/1,2,6`](`deflateInit/1`) or `deflateReset/1`, before any call of
`deflate/3`.

The compressor and decompressor must use the same dictionary (see
`inflateSetDictionary/2`).

The Adler checksum of the dictionary is returned.
""".
-spec deflateSetDictionary(Z, Dictionary) -> Adler32 when
      Z :: zstream(),
      Dictionary :: iodata(),
      Adler32 :: non_neg_integer().
deflateSetDictionary(Z, Dictionary) ->
    deflateSetDictionary_nif(Z, Dictionary).
deflateSetDictionary_nif(_Z, _Dictionary) ->
    erlang:nif_error(undef).

-doc """
Equivalent to `deflateEnd/1` followed by [`deflateInit/1,2,6`](`deflateInit/1`),
but does not free and reallocate all the internal compression state.

The stream keeps the same compression level and any other attributes.
""".
-spec deflateReset(Z) -> 'ok' when
      Z :: zstream().
deflateReset(Z) ->
    deflateReset_nif(Z).
deflateReset_nif(_Z) ->
    erlang:nif_error(undef).

-doc """
Dynamically updates the compression level and compression strategy.

The interpretation of `Level` and `Strategy` is as in `deflateInit/6`. This can be
used to switch between compression and straight copy of the input data, or to
switch to a different kind of input data requiring a different strategy. If the
compression level is changed, the input available so far is compressed with the
old level (and can be flushed); the new level takes effect only at the next call
of `deflate/3`.

Before the call of `deflateParams`, the stream state must be set as for a call
of [`deflate/3`](`deflate/3`), as the currently available input may have to be
compressed and flushed.
""".
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

-doc "Same as [`deflate(Z, Data, none)`](`deflate/3`).".
-spec deflate(Z, Data) -> Compressed when
      Z :: zstream(),
      Data :: iodata(),
      Compressed :: iolist().
deflate(Z, Data) ->
    deflate(Z, Data, none).

-doc """
Compresses as much data as possible, and stops when the input buffer becomes
empty.

It can introduce some output latency (reading input without producing any
output) except when forced to flush.

If `Flush` is set to `sync`, all pending output is flushed to the output buffer
and the output is aligned on a byte boundary, so that the decompressor can get
all input data available so far. Flushing can degrade compression for some
compression algorithms; thus, use it only when necessary.

If `Flush` is set to `full`, all output is flushed as with `sync`, and the
compression state is reset so that decompression can restart from this point if
previous compressed data has been damaged or if random access is desired. Using
`full` too often can seriously degrade the compression.

If `Flush` is set to `finish`, pending input is processed, pending output is
flushed, and [`deflate/3`](`deflate/3`) returns. Afterwards the only possible
operations on the stream are `deflateReset/1` or `deflateEnd/1`.

`Flush` can be set to `finish` immediately after
[`deflateInit`](`deflateInit/1`) if all compression is to be done in one step.

Example:

```erlang
zlib:deflateInit(Z),
B1 = zlib:deflate(Z,Data),
B2 = zlib:deflate(Z,<< >>,finish),
zlib:deflateEnd(Z),
list_to_binary([B1,B2])
```
""".
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

-doc """
Ends the deflate session and cleans all data used.

Notice that this function throws a `data_error` exception if the last call to
`deflate/3` was not called with `Flush` set to `finish`.
""".
-spec deflateEnd(Z) -> 'ok' when
      Z :: zstream().
deflateEnd(Z) ->
    deflateEnd_nif(Z).
deflateEnd_nif(_Z) ->
    erlang:nif_error(undef).

-doc "Initializes a zlib stream for decompression.".
-spec inflateInit(Z) -> 'ok' when
      Z :: zstream().
inflateInit(Z) ->
    inflateInit(Z, ?DEFAULT_WBITS).

-doc """
Initializes a decompression session on zlib stream.

`WindowBits` is the base two logarithm of the maximum window size (the size of
the history buffer). It is to be in the range 8 through 15. Default to 15 if
`inflateInit/1` is used.

If a compressed stream with a larger window size is specified as input,
`inflate/2` throws the `data_error` exception.

A negative `WindowBits` value makes zlib ignore the zlib header (and checksum)
from the stream. Notice that the zlib source mentions this only as a
undocumented feature.
""".
-spec inflateInit(Z, WindowBits) -> 'ok' when
      Z :: zstream(),
      WindowBits :: zwindowbits().
inflateInit(Z, WindowBits) ->
    inflateInit(Z, WindowBits, cut).

-doc false.
-spec inflateInit(Z, WindowBits, EoSBehavior) -> 'ok' when
      Z :: zstream(),
      WindowBits :: zwindowbits(),
      EoSBehavior :: error | reset | cut.
inflateInit(Z, WindowBits, EoSBehavior) ->
    inflateInit_nif(Z, arg_bitsz(WindowBits), arg_eos_behavior(EoSBehavior)).
inflateInit_nif(_Z, _WindowBits, _EoSBehavior) ->
    erlang:nif_error(undef).

-doc """
Initializes the decompression dictionary from the specified uncompressed byte
sequence.

This function must be called as a response to an inflate operation
(eg. `safeInflate/2`) returning `{need_dictionary,Adler,Output}` or in the case
of deprecated functions, throwing an
`{'EXIT',{{need_dictionary,Adler},_StackTrace}}` exception.

The dictionary chosen by the compressor can be determined from the Adler value
returned or thrown by the call to the inflate function. The compressor and
decompressor must use the same dictionary (See `deflateSetDictionary/2`).

After setting the dictionary the inflate operation should be retried without new
input.

Example:

```erlang
deprecated_unpack(Z, Compressed, Dict) ->
     case catch zlib:inflate(Z, Compressed) of
          {'EXIT',{{need_dictionary,_DictID},_}} ->
                 ok = zlib:inflateSetDictionary(Z, Dict),
                 Uncompressed = zlib:inflate(Z, []);
          Uncompressed ->
                 Uncompressed
     end.

new_unpack(Z, Compressed, Dict) ->
    case zlib:inflate(Z, Compressed, [{exception_on_need_dict, false}]) of
        {need_dictionary, _DictId, Output} ->
            ok = zlib:inflateSetDictionary(Z, Dict),
            [Output | zlib:inflate(Z, [])];
        Uncompressed ->
            Uncompressed
    end.
```
""".
-spec inflateSetDictionary(Z, Dictionary) -> 'ok' when
      Z :: zstream(),
      Dictionary :: iodata().
inflateSetDictionary(Z, Dictionary) ->
    inflateSetDictionary_nif(Z, Dictionary).
inflateSetDictionary_nif(_Z, _Dictionary) ->
    erlang:nif_error(undef).

-doc """
Returns the decompression dictionary currently in use by the stream.

This function must be called between [`inflateInit/1,2`](`inflateInit/1`) and
[`inflateEnd`](`inflateEnd/1`).

Only supported if ERTS was compiled with zlib >= 1.2.8.
""".
-doc(#{since => <<"OTP 20.0">>}).
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

-doc """
Equivalent to `inflateEnd/1` followed by `inflateInit/1`, but does not free and
reallocate all the internal decompression state. The stream will keep attributes
that could have been set by `inflateInit/1,2`.
""".
-spec inflateReset(Z) -> 'ok' when
      Z :: zstream().
inflateReset(Z) ->
    inflateReset_nif(Z).
inflateReset_nif(_Z) ->
    erlang:nif_error(undef).

-doc "Equivalent to [`inflate(Z, Data, [])`](`inflate/3`)".
-spec inflate(Z, Data) -> Decompressed when
      Z :: zstream(),
      Data :: iodata(),
      Decompressed :: iolist().
inflate(Z, Data) ->
    inflate(Z, Data, []).

-doc """
Decompresses as much data as possible. It can introduce some output latency
(reading input without producing any output).

Currently the only available option is `{exception_on_need_dict,boolean()}`
which controls whether the function should throw an exception when a preset
dictionary is required for decompression. When set to false, a `need_dictionary`
tuple will be returned instead. See `inflateSetDictionary/2` for details.

> #### Warning {: .warning }
>
> This option defaults to `true` for backwards compatibility but we intend to
> remove the exception behavior in a future release. New code that needs to
> handle dictionaries manually should always specify
> `{exception_on_need_dict,false}`.
""".
-doc(#{since => <<"OTP 20.1">>}).
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

exception_on_need_dict(Z, {need_dictionary, Adler, Output}) ->
    Progress = restore_progress(Z, inflate),
    save_progress(Z, inflate, append_iolist(Progress, Output)),
    erlang:error({need_dictionary, Adler});
exception_on_need_dict(Z, Output) when is_list(Output); is_binary(Output) ->
    Progress = restore_progress(Z, inflate),
    append_iolist(Progress, Output).

-doc """
Like `inflate/2`, but returns once it has expanded beyond a small
implementation-defined threshold. It's useful when decompressing untrusted input
which could have been maliciously crafted to expand until the system runs out of
memory.

This function returns `{continue | finished, Output}`, where Output is the data
that was decompressed in this call. New input can be queued up on each call if
desired, and the function will return `{finished, Output}` once all queued data
has been decompressed.

This function can introduce some output latency (reading input without producing
any output).

If a preset dictionary is required for further decompression, this function
returns a `need_dictionary` tuple. See `inflateSetDictionary/2`) for details.

Example:

```erlang
walk(Compressed, Handler) ->
    Z = zlib:open(),
    zlib:inflateInit(Z),
    loop(Z, Handler, zlib:safeInflate(Z, Compressed)),
    zlib:inflateEnd(Z),
    zlib:close(Z).

loop(Z, Handler, {continue, Output}) ->
    Handler(Output),
    loop(Z, Handler, zlib:safeInflate(Z, []));
loop(Z, Handler, {finished, Output}) ->
    Handler(Output).
```
""".
-doc(#{since => <<"OTP 20.1">>}).
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

-doc """
Ends the inflate session and cleans all data used.

Notice that this function throws a `data_error` exception if no end of stream
was found (meaning that not all data has been uncompressed).
""".
-spec inflateEnd(Z) -> 'ok' when
      Z :: zstream().
inflateEnd(Z) ->
    inflateEnd_nif(Z).
inflateEnd_nif(_Z) ->
    erlang:nif_error(undef).

%% compress/uncompress zlib with header
-doc "Compresses data with zlib headers and checksum.".
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

-doc "Uncompresses data with zlib headers and checksum.".
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
-doc "Compresses data without zlib headers and checksum.".
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

-doc "Uncompresses data without zlib headers and checksum.".
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

-doc "Compresses data with gz headers and checksum.".
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

-doc "Uncompresses data with gz headers and checksum.".
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
