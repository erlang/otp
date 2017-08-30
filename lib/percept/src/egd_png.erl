%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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


%% This code was originally written by Dan Gudmundsson for png-handling in
%% wings3d (e3d__png).
%%
%% @doc egd 
%%

-module(egd_png).

-export([binary/3]).

-include("egd.hrl").

-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(GREYSCALE,   0).
-define(TRUECOLOUR,  2).
-define(INDEXED,     3).
-define(GREYSCALE_A, 4).
-define(TRUECOLOUR_A,6).

-define(MAX_WBITS,15).

-define(CHUNK, 240).

-define(get4p1(Idx),((Idx)  bsr 4)).
-define(get4p2(Idx),((Idx)  band 16#0F)).
-define(get2p1(Idx),((Idx)  bsr 6)).
-define(get2p2(Idx),(((Idx) bsr 4) band 3)).
-define(get2p3(Idx),(((Idx) bsr 2) band 3)).
-define(get2p4(Idx),((Idx)  band 3)).
-define(get1p1(Idx),((Idx)  bsr 7)).
-define(get1p2(Idx),(((Idx) bsr 6) band 1)).
-define(get1p3(Idx),(((Idx) bsr 5) band 1)).
-define(get1p4(Idx),(((Idx) bsr 4) band 1)).
-define(get1p5(Idx),(((Idx) bsr 3) band 1)).
-define(get1p6(Idx),(((Idx) bsr 2) band 1)).
-define(get1p7(Idx),(((Idx) bsr 1) band 1)).
-define(get1p8(Idx),((Idx)  band 1)).

binary(W, H, Bitmap) when is_binary(Bitmap) ->
    Z = zlib:open(),
    Binary = bitmap2png(W, H, Bitmap, Z),
    zlib:close(Z),
    Binary.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Begin Tainted

bitmap2png(W, H, Bitmap,Z) ->
    HDR = create_chunk(<<"IHDR",W:32,H:32,8:8,(png_type(r8g8b8)):8,0:8,0:8,0:8>>,Z),
    DATA = create_chunk(["IDAT",compress_image(0,3*W,Bitmap,[])],Z),
    END  = create_chunk(<<"IEND">>,Z),
    list_to_binary([?MAGIC,HDR,DATA,END]).

compress_image(I,RowLen, Bin, Acc) ->
    Pos = I*RowLen,
    case Bin of
	<<_:Pos/binary,Row:RowLen/binary,_/binary>> ->
	    Filtered = filter_row(Row,RowLen),
	    compress_image(I+1,RowLen,Bin,[Filtered|Acc]);
	_ when Pos == size(Bin) ->
	    Filtered = list_to_binary(lists:reverse(Acc)),
	    Compressed = zlib:compress(Filtered),
	    Compressed
    end.

filter_row(Row,_RowLen) ->
    [0,Row].

% dialyzer warnings
%png_type(g8) -> ?GREYSCALE;
%png_type(a8) -> ?GREYSCALE;
%png_type(r8g8b8a8) -> ?TRUECOLOUR_A;
png_type(r8g8b8) -> ?TRUECOLOUR.

create_chunk(Bin,Z) when is_list(Bin) ->
    create_chunk(list_to_binary(Bin),Z);
create_chunk(Bin,Z) when is_binary(Bin) ->
    Sz = size(Bin)-4,
    Crc = zlib:crc32(Z,Bin),
    <<Sz:32,Bin/binary,Crc:32>>.

% End tainted
