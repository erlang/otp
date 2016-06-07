%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%% Description: Implements chunked transfer encoding see RFC2616 section
%%              3.6.1

-module(http_chunk).

-include("http_internal.hrl").

%% API
-export([decode/3, encode/1, encode_last/0, encode_last/1, handle_headers/2]).
%% Callback API - used for example if the chunkedbody is received a
%% little at a time on a socket. 
-export([decode_size/1, ignore_extensions/1, decode_data/1, decode_trailer/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
%%-------------------------------------------------------------------------
%% decode(ChunkedBody, MaxBodySize, MaxHeaderSize) ->
%%       {ok, {Headers, Body}} | {Module, Function, Args}
%%
%%      Headers = ["Header:Value"]
%%      ChunkedBody = binary()
%%      MaxBodySize = integer()
%%      MaxHeaderSize = integer()
%%
%% Description: Decodes a body encoded by the chunked transfer
%% encoding. If the ChunkedBody is not compleate it returns {Module,
%% Function, Args} so that decoding can be continued when more of the
%% data has been received by calling Module:Function([NewData | Args]).
%%
%% Note: In the case of pipelining a call to decode might contain data
%% that belongs to the next request/response and will be returned as
%% part of the body, hence functions calling http_chunk:decode must
%% look at the returned content-length header to make sure that they
%% split the actual body and data that possible should be passed along to 
%% the next pass in the loop. 
%%-------------------------------------------------------------------------
decode(ChunkedBody, MaxBodySize, MaxHeaderSize) ->
     %% Note decode_size will call decode_data.
    decode_size([ChunkedBody, <<>>, [], 0,
                 {MaxBodySize, <<>>, 0, MaxHeaderSize}]).

%%-------------------------------------------------------------------------
%% encode(Chunk) -> EncodedChunk
%%     
%%      Chunked = binary()
%%      EncodedChunk = binary()
%%                                    
%% Description: Encodes a body part with the chunked transfer encoding. 
%%              Chunks are returned as lists or binaries depending on the
%%              input format. When sending the data on the both formats 
%%              are accepted.
%%-------------------------------------------------------------------------
encode(Chunk) when is_binary(Chunk)->
    HEXSize = list_to_binary(http_util:integer_to_hexlist(size(Chunk))),
    <<HEXSize/binary, ?CR, ?LF, Chunk/binary, ?CR, ?LF>>;

encode([<<>>]) ->
    [];

encode(Chunk) when is_list(Chunk)->
    HEXSize = http_util:integer_to_hexlist(erlang:iolist_size(Chunk)),
    [HEXSize,  ?CR, ?LF, Chunk, ?CR, ?LF].

encode_last() ->
    <<$0, ?CR, ?LF, ?CR, ?LF >>.

encode_last([]) ->
    encode_last();
encode_last(Trailers0) ->
     Trailers = list_to_binary(encode_trailers(Trailers0)),
    <<$0, ?CR, ?LF, Trailers/binary>>.

%%-------------------------------------------------------------------------
%% handle_headers(HeaderRecord, ChunkedHeaders) -> NewHeaderRecord
%%
%%	HeaderRecord = NewHeaderRecord = #http_request_h{} | #http_response_h{}
%%      ChunkedHeaders = ["Header:Value"] as returnde by http_chunk:decode/3
%%                                    
%% Description: Removes chunked from the header as we now have decode
%% the body and adds a content-length header and any other headers
%% found in the chunked trail.
%%-------------------------------------------------------------------------
handle_headers(RequestHeaderRecord = #http_request_h{}, ChunkedHeaders) ->
    NewHeaders = http_request:headers(ChunkedHeaders, RequestHeaderRecord),
    TransferEncoding = 
	case NewHeaders#http_request_h.'transfer-encoding' -- "chunked" of
	    ""  ->
		undefined;
	    Other ->
		Other
	end,
    NewHeaders#http_request_h{'transfer-encoding' = TransferEncoding};

handle_headers(ResponseHeaderRecord = #http_response_h{},  ChunkedHeaders) ->
    NewHeaders = http_response:headers(ChunkedHeaders, ResponseHeaderRecord),
    TransferEncoding = 
	case NewHeaders#http_response_h.'transfer-encoding' -- "chunked" of
	    ""  ->
		undefined;
	    Other ->
		Other
	end,
    NewHeaders#http_response_h{'transfer-encoding' = TransferEncoding}.

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
decode_size([Bin, Rest, HexList, AccSize, Info]) ->
    decode_size(<<Rest/binary, Bin/binary>>, HexList, AccSize, Info).

ignore_extensions([Bin, Rest, RemainingSize, TotalMaxHeaderSize, NextFunction]) ->
    ignore_extensions(<<Rest/binary, Bin/binary>>, RemainingSize, TotalMaxHeaderSize, NextFunction).

decode_data([Bin, ChunkSize, TotalChunk, Info]) ->
    decode_data(ChunkSize, <<TotalChunk/binary, Bin/binary>>, Info).

decode_trailer([Bin, Rest, Header, Headers, Body, 
		BodyLength, RemainingSize, TotalMaxHeaderSize]) ->
    decode_trailer(<<Rest/binary, Bin/binary>>, 
		   Header, Headers, Body, BodyLength, RemainingSize, TotalMaxHeaderSize).

%%%========================================================================
%%% Internal functions
%%%========================================================================
decode_size(_, _, AccHeaderSize, {_,_,_, MaxHeaderSize}) when
      AccHeaderSize > MaxHeaderSize ->
    throw({error, {header_too_long, {max, MaxHeaderSize}}});

decode_size(<<>>, HexList, AccHeaderSize, Info) ->
    {?MODULE, decode_size, [<<>>, HexList, AccHeaderSize, Info]};
decode_size(Data = <<?CR, ?LF, ChunkRest/binary>>, HexList, AccHeaderSize, 
	    {MaxBodySize, Body, 
	     AccLength,
	     MaxHeaderSize}) ->
    try http_util:hexlist_to_integer(lists:reverse(string:strip(HexList, left))) of
	0 -> % Last chunk, there was no data
	    ignore_extensions(Data, remaing_size(MaxHeaderSize, AccHeaderSize), MaxHeaderSize,
			      {?MODULE, decode_trailer, 
			       [<<>>, [],[],
				Body,
				integer_to_list(AccLength)]});  
	ChunkSize ->
	    %% Note decode_data may call decode_size again if there
	    %% is more than one chunk, hence here is where the last parameter
	    %% to this function comes in.
	    decode_data(ChunkSize, ChunkRest, {MaxBodySize, Body, 
					       ChunkSize + AccLength, 
					       MaxHeaderSize})
    catch
	_:_ ->
	    throw({error, {chunk_size, lists:reverse(HexList)}})
    end;
decode_size(<<";", Rest/binary>>, HexList, AccHeaderSize, {_,_,_, MaxHeaderSize} = Info) ->
    %% Note ignore_extensions will call decode_size/1 again when
    %% it ignored all extensions.
    ignore_extensions(Rest, remaing_size(MaxHeaderSize, AccHeaderSize), MaxHeaderSize,
		      {?MODULE, decode_size, [<<>>, HexList, AccHeaderSize, Info]});
decode_size(<<?CR>> = Data, HexList, AccHeaderSize, Info) ->
      {?MODULE, decode_size, [Data, HexList, AccHeaderSize, Info]};
decode_size(<<Octet, Rest/binary>>, HexList, AccHeaderSize, Info) ->
    decode_size(Rest, [Octet | HexList], AccHeaderSize + 1, Info).

%% "All applications MUST ignore chunk-extension extensions they
%% do not understand.", see RFC 2616 Section 3.6.1 We don't
%% understand any extension...
ignore_extensions(_, 0, TotalMaxHeaderSize, _) ->
    throw({error, {header_too_long, {max, TotalMaxHeaderSize}}});
ignore_extensions(<<>>, RemainingSize, TotalMaxHeaderSize, NextFunction) ->
    {?MODULE, ignore_extensions, [<<>>, RemainingSize, TotalMaxHeaderSize, NextFunction]};
ignore_extensions(Data = <<?CR, ?LF, _ChunkRest/binary>>, RemainingSize, TotalMaxHeaderSize,
		  {Module, Function, Args}) ->
    case Function of
	decode_trailer ->
	    Module:Function([Data | Args ++ [RemainingSize, TotalMaxHeaderSize]]);
	 _ ->
	    Module:Function([Data | Args])
    end;
ignore_extensions(<<?CR>> = Data, RemainingSize, TotalMaxHeaderSize, NextFunction) ->
    {?MODULE, ignore_extensions, [Data, RemainingSize, TotalMaxHeaderSize, NextFunction]};
ignore_extensions(<<_Octet, Rest/binary>>, RemainingSize, TotalMaxHeaderSize, NextFunction) ->
    ignore_extensions(Rest, remaing_size(RemainingSize, 1), TotalMaxHeaderSize, NextFunction).

decode_data(ChunkSize, TotalChunk,
	    Info = {MaxBodySize, BodySoFar, AccLength, MaxHeaderSize}) 
  when ChunkSize =< size(TotalChunk) ->
    case TotalChunk of
	%% Last chunk
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ";">> ->
	    %% Note ignore_extensions will call decode_trailer/1
	    %% once it ignored all extensions.
	    {?MODULE, ignore_extensions, 
	     [<<>>, 
	      {?MODULE, decode_trailer, [<<>>, [],[],
					 <<BodySoFar/binary, Data/binary>>,
					 integer_to_list(AccLength)]}]};
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ";", Rest/binary>> ->
	    %% Note ignore_extensions will call decode_trailer/1
	    %% once it ignored all extensions.
	    ignore_extensions(Rest, MaxHeaderSize, MaxHeaderSize,
			      {?MODULE, decode_trailer, 
				     [<<>>, [],[],
				      <<BodySoFar/binary, Data/binary>>,
				      integer_to_list(AccLength)]});
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ?CR, ?LF>> ->
	    {?MODULE, decode_trailer, [<<?CR, ?LF>>, [],[],
				       <<BodySoFar/binary, Data/binary>>,
				       integer_to_list(AccLength), MaxHeaderSize, MaxHeaderSize]};
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ?CR, ?LF, Rest/binary>> ->
	    decode_trailer(<<?CR, ?LF, Rest/binary>>, [],[],
			   <<BodySoFar/binary, Data/binary>>,
			   integer_to_list(AccLength), MaxHeaderSize, MaxHeaderSize);
	%% There are more chunks, so here we go again...
	<<Data:ChunkSize/binary, ?CR, ?LF>> ->
	    NewBody = <<BodySoFar/binary, Data/binary>>,
	    {?MODULE, decode_size, [<<>>, [], 0, {MaxBodySize, NewBody, AccLength, MaxHeaderSize}]};
	<<Data:ChunkSize/binary, ?CR, ?LF, Rest/binary>> 
	when (AccLength < MaxBodySize) or (MaxBodySize == nolimit)  ->
	    decode_size(Rest, [], 0,
			{MaxBodySize, <<BodySoFar/binary, Data/binary>>,
			 AccLength, MaxHeaderSize});
	<<_:ChunkSize/binary, ?CR, ?LF, _/binary>> ->
	    throw({error, {body_too_big, {max, MaxBodySize}}});
	_ ->
	    {?MODULE, decode_data, [ChunkSize, TotalChunk, Info]}
    end;	
decode_data(ChunkSize, TotalChunk, Info) ->
    {?MODULE, decode_data, [ChunkSize, TotalChunk, Info]}.

decode_trailer(_,_,_,_,_, 0, TotalMaxHeaderSize) ->
    throw({error, {header_too_long, {max, TotalMaxHeaderSize}}});
decode_trailer(<<>>, Header, Headers, Body, BodyLength, RemainingSize, TotalMaxHeaderSize) ->
    {?MODULE, decode_trailer, [<<>>, Header, Headers, Body, 
			       BodyLength, RemainingSize, TotalMaxHeaderSize]};
%% Note: If Bin is not empty it is part of a pipelined request/response. 
decode_trailer(<<?CR,?LF,?CR,?LF, Bin/binary>>, [], [], Body, BodyLength, _, _) ->
    {ok, {["content-length:" ++ BodyLength], <<Body/binary, Bin/binary>>}};
decode_trailer(<<?CR,?LF,?CR,?LF, Bin/binary>>, 
	       Header, Headers, Body, BodyLength, _, _) ->
    NewHeaders = case Header of
		     [] ->
			 Headers;
		     _ ->
			 [lists:reverse(Header) | Headers]
		 end,
    {ok, {["content-length:" ++ BodyLength | NewHeaders], 
	  <<Body/binary, Bin/binary>>}};
decode_trailer(<<?CR,?LF,?CR>> = Data, Header, Headers, 
	       Body, BodyLength, RemainingSize, TotalMaxHeaderSize) ->
    {?MODULE, decode_trailer, [Data, Header, Headers, Body, 
			       BodyLength, RemainingSize, TotalMaxHeaderSize]};
decode_trailer(<<?CR,?LF>> = Data, Header, Headers, 
	       Body, BodyLength, RemainingSize, TotalMaxHeaderSize) ->
    {?MODULE, decode_trailer, [Data, Header, Headers, Body, 
			       BodyLength, RemainingSize, TotalMaxHeaderSize]};
decode_trailer(<<?CR>> = Data, Header, Headers, 
	       Body, BodyLength, RemainingSize, TotalMaxHeaderSize) ->
    {?MODULE, decode_trailer, [Data, Header, Headers, Body, 
			       BodyLength, RemainingSize, TotalMaxHeaderSize]};
decode_trailer(<<?CR, ?LF, Rest/binary>>, Header, Headers, Body, BodyLength, RemainingSize, TotalMaxHeaderSize) ->
    decode_trailer(Rest, [], [lists:reverse(Header) | Headers], 
		   Body, BodyLength, RemainingSize, TotalMaxHeaderSize);
decode_trailer(<<Octet, Rest/binary>>, Header, Headers, Body,
	       BodyLength, RemainingSize, TotalMaxHeaderSize) ->
    decode_trailer(Rest, [Octet | Header], Headers, 		   
		   Body, BodyLength, remaing_size(RemainingSize, 1), TotalMaxHeaderSize).

remaing_size(nolimit, _) ->
    nolimit;
remaing_size(Total, Consumed) ->
    Total - Consumed.

encode_trailers(Trailers) ->
    encode_trailers(Trailers, "").
    
encode_trailers([], Acc) ->
    Acc ++ ?CRLF ++ ?CRLF;
encode_trailers([{Header, Value} | Rest], Acc) ->
    encode_trailers(Rest, Header ++ ":" ++ Value ++ ?CRLF ++ Acc).
