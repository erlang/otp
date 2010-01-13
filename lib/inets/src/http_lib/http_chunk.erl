%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% Description: Implements chunked transfer encoding see RFC2616 section
%% 3.6.1
-module(http_chunk).

-include("http_internal.hrl").

%% API
-export([decode/3, decode/4, encode/1, encode_last/0, handle_headers/2]).
%% Callback API - used for example if the chunkedbody is received a
%% little at a time on a socket. 
-export([decode_size/1, ignore_extensions/1, decode_data/1, decode_trailer/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
%%-------------------------------------------------------------------------
%% decode(ChunkedBody, MaxBodySize, MaxHeaderSize, <Stream>) -> 
%%       {ok, {Headers, Body}} | {Module, Function, Args}
%%
%%      Headers = ["Header:Value"]
%%      ChunkedBody = binary()
%%      MaxBodySize = integer()
%%      MaxHeaderSize = integer()
%%      Stream = {Code, Request} - if Request#request.stream =/= none
%%      and Code == 200 the side effect of sending each decode chunk to the
%%      client/file before the whole body is received will take place.
%%
%% Note: decode/4 should only be used from httpc_handler module.
%% Otherwhise use the side effect free decode/3.
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
    decode(ChunkedBody, MaxBodySize, MaxHeaderSize, false).

decode(ChunkedBody, MaxBodySize, MaxHeaderSize, Stream) ->
     %% Note decode_size will call decode_data.
    decode_size([ChunkedBody, <<>>, [], 
		 {MaxBodySize, <<>>, 0, MaxHeaderSize, Stream}]).

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

encode(Chunk) when is_list(Chunk)->
    HEXSize = http_util:integer_to_hexlist(erlang:iolist_size(Chunk)),
    [HEXSize,  ?CR, ?LF, Chunk, ?CR, ?LF].

encode_last() ->
    <<$0, ?CR, ?LF, ?CR, ?LF >>.

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
decode_size([Bin, Rest, HexList, Info]) ->
    decode_size(<<Rest/binary, Bin/binary>>, HexList, Info).

ignore_extensions([Bin, Rest, NextFunction]) ->
    ignore_extensions(<<Rest/binary, Bin/binary>>, NextFunction).

decode_data([Bin, ChunkSize, TotalChunk, Info]) ->
    decode_data(ChunkSize, <<TotalChunk/binary, Bin/binary>>, Info).

decode_trailer([Bin, Rest, Header, Headers, MaxHeaderSize, Body, 
		BodyLength]) ->
    decode_trailer(<<Rest/binary, Bin/binary>>, 
		   Header, Headers, MaxHeaderSize, Body, BodyLength).

%%%========================================================================
%%% Internal functions
%%%========================================================================
decode_size(<<>>, HexList, Info) ->
    {?MODULE, decode_size, [<<>>, HexList, Info]};
decode_size(Data = <<?CR, ?LF, ChunkRest/binary>>, HexList, 
	    {MaxBodySize, Body, 
	     AccLength,
	     MaxHeaderSize, Stream}) ->
    ChunkSize =  http_util:hexlist_to_integer(lists:reverse(HexList)),
     case ChunkSize of
	0 -> % Last chunk, there was no data
	    ignore_extensions(Data, {?MODULE, decode_trailer, 
				      [<<>>, [],[], MaxHeaderSize,
				       Body,
				       integer_to_list(AccLength)]});  
	_ ->
	    %% Note decode_data may call decode_size again if there
	    %% is more than one chunk, hence here is where the last parameter
	    %% to this function comes in.
	    decode_data(ChunkSize, ChunkRest, {MaxBodySize, Body, 
					       ChunkSize + AccLength , 
					       MaxHeaderSize, Stream})
    end;
decode_size(<<";", Rest/binary>>, HexList, Info) ->
    %% Note ignore_extensions will call decode_size/1 again when
    %% it ignored all extensions.
    ignore_extensions(Rest, {?MODULE, decode_size, [<<>>, HexList, Info]});
decode_size(<<?CR>> = Data, HexList, Info) ->
      {?MODULE, decode_size, [Data, HexList, Info]};
decode_size(<<Octet, Rest/binary>>, HexList, Info) ->
    decode_size(Rest, [Octet | HexList], Info).

%% "All applications MUST ignore chunk-extension extensions they
%% do not understand.", see RFC 2616 Section 3.6.1 We don't
%% understand any extension...
ignore_extensions(<<>>, NextFunction) ->
    {?MODULE, ignore_extensions, [<<>>, NextFunction]};
ignore_extensions(Data = <<?CR, ?LF, _ChunkRest/binary>>, 
		  {Module, Function, Args}) ->
    Module:Function([Data | Args]);
ignore_extensions(<<?CR>> = Data, NextFunction) ->
    {?MODULE, ignore_extensions, [Data, NextFunction]};
ignore_extensions(<<_Octet, Rest/binary>>, NextFunction) ->
    ignore_extensions(Rest, NextFunction).

decode_data(ChunkSize, TotalChunk,
	    Info = {MaxBodySize, BodySoFar, AccLength, MaxHeaderSize, Stream}) 
  when ChunkSize =< size(TotalChunk) ->
    case TotalChunk of
	%% Last chunk
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ";">> ->
	    %% Note ignore_extensions will call decode_trailer/1
	    %% once it ignored all extensions.
	    {NewBody, _} = 
		stream(<<BodySoFar/binary, Data/binary>>, Stream),
	    {?MODULE, ignore_extensions, 
	     [<<>>, 
	      {?MODULE, decode_trailer, [<<>>, [],[], MaxHeaderSize,
					 NewBody,
					 integer_to_list(AccLength)]}]};
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ";", Rest/binary>> ->
	    %% Note ignore_extensions will call decode_trailer/1
	    %% once it ignored all extensions.
	    {NewBody, _} = stream(<<BodySoFar/binary, Data/binary>>, Stream),
	    ignore_extensions(Rest, {?MODULE, decode_trailer, 
				     [<<>>, [],[], MaxHeaderSize,
				      NewBody,
				      integer_to_list(AccLength)]});
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ?CR, ?LF>> ->
	    {NewBody, _} = stream(<<BodySoFar/binary, Data/binary>>, Stream),
	    {?MODULE, decode_trailer, [<<?CR, ?LF>>, [],[], MaxHeaderSize,
				       NewBody,
				       integer_to_list(AccLength)]};
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", ?CR, ?LF, Rest/binary>> ->
	    {NewBody,_}= stream(<<BodySoFar/binary, Data/binary>>, Stream),
	    decode_trailer(<<?CR, ?LF, Rest/binary>>, [],[], MaxHeaderSize,
			   NewBody,
			   integer_to_list(AccLength));
	%% There are more chunks, so here we go agin...
	<<Data:ChunkSize/binary, ?CR, ?LF>> ->
	    {NewBody, NewStream} = 
		stream(<<BodySoFar/binary, Data/binary>>, Stream),
	    {?MODULE, decode_size, [<<>>, [], {MaxBodySize, NewBody, AccLength, MaxHeaderSize, NewStream}]};
	<<Data:ChunkSize/binary, ?CR, ?LF, Rest/binary>> 
	when (AccLength < MaxBodySize) or (MaxBodySize == nolimit)  ->
	    {NewBody, NewStream} = 
		stream(<<BodySoFar/binary, Data/binary>>, Stream),
	    decode_size(Rest, [], 
			{MaxBodySize, NewBody,
			 AccLength, MaxHeaderSize, NewStream});
	<<_:ChunkSize/binary, ?CR, ?LF, _/binary>> ->
	    throw({error, body_too_big});
	_ ->
	    {?MODULE, decode_data, [ChunkSize, TotalChunk, Info]}
    end;	
decode_data(ChunkSize, TotalChunk, Info) ->
    {?MODULE, decode_data, [ChunkSize, TotalChunk, Info]}.

decode_trailer(<<>>, Header, Headers, MaxHeaderSize, Body, BodyLength) ->
    {?MODULE, decode_trailer, [<<>>, Header, Headers, MaxHeaderSize, Body, 
			       BodyLength]};

%% Note: If Bin is not empty it is part of a pipelined request/response. 
decode_trailer(<<?CR,?LF,?CR,?LF, Bin/binary>>, [], [], _, Body, BodyLength) ->
    {ok, {["content-length:" ++ BodyLength], <<Body/binary, Bin/binary>>}};
decode_trailer(<<?CR,?LF,?CR,?LF, Bin/binary>>, 
	       Header, Headers, MaxHeaderSize, Body, BodyLength) ->
    NewHeaders = case Header of
		     [] ->
			 Headers;
		     _ ->
			 [lists:reverse(Header) | Headers]
		 end,
    Length =  length(NewHeaders), 
    case Length > MaxHeaderSize of
	true ->
	    throw({error, {header_too_long, MaxHeaderSize, 
			   MaxHeaderSize-Length}});
	false ->
	    {ok, {["content-length:" ++ BodyLength | NewHeaders], 
		  <<Body/binary, Bin/binary>>}}
    end;
decode_trailer(<<?CR,?LF,?CR>> = Data, Header, Headers, MaxHeaderSize, 
	       Body, BodyLength) ->
    {?MODULE, decode_trailer, [Data, Header, Headers, MaxHeaderSize, Body, 
			       BodyLength]};
decode_trailer(<<?CR,?LF>> = Data, Header, Headers, MaxHeaderSize, 
	       Body, BodyLength) ->
    {?MODULE, decode_trailer, [Data, Header, Headers, MaxHeaderSize, Body, 
			       BodyLength]};
decode_trailer(<<?CR>> = Data, Header, Headers, MaxHeaderSize, 
	       Body, BodyLength) ->
    {?MODULE, decode_trailer, [Data, Header, Headers, MaxHeaderSize, Body, 
			       BodyLength]};
decode_trailer(<<?CR, ?LF, Rest/binary>>, Header, Headers, 
	       MaxHeaderSize, Body, BodyLength) ->
    decode_trailer(Rest, [], [lists:reverse(Header) | Headers], 
		   MaxHeaderSize, Body, BodyLength);

decode_trailer(<<Octet, Rest/binary>>, Header, Headers, MaxHeaderSize, Body,
	       BodyLength) ->
    decode_trailer(Rest, [Octet | Header], Headers, MaxHeaderSize, 
		   Body, BodyLength).

stream(BodyPart, false) ->
    {BodyPart, false};
stream(BodyPart, {Code, Request}) ->
    {NewBody, NewRequest} = httpc_handler:stream(BodyPart, Request, Code),
    {NewBody, {Code, NewRequest}}.
