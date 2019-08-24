%%%=======================================================================
%%% File        : wsp_pdu.erl
%%% Author      : Tony Rogvall <tony@bit.hemma.se>
%%% Description : WSP PDU
%%% Created     : 18 Aug 2003 by <tony@bit.hemma.se>
%%%=======================================================================
%%%
%%% There are a couple of bugs in this file. Some are detected by
%%% Dialyzer v1.1 starting both from byte code and from source, some
%%% other ones are detected only starting from sourse, while some
%%% others go unnoticed (these are identified by "BUG" below). It is
%%% expected that at least some of them are detected when the new type
%%% analysis is integrated into Dialyzer. Some other ones, like the
%%% one with the unused _Acc argument are harder to detect and might
%%% require different techniques.
%%%
%%%=======================================================================

-module(wsp_pdu).
-export([encode/1, encode/2, decode/1, decode/2]).

%% The following is just to suppress unused function warnings
-export([decode_address/1, decode_header/2,
	 decode_headers/1, decode_mms_version/1, decode_multipart/1,
	 encode_headers/1, encode_mms_version/1, encode_multipart/1,
	 encode_language/1, encode_short_integer/1,
	 fmt_current_date/0,
	 format_header/1, format_headers/1,
	 parse_header/1, format/1]).

-include("wsp.hrl").
-include("wdp.hrl").

-ifdef(debug).
-define(dbg(Fmt,Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmt,Args), ok).
-endif.

-define(WARN(Cond, Message),
	if (Cond) ->
		io:format("Warning: ~s\n", [(Message)]);
	   true ->
		ok
	end).


format(Pdu) ->
    if record(Pdu, wsp_connect) ->
	    fmt(Pdu, record_info(fields, wsp_connect));
       record(Pdu, wsp_connect_reply) ->
	    fmt(Pdu, record_info(fields, wsp_connect_reply));
       record(Pdu, wsp_redirect) ->
	    fmt(Pdu, record_info(fields, wsp_redirect));
       record(Pdu, wsp_disconnect) ->
	    fmt(Pdu, record_info(fields, wsp_disconnect));
       record(Pdu, wsp_get) ->
	    fmt(Pdu, record_info(fields, wsp_get));
       record(Pdu, wsp_post) ->
	    fmt(Pdu, record_info(fields, wsp_post));
       record(Pdu,wsp_reply) ->
	    fmt(Pdu, record_info(fields, wsp_reply));
       record(Pdu,wsp_data_fragment_pdu) ->
	    fmt(Pdu, record_info(fields, wsp_data_fragment_pdu));
       record(Pdu,wsp_push) ->
	    fmt(Pdu, record_info(fields, wsp_push));
       record(Pdu, wsp_suspend) ->
	    fmt(Pdu, record_info(fields, wsp_suspend));
       record(Pdu, wsp_resume) ->
	    fmt(Pdu, record_info(fields, wsp_resume));
       record(Pdu, wsp_unknown_pdu) ->
	    fmt(Pdu, record_info(fields, wsp_unknown_pdu))
    end.

fmt(Pdu, Fs) ->
    [Name | Vs] = tuple_to_list(Pdu),
    lists:flatten(["\n",atom_to_list(Name)," {\n" , fmt1(Fs, Vs), "\n}"]).

fmt1([F|Fs],[V|Vs]) ->
    [io_lib:format("  ~s: ~s;\n", [F,fmt_value(V)]) | fmt1(Fs, Vs)];
fmt1([], []) ->
    "".

fmt_value(V) when binary(V) -> "#Bin";
fmt_value(V) -> lists:flatten(io_lib:format("~p",[V])).


%%
%% Wsp pdu encoder
%%
encode(Pdu) ->
    encode(Pdu, ?WSP_DEFAULT_VERSION).

encode(Pdu, Version) ->
    ?dbg("encode pdu using encoding version ~p\n", [Version]),
    Enc = encode1(Pdu, Version),
    ?dbg("pdu: ~p\nreversed pdu: ~p\n",
	 [Pdu, decode(Enc, Version)]),
    Enc.


encode1(Pdu, Version) ->
    case Pdu of
	#wsp_connect_reply {server_session_id=ServerSessionId,
			    capabilities=Capabilities,
			    headers=Headers} ->
	    EncServerSessionId = e_uintvar(ServerSessionId),
	    EncCapabilities = encode_capabilities(Capabilities),
	    EncCapabilitiesLength = e_uintvar(size(EncCapabilities)),
	    EncHeaders = encode_headers(Headers,Version),
	    EncHeadersLength = e_uintvar(size(EncHeaders)),
	    <<?WSP_ConnectReply,
	     EncServerSessionId/binary,
	     EncCapabilitiesLength/binary, EncHeadersLength/binary,
	     EncCapabilities/binary, EncHeaders/binary>>;

	#wsp_reply{ status=Status,
		    content_type=ContentType,
		    headers=Headers,
		    data=Data} ->
	    EncStatus = encode_status_code(Status),
	    EncContentType = encode_content_type(ContentType,Version),
	    EncHeaders = encode_headers(Headers,Version),
	    EncHeadersLength = e_uintvar(size(EncContentType)+
					 size(EncHeaders)),
	    <<?WSP_Reply,
	     EncStatus:8,
	     EncHeadersLength/binary,
	     EncContentType/binary,
	     EncHeaders/binary,
	     Data/binary>>;

	#wsp_post{type=Type, uri=URI, content_type=ContentType,
		  headers=Headers, data=Data} ->
	    %% WSP_Post, WSP_Put
	    PDUType = encode_pdu_type(Type),
	    UriLength = e_uintvar(length(URI)),
	    EncContentType = encode_content_type(ContentType,Version),
	    EncHeaders = encode_headers(Headers,Version),
	    EncHeadersLength = e_uintvar(size(EncContentType)+
					 size(EncHeaders)),
	    %% FIXME
	    <<PDUType:8,
	     UriLength/binary,
	     EncHeadersLength/binary,
	     (list_to_binary(URI))/binary,
	     EncContentType/binary,
	     EncHeaders/binary,
	     Data/binary>>;

	#wsp_push{type=Type, content_type=ContentType,
		  headers=Headers, data=Data} ->
	    %% WSP_Push, WSP_ConfirmedPush
	    PDUType = encode_pdu_type(Type),
	    EncContentType = encode_content_type(ContentType,Version),
	    EncHeaders = encode_headers(Headers,Version),
	    ?dbg("Version ~p Headers ~p", [Version, Headers]),
	    ?dbg("EncHeaders ~p", [EncHeaders]),
	    EncHeadersLength = e_uintvar(size(EncContentType)+
					 size(EncHeaders)),
	    ?dbg("EncCT = ~w ~w", [ContentType, EncContentType]),
	    ?dbg("EncHL = ~w", [EncHeadersLength]),
	    <<PDUType:8,
	     EncHeadersLength/binary,
	     EncContentType/binary,
	     EncHeaders/binary,
	     Data/binary>>;

	#wsp_get{type=Type, uri=URI, headers=Headers} ->
	    %% WSP_Get, WSP_Options, WSP_Head, WSP_Delete, WSP_Trace
	    PDUType = encode_pdu_type(Type),
	    UriLength = length(URI),
	    EncHeaders = encode_headers(Headers,Version),
	    <<PDUType:8,
	     (e_uintvar(UriLength))/binary,
	     (list_to_binary(URI))/binary,
	     EncHeaders/binary>>;

	#wsp_redirect { flags = Flags, addresses = Addrs } ->
	    Flg = lists:foldl(fun(permanent,F) ->
				       ?WSP_PERMANENT_REDIRECT bor F;
			          (resue, F) ->
				       ?WSP_REUSE_SECURITY bor F
			       end, 0, Flags),
	    EncAddr = encode_addresses(Addrs),
	    <<?WSP_Redirect, Flg:8, EncAddr/binary >>;


	#wsp_data_fragment_pdu { headers=Headers, data=Data } ->
	    EncHeaders = encode_headers(Headers,Version),
	    << ?WSP_DataFragmentPDU, EncHeaders/binary, Data/binary >>
    end.

decode(Data) ->
    decode(Data, ?WSP_COMPLIENT_VERSION).

decode(Data0, Version) ->
    case Data0 of
	<<?WSP_Connect:8,PduVersion:8,D0/binary>> ->
	    %% 8.2.2.1
	    {CapabilitiesLen,D1} = d_uintvar(D0),
	    {HeadersLen,D2} = d_uintvar(D1),
	    {Capabilities,D3} = split_binary(D2, CapabilitiesLen),
	    Caps = decode_capabilities(Capabilities,#wsp_capabilities{}),
	    {Headers,D4} = split_binary(D3, HeadersLen),
	    DecHeaders = decode_headers(Headers, Version),
	    ?WARN(D4 =/= <<>>, "Connect pdu contains trailing data"),
	    %% FIXME: warn when D4 is not <<>>
	    #wsp_connect{ version = PduVersion,
			  capabilities=Caps,
			  headers = DecHeaders };

	<<?WSP_ConnectReply:8,D0/binary>> ->
	    %% 8.2.2.2
	    {ServerSessionId,D1} = d_uintvar(D0),
	    {CapabilitiesLen,D2} = d_uintvar(D1),
	    {HeadersLen,D3} = d_uintvar(D2),
	    {Capabilities,D4} = split_binary(D3, CapabilitiesLen),
	    Caps = decode_capabilities(Capabilities,#wsp_capabilities{}),
	    {Headers,D5} = split_binary(D4, HeadersLen),
	    DecHeaders = decode_headers(Headers, Version),
	    ?WARN(D5 =/= <<>>, "ConnectReply pdu contains trailing data"),
	    #wsp_connect_reply{server_session_id=ServerSessionId,
			       capabilities=Caps,
			       headers=DecHeaders};

	<<?WSP_Redirect:8,Flg:8,D0/binary>> ->
	    Flags =
		if Flg band ?WSP_PERMANENT_REDIRECT =/= 0 -> [permanent];
		   true -> []
		end ++
		if Flg band ?WSP_REUSE_SECURITY =/= 0 -> [security];
		   true -> []
		end,
	    Addrs = decode_addresses(D0),
	    %% 8.2.2.3 Redirect
	    #wsp_redirect{flags=Flags,addresses=Addrs};


	<<?WSP_Disconnect:8,D0/binary>> ->
	    %% 8.2.2.4 Disconnect
	    {ServerSessionId,_D1} = d_uintvar(D0),
	    #wsp_disconnect{server_session_id=ServerSessionId};

	<<?WSP_Get:8,D0/binary>> ->
	    {URILength, D1} = d_uintvar(D0),
	    <<UriData:URILength/binary,D2/binary>> = D1,
	    Hs = decode_headers(D2, Version),
	    #wsp_get{type='GET',uri=binary_to_list(UriData),headers=Hs };

	<<?WSP_Options:8,D0/binary>> ->
	    {URILength, D1} = d_uintvar(D0),
	    <<UriData:URILength/binary,D2/binary>> = D1,
	    Hs = decode_headers(D2, Version),
	    #wsp_get{type='OPTIONS',uri=binary_to_list(UriData),headers=Hs };

	<<?WSP_Head:8,D0/binary>> ->
	    {URILength, D1} = d_uintvar(D0),
	    <<UriData:URILength/binary,D2/binary>> = D1,
	    Hs = decode_headers(D2, Version),
	    #wsp_get{type='HEAD',uri=binary_to_list(UriData),headers=Hs };

	<<?WSP_Delete:8,D0/binary>> ->
	    {URILength, D1} = d_uintvar(D0),
	    <<UriData:URILength/binary,D2/binary>> = D1,
	    Hs = decode_headers(D2, Version),
	    #wsp_get{type='DELETE',uri=binary_to_list(UriData),headers=Hs };

	<<?WSP_Trace:8,D0/binary>> ->
	    {URILength, D1} = d_uintvar(D0),
	    <<UriData:URILength/binary,D2/binary>> = D1,
	    Hs = decode_headers(D2, Version),
	    #wsp_get{type='TRACE',uri=binary_to_list(UriData),headers=Hs };

	%% 8.2.3.2 Post
	<<?WSP_Post:8,D0/binary>> ->
	    {URILen, D1} = d_uintvar(D0),
	    {HL0, D2} = d_uintvar(D1),
	    <<UriData:URILen/binary,D3/binary>> = D2,
	    {FieldData,D4} = scan_header_data(D3),
	    HL1 = (HL0-(size(D3)-size(D4))),
	    <<D5:HL1/binary,Data/binary>> = D4,
	    ContentType = decode_content_type(FieldData, Version),
	    Headers = decode_headers(D5, Version),
	    #wsp_post{ type='POST', uri=binary_to_list(UriData),
		       content_type=ContentType, headers=Headers, data=Data};

	<<?WSP_Put:8,D0/binary>> ->
	    {URILen, D1} = d_uintvar(D0),
	    {HL0, D2} = d_uintvar(D1),
	    <<UriData:URILen/binary,D3/binary>> = D2,
	    {FieldData,D4} = scan_header_data(D3),
	    HL1 = (HL0-(size(D3)-size(D4))),
	    <<D5:HL1/binary,Data/binary>> = D4,
	    ContentType = decode_content_type(FieldData, Version),
	    Headers = decode_headers(D5, Version),
	    #wsp_post{ type='PUT', uri=binary_to_list(UriData),
		       content_type=ContentType, headers=Headers, data=Data};

	<<?WSP_Reply:8,StatusCode:8,D0/binary>> ->
	    %% 8.2.3.3 Reply
	    Status = decode_status_code(StatusCode),
	    {HL0, D1} = d_uintvar(D0),
	    {FieldData, D2} = scan_header_data(D1),
	    ContentType = decode_content_type(FieldData, Version),
	    %% Headers are headersLength - binary size of content type
	    HL1 = (HL0-(size(D1)-size(D2))),
	    <<D3:HL1/binary,Data/binary>> = D2,
	    Hs = decode_headers(D3, Version),
	    #wsp_reply{status=Status, content_type=ContentType,
		       headers=Hs, data=Data};

	<<?WSP_DataFragmentPDU:8,D0/binary>> ->
	    %% 8.2.3.4 Data Fragment PDU
	    {HL0, D1} = d_uintvar(D0),
	    <<D2:HL0/binary,Data/binary>> = D1,
	    Hs = decode_headers(D2, Version),
	    #wsp_data_fragment_pdu{headers=Hs, data=Data};

	%% 8.2.4.1 Push or ConfirmedPush
	<<?WSP_Push:8,D0/binary>> ->
	    {HeadersLength, T200} = d_uintvar(D0),
	    {FieldData, T300} = scan_header_data(T200),
	    ContentType = decode_content_type(FieldData, Version),
	    RealHeadersLength = (HeadersLength-(size(T200)-size(T300))),
	    <<T400:RealHeadersLength/binary,Data/binary>> = T300,
	    Headers = decode_headers(T400, Version),
	    #wsp_push{type=push,content_type=ContentType,
		      headers=Headers,data=Data};

	<<?WSP_ConfirmedPush:8,D0/binary>> ->
	    {HeadersLength, T200} = d_uintvar(D0),
	    {FieldData, T300} = scan_header_data(T200),
	    ContentType = decode_content_type(FieldData, Version),
	    RealHeadersLength = (HeadersLength-(size(T200)-size(T300))),
	    <<T400:RealHeadersLength/binary,Data/binary>> = T300,
	    Headers = decode_headers(T400, Version),
	    #wsp_push{type=confirmed_push,
		      content_type=ContentType,
		      headers=Headers,data=Data};

	<<PDUType:8,T100/binary>> ->
	    #wsp_unknown_pdu { type = PDUType, data = T100 }
    end.


encode_pdu_type(connect) -> ?WSP_Connect;
encode_pdu_type(connect_reply) -> ?WSP_ConnectReply;
encode_pdu_type(redirect) -> ?WSP_Redirect;
encode_pdu_type(reply) -> ?WSP_Reply;
encode_pdu_type(disconnect) -> ?WSP_Disconnect;
encode_pdu_type(push) -> ?WSP_Push;
encode_pdu_type(confirmed_push) -> ?WSP_ConfirmedPush;
encode_pdu_type(suspend) -> ?WSP_Suspend;
encode_pdu_type(resume) -> ?WSP_Resume;
encode_pdu_type(data_fragment_pdu) -> ?WSP_DataFragmentPDU;
encode_pdu_type('GET') -> ?WSP_Get;
encode_pdu_type('OPTIONS') -> ?WSP_Options;
encode_pdu_type('HEAD') -> ?WSP_Head;
encode_pdu_type('DELETE') -> ?WSP_Delete;
encode_pdu_type('TRACE') -> ?WSP_Trace;
encode_pdu_type('POST') -> ?WSP_Post;
encode_pdu_type('PUT') -> ?WSP_Put;
encode_pdu_type(Type) when integer(Type) -> Type.


decode_pdu_type(?WSP_Connect) -> connect;
decode_pdu_type(?WSP_ConnectReply) -> connect_reply;
decode_pdu_type(?WSP_Redirect) -> redirect;
decode_pdu_type(?WSP_Reply) -> reply;
decode_pdu_type(?WSP_Disconnect) -> disconnect;
decode_pdu_type(?WSP_Push) -> push;
decode_pdu_type(?WSP_ConfirmedPush) -> confirmed_push;
decode_pdu_type(?WSP_Suspend) -> suspend;
decode_pdu_type(?WSP_Resume) -> resume;
decode_pdu_type(?WSP_DataFragmentPDU) -> data_fragment_pdu;
decode_pdu_type(?WSP_Get) -> 'GET';
decode_pdu_type(?WSP_Options) -> 'OPTIONS';
decode_pdu_type(?WSP_Head) -> 'HEAD';
decode_pdu_type(?WSP_Delete) -> 'DELETE';
decode_pdu_type(?WSP_Trace) -> 'TRACE';
decode_pdu_type(?WSP_Post) -> 'POST';
decode_pdu_type(?WSP_Put) -> 'PUT';
decode_pdu_type(Type) -> Type.  %% allow unknown pdu types.


%% Convert various data types to list

to_list(I) when integer(I) ->
    integer_to_list(I);
to_list(A) when atom(A) ->
    atom_to_list(A);
to_list(Version={X,Y}) when integer(X), integer(Y) ->
    format_version(Version);
to_list(DateTime={{_,_,_},{_,_,_}}) ->
    fmt_date(DateTime);
to_list(L) when list(L) ->
    L.



encode_capabilities(Capa) ->
    encode_capabilities(Capa,#wsp_capabilities{}).

encode_capabilities(Cap,Def) ->
    Known =
	[encode_capability(?WSP_CAP_ALIASES,
			   Cap#wsp_capabilities.aliases,
			   Def#wsp_capabilities.aliases),
	 encode_capability(?WSP_CAP_CLIENT_SDU_SIZE,
			   Cap#wsp_capabilities.client_sdu_size,
			   Def#wsp_capabilities.client_sdu_size),
	 encode_capability(?WSP_CAP_SERVER_SDU_SIZE,
			   Cap#wsp_capabilities.server_sdu_size,
			   Def#wsp_capabilities.server_sdu_size),
	 encode_capability(?WSP_CAP_PROTOCOL_OPTIONS,
			   Cap#wsp_capabilities.protocol_options,
			   Def#wsp_capabilities.protocol_options),
	 encode_capability(?WSP_CAP_METHOD_MOR,
			   Cap#wsp_capabilities.method_mor,
			   Def#wsp_capabilities.method_mor),
	 encode_capability(?WSP_CAP_PUSH_MOR,
			   Cap#wsp_capabilities.push_mor,
			   Def#wsp_capabilities.push_mor),
	 encode_capability(?WSP_CAP_EXTENDED_METHODS,
			   Cap#wsp_capabilities.extended_methods,
			   Def#wsp_capabilities.extended_methods),
	 encode_capability(?WSP_CAP_HEADER_CODE_PAGES,
			   Cap#wsp_capabilities.header_code_pages,
			   Def#wsp_capabilities.header_code_pages),
	 encode_capability(?WSP_CAP_CLIENT_MESSAGE_SIZE,
			   Cap#wsp_capabilities.client_message_size,
			   Def#wsp_capabilities.client_message_size),
	 encode_capability(?WSP_CAP_SERVER_MESSAGE_SIZE,
			   Cap#wsp_capabilities.server_message_size,
			   Def#wsp_capabilities.server_message_size)],
    Unknown =
	lists:map(fun({Id, Data}) when integer(Id) ->
			  <<1:1, Id:7, Data/binary>>;
		     ({Id,Data}) ->
			  <<(encode_text_string(Id))/binary, Data/binary>>
			      end, Cap#wsp_capabilities.unknown),
    list_to_binary(
      lists:map(fun(<<>>) -> [];
		   (Bin) ->
			[e_uintvar(size(Bin)), Bin]
		end, Known ++ Unknown)).




encode_capability(_Capa, Default, Default) ->
    <<>>;
encode_capability(Capa, Value, _) ->
    case Capa of
	?WSP_CAP_ALIASES ->
	    <<1:1, ?WSP_CAP_ALIASES:7, (encode_addresses(Value))/binary>>;

	?WSP_CAP_CLIENT_SDU_SIZE ->
	    <<1:1, ?WSP_CAP_CLIENT_SDU_SIZE:7, (e_uintvar(Value))/binary>>;

	?WSP_CAP_SERVER_SDU_SIZE ->
	    <<1:1, ?WSP_CAP_SERVER_SDU_SIZE:7, (e_uintvar(Value))/binary>>;

	?WSP_CAP_PROTOCOL_OPTIONS ->
	    Opts = case lists:member(confirmed_push, Value) of
		       true -> 16#80;
		       false -> 0
		   end bor
		   case lists:member(push, Value) of
		       true -> 16#40;
		       false -> 0
		   end bor
		   case lists:member(resume, Value) of
		       true -> 16#20;
		       false -> 0
		   end bor
		   case lists:member(acknowledgement_headers, Value) of
		       true -> 16#10;
		       false -> 0
		   end,
	    %% FIXME: symbolic encode/decode of options
	    <<1:1, ?WSP_CAP_PROTOCOL_OPTIONS:7, Opts>>;

	?WSP_CAP_METHOD_MOR ->
	    <<1:1, ?WSP_CAP_METHOD_MOR:7, (e_uintvar(Value))/binary>>;

	?WSP_CAP_PUSH_MOR ->
	    <<1:1, ?WSP_CAP_PUSH_MOR:7, (e_uintvar(Value))/binary>>;

	?WSP_CAP_EXTENDED_METHODS ->
	    <<1:1, ?WSP_CAP_EXTENDED_METHODS:7,
	     (encode_extended_methods(Value))/binary>>;

	?WSP_CAP_HEADER_CODE_PAGES ->
	    Data = list_to_binary(
		     lists:map(fun(Page) when integer(Page) -> Page;
				  ({Page,Name}) ->
				       [Page, encode_text_string(Name)]
			       end, Value)),
	    <<1:1, ?WSP_CAP_HEADER_CODE_PAGES:7, Data/binary>>;

	?WSP_CAP_CLIENT_MESSAGE_SIZE ->
	    <<1:1, ?WSP_CAP_CLIENT_MESSAGE_SIZE:7,
	     (e_uintvar(Value))/binary>>;

	?WSP_CAP_SERVER_MESSAGE_SIZE ->
	    <<1:1, ?WSP_CAP_SERVER_MESSAGE_SIZE:7,
	     (e_uintvar(Value))/binary>>;
	_ when integer(Capa) ->
	    <<1:1, Capa:7, Value/binary>>;
	_ when list(Capa) ->
	    <<(encode_text_string(Capa))/binary, Value/binary>>
   end.


decode_capabilities(<<>>, WspCaps) ->
    WspCaps;
decode_capabilities(D0,WspCaps) ->
    {Len, D1} = d_uintvar(D0),
    <<Capa:Len/binary, D2/binary>> = D1,
    WspCaps1 =
	case Capa of
	    <<1:1, Id:7, Data/binary>> ->
		decode_capa(Id, Data, WspCaps);
	    _ ->
		{Id,Data} = d_text_string(Capa),
		decode_capa(Id, Data, WspCaps)
	end,
    decode_capabilities(D2, WspCaps1).



decode_capa(Id,Data, WspCaps) ->
    case Id of
	?WSP_CAP_SERVER_SDU_SIZE ->
	    {Val,_} = d_uintvar(Data),
	    WspCaps#wsp_capabilities{server_sdu_size=Val};

	?WSP_CAP_CLIENT_SDU_SIZE ->
	    {Val,_} = d_uintvar(Data),
	    WspCaps#wsp_capabilities{client_sdu_size=Val};

	?WSP_CAP_PROTOCOL_OPTIONS ->
	    <<POP,_/binary>> = Data,
	    Opts =
		if POP band 16#80 == 16#80 -> [confirmed_push];
		   true -> []
		end ++
		if POP band 16#40 == 16#40 -> [push];
		   true -> []
		end ++
		if POP band 16#20 == 16#20 -> [resume];
		   true -> []
		end ++
		if POP band 16#10 == 16#10 -> [acknowledgement_headers];
		   true -> []
		end,
	    WspCaps#wsp_capabilities{protocol_options=Opts};

	?WSP_CAP_METHOD_MOR ->
	    {Val,_} = d_uintvar(Data),
	    WspCaps#wsp_capabilities{method_mor=Val};

	?WSP_CAP_PUSH_MOR ->
	    {Val,_} = d_uintvar(Data),
	    WspCaps#wsp_capabilities{push_mor=Val};

	?WSP_CAP_EXTENDED_METHODS ->
	    Extended = decode_extended_methods(Data),
	    WspCaps#wsp_capabilities { extended_methods = Extended };

	?WSP_CAP_HEADER_CODE_PAGES ->
	    %% Client send [Code(uint8) Name(text-string)]*
	    %% Server send [Code(uint8)]*
	    io:format("FIXME: Header Code Pages = ~p\n",[Data]),
	    WspCaps;

	?WSP_CAP_ALIASES ->
	    Aliases = decode_addresses(Data),
	    WspCaps#wsp_capabilities { aliases = Aliases };

	?WSP_CAP_CLIENT_MESSAGE_SIZE ->
	    {Val,_} = d_uintvar(Data),
	    WspCaps#wsp_capabilities{client_message_size=Val};

	?WSP_CAP_SERVER_MESSAGE_SIZE ->
	    {Val,_} = d_uintvar(Data),
	    WspCaps#wsp_capabilities{server_message_size=Val};
	_  ->
	    Unknown = [{Id, Data} | WspCaps#wsp_capabilities.unknown],
	    io:format("WARNING: ignoring unknown capability ~p\n",
		      [Unknown]),
	    WspCaps#wsp_capabilities{unknown = Unknown}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Headers = [ Header ]
%% Header  = {FieldName, FieldValue}
%% FieldName = atom()
%% FieldValue = {Value, Params}
%%            | Value
%%
%% Params = [{Param,Value} | Param]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(WH(Name,Value,Params),
	#wsp_header { name = (Name), value = (Value), params = Params}).

encode_headers(Headers) ->
    encode_headers(Headers, ?WSP_DEFAULT_VERSION).

encode_headers(Headers, Version) ->
    encode_headers(Headers, Version, []).

encode_headers([H|T], Version, Acc) ->
    encode_headers(T, Version, [encode_header(H, Version)|Acc]);
encode_headers([], _,  Acc) ->
    list_to_binary(lists:reverse(Acc)).


decode_headers(Bin) ->
    decode_headers(Bin, ?WSP_DEFAULT_VERSION).

decode_headers(<<>>, _Version) ->
    [];
decode_headers(Data, Version) ->
    decode_headers(Data, [], Version, ?WSP_DEFAULT_CODEPAGE).


decode_headers(<<1:1,Code:7,Data/binary>>,Acc,Version,CP) ->
    FieldName = lookup_field_name(Code),
    {FieldData,Data1} = scan_header_data(Data),
    H = decode_header(FieldName, FieldData,Version,CP),
    ?dbg("header: ~p, field data=~p, header=~p\n",
	 [FieldName, FieldData, H]),
    if H#wsp_header.name == 'Encoding-Version' ->
	    Version1 = H#wsp_header.value,
	    ?dbg("Version switch from ~w to ~w\n", [Version, Version1]),
	    decode_headers(Data1,[H|Acc],Version1, CP);
       true ->
	    decode_headers(Data1,[H|Acc],Version, CP)
    end;
decode_headers(Data = <<Code,_/binary>>,Acc,Version,CP)
  when Code >= 32, Code < 127->
    {TmpField,Data1} = d_text_string(Data),
    FieldName = normalise_field_name(TmpField),
    {FieldData,Data2} = scan_header_data(Data1),
    H = decode_header(FieldName,FieldData,Version,CP),
    ?dbg("header: ~p, field data=~p, header=~p\n",
	 [FieldName, FieldData, H]),
    if H#wsp_header.name == 'Encoding-Version' ->
	    Version1 = H#wsp_header.value,
	    ?dbg("Version switch from ~w to ~w\n", [Version, Version1]),
	    decode_headers(Data2,[H|Acc],Version1, CP);
       true ->
	    decode_headers(Data2,[H|Acc],Version, CP)
    end;
decode_headers(<<CP1,Data/binary>>,Acc,Version,_CP) when CP1 >= 1, CP1 =< 31 ->
    ?dbg("decode_headers: codpage changed form ~w -> ~w\n",[_CP,CP1]),
    decode_headers(Data,Acc,Version,CP1);
decode_headers(<<16#7f,CP1,Data/binary>>,Acc,Version,_CP) ->
    ?dbg("decode_headers: codpage changed form ~w -> ~w\n",[_CP,CP1]),
    decode_headers(Data,Acc,Version,CP1);

decode_headers(<<>>, Acc, _Version, _CP) ->
    lists:reverse(Acc).

%%
%% Retrive the header data
%%  (this makes it possible to skip unknown encoding)
%%
scan_header_data(Data = <<N,Data0/binary>>) ->
    if N >= 0, N =< 30 ->
	    <<Value:N/binary, Data1/binary>> = Data0,
	    {{short,Value}, Data1};
       N == 31 ->
	    {N1, Data1} = d_uintvar(Data0),
	    <<Value:N1/binary, Data2/binary>> = Data1,
	    {{long,Value}, Data2};
       N >= 32, N =< 127 ->
	    d_text_string(Data);
       true ->
	    { N band 16#7f, Data0}
    end.

%%
%% Decode header: return #wsp_header
%%
decode_header(Field, Value) ->
    decode_header(Field, Value,
		  ?WSP_DEFAULT_VERSION,
		  ?WSP_DEFAULT_CODEPAGE).

decode_header(Field, Value, Version, 1) ->
    case Field of
	'Accept' ->
	    decode_accept(Value, Version);

	'Accept-Charset' when Version >= ?WSP_13 ->
	    decode_accept_charset(Value, Version);
	'Accept-Charset' ->
	    decode_accept_charset(Value, Version);

	'Accept-Encoding' when Version >= ?WSP_13 ->
	    decode_accept_encoding(Value, Version);
	'Accept-Encoding' ->
	    decode_accept_encoding(Value, Version);

	'Accept-Language' ->
	    decode_accept_language(Value, Version);
	'Accept-Ranges' ->
	    decode_accept_ranges(Value, Version);
	'Age' ->
	    decode_age(Value,Version);
	'Allow' ->
	    decode_allow(Value,Version);
	'Authorization' ->
	    decode_authorization(Value,Version);

	'Cache-Control' when Version >= ?WSP_14 ->
	    decode_cache_control(Value,Version);
	'Cache-Control' when Version >= ?WSP_13 ->
	    decode_cache_control(Value,Version);
	'Cache-Control' ->
	    decode_cache_control(Value,Version);

	'Connection' ->
	    decode_connection(Value,Version);
	'Content-Base' ->
	    decode_content_base(Value,Version);
	'Content-Encoding' ->
	    decode_content_encoding(Value,Version);
	'Content-Language' ->
	    decode_content_language(Value,Version);
	'Content-Length' ->
	    decode_content_length(Value,Version);
	'Content-Location' ->
	    decode_content_location(Value,Version);
	'Content-Md5' ->
	    decode_content_md5(Value,Version);

	'Content-Range' when Version >= ?WSP_13 ->
	    decode_content_range(Value,Version);
	'Content-Range' ->
	    decode_content_range(Value,Version);

	'Content-Type' ->
	    decode_content_type(Value,Version);
	'Date' ->
	    decode_date(Value, Version);
	'Etag' ->
	    decode_etag(Value,Version);
	'Expires' ->
	    decode_expires(Value,Version);
	'From' ->
	    decode_from(Value,Version);
	'Host' ->
	    decode_host(Value,Version);
	'If-Modified-Since' ->
	    decode_if_modified_since(Value,Version);
	'If-Match' ->
	    decode_if_match(Value,Version);
	'If-None-Match' ->
	    decode_if_none_match(Value,Version);
	'If-Range' ->
	    decode_if_range(Value,Version);
	'If-Unmodified-Since' ->
	    decode_if_unmodified_since(Value,Version);
	'Location' ->
	    decode_location(Value,Version);
	'Last-Modified' ->
	    decode_last_modified(Value,Version);
	'Max-Forwards' ->
	    decode_max_forwards(Value,Version);
	'Pragma' ->
	    decode_pragma(Value,Version);
	'Proxy-Authenticate' ->
	    decode_proxy_authenticate(Value,Version);
	'Proxy-Authorization' ->
	    decode_proxy_authorization(Value,Version);
	'Public' ->
	    decode_public(Value,Version);
	'Range' ->
	    decode_range(Value,Version);
	'Referer' ->
	    decode_referer(Value,Version);
	'Retry-After' ->
	    decode_retry_after(Value,Version);
	'Server' ->
	    decode_server(Value,Version);
	'Transfer-Encoding' ->
	    decode_transfer_encoding(Value,Version);
	'Upgrade' ->
	    decode_upgrade(Value,Version);
	'User-Agent' ->
	    decode_user_agent(Value,Version);
	'Vary' ->
	    decode_vary(Value,Version);
	'Via' ->
	    decode_via(Value,Version);
	'Warning' ->
	    decode_warning(Value,Version);
	'Www-Authenticate' ->
	    decode_www_authenticate(Value,Version);

	'Content-Disposition' when Version >= ?WSP_14 ->
	    decode_content_disposition(Value,Version);
	'Content-Disposition' ->
	    decode_content_disposition(Value,Version);

	'X-Wap-Application-Id' when Version >= ?WSP_12 ->
	    decode_x_wap_application_id(Value,Version);

	'X-Wap-Content-Uri' when Version >= ?WSP_12 ->
	    decode_x_wap_content_uri(Value,Version);

	'X-Wap-Initiator-Uri' when Version >= ?WSP_12 ->
	    decode_x_wap_initiator_uri(Value,Version);

	'Accept-Application' when Version >= ?WSP_12 ->
	    decode_accept_application(Value,Version);

	'Bearer-Indication' when Version >= ?WSP_12 ->
	    decode_bearer_indication(Value,Version);

	'Push-Flag' when Version >= ?WSP_12 ->
	    decode_push_flag(Value,Version);

	'Profile' when Version >= ?WSP_12 ->
	    decode_profile(Value,Version);

	'Profile-Diff' when Version >= ?WSP_12 ->
	    decode_profile_diff(Value,Version);

	'Profile-Warning' when Version >= ?WSP_12 ->
	    decode_profile_warning(Value,Version);

	'Expect' when Version >= ?WSP_15 ->
	    decode_expect(Value,Version);
	'Expect' when Version >= ?WSP_13 ->
	    decode_expect(Value,Version);

	'Te' when Version >= ?WSP_13 ->
	    decode_te(Value,Version);
	'Trailer' when Version >= ?WSP_13 ->
	    decode_trailer(Value,Version);

	'X-Wap-Tod' when Version >= ?WSP_13 ->
	    decode_x_wap_tod(Value,Version);
	'X-Wap.tod' when Version >= ?WSP_13 ->
	    decode_x_wap_tod(Value,Version);

	'Content-Id' when Version >= ?WSP_13 ->
	    decode_content_id(Value,Version);
	'Set-Cookie' when Version >= ?WSP_13 ->
	    decode_set_cookie(Value,Version);
	'Cookie' when Version >= ?WSP_13 ->
	    decode_cookie(Value,Version);

	'Encoding-Version' when Version >= ?WSP_13 ->
	    decode_encoding_version(Value,Version);
	'Profile-Warning' when Version >= ?WSP_14 ->
	    decode_profile_warning(Value,Version);

	'X-Wap-Security' when Version >= ?WSP_14 ->
	    decode_x_wap_security(Value,Version);
	'X-Wap-Loc-Invocation' when Version >= ?WSP_15 ->
	    decode_x_wap_loc_invocation(Value,Version); %% ???
	'X-Wap-Loc-Delivery' when Version >= ?WSP_15 ->
	    decode_x_wap_loc_delivery(Value,Version);   %% ???
	_ ->
	    ?dbg("Warning: none standard field ~p in version ~p codepage=1\n",
		 [Field, Version]),
	    ?WH(Field, Value, [])
    end;
decode_header(Field, Value, _Version, _CP) ->
    ?dbg("Warning: none standard field ~p in version ~p codepage=~w\n",
	 [Field, _Version, _CP]),
    ?WH(Field, Value, []).

%%
%% Encode field and value according to version
%% FIXME: spilt multiple header values (i.e Via) into multiple
%%        headers
%%
encode_header(H, Version) ->
    case H#wsp_header.name of
	'Accept' ->
	    [16#80, encode_accept(H, Version)];
	'Accept-Charset' when Version >= ?WSP_13 ->
	    [16#bb, encode_accept_charset(H, Version)];
	'Accept-Charset' ->
	    [16#81, encode_accept_charset(H, Version)];
	'Accept-Encoding' when Version >= ?WSP_13 ->
	    [16#bc, encode_accept_encoding(H, Version)];
	'Accept-Encoding' ->
	    [16#82, encode_accept_encoding(H, Version)];
	'Accept-Language' ->
	    [16#83, encode_accept_language(H, Version)];
	'Accept-Ranges' ->
	    [16#84, encode_accept_ranges(H, Version)];
	'Accept-Application' when Version >= ?WSP_12 ->
	    [16#b2, encode_accept_application(H,Version)];
	'Age' ->
	    [16#85, encode_age(H, Version)];
	'Allow' ->
	    [16#86, encode_allow(H, Version)];
	'Authorization' ->
	    [16#87, encode_authorization(H, Version)];
	'Cache-Control' when Version >= ?WSP_14 ->
	    [16#c7, encode_cache_control(H, Version)];
	'Cache-Control' when Version >= ?WSP_13 ->
	    [16#bd, encode_cache_control(H, Version)];
	'Cache-Control' ->
	    [16#88, encode_cache_control(H, Version)];
	'Connection' ->
	    [16#89, encode_connection(H, Version)];
	'Content-Base' ->
	    [16#8a, encode_content_base(H, Version)];
	'Content-Encoding' ->
	    [16#8b, encode_content_encoding(H, Version)];

	'Content-Language' ->
	    [16#8c, encode_content_language(H,Version)];
	'Content-Length' ->
	    [16#8d, encode_content_length(H,Version)];
	'Content-Location' ->
	    [16#8e, encode_content_location(H,Version)];
	'Content-Md5' ->
	    [16#8f, encode_content_md5(H,Version)];
	'Content-Range' when Version >= ?WSP_13 ->
	    [16#be, encode_content_range(H,Version)];
	'Content-Range' ->
	    [16#90, encode_content_range(H,Version)];
	'Content-Type' ->
	    [16#91, encode_content_type(H,Version)];
	'Date' ->
	    [16#92, encode_date(H,Version)];
	'Etag' ->
	    [16#93, encode_etag(H,Version)];
	'Expires' ->
	    [16#94, encode_expires(H,Version)];
	'From' ->
	    [16#95, encode_from(H,Version)];
	'Host' ->
	    [16#96, encode_host(H,Version)];
	'If-Modified-Since' ->
	    [16#97, encode_if_modified_since(H,Version)];
	'If-Match' ->
	    [16#98, encode_if_match(H,Version)];
	'If-None-Match' ->
	    [16#99, encode_if_none_match(H,Version)];
	'If-Range' ->
	    [16#9a, encode_if_range(H,Version)];
	'If-Unmodified-Since' ->
	    [16#9b, encode_if_unmodified_since(H,Version)];
	'Location' ->
	    [16#9c, encode_location(H,Version)];
	'Last-Modified' ->
	    [16#9d, encode_last_modified(H,Version)];
	'Max-Forwards' ->
	    [16#9e, encode_max_forwards(H,Version)];
	'Pragma' ->
	    [16#9f, encode_pragma(H,Version)];
	'Proxy-Authenticate' ->
	    [16#a0, encode_proxy_authenticate(H,Version)];
	'Proxy-Authorization' ->
	    [16#a1, encode_proxy_authorization(H,Version)];
	'Public' ->
	    [16#a2, encode_public(H,Version)];
	'Range' ->
	    [16#a3, encode_range(H,Version)];
	'Referer' ->
	    [16#a4, encode_referer(H,Version)];
	'Retry-After' ->
	    [16#a5, encode_retry_after(H,Version)];
	'Server' ->
	    [16#a6, encode_server(H,Version)];
	'Transfer-Encoding' ->
	    [16#a7, encode_transfer_encoding(H,Version)];
	'Upgrade' ->
	    [16#a8, encode_upgrade(H,Version)];
	'User-Agent' ->
	    [16#a9, encode_user_agent(H,Version)];
	'Vary' ->
	    [16#aa, encode_vary(H,Version)];
	'Via' ->
	    [16#ab, encode_via(H,Version)];
	'Warning' ->
	    [16#ac, encode_warning(H,Version)];
	'Www-Authenticate' ->
	    [16#ad, encode_www_authenticate(H,Version)];

	'Content-Disposition' when Version >= ?WSP_14 ->
	    [16#c5, encode_content_disposition(H,Version)];
	'Content-Disposition' ->
	    [16#ae, encode_content_disposition(H,Version)];


	'X-Wap-Application-Id' when Version >= ?WSP_12 ->
	    [16#af, encode_x_wap_application_id(H,Version)];
	'X-Wap-Content-Uri' when Version >= ?WSP_12 ->
	    [16#b0, encode_x_wap_content_uri(H,Version)];
	'X-Wap-Initiator-Uri' when Version >= ?WSP_12 ->
	    [16#b1, encode_x_wap_initiator_uri(H,Version)];

	'Bearer-Indication' when Version >= ?WSP_12 ->
	    [16#b3, encode_bearer_indication(H,Version)];
	'Push-Flag' when Version >= ?WSP_12 ->
	    [16#b4, encode_push_flag(H,Version)];

	'Profile' when Version >= ?WSP_12 ->
	    [16#b5, encode_profile(H,Version)];
	'Profile-Diff' when Version >= ?WSP_12 ->
	    [16#b6, encode_profile_diff(H,Version)];
	'Profile-Warning' when Version >= ?WSP_14 ->
	    [16#c4, encode_profile_warning(H,Version)];
	'Profile-Warning' when Version >= ?WSP_12 ->
	    [16#b7, encode_profile_warning(H,Version)];

	'Expect' when Version >= ?WSP_15 ->
	    [16#c8, encode_expect(H,Version)];
	'Expect' when Version >= ?WSP_13 ->
	    [16#b8, encode_expect(H,Version)];
	'Te' when Version >= ?WSP_13 ->
	    [16#b9, encode_te(H,Version)];
	'Trailer' when Version >= ?WSP_13 ->
	    [16#ba, encode_trailer(H,Version)];
	'X-Wap-Tod' when Version >= ?WSP_13 ->
	    [16#bf, encode_x_wap_tod(H,Version)];
	'Content-Id' when Version >= ?WSP_13 ->
	    [16#c0, encode_content_id(H,Version)];
	'Set-Cookie' when Version >= ?WSP_13 ->
	    [16#c1, encode_set_cookie(H,Version)];
	'Cookie' when Version >= ?WSP_13 ->
	    [16#c2, encode_cookie(H,Version)];
	'Encoding-Version' when Version >= ?WSP_13 ->
	    [16#c3, encode_encoding_version(H,Version)];
	'Encoding-Version' when Version < ?WSP_13 ->
	    [encode_text_string("Encoding-Version"),
	     encode_text_string(lists:flatten(format_version(H#wsp_header.value)))];

	'X-Wap-Security' when Version >= ?WSP_14 ->
	    [16#c6, encode_x_wap_security(H,Version)];
	'X-Wap-Loc-Invocation' when Version >= ?WSP_15 ->
	    [16#c9, encode_x_wap_loc_invocation(H,Version)];
	'X-Wap-Loc-Delivery' when Version >= ?WSP_15 ->
	    [16#ca, encode_x_wap_loc_delivery(H,Version)];
	Field when atom(Field) ->
	    [encode_text_string(atom_to_list(Field)),
	     encode_text_string(H#wsp_header.value)];
	Field when list(Field) ->
	    [encode_text_string(Field),
	     encode_text_string(H#wsp_header.value)]
    end.

%%
%% Convert HTTP headers into WSP headers
%%
parse_headers([H | Hs]) ->
    parse_header(H, Hs);
parse_headers([]) ->
    [].

parse_header(H) ->
    parse_header(H, []).

parse_header({FieldName,FieldValue}, Hs) ->
    case single_comma_field(FieldName) of
	true ->
	    io:format("parse: ~s: ~s\n", [FieldName, FieldValue]),
	    H = parse_hdr(FieldName,FieldValue),
	    io:format("header: ~p\n", [H]),
	    [H | parse_headers(Hs)];
	false ->
	    Values = string:tokens(FieldValue, ","),
	    parse_header(FieldName, Values, Hs)
    end.

parse_header(FieldName, [Value|Vs], Hs) ->
    io:format("parse: ~s: ~s\n", [FieldName, Value]),
    H = parse_hdr(FieldName, Value),
    io:format("header: ~p\n", [H]),
    [H | parse_header(FieldName, Vs, Hs)];
parse_header(_FieldName, [], Hs) ->
    parse_headers(Hs).


single_comma_field(Field) ->
    case Field of
	'Set-Cookie' -> true; %% FIXME (Is multiple!)
	'Date' -> true;
	'Expires' -> true;
	'If-Modified-Since' -> true;
	'If-Range' -> true;
	'If-Unmodified-Since' -> true;
	'Last-Modified' -> true;
	'Retry-After' -> true;
	'X-Wap-Tod' -> true;
	_ -> false
    end.


parse_hdr(Field, Value0) ->
    Value = trim(Value0),
    case Field of
	'Accept' ->           parse_accept(Value);
	'Accept-Charset' ->   parse_accept_charset(Value);
	'Accept-Encoding' ->  parse_accept_encoding(Value);
	'Accept-Language' ->  parse_accept_language(Value);
	'Accept-Ranges' ->    parse_accept_ranges(Value);
	'Age' ->              parse_age(Value);
	'Allow' ->            parse_allow(Value);
	'Authorization' ->    parse_authorization(Value);
	'Cache-Control' ->    parse_cache_control(Value);
	'Connection' ->       parse_connection(Value);
	'Content-Base' ->     parse_content_base(Value);
	'Content-Encoding' -> parse_content_encoding(Value);
	'Content-Language' -> parse_content_language(Value);
	'Content-Length' ->   parse_content_length(Value);
	'Content-Location' -> parse_content_location(Value);
	'Content-Md5' ->      parse_content_md5(Value);
	'Content-Range' ->    parse_content_range(Value);
	'Content-Type' ->     parse_content_type(Value);
	'Date' ->             parse_date(Value);
	'Etag' ->             parse_etag(Value);
	'Expires' ->          parse_expires(Value);
	'From' ->             parse_from(Value);
	'Host' ->             parse_host(Value);
	'If-Modified-Since' -> parse_if_modified_since(Value);
	'If-Match' ->         parse_if_match(Value);
	'If-None-Match' ->    parse_if_none_match(Value);
	'If-Range' ->         parse_if_range(Value);
	'If-Unmodified-Since' -> parse_if_unmodified_since(Value);
	'Location' ->         parse_location(Value);
	'Last-Modified' ->    parse_last_modified(Value);
	'Max-Forwards' ->     parse_max_forwards(Value);
	'Pragma' ->           parse_pragma(Value);
	'Proxy-Authenticate' -> parse_proxy_authenticate(Value);
	'Proxy-Authorization' -> parse_proxy_authorization(Value);
	'Public' ->           parse_public(Value);
	'Range' ->            parse_range(Value);
	'Referer' ->          parse_referer(Value);
	'Retry-After' ->      parse_retry_after(Value);
	'Server' ->           parse_server(Value);
	'Transfer-Encoding' -> parse_transfer_encoding(Value);
	'Upgrade' ->          parse_upgrade(Value);
	'User-Agent' ->       parse_user_agent(Value);
	'Vary' ->             parse_vary(Value);
	'Via' ->              parse_via(Value);
	'Warning' ->          parse_warning(Value);
	'Www-Authenticate' -> parse_www_authenticate(Value);
	'Content-Disposition' -> parse_content_disposition(Value);
	'X-Wap-Application-Id' -> parse_x_wap_application_id(Value);
	'X-Wap-Content-Uri' -> parse_x_wap_content_uri(Value);
	'X-Wap-Initiator-Uri' -> parse_x_wap_initiator_uri(Value);
	'Accept-Application' -> parse_accept_application(Value);
	'Bearer-Indication' -> parse_bearer_indication(Value);
	'Push-Flag' ->        parse_push_flag(Value);
	'Profile' ->          parse_profile(Value);
	'Profile-Diff' ->     parse_profile_diff(Value);
	'Profile-Warning' ->  parse_profile_warning(Value);
	'Expect' ->           parse_expect(Value);
	'Te' ->               parse_te(Value);
	'Trailer' ->          parse_trailer(Value);
	'X-Wap-Tod' ->        parse_x_wap_tod(Value);
	'Content-Id' ->       parse_content_id(Value);
	'Set-Cookie' ->       parse_set_cookie(Value);
	'Cookie' ->           parse_cookie(Value);
	'Encoding-Version' -> parse_encoding_version(Value);
	'X-Wap-Security' ->   parse_x_wap_security(Value);
	'X-Wap-Loc-Invocation' -> parse_x_wap_loc_invocation(Value);
	'X-Wap-Loc-Delivery' -> parse_x_wap_loc_delivery(Value);
	_ ->
	    ?dbg("Warning: header field ~p not recognissed\n",[Field]),
	    #wsp_header { name = Field, value = Value}
    end.

%%
%% Format headers, will combine multiple headers into one
%% FIXME: if length is < MAX_HTTP_HEADER_LENGTH
%%
format_headers(Hs) ->
    format_hdrs(lists:keysort(#wsp_header.name,Hs), []).

format_hdrs([H | Hs], Acc) ->
    V1 = format_value(H),
    format_hdrs(Hs, H#wsp_header.name, V1, Acc);
format_hdrs([], Acc) ->
    lists:reverse(Acc).

format_hdrs([H|Hs], FieldName, FieldValue, Acc)
  when FieldName == H#wsp_header.name ->
    V1 = format_value(H),
    format_hdrs(Hs, FieldName, [FieldValue,",",V1], Acc);
format_hdrs(Hs, FieldName, FieldValue, Acc) ->
    format_hdrs(Hs, [{FieldName, lists:flatten(FieldValue)} | Acc]).


%%
%% Format header: #wsp_header => {FieldName, Value}
%%

format_header(H) ->
    {H#wsp_header.name, format_value(H)}.

format_value(H) ->
    case H#wsp_header.name of
	'Accept' ->   format_accept(H);
	'Accept-Charset' ->   format_accept_charset(H);
	'Accept-Encoding' ->  format_accept_encoding(H);
	'Accept-Language' ->  format_accept_language(H);
	'Accept-Ranges' ->    format_accept_ranges(H);
	'Age' ->     format_age(H);
	'Allow' ->   format_allow(H);
	'Authorization' ->   format_authorization(H);
	'Cache-Control' ->   format_cache_control(H);
	'Connection' ->      format_connection(H);
	'Content-Base' ->    format_content_base(H);
	'Content-Encoding' -> format_content_encoding(H);
	'Content-Language' -> format_content_language(H);
	'Content-Length' -> format_content_length(H);
	'Content-Location' -> format_content_location(H);
	'Content-Md5' -> format_content_md5(H);
	'Content-Range' -> format_content_range(H);
	'Content-Type' -> format_content_type(H);
	'Date' -> format_date(H);
	'Etag' -> format_etag(H);
	'Expires' -> format_expires(H);
	'From' -> format_from(H);
	'Host' -> format_host(H);
	'If-Modified-Since' -> format_if_modified_since(H);
	'If-Match' -> format_if_match(H);
	'If-None-Match' -> format_if_none_match(H);
	'If-Range' -> format_if_range(H);
	'If-Unmodified-Since' -> format_if_unmodified_since(H);
	'Location' -> format_location(H);
	'Last-Modified' -> format_last_modified(H);
	'Max-Forwards' -> format_max_forwards(H);
	'Pragma' -> format_pragma(H);
	'Proxy-Authenticate' -> format_proxy_authenticate(H);
	'Proxy-Authorization' -> format_proxy_authorization(H);
	'Public' -> format_public(H);
	'Range' -> format_range(H);
	'Referer' -> format_referer(H);
	'Retry-After' -> format_retry_after(H);
	'Server' -> format_server(H);
	'Transfer-Encoding' -> format_transfer_encoding(H);
	'Upgrade' -> format_upgrade(H);
	'User-Agent' -> format_user_agent(H);
	'Vary' -> format_vary(H);
	'Via' -> format_via(H);
	'Warning' -> format_warning(H);
	'Www-Authenticate' -> format_www_authenticate(H);
	'Content-Disposition' -> format_content_disposition(H);
	'X-Wap-Application-Id' -> format_x_wap_application_id(H);
	'X-Wap-Content-Uri' -> format_x_wap_content_uri(H);
	'X-Wap-Initiator-Uri' -> format_x_wap_initiator_uri(H);
	'Accept-Application' -> format_accept_application(H);
	'Bearer-Indication' -> format_bearer_indication(H);
	'Push-Flag' -> format_push_flag(H);
	'Profile' -> format_profile(H);
	'Profile-Diff' -> format_profile_diff(H);
	'Profile-Warning' -> format_profile_warning(H);
	'Expect' ->  format_expect(H);
	'Te' -> format_te(H);
	'Trailer' -> format_trailer(H);
	'X-Wap-Tod' ->  format_x_wap_tod(H);
	'Content-Id' -> format_content_id(H);
	'Set-Cookie' -> format_set_cookie(H);
	'Cookie' -> format_cookie(H);
	'Encoding-Version' -> format_encoding_version(H);
	'X-Wap-Security' -> format_x_wap_security(H);
	'X-Wap-Loc-Invocation' -> format_x_wap_loc_invocation(H);
	'X-Wap-Loc-Delivery' -> format_x_wap_loc_delivery(H);
	_Field ->
	    ?dbg("Warning: header field ~s not recognissed\n",[_Field]),
	    to_list(H#wsp_header.value)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encode of field values
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Accept: <content-type> [q=<q-value>] [params]
%% Type: Multiple
%% Ref: 8.4.2.7
%%
%%  Accept-value = Constrained-media | Accept-general-form
%%
%%  Accept-general-form = Value-length Media-range [Accept-parameters]
%%  Media-range = (Well-known-media | Extension-media) *(Parameter)
%%  Accept-parameters = Q-token Q-value *(Accept-extension)
%%  Accept-extension = Parameter
%%  Constrain-media = Constrained-encoding
%%  Well-known-media = Integer-value
%%  Constrained-encoding = Short-Integer | Extension-media
%%  Q-token = <Octet 128>
%%
parse_accept(String) ->
    %% FIXME
    ?WH('Accept',String,[]).

format_accept(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_accept(H, Version) ->
    case encode_params(H#wsp_header.params,Version) of
	<<>> ->
	    encode_well_known_media(H#wsp_header.value, Version);
	Params ->
	    Media = encode_well_known_media(H#wsp_header.value, Version),
	    e_value(Media, Params)
    end.

decode_accept(Value, Version) when integer(Value) ->
    %% Constrained-encoding: Short-Integer
    ?WH('Accept',decode_well_known_media(Value, Version),[]);
decode_accept(Value, Version) when list(Value) ->
    ?WH('Accept',decode_well_known_media(Value,Version),[]);
decode_accept({_,Data}, Version)  ->
    %% Accept-general-form
    {Value,QData} = scan_header_data(Data),
    Media_Range = decode_well_known_media(Value,Version),
    Params =  decode_params(QData, Version),
    ?WH('Accept',Media_Range,Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Accept-Charset: <charset> | * [q=<q-value>]
%% Type: Multiple
%% Ref: 8.4.2.8
%% Note that the definition of this one is a mess!!!!
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_accept_charset(String) ->
    %% FIXME
    ?WH('Accept-Charset',String,[]).

format_accept_charset(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_accept_charset(H, _Version) ->
    %% FIXME
    encode_text_string(H#wsp_header.value).

decode_accept_charset(0, _Version) ->
    ?WH('Accept-Charset',"*",[]);
decode_accept_charset(Value, _Version) when integer(Value) ->
    ?WH('Accept-Charset', decode_charset(Value),[]);
decode_accept_charset(Value, _Version) when list(Value) ->
    ?WH('Accept-Charset',Value,[]);
decode_accept_charset({short,Data}, _Version) ->
    %% Me guessing that the short form SHOULD be mulit octet integer!!!
    Value = d_long(Data),
    ?WH('Accept-Charset', decode_charset(Value),[]);
decode_accept_charset({long,Value}, _Version) ->
    {Data1, QData} = scan_header_data(Value),
    CharSet = case Data1 of
		  0 ->
		      "*";
		  Value1 when integer(Value1) ->
		      decode_charset(Value1);
		  Value1 when list(Value1) ->
		      Value1;
		  {short,Value1} ->
		      Value2 = d_long(Value1),
		      decode_charset(Value2)
	      end,
    Params = if QData == <<>> ->
		     [];
		true ->
		     {QValue,_} = d_q_value(QData),
		     {CharSet,[{q, QValue}]}
	     end,
    ?WH('Accept-Charset',CharSet, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Accept-Encoding: gzip | compress | deflate | * [q=<q-value>]
%% Ref:
%% Type: Multiple
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_accept_encoding(String) ->
    ?WH('Accept-Encoding',String,[]).

format_accept_encoding(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_accept_encoding(H, _Version) ->
    %% FIXME general form
    case H#wsp_header.value of
	"gzip"     -> ?ENCODE_SHORT(0);
	"compress" -> ?ENCODE_SHORT(1);
	"deflate"  -> ?ENCODE_SHORT(2);
	Value      -> encode_text_string(Value)
    end.

decode_accept_encoding(0, _Version) ->
    ?WH('Accept-Encoding',"gzip",[]);
decode_accept_encoding(1, _Version) ->
    ?WH('Accept-Encoding',"compress",[]);
decode_accept_encoding(2, _Version) ->
    ?WH('Accept-Encoding',"deflate",[]);
decode_accept_encoding(Value, Version) when list(Version) ->
    ?WH('Accept-Encoding',Value,[]);
decode_accept_encoding({_,Data}, _Version) when binary(Data) ->
    {Enc, Data1} = scan_header_data(Data),
    Params = if Data1 == <<>> ->
		     [];
		true ->
		     {QVal,_} = d_q_value(Data1),
		     [{q, QVal}]
	     end,
    case Enc of
	0 -> ?WH('Accept-Encoding',"gzip",Params);
	1 -> ?WH('Accept-Encoding',"compress",Params);
	2 -> ?WH('Accept-Encoding',"deflate",Params);
	3 -> ?WH('Accept-Encoding',"*",Params);
	_ when list(Enc) ->
	    ?WH('Accept-Encoding',Enc,Params)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	%%
%% Accept-Language: * | <lang> [q=<q-value>]
%% Type: Multiple
%% Ref: 8.4.2.10
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_accept_language(Value) ->
    ?WH('Accept-Language',Value,[]).

format_accept_language(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_accept_language(H, _Version) ->
    case H#wsp_header.value of
	"*" -> ?ENCODE_SHORT(0);
	Lang -> case catch encode_lang(Lang) of
		    {'EXIT', _} -> encode_text_string(Lang);
		    Code -> encode_integer(Code)
		end
    end.

decode_accept_language(0, _Version) ->
    ?WH('Accept-Language',"*",[]);
decode_accept_language(Value, _Version) when integer(Value) ->
    ?WH('Accept-Language',decode_lang(Value),[]);
decode_accept_language(Value, _Version) when list(Value) ->
    ?WH('Accept-Language',Value,[]);
decode_accept_language({_,Data}, _Version) ->
    {Data1, QData} = scan_header_data(Data),
    Charset = case Data1 of
		  0 ->
		      "*";
		  Value1 when integer(Value1) ->
		      decode_lang(Value1);
		  Value1 when list(Value1) ->
		      Value1;
		  {short,Data2} ->
		      decode_lang(d_long(Data2))
	      end,
    Params =
	if QData == <<>> ->
		[];
	   true ->
		{QVal,_} = d_q_value(QData),
		[{q, QVal}]
	end,
    ?WH('Accept-Language',Charset,Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Accept-Ranges: none | bytes | <extension>
%% Type: single
%% Ref:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_accept_ranges(Value) ->
    ?WH('Accept-Ranges', Value, []).

format_accept_ranges(H) ->
    H#wsp_header.value.

encode_accept_ranges(H, _Version) ->
    case H#wsp_header.value of
	"none" -> ?ENCODE_SHORT(0);
	"bytes" -> ?ENCODE_SHORT(1);
	Value -> encode_text_string(Value)
    end.

decode_accept_ranges(0, _Version) ->
    ?WH('Accept-Ranges', "none", []);
decode_accept_ranges(1, _Version) ->
    ?WH('Accept-Ranges', "bytes", []);
decode_accept_ranges(Value, _Version) when list(Value) ->
    ?WH('Accept-Ranges', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Age: <delta-seconds>
%% Type: single
%% Ref:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_age(Value) ->
    %% FIXME
    ?WH('Age', Value, []).

format_age(H) ->
    integer_to_list(H#wsp_header.value).

encode_age(H, _Version) ->
    e_delta_seconds(H#wsp_header.value).

decode_age(Value, _Version) when integer(Value) ->
    ?WH('Age', Value, []);
decode_age({short,Data}, _Version) ->
    ?WH('Age', d_long(Data), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Allow: <well-known-method>
%% Type: multiple
%% Ref:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_allow(Value) ->
    ?WH('Allow', parse_well_known_method(Value), []).

format_allow(H) ->
    atom_to_list(H#wsp_header.value).

encode_allow(H, Version) ->
    encode_well_known_method(H#wsp_header.value, Version).

decode_allow(Value, Version) ->
    ?WH('Allow', decode_well_known_method(Value,Version), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Authorization:
%% Ref: 8.4.2.14
%% Type: server-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_authorization(Value) ->
    parse_credentials('Authorization', Value).

format_authorization(H) ->
    format_credentials(H#wsp_header.value, H#wsp_header.params).

encode_authorization(H, Version) ->
    encode_credentials(H#wsp_header.value, H#wsp_header.params, Version).

decode_authorization({_,Data}, Version) ->
    decode_credentials('Authorization', Data, Version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Cache-Control:
%% 8.4.2.15
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_cache_control(Value) ->
    case Value of
	"no-cache" -> ?WH('Cache-Control',Value,[]);
	"no-store" -> ?WH('Cache-Control',Value,[]);
	"max-stale" -> ?WH('Cache-Control',Value,[]);
	"only-if-cached" -> ?WH('Cache-Control',Value,[]);
	"private" -> ?WH('Cache-Control',Value,[]);
	"public" -> ?WH('Cache-Control',Value,[]);
	"no-transform" -> ?WH('Cache-Control',Value,[]);
	"must-revalidate" -> ?WH('Cache-Control',Value,[]);
	"proxy-revalidate" -> ?WH('Cache-Control',Value,[]);
	_ ->
	    Params = parse_params([Value]),
	    ?WH('Cache-Control',"",Params)
    end.

format_cache_control(H) ->
    if H#wsp_header.value == "" ->
	    format_params0(H#wsp_header.params);
       true ->
	    [H#wsp_header.value, format_params(H#wsp_header.params)]
    end.



encode_cache_control(H, Version) ->
    case H#wsp_header.value of
	"no-cache" -> ?ENCODE_SHORT(0);
	"no-store" -> ?ENCODE_SHORT(1);
	"max-stale" -> ?ENCODE_SHORT(3);
	"only-if-cached" -> ?ENCODE_SHORT(5);
	"private" -> ?ENCODE_SHORT(7);
	"public" -> ?ENCODE_SHORT(6);
	"no-transform" -> ?ENCODE_SHORT(8);
	"must-revalidate" -> ?ENCODE_SHORT(9);
	"proxy-revalidate" -> ?ENCODE_SHORT(10);
	"" ->
	    case H#wsp_header.params of
		[{'no-cache',Field}] ->
		    e_value(?ENCODE_SHORT(0),
			    e_field_name(Field,Version));
		[{'max-age',Sec}] ->
		    e_value(?ENCODE_SHORT(2),
			    e_delta_seconds(Sec));
		[{'max-fresh',Sec}] ->
		    e_value(?ENCODE_SHORT(4),
			    e_delta_seconds(Sec));
		[{'private',Field}] ->
		    e_value(?ENCODE_SHORT(7),
			    e_field_name(Field,Version));
		[{'s-maxage',Sec}] ->
		    e_value(?ENCODE_SHORT(11),
			    e_delta_seconds(Sec))
	    end;
	Ext ->
	    [Param] = H#wsp_header.params,
	    e_value(encode_text_string(Ext),
		    encode_parameter(Param, Version))
    end.


decode_cache_control(Value, _Version) when integer(Value) ->
    case Value of
	0 -> ?WH('Cache-Control',"no-cache",[]);
	1 -> ?WH('Cache-Control',"no-store",[]);
	3 -> ?WH('Cache-Control',"max-stale",[]);
	5 -> ?WH('Cache-Control',"only-if-cached",[]);
	7 -> ?WH('Cache-Control',"private",[]);
	6 -> ?WH('Cache-Control',"public",[]);
	8 -> ?WH('Cache-Control',"no-transform",[]);
	9 -> ?WH('Cache-Control',"must-revalidate",[]);
	10 -> ?WH('Cache-Control',"proxy-revalidate",[])
    end;
decode_cache_control(Value, _Version) when list(Value) ->
    ?WH('Cache-Control',Value,[]);
decode_cache_control({_,Data},Version) ->
    {CacheDir, Data1} = scan_header_data(Data),
    case CacheDir of
	0 ->
	    {Field,_} = d_field_name(Data1),
	    ?WH('Cache-Control',"",[{'no-cache',Field}]);
	2 ->
	    {Sec,_} = d_integer_value(Data1),
	    ?WH('Cache-Control',"",[{'max-age',Sec}]);
	4 ->
	    {Sec,_} = d_integer_value(Data1),
	    ?WH('Cache-Control',"",[{'max-fresh',Sec}]);
	7 ->
	    {Field,_} = d_field_name(Data1),
	    ?WH('Cache-Control',"",[{private,Field}]);
	11 ->
	    {Sec,_} = d_integer_value(Data1),
	    ?WH('Cache-Control',"",[{'s-maxage',Sec}]);
	Ext when list(Ext) ->
	    {Param,_} = decode_parameter(Data1, Version),
	    ?WH('Cache-Control',Ext,[Param])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Connection: close | Ext
%% Type: single
%% Ref: 8.4.2.16
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_connection(Value) ->
    ?WH('Connection', Value, []).

format_connection(H) ->
    H#wsp_header.value.

encode_connection(H, _Version) ->
    case H#wsp_header.value of
	"close" -> ?ENCODE_SHORT(0);
	Value ->  encode_text_string(Value)
    end.

decode_connection(0, _Version) ->
    ?WH('Connection', "close", []);
decode_connection(Value, _Version) when list(Value) ->
    ?WH('Connection', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Base: <uri>
%% Type: single
%% Ref:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_content_base(Value) ->
    ?WH('Content-Base', Value, []).

format_content_base(H) ->
    H#wsp_header.value.

encode_content_base(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_content_base(Value, _Version) when list(Value) ->
    ?WH('Content-Base', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Encoding:
%% Ref: 8.4.2.18
%% Type: single
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_content_encoding(Value) ->
    ?WH('Content-Encoding', tolower(Value), []).

format_content_encoding(H) ->
    H#wsp_header.value.

encode_content_encoding(H, _Version) ->
    case H#wsp_header.value of
	"gzip"     -> ?ENCODE_SHORT(0);
	"compress" -> ?ENCODE_SHORT(1);
	"deflate"  -> ?ENCODE_SHORT(2);
	Value -> encode_text_string(Value)
    end.

decode_content_encoding(0, _Version) ->
    ?WH('Content-Encoding', "gzip", []);
decode_content_encoding(1, _Version) ->
    ?WH('Content-Encoding', "compress", []);
decode_content_encoding(2, _Version) ->
    ?WH('Content-Encoding',"deflate", []);
decode_content_encoding(Value, _Version) when list(Value) ->
    ?WH('Content-Encoding', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Language:
%% Ref: 8.4.2.19
%% Type: single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_content_language(Value) ->
    ?WH('Content-Language', Value, []).

format_content_language(H) ->
    H#wsp_header.value.

encode_content_language(H, _Version) ->
    case H#wsp_header.value of
	"*" -> ?ENCODE_SHORT(0);
	Lang -> case catch encode_lang(Lang) of
		    {'EXIT', _} -> encode_text_string(Lang);
		    Code -> encode_integer(Code)
		end
    end.

decode_content_language(0, _Version) ->
    ?WH('Content-Language',"*",[]);
decode_content_language(Value, _Version) when integer(Value) ->
    ?WH('Content-Language',decode_lang(Value),[]);
decode_content_language(Value, _Version) when list(Value) ->
    ?WH('Content-Language',Value,[]);
decode_content_language({short,Data}, _Version) ->
    Value = d_long(Data),
    ?WH('Content-Language',decode_lang(Value),[]);
decode_content_language(Value, _Version) when list(Value) ->
    ?WH('Content-Language',Value,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Length: <integer-value>
%% Ref: 8.4.2.20
%% Type: single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_content_length(Value) ->
    ?WH('Content-Length', list_to_integer(Value), []).

format_content_length(H) ->
    integer_to_list(H#wsp_header.value).

encode_content_length(H, _Version) ->
    encode_integer(H#wsp_header.value).

decode_content_length(Value, _Version) when integer(Value) ->
    ?WH('Content-Length', Value, []);
decode_content_length({short,Data}, _Version) ->
    Value = d_long(Data),
    ?WH('Content-Length', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Location: <uri-value>
%% Ref: 8.4.2.21
%% Type: single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_content_location(Value) ->
    ?WH('Content-Location', Value, []).

format_content_location(H) ->
    H#wsp_header.value.

encode_content_location(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_content_location(Value, _Version) when list(Value) ->
    ?WH('Content-Location', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Md5: <value-length> <digest>
%% Ref: 8.4.2.22
%% Type: single, end-to-end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_content_md5(Value) ->
    ?WH('Content-Md5', base64:decode(Value), []).

format_content_md5(H) ->
    base64:encode(H#wsp_header.value).

encode_content_md5(H, _Version) ->
    e_value(H#wsp_header.value).

decode_content_md5({_,Data}, _Version) ->
    ?WH('Content-Md5', Data, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Range: <first-byte-pos> <entity-len>
%% Ref: 8.4.2.23
%% Type: single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_content_range(Value) ->
    %% FIXME:
    ?WH('Content-Range', Value, []).

format_content_range(H) ->
    {Pos,Len} = H#wsp_header.value,
    if Len == "*" ->
	    ["bytes ", integer_to_list(Pos), "-*/*"];
       true ->
	    ["bytes ", integer_to_list(Pos),"-",integer_to_list(Len-1),
	     "/", integer_to_list(Len)]
    end.

encode_content_range(H, _Version) ->
    case H#wsp_header.value of
	{Pos, "*"} ->
	    e_value(e_uintvar(Pos), <<128>>);
	{Pos, Len} ->
	    e_value(e_uintvar(Pos), e_uintvar(Len))
    end.

decode_content_range({_, Data}, _Version) ->
    {Pos, Data1} = d_uintvar(Data),
    Len =
	case Data1 of
	    <<128>> -> "*";
	    _ ->
		{L, _} = d_uintvar(Data1),
		L
	end,
    ?WH('Content-Range', {Pos,Len}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Type:
%% Ref: 8.4.2.24
%% Type: single, end-to-end
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_content_type(Value) ->
    case string:tokens(Value, ";") of
	[Type | Ps] ->
	    Params = parse_params(Ps),
	    ?WH('Content-Type', Type, Params);
	[] ->
	    ?WH('Content-Type', Value, [])
    end.

format_content_type(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_content_type(H, Version) ->
    case encode_params(H#wsp_header.params,Version) of
	<<>> ->
	    encode_well_known_media(H#wsp_header.value, Version);
	Params ->
	    Media = encode_well_known_media(H#wsp_header.value, Version),
	    e_value(Media, Params)
    end.

decode_content_type(Value,Version) when integer(Value) ->
    ?WH('Content-Type', decode_well_known_media(Value,Version), []);
decode_content_type(Value,Version) when list(Value) ->
    ?WH('Content-Type', decode_well_known_media(Value,Version), []);
decode_content_type({_, Data}, Version) ->
    {Value,Data1} = scan_header_data(Data),
    ContentType = if integer(Value) ->
			  decode_well_known_media(Value,Version);
		     list(Value) ->
			  decode_well_known_media(Value,Version);
		     true ->
			  {_,Data2} = Value,
			  decode_well_known_media(d_long(Data2),Version)
		  end,
    Params = decode_params(Data1, Version),
    ?WH('Content-Type', ContentType, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Date: <http-date>
%% Ref: 8.2.4.25
%% Type: single, end-to-end
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_date(String) ->
    {DateTime, _} = parse_http_date(String),
    ?WH('Date', DateTime, []).

format_date(H) ->
    fmt_date(H#wsp_header.value).

encode_date(H, _Version) ->
    e_date(H#wsp_header.value).

decode_date(Value, _Version) ->
    ?WH('Date', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Etag: <text-string>
%% Ref: 8.2.4.26
%% Type: single, end-to-end
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_etag(Value) ->
    ?WH('Etag', Value, []).

format_etag(H) ->
    H#wsp_header.value.

encode_etag(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_etag(Value, _Version) ->
    ?WH('Etag', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Expires: <date-value>
%% Ref: 8.4.2.27
%% Type: single, end-to-end, server-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_expires(String) ->
    {DateTime, _} = parse_http_date(String),
    ?WH('Expires', DateTime, []).

format_expires(H) ->
    fmt_date(H#wsp_header.value).

encode_expires(H, _Version) ->
    e_date(H#wsp_header.value).

decode_expires(Value, _Version) ->
    ?WH('Expires', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% From: <text-string>
%% Ref: 8.4.2.28
%% Type: single,
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_from(Value) ->
    ?WH('From', Value, []).

format_from(H) ->
    H#wsp_header.value.

encode_from(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_from(Value, _Version) ->
    ?WH('From', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Host: <text-string>
%% Ref: 8.4.2.29
%% Type: single, end-to-end, client-to-server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_host(Value) ->
    ?WH('Host', Value, []).

format_host(H) ->
    H#wsp_header.value.

encode_host(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_host(Value, _Version) ->
    ?WH('Host', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% If-Modified-Since: <date-value>
%% Ref: 8.4.2.30
%% Type: single, end-to-end, client-to-server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_if_modified_since(String) ->
    {DateTime, _} = parse_http_date(String),
    ?WH('If-Modified-Since', DateTime, []).

format_if_modified_since(H) ->
    fmt_date(H#wsp_header.value).

encode_if_modified_since(H, _Version) ->
    e_date(H#wsp_header.value).

decode_if_modified_since(Value, _Version) ->
    ?WH('If-Modified-Since', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% If-Match: <text-string>
%% Ref: 8.4.2.31
%% Type: end-to-end, client-to-server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_if_match(Value) ->
    ?WH('If-Match', Value, []).

format_if_match(H) ->
    H#wsp_header.value.

encode_if_match(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_if_match(Value, _Version) ->
    ?WH('If-Match', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% If-None-Match: <text-string>
%% Ref: 8.4.2.32
%% Type: end-to-end, client-to-server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_if_none_match(Value) ->
    ?WH('If-None-Match', Value, []).

format_if_none_match(H) ->
    H#wsp_header.value.

encode_if_none_match(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_if_none_match(Value, _Version) ->
    ?WH('If-None-Match', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% If-Range: Text | Date
%% Ref: 8.4.2.33
%% Type: end-to-end, client-to-server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_if_range(Value) ->
    case catch parse_http_date(Value) of
	{'EXIT', _} ->
	    ?WH('If-Range', Value, []);
	{DateTime,_} ->
	    ?WH('If-Range', DateTime, [])
    end.


format_if_range(H) ->
    case H#wsp_header.value of
	Value when list(Value) -> Value;
	DateTime -> fmt_date(DateTime)
    end.

encode_if_range(H, _Version) ->
    case H#wsp_header.value of
	Value when list(Value) ->
	    encode_text_string(Value);
	DateTime ->
	    e_date(DateTime)
    end.

decode_if_range(Value, _Version) when list(Value) ->
    ?WH('If-Range', decode_text_string(Value), []);
decode_if_range(Value, _Version) ->
    ?WH('If-Range', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% If-Unmodified-Since: <date-value>
%% Ref: 8.4.2.34
%% Type: single, end-to-end, client-to-server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_if_unmodified_since(String) ->
    {DateTime, _} = parse_http_date(String),
    ?WH('If-Unmodified-Since', DateTime, []).

format_if_unmodified_since(H) ->
    fmt_date(H#wsp_header.value).

encode_if_unmodified_since(H, _Version) ->
    e_date(H#wsp_header.value).

decode_if_unmodified_since(Value, _Version) ->
    ?WH('If-Unmodified-Since', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Location: <uri-value>
%% Ref: 8.4.2.36
%% Type: single, end-to-end, server-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_location(Value) ->
    ?WH('Location', Value, []).

format_location(H) ->
    H#wsp_header.value.

encode_location(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_location(Value, _Version) when list(Value) ->
    ?WH('Location', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Last-Modified: <date-value>
%% Ref: 8.4.2.35
%% Type: single, end-to-end, server-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_last_modified(String) ->
    {DateTime, _} = parse_http_date(String),
    ?WH('Last-Modified', DateTime, []).

format_last_modified(H) ->
    fmt_date(H#wsp_header.value).

encode_last_modified(H, _Version) ->
    e_date(H#wsp_header.value).

decode_last_modified(Value, _Version) ->
    ?WH('Last-Modified', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Max-Forwards: <integer-value>
%% Ref: 8.4.2.37
%% Type: single, end-to-end, server-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_max_forwards(String) ->
    ?WH('Max-Forwards', list_to_integer(String), []).

format_max_forwards(H) ->
    integer_to_list(H#wsp_header.value).

encode_max_forwards(H, _Version) ->
    encode_integer(H#wsp_header.value).

decode_max_forwards(Value, _Version) ->
    decode_integer(Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Pragma: No-Cache | value-length Parameter
%% Ref:
%% Type:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_pragma(Value) ->
    ?WH('Pragma',Value,[]).

format_pragma(H) ->
    case H#wsp_header.value of
	"" -> format_params(H#wsp_header.params);
	Value -> Value
    end.

encode_pragma(H, Version) ->
    case H#wsp_header.value of
	"no-cache" -> ?ENCODE_SHORT(0);
	"" ->
	    encode_parameter(hd(H#wsp_header.params), Version)
    end.

decode_pragma(0, _Version) ->
    ?WH('Pragma',"no-cache",[]);
decode_pragma({_,Data}, Version) ->
    {Param,_} = decode_parameter(Data, Version),
    ?WH('Pragma',"",[Param]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Proxy-Authenticate:
%% Ref: 8.4.2.39
%% Type: single?, client-to-proxy
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_proxy_authenticate(Value) ->
    parse_challenge('Proxy-Authenticate', Value).

format_proxy_authenticate(H) ->
    format_challenge(H#wsp_header.value, H#wsp_header.params).

encode_proxy_authenticate(H, Version) ->
    encode_challenge(H#wsp_header.value,
		     H#wsp_header.params, Version).

decode_proxy_authenticate({_, Data}, Version) ->
    decode_challenge('Proxy-Authenticate', Data, Version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Proxy-authorization:
%% Ref: 8.4.2.40
%% Type: single?, proxy-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_proxy_authorization(Value) ->
    parse_credentials('Proxy-Authorization', Value).

format_proxy_authorization(H) ->
    format_credentials(H#wsp_header.value, H#wsp_header.params).

encode_proxy_authorization(H, Version) ->
    encode_credentials(H#wsp_header.value, H#wsp_header.params, Version).

decode_proxy_authorization({_,Data}, Version) ->
    decode_credentials('Proxy-Authorization', Data, Version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Public: <well-known-method> | Token-Text
%% Ref: 8.4.2.41
%% Type:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_public(Value) ->
    ?WH('Public', parse_well_known_method(Value), []).

format_public(H) ->
    if atom(H#wsp_header.value) ->
	    atom_to_list(H#wsp_header.value);
       list(H#wsp_header.value) ->
	    H#wsp_header.value
    end.

encode_public(H, Version) ->
    if atom(H#wsp_header.value) ->
	    encode_well_known_method(H#wsp_header.value,Version);
       list(H#wsp_header.value) ->
	    encode_text_string(H#wsp_header.value)
    end.

decode_public(Value, _Version) when list(Value) ->
    ?WH('Public', Value, []);
decode_public(Value, Version) ->
    ?WH('Public', decode_well_known_method(Value,Version), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Range:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_range(Value) ->
    %% FIXME:
    ?WH('Range', Value, []).

format_range(H) ->
    case H#wsp_header.value of
	{First,undefined} ->
	    ["bytes=", integer_to_list(First), "-"];
	{First,Last} ->
	    ["bytes=", integer_to_list(First), "-", integer_to_list(Last)];
	Len when integer(Len) ->
	    ["bytes=-", integer_to_list(Len)]
    end.

encode_range(H, _Version) ->
    case H#wsp_header.value of
	{First,undefined} ->
	    e_value(?ENCODE_SHORT(0),
		    e_uintvar(First));
	{First,Last} ->
	    e_value(?ENCODE_SHORT(0),
		    e_uintvar(First),
		    e_uintvar(Last));
	Len when integer(Len) ->
	    e_value(?ENCODE_SHORT(1),
		    e_uintvar(Len))
    end.

decode_range({_,Data}, _Version) ->
    case scan_header_data(Data) of
	{0, Data1} ->
	    case d_uintvar(Data1) of
		{First, <<>>} ->
		    ?WH('Range', {First, undefined},[]);
		{First, Data2} ->
		    {Last, _} = d_uintvar(Data2),
		    ?WH('Range', {First, Last}, [])
	    end;
	{1, Data1} ->
	    {Len, _} =d_uintvar(Data1),
	    ?WH('Range', Len, [])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Referer: <uri-value>
%% Ref: 8.4.2.43
%% Type: single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_referer(Value) ->
    ?WH('Referer', Value, []).

format_referer(H) ->
    H#wsp_header.value.

encode_referer(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_referer(Value, _Version) when list(Value) ->
    ?WH('Referer', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Retry-After: Value-length (Retry-date-value | Retry-delta-seconds)
%% Ref: 8.4.2.44
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_retry_after(Value) ->
    case catch parse_http_date(Value) of
	{'EXIT', _} ->
	    ?WH('Retry-After', list_to_integer(Value), []);
	{DateTime,_} ->
	    ?WH('Retry-After', DateTime, [])
    end.

format_retry_after(H) ->
    Value = H#wsp_header.value,
    if integer(Value) ->
	    integer_to_list(Value);
       true ->
	    fmt_date(Value)
    end.

encode_retry_after(H, _Version) ->
    Value = H#wsp_header.value,
    if integer(Value) ->
	    e_value(?ENCODE_SHORT(1),
		    e_delta_seconds(Value));
       true ->
	    e_value(?ENCODE_SHORT(0),
		    e_date(Value))
    end.

decode_retry_after({_,Data}, _Version) ->
    case scan_header_data(Data) of
	{0, Data1} ->
	    ?WH('Retry-After', d_date(Data1), []);
	{1, Data1} ->
	    case scan_header_data(Data1) of
		Sec when integer(Sec) ->
		    ?WH('Retry-After', Sec, []);
		{short,Data2} ->
		    ?WH('Retry-After', d_long(Data2), [])
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Server: <text-string>
%% Ref: 8.4.2.45
%% Type: server-to-client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_server(Value) ->
    ?WH('Server', Value, []).

format_server(H) ->
    H#wsp_header.value.

encode_server(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_server(Value, _Version) ->
    ?WH('Server', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Transfer-Encoding:
%% Ref: 8.4.2.46
%% Type: hop-by-hop
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_transfer_encoding(Value) ->
    ?WH('Transfer-Encoding', Value, []).

format_transfer_encoding(H) ->
    H#wsp_header.value.

encode_transfer_encoding(H, _Version) ->
    case H#wsp_header.value of
	"chunked" -> ?ENCODE_SHORT(0);
	Value -> encode_text_string(Value)
    end.

decode_transfer_encoding(0, _Version) ->
    ?WH('Transfer-Encoding', "chunked", []);
decode_transfer_encoding(Value, _Version) when list(Value)->
    ?WH('Transfer-Encoding', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Upgrade: Text-String
%% Ref: 8.4.2.47
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_upgrade(Value) ->
    ?WH('Upgrade', Value, []).

format_upgrade(H) ->
    H#wsp_header.value.

encode_upgrade(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_upgrade(Value, _Version) when list(Value) ->
    ?WH('Upgrade', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% User-Agent:
%% Ref: 8.4.2.48
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_user_agent(Value) ->
    ?WH('User-Agent', Value, []).

format_user_agent(H) ->
    H#wsp_header.value.

encode_user_agent(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_user_agent(Value, _Version) ->
    ?WH('User-Agent', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Vary: Well-known-header-field | Token-text
%% Ref: 8.4.2.49
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_vary(Value) ->
    ?WH('Vary', normalise_field_name(Value), []).

format_vary(H) ->
    to_list(H#wsp_header.value).

encode_vary(H, Version) ->
    e_field_name(H#wsp_header.value, Version).

decode_vary(Value, _Version) when integer(Value) ->
    ?WH('Vary', lookup_field_name(Value), []);
decode_vary(Value, _Version) when list(Value) ->
    ?WH('Vary', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Via: <text-string>
%% Ref: 8.4.2.50
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_via(Value) ->
    ?WH('Via', Value, []).

format_via(H) ->
    H#wsp_header.value.

encode_via(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_via(Value, _Version) when list(Value) ->
    ?WH('Via', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Warning: Warn-Code | Warning-value
%% Ref: 8.4.2.51
%% Type: general, multiple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_warning(Value) ->
    case string:tokens(Value, " ") of
	[Code] ->
	    ?WH('Warning', {list_to_integer(Code),"",""}, []);
	[Code,Agent,Text] ->
	    ?WH('Warning', {list_to_integer(Code), Agent, Text}, [])
    end.

format_warning(H) ->
    case H#wsp_header.value of
	{Code, "", ""} ->
	    integer_to_list(Code);
	{Code, Agent, Text} ->
	    [integer_to_list(Code), " ", Agent, " ", Text]
    end.

encode_warning(H, _Version) ->
    case H#wsp_header.value of
	{Code,"",""} ->
	    ?ENCODE_SHORT(Code);
	{Code, Agent, Text} ->
	    e_value(?ENCODE_SHORT(Code),
		    encode_text_string(Agent),
		    encode_text_string(Text))
    end.

decode_warning(Value, _Version) when integer(Value) ->
    ?WH('Warning', {Value, "", ""}, []);
decode_warning({_, Data}, _Version) ->
    {Code,Data1}= scan_header_data(Data),
    {Agent,Data2} = d_text_string(Data1),
    {Text,_Data3} = d_text_string(Data2),
    ?WH('Warning', {Code,Agent,Text}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WWW-Authenticate: challenge
%% Ref: 8.4.2.52
%% Type: single? client-to-server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_www_authenticate(Value) ->
    parse_challenge('Www-Authenticate', Value).

format_www_authenticate(H) ->
    format_challenge(H#wsp_header.value, H#wsp_header.params).

encode_www_authenticate(H, Version) ->
    encode_challenge(H#wsp_header.value,
		     H#wsp_header.params, Version).

decode_www_authenticate({_, Data}, Version) ->
    decode_challenge('Www-Authenticate', Data, Version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Disposition: "form-data" | "attachment" [<param>]*
%% Ref: 8.4.2.53
%% Type: single
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_content_disposition(Value) ->
    ?WH('Content-Disposition', Value, []).

format_content_disposition(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_content_disposition(H, Version) ->
    case H#wsp_header.value of
	"form-data" ->
	    e_value(?ENCODE_SHORT(0),
		    encode_params(H#wsp_header.params, Version));
	"attachment" ->
	    e_value(?ENCODE_SHORT(1),
		    encode_params(H#wsp_header.params, Version))
    end.

decode_content_disposition({_,Data}, Version) when binary(Data) ->
    case scan_header_data(Data) of
	{0, Data1} ->
	    Params = decode_params(Data1, Version),
	    ?WH('Content-Disposition', "form-data", Params);
	{1, Data1} ->
	    Params = decode_params(Data1, Version),
	    ?WH('Content-Disposition', "attachment", Params)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Application-Id:
%% Ref: 8.4.2.54
%% Type:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_application_id(Value) ->
    ?WH('X-Wap-Application-Id', Value, []).

format_x_wap_application_id(H) ->
    H#wsp_header.value.

encode_x_wap_application_id(H, _Version) ->
    encode_push_application(H#wsp_header.value).

decode_x_wap_application_id(Value, _Version) ->
    ?WH('X-Wap-Application-Id', decode_push_application(Value),[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Content-Uri: <uri-value>
%% Ref: 8.4.2.55
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_content_uri(Value) ->
    ?WH('X-Wap-Content-Uri', Value, []).

format_x_wap_content_uri(H) ->
    H#wsp_header.value.

encode_x_wap_content_uri(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_x_wap_content_uri(Value, _Version) when list(Value) ->
    ?WH('X-Wap-Content-Uri', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Initiator-Uri: <uri-value>
%% Ref: 8.4.2.56
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_initiator_uri(Value) ->
    ?WH('X-Wap-Initiator-Uri', Value, []).

format_x_wap_initiator_uri(H) ->
    H#wsp_header.value.

encode_x_wap_initiator_uri(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_x_wap_initiator_uri(Value, _Version) when list(Value) ->
    ?WH('X-Wap-Initiator-Uri', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Accept-Application: Any-Application | Appication-Id-Value
%% Ref: 8.4.2.57
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_accept_application(Value) ->
    ?WH('Accept-Application', Value, []).

format_accept_application(H) ->
    H#wsp_header.value.


encode_accept_application(H, _Version) ->
    case H#wsp_header.value of
	"*" -> ?ENCODE_SHORT(0);
	Value ->
	    case catch encode_push_application(Value) of
		{'EXIT',_} ->
		    encode_uri_value(Value);
		App ->
		    encode_integer(App)
	    end
    end.

decode_accept_application(0, _Version) ->
    ?WH('Accept-Application', "*", []);
decode_accept_application(Value, _Version) when integer(Value) ->
    ?WH('Accept-Application', decode_push_application(Value), []);
decode_accept_application({short,Data}, _Version) ->
    Value = d_long(Data),
    ?WH('Accept-Application', decode_push_application(Value), []);
decode_accept_application(Value, _Version) when list(Value) ->
    ?WH('Accept-Application', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Bearer-Indication: <integer-value>
%% Type: sinlge
%% Ref: 8.4.2.58
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_bearer_indication(Value) ->
    ?WH('Bearer-Indication', Value, []).

format_bearer_indication(H) ->
    integer_to_list(H#wsp_header.value).

encode_bearer_indication(H, _Version) ->
    encode_integer(H#wsp_header.value).

decode_bearer_indication(Value, _Version) when integer(Value) ->
    ?WH('Bearer-Indication', Value, []);
decode_bearer_indication({short,Data}, _Version) ->
    Value = d_long(Data),
    ?WH('Bearer-Indication', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Push-Flag: Short-Integer
%% Type: single
%% Ref: 8.4.2.59
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_push_flag(Value) ->
    ?WH('Push-Flag', integer_to_list(Value), []).

format_push_flag(H) ->
    integer_to_list(H#wsp_header.value).

encode_push_flag(H, _Version) ->
    ?ENCODE_SHORT(H#wsp_header.value).

decode_push_flag(Value, _Version) when integer(Value) ->
    ?WH('Push-Flag', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Profile: <uri-value>
%% Ref: 8.4.2.60
%% Type: single, hop-by-hop, client-to-proxy
%%
%% Note: Normally transfered as 'X-Wap-Profile'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_profile(Value) ->
    ?WH('Profile', Value, []).

format_profile(H) ->
    H#wsp_header.value.

encode_profile(H, _Version) ->
    encode_uri_value(H#wsp_header.value).

decode_profile(Value, _Version) ->
    ?WH('Profile', decode_uri_value(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Profile-Diff: Value-Length Octets
%% Ref: 8.4.2.61
%% Type: single, hop-by-hop, client-to-proxy
%%
%%  Value is WBXML encoded profile diff information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_profile_diff(Value) ->
    %% FIXME parse XML code?
    ?WH('Profile-Diff', Value, []).

format_profile_diff(_H) ->
    %% FIXME emit ???
    "WBXML".

encode_profile_diff(H, _Version) ->
    e_value(H#wsp_header.value).

decode_profile_diff({_,Value}, _Version) ->
    ?WH('Profile-Diff', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Profile-Warning: Code
%% Ref: 8.4.2.62
%% Type: single
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_profile_warning(Value) ->
    ?WH('Profile-Warning', {Value,"",undefined}, []).

format_profile_warning(H) ->
    {Code,Target,Date} = H#wsp_header.value,
    CodeData = integer_to_list(Code),
    if Target == "", Date == undefined ->
	    CodeData;
       Date == undefined ->
	    [CodeData," ",Target];
       true ->
	    [CodeData," ",Target," ",format_date(Date)]
    end.


encode_profile_warning(H, _Version) ->
    {Code,Target,Date} = H#wsp_header.value,
    CodeData = case Code of
		   100 -> ?ENCODE_SHORT(16#10);
		   101 -> ?ENCODE_SHORT(16#11);
		   102 -> ?ENCODE_SHORT(16#12);
		   200 -> ?ENCODE_SHORT(16#20);
		   201 -> ?ENCODE_SHORT(16#21);
		   202 -> ?ENCODE_SHORT(16#22);
		   203 -> ?ENCODE_SHORT(16#23)
	       end,
    if Target == "", Date == undefined ->
	    CodeData;
       Date == undefined ->
	    e_value(CodeData, encode_text_string(Target));
       true ->
	    e_value(CodeData, encode_text_string(Target), e_date(Date))
    end.


decode_profile_warning(Value, _Version) when integer(Value) ->
    Code = case Value of
	       16#10 -> 100;
	       16#11 -> 101;
	       16#12 -> 102;
	       16#20 -> 200;
	       16#21 -> 201;
	       16#22 -> 202;
	       16#23 -> 203
	   end,
    ?WH('Profile-Warning', {Code,"",undefined}, []);
decode_profile_warning({_, <<1:1, Value:7, Data>>}, _Version) ->
    Code = case Value of
	       16#10 -> 100;
	       16#11 -> 101;
	       16#12 -> 102;
	       16#20 -> 200;
	       16#21 -> 201;
	       16#22 -> 202;
	       16#23 -> 203
	   end,
    {Target,Data1} = d_text_string(Data),
    Date =
	if Data1 == <<>> ->
		undefined;
	   true ->
		{DateValue,_} = scan_header_data(Data1),
		d_date(DateValue)
	end,
    ?WH('Profile-Warning', {Code,Target,Date}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Expect: 100-contine | Expect-expression
%% Ref: 8.4.2.63
%% Type: client-to-server
%% Note: Bug in the spec value-length is missing !!!
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_expect(Value) ->
    ?WH('Expect', Value, []).

format_expect(H) ->
    case H#wsp_header.value of
	{Var,Val} ->
	    [Var,"=",Val, format_params(H#wsp_header.params)];
	Val when list(Val) ->
	    Val
    end.

encode_expect(H, Version) ->
    case H#wsp_header.value of
	"100-continue" ->
	    ?ENCODE_SHORT(0);
	{Var,Val} ->
	    e_value(encode_text_string(Var),
		    encode_text_string(Val),
		    encode_params(H#wsp_header.params,Version))
    end.

decode_expect(0, _Version) ->
    ?WH('Expect', "100-continue", []);
decode_expect({_, Data}, Version) ->
    {Var, Data1} = d_text_string(Data),
    {Val, Data2} = d_text_string(Data1),
    Params = decode_params(Data2, Version),
    ?WH('Expect', {decode_text_string(Var),
		   decode_text_string(Val)}, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Te: Trailers | TE-General-From
%% Ref: 8.4.2.64
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_te(Value) ->
    ?WH('Te', Value, []).

format_te(H) ->
    [H#wsp_header.value, format_params(H#wsp_header.params)].

encode_te(H, Version) ->
    case H#wsp_header.value of
	"trailers" -> ?ENCODE_SHORT(1);
	"chunked" ->
	    e_value(?ENCODE_SHORT(2),
		    encode_params(H#wsp_header.params,Version));
	"identity" ->
	    e_value(?ENCODE_SHORT(3),
		    encode_params(H#wsp_header.params,Version));
	"gzip" ->
	    e_value(?ENCODE_SHORT(4),
		    encode_params(H#wsp_header.params,Version));
	"compress" ->
	    e_value(?ENCODE_SHORT(5),
		    encode_params(H#wsp_header.params,Version));
	"deflate" ->
	    e_value(?ENCODE_SHORT(6),
		    encode_params(H#wsp_header.params,Version));
	Value ->
	    e_value(encode_text_string(Value),
		    encode_params(H#wsp_header.params,Version))
    end.

decode_te(1, _Version) ->
    ?WH('Te', "trailers", []);
decode_te({_, Data}, _Version) ->
    {Val, Data1} = scan_header_data(Data),
    Value =
	case Val of
	    2 -> "chunked";
	    3 -> "identity";
	    4 -> "gzip";
	    5 -> "compress";
	    6 -> "deflate";
	    V when list(V) -> V
	end,
    Params = case Data1 of
		 <<>> ->  [];
		 <<128, QData>> ->
		     {QValue, _} = d_q_value(QData),
		     [{q, QValue}]
	     end,
    ?WH('Te', Value, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Trailer: Well-known-header-field | Token-text
%% Ref: 8.4.2.65
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_trailer(Value) ->
    ?WH('Trailer', normalise_field_name(Value), []).

format_trailer(H) ->
    to_list(H#wsp_header.value).

encode_trailer(H, Version) ->
    e_field_name(H#wsp_header.value, Version).

decode_trailer(Value, _Version) when integer(Value) ->
    ?WH('Trailer', lookup_field_name(Value), []);
decode_trailer(Value, _Version) when list(Value) ->
    ?WH('Trailer', Value, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Tod:
%% Ref: 8.4.2.66
%% Type: hop-by-hop
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_tod(String) ->
    {DateTime, _} = parse_http_date(String),
    ?WH('X-Wap-Tod', DateTime, []).

format_x_wap_tod(H) ->
    fmt_date(H#wsp_header.value).

encode_x_wap_tod(H, _Version) ->
    e_date(H#wsp_header.value).

decode_x_wap_tod(Value, _Version) ->
    ?WH('X-Wap-Tod', d_date(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Content-Id: <quoted-string>
%% Type:
%% Ref: 8.4.2.67
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_content_id(Value) ->
    ?WH('Content-Id', Value, []).

format_content_id(H) ->
    [$", H#wsp_header.value, $"].

encode_content_id(H, _Version) ->
    encode_quoted_string(H#wsp_header.value).

decode_content_id(Value, _Version) when list(Value) ->
    ?WH('Content-Id', decode_quoted_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Set-Cookie: <len> <cookie-version> <cookie-name> <cokie-value> <parm>*
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_set_cookie(String) ->
    %% MEGA FIXME; Cookie-value may be a quoted string and
    %% contain both ,=; etc Fix several cookies on same line!!
    case string:tokens(String, ";") of
	[Cookie | Ps] ->
	    case string:tokens(Cookie, "=") of
		[Name,Value] ->
		    Params = parse_params(Ps),
		    ?WH('Set-Cookie', {{1,0}, Name, Value}, Params);
		[Name] ->
		    Params = parse_params(Ps),
		    ?WH('Set-Cookie', {{1,0}, Name, ""}, Params)
	    end;
	[] ->
	    ?WH('Set-Cookie', {{1,0}, String, ""}, [])
    end.

format_set_cookie(H) ->
    case H#wsp_header.value of
	{{1,0},Name,Value} ->
	    [Name, "=", Value,format_params(H#wsp_header.params)];
	{Version,Name,Value} ->
	    [format_version(Version)," ",
	     Name, "=", Value,
	     format_params(H#wsp_header.params)]
    end.

encode_set_cookie(H, Version) ->
    {CookieVersion,Name,Value} = H#wsp_header.value,
    e_value(encode_version(CookieVersion),
	    encode_text_string(Name),
	    encode_text_string(Value),
	    encode_params(H#wsp_header.params, Version)).

decode_set_cookie({_, Data}, Version) ->
    {CookieVersion, Data1} = scan_header_data(Data),
    {CookieName, Data2} = scan_header_data(Data1),
    {CookieValue, Data3} = scan_header_data(Data2),
    Params = decode_params(Data3, Version),
    ?WH('Set-Cookie', {decode_version(CookieVersion),
		       decode_text_string(CookieName),
		       decode_text_string(CookieValue)}, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Cookie:
%% Ref: 8.4.2.69
%% Type: single?, client-to-server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_cookie(Value) ->
    %% FIXME parse cookie version etc
    ?WH('Cookie', {{1,0},Value}, []).

format_cookie(H) ->
    case H#wsp_header.value of
	{{1,0}, Cookies} ->
	    lists:map(fun({Name,Value,Ps}) ->
			      [Name,"=",Value, format_params(Ps)]
		      end, Cookies);
	{Version, Cookies} ->
	    [format_version(Version)," ",
	     lists:map(fun({Name,Value,Ps}) ->
			       [Name,"=",Value, format_params(Ps)]
		       end, Cookies)]
    end.

encode_cookie(H, Version) ->
    {Version, Cookies} = H#wsp_header.value,
    e_value(encode_version(Version),
	    encode_cookies(Cookies, [])).

encode_cookies([{Name,Value,Ps} | Cs], Acc) ->
    List =
	[encode_text_string(Name),
	 encode_text_string(Value) |
	 case Ps of
	     [{path,P},{domain,D}] ->
		 [encode_text_string(P), encode_text_string(D)];
	     [{domain,D},{path,P}] ->
		 [encode_text_string(P), encode_text_string(D)];
	     [{path,P}] ->
		 [encode_text_string(P)];
	     [{domain,D}] ->
		 [encode_text_string(""), encode_text_string(D)];
	     [] ->
		 []
	 end],
    Sz = lists:sum(lists:map(fun(B) -> size(B) end, List)),
    encode_cookies(Cs, [[e_uintvar(Sz) | List] | Acc]);
encode_cookies([], Acc) ->
    list_to_binary(lists:reverse(Acc)).


decode_cookie({_, Data}, _Version) ->
    {CookieVersion, Data1} = scan_header_data(Data),
    Cookies = decode_cookies(Data1, []),
    ?WH('Cookie', {decode_version(CookieVersion), Cookies}, []).

decode_cookies(<<>>, Acc) ->
    lists:reverse(Acc);
decode_cookies(Data0, _Acc) ->	%% IS IGNORING Acc A BUG OR NOT ?
    {Len, Data1} = d_uintvar(Data0),
    <<C0:Len/binary, Data2/binary>> = Data1,
    {Name, C1} = scan_header_data(C0),
    {Value, C2} = scan_header_data(C1),
    {Ps1, C3} =
	case d_text_string(C2) of
	    {"", C21} -> {[], C21};
	    {Path,C21} -> {[{path,Path}], C21}
	end,
    {Ps2, _} =
	case C3 of
	    <<>> -> {[], <<>>};
	    _ ->
		{Domain,C4} = d_text_string(C3),
		{[{domain,Domain}], C4}
	end,
    decode_cookies(Data2, [{decode_text_string(Name),
			    decode_text_string(Value),
			    Ps1++Ps2}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encoding-Version: Version-Value | Value-length Code-Page [Version-Value]
%% Ref: 8.4.2.70
%% Type: single, hop-by-hop, client-and-proxys
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_encoding_version(Value) ->
    ?WH('Encoding-Version', parse_version(Value), []).

format_encoding_version(H) ->
    format_version(H#wsp_header.value).

encode_encoding_version(H, _Version) ->
    encode_version(H#wsp_header.value).

decode_encoding_version(Value, _Version) when integer(Value) ->
    ?WH('Encoding-Version', decode_version(Value), []);
decode_encoding_version(Value, _Version) when list(Value) ->
    %% Note: in this case we parse the Value since we
    %% Must know the Encoding version
    ?WH('Encoding-Version', parse_version(Value), []);
decode_encoding_version({_,<<_:1,_CodePage:7>>}, _Version) ->
    %% ??? FIXME
    ?WH('Encoding-Version', "", []);
decode_encoding_version({_,<<_:1,_CodePage:7, Data1/binary>>}, _Version) ->
    {Value,_Data2} = scan_header_data(Data1),
    %% FIXME CodePage
    ?WH('Encoding-Version', decode_version(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Security:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_security(Value) ->
    ?WH('X-Wap-Security', Value, []).

format_x_wap_security(H) ->
    H#wsp_header.value.

encode_x_wap_security(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_x_wap_security(Value, _Version) ->
    ?WH('X-Wap-Security', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Loc-Invocation:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_loc_invocation(Value) ->
    ?WH('X-Wap-Loc-Invocation', Value, []).

format_x_wap_loc_invocation(H) ->
    H#wsp_header.value.

encode_x_wap_loc_invocation(H, _Version) ->
    encode_text_string(H#wsp_header.value).

decode_x_wap_loc_invocation(Value, _Version) ->
    ?WH('X-Wap-Loc-Invocation', decode_text_string(Value), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% X-Wap-Loc-Delivery:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_x_wap_loc_delivery(Value) ->
    ?WH('X-Wap-Loc-Delivery', Value, []).

format_x_wap_loc_delivery(H) ->
    H#wsp_header.value.

encode_x_wap_loc_delivery(H, _Value) ->
    encode_text_string(H#wsp_header.value).

decode_x_wap_loc_delivery(Value, _Version) ->
    ?WH('X-Wap-Loc-Delivery', decode_text_string(Value), []).


%%
%% Header Field parameters
%%

parse_params([Param|Ps]) ->
    case string:tokens(Param, "=") of
	[Name,Value0] ->
	    Val = trim(Value0),
	    P = case trim(tolower(Name)) of
		    "q" ->{q,Val};
		    "charset" -> {charset,Val};
		    "level" -> {level,Val};
		    "type" -> {type,Val};
		    "name" -> {name,Val};
		    "filename" -> {filename,Val};
		    "differences" -> {differences,Val};
		    "padding" -> {padding,Val};
		    "start" -> {start,Val};
		    "start-info" -> {'start-info',Val};
		    "comment" -> {comment,Val};
		    "domain" -> {domain,Val};
		    "max-age" -> {'max-age',Val};
		    "path" -> {path,Val};
		    "secure" -> {secure,no_value};
		    "sec" -> {sec, Val};
		    "mac" -> {mac, Val};
		    "creation-date" -> {'creation-date', Val};
		    "modification-date" -> {'modification-date', Val};
		    "read-date" -> {'read-date', Val};
		    "size" -> {size, Val};
		    Nm -> {Nm, Val}
		end,
	    [P | parse_params(Ps)];
	_ ->
	    parse_params(Ps)
    end;
parse_params([]) ->
    [].

%% format Params without leading ";"
format_params0([{Param,no_value}|Ps]) ->
    [to_list(Param) | format_params(Ps)];
format_params0([{Param,Value}|Ps]) ->
    [to_list(Param),"=",to_list(Value) | format_params(Ps)].

format_params(Ps) ->
    lists:map(fun({Param,no_value}) ->
		      ["; ", to_list(Param)];
		 ({Param,Value})->
		      ["; ", to_list(Param),"=",to_list(Value)]
	      end, Ps).


encode_params(Params, Version) ->
    list_to_binary(encode_params1(Params,Version)).

encode_params1([Param|Ps], Version) ->
    [ encode_parameter(Param, Version) | encode_params1(Ps, Version)];
encode_params1([], _Version) ->
    [].


decode_params(Data, Version) ->
    decode_params(Data, [], Version).

decode_params(<<>>, Ps, _Version) ->
    lists:reverse(Ps);
decode_params(Data, Ps, Version) ->
    {ParamVal, Data1} = decode_parameter(Data, Version),
    decode_params(Data1, [ParamVal | Ps], Version).




encode_parameter({ParamName, ParamValue}, Ver) ->
    case ParamName of
	q when Ver >= 16#01 ->
	    <<1:1, 16#00:7,
	     (encode_typed_field(Ver,'Q-value', ParamValue))/binary>>;
	charset when Ver >= 16#01 ->
	    <<1:1, 16#01:7,
	     (encode_typed_field(Ver,'Well-known-charset',ParamValue))/binary>>;
	level when Ver >= 16#01 ->
	    <<1:1, 16#02:7,
	     (encode_typed_field(Ver,'Ver-value',ParamValue))/binary>>;

	type when Ver >= ?WSP_12 ->
	    <<1:1, 16#09:7,
	     (encode_typed_field(Ver,'Constrained-encoding',ParamValue))/binary>>;
	type when Ver >= 16#01 ->
	    <<1:1, 16#03:7,
	     (encode_typed_field(Ver,'Integer-value',ParamValue))/binary>>;

	name when Ver >= ?WSP_14 ->
	    <<1:1, 16#17:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	name when Ver >= 16#01 ->
	    <<1:1, 16#05:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;

	filename when Ver >= ?WSP_14 ->
	    <<1:1, 16#18:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	filename when Ver >= 16#01 ->
	    <<1:1, 16#06:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;

	differences when Ver >= 16#01 ->
	    <<1:1, 16#07:7,
	     (encode_typed_field(Ver,'Field-name',ParamValue))/binary>>;

	padding when Ver >= 16#01 ->
	    <<1:1, 16#08:7,
	     (encode_typed_field(Ver,'Short-integer',ParamValue))/binary>>;


	start when Ver >= ?WSP_14 ->
	    <<1:1, 16#19:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	start when Ver >= ?WSP_12 ->
	    <<1:1, 16#0A:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;


	'start-info' when Ver >= ?WSP_14 ->
	    <<1:1, 16#1A:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	'start-info' when Ver >= ?WSP_12 ->
	    <<1:1, 16#0B:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;

        comment when Ver >= ?WSP_14 ->
	    <<1:1, 16#1B:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	comment when Ver >= ?WSP_13 ->
	    <<1:1, 16#0C:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;

	domain when Ver >= ?WSP_14 ->
	    <<1:1, 16#1C:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	domain when Ver >= ?WSP_13 ->
	    <<1:1, 16#0D:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;

	'max-age' when Ver >= ?WSP_13 ->
	    <<1:1, 16#0E:7,
	     (encode_typed_field(Ver,'Delta-seconds-value',ParamValue))/binary>>;

	path when Ver >= ?WSP_14 ->
	    <<1:1, 16#1D:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	path when Ver >= ?WSP_13 ->
	    <<1:1, 16#0F:7,
	     (encode_typed_field(Ver,'Text-string',ParamValue))/binary>>;

	secure when Ver >= ?WSP_13 ->
	    <<1:1, 16#10:7,
	     (encode_typed_field(Ver,'No-value',ParamValue))/binary>>;
	%% NOTE: "sec" and "mac" are really 1.4 features but used by 1.3 client provisioning
	%"sec" when Ver >= ?WSP_14 ->
	sec when Ver >= ?WSP_13 ->
	    <<1:1, 16#11:7,
	     (encode_typed_field(Ver,'Short-integer',ParamValue))/binary>>;
	%"mac" when Ver >= ?WSP_14 ->
	mac when Ver >= ?WSP_13 ->
	    <<1:1, 16#12:7,
	     (encode_typed_field(Ver,'Text-value',ParamValue))/binary>>;
	'creation-date' when Ver >= ?WSP_14 ->
	    <<1:1, 16#13:7,
	     (encode_typed_field(Ver,'Date-value',ParamValue))/binary>>;
	'modification-date' when Ver >= ?WSP_14 ->
	    <<1:1, 16#14:7,
	     (encode_typed_field(Ver,'Date-value',ParamValue))/binary>>;
	'read-date' when Ver >= ?WSP_14 ->
	    <<1:1, 16#15:7,
	     (encode_typed_field(Ver,'Date-value',ParamValue))/binary>>;
	size when Ver >= ?WSP_14 ->
	    <<1:1, 16#16:7,
	     (encode_typed_field(Ver,'Integer-value',ParamValue))/binary>>;
	_ ->
	    <<(encode_text_string(ParamName))/binary,
	      (encode_text_string(ParamValue))/binary >>
   end.

%% decode_parameter: return {ParameterName, ParamterValue}
decode_parameter(<<1:1,Code:7,Data/binary>>, Version) ->
    case Code of
	16#00 ->
	    {Val,Data1} = decode_typed_field('Q-value', Data, Version),
	    {{ q, Val}, Data1};

	16#01 ->
	    {Val,Data1} = decode_typed_field('Well-known-charset',Data,Version),
	    {{charset, Val}, Data1};

	16#02 ->
	    {Val,Data1} = decode_typed_field('Version-value',Data,Version),
	    {{level, Val}, Data1};

	16#03 ->
	    {Val,Data1} = decode_typed_field('Integer-value', Data,Version),
	    {{type, Val}, Data1};

	16#05 ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{name, Val}, Data1};

	16#06 ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{filename, Val}, Data1};

	16#07 ->
	    {Val,Data1} = decode_typed_field('Field-name', Data,Version),
	    {{differences, Val}, Data1};

	16#08 ->
	    {Val,Data1} = decode_typed_field('Short-integer', Data,Version),
	    {{padding, Val}, Data1};

	16#09 ->
	    {Val,Data1} = decode_typed_field('Constrained-encoding', Data,Version),
	    {{type, Val}, Data1};

	16#0A ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{start, Val}, Data1};

	16#0B ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{'start-info', Val}, Data1};

	16#0C ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{comment, Val}, Data1};

	16#0D ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{domain, Val}, Data1};

	16#0E ->
	    {Val,Data1} = decode_typed_field('Delta-seconds-value', Data,Version),
	    {{'max-age', Val}, Data1};

	16#0F ->
	    {Val,Data1} = decode_typed_field('Text-string', Data,Version),
	    {{path, Val}, Data1};

	16#10 ->
	    {Val,Data1} = decode_typed_field('No-value', Data,Version),
	    {{secure, Val}, Data1};

	16#11 ->
	    {Val,Data1} = decode_typed_field('Short-integer', Data,Version),
	    {{sec, Val}, Data1};

	16#12 ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{mac, Val}, Data1};

	16#13 ->
	    {Val,Data1} = decode_typed_field('Date-value', Data,Version),
	    {{'creation-date', Val}, Data1};

	16#14 ->
	    {Val,Data1} = decode_typed_field('Date-value', Data,Version),
	    {{'modification-date', Val}, Data1};

	16#15 ->
	    {Val,Data1} = decode_typed_field('Date-value', Data,Version),
	    {{'read-date', Val}, Data1};

	16#16 ->
	    {Val,Data1} = decode_typed_field('Integer-value', Data,Version),
	    {{size, Val}, Data1};

	16#17 ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{name, Val}, Data1};

	16#18 ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{filename, Val}, Data1};

	16#19 ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{start, Val}, Data1};

	16#1A ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{'start-info', Val}, Data1};

	16#1B ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{comment, Val}, Data1};

	16#1C ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{domain, Val}, Data1};

	16#1D ->
	    {Val,Data1} = decode_typed_field('Text-value', Data,Version),
	    {{path, Val}, Data1};
	_ ->
	    exit({error, unknown_parameter})
    end;
decode_parameter(Data, _Version) ->
    %% Untyped-parameter: Token-Text Untype-value
    {ParamName,Data1} = d_text_string(Data),
    %% Untype-value: Integer-Value | Text-Value!
    {ParamValue, Data2} = decode_untyped_value(Data1),
    {{ParamName,ParamValue}, Data2}.


encode_typed_field(Ver,Type,Value) ->
    case Type of
	'Well-known-charset' ->
	    MIBenum = encode_charset(Value),
	    encode_integer(MIBenum);

	'Constrained-encoding' ->
	    encode_constrained_media(Value, Ver);

	'Text-string' ->
	    encode_text_string(Value);

	'Text-value' ->
	    encode_text_value(Value);

	'Short-integer' ->
	    ?ENCODE_SHORT(Value);

	'Date-value' ->
	    e_date(Value);

	'Delta-Seconds-value' ->
	    e_delta_seconds(Value);

	'No-value' ->
	    e_no_value(Value);

	_ ->
	    io:format("FIXME: encode_typed_field unsupported type = ~p\n",
		      [Type]),
	    exit({error,badtype})
    end.


decode_typed_field(Type, Data, Version) ->
    case Type of
	'Q-value' ->
	    d_q_value(Data);

	'Well-known-charset' ->
	    {MIBenum, T100} = d_integer_value(Data),
	    {decode_charset(MIBenum), T100};

	'Constrained-encoding' ->
	    {Value, Data1} = scan_header_data(Data),
	    {decode_constrained_media(Value,Version), Data1};

	'Text-string' ->
	    d_text_string(Data);

	'Text-value'  ->
	    d_text_value(Data);

	'Short-integer' ->
	    decode_short_integer(Data);

	'Delta-seconds-value' ->
	    d_integer_value(Data);

	'Date-value' ->
	    {Val, Data1} = decode_long_integer(Data),
	    {d_date(Val), Data1};

	'Field-name' ->
	    d_field_name(Data);

	'No-value' ->
	    d_no_value(Data);

	_ ->
	    io:format("FIXME: unsupported type = ~p\n",[Type]),
	    exit({error,badtype})
    end.


%% Integer-Value | Text-Value
%% return as {Value, Tail}
decode_untyped_value(<<1:1, Short:7, Tail/binary>>) ->
    {Short, Tail};
decode_untyped_value(<<0:3, Len:5, Data/binary>>) when Len =/= 31 ->
    Sz = Len*8,
    <<Long:Sz, Tail/binary>> = Data,
    {Long, Tail};
decode_untyped_value(Data) ->
    d_text_string(Data).


e_field_name(Value, Version) ->
    case normalise_field_name(Value) of
	'Accept' -> <<16#80>>;
	'Accept-Charset' when Version >= ?WSP_13 -> <<16#bb>>;
	'Accept-Charset' -> <<16#81>>;
	'Accept-Encoding' when Version >= ?WSP_13 -> <<16#bc>>;
	'Accept-Encoding' -> <<16#82>>;
	'Accept-Language' -> <<16#83>>;
	'Accept-Ranges' -> <<16#84>>;
	'Age' -> <<16#85>>;
	'Allow' -> <<16#86>>;
	'Authorization' -> <<16#87>>;
	'Cache-Control' when Version >= ?WSP_14 -> <<16#c7>>;
	'Cache-Control' when Version >= ?WSP_13 -> <<16#bd>>;
	'Cache-Control' -> <<16#88>>;
	'Connection' -> <<16#89>>;
	'Content-Base' -> <<16#8a>>;
	'Content-Encoding' -> <<16#8b>>;
	'Content-Language' -> <<16#8c>>;
	'Content-Length' -> <<16#8d>>;
	'Content-Location' -> <<16#8e>>;
	'Content-Md5' -> <<16#8f>>;
	'Content-Range' when Version >= ?WSP_13 -> <<16#be>>;
	'Content-Range' -> <<16#90>>;
	'Content-Type' -> <<16#91>>;
	'Date' -> <<16#92>>;
	'Etag' -> <<16#93>>;
	'Expires' -> <<16#94>>;
	'From' -> <<16#95>>;
	'Host' -> <<16#96>>;
	'If-Modified-Since' -> <<16#97>>;
	'If-Match' -> <<16#98>>;
	'If-None-Match' -> <<16#99>>;
	'If-Range' -> <<16#9a>>;
	'If-Unmodified-Since' -> <<16#9b>>;
	'Location' -> <<16#9c>>;
	'Last-Modified' -> <<16#9d>>;
	'Max-Forwards' -> <<16#9e>>;
	'Pragma' -> <<16#9f>>;
	'Proxy-Authenticate' -> <<16#a0>>;
	'Proxy-Authorization' -> <<16#a1>>;
	'Public' -> <<16#a2>>;
	'Range' -> <<16#a3>>;
	'Referer' -> <<16#a4>>;
	'Retry-After' -> <<16#a5>>;
	'Server' -> <<16#a6>>;
	'Transfer-Encoding' -> <<16#a7>>;
	'Upgrade' -> <<16#a8>>;
	'User-Agent' -> <<16#a9>>;
	'Vary' -> <<16#aa>>;
	'Via' -> <<16#ab>>;
	'Warning' -> <<16#ac>>;
	'Www-Authenticate' -> <<16#ad>>;
	'Content-Disposition' when Version >= ?WSP_14 -> <<16#c5>>;
	'Content-Disposition' -> <<16#ae>>;
	%% VERSION > 1.1
	'X-Wap-Application-Id' when Version >= ?WSP_12 -> <<16#af>>;
	'X-Wap-Content-Uri' when Version >= ?WSP_12 -> <<16#b0>>;
	'X-Wap-Initiator-Uri' when Version >= ?WSP_12 -> <<16#b1>>;
	'Accept-Application' when Version >= ?WSP_12 -> <<16#b2>>;
	'Bearer-Indication' when Version >= ?WSP_12 -> <<16#b3>>;
	'Push-Flag' when Version >= ?WSP_12 -> <<16#b4>>;
	'Profile' when Version >= ?WSP_12 -> <<16#b5>>;
	'Profile-Diff' when Version >= ?WSP_12 -> <<16#b6>>;
	'Profile-Warning' when Version >= ?WSP_12 -> <<16#b7>>;
	'Expect' when Version >= ?WSP_15 -> <<16#c8>>;
	'Expect' when Version >= ?WSP_13 -> <<16#b8>>;
	'Te' when Version >= ?WSP_13 -> <<16#b9>>;
	'Trailer' when Version >= ?WSP_13 -> <<16#ba>>;
	'X-Wap-Tod' when Version >= ?WSP_13 -> <<16#bf>>;
	'Content-Id' when Version >= ?WSP_13 -> <<16#c0>>;
	'Set-Cookie' when Version >= ?WSP_13 -> <<16#c1>>;
	'Cookie' when Version >= ?WSP_13 -> <<16#c2>>;
	'Encoding-Version' when Version >= ?WSP_13 -> <<16#c3>>;
	'Profile-Warning' when Version >= ?WSP_14 -> <<16#c4>>;
	'X-Wap-Security' when Version >= ?WSP_14 -> <<16#c6>>;
	'X-Wap-Loc-Invocation' when Version >= ?WSP_15 -> <<16#c9>>;
	'X-Wap-Loc-Delivery' when Version >= ?WSP_15 -> <<16#ca>>;
	Field -> encode_text_string(atom_to_list(Field))
    end.


%%
%% decode and normalise on form list_to_atom("Ulll-Ulll-Ull")
%%
normalise_field_name(Cs) when atom(Cs) ->
    Cs;
normalise_field_name(Cs) ->
    list_to_atom(normalise_fieldU(Cs)).

normalise_fieldU([C|Cs]) when C >= $a, C =< $z ->
    [(C-$a)+$A | normalise_fieldL(Cs)];
normalise_fieldU([C|Cs]) -> [ C | normalise_fieldL(Cs)];
normalise_fieldU([]) -> [].

normalise_fieldL([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | normalise_fieldL(Cs)];
normalise_fieldL([$-|Cs]) ->  [$- | normalise_fieldU(Cs)];
normalise_fieldL([C|Cs]) ->   [C | normalise_fieldL(Cs)];
normalise_fieldL([]) ->  [].


tolower([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs]) ->  [C|tolower(Cs)];
tolower([]) -> [].

trim(Cs) ->
    lists:reverse(trim1(lists:reverse(trim1(Cs)))).

trim1([$\s|Cs]) -> trim1(Cs);
trim1([$\t|Cs]) -> trim1(Cs);
trim1([$\r|Cs]) -> trim1(Cs);
trim1([$\n|Cs]) -> trim1(Cs);
trim1(Cs) -> Cs.


d_field_name(Data) ->
    case scan_header_data(Data) of
	{Code, Data1} when integer(Code) ->
	    {lookup_field_name(Code), Data1};
	{TmpField,Data1} when list(TmpField) ->
	    {normalise_field_name(TmpField), Data1}
    end.

d_no_value(<<0, Data/binary>>) ->
    {no_value, Data}.

e_no_value(_) ->
    <<0>>.


lookup_field_name(Code) ->
    case Code of
%%% Version 1.1
	16#00 -> 'Accept';
	16#01 -> 'Accept-Charset';
	16#02 -> 'Accept-Encoding';
	16#03 -> 'Accept-Language';
	16#04 -> 'Accept-Ranges';
	16#05 -> 'Age';
	16#06 -> 'Allow';
	16#07 -> 'Authorization';
	16#08 -> 'Cache-Control';
	16#09 -> 'Connection';
	16#0a -> 'Content-Base';
	16#0b -> 'Content-Encoding';
	16#0c -> 'Content-Language';
	16#0d -> 'Content-Length';
	16#0e -> 'Content-Location';
	16#0f -> 'Content-Md5';
	16#10 -> 'Content-Range';
	16#11 -> 'Content-Type';
	16#12 -> 'Date';
	16#13 -> 'Etag';
	16#14 -> 'Expires';
	16#15 -> 'From';
	16#16 -> 'Host';
	16#17 -> 'If-Modified-Since';
	16#18 -> 'If-Match';
	16#19 -> 'If-None-Match';
	16#1a -> 'If-Range';
	16#1b -> 'If-Unmodified-Since';
	16#1c -> 'Location';
	16#1d -> 'Last-Modified';
	16#1e -> 'Max-Forwards';
	16#1f -> 'Pragma';
	16#20 -> 'Proxy-Authenticate';
	16#21 -> 'Proxy-Authorization';
	16#22 -> 'Public';
	16#23 -> 'Range';
	16#24 -> 'Referer';
	16#25 -> 'Retry-After';
	16#26 -> 'Server';
	16#27 -> 'Transfer-Encoding';
	16#28 -> 'Upgrade';
	16#29 -> 'User-Agent';
	16#2a -> 'Vary';
	16#2b -> 'Via';
	16#2c -> 'Warning';
	16#2d -> 'Www-Authenticate';
	16#2e -> 'Content-Disposition';
%%% Version 1.2
	16#2f -> 'X-Wap-Application-Id';
	16#30 -> 'X-Wap-Content-Uri';
	16#31 -> 'X-Wap-Initiator-Uri';
	16#32 -> 'Accept-Application';
	16#33 -> 'Bearer-Indication';
	16#34 -> 'Push-Flag';
	16#35 -> 'Profile';
	16#36 -> 'Profile-Diff';
	16#37 -> 'Profile-Warning';
%%% Version 1.3
	16#38 -> 'Expect';
	16#39 -> 'Te';
	16#3a -> 'Trailer';
	16#3b -> 'Accept-Charset';
	16#3c -> 'Accept-Encoding';
	16#3d -> 'Cache-Control';
	16#3e -> 'Content-Range';
	16#3f -> 'X-Wap-Tod';
	16#40 -> 'Content-Id';
	16#41 -> 'Set-Cookie';
	16#42 -> 'Cookie';
	16#43 -> 'Encoding-Version';
%%% Version 1.4
	16#44 -> 'Profile-Warning';
	16#45 -> 'Content-Disposition';
	16#46 -> 'X-Wap-Security';
	16#47 -> 'Cache-Control';
%%% Version 1.5
	16#48 -> 'Expect';
	16#49 -> 'X-Wap-Loc-Invocation';
	16#4a -> 'X-Wap-Loc-Delivery';
%% Unknown
	_ ->
	    list_to_atom("X-Unknown-"++erlang:integer_to_list(Code, 16))
    end.


encode_charset(Charset) ->
    %% FIXME: we should really resolve aliases as well
    %% charset:from_aliases(Charset)
    case charset:from_mime_name(Charset) of
	0 -> exit({error, unknown_charset});
	MIBenum	-> MIBenum
    end.

encode_language(Language) ->
    Code = encode_lang(tolower(Language)),
    <<Code>>.



decode_charset(MIBenum) ->
    case charset:to_mime_name(MIBenum) of
	undefined ->
	    exit({error, unknown_charset});
	Preferred ->
	    Preferred
    end.

%% ISO 639 Language Assignments, Appendix A, Table 41, Page 102-103
decode_lang(Code) ->
    case lookup_language(Code) of
	[L|_] -> atom_to_list(L);
	[] -> ""
    end.


lookup_language(Code) ->
    case Code of
	 16#01 -> ['aa','afar'];
	 16#02 -> ['ab','abkhazian'];
	 16#03 -> ['af','afrikans'];
	 16#04 -> ['am','amharic'];
	 16#05 -> ['ar','arabic'];
	 16#06 -> ['as','assamese'];
	 16#07 -> ['ay','aymara'];
	 16#08 -> ['az','azerbaijani'];
	 16#09 -> ['ba','bashkir'];
	 16#0a -> ['be','byelorussian'];
	 16#0b -> ['bg','bulgarian'];
	 16#0c -> ['bh','bihari'];
	 16#0d -> ['bi','bislama'];
	 16#0e -> ['bn','bangla','bengali'];
	 16#0f -> ['bo','tibetan'];
	 16#10 -> ['br','breton'];
	 16#11 -> ['ca','catalan'];
	 16#12 -> ['co','corsican'];
	 16#13 -> ['cs','czech'];
	 16#14 -> ['cy','welsh'];
	 16#15 -> ['da','danish'];
	 16#16 -> ['de','german'];
	 16#17 -> ['dz','bhutani'];
	 16#18 -> ['el','greek'];
	 16#19 -> ['en','english'];
	 16#1a -> ['eo','esperanto'];
	 16#1b -> ['es','spanish'];
	 16#1c -> ['et','estonian'];
	 16#1d -> ['eu','basque'];
	 16#1e -> ['fa','persian'];
	 16#1f -> ['fi','finnish'];
	 16#20 -> ['fj','fiji'];
	 16#82 -> ['fo','faeroese'];
	 16#22 -> ['fr','french'];
	 16#83 -> ['fy','frisian'];
	 16#24 -> ['ga','irish'];
	 16#25 -> ['gd','scots-gaelic'];
	 16#26 -> ['gl','galician'];
	 16#27 -> ['gn','guarani'];
	 16#28 -> ['gu','gujarati'];
	 16#29 -> ['ha','hausa'];
	 16#2a -> ['he','hebrew'];
	 16#2b -> ['hi','hindi'];
	 16#2c -> ['hr','croatian'];
	 16#2d -> ['hu','hungarian'];
	 16#2e -> ['hy','armenian'];
	 16#84 -> ['ia','interlingua'];
	 16#30 -> ['id','indonesian'];
	 16#86 -> ['ie','interlingue'];
	 16#87 -> ['ik','inupiak'];
	 16#33 -> ['is','icelandic'];
	 16#34 -> ['it','italian'];
	 16#89 -> ['iu','inuktitut'];
	 16#36 -> ['ja','japanese'];
	 16#37 -> ['jw','javanese'];
	 16#38 -> ['ka','georgian'];
	 16#39 -> ['kk','kazakh'];
	 16#8a -> ['kl','greenlandic'];
	 16#3b -> ['km','cambodian'];
	 16#3c -> ['kn','kannada'];
	 16#3d -> ['ko','korean'];
	 16#3e -> ['ks','kashmiri'];
	 16#3f -> ['ku','kurdish'];
	 16#40 -> ['ky','kirghiz'];
	 16#8b -> ['la','latin'];
	 16#42 -> ['ln','lingala'];
	 16#43 -> ['lo','laothian'];
	 16#44 -> ['lt','lithuanian'];
	 16#45 -> ['lv','lettish','latvian'];
	 16#46 -> ['mg','malagese'];
	 16#47 -> ['mi','maori'];
	 16#48 -> ['mk','macedonian'];
	 16#49 -> ['ml','malayalam'];
	 16#4a -> ['mn','mongolian'];
	 16#4b -> ['mo','moldavian'];
	 16#4c -> ['mr','marathi'];
	 16#4d -> ['ms','malay'];
	 16#4e -> ['mt','maltese'];
	 16#4f -> ['my','burmese'];
	 16#81 -> ['na','nauru'];
	 16#51 -> ['ne','nepali'];
	 16#52 -> ['nl','dutch'];
	 16#53 -> ['no','norwegian'];
	 16#54 -> ['oc','occitan'];
	 16#55 -> ['om','oromo'];
	 16#56 -> ['or','oriya'];
	 16#57 -> ['pa','punjabi'];
	 16#58 -> ['po','polish'];
	 16#59 -> ['ps','pushto','pashto'];
	 16#5a -> ['pt','portugese'];
	 16#5b -> ['qu','quechua'];
	 16#8c -> ['rm','rhaeto-romance'];
	 16#5d -> ['rn','kirundi'];
	 16#5e -> ['ro','romanian'];
	 16#5f -> ['ru','russian'];
	 16#60 -> ['rw','kinyarwanda'];
	 16#61 -> ['sa','sanskrit'];
	 16#62 -> ['sd','sindhi'];
	 16#63 -> ['sg','sangho'];
	 16#64 -> ['sh','serbo-croatian'];
	 16#65 -> ['si','sinhalese'];
	 16#66 -> ['sk','slovak'];
	 16#67 -> ['sl','slovenian'];
	 16#68 -> ['sm','samoan'];
	 16#69 -> ['sn','shona'];
	 16#6a -> ['so','somali'];
	 16#6b -> ['sq','albanian'];
	 16#6c -> ['sr','serbian'];
	 16#6d -> ['ss','siswati'];
	 16#6e -> ['st','seshoto'];
	 16#6f -> ['su','sundanese'];
	 16#70 -> ['sv','swedish'];
	 16#71 -> ['sw','swahili'];
	 16#72 -> ['ta','tamil'];
	 16#73 -> ['te','telugu'];
	 16#74 -> ['tg','tajik'];
	 16#75 -> ['th','thai'];
	 16#76 -> ['ti','tigrinya'];
	 16#77 -> ['tk','turkmen'];
	 16#78 -> ['tl','tagalog'];
	 16#79 -> ['tn','setswana'];
	 16#7a -> ['to','tonga'];
	 16#7b -> ['tr','turkish'];
	 16#7c -> ['ts','tsonga'];
	 16#7d -> ['tt','tatar'];
	 16#7e -> ['tw','twi'];
	 16#7f -> ['ug','uighur'];
	 16#50 -> ['uk','ukrainian'];
	 16#21 -> ['ur','urdu'];
	 16#23 -> ['uz','uzbek'];
	 16#2f -> ['vi','vietnamese'];
	 16#85 -> ['vo','volapuk'];
	 16#31 -> ['wo','wolof'];
	 16#32 -> ['xh','xhosa'];
	 16#88 -> ['yi','yiddish'];
	 16#35 -> ['yo','yoruba'];
	 16#3a -> ['za','zhuang'];
	 16#41 -> ['zh','chinese'];
	16#5c -> ['zu','zulu'];
	_ -> []
    end.

encode_lang(Language) ->
    case tolower(Language) of
	"aa" -> 16#01;
	"afar" -> 16#01;
	"ab" -> 16#02;
	"abkhazian" -> 16#02;
	"af" -> 16#03;
	"afrikans" -> 16#03;
	"am" -> 16#04;
	"amharic" -> 16#04;
	"ar" -> 16#05;
	"arabic" -> 16#05;
	"as" -> 16#06;
	"assamese" -> 16#06;
	"ay" -> 16#07;
	"aymara" -> 16#07;
	"az" -> 16#08;
	"azerbaijani" -> 16#08;
	"ba" -> 16#09;
	"bashkir" -> 16#09;
	"be" -> 16#0a;
	"byelorussian" -> 16#0a;
	"bg" -> 16#0b;
	"bulgarian" -> 16#0b;
	"bh" -> 16#0c;
	"bihari" -> 16#0c;
	"bi" -> 16#0d;
	"bislama" -> 16#0d;
	"bn" -> 16#0e;
	"bangla" -> 16#0e;
	"bengali" -> 16#0e;
	"bo" -> 16#0f;
	"tibetan" -> 16#0f;
	"br" -> 16#10;
	"breton" -> 16#10;
	"ca" -> 16#11;
	"catalan" -> 16#11;
	"co" -> 16#12;
	"corsican" -> 16#12;
	"cs" -> 16#13;
	"czech" -> 16#13;
	"cy" -> 16#14;
	"welsh" -> 16#14;
	"da" -> 16#15;
	"danish" -> 16#15;
	"de" -> 16#16;
	"german" -> 16#16;
	"dz" -> 16#17;
	"bhutani" -> 16#17;
	"el" -> 16#18;
	"greek" -> 16#18;
	"en" -> 16#19;
	"english" -> 16#19;
	"eo" -> 16#1a;
	"esperanto" -> 16#1a;
	"es" -> 16#1b;
	"spanish" -> 16#1b;
	"et" -> 16#1c;
	"estonian" -> 16#1c;
	"eu" -> 16#1d;
	"basque" -> 16#1d;
	"fa" -> 16#1e;
	"persian" -> 16#1e;
	"fi" -> 16#1f;
	"finnish" -> 16#1f;
	"fj" -> 16#20;
	"fiji" -> 16#20;
	"fo" -> 16#82;
	"faeroese" -> 16#82;
	"fr" -> 16#22;
	"french" -> 16#22;
	"fy" -> 16#83;
	"frisian" -> 16#83;
	"ga" -> 16#24;
	"irish" -> 16#24;
	"gd" -> 16#25;
	"scots-gaelic" -> 16#25;
	"gl" -> 16#26;
	"galician" -> 16#26;
	"gn" -> 16#27;
	"guarani" -> 16#27;
	"gu" -> 16#28;
	"gujarati" -> 16#28;
	"ha" -> 16#29;
	"hausa" -> 16#29;
	"he" -> 16#2a;
	"hebrew" -> 16#2a;
	"hi" -> 16#2b;
	"hindi" -> 16#2b;
	"hr" -> 16#2c;
	"croatian" -> 16#2c;
	"hu" -> 16#2d;
	"hungarian" -> 16#2d;
	"hy" -> 16#2e;
	"armenian" -> 16#2e;
	"ia" -> 16#84;
	"interlingua" -> 16#84;
	"id" -> 16#30;
	"indonesian" -> 16#30;
	"ie" -> 16#86;
	"interlingue" -> 16#86;
	"ik" -> 16#87;
	"inupiak" -> 16#87;
	"is" -> 16#33;
	"icelandic" -> 16#33;
	"it" -> 16#34;
	"italian" -> 16#34;
	"iu" -> 16#89;
	"inuktitut" -> 16#89;
	"ja" -> 16#36;
	"japanese" -> 16#36;
	"jw" -> 16#37;
	"javanese" -> 16#37;
	"ka" -> 16#38;
	"georgian" -> 16#38;
	"kk" -> 16#39;
	"kazakh" -> 16#39;
	"kl" -> 16#8a;
	"greenlandic" -> 16#8a;
	"km" -> 16#3b;
	"cambodian" -> 16#3b;
	"kn" -> 16#3c;
	"kannada" -> 16#3c;
	"ko" -> 16#3d;
	"korean" -> 16#3d;
	"ks" -> 16#3e;
	"kashmiri" -> 16#3e;
	"ku" -> 16#3f;
	"kurdish" -> 16#3f;
	"ky" -> 16#40;
	"kirghiz" -> 16#40;
	"la" -> 16#8b;
	"latin" -> 16#8b;
	"ln" -> 16#42;
	"lingala" -> 16#42;
	"lo" -> 16#43;
	"laothian" -> 16#43;
	"lt" -> 16#44;
	"lithuanian" -> 16#44;
	"lv" -> 16#45;
	"lettish" -> 16#45;
	"latvian" -> 16#45;
	"mg" -> 16#46;
	"malagese" -> 16#46;
	"mi" -> 16#47;
	"maori" -> 16#47;
	"mk" -> 16#48;
	"macedonian" -> 16#48;
	"ml" -> 16#49;
	"malayalam" -> 16#49;
	"mn" -> 16#4a;
	"mongolian" -> 16#4a;
	"mo" -> 16#4b;
	"moldavian" -> 16#4b;
	"mr" -> 16#4c;
	"marathi" -> 16#4c;
	"ms" -> 16#4d;
	"malay" -> 16#4d;
	"mt" -> 16#4e;
	"maltese" -> 16#4e;
	"my" -> 16#4f;
	"burmese" -> 16#4f;
	"na" -> 16#81;
	"nauru" -> 16#81;
	"ne" -> 16#51;
	"nepali" -> 16#51;
	"nl" -> 16#52;
	"dutch" -> 16#52;
	"no" -> 16#53;
	"norwegian" -> 16#53;
	"oc" -> 16#54;
	"occitan" -> 16#54;
	"om" -> 16#55;
	"oromo" -> 16#55;
	"or" -> 16#56;
	"oriya" -> 16#56;
	"pa" -> 16#57;
	"punjabi" -> 16#57;
	"po" -> 16#58;
	"polish" -> 16#58;
	"ps" -> 16#59;
	"pushto" -> 16#59;
	"pt" -> 16#5a;
	"portugese" -> 16#5a;
	"qu" -> 16#5b;
	"quechua" -> 16#5b;
	"rm" -> 16#8c;
	"rhaeto-romance" -> 16#8c;
	"rn" -> 16#5d;
	"kirundi" -> 16#5d;
	"ro" -> 16#5e;
	"romanian" -> 16#5e;
	"ru" -> 16#5f;
	"russian" -> 16#5f;
	"rw" -> 16#60;
	"kinyarwanda" -> 16#60;
	"sa" -> 16#61;
	"sanskrit" -> 16#61;
	"sd" -> 16#62;
	"sindhi" -> 16#62;
	"sg" -> 16#63;
	"sangho" -> 16#63;
	"sh" -> 16#64;
	"serbo-croatian" -> 16#64;
	"si" -> 16#65;
	"sinhalese" -> 16#65;
	"sk" -> 16#66;
	"slovak" -> 16#66;
	"sl" -> 16#67;
	"slovenian" -> 16#67;
	"sm" -> 16#68;
	"samoan" -> 16#68;
	"sn" -> 16#69;
	"shona" -> 16#69;
	"so" -> 16#6a;
	"somali" -> 16#6a;
	"sq" -> 16#6b;
	"albanian" -> 16#6b;
	"sr" -> 16#6c;
	"serbian" -> 16#6c;
	"ss" -> 16#6d;
	"siswati" -> 16#6d;
	"st" -> 16#6e;
	"seshoto" -> 16#6e;
	"su" -> 16#6f;
	"sundanese" -> 16#6f;
	"sv" -> 16#70;
	"swedish" -> 16#70;
	"sw" -> 16#71;
	"swahili" -> 16#71;
	"ta" -> 16#72;
	"tamil" -> 16#72;
	"te" -> 16#73;
	"telugu" -> 16#73;
	"tg" -> 16#74;
	"tajik" -> 16#74;
	"th" -> 16#75;
	"thai" -> 16#75;
	"ti" -> 16#76;
	"tigrinya" -> 16#76;
	"tk" -> 16#77;
	"turkmen" -> 16#77;
	"tl" -> 16#78;
	"tagalog" -> 16#78;
	"tn" -> 16#79;
	"setswana" -> 16#79;
	"to" -> 16#7a;
	"tonga" -> 16#7a;
	"tr" -> 16#7b;
	"turkish" -> 16#7b;
	"ts" -> 16#7c;
	"tsonga" -> 16#7c;
	"tt" -> 16#7d;
	"tatar" -> 16#7d;
	"tw" -> 16#7e;
	"twi" -> 16#7e;
	"ug" -> 16#7f;
	"uighur" -> 16#7f;
	"uk" -> 16#50;
	"ukrainian" -> 16#50;
	"ur" -> 16#21;
	"urdu" -> 16#21;
	"uz" -> 16#23;
	"uzbek" -> 16#23;
	"vi" -> 16#2f;
	"vietnamese" -> 16#2f;
	"vo" -> 16#85;
	"volapuk" -> 16#85;
	"wo" -> 16#31;
	"wolof" -> 16#31;
	"xh" -> 16#32;
	"xhosa" -> 16#32;
	"yi" -> 16#88;
	"yiddish" -> 16#88;
	"yo" -> 16#35;
	"yoruba" -> 16#35;
	"za" -> 16#3a;
	"zhuang" -> 16#3a;
	"zh" -> 16#41;
	"chinese" -> 16#41;
	"zu" -> 16#5c;
	"zulu" -> 16#5c
    end.


%% Push Application ID Assignments
%%
%% Assingment are found at http://www.wapforum.org/wina/push-app-id.htm
%%
decode_push_application({short,Data}) ->
    decode_push_application(d_long(Data));

decode_push_application(Code) when integer(Code) ->
    case Code of
	16#00 ->    "x-wap-application:*";
	16#01 ->    "x-wap-application:push.sia";
	16#02 ->    "x-wap-application:wml.ua";
	16#03 ->    "x-wap-application:wta.ua";
	16#04 ->    "x-wap-application:mms.ua";
	16#05 ->    "x-wap-application:push.syncml";
	16#06 ->    "x-wap-application:loc.ua";
	16#07 ->    "x-wap-application:syncml.dm";
	16#08 ->    "x-wap-application:drm.ua";
	16#09 ->    "x-wap-application:emn.ua";
	16#0A ->    "x-wap-application:wv.ua";
	16#8000 ->  "x-wap-microsoft:localcontent.ua";
	16#8001 ->  "x-wap-microsoft:IMclient.ua";
	16#8002 ->  "x-wap-docomo:imode.mail.ua";
	16#8003 ->  "x-wap-docomo:imode.mr.ua";
	16#8004 ->  "x-wap-docomo:imode.mf.ua";
	16#8005 ->  "x-motorola:location.ua";
	16#8006 ->  "x-motorola:now.ua";
	16#8007 ->  "x-motorola:otaprov.ua";
	16#8008 ->  "x-motorola:browser.ua";
	16#8009 ->  "x-motorola:splash.ua";
	16#800B ->  "x-wap-nai:mvsw.command";
	16#8010 ->  "x-wap-openwave:iota.ua"
    end;
decode_push_application(App) when list(App) ->
    App.



encode_push_application(App) ->
    case App of
	"x-wap-application:*" -> ?ENCODE_SHORT(16#00);
	"x-wap-application:push.sia" -> ?ENCODE_SHORT(16#01);
	"x-wap-application:wml.ua" -> ?ENCODE_SHORT(16#02);
	"x-wap-application:wta.ua" -> ?ENCODE_SHORT(16#03);
	"x-wap-application:mms.ua" -> ?ENCODE_SHORT(16#04);
	"x-wap-application:push.syncml" -> ?ENCODE_SHORT(16#05);
	"x-wap-application:loc.ua" -> ?ENCODE_SHORT(16#06);
	"x-wap-application:syncml.dm" -> ?ENCODE_SHORT(16#07);
	"x-wap-application:drm.ua" -> ?ENCODE_SHORT(16#08);
	"x-wap-application:emn.ua" -> ?ENCODE_SHORT(16#09);
	"x-wap-application:wv.ua" -> ?ENCODE_SHORT(16#0A);
	"x-wap-microsoft:localcontent.ua" -> encode_integer(16#8000);
	"x-wap-microsoft:IMclient.ua" -> encode_integer(16#8001);
	"x-wap-docomo:imode.mail.ua" -> encode_integer(16#8002);
	"x-wap-docomo:imode.mr.ua" -> encode_integer(16#8003);
	"x-wap-docomo:imode.mf.ua" -> encode_integer(16#8004);
	"x-motorola:location.ua" -> encode_integer(16#8005);
	"x-motorola:now.ua" -> encode_integer(16#8006);
	"x-motorola:otaprov.ua" -> encode_integer(16#8007);
	"x-motorola:browser.ua" -> encode_integer(16#8008);
	"x-motorola:splash.ua" -> encode_integer(16#8009);
	"x-wap-nai:mvsw.command" -> encode_integer(16#800B);
	"x-wap-openwave:iota.ua" -> encode_integer(16#8010);
	_ -> encode_uri_value(App)
    end.




%% WSP 8.5 Multipart handling

encode_multipart(Entries) ->
    encode_multipart(Entries, ?WSP_DEFAULT_VERSION).

encode_multipart([], _Version) ->
    <<>>;
encode_multipart(Entries, Version) ->
    EncEntries = encode_multipart_entries(Entries, Version),
    <<(e_uintvar(length(Entries)))/binary, EncEntries/binary >>.

encode_multipart_entries(Entries, Version) ->
    encode_multipart_entries(Entries, Version, []).

encode_multipart_entries([], _Version, Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_multipart_entries([Entry|T], Version, Acc) ->
    EncEntry = encode_multipart_entry(Entry, Version),
    encode_multipart_entries(T, Version, [EncEntry | Acc]).

encode_multipart_entry(Entry, Version) ->
    #wsp_multipart_entry { content_type = ContentType,
			   headers = Headers,
			   data = Data } = Entry,
    EncContentType = encode_content_type(ContentType,Version),
    EncHeaders = encode_headers(Headers, Version),
    EncHeadersLength = e_uintvar(size(EncContentType)+size(EncHeaders)),
    DataLen = e_uintvar(size(Data)),
    <<EncHeadersLength/binary,
     DataLen/binary,
     EncContentType/binary,
     EncHeaders/binary,
     Data/binary>>.


decode_multipart(Data) ->
    decode_multipart(Data, ?WSP_DEFAULT_VERSION).

decode_multipart(<<>>, _Version) ->
    {[], <<>>};
decode_multipart(Data, Version) ->
    {Entries, Data1} = d_uintvar(Data),
    decode_multipart_entries(Entries, Data1, Version).

decode_multipart_entries(Entries, Data, Version) ->
    decode_multipart_entries(Entries, Data, Version, []).

decode_multipart_entries(0, Data, _Version, Acc) ->
    {lists:reverse(Acc), Data};
decode_multipart_entries(Entries, Data, Version, Acc) ->
    {MultiPartEntry, Data1} = decode_multipart_entry(Data,Version),
    decode_multipart_entries(Entries-1, Data1, Version, [MultiPartEntry|Acc]).

decode_multipart_entry(Data, Version) ->
    {HeadersLen, Data1} = d_uintvar(Data),
    {DataLen, Data2} = d_uintvar(Data1),
    {FieldData,Data3} = scan_header_data(Data2),
    ContentType = decode_content_type(FieldData, Version),
    BinHeadersLen = (HeadersLen-(size(Data2)-size(Data3))),
    <<BinHeaders:BinHeadersLen/binary,Data4/binary>> = Data3,
    Headers = decode_headers(BinHeaders, Version),
    <<ValueData:DataLen/binary, Data5/binary>> = Data4,
    {#wsp_multipart_entry{content_type=ContentType,
			  headers=Headers,
			  data=ValueData},Data5}.


parse_credentials(Field, Value) ->
    %% FIXME
    ?WH(Field, Value, []).

format_credentials("basic", [User,Password]) ->
    ["Basic ", base64:encode(User++":"++Password)];
format_credentials(Scheme, Params) ->
    [Scheme, format_params(Params)].

encode_credentials("basic", [User,Password], _Version) ->
    e_value(?ENCODE_SHORT(0),
	    encode_text_string(User),
	    encode_text_string(Password));
encode_credentials(Scheme, Params, Version) ->
    e_value(encode_text_string(Scheme), encode_params(Params, Version)).

decode_credentials(Field, Data, Version) ->
    case scan_header_data(Data) of
	{0, Data0} ->
	    {User,Data1} = d_text_string(Data0),
	    {Password,_Data2} = d_text_string(Data1),
	    ?WH(Field, "basic", [User,Password]);
	{Scheme, Data0} when list(Scheme) ->
	    Params = decode_params(Data0, Version),
	    ?WH(Field, Scheme, Params)
    end.

%%
%% Challenge: Basic Realm-value | Auth-Scheme Realm *Auth-Params
%%

parse_challenge(Field, Value) ->
    %% FIXME
    ?WH(Field, Value, []).

format_challenge({"basic",Realm}, []) ->
    ["Basic ", Realm];
format_challenge({Scheme,Realm}, Params) ->
    [Scheme," ",Realm, format_params(Params)].

encode_challenge({"basic",Realm}, [], _Version) ->
    e_value(?ENCODE_SHORT(0),
	    encode_text_string(Realm));
encode_challenge({Scheme,Realm}, Params, Version) ->
    e_value(encode_text_string(Scheme),
	    encode_text_string(Realm),
	    encode_params(Params, Version)).

decode_challenge(Field, Data, Version) ->
    case scan_header_data(Data) of
	{0, Data0} ->
	    {Realm,_} = d_text_string(Data0),
	    ?WH(Field, {"basic", Realm}, []);
	{Scheme, Data0} when list(Scheme) ->
	    {Realm,_} = d_text_string(Data0),
	    Params = decode_params(Data0, Version),
	    ?WH(Field, {Scheme,Realm}, Params)
    end.


parse_well_known_method(Value) ->
    case Value of
	"GET" ->  'GET';
	"OPTIONS" -> 'OPTIONS';
	"HEAD" -> 'HEAD';
	"DELETE" -> 'DELETE';
	"TRACE" -> 'TRACE';
	"POST" -> 'POST';
	"PUT" -> 'PUT'
    end.

encode_well_known_method(Value, _Version) ->
    case Value of
	'GET'     -> ?ENCODE_SHORT(16#40);
	'OPTIONS' -> ?ENCODE_SHORT(16#41);
	'HEAD'    -> ?ENCODE_SHORT(16#42);
	'DELETE'  -> ?ENCODE_SHORT(16#43);
	'TRACE'   -> ?ENCODE_SHORT(16#44);
	'POST'    -> ?ENCODE_SHORT(16#60);
	'PUT'     -> ?ENCODE_SHORT(16#61)
    end.

decode_well_known_method(Value, _Version) ->
    case Value of
	16#40 -> 'GET';
	16#41 -> 'OPTIONS';
	16#42 -> 'HEAD';
	16#43 -> 'DELETE';
	16#44 -> 'TRACE';
	16#60 -> 'POST';
	16#61 -> 'PUT'
    end.



%%
%% WSP Table 36. Status Code Assignments
%%

encode_status_code(Status) ->
    case Status of
	100 -> 16#10; %% 'Continue'
	101 -> 16#11; %% 'Switching Protocols'
	200 -> 16#20; %% 'OK, Success'
	201 -> 16#21; %% 'Created'
	202 -> 16#22; %% 'Accepted'
	203 -> 16#23; %% 'Non-Authoritative Information'
	204 -> 16#24; %% 'No Content'
	205 -> 16#25; %% 'Reset Content'
	206 -> 16#26; %% 'Partial Content'
	300 -> 16#30; %% 'Multiple Choices'
	301 -> 16#31; %% 'Moved Permanently'
	302 -> 16#32; %% 'Moved temporarily'
	303 -> 16#33; %% 'See Other'
	304 -> 16#34; %% 'Not modified'
	305 -> 16#35; %% 'Use Proxy'
	306 -> 16#36; %% '(reserved)'
	307 -> 16#37; %% 'Temporary Redirect'
	400 -> 16#40; %% 'Bad Request - server could not understand request'
	401 -> 16#41; %% 'Unauthorized'
	402 -> 16#42; %% 'Payment required'
	403 -> 16#43; %% 'Forbidden operation is understood but refused'
	404 -> 16#44; %% 'Not Found'
	405 -> 16#45; %% 'Method not allowed'
	406 -> 16#46; %% 'Not Acceptable'
	407 -> 16#47; %% 'Proxy Authentication required'
	408 -> 16#48; %% 'Request Timeout'
	409 -> 16#49; %% 'Conflict'
	410 -> 16#4A; %% 'Gone'
	411 -> 16#4B; %% 'Length Required'
	412 -> 16#4C; %% 'Precondition failed'
	413 -> 16#4D; %% 'Request entity too large'
	414 -> 16#4E; %% 'Request-URI too large'
	415 -> 16#4F; %% 'Unsupported media type'
	416 -> 16#50; %% 'Requested Range Not Satisfiable'
	417 -> 16#51; %% 'Expectation Failed'
	500 -> 16#60; %% 'Internal Server Error'
	501 -> 16#61; %% 'Not Implemented'
	502 -> 16#62; %% 'Bad Gateway'
	503 -> 16#63; %% 'Service Unavailable'
	504 -> 16#64; %% 'Gateway Timeout'
	505 -> 16#65  %% 'HTTP version not supported'
    end.


decode_status_code(StatusCode) ->
    case StatusCode of
	16#10 -> 100; %% 'Continue'
	16#11 -> 101; %% 'Switching Protocols'
	16#20 -> 200; %% 'OK, Success'
	16#21 -> 201; %% 'Created'
	16#22 -> 202; %% 'Accepted'
	16#23 -> 203; %% 'Non-Authoritative Information'
	16#24 -> 204; %% 'No Content'
	16#25 -> 205; %% 'Reset Content'
	16#26 -> 206; %% 'Partial Content'
	16#30 -> 300; %% 'Multiple Choices'
	16#31 -> 301; %% 'Moved Permanently'
	16#32 -> 302; %% 'Moved temporarily'
	16#33 -> 303; %% 'See Other'
	16#34 -> 304; %% 'Not modified'
	16#35 -> 305; %% 'Use Proxy'
	16#36 -> 306; %% '(reserved)'
	16#37 -> 307; %% 'Temporary Redirect'
	16#40 -> 400; %% 'Bad Request - server could not understand request'
	16#41 -> 401; %% 'Unauthorized'
	16#42 -> 402; %% 'Payment required'
	16#43 -> 403; %% 'Forbidden operation is understood but refused'
	16#44 -> 404; %% 'Not Found'
	16#45 -> 405; %% 'Method not allowed'
	16#46 -> 406; %% 'Not Acceptable'
	16#47 -> 407; %% 'Proxy Authentication required'
	16#48 -> 408; %% 'Request Timeout'
	16#49 -> 409; %% 'Conflict'
	16#4A -> 410; %% 'Gone'
	16#4B -> 411; %% 'Length Required'
	16#4C -> 412; %% 'Precondition failed'
	16#4D -> 413; %% 'Request entity too large'
	16#4E -> 414; %% 'Request-URI too large'
	16#4F -> 415; %% 'Unsupported media type'
	16#50 -> 416; %% 'Requested Range Not Satisfiable'
	16#51 -> 417; %% 'Expectation Failed'
	16#60 -> 500; %% 'Internal Server Error'
	16#61 -> 501; %% 'Not Implemented'
	16#62 -> 502; %% 'Bad Gateway'
	16#63 -> 503; %% 'Service Unavailable'
	16#64 -> 504; %% 'Gateway Timeout'
	16#65 -> 505  %% 'HTTP version not supported'
    end.


%%
%% Content Type Assignments
%%
%% Assingment are found at http://www.wapforum.org/wina/wsp-content-type.htm
%%
%%
%% string(Version, ContentType) -> Code
%%
encode_well_known_media(ContentType, Version) ->
    case ContentType of
	%% WSP_REGISTERED_CONTENT_TYPES
	"application/vnd.uplanet.cacheop-wbxml" ->
	    encode_integer(16#0201);
	"application/vnd.uplanet.signal" ->
	    encode_integer(16#0202);
	"application/vnd.uplanet.alert-wbxml" ->
	    encode_integer(16#0203);
	"application/vnd.uplanet.list-wbxml" ->
	    encode_integer(16#0204);
	"application/vnd.uplanet.listcmd-wbxml" ->
	    encode_integer(16#0205);
	"application/vnd.uplanet.channel-wbxml" ->
	    encode_integer(16#0206);
	"application/vnd.uplanet.provisioning-status-uri" ->
	    encode_integer(16#0207);
	"x-wap.multipart/vnd.uplanet.header-set" ->
	    encode_integer(16#0208);
	"application/vnd.uplanet.bearer-choice-wbxml" ->
	    encode_integer(16#0209);
	"application/vnd.phonecom.mmc-wbxml" ->
	    encode_integer(16#020A);
	"application/vnd.nokia.syncset+wbxml" ->
	    encode_integer(16#020B);
	"image/x-up-wpng" ->
	    encode_integer(16#020C);
	_ ->
	    encode_constrained_media(ContentType, Version)
    end.


encode_constrained_media(ContentType, Version) ->
    case ContentType of
	"*/*"    -> ?ENCODE_SHORT(16#00);
	"text/*" -> ?ENCODE_SHORT(16#01);
	"text/html" -> ?ENCODE_SHORT(16#02);
	"text/plain" -> ?ENCODE_SHORT(16#03);
	"text/x-hdml" -> ?ENCODE_SHORT(16#04);
	"text/x-ttml" -> ?ENCODE_SHORT(16#05);
	"text/x-vcalendar" -> ?ENCODE_SHORT(16#06);
	"text/x-vcard" -> ?ENCODE_SHORT(16#07);
	"text/vnd.wap.wml" -> ?ENCODE_SHORT(16#08);
	"text/vnd.wap.wmlscript" -> ?ENCODE_SHORT(16#09);
	"text/vnd.wap.wta-event" -> ?ENCODE_SHORT(16#0A);
	"multipart/*" -> ?ENCODE_SHORT(16#0B);
	"multipart/mixed" -> ?ENCODE_SHORT(16#0C);
	"multipart/form-data" -> ?ENCODE_SHORT(16#0D);
	"multipart/byterantes" -> ?ENCODE_SHORT(16#0E);
	"multipart/alternative" -> ?ENCODE_SHORT(16#0F);
	"application/*" -> ?ENCODE_SHORT(16#10);
	"application/java-vm" -> ?ENCODE_SHORT(16#11);
	"application/x-www-form-urlencoded" -> ?ENCODE_SHORT(16#12);
	"application/x-hdmlc" -> ?ENCODE_SHORT(16#13);
	"application/vnd.wap.wmlc" -> ?ENCODE_SHORT(16#14);
	"application/vnd.wap.wmlscriptc" -> ?ENCODE_SHORT(16#15);
	"application/vnd.wap.wta-eventc" -> ?ENCODE_SHORT(16#16);
	"application/vnd.wap.uaprof" -> ?ENCODE_SHORT(16#17);
	"application/vnd.wap.wtls-ca-certificate" -> ?ENCODE_SHORT(16#18);
	"application/vnd.wap.wtls-user-certificate" -> ?ENCODE_SHORT(16#19);
	"application/x-x509-ca-cert" -> ?ENCODE_SHORT(16#1A);
	"application/x-x509-user-cert" -> ?ENCODE_SHORT(16#1B);
	"image/*" -> ?ENCODE_SHORT(16#1C);
	"image/gif" -> ?ENCODE_SHORT(16#1D);
	"image/jpeg" -> ?ENCODE_SHORT(16#1E);
	"image/tiff" -> ?ENCODE_SHORT(16#1F);
	"image/png" -> ?ENCODE_SHORT(16#20);
	"image/vnd.wap.wbmp" -> ?ENCODE_SHORT(16#21);
	"application/vnd.wap.multipart.*" -> ?ENCODE_SHORT(16#22);
	"application/vnd.wap.multipart.mixed" -> ?ENCODE_SHORT(16#23);
	"application/vnd.wap.multipart.form-data" -> ?ENCODE_SHORT(16#24);
	"application/vnd.wap.multipart.byteranges" -> ?ENCODE_SHORT(16#25);
	"application/vnd.wap.multipart.alternative" -> ?ENCODE_SHORT(16#26);
	"application/xml" -> ?ENCODE_SHORT(16#27);
	"text/xml" -> ?ENCODE_SHORT(16#28);
	"application/vnd.wap.wbxml" -> ?ENCODE_SHORT(16#29);
	"application/x-x968-cross-cert" -> ?ENCODE_SHORT(16#2A);
	"application/x-x968-ca-cert" -> ?ENCODE_SHORT(16#2B);
	"application/x-x968-user-cert" -> ?ENCODE_SHORT(16#2C);

	%% WAP Version 1.2
	"text/vnd.wap.si" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#2D);
	"application/vnd.wap.sic" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#2E);
	"text/vnd.wap.sl" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#2F);
	"application/vnd.wap.slc" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#30);
	"text/vnd.wap.co" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#31);
	"application/vnd.wap.coc" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#32);
	"application/vnd.wap.multipart.related" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#33);
	"application/vnd.wap.sia" when Version >= ?WSP_12 ->
	    ?ENCODE_SHORT(16#34);
	%% WAP Version 1.3
	"text/vnd.wap.connectivity-xml" when Version >= ?WSP_13 ->
	    ?ENCODE_SHORT(16#35);
	"application/vnd.wap.connectivity-wbxml" when Version >= ?WSP_13 ->
	    ?ENCODE_SHORT(16#36);
	%% WAP Version 1.4
	"application/pkcs7-mime" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#37);
	"application/vnd.wap.hashed-certificate" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#38);
	"application/vnd.wap.signed-certificate" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#39);
	"application/vnd.wap.cert-response" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#3A);
	"application/xhtml+xml" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#3B);
	"application/wml+xml" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#3C);
	"text/css" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#3D);
	"application/vnd.wap.mms-message" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#3E);
	"application/vnd.wap.rollover-certificate" when Version >= ?WSP_14 ->
	    ?ENCODE_SHORT(16#3F);
	%% WAP Version 1.5
	"application/vnd.wap.locc+wbxml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#40);
	"application/vnd.wap.loc+xml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#41);
	"application/vnd.syncml.dm+wbxml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#42);
	"application/vnd.syncml.dm+xml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#43);
	"application/vnd.syncml.notification" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#44);
	"application/vnd.wap.xhtml+xml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#45);
	"application/vnd.wv.csp.cir" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#46);
	"application/vnd.oma.dd+xml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#47);
	"application/vnd.oma.drm.message" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#48);
	"application/vnd.oma.drm.content" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#49);
	"application/vnd.oma.drm.rights+xml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#4A);
	"application/vnd.oma.drm.rights+wbxml" when Version >= ?WSP_15 ->
	    ?ENCODE_SHORT(16#4B);
	_ ->
	    encode_text_string(ContentType)
    end.


decode_well_known_media(Code, Version) when integer(Code) ->
    case Code of
	%% WSP_REGISTERED_CONTENT_TYPES
	16#0201 -> "application/vnd.uplanet.cacheop-wbxml";
	16#0202 -> "application/vnd.uplanet.signal";
	16#0203 -> "application/vnd.uplanet.alert-wbxml";
	16#0204 -> "application/vnd.uplanet.list-wbxml";
	16#0205 -> "application/vnd.uplanet.listcmd-wbxml";
	16#0206 -> "application/vnd.uplanet.channel-wbxml";
	16#0207 -> "application/vnd.uplanet.provisioning-status-uri";
	16#0208 -> "x-wap.multipart/vnd.uplanet.header-set";
	16#0209 -> "application/vnd.uplanet.bearer-choice-wbxml";
	16#020A -> "application/vnd.phonecom.mmc-wbxml";
	16#020B -> "application/vnd.nokia.syncset+wbxml";
	16#020C -> "image/x-up-wpng";
	_ -> decode_constrained_media(Code, Version)
    end;
decode_well_known_media(Media, _Version) when list(Media) ->
    Media;
decode_well_known_media({short,_Data}, Version) ->
    decode_well_known_media(d_long(data), Version).	%% BUG HERE: Data


decode_constrained_media(Code, _Version) when integer(Code) ->
    case Code of
	16#00 -> "*/*";
	16#01 -> "text/*";
	16#02 -> "text/html";
	16#03 -> "text/plain";
	16#04 -> "text/x-hdml";
	16#05 -> "text/x-ttml";
	16#06 -> "text/x-vcalendar";
	16#07 -> "text/x-vcard";
	16#08 -> "text/vnd.wap.wml";
	16#09 -> "text/vnd.wap.wmlscript";
	16#0A -> "text/vnd.wap.wta-event";
	16#0B -> "multipart/*";
	16#0C -> "multipart/mixed";
	16#0D -> "multipart/form-data";
	16#0E -> "multipart/byterantes";
	16#0F -> "multipart/alternative";
	16#10 -> "application/*";
	16#11 -> "application/java-vm";
	16#12 -> "application/x-www-form-urlencoded";
	16#13 -> "application/x-hdmlc";
	16#14 -> "application/vnd.wap.wmlc";
	16#15 -> "application/vnd.wap.wmlscriptc";
	16#16 -> "application/vnd.wap.wta-eventc";
	16#17 -> "application/vnd.wap.uaprof";
	16#18 -> "application/vnd.wap.wtls-ca-certificate";
	16#19 -> "application/vnd.wap.wtls-user-certificate";
	16#1A -> "application/x-x509-ca-cert";
	16#1B -> "application/x-x509-user-cert";
	16#1C -> "image/*";
	16#1D -> "image/gif";
	16#1E -> "image/jpeg";
	16#1F -> "image/tiff";
	16#20 -> "image/png";
	16#21 -> "image/vnd.wap.wbmp";
	16#22 -> "application/vnd.wap.multipart.*";
	16#23 -> "application/vnd.wap.multipart.mixed";
	16#24 -> "application/vnd.wap.multipart.form-data";
	16#25 -> "application/vnd.wap.multipart.byteranges";
	16#26 -> "application/vnd.wap.multipart.alternative";
	16#27 -> "application/xml";
	16#28 -> "text/xml";
	16#29 -> "application/vnd.wap.wbxml";
	16#2A -> "application/x-x968-cross-cert";
	16#2B -> "application/x-x968-ca-cert";
	16#2C -> "application/x-x968-user-cert";
	%% WAP Version 1.2
	16#2D -> "text/vnd.wap.si";
	16#2E -> "application/vnd.wap.sic";
	16#2F -> "text/vnd.wap.sl";
	16#30 -> "application/vnd.wap.slc";
	16#31 -> "text/vnd.wap.co";
	16#32 -> "application/vnd.wap.coc";
	16#33 -> "application/vnd.wap.multipart.related";
	16#34 -> "application/vnd.wap.sia";
	%% WAP Version 1.3
	16#35 -> "text/vnd.wap.connectivity-xml";
	16#36 -> "application/vnd.wap.connectivity-wbxml";
	%% WAP Version 1.4
	16#37 -> "application/pkcs7-mime";
	16#38 -> "application/vnd.wap.hashed-certificate";
	16#39 -> "application/vnd.wap.signed-certificate";
	16#3A -> "application/vnd.wap.cert-response";
	16#3B -> "application/xhtml+xml";
	16#3C -> "application/wml+xml";
	16#3D -> "text/css";
	16#3E -> "application/vnd.wap.mms-message";
	16#3F -> "application/vnd.wap.rollover-certificate";
	%% WAP Version 1.5
	16#40 -> "application/vnd.wap.locc+wbxml";
	16#41 -> "application/vnd.wap.loc+xml";
	16#42 -> "application/vnd.syncml.dm+wbxml";
	16#43 -> "application/vnd.syncml.dm+xml";
	16#44 -> "application/vnd.syncml.notification";
	16#45 -> "application/vnd.wap.xhtml+xml";
	16#46 -> "application/vnd.wv.csp.cir";
	16#47 -> "application/vnd.oma.dd+xml";
	16#48 -> "application/vnd.oma.drm.message";
	16#49 -> "application/vnd.oma.drm.content";
	16#4A -> "application/vnd.oma.drm.rights+xml";
	16#4B -> "application/vnd.oma.drm.rights+wbxml"
    end;
decode_constrained_media(Media, _Version) when list(Media) ->
    Media.


%% Parse <integer> or <integer>.<integer>

parse_version(Value) ->
    case string:tokens(Value, ".") of
	[Major,Minor] ->
	    {list_to_integer(Major), list_to_integer(Minor)};
	[Major] ->
	    case catch list_to_integer(Major) of
		{'EXIT', _} ->
		    Value;
		V -> V
	    end
    end.

format_version({Major,Minor}) ->
    [integer_to_list(Major),".",integer_to_list(Minor)];
format_version(Major) when integer(Major) ->
    integer_to_list(Major);
format_version(Version) when list(Version) ->
    Version.

encode_version({Major,Minor}) ->
    Ver = (((Major-1) band 16#7) bsl 4) bor (Minor band 16#f),
    ?ENCODE_SHORT(Ver);
encode_version(Major) when integer(Major) ->
    Ver = ((Major band 16#7) bsl 4) bor 16#f,
    ?ENCODE_SHORT(Ver);
encode_version(Value) when list(Value) ->
    encode_text_string(Value).


decode_version(Value) when integer(Value) ->
    Major = (Value bsr 4) band 16#7,
    Minor = Value band 16#f,
    if Minor == 16#f ->
	    Major;
       true ->
	    {Major+1,Minor}
    end;
decode_version(Value) when list(Value) ->
    Value.


encode_mms_version({Major,Minor}) ->
    Ver = ((Major band 16#7) bsl 4) bor (Minor band 16#f),
    ?ENCODE_SHORT(Ver);
encode_mms_version(Major) when integer(Major) ->
    Ver = ((Major band 16#7) bsl 4) bor 16#f,
    ?ENCODE_SHORT(Ver);
encode_mms_version(Value) when list(Value) ->
    encode_text_string(Value).


decode_mms_version(Value) when integer(Value) ->
    Major = (Value bsr 4) band 16#7,
    Minor = Value band 16#f,
    if Minor == 16#f ->
	    Major;
       true ->
	    {Major,Minor}
    end;
decode_mms_version(Value) when list(Value) ->
    Value.


%%%
%%% Basic data types
%%%

e_delta_seconds(Value) ->
    encode_integer(Value).


encode_integer(I) when integer(I), I >= 0 , I < 127 ->
    ?ENCODE_SHORT(I);
encode_integer(I) when integer(I) ->
    encode_long_integer(I);
encode_integer(List) when list(List) ->
    encode_integer(list_to_integer(List)).

decode_integer(Value) when integer(Value) ->
    Value;
decode_integer({short,Data}) ->
    Sz = size(Data)*8,
    <<Value:Sz>> = Data,
    Value.

encode_short_integer(I) ->
    ?ENCODE_SHORT(I).

encode_long_integer(I) when I >= 0 ->
    MOInt = encode_multioctet_integer(I, []),
    MOIntLen = length(MOInt),
    list_to_binary([MOIntLen band 16#1f | MOInt]).

encode_multioctet_integer(I,Acc) when I < 256 ->
    [I | Acc];
encode_multioctet_integer(I,Acc) ->
    encode_multioctet_integer(I bsr 8, [(I band 16#ff) | Acc]).


%% Integer-Value: Short-Integer | Long-Integer
%% Short-Integer: <<1:Short:7>>
%% Long-Integer:  <<0-30, X:0-30>>
%% return {Integer,Tail}
d_integer_value(<<1:1,Integer:7,Tail/binary>>) ->
    {Integer, Tail};
d_integer_value(<<0:3,Len:5,Data/binary>>) when Len =/= 31 ->
    Sz = Len*8,
    <<Integer:Sz, Tail/binary>> = Data,
    {Integer, Tail}.

decode_short_integer(<<1:1,Septet:7,T100/binary>>) ->
    {Septet, T100}.

decode_long_integer(<<0:3,Len:5,Data/binary>>) when Len =/= 31 ->
    Sz = Len*8,
    <<Val:Sz, Tail/binary>> = Data,
    {Val, Tail}.

d_long(Data) ->
    Sz = size(Data)*8,
    <<Value:Sz>> = Data,
    Value.


encode_uri_value(Data) ->
    encode_text_string(Data).

decode_uri_value(Data) when list(Data) ->
    Data.

%% parse quoted string
decode_quoted_string([$" | List]) ->
    List.

encode_quoted_string([$" | Value]) ->
    case lists:reverse(Value) of
	[$" | Value1] ->
	    <<$", (list_to_binary(lists:reverse(Value1)))/binary, 0>>;
	_ ->
	    <<$", (list_to_binary(Value))/binary, 0>>
    end;
encode_quoted_string(Value) ->
    <<$", (list_to_binary(Value))/binary, 0>>.



decode_text_string(List) when list(List) ->
    List;
decode_text_string(Bin) when binary(Bin) ->
    binary_to_list(Bin).



encode_text_string(A) when atom(A) ->
    encode_text_string(atom_to_list(A));
encode_text_string([H|T]) when H >= 128 ->
    <<(list_to_binary([127,H|T]))/binary,0>>;
encode_text_string(S) ->
    <<(list_to_binary(S))/binary,0>>.


encode_text_value(undefined) ->
    <<0>>;
encode_text_value([$"|T]) ->
    %% remove ending quote ?
    <<34,(list_to_binary(T))/binary>>;
encode_text_value(L) ->
    encode_text_string(L).


d_text_value(<<0,T100/binary>>) ->
    { "", T100};
d_text_value(<<34,_Tail/binary>>=Data) ->
    d_text_string(Data);
d_text_value(Data) ->
    d_text_string(Data).


d_text_string(<<127,Data/binary>>) -> %% Remove quote
    d_text_string(Data,[]);
d_text_string(Data) ->
    d_text_string(Data,[]).

d_text_string(<<0,Tail/binary>>,A) ->
    {lists:reverse(A), Tail};
d_text_string(<<C,Tail/binary>>,A) ->
    d_text_string(Tail,[C|A]);
d_text_string(<<>>, A) ->
    {lists:reverse(A), <<>>}.


d_q_value(<<0:1,Q:7,Tail/binary>>) ->
    QVal =
	if Q >= 1, Q =< 100 ->
		lists:flatten(io_lib:format("0.~2..0w", [Q-1]));
	   Q >= 101, Q =< 1099 ->
		lists:flatten(io_lib:format("0.~3..0w", [Q-100]));
	   true ->
		io:format("Q-value to big ~w\n", [Q]),
		"***"
	end,
    {QVal, Tail};
d_q_value(<<1:1,Q1:7,0:1,Q0:7,Tail/binary>>) ->
    Q = (Q1 bsl 7) bor Q0,
    QVal =
	if Q >= 1, Q =< 100 ->
		lists:flatten(io_lib:format("0.~2..0w", [Q-1]));
	   Q >= 101, Q =< 1099 ->
		lists:flatten(io_lib:format("0.~3..0w", [Q-100]));
	   true ->
		io:format("Q-value to big ~w\n", [Q]),
		"***"
	end,
    {QVal, Tail}.


%%
%% Decode uintvar
%%
d_uintvar(<<0:1,S0:7,T100/binary>>) ->
    {S0, T100};
d_uintvar(<<1:1,S1:7,0:1,S0:7,T100/binary>>) ->
    {(S1 bsl 7) bor S0, T100};
d_uintvar(<<1:1,S2:7,1:1,S1:7,0:1,S0:7,T100/binary>>) ->
    {(S2 bsl 14) bor (S1 bsl 7) bor S0, T100};
d_uintvar(<<1:1,S3:7,1:1,S2:7,1:1,S1:7,0:1,S0:7,T100/binary>>) ->
    {(S3 bsl 21) bor (S2 bsl 14) bor (S1 bsl 7) bor S0, T100};
d_uintvar(<<1:1,S4:7,1:1,S3:7,1:1,S2:7,1:1,S1:7,0:1,S0:7,T100/binary>>) ->
    {(S4 bsl 28) bor (S3 bsl 21) bor (S2 bsl 14) bor (S1 bsl 7) bor S0, T100}.


e_uintvar(I) when I < 128 -> <<I>>;
e_uintvar(I) -> e_uintvar(I,[]).

e_uintvar(0,Acc) ->
    list_to_binary(Acc);
e_uintvar(I,[]) ->
    e_uintvar(I bsr 7, [I band 16#7f]);
e_uintvar(I,Acc) ->
    e_uintvar(I bsr 7, [16#80 bor (I band 16#7f) | Acc]).


e_value(B) ->
    Sz = size(B),
    if Sz =< 30 ->
	    <<Sz:8, B/binary>>;
       true ->
	    <<31:8, (e_uintvar(Sz))/binary, B/binary >>
    end.

e_value(B1,B2) ->
    Sz = size(B1)+size(B2),
    if Sz =< 30 ->
	    <<Sz:8, B1/binary, B2/binary>>;
       true ->
	    <<31:8, (e_uintvar(Sz))/binary, B1/binary, B2/binary >>
    end.

e_value(B1,B2,B3) ->
    Sz = size(B1)+size(B2)+size(B3),
    if Sz =< 30 ->
	    <<Sz:8, B1/binary,B2/binary,B3/binary>>;
       true ->
	    <<31:8,(e_uintvar(Sz))/binary,B1/binary,B2/binary,B3/binary>>
    end.

e_value(B1,B2,B3,B4) ->
    Sz = size(B1)+size(B2)+size(B3)+size(B4),
    if Sz =< 30 ->
	    <<Sz:8, B1/binary,B2/binary,B3/binary,B4/binary>>;
       true ->
	    <<31:8,(e_uintvar(Sz))/binary,B1/binary,
	     B2/binary,B3/binary,B4/binary>>
    end.

%%
%% Extened methods
%%
decode_extended_methods(<<PduType:8, Data/binary>>) ->
    Type = decode_pdu_type(PduType),
    {Method, Data1} = d_text_string(Data),
    [{Type,Method} | decode_extended_methods(Data1)];
decode_extended_methods(<<>>) ->
    [].

encode_extended_methods(Ms) ->
    list_to_binary(encode_ext_methods(Ms)).

encode_ext_methods([{Type,Method} | T]) ->
    [ encode_pdu_type(Type), encode_text_string(Method) |
      encode_ext_methods(T)];
encode_ext_methods([]) ->
    [].

%%
%% Address lists used by redirect-pdu and aliases-capability
%%
decode_address(D0) ->
    [A] = decode_addresses(D0),
    A.

decode_addresses(D0) ->
    case D0 of
	<<1:1, 1:1,Len:6,B:8,P:16,Addr:Len/binary,D1/binary>> ->
	    [#wdp_address { bearer = B, address = Addr, portnum=P } |
	     decode_addresses(D1)];
	<<1:1, 0:1,Len:6,B:8,Addr:Len/binary,D1/binary>> ->
	    [#wdp_address { bearer = B, address = Addr } |
	     decode_addresses(D1)];
	<<0:1, 1:1,Len:6,P:16,Addr:Len/binary,D1/binary>> ->
	    [#wdp_address { portnum=P, address=Addr } |
	     decode_addresses(D1)];
	<<0:1, 0:1,Len:6,Addr:Len/binary,D1/binary>> ->
	    [#wdp_address { address=Addr } |
	     decode_addresses(D1)];
	<<>> ->
	    []
    end.

encode_addresses(As) ->
    encode_addresses(As, []).

encode_addresses([A|As], Acc) ->
    encode_addresses(As, [encode_address(A)|Acc]);
encode_addresses([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

encode_address(#wdp_address { bearer = B, address = Addr, portnum = P }) ->
    BAddr = if tuple(Addr) ->
		    list_to_binary(inet:ip_to_bytes(Addr));
	       binary(Addr) ->
		    Addr
	    end,
    Len = size(BAddr),
    if B == undefined, P == undefined ->
	    <<0:1, 0:1, Len:6, BAddr/binary>>;
       B == undefined ->
	    <<0:1, 1:1, Len:6, P:16, BAddr/binary>>;
       P == undefined ->
	    <<1:1, 0:1, Len:6, B:8, BAddr/binary>>;
       true ->
	    <<1:1, 1:1, Len:6, B:8, P:16, BAddr/binary>>
    end.




-define(UNIX_TIME_OFFSET, 62167219200).

d_date(Val) when integer(Val) ->
    calendar:gregorian_seconds_to_datetime(Val+?UNIX_TIME_OFFSET);
d_date({short,Data}) ->
    Sz = size(Data)*8,
    <<Sec:Sz>> = Data,
    calendar:gregorian_seconds_to_datetime(Sec+?UNIX_TIME_OFFSET).

e_date(DateTime) ->
    Sec = calendar:datetime_to_gregorian_seconds(DateTime),
    encode_long_integer(Sec -  ?UNIX_TIME_OFFSET).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode http-date (RFC 2068). (MUST be send in RFC1123 date format)
%%          HTTP-date    = rfc1123-date | rfc850-date | asctime-date
%%          rfc1123-date = wkday "," SP date1 SP time SP "GMT"
%%          rfc850-date  = weekday "," SP date2 SP time SP "GMT"
%%          asctime-date = wkday SP date3 SP time SP 4DIGIT
%%
%%          date1        = 2DIGIT SP month SP 4DIGIT
%%                         ; day month year (e.g., 02 Jun 1982)
%%          date2        = 2DIGIT "-" month "-" 2DIGIT
%%                         ; day-month-year (e.g., 02-Jun-82)
%%          date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
%%                         ; month day (e.g., Jun  2)
%%
%%          time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
%%                         ; 00:00:00 - 23:59:59
%%
%%          wkday        = "Mon" | "Tue" | "Wed"
%%                       | "Thu" | "Fri" | "Sat" | "Sun"
%%
%%
%%          weekday      = "Monday" | "Tuesday" | "Wednesday"
%%                       | "Thursday" | "Friday" | "Saturday" | "Sunday"
%%
%%          month        = "Jan" | "Feb" | "Mar" | "Apr"
%%                       | "May" | "Jun" | "Jul" | "Aug"
%%                       | "Sep" | "Oct" | "Nov" | "Dec"
%%
%% decode date or crash!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_http_date(Date) ->
    parse_hdate(tolower(Date)).

parse_hdate([$m,$o,$n,$d,$a,$y,$  | Cs]) -> date2(Cs);
parse_hdate([$t,$u,$e,$s,$d,$a,$y,$  | Cs]) -> date2(Cs);
parse_hdate([$w,$e,$d,$n,$s,$d,$a,$y,$  | Cs]) -> date2(Cs);
parse_hdate([$t,$h,$u,$r,$s,$d,$a,$y,$  | Cs]) -> date2(Cs);
parse_hdate([$f,$r,$i,$d,$a,$y,$  | Cs]) -> date2(Cs);
parse_hdate([$s,$a,$t,$u,$r,$d,$a,$y,$   | Cs]) -> date2(Cs);
parse_hdate([$s,$u,$n,$d,$a,$y,$  | Cs]) -> date2(Cs);
parse_hdate([$m,$o,$n,X | Cs]) -> date13(X,Cs);
parse_hdate([$t,$u,$e,X  | Cs]) -> date13(X,Cs);
parse_hdate([$w,$e,$d,X  | Cs]) -> date13(X,Cs);
parse_hdate([$t,$h,$u,X  | Cs]) -> date13(X,Cs);
parse_hdate([$f,$r,$i,X  | Cs]) -> date13(X,Cs);
parse_hdate([$s,$a,$t,X  | Cs]) -> date13(X,Cs);
parse_hdate([$s,$u,$n,X  | Cs]) -> date13(X,Cs).

date13($ , Cs) -> date3(Cs);
date13($,, [$ |Cs]) -> date1(Cs).

%% date1
date1([D1,D2,$ ,M1,M2,M3,$ ,Y1,Y2,Y3,Y4,$  | Cs]) ->
    M = parse_month([M1,M2,M3]),
    D = list_to_integer([D1,D2]),
    Y = list_to_integer([Y1,Y2,Y3,Y4]),
    {Time,[$ ,$g,$m,$t|Cs1]} = parse_time(Cs),
    { {{Y,M,D},Time}, Cs1}.

%% date2
date2([D1,D2,$-,M1,M2,M3,$-,Y1,Y2 | Cs]) ->
    M = parse_month([M1,M2,M3]),
    D = list_to_integer([D1,D2]),
    Y = 1900 + list_to_integer([Y1,Y2]),
    {Time, [$ ,$g,$m,$t|Cs1]} = parse_time(Cs),
    {{{Y,M,D}, Time}, Cs1}.

%% date3
date3([M1,M2,M3,$ ,D1,D2,$ | Cs]) ->
    M = parse_month([M1,M2,M3]),
    D = if D1 == $  -> list_to_integer([D2]);
	   true -> list_to_integer([D1,D2])
	end,
    {Time,[$ ,Y1,Y2,Y3,Y4|Cs1]} = parse_time(Cs),
    Y = list_to_integer([Y1,Y2,Y3,Y4]),
    { {{Y,M,D}, Time}, Cs1 }.

%% decode lowercase month
parse_month("jan") -> 1;
parse_month("feb") -> 2;
parse_month("mar") -> 3;
parse_month("apr") -> 4;
parse_month("may") -> 5;
parse_month("jun") -> 6;
parse_month("jul") -> 7;
parse_month("aug") -> 8;
parse_month("sep") -> 9;
parse_month("oct") -> 10;
parse_month("nov") -> 11;
parse_month("dec") -> 12.

%% decode time HH:MM:SS
parse_time([H1,H2,$:,M1,M2,$:,S1,S2|Cs]) ->
    { {list_to_integer([H1,H2]),
       list_to_integer([M1,M2]),
       list_to_integer([S1,S2]) }, Cs}.

%% encode date into rfc1123-date (must be a GMT time!!!)
fmt_date({{Y,M,D},{TH,TM,TS}}) ->
    WkDay = case calendar:day_of_the_week({Y,M,D}) of
		1 -> "Mon";
		2 -> "Tue";
		3 -> "Wed";
		4 -> "Thu";
		5 -> "Fri";
		6 -> "Sat";
		7 -> "Sun"
	    end,
    lists:flatten(io_lib:format("~s, ~2..0w ~s ~4..0w "
				"~2..0w:~2..0w:~2..0w GMT",
				[WkDay, D, fmt_month(M), Y, TH, TM, TS])).

fmt_current_date() ->
    fmt_date(calendar:universal_time()).

%% decode lowercase month
fmt_month(1) -> "Jan";
fmt_month(2) -> "Feb";
fmt_month(3) -> "Mar";
fmt_month(4) -> "Apr";
fmt_month(5) -> "May";
fmt_month(6) -> "Jun";
fmt_month(7) -> "Jul";
fmt_month(8) -> "Aug";
fmt_month(9) -> "Sep";
fmt_month(10) -> "Oct";
fmt_month(11) -> "Nov";
fmt_month(12) -> "Dec".
