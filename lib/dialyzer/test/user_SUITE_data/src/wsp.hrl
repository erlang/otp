
%% WSP Table 34. PDU Type Assignments
%%

-define(WSP_Connect,       16#01).
-define(WSP_ConnectReply,  16#02).
-define(WSP_Redirect,      16#03).
-define(WSP_Reply,         16#04).
-define(WSP_Disconnect,    16#05).
-define(WSP_Push,          16#06).
-define(WSP_ConfirmedPush, 16#07).
-define(WSP_Suspend,       16#08).
-define(WSP_Resume,        16#09).

-define(WSP_Get,           16#40).
-define(WSP_Options,       16#41).
-define(WSP_Head,          16#42).
-define(WSP_Delete,        16#43).
-define(WSP_Trace,         16#44).

-define(WSP_Post,          16#60).
-define(WSP_Put,           16#61).

-define(WSP_DataFragmentPDU, 16#80).

%%
%% WSP Table 37. Capability Assignments
%%

-define(WSP_CAP_CLIENT_SDU_SIZE,     16#00).
-define(WSP_CAP_SERVER_SDU_SIZE,     16#01).
-define(WSP_CAP_PROTOCOL_OPTIONS,    16#02).
-define(WSP_CAP_METHOD_MOR,          16#03).
-define(WSP_CAP_PUSH_MOR,            16#04).
-define(WSP_CAP_EXTENDED_METHODS,    16#05).
-define(WSP_CAP_HEADER_CODE_PAGES,   16#06).
-define(WSP_CAP_ALIASES,             16#07).
-define(WSP_CAP_CLIENT_MESSAGE_SIZE, 16#08).
-define(WSP_CAP_SERVER_MESSAGE_SIZE, 16#09).

-define(WSP_CODEPAGE_1, 1).
-define(WSP_DEFAULT_CODEPAGE, ?WSP_CODEPAGE_1).

-define(ANY_LANGUAGE,128).

-define(WSP_10, {1,0}).
-define(WSP_11, {1,1}).
-define(WSP_12, {1,2}).
-define(WSP_13, {1,3}).
-define(WSP_14, {1,4}).
-define(WSP_15, {1,5}).

-define(WSP_COMPLIENT_VERSION, ?WSP_15).
-define(WSP_DEFAULT_VERSION,   ?WSP_12).

-define(WSP_STATUS_CONTINUE, 100).
-define(WSP_STATUS_SWITCHING_PROTOCOLS, 101).
-define(WSP_STATUS_OK, 200).
-define(WSP_STATUS_CREATED, 201).
-define(WSP_STATUS_ACCEPTED, 202).
-define(WSP_STATUS_NON_AUTHORITATIVE_INFORMATION, 203).
-define(WSP_STATUS_NO_CONTENT, 204).
-define(WSP_STATUS_RESET_CONTENT, 205).
-define(WSP_STATUS_PARTIAL_CONTENT, 206).
-define(WSP_STATUS_MULTIPLE_CHOICES, 300).
-define(WSP_STATUS_MOVED_PERMANENTLY, 301).
-define(WSP_STATUS_MOVED_TEMPORARILY, 302).
-define(WSP_STATUS_SEE_OTHER, 303).
-define(WSP_STATUS_NOT_MODIFIED, 304).
-define(WSP_STATUS_USE_PROXY, 305).
-define(WSP_STATUS_RESERVED, 306).
-define(WSP_STATUS_TEMPORARY_REDIRECT, 307).
-define(WSP_STATUS_BAD_REQUEST, 400).
-define(WSP_STATUS_UNAUTHORIZED, 401).
-define(WSP_STATUS_PAYMENT_REQUIRED, 402).
-define(WSP_STATUS_FORBIDDEN, 403).
-define(WSP_STATUS_NOT_FOUND, 404).
-define(WSP_STATUS_METHOD_NOT_ALLOWED, 405).
-define(WSP_STATUS_NOT_ACCEPTABLE, 406).
-define(WSP_STATUS_PROXY_AUTHENTICATION_REQUIRED, 407).
-define(WSP_STATUS_REQUEST_TIMEOUT, 408).
-define(WSP_STATUS_CONFLICT, 409).
-define(WSP_STATUS_GONE, 410).
-define(WSP_STATUS_LENGTH_REQUIRED, 411).
-define(WSP_STATUS_PRECONDITION_FAILED, 412).
-define(WSP_STATUS_REQUEST_ENTITY_TOO_LARGE, 413).
-define(WSP_STATUS_REQUEST_URI_TOO_LARGE, 414).
-define(WSP_STATUS_UNSUPPORTED_MEDIA_TYPE, 415).
-define(WSP_STATUS_REQUESTED_RANGE_NOT_SATISFIABLE, 416).
-define(WSP_STATUS_EXPECTATION_FAILED, 417).
-define(WSP_STATUS_INTERNAL_SERVER_ERROR, 500).
-define(WSP_STATUS_NOT_IMPLEMENTED, 501).
-define(WSP_STATUS_BAD_GATEWAY, 502).
-define(WSP_STATUS_SERVICE_UNAVAILABLE, 503).
-define(WSP_STATUS_GATEWAY_TIMEOUT, 504).
-define(WSP_STATUS_HTTP_VERSION_NOT_SUPPORTED, 505).

-define(ENCODE_SHORT(X), <<1:1, (X):7>>).

-define(ENCODE_LONG(X),
	if (X) =< 16#ff   -> <<1, (X):8>>;
	   (X) =< 16#ffff -> <<2, (X):16>>;
	   (X) =< 16#ffffff -> <<3, (X):24>>;
	   (X) =< 16#ffffffff -> <<4, (X):32>>;
	   true -> encode_long1(X)
	end).


-record(wsp_session,
	{
	  id,            %% uniq session id
	  ref,           %% address quadruple (socketpair)
	  state=null,    %% connected, suspended
	  version,       %% encoding version to use
	  capabilities,  %% client capabilities
	  headers        %% client hop-by-hop headers!!!
	 }).

-record(wsp_header,
	{
	  name,     %% field name
	  value,    %% field value (binary value)
	  params=[] %% field params [{Name,Value} | Value]
	 }).

-record(wsp_multipart_entry,
	{
	  content_type,  %% #wsp_header
	  headers=[],
	  data=(<<>>)
	 }).

-record(wsp_capabilities,
	{
	  aliases=[],             %% [#wdp_address]
	  client_sdu_size=1400,
	  extended_methods=[],    %% [{PduType, Name}]
	  header_code_pages=[],   %% [{Page,Name}] | [Page]
	  protocol_options=[],    %% [push,confirmed_push,resume,
	                          %%  acknowledgement_headers]
	  method_mor = 10,        %% 1?
	  push_mor = 10,          %% 1?
	  server_sdu_size=1400,
	  client_message_size,
	  server_message_size,
	  unknown=[]
	 }).

%% WSP PDU records

-record(wsp_connect,
	{
	  version,     %% protocol version, not wsp version?
	  capabilities,
	  headers
	 }).

-record(wsp_connect_reply,
	{
	  server_session_id,
	  capabilities,
	  headers=[]
	}).

-define(WSP_PERMANENT_REDIRECT, 16#80).
-define(WSP_REUSE_SECURITY,     16#40).

-record(wsp_redirect,
	{
	  flags=[],
	  addresses=[]
	 }).

-record(wsp_disconnect,
	{
	  server_session_id
	 }).

-record(wsp_get,
	{
	  type,
	  uri,
	  headers=[]
	 }).

-record(wsp_post,
	{
	  type,
	  uri,
	  content_type, %% #wsp_header
	  headers=[],
	  data
	 }).

-record(wsp_reply,
	{
	  status,
	  content_type, %% #wsp_header
	  headers=[],
	  data
	}).

-record(wsp_data_fragment_pdu,
	{
	  headers=[],
	  data
	 }).

-record(wsp_push,
	{
	  type = push,
	  content_type, %% #wsp_header
	  headers=[],
	  data
	 }).

-record(wsp_suspend,
	{
	  session_id
	}).

-record(wsp_resume,
	{
	  session_id,
	  capabilities,
	  headers
	 }).

%% NOTE: not a real pdu
-record(wsp_acknowledgement_headers,
	{
	  headers=[]
	}).

-record(wsp_unknown_pdu,
	{
	  type,  %% integer
	  data   %% the payload
	 }).
