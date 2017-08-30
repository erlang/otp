%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2015. All Rights Reserved.
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
%%

-module(http_format_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("http_internal.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

all() -> 
    [{group, chunk}, http_response, http_request,
     validate_request_line, {group, script}, is_absolut_uri,
     convert_netscapecookie_date, check_content_length_encoding].

groups() -> 
    [{script, [], [esi_parse_headers, cgi_parse_headers]},
     {chunk, [],
      [chunk_decode, chunk_encode, chunk_extensions_otp_6005,
       chunk_decode_otp_6264,
       chunk_decode_empty_chunk_otp_6511,
       chunk_whitespace_suffix,
       chunk_decode_trailer, chunk_max_headersize, chunk_max_bodysize, chunk_not_hex]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

chunk_decode() ->
    [{doc, "Test http_chunk:decode/3"}].
chunk_decode(Config) when is_list(Config) ->
    ReqHeaders =  #http_request_h{'transfer-encoding' = "chunked"},
    ChunkedBody = "A" ++ ?CRLF ++ "1234567890" ++ ?CRLF ++ "4" ++
	?CRLF ++ "HEJ!" ++ ?CRLF ++ "0" ++ ?CRLF ++ ?CRLF,    
    {ok, {Headers, Body}} = 
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    "1234567890HEJ!"  = binary_to_list(Body),
    %% When the "chunked" is removed by the decoding the header
    %% will become empty in this case i.e. undefined!
    NewReqHeaders = http_chunk:handle_headers(ReqHeaders, Headers),
    undefined = NewReqHeaders#http_request_h.'transfer-encoding',

    NewChunkedBody = ["A" ++ [?CR], [?LF] ++ "12345", "67890" ++ ?CRLF ++ "4"
		      ++ ?CRLF ++ "HEJ!" ++ ?CRLF ++ "0" ++ [?CR], 
		      [?LF, ?CR, ?LF]], 

    {Module, Function, Args} = 
	http_chunk:decode(list_to_binary(hd(NewChunkedBody)),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),

    {_, Body} = parse(Module, Function, Args, tl(NewChunkedBody)),
    "1234567890HEJ!" = binary_to_list(Body).

%%-------------------------------------------------------------------------
chunk_extensions_otp_6005() ->
    [{doc, "Make sure so called extensions are ignored"}].
chunk_extensions_otp_6005(Config) when is_list(Config)->
    ChunkedBody = "A;ignore this" ++ ?CRLF ++ "1234567890" ++
	?CRLF ++ "4" ++ ?CRLF  ++ "HEJ!"++ ?CRLF  ++ "0" ++
	";extensionname=extensionvalue;foo=bar" ++ ?CRLF ++ ?CRLF,
     {ok, {["content-length:14"], Body}} = 
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
	"1234567890HEJ!"  = binary_to_list(Body),

    ChunkedBody1 = ["A;", "ignore this" ++ [?CR], [?LF] ++ "1234567890" ++
		    ?CRLF ++ "4" ++ ?CRLF  ++ "HEJ!"++ ?CRLF  ++ "0" ++
		    ";extensionname=extensionvalue;foo=bar" ++ ?CRLF ++ ?CRLF],
    
    {Module1, Function1, Args1} = 
	http_chunk:decode(list_to_binary(hd(ChunkedBody1)),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    {_, NewBody} = parse(Module1, Function1, Args1, tl(ChunkedBody1)),
    "1234567890HEJ!" = binary_to_list(NewBody).

%%-------------------------------------------------------------------------
chunk_decode_otp_6264() ->
    [{doc, "Check that 0 in the body does not count as the last chunk"}].
chunk_decode_otp_6264(Config) when is_list(Config)->
    ChunkedBody = "A;ignore this" ++ ?CRLF ++ "1234567890" ++
	?CRLF ++ "4" ++ ?CRLF  ++ "0123"++ ?CRLF  ++ "0" ++
	";extensionname=extensionvalue;foo=bar" ++ ?CRLF ++ ?CRLF,
     {ok, {["content-length:14"], Body}} = 
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
	"12345678900123"  = binary_to_list(Body),

     NewChunkedBody = ["A" ++ [?CR], [?LF] ++ "12345", "67890" ++ ?CRLF ++ "1"
		      ++ ?CRLF ++ "0" ++ ?CRLF ++ "0" ++ [?CR], 
		      [?LF, ?CR, ?LF]],

    {Module, Function, Args} = 
	http_chunk:decode(list_to_binary(hd(NewChunkedBody)),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    {_, NewBody} = parse(Module, Function, Args, tl(NewChunkedBody)),
    "12345678900" = binary_to_list(NewBody),
    
    NewChunkedBody1 = ["A" ++ [?CR], [?LF] ++ "12345", "67890" ++ ?CRLF ++ "1"
		      ++ ?CRLF ++ "0", ?CRLF ++ "0", [?CR], [?LF], 
		      [?CR], [?LF]],

    {Module1, Function1, Args1} = 
	http_chunk:decode(list_to_binary(hd(NewChunkedBody1)),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    {_, NewBody} = parse(Module1, Function1, Args1, tl(NewChunkedBody1)),
    "12345678900" = binary_to_list(NewBody).
%%-------------------------------------------------------------------------
chunk_decode_empty_chunk_otp_6511(Config) when is_list(Config) ->
    ChunkedBody = "0" ++ ?CRLF ++ ?CRLF,
    {ok,{["content-length:0"],<<>>}}  = 
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE).
    
%%-------------------------------------------------------------------------
chunk_whitespace_suffix() ->
    [{doc, "Test whitespace after chunked length header"}].
chunk_whitespace_suffix(Config) when is_list(Config) ->
    ChunkedBody = "1a  ; ignore-stuff-here" ++ ?CRLF ++
	"abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10   "  ++ ?CRLF
	++ "1234567890abcdef" ++ ?CRLF  ++ "0  " ++ ?CRLF
	++ "some-footer:some-value"  ++ ?CRLF
	++ "another-footer:another-value" ++ ?CRLF ++ ?CRLF,
    {ok, {["content-length:42", "another-footer:another-value",
	   "some-footer:some-value", ""],
	  <<"abcdefghijklmnopqrstuvwxyz1234567890abcdef">>}} =
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE).

%%-------------------------------------------------------------------------
chunk_decode_trailer() ->
    [{doc,"Make sure trailers are handled correctly. Trailers should"
     "become new headers"}].
chunk_decode_trailer(Config) when is_list(Config)->
    ChunkedBody = "1a; ignore-stuff-here" ++ ?CRLF ++ 
	"abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10"  ++ ?CRLF 
	++ "1234567890abcdef" ++ ?CRLF  ++ "0" ++ ?CRLF 
	++ "some-footer:some-value"  ++ ?CRLF 
	++ "another-footer:another-value" ++ ?CRLF ++ ?CRLF,

    {ok, {Headers, Body}} = 
    http_chunk:decode(list_to_binary(ChunkedBody),
		      ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    %% There is no guaranteed order of headers.
    true = lists:member("content-length:42", Headers),
    true = lists:member("some-footer:some-value", Headers),
    true = lists:member("another-footer:another-value", Headers),
    "abcdefghijklmnopqrstuvwxyz1234567890abcdef"  = binary_to_list(Body), 

    ChunkedBody1 = "1a" ++ ?CRLF ++ 
	"abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10"  ++ ?CRLF 
	++ "1234567890abcdef" ++ ?CRLF  ++ "0" ++ ?CRLF
	++ "some-footer:some-value"  ++ ?CRLF  ++ ?CRLF,
    
    {ok, {Headers1, Body1}} = 
	http_chunk:decode(list_to_binary(ChunkedBody1),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    true = lists:member("content-length:42", Headers1),
    true = lists:member("some-footer:some-value", Headers1),
    false = lists:member("another-footer:another-value", Headers1),
    "abcdefghijklmnopqrstuvwxyz1234567890abcdef"  = binary_to_list(Body1), 


    ChunkedBody2 = ["1a", ?CRLF ++ 
		    "abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10"  ++ ?CRLF 
		    ++ "1234567890abcdef" ++ ?CRLF  ++ "0", ";", 
		    "ignore stuff here=foobar", ?CRLF ++
		    "some-footer:some-value", ?CRLF, ?CRLF],
      
     {Module, Function, Args} = 
 	http_chunk:decode(list_to_binary(hd(ChunkedBody2)),
 			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
     {_, NewBody} = parse(Module, Function, Args, tl(ChunkedBody2)),
     "abcdefghijklmnopqrstuvwxyz1234567890abcdef" = binary_to_list(NewBody),
   
    ChunkedBody3 = ["1a", ?CRLF ++ 
		    "abcdefghijklmnopqrstuvwxyz", ?CRLF ++ "10"  ++  ?CRLF
		    ++ "1234567890abcdef" ++  ?CRLF ++ "0" ++ ?CRLF 
		    ++ "some-footer:some-value", [?CR], [?LF] , ?CRLF],
    
     {Module1, Function1, Args1} = 
 	http_chunk:decode(list_to_binary(hd(ChunkedBody3)),
 			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
     {_, NewBody} = parse(Module1, Function1, Args1, tl(ChunkedBody3)),
     "abcdefghijklmnopqrstuvwxyz1234567890abcdef" = binary_to_list(NewBody).

%%-------------------------------------------------------------------------
chunk_encode() ->
    [{doc, "Test http_chunk:encode/1 & http_chunk:encode_last/0"}].
chunk_encode(Config) when is_list(Config) ->
    <<54, ?CR, ?LF, 102,111,111,98,97,114, ?CR, ?LF>> = 
	http_chunk:encode(list_to_binary("foobar")),
    ["6", ?CR, ?LF,"foobar", ?CR, ?LF] = http_chunk:encode("foobar"),
    <<$0, ?CR, ?LF, ?CR, ?LF >> = http_chunk:encode_last().
%%-------------------------------------------------------------------------
chunk_max_headersize() ->
    [{doc, "Test max header limit"}].
chunk_max_headersize(Config) when is_list(Config) ->
    ChunkedBody = "1a; ignore-stuff-here" ++ ?CRLF ++ 
	"abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10"  ++ ?CRLF 
	++ "1234567890abcdef" ++ ?CRLF  ++ "0" ++ ?CRLF 
	++ "some-footer:some-value"  ++ ?CRLF 
	++ "another-footer:another-value" ++ ?CRLF ++ ?CRLF,
    
    {ok, {_, _}} = 
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    %% Too long in length header
    {error,{header_too_long, {max, 1}}} = 
	(catch http_chunk:decode(list_to_binary(ChunkedBody),
				 ?HTTP_MAX_BODY_SIZE, 1)),
    
    %% Too long in extension field
    {error,{header_too_long, {max, 10}}} = 
	(catch http_chunk:decode(list_to_binary(ChunkedBody),
				 ?HTTP_MAX_BODY_SIZE, 10)),
    
    %% Too long in trailer
    {error,{header_too_long, {max, 30}}} = 
	(catch http_chunk:decode(list_to_binary(ChunkedBody),
				 ?HTTP_MAX_BODY_SIZE, 30)).
%%-------------------------------------------------------------------------
chunk_not_hex() ->
    [{doc, "Test bad chunked length header"}].
chunk_not_hex(Config) when is_list(Config) ->
     ChunkedBody = "åäö; ignore-stuff-here" ++ ?CRLF ++ 
	"abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10"  ++ ?CRLF 
	++ "1234567890abcdef" ++ ?CRLF  ++ "0" ++ ?CRLF 
	++ "some-footer:some-value"  ++ ?CRLF 
	++ "another-footer:another-value" ++ ?CRLF ++ ?CRLF,
     {error,{chunk_size, "åäö"}} = 
	(catch http_chunk:decode(list_to_binary(ChunkedBody),
				 ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE)).
%%-------------------------------------------------------------------------
chunk_max_bodysize() ->
    [{doc, "Test max body limit"}].
chunk_max_bodysize(Config) when is_list(Config) ->
     ChunkedBody = "1a; ignore-stuff-here" ++ ?CRLF ++ 
	"abcdefghijklmnopqrstuvwxyz" ++ ?CRLF ++ "10"  ++ ?CRLF 
	++ "1234567890abcdef" ++ ?CRLF  ++ "0" ++ ?CRLF 
	++ "some-footer:some-value"  ++ ?CRLF 
	++ "another-footer:another-value" ++ ?CRLF ++ ?CRLF,
    {ok, {_, _}} = 
	http_chunk:decode(list_to_binary(ChunkedBody),
			  ?HTTP_MAX_BODY_SIZE, ?HTTP_MAX_HEADER_SIZE),
    
    %% Too long body
    {error,{body_too_big, {max, 10}}} = 
	 (catch http_chunk:decode(list_to_binary(ChunkedBody),
				  10, ?HTTP_MAX_HEADER_SIZE)).

%%-------------------------------------------------------------------------
http_response() ->
    [{doc, "Test httpc_response:parse*. This test case will simulate that the "
     "message will be recived a little at the time on a socket and the "
     "package may be broken up into smaller parts at arbitrary point."}].
http_response(Config) when is_list(Config) ->

    HttpHead1 = ["HTTP", "/1.1 ", "20", "0 ", "ok", [?CR, ?LF], 
		 "content-length:83" ++ ?CRLF ++ "content", "-type:", 
		 "text/html" ++ ?CRLF ++  
		 "date:Thu, 28 Oct 2004 07:57:43 GMT" ++ 
		 [?CR], [?LF, ?CR, ?LF]], 
    {"HTTP/1.1",
     200,
     "ok",
     #http_response_h{'content-length' = "83",
		      'content-type' = "text/html",
		      date = "Thu, 28 Oct 2004 07:57:43 GMT"},
     <<>>} =
	parse(httpc_response, parse, [?HTTP_MAX_HEADER_SIZE, false],
	      HttpHead1),

    HttpHead2 = ["HTTP/1.1 200", " ok", [?CR], [?LF] ++ 
		 "content-length:83" ++ ?CRLF ++ "content-type:", 
		 "text/html" ++ ?CRLF ++  
		 "date:" ++ "Thu, 28 Oct 2004 07:57:43 GMT" ++ 
		 ?CRLF, ?CRLF], 
    {"HTTP/1.1",
     200,
     "ok",
     #http_response_h{'content-length' = "83",
		      'content-type' = "text/html",
		      date = "Thu, 28 Oct 2004 07:57:43 GMT"},
     <<>>} =
	parse(httpc_response, parse, [?HTTP_MAX_HEADER_SIZE, false], 
	      HttpHead2),

    HttpHead3 = ["HTTP/1.1 200 ", "ok", ?CRLF ++ 
		 "content-length:83" ++ ?CRLF ++ "content-type:", 
		 "text/html" ++ ?CRLF ++  
		 "date:" ++ "Thu, 28 Oct 2004 07:57:43 GMT" ++ 
		 [?CR, ?LF,?CR], [?LF]], 
    {"HTTP/1.1",
     200,
     "ok",
     #http_response_h{'content-length' = "83",
		      'content-type' = "text/html",
		      date = "Thu, 28 Oct 2004 07:57:43 GMT"},
     <<>>} =
	parse(httpc_response, parse, [?HTTP_MAX_HEADER_SIZE, false], 
	      HttpHead3),
    
    HttpBody = ["<HTML>\n<HEAD>\n<TITLE> dummy </TITLE>\n</HEAD>\n<BODY>\n",
		"<H1>dummy</H1>\n</BODY>\n</HTML>\n"],
    
    NewBody = lists:flatten(HttpBody), 
    Length = length(NewBody),
    NewBody = 
	binary_to_list(parse
		       (httpc_response, whole_body, [<<>>,Length], 
			HttpBody)),

    HttpBody1 = ["<HTML", ">\n<HEAD>", "\n<TITLE> dummy </TITLE>\n</HEAD>\n",
		 "<BODY>\n", "<H1>du", "mmy</H1>\n</BODY>\n</HTML>\n"],

    NewBody1 = lists:flatten(HttpBody1),
    Length1 = length(NewBody1),
    NewBody1 = binary_to_list(parse
			      (httpc_response, whole_body,
			       [<<>>,Length1], HttpBody1)),
    ok.
%%-------------------------------------------------------------------------
http_request() ->
    [{doc, "Test httpd_request:parse* This test case will simulate that the " 
     "message will be recived a little at the time on a socket and the " 
      "package may be broken up into smaller parts at arbitrary point."}].
http_request(Config) when is_list(Config) ->

    HttpHead = ["GE", "T ", "http://www.erlang", ".org ", "HTTP", 
		"/1.1" ++ ?CRLF ++ "host:", 
		"www.erlang.org" ++ [?CR], 
		[?LF] ++ "te: " ++ ?CRLF, ?CRLF], 
    {"GET",
     "http://www.erlang.org",
     "HTTP/1.1",
     {#http_request_h{host = "www.erlang.org", te = []},
      [{"te", []}, {"host", "www.erlang.org"}]}, <<>>} =
	parse(httpd_request, parse, [[{max_header, ?HTTP_MAX_HEADER_SIZE},
				      {max_version, ?HTTP_MAX_VERSION_STRING}, 
				      {max_method, ?HTTP_MAX_METHOD_STRING},
				      {max_content_length, ?HTTP_MAX_CONTENT_LENGTH}
				     ]],
	      HttpHead),

    HttpHead1 = ["GET http://www.erlang.org HTTP/1.1" ++ 
		 [?CR], [?LF, ?CR, ?LF]],
    {"GET",
     "http://www.erlang.org",
     "HTTP/1.1",
     {#http_request_h{}, []}, <<>>} =
	parse(httpd_request, parse,  [[{max_header, ?HTTP_MAX_HEADER_SIZE},
				       {max_version, ?HTTP_MAX_VERSION_STRING}, 
				       {max_method, ?HTTP_MAX_METHOD_STRING},
				       {max_content_length, ?HTTP_MAX_CONTENT_LENGTH}
				      ]], HttpHead1),


    HttpHead2 = ["GET http://www.erlang.org HTTP/1.1" ++ 
		 [?CR, ?LF, ?CR], [?LF]],
    {"GET",
     "http://www.erlang.org",
     "HTTP/1.1",
     {#http_request_h{}, []}, <<>>} =
	parse(httpd_request, parse, [[{max_header, ?HTTP_MAX_HEADER_SIZE},
				      {max_version, ?HTTP_MAX_VERSION_STRING}, 
				      {max_method, ?HTTP_MAX_METHOD_STRING},
				      {max_content_length, ?HTTP_MAX_CONTENT_LENGTH}
				     ]], HttpHead2),

    %% Note the following body is not related to the headers above
    HttpBody = ["<HTML>\n<HEAD>\n<TITLE> dummy </TITLE>\n</HEAD>\n<BODY>\n",
		"<H1>dummy</H1>\n</BODY>\n</HTML>\n"],

    NewBody = lists:flatten(HttpBody),
    Length = length(NewBody),
    NewBody = 
	binary_to_list(parse
		       (httpd_request, whole_body, [<<>>,Length], HttpBody)),

    HttpBody1 = ["<HTML", ">\n<HEAD>", "\n<TITLE> dummy </TITLE>\n</HEAD>\n",
		 "<BODY>\n", "<H1>du", "mmy</H1>\n</BODY>\n</HTML>\n"],

    NewBody1 = lists:flatten(HttpBody1), 
    Length1 = length(NewBody1),
    NewBody1 = 	
	binary_to_list(parse
		       (httpd_request, whole_body, 
			[<<>>, Length1], HttpBody1)).
%%-------------------------------------------------------------------------
validate_request_line() ->
    [{doc, "Test httpd_request:validate/3. Makes sure you can not get past"
     " the server_root and that the request is recognized by the server"
     " and protcol version."}].
validate_request_line(Config) when is_list(Config) ->

    %% HTTP/0.9 only has GET requests
    ok = 
	httpd_request:validate("GET", "http://www.erlang/org", "HTTP/0.9"),
    {error, {not_supported, 
	     {"HEAD", "http://www.erlang/org", "HTTP/0.9"}}} =
	httpd_request:validate("HEAD", "http://www.erlang/org", "HTTP/0.9"),
    {error, {not_supported, 
	     {"TRACE", "http://www.erlang/org", "HTTP/0.9"}}} =
	httpd_request:validate("TRACE", "http://www.erlang/org", "HTTP/0.9"),
    {error, {not_supported, 
	     {"POST", "http://www.erlang/org", "HTTP/0.9"}}} =
	httpd_request:validate("POST", "http://www.erlang/org", "HTTP/0.9"),

    %% HTTP/1.* 
    ok = httpd_request:validate("HEAD", "http://www.erlang/org", 
			       "HTTP/1.1"),
    ok = httpd_request:validate("GET", "http://www.erlang/org", 
			       "HTTP/1.1"),  
    ok = httpd_request:validate("POST","http://www.erlang/org", 
			       "HTTP/1.1"),
    ok = httpd_request:validate("TRACE","http://www.erlang/org",
			       "HTTP/1.1"),
    {error, {not_supported, 
	     {"FOOBAR", "http://www.erlang/org", "HTTP/1.1"}}} =
	httpd_request:validate("FOOBAR", "http://www.erlang/org", 
			       "HTTP/1.1"),

    %% Attempts to get outside of server_root directory by relative links 
    ForbiddenUri = "http://127.0.0.1:8888/../../../../../etc/passwd",
    {error, {bad_request, {forbidden, ForbiddenUri}}} = 
	httpd_request:validate("GET", ForbiddenUri, "HTTP/1.1"),

    ForbiddenUri2 = 
	"http://127.0.0.1:8888/././././././../../../../../etc/passwd",
    {error, {bad_request, {forbidden, ForbiddenUri2}}} = 
	httpd_request:validate("GET", ForbiddenUri2, "HTTP/1.1"),

    HexForbiddenUri = "http://127.0.0.1:8888/%2e%2e/%2e%2e/%2e%2e/" 
	"home/ingela/test.html",
    {error, {bad_request, {forbidden, HexForbiddenUri}}} = 
	httpd_request:validate("GET", HexForbiddenUri, "HTTP/1.1"),

    NewForbiddenUri = 
	"http://127.0.0.1:8888/foobar/../../../home/ingela/test.html",
    {error, {bad_request, {forbidden, NewForbiddenUri}}} = 
	httpd_request:validate("GET", NewForbiddenUri, "HTTP/1.1"),

    NewForbiddenUri1 = 
	"http://127.0.0.1:8888/../home/ingela/test.html",
    {error, {bad_request, {forbidden, NewForbiddenUri1}}} = 
	httpd_request:validate("GET", NewForbiddenUri1, "HTTP/1.1").

%%-------------------------------------------------------------------------
check_content_length_encoding() ->
    [{doc, "Test http_request:headers/2. Check that the content-length is"
      " encoded even when it is zero."}].
check_content_length_encoding(Config) when is_list(Config) ->

    %% Check that the content-length is preserved.
    %% Sanity check.
    Header1 = http_request:http_headers(#http_request_h{'content-length'="123"}),
    true = (string:str(Header1, "content-length: 123\r\n") > 0),
    %% Check that content-length=0 is handled correctly.
    Header2 = http_request:http_headers(#http_request_h{'content-length'="0"}),
    true = (string:str(Header2, "content-length: 0\r\n") > 0).

%%-------------------------------------------------------------------------
esi_parse_headers() ->
    [{doc, "Test httpd_esi:*. All header values are received in the same"
      " erlang message."}].
esi_parse_headers(Config) when is_list(Config) ->

    ESIResult = "content-type:text/html\r\ndate:Thu, 28 Oct 2004 07:57:43 "
 	"GMT\r\nstatus:200 OK\r\n\r\nFoobar",
    
    {"content-type:text/html\r\ndate:Thu, 28 Oct 2004 07:57:43 GMT\r\nst"
     "atus:200 OK\r\n" = Headers,
     "Foobar"} = httpd_esi:parse_headers(ESIResult),
    
    {ok,[{"date","Thu, 28 Oct 2004 07:57:43 GMT"},
	 {"content-type","text/html"}], 200} = 
	httpd_esi:handle_headers(Headers),
    
    ESIResult2 = 
	"location:http://foo.bar.se\r\ndate:Thu, 28 Oct 2004 07:57:43 "
	"GMT\r\n\r\n",
    
    {"location:http://foo.bar.se\r\ndate:Thu, 28 Oct 2004 07:57:43 GMT\r\n" = 
     Headers2,[]}
	= httpd_esi:parse_headers(ESIResult2),
    
    {ok,[{"date","Thu, 28 Oct 2004 07:57:43 GMT"},
	 {"location","http://foo.bar.se"}], 302} =
	httpd_esi:handle_headers(Headers2),
    
    {proceed,"/foo/bar.html"} = 
	httpd_esi:handle_headers("location:/foo/bar.html\r\n").

%%--------------------------------------------------------------------
cgi_parse_headers() ->
    [{doc, "Test httpd_cgi:*. This test case will simulate that the "
     "message will be recived a little at the time on a socket and the "
     "package may be broken up into smaller parts at arbitrary point."}].

cgi_parse_headers(Config) when is_list(Config) ->

    CGIResult = ["content-type:text", "/html\ndate:Thu, 28 Oct 2004 07:57:43 "
		 "GMT\nst", "atus:200 OK\n", "\nFoobar"],

    {Headers, Body} = 
	parse(httpd_cgi, parse_headers, [<<>>, [], []], CGIResult),
    
    "Foobar" = binary_to_list(Body),

    {ok,[{"content-type","text/html"},
	 {"date","Thu, 28 Oct 2004 07:57:43 GMT"}], {200,"OK"}}  = 
	httpd_cgi:handle_headers(Headers),

    CGIResult2 = ["location:http://foo.bar.se\ndate:Thu, 28 Oct 2004"
		  " 07:57:43 GMT\n\n"],
    {Headers2,  _} = parse(httpd_cgi, parse_headers, 
			   [<<>>, [], []], CGIResult2),

    {ok,[{"location","http://foo.bar.se"},
	 {"date","Thu, 28 Oct 2004 07:57:43 GMT"}], {302,"Redirect"}} =
	httpd_cgi:handle_headers(Headers2),
    
    {proceed,"/foo/bar.html"} = 
	httpd_cgi:handle_headers(["location:/foo/bar.html\n"]),

    CGIHTTPResult = ["Content-Type:text", "/html\n", "Connection:close\r\n",
		     "Content-Language:en\r\nAge:", "4711\r\n\r\n\nfoobar"],

    {Headers3,  _} = parse(httpd_cgi, parse_headers, 
			   [<<>>, [], []], CGIHTTPResult),

    {ok,[{"content-type","text/html"},
	 {"connection","close"},
	 {"content-language","en"},
	 {"age","4711"}], {200,"ok"}}  = httpd_cgi:handle_headers(Headers3).    
%%-------------------------------------------------------------------------
is_absolut_uri() ->
    [{doc, "Test http_request:is_absolut_uri/1."}].
is_absolut_uri(Config) when is_list(Config) ->
    true = http_request:is_absolut_uri("http://www.erlang.org"),
    true = http_request:is_absolut_uri("https://www.erlang.org"),
    false = http_request:is_absolut_uri("index.html").

%%-------------------------------------------------------------------------
convert_netscapecookie_date() ->
    [{doc, "Test http_util:convert_netscapecookie_date/1."}].
convert_netscapecookie_date(Config) when is_list(Config) ->
    {{2006,1,6},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Mon, 06-Jan-2006 08:59:38 GMT"),
    {{2006,2, 7},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Tue, 07-Feb-2006 08:59:38 GMT"),
    {{2006,3,8},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Wdy, 08-Mar-2006 08:59:38 GMT"),
    {{2006,4,9},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Thu, 09-Apr-2006 08:59:38 GMT"),
    {{2006,5,10},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Fri, 10-May-2006 08:59:38 GMT"),
    {{2006,6,11},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sat, 11-Jun-2006 08:59:38 GMT"),
    {{2006,7,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Jul-2006 08:59:38 GMT"),
    {{2006,8,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Aug-2006 08:59:38 GMT"),
    {{2006,9,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Sep-2006 08:59:38 GMT"),
    {{2006,10,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Oct-2006 08:59:38 GMT"),
    {{2006,11,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Nov-2006 08:59:38 GMT"),
    {{2006,12,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Dec-2006 08:59:38 GMT"),
    {{2006,12,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun 12-Dec-2006 08:59:38 GMT"),
    {{2006,12,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun, 12-Dec-06 08:59:38 GMT"),
    {{2006,12,12},{8,59,38}} = 
	http_util:convert_netscapecookie_date("Sun 12-Dec-06 08:59:38 GMT"),
    {{2036,1,1},{8,0,1}} = 
	http_util:convert_netscapecookie_date("Tue Jan 01 08:00:01 2036 GMT").
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

parse(Module, Function, Args, [Data | Rest]) ->
    case  Module:Function([list_to_binary(Data) | Args]) of
	{ok, Result} ->
	    Result;
	{NewModule, NewFunction, NewArgs} ->
	    parse(NewModule, NewFunction, NewArgs, Rest)
    end.



