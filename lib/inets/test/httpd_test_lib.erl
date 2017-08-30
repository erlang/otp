%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(httpd_test_lib).

-include("inets_test_lib.hrl").

%% Poll functions
-export([verify_request/6, verify_request/7, verify_request/8, is_expect/1]).

-record(state, {request,        % string()
		socket,         % socket()
		status_line,    % {Version, StatusCode, ReasonPharse}
		headers,        % #http_response_h{}
		body,           % binary()
		mfa = {httpc_response, parse, [nolimit, false]}, 
		canceled = [],	       % [RequestId]
		max_header_size = nolimit,   % nolimit | integer() 
		max_body_size = nolimit,    % nolimit | integer()
		print = false
	       }).

%%% Part of http.hrl - Temporary solution %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Response headers
-record(http_response_h,{
%%% --- Standard "General" headers
 	  'cache-control',
 	  connection,
 	  date,
 	  pragma,
 	  trailer,
 	  'transfer-encoding',
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Response" headers
 	  'accept-ranges',
 	  age,
 	  etag,
 	  location,
 	  'proxy-authenticate',
 	  'retry-after',
 	  server,
 	  vary,
 	  'www-authenticate',
%%% --- Standard "Entity" headers
 	  allow,
 	  'content-encoding',
 	  'content-language',
 	  'content-length' = "0",
 	  'content-location',
	  'content-md5',
 	  'content-range',
 	  'content-type',
 	  expires,
 	  'last-modified',
	  other=[]        % list() - Key/Value list with other headers
	 }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% API
%%------------------------------------------------------------------

verify_request(SocketType, Host, Port, Node, RequestStr, Options) ->
    verify_request(SocketType, Host, Port, Node, RequestStr, Options, 30000).

verify_request(SocketType, Host, Port, TranspOpts, Node, RequestStr, Options) 
  when is_list(TranspOpts) ->
    verify_request(SocketType, Host, Port, TranspOpts, Node, RequestStr, Options, 30000);

verify_request(SocketType, Host, Port, Node, RequestStr, Options, TimeOut) 
  when (is_integer(TimeOut) orelse (TimeOut =:= infinity)) ->
    verify_request(SocketType, Host, Port, [], Node, RequestStr, Options, TimeOut).

verify_request(SocketType, Host, Port, TranspOpts, Node, RequestStr, Options, TimeOut) ->
    try inets_test_lib:connect_bin(SocketType, Host, Port, TranspOpts) of
	{ok, Socket} ->
	    ok = inets_test_lib:send(SocketType, Socket, RequestStr),
	    State = case re:run(RequestStr, "printenv", [{capture, none}]) of
			nomatch ->
			    #state{};
			match ->
			    #state{print = true}
		    end,
	    
	    case request(State#state{request = RequestStr, 
				     socket  = Socket}, TimeOut) of
		{error, Reason} ->
		    {error, Reason};
		NewState ->
		    ValidateResult = 
			validate(RequestStr, NewState, Options, Node, Port),
		    inets_test_lib:close(SocketType, Socket),
		    ValidateResult
	    end;

	ConnectError ->
	    ct:fail({connect_error, ConnectError, 
		     [SocketType, Host, Port, TranspOpts]})
    catch
	T:E ->
	    ct:fail({connect_failure, 
		     [{type,       T}, 
		      {error,      E}, 
		      {stacktrace, erlang:get_stacktrace()}, 
		      {args,       [SocketType, Host, Port, TranspOpts]}]}) 
    end.

request(#state{mfa = {Module, Function, Args}, 
	       request = RequestStr, socket = Socket} = State, TimeOut) ->
            
    HeadRequest = lists:sublist(RequestStr, 1, 4),
    receive 
	{tcp, Socket, Data} ->
	    case Module:Function([Data | Args]) of
		{ok, Parsed} ->
		    handle_http_msg(Parsed, State); 
		{_, whole_body, _} when HeadRequest =:= "HEAD" ->
		    State#state{body = <<>>}; 
		NewMFA ->
		    request(State#state{mfa = NewMFA}, TimeOut)
	    end;
	{tcp_closed, Socket} when Function =:= whole_body ->
	    State#state{body = hd(Args)}; 
	{tcp_closed, Socket} ->
	    exit({test_failed, connection_closed});
	{tcp_error, Socket, Reason} ->
	    ct:fail({tcp_error, Reason});    
	{ssl, Socket, Data} ->
	    case Module:Function([Data | Args]) of
		{ok, Parsed} ->
		    handle_http_msg(Parsed, State); 
		{_, whole_body, _} when HeadRequest =:= "HEAD" ->
		    State#state{body = <<>>}; 
		NewMFA ->
		    request(State#state{mfa = NewMFA}, TimeOut)
	    end;
	{ssl_closed, Socket}  when Function =:= whole_body ->
	    State#state{body = hd(Args)};
	{ssl_closed, Socket} ->
	    exit({test_failed, connection_closed});
	{ssl_error, Socket, Reason} ->
	    ct:fail({ssl_error, Reason})
    after TimeOut ->
	    ct:pal("~p ~w[~w]request -> timeout"
		   "~n", [self(), ?MODULE, ?LINE]),
	    ct:fail(connection_timed_out)    
    end.

handle_http_msg({Version, StatusCode, ReasonPharse, Headers, Body}, 
		State = #state{request = RequestStr}) ->
    case is_expect(RequestStr) of
	true ->
	    State#state{status_line = {Version, 
				       StatusCode,
				       ReasonPharse},
			headers = Headers};
	false ->
	    handle_http_body(Body, 
			     State#state{status_line = {Version, 
							StatusCode,
							ReasonPharse},
					 headers = Headers})
    end;

handle_http_msg({ChunkedHeaders, Body}, 
		State = #state{headers = Headers}) ->
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    State#state{headers = NewHeaders, body = Body};

handle_http_msg(Body, State) ->
    State#state{body = Body}.

handle_http_body(<<>>, State = #state{request = "HEAD" ++ _}) ->
    State#state{body = <<>>};

handle_http_body(Body, State = #state{headers = Headers, 
				      max_body_size = MaxBodySize}) ->
     case Headers#http_response_h.'transfer-encoding' of
        "chunked" ->
	    case http_chunk:decode(Body, State#state.max_body_size, 
				   State#state.max_header_size) of
		{Module, Function, Args} ->
		   request(State#state{mfa = {Module, Function, Args}},
			   30000);
		{ok, {ChunkedHeaders, NewBody}} ->
		    NewHeaders = http_chunk:handle_headers(Headers, 
							   ChunkedHeaders),
		    State#state{headers = NewHeaders, body = NewBody}
	    end;
	 _ ->
	     Length =
		 list_to_integer(Headers#http_response_h.'content-length'),
	     case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
		 true ->
		     case httpc_response:whole_body(Body, Length) of
			 {ok, NewBody} ->
			     State#state{body = NewBody};
			 MFA ->
			     request(State#state{mfa = MFA}, 5000) 
		     end;
		 false ->
		     ct:fail(body_too_big)
	     end
     end.

validate(RequestStr, #state{status_line = {Version, StatusCode, _},
			    headers     = Headers, 
			    body        = Body}, Options, N, P) ->
    
    check_version(Version, Options),
    case lists:keysearch(statuscode, 1, Options) of
	{value, _} ->
	    check_status_code(StatusCode, Options, Options);
	_ ->
	    ok
    end,
    HList = http_response:header_list(Headers),
    do_validate(HList, Options, N, P),
    case lists:keysearch("warning", 1, HList) of
	{value, _} ->
	    ok;
	_ ->
	    check_body(RequestStr, StatusCode, 
		       Headers#http_response_h.'content-type',
		       list_to_integer(Headers#http_response_h.'content-length'),
		       Body)
    end.

%--------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------
check_version(Version, Options) ->
    case lists:keysearch(version, 1, Options) of
	{value, {version, Version}} ->
	    	   ok;
	{value, {version, Ver}} ->
	    ct:fail({wrong_version, [{got, Version},
				     {expected, Ver}]});
	_ ->
	    case Version of
		"HTTP/1.1" ->
		    ok;
	       _ ->
		    ct:fail({wrong_version, [{got,      Version}, 
					     {expected, "HTTP/1.1"}]})
	    end
    end.

check_status_code(StatusCode, [], Options) ->
    ct:fail({wrong_status_code, [{got, StatusCode}, {expected, Options}]});
check_status_code(StatusCode, Current = [_ | Rest], Options) ->
    case lists:keysearch(statuscode, 1, Current) of
	{value, {statuscode, StatusCode}} ->
	    ok;
	{value, {statuscode, _OtherStatus}} ->
	    check_status_code(StatusCode, Rest, Options);
	false ->
	    ct:fail({wrong_status_code, [{got, StatusCode}, {expected, Options}]})
    end.

do_validate(_, [], _, _) ->
    ok;
do_validate(Header, [{statuscode, _Code} | Rest], N, P) ->
    do_validate(Header, Rest, N, P);
do_validate(Header, [{header, HeaderField}|Rest], N, P) ->
    LowerHeaderField = http_util:to_lower(HeaderField),
    case lists:keysearch(LowerHeaderField, 1, Header) of
	{value, {LowerHeaderField, _Value}} ->
	    ok;
	false ->
	    ct:fail({missing_header_field, LowerHeaderField, Header});
	_ ->
	    ct:fail({missing_header_field, LowerHeaderField, Header})
    end,
    do_validate(Header, Rest, N, P);
do_validate(Header, [{header, HeaderField, Value}|Rest],N,P) ->
    LowerHeaderField = http_util:to_lower(HeaderField),
    case lists:keysearch(LowerHeaderField, 1, Header) of
	{value, {LowerHeaderField, Value}} ->
	    ok;
	false ->
	    ct:fail({wrong_header_field_value, LowerHeaderField, Header, Value});
	_ ->
	    ct:fail({wrong_header_field_value, LowerHeaderField, Header, Value})
    end,
    do_validate(Header, Rest, N, P);
do_validate(Header,[{no_header, HeaderField}|Rest],N,P) ->
    case lists:keysearch(HeaderField,1,Header) of
	{value,_} ->
	    ct:fail({wrong_header_field_value, HeaderField, Header});
	_ ->
	    ok
    end,
    do_validate(Header, Rest, N, P);
do_validate(Header, [_Unknown | Rest], N, P) ->
    do_validate(Header, Rest, N, P).

is_expect(RequestStr) ->
    case re:run(RequestStr, "xpect:100-continue", [{capture, none}]) of
	match->
	    true;
	nomatch ->
	    false
    end.

%% OTP-5775, content-length
check_body("GET /cgi-bin/erl/httpd_example:get_bin HTTP/1.0\r\n\r\n", 200, "text/html", Length, _Body) when (Length =/= 274) ->
    ct:fail(content_length_error);
check_body("GET /cgi-bin/cgi_echo HTTP/1.0\r\n\r\n", 200, "text/plain", 
	   _, Body) ->
    case size(Body) of
	100 ->
	    ok;
	_ ->
	    ct:fail(content_length_error)
    end;

check_body(RequestStr, 200, "text/html", _, Body) ->
    HeadRequest = lists:sublist(RequestStr, 1, 3),
    case HeadRequest of
	"GET" ->
	    inets_test_lib:check_body(binary_to_list(Body));
	_ ->
	    ok
    end;

check_body(_, _, _, _,_) ->
    ok.

