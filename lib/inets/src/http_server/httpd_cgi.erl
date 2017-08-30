%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(httpd_cgi).

-export([parse_headers/1, handle_headers/1]).

-include_lib("inets/src/inets_app/inets_internal.hrl").


%%%=========================================================================
%%%  Internal application API 
%%%=========================================================================

%%--------------------------------------------------------------------------
%% parse_headers([Bin, Data, Header, Headers]) -> {RevHeaders, Body} | 
%%                                                {Module, Function, Args}
%% Bin = Data = binary()
%% Header = string() - Accumulator should be [] in first call
%% Headers = [Header] - Accumulator should be [] in first call
%% Body = string()
%% RevHeaders = string() - Note CGI-headers not HTTP-headers 
%%
%% Description: Parses "<<Bin/binary, Data/binary>>" returned from the
%% CGI-script until it findes the end of the CGI-headers (at least one
%% CGI-HeaderField must be supplied) then it returns the CGI-headers
%% and maybe some body data. If {Module, Function, Args} is
%% returned it means that more data needs to be collected from the
%% cgi-script as the end of the headers was not yet found. When more
%% data has been collected call Module:Function([NewData | Args]).
%%
%% NOTE: The headers are backwards and should
%% be so, devide_and_reverse_headers will reverse them back after
%% taking advantage of the fact that they where backwards.  
%%--------------------------------------------------------------------------
parse_headers([Data, Bin,  Header, Headers]) ->
    parse_headers(<<Bin/binary, Data/binary>>, Header, Headers).

%%--------------------------------------------------------------------------
%% handle_headers(CGIHeaders) -> {ok, HTTPHeaders, StatusCode} |
%%                            {proceed, AbsPath}  
%%	CGIHeaders = [string()]   
%%	HTTPHeaders = [{HeaderField, HeaderValue}]
%%      HeaderField = string()
%%      HeaderValue = string()
%%      StatusCode = integer()
%% 
%% Description: Interprets CGI headers and creates HTTP headers and a  
%% appropriate HTTP status code. Note if a CGI location header is present
%% the return value will be {proceed, AbsPath}
%%--------------------------------------------------------------------------
handle_headers(CGIHeaders) ->
    handle_headers(CGIHeaders, [], {200, "ok"}).

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_headers(<<>>, Header, Headers) ->
    {?MODULE, parse_headers, [<<>>, Header, Headers]};
parse_headers(<<?CR,?LF>>, Header, Headers) ->
    {?MODULE, parse_headers, [<<?CR,?LF>>, Header, Headers]};
parse_headers(<<?LF>>, Header, Headers) ->
    {?MODULE, parse_headers, [<<?LF>>, Header, Headers]};
parse_headers(<<?CR, ?LF, ?CR, ?LF, Rest/binary>>, Header, Headers) ->
    {ok, {[lists:reverse([?LF, ?CR | Header]) | Headers], Rest}};
parse_headers(<<?LF, ?LF, Rest/binary>>, Header, Headers) ->
    {ok, {[lists:reverse([?LF | Header]) | Headers], Rest}};
parse_headers(<<?CR, ?LF, Rest/binary>>, Header, Headers) ->
    parse_headers(Rest, [], [lists:reverse([?LF, ?CR | Header]) | Headers]);
parse_headers(<<?LF, Rest/binary>>, Header, Headers) ->
    parse_headers(Rest, [], [lists:reverse([?LF | Header]) | Headers]);
parse_headers(<<Octet, Rest/binary>>, Header, Headers) ->
    parse_headers(Rest, [Octet | Header], Headers).

handle_headers([], HTTPHeaders, Status) ->
    {ok, HTTPHeaders, Status};

handle_headers([CGIHeader | CGIHeaders], HTTPHeaders, Status) ->
    
    {FieldName, FieldValue} = httpd_response:split_header(CGIHeader, []),
   
    case FieldName of
	"content-type" ->
	    handle_headers(CGIHeaders,
			   [{FieldName, FieldValue} | HTTPHeaders], 
			   Status);
	"location" ->
	    case http_request:is_absolut_uri(FieldValue) of
		true ->
		    handle_headers(CGIHeaders, 
				       [{FieldName, FieldValue} | 
					HTTPHeaders], {302, "Redirect"});
		false ->
		    {proceed, FieldValue}
	    end;
	"status" ->
	    CodePhrase = 
		case httpd_util:split(FieldValue," ",2) of
		    {ok,[Code, Phrase]} ->
			{list_to_integer(Code), Phrase};
		    _ ->
			{200, "OK"}
		end,
	    handle_headers(CGIHeaders, HTTPHeaders, CodePhrase);
	_ -> %% Extension headers
	    handle_headers(CGIHeaders,
			   [{FieldName, FieldValue} | HTTPHeaders], Status)
    end.	

