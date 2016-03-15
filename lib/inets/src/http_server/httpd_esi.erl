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
-module(httpd_esi).

-export([parse_headers/1, handle_headers/1]).

-include_lib("inets/src/inets_app/inets_internal.hrl").


%%%=========================================================================
%%%  Internal application API 
%%%=========================================================================

%%--------------------------------------------------------------------------
%% parse_headers(Data) -> {Headers, Body}
%%
%% Data = string() | io_list()
%% Headers = string()
%% Body = io_list()
%%
%% Description: Parses <Data> and divides it to a header part and a
%% body part. Note that it is presumed that <Data> starts with a
%% string including "\r\n\r\n" if there is any header information
%% present. The returned headers will not contain the HTTP header body
%% delimiter \r\n. (All header, header delimiters are kept.)
%% Ex: ["Content-Type : text/html\r\n Connection : closing \r\n\r\n" | 
%% io_list()] -->  {"Content-Type : text/html\r\n Connection : closing \r\n",
%% io_list()}
%%--------------------------------------------------------------------------
parse_headers(Data) ->
    parse_headers(Data, []).

%%--------------------------------------------------------------------------
%% handle_headers(Headers) -> {ok, HTTPHeaders, StatusCode} |
%%                            {proceed, AbsPath}  
%%	Headers = string()   
%%	HTTPHeaders = [{HeaderField, HeaderValue}]
%%      HeaderField = string()
%%      HeaderValue = string() 
%%      StatusCode = integer()
%% 
%% Description: Transforms the plain HTTP header string data received
%% from the ESI program into a list of header values and an
%% appropriate HTTP status code. Note if a location header is present
%% the return value will be {proceed, AbsPath}
%%--------------------------------------------------------------------------
handle_headers("") ->
    {ok, [], 200};
handle_headers(Headers) ->
    NewHeaders = string:tokens(Headers, ?CRLF),
    handle_headers(NewHeaders, [], 200).

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_headers([], Acc) ->
    {[], lists:reverse(Acc)};
parse_headers([?CR, ?LF, ?CR, ?LF], Acc) ->
    {lists:reverse(Acc) ++ [?CR, ?LF], []};
parse_headers([?CR, ?LF, ?CR, ?LF | Rest], Acc) ->
    {lists:reverse(Acc) ++ [?CR, ?LF], Rest};
parse_headers([Char | Rest], Acc) ->
    parse_headers(Rest, [Char | Acc]).
 
handle_headers([], NewHeaders, StatusCode) ->
    {ok, NewHeaders, StatusCode};

handle_headers([Header | Headers], NewHeaders, StatusCode) -> 
    {FieldName, FieldValue} = httpd_response:split_header(Header, []),
    case FieldName of
	"location" ->
	    case http_request:is_absolut_uri(FieldValue) of
		true ->
		    handle_headers(Headers, 
				   [{FieldName, FieldValue} | NewHeaders], 
				   302);
		false ->
		    {proceed, FieldValue}
	    end;
	"status" ->
	    NewStatusCode = 
		case httpd_util:split(FieldValue," ",2) of
		    {ok,[Code,_]} ->
			list_to_integer(Code);
		    _ ->
			200
		end,
	    handle_headers(Headers, NewHeaders, NewStatusCode);
	_ -> 
	    handle_headers(Headers, 
			     [{FieldName, FieldValue}| NewHeaders], StatusCode)
    end.	
