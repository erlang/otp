%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2024. All Rights Reserved.
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

-module(http_request).
-moduledoc false.

-include("http_internal.hrl").

-export([headers/2, http_headers/1, is_absolut_uri/1, key_value/1, normalize_host/3]).


key_value(KeyValueStr) ->
    case lists:splitwith(fun($:) -> false; (_) -> true end, KeyValueStr) of
	{Key, [$: | Value]} when Key =/= [] ->
            %% RFC 7230 - 3.2.4 ... No whitespace is allowed between the header field-name and colon. 
            case string:strip(Key, right) of
                Key ->
                    {http_util:to_lower(string:strip(Key, left)),  string:strip(Value)};
                 _ ->
                    %% Ignore invalid header
                    undefined
            end;
	{_, []} -> 
	    undefined;
        _ ->
            undefined 
    end.
%%-------------------------------------------------------------------------
%% headers(HeaderList, #http_request_h{}) -> #http_request_h{}
%%   HeaderList - ["HeaderField:Value"]     	
%%   HeaderField - string()
%%   Value - string()	
%%                                   
%% Description: Creates a http_request_h-record used internally to
%%              handle http-headers.
%%-------------------------------------------------------------------------
headers([], Headers) ->
    Headers;
headers([{Key, Value} | Tail], Headers) ->  
    headers(Tail, headers(Key, Value, Headers));
headers([undefined], Headers) -> 
    Headers;
headers(KeyValues, Headers) -> 
    headers([key_value(KeyValue) || KeyValue <-  KeyValues], Headers).

%%-------------------------------------------------------------------------
%% headers(#http_request_h{}) -> HeaderList
%%   HeaderList - ["HeaderField:Value"]     	
%%   HeaderField - string()
%%   Value - string()	
%%                                   
%% Description: Creates a HTTP header string.
%%-------------------------------------------------------------------------
http_headers(Headers = #http_request_h{other = Other}) ->
    HeaderFields = record_info(fields, http_request_h) -- [other],
    HeaderStr = lists:foldl(fun(Key, Acc) -> 
				    case key_value_str(Key, Headers) of
					undefined ->
					    Acc;
					Str ->
					    [Str | Acc]
				    end
			    end,
			    [], HeaderFields),
    
    lists:flatten([HeaderStr | headers_other(Other, [])]).

%%-------------------------------------------------------------------------
%% is_absolut_uri(URI) -> true | false
%%   URI - string()	
%%                                   
%% Description: Checks if an URI is absolute or relative
%%-------------------------------------------------------------------------
is_absolut_uri("http://" ++ _) ->
    true;
is_absolut_uri("https://" ++ _) ->
    true;
is_absolut_uri(_) ->
    false.

%%-------------------------------------------------------------------------
%% normalize_host(Scheme, Host, Port) -> string()
%%   Scheme - http | https
%%   Host - string()
%%   Port - integer()
%%
%% Description: returns a normalized Host header value, with the port
%% number omitted for well-known ports
%%-------------------------------------------------------------------------
normalize_host(https, Host, 443 = _Port) ->
    Host;
normalize_host(http, Host, 80 = _Port) ->
    Host;
normalize_host(_Scheme, Host, Port) ->
    Host ++ ":" ++ integer_to_list(Port).

%%%========================================================================
%%% Internal functions
%%%========================================================================

%%% --- Request headers
headers("accept", Value, Headers) ->
    Headers#http_request_h{accept = Value};
headers("accept-charset", Value, Headers) ->
    Headers#http_request_h{'accept-charset' = Value};
headers("accept-encoding", Value, Headers) ->
    Headers#http_request_h{'accept-encoding' = Value};
headers("accept-language", Value, Headers) ->
    Headers#http_request_h{'accept-language' = Value};
headers("authorization", Value, Headers) ->
    Headers#http_request_h{authorization = Value};
headers("expect", Value, Headers) ->
    Headers#http_request_h{expect = Value};
headers("from", Value, Headers) ->
    Headers#http_request_h{from = Value};
headers("host", Value, Headers) ->
    Headers#http_request_h{host = Value};
headers("if-match", Value, Headers) ->
    Headers#http_request_h{'if-match' = Value};
headers("if-modified-since", Value, Headers) ->
    Headers#http_request_h{'if-modified-since' = Value};
headers("if-none-match", Value, Headers) ->
    Headers#http_request_h{'if-none-match' = Value};
headers("if-range", Value, Headers) ->
    Headers#http_request_h{'if-range' = Value};
headers("if-unmodified-since", Value, Headers) ->
    Headers#http_request_h{'if-unmodified-since' = Value};
headers("max-forwards", Value, Headers) ->
    Headers#http_request_h{'max-forwards' = Value};
headers("proxy-authorization", Value, Headers) ->
    Headers#http_request_h{'proxy-authorization' = Value};
headers("range", Value, Headers) ->
    Headers#http_request_h{range = Value};
headers("referer", Value, Headers) ->
    Headers#http_request_h{referer = Value};
headers("te", Value, Headers) ->
    Headers#http_request_h{te = Value};
headers("user-agent", Value, Headers) ->
    Headers#http_request_h{'user-agent' = Value};

%% General-Headers
headers("cache-control", Value, Headers) ->
    Headers#http_request_h{'cache-control' = Value};
headers("connection", Value, Headers) ->
    Headers#http_request_h{connection = Value};
headers("date", Value, Headers) ->
    Headers#http_request_h{date = Value};
headers("pragma", Value, Headers) ->
    Headers#http_request_h{pragma = Value};
headers("trailer", Value, Headers) ->
    Headers#http_request_h{trailer = Value};
headers("transfer-encoding", Value, Headers) ->
    Headers#http_request_h{'transfer-encoding' = Value};
headers("upgrade", Value, Headers) ->		
    Headers#http_request_h{upgrade = Value};
headers("via", Value, Headers) ->
    Headers#http_request_h{via = Value};
headers("warning", Value, Headers) ->
    Headers#http_request_h{warning = Value};

%% Entity header
headers("allow", Value, Headers) ->
    Headers#http_request_h{allow = Value};
headers("content-encoding", Value, Headers) ->
    Headers#http_request_h{'content-encoding' = Value};
headers("content-language", Value, Headers) ->
    Headers#http_request_h{'content-language' = Value};
headers("content-length", Value, Headers) ->
    Headers#http_request_h{'content-length' = Value};
headers("content-location", Value, Headers) ->
    Headers#http_request_h{'content-location' = Value};
headers("content-md5", Value, Headers) ->
    Headers#http_request_h{'content-md5' = Value};
headers("content-range", Value, Headers) ->
    Headers#http_request_h{'content-range' = Value};
headers("content-type", Value, Headers) ->
    Headers#http_request_h{'content-type' = Value};
headers("expires", Value, Headers) ->
    Headers#http_request_h{expires = Value};
headers("last-modified", Value, Headers) ->
    Headers#http_request_h{'last-modified' = Value};
headers(Key, Value, Headers) ->
    Headers#http_request_h{other=
			   [{Key, Value} | Headers#http_request_h.other]}.

key_value_str(Key, Headers) ->
    case key_value(Key, Headers) of
        undefined ->
            undefined;
        Value ->
            mk_key_value_str(atom_to_list(Key), Value)
    end.

key_value('cache-control', Headers) ->
    Headers#http_request_h.'cache-control';
key_value(connection, Headers) ->
    Headers#http_request_h.connection;
key_value(date, Headers) ->
    Headers#http_request_h.date;
key_value(pragma, Headers) ->
    Headers#http_request_h.pragma;
key_value(trailer, Headers) ->
    Headers#http_request_h.trailer;
key_value('transfer-encoding', Headers) ->
    Headers#http_request_h.'transfer-encoding';
key_value(upgrade, Headers) ->
    Headers#http_request_h.upgrade;
key_value(via, Headers) ->
    Headers#http_request_h.via;
key_value(warning, Headers) ->
    Headers#http_request_h.warning;
key_value(accept, Headers) ->
    Headers#http_request_h.accept;
key_value('accept-charset', Headers) ->
    Headers#http_request_h.'accept-charset';
key_value('accept-encoding', Headers) ->
    Headers#http_request_h.'accept-encoding';
key_value('accept-language', Headers) ->
    Headers#http_request_h.'accept-language';
key_value(authorization, Headers) ->
    Headers#http_request_h.authorization;
key_value(expect, Headers) ->
    Headers#http_request_h.expect;
key_value(from, Headers) ->
    Headers#http_request_h.from;
key_value(host, Headers) ->
    Headers#http_request_h.host;
key_value('if-match', Headers) ->
    Headers#http_request_h.'if-match';
key_value('if-modified-since', Headers) ->
    Headers#http_request_h.'if-modified-since';
key_value('if-none-match', Headers) ->
    Headers#http_request_h.'if-none-match';
key_value('if-range', Headers) ->
    Headers#http_request_h.'if-range';
key_value('if-unmodified-since', Headers) ->
    Headers#http_request_h.'if-unmodified-since';
key_value('max-forwards', Headers) ->
    Headers#http_request_h.'max-forwards';
key_value('proxy-authorization', Headers) ->
    Headers#http_request_h.'proxy-authorization';
key_value(range, Headers) ->
    Headers#http_request_h.range;
key_value(referer, Headers) ->
    Headers#http_request_h.referer;
key_value(te, Headers) ->
    Headers#http_request_h.te;
key_value('user-agent', Headers) ->
    Headers#http_request_h.'user-agent';
key_value(allow, Headers) ->
    Headers#http_request_h.allow;
key_value('content-encoding', Headers) ->
    Headers#http_request_h.'content-encoding';
key_value('content-language', Headers) ->
    Headers#http_request_h.'content-language';
key_value('content-length', Headers) ->
    Headers#http_request_h.'content-length';
key_value('content-location', Headers) ->
    Headers#http_request_h.'content-location';
key_value('content-md5', Headers) ->
    Headers#http_request_h.'content-md5';
key_value('content-range', Headers) ->
    Headers#http_request_h.'content-range';
key_value('content-type', Headers) ->
    Headers#http_request_h.'content-type';
key_value(expires, Headers) ->
    Headers#http_request_h.expires;
key_value('last-modified', Headers) ->
    Headers#http_request_h.'last-modified'.

headers_other([], Headers) ->
    Headers;
headers_other([{Key, Value} | Rest], Headers) ->
    headers_other(Rest, [mk_key_value_str(Key, Value) | Headers]).

mk_key_value_str(Key, Value) ->
    try Key ++ ": " ++ value_to_list(Value) ++ ?CRLF of
        HeaderStr ->
            HeaderStr
    catch
        error:_ ->
            throw({invalid_header, {Key, Value}})
    end.

value_to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
value_to_list(List) when is_list(List) ->
    binary_to_list(iolist_to_binary(List)).
