%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2015. All Rights Reserved.
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

-include("http_internal.hrl").

-export([headers/2, http_headers/1, is_absolut_uri/1, key_value/1, normalize_host/3]).


key_value(KeyValueStr) ->
    case lists:splitwith(fun($:) -> false; (_) -> true end, KeyValueStr) of
	{Key, [$: | Value]} ->
	    {http_util:to_lower(string:strip(Key)),  string:strip(Value)};
	{_, []} -> 
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

key_value_str(Key = 'cache-control', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'cache-control');
key_value_str(Key = connection, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.connection);
key_value_str(Key = date, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.date);
key_value_str(Key = pragma, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.pragma);
key_value_str(Key = trailer, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.trailer);
key_value_str(Key = 'transfer-encoding', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'transfer-encoding');
key_value_str(Key = upgrade, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.upgrade);
key_value_str(Key = via, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.via);
key_value_str(Key = warning, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.warning);
key_value_str(Key = accept, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.accept);
key_value_str(Key = 'accept-charset', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'accept-charset');
key_value_str(Key = 'accept-encoding', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'accept-encoding');
key_value_str(Key = 'accept-language', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'accept-language');
key_value_str(Key = authorization, Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.authorization);
key_value_str(Key = expect, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.expect);
key_value_str(Key = from, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.from);
key_value_str(Key = host, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.host);
key_value_str(Key = 'if-match', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'if-match');
key_value_str(Key = 'if-modified-since', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'if-modified-since');
key_value_str(Key = 'if-none-match', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'if-none-match');
key_value_str(Key = 'if-range', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'if-range');
key_value_str(Key = 'if-unmodified-since', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'if-unmodified-since');
key_value_str(Key = 'max-forwards', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'max-forwards');
key_value_str(Key = 'proxy-authorization', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'proxy-authorization');
key_value_str(Key = range, Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.range);
key_value_str(Key = referer, Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.referer);
key_value_str(Key = te, Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.te);
key_value_str(Key = 'user-agent', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'user-agent');
key_value_str(Key = allow, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.allow);
key_value_str(Key = 'content-encoding', Headers) ->
    key_value_str(atom_to_list(Key), 
		    Headers#http_request_h.'content-encoding');
key_value_str(Key = 'content-language', Headers) ->
    key_value_str(atom_to_list(Key), 
		    Headers#http_request_h.'content-language');
key_value_str(Key = 'content-length', Headers) ->
    key_value_str(atom_to_list(Key), 
		    Headers#http_request_h.'content-length');
key_value_str(Key = 'content-location', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'content-location');
key_value_str(Key = 'content-md5', Headers) ->
    key_value_str(atom_to_list(Key),
		    Headers#http_request_h.'content-md5');
key_value_str(Key = 'content-range', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'content-range');
key_value_str(Key = 'content-type', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'content-type');
key_value_str(Key = expires, Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.expires);
key_value_str(Key = 'last-modified', Headers) ->
    key_value_str(atom_to_list(Key), Headers#http_request_h.'last-modified');
key_value_str(_, undefined) ->
    undefined;
key_value_str(Key, Value)  ->
    Key ++ ": " ++ Value ++ ?CRLF.

headers_other([], Headers) ->
    Headers;
headers_other([{Key,Value} | Rest], Headers) ->
    Header = Key ++ ": " ++ Value ++ ?CRLF,
    headers_other(Rest, [Header | Headers]).
