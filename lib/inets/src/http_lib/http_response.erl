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

-module(http_response).

-include("http_internal.hrl").

-export([headers/2, header_list/1]).

%%-------------------------------------------------------------------------
%% headers(HeaderList, #http_response_h{}) -> #http_response_h{}
%%   HeaderList - ["HeaderField:Value"]     	
%%   HeaderField - string()
%%   Value - string()	
%%                                   
%% Description: Creates a http_response_h-record used internally to
%%              handle http-headers, assumes reversed list of headers
%%              to unfold multiline headers with obs-folds
%%-------------------------------------------------------------------------
headers(RevLines, Headers) ->
    fill_headers(RevLines, [], Headers).

%%-------------------------------------------------------------------------
%% headers(#http_response_h{}) -> HeaderList
%%   HeaderList - [{"HeaderField", Value"}]     	
%%   HeaderField - string()
%%   Value - string()	
%%                                   
%% Description: Creates a list of key value tuples from the #http_response_h  
%%              record, to be returned to the application programmer. We
%%              do not wish to make the application programmer dependent on
%%              our records.
%%-------------------------------------------------------------------------
header_list(Headers) ->
    HeaderFields = record_info(fields, http_response_h) -- [other],
    HeaderList = lists:foldl(fun(Key, Acc) -> 
				     case key_value_tuple(Key, Headers) of
					 undefined ->
					     Acc;
					 Tuple ->
					     [Tuple | Acc]
				     end
			     end,
			     [], HeaderFields),
    lists:reverse(HeaderList) ++ Headers#http_response_h.other.
%%%========================================================================
%%% Internal functions
%%%========================================================================
fill_headers([], _, Headers) ->
    Headers;
fill_headers([[]], _, Headers) ->
    Headers;
fill_headers([[Ch|HeaderFold]|Tail], Folded, Headers)
  when Ch == $\t; Ch == $\s ->
    fill_headers(Tail, [HeaderFold|Folded], Headers);
fill_headers([Header | Tail], Folded, Headers) ->
    Unfolded = unfold([Header|Folded]),
    {Key, [$: | Value]} =
	lists:splitwith(fun($:) -> false; (_) -> true end, Unfolded),
    fill_headers(Tail, [], headers(http_util:to_lower(string:strip(Key)),
				   string:strip(Value), Headers)).

unfold([L]) ->
    L;
unfold(Folded) ->
    string:join(Folded, " ").

headers("cache-control", Value, Headers) ->
    Headers#http_response_h{'cache-control'= Value};
headers("connection", Value, Headers) ->
    Headers#http_response_h{connection = Value};
headers("date", Value, Headers) ->
    Headers#http_response_h{date = Value};
headers("pragma", Value, Headers) ->
    Headers#http_response_h{pragma = Value};
headers("trailer", Value, Headers) ->
    Headers#http_response_h{trailer = Value};
headers("transfer-encoding", Value, Headers) ->
    Headers#http_response_h{'transfer-encoding' = Value};
headers("upgrade", Value, Headers) ->
    Headers#http_response_h{upgrade = Value};
headers("via", Value, Headers) ->
    Headers#http_response_h{via = Value};
headers("warning", Value, Headers) ->
    Headers#http_response_h{warning = Value};
headers("accept-ranges", Value, Headers) ->
    Headers#http_response_h{'accept-ranges' = Value};
headers("age", Value, Headers) ->
    Headers#http_response_h{age = Value};
headers("etag", Value, Headers) ->
    Headers#http_response_h{etag = Value};
headers("location", Value, Headers) ->
    Headers#http_response_h{location = Value};
headers("proxy-authenticate", Value, Headers) ->
    Headers#http_response_h{'proxy-authenticate' = Value};
headers("retry-after", Value, Headers) ->
    Headers#http_response_h{'retry-after' = Value};
headers("server", Value, Headers) ->
    Headers#http_response_h{server = Value};
headers("vary", Value, Headers) ->
    Headers#http_response_h{vary = Value};
headers("www-authenticate", Value, Headers) ->
    Headers#http_response_h{'www-authenticate' = Value};
headers("allow", Value, Headers) ->
    Headers#http_response_h{allow = Value};
headers("content-encoding", Value, Headers) ->
    Headers#http_response_h{'content-encoding' = Value};
headers("content-language", Value, Headers) ->
    Headers#http_response_h{'content-language' = Value};
headers("content-length", Value, Headers) ->
    Headers#http_response_h{'content-length' = Value};
headers("content-location", Value, Headers) ->
    Headers#http_response_h{'content-location' = Value};
headers("content-md5", Value, Headers) ->
    Headers#http_response_h{'content-md5' = Value};
headers("content-range", Value, Headers) ->
    Headers#http_response_h{'content-range' = Value};
headers("content-type", Value, Headers) ->
    Headers#http_response_h{'content-type' = Value};
headers("expires", Value, Headers) ->
    Headers#http_response_h{expires = Value};
headers("last-modified", Value, Headers) ->
    Headers#http_response_h{'last-modified' = Value};
headers(Key, Value, Headers) ->
    Headers#http_response_h{other=
			    [{Key, Value} | Headers#http_response_h.other]}.


key_value_tuple(Key = 'cache-control', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'cache-control');
key_value_tuple(Key = connection, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.connection);
key_value_tuple(Key = date, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.date);
key_value_tuple(Key = pragma, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.pragma);
key_value_tuple(Key = trailer, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.trailer);
key_value_tuple(Key ='transfer-encoding', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'transfer-encoding');
key_value_tuple(Key = upgrade, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.upgrade);
key_value_tuple(Key = via, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.via);
key_value_tuple(Key = warning, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.warning);
key_value_tuple(Key = 'accept-ranges', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'accept-ranges');
key_value_tuple(Key = age, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.age);
key_value_tuple(Key = etag, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.etag);
key_value_tuple(Key = location, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.location);
key_value_tuple(Key = 'proxy-authenticate', Headers) ->
    key_value_tuple(atom_to_list(Key),
		    Headers#http_response_h.'proxy-authenticate');
key_value_tuple(Key = 'retry-after', Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.'retry-after');
key_value_tuple(Key = server, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.server);
key_value_tuple(Key = vary, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.vary);
key_value_tuple(Key = 'www-authenticate', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'www-authenticate');
key_value_tuple(Key = allow, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.allow);
key_value_tuple(Key = 'content-encoding', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'content-encoding');
key_value_tuple(Key = 'content-language', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'content-language');
key_value_tuple(Key = 'content-length', Headers) ->
    case Headers#http_response_h.'content-length' of
	"-1" ->
	    undefined;
	_ -> 
	    key_value_tuple(atom_to_list(Key), 
			    Headers#http_response_h.'content-length')
    end;
key_value_tuple(Key = 'content-location', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'content-location');
key_value_tuple(Key = 'content-md5', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'content-md5');
key_value_tuple(Key = 'content-range', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'content-range');
key_value_tuple(Key = 'content-type', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'content-type');
key_value_tuple(Key = expires, Headers) ->
    key_value_tuple(atom_to_list(Key), Headers#http_response_h.expires);
key_value_tuple(Key = 'last-modified', Headers) ->
    key_value_tuple(atom_to_list(Key), 
		    Headers#http_response_h.'last-modified');
key_value_tuple(_, undefined) ->
    undefined;
key_value_tuple(Key, Value) ->
    {Key, Value}.
