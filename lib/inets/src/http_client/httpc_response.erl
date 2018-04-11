%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

-module(httpc_response).

-include_lib("inets/src/http_lib/http_internal.hrl").
-include("httpc_internal.hrl").

%% API
%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([parse/1, result/2, send/2, error/2, is_server_closing/1, 
	 stream_start/3]).

%% Callback API - used for example if the header/body is received a
%% little at a time on a socket. 
-export([parse_version/1, parse_status_code/1, parse_reason_phrase/1,
	 parse_headers/1, whole_body/1, whole_body/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

parse([Bin, MaxHeaderSize, Relaxed]) ->
    parse_version(Bin, [], MaxHeaderSize, [], Relaxed).

whole_body([Bin, Body, Length])  ->
    whole_body(<<Body/binary, Bin/binary>>, Length).

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
parse_version([Bin, Version, MaxHeaderSize, Result, Relaxed]) ->
    parse_version(Bin, Version, MaxHeaderSize, Result, Relaxed).

parse_status_code([Bin, Code, MaxHeaderSize, Result, Relaxed]) ->
    parse_status_code(Bin, Code, MaxHeaderSize, Result, Relaxed).

parse_reason_phrase([Bin, Rest, Phrase, MaxHeaderSize, Result, Relaxed]) ->
    parse_reason_phrase(<<Rest/binary, Bin/binary>>, Phrase, 
			MaxHeaderSize, Result, Relaxed).

parse_headers([Bin, Rest,Header, Headers, MaxHeaderSize, Result, Relaxed]) ->
    parse_headers(<<Rest/binary, Bin/binary>>, Header, Headers, 
		  MaxHeaderSize, Result, Relaxed).
    
whole_body(Body, Length) ->
    case size(Body) of
	N when (N < Length) andalso (N > 0)  ->
	    {?MODULE, whole_body, [Body, Length]};
	%% OBS!  The Server may close the connection to indicate that the
	%% whole body is now sent instead of sending a lengh
	%% indicator.In this case the lengh indicator will be
	%% -1.
	N when (N >= Length) andalso (Length >= 0) -> 
	    %% Potential trailing garbage will be thrown away in
	    %% format_response/1 Some servers may send a 100-continue
	    %% response without the client requesting it through an
	    %% expect header in this case the trailing bytes may be
	    %% part of the real response message.
	    {ok, Body};
	_ -> %% Length == -1
	    {?MODULE, whole_body, [Body, Length]} 
    end.

%%-------------------------------------------------------------------------
%% result(Response, Request) ->
%%   Response - {StatusLine, Headers, Body}
%%   Request - #request{}
%%                                   
%% Description: Checks the status code ...
%%-------------------------------------------------------------------------
result(Response = {{_, Code,_}, _, _}, 
       Request = #request{stream = Stream}) 
  when ((Code =:= 200) orelse (Code =:= 206)) andalso (Stream =/= none) ->
    stream_end(Response, Request);

result(Response = {{_,100,_}, _, _}, Request) ->
    status_continue(Response, Request);

%% In redirect loop
result(Response = {{_, Code, _}, _, _}, Request =
       #request{redircount = Redirects,
		settings = #http_options{autoredirect = true}}) 
  when ((Code div 100) =:= 3) andalso (Redirects > ?HTTP_MAX_REDIRECTS) ->
    transparent(Response, Request);

%% multiple choices 
result(Response = {{_, 300, _}, _, _}, 
       Request = #request{settings = 
			  #http_options{autoredirect = 
					true}}) ->
    redirect(Response, Request);

result(Response = {{_, Code, _}, _, _}, 
       Request = #request{settings =
              #http_options{autoredirect = true},
              method = post}) when (Code =:= 301) orelse
                           (Code =:= 302) orelse
                           (Code =:= 303) ->
    redirect(Response, Request#request{method = get});
result(Response = {{_, Code, _}, _, _}, 
       Request = #request{settings =
              #http_options{autoredirect = true},
              method = post}) when (Code =:= 307) ->
    redirect(Response, Request);
result(Response = {{_, Code, _}, _, _}, 
       Request = #request{settings = 
			  #http_options{autoredirect = true},
			  method = Method}) when (Code =:= 301) orelse
					       (Code =:= 302) orelse
					       (Code =:= 303) orelse
					       (Code =:= 307) ->
    case lists:member(Method, [get, head, options, trace]) of
    true ->
        redirect(Response, Request);
    false ->
        transparent(Response, Request)
    end;

result(Response = {{_,503,_}, _, _}, Request) ->
    status_service_unavailable(Response, Request);
result(Response = {{_,Code,_}, _, _}, Request) when (Code div 100) =:= 5 ->
    status_server_error_50x(Response, Request);

result(Response, Request) -> 
    transparent(Response, Request).

send(Receiver, Msg) when is_pid(Receiver) ->
    Receiver ! {http, Msg};
send(Receiver, Msg) when is_function(Receiver) ->
    (catch Receiver(Msg));
send({Module, Function, Args}, Msg) ->
    (catch apply(Module, Function, [Msg | Args])).


%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_version(<<>>, Version, MaxHeaderSize, Result, Relaxed) ->
    {?MODULE, parse_version, [Version, MaxHeaderSize,Result, Relaxed]};
parse_version(<<?SP, Rest/binary>>, Version, 
	      MaxHeaderSize, Result, Relaxed) ->
    case lists:reverse(Version) of
	"HTTP/" ++ _ = Newversion ->
	    parse_status_code(Rest, [], MaxHeaderSize,
			      [Newversion | Result], Relaxed);
	NewVersion ->
	    throw({error, {invalid_version, NewVersion}})
    end;	  

parse_version(<<Octet, Rest/binary>>, Version, 
	      MaxHeaderSize, Result, Relaxed) ->
    parse_version(Rest, [Octet | Version], MaxHeaderSize,Result, Relaxed).

parse_status_code(<<>>, StatusCodeStr, MaxHeaderSize, Result, Relaxed) -> 
    {?MODULE, parse_status_code, 
     [StatusCodeStr, MaxHeaderSize, Result, Relaxed]};

%% Some Apache servers has been known to leave out the reason phrase,
%% in relaxed mode we will allow this.
parse_status_code(<<?CR>> = Data, StatusCodeStr, 
		  MaxHeaderSize, Result, true) ->
    {?MODULE, parse_status_code, 
     [Data, StatusCodeStr, MaxHeaderSize, Result, true]};
parse_status_code(<<?LF>>, StatusCodeStr, 
		  MaxHeaderSize, Result, true) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_status_code(<<?CR, ?LF>>, StatusCodeStr, 
		      MaxHeaderSize, Result, true);

parse_status_code(<<?CR, ?LF, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize, Result, true) ->
    parse_headers(Rest, [], [], MaxHeaderSize,
 		  [" ", list_to_integer(lists:reverse(
                                          string:trim(StatusCodeStr)))
		   | Result], true); 

parse_status_code(<<?SP, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize, Result, Relaxed) ->
    parse_reason_phrase(Rest, [], MaxHeaderSize, 
			[list_to_integer(lists:reverse(StatusCodeStr)) | 
			 Result], Relaxed);

parse_status_code(<<Octet, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize,Result, Relaxed) ->
    parse_status_code(Rest, [Octet | StatusCodeStr], MaxHeaderSize, Result,
		      Relaxed).

parse_reason_phrase(<<>>, Phrase, MaxHeaderSize, Result, Relaxed) ->
    {?MODULE, parse_reason_phrase, 
     [<<>>, Phrase, MaxHeaderSize, Result, Relaxed]};

parse_reason_phrase(<<?CR, ?LF, ?LF, Body/binary>>, Phrase, 
  		    MaxHeaderSize, Result, Relaxed) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_reason_phrase(<<?CR, ?LF, ?CR, ?LF, Body/binary>>, Phrase, 
			MaxHeaderSize, Result, Relaxed); 

parse_reason_phrase(<<?CR, ?LF, ?CR, ?LF, Body/binary>>, Phrase, 
  		    _, Result, _) ->
    ResponseHeaderRcord = 
   	http_response:headers([], #http_response_h{}),
     {ok, list_to_tuple(
	    lists:reverse([Body, ResponseHeaderRcord | 
			   [lists:reverse(Phrase) | Result]]))};

parse_reason_phrase(<<?CR, ?LF, ?CR>> = Data, Phrase, MaxHeaderSize, Result,
		    Relaxed) ->
    {?MODULE, parse_reason_phrase, [Data, Phrase, MaxHeaderSize, Result],
     Relaxed};

parse_reason_phrase(<<?CR, ?LF>> = Data, Phrase, MaxHeaderSize, Result, 
		    Relaxed) ->
    {?MODULE, parse_reason_phrase, [Data, Phrase, MaxHeaderSize, Result,
				    Relaxed]};
parse_reason_phrase(<<?LF, Rest/binary>>, Phrase, 
 		    MaxHeaderSize, Result, Relaxed) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_reason_phrase(<<?CR, ?LF, Rest/binary>>, Phrase, 
			MaxHeaderSize, Result, Relaxed);
parse_reason_phrase(<<?CR, ?LF, Rest/binary>>, Phrase, 
 		    MaxHeaderSize, Result, Relaxed) ->
    parse_headers(Rest, [], [], MaxHeaderSize,
 		  [lists:reverse(Phrase) | Result], Relaxed); 
parse_reason_phrase(<<?LF>>, Phrase, MaxHeaderSize, Result, Relaxed) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_reason_phrase(<<?CR, ?LF>>, Phrase, MaxHeaderSize, Result, 
			Relaxed);
parse_reason_phrase(<<?CR>> = Data, Phrase, MaxHeaderSize, Result, Relaxed) ->
    {?MODULE, parse_reason_phrase, 
     [Data, Phrase, MaxHeaderSize, Result, Relaxed]};
parse_reason_phrase(<<Octet, Rest/binary>>, Phrase, MaxHeaderSize, Result, 
		    Relaxed) ->
    parse_reason_phrase(Rest, [Octet | Phrase], MaxHeaderSize, 
			Result, Relaxed).

parse_headers(<<>>, Header, Headers, MaxHeaderSize, Result, Relaxed) -> 
    {?MODULE, parse_headers, [<<>>, Header, Headers, MaxHeaderSize, Result,
			      Relaxed]};

parse_headers(<<?CR,?LF,?LF,Body/binary>>, Header, Headers,
	      MaxHeaderSize, Result, Relaxed) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers,
		  MaxHeaderSize, Result, Relaxed);

parse_headers(<<?LF,?LF,Body/binary>>, Header, Headers,
	      MaxHeaderSize, Result, Relaxed) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers,
		  MaxHeaderSize, Result, Relaxed);

parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers,
	      MaxHeaderSize, Result, Relaxed) ->
    HTTPHeaders = [lists:reverse(Header) | Headers],
    Length = lists:foldl(fun(H, Acc) -> length(H) + Acc end,
			   0, HTTPHeaders),
    case ((Length =< MaxHeaderSize) or (MaxHeaderSize == nolimit)) of
 	true ->   
	    ResponseHeaderRcord = 
		http_response:headers(HTTPHeaders, #http_response_h{}),

            %% RFC7230, Section 3.3.3
            %% If a message is received with both a Transfer-Encoding and a
            %% Content-Length header field, the Transfer-Encoding overrides the
            %% Content-Length. Such a message might indicate an attempt to
            %% perform request smuggling (Section 9.5) or response splitting
            %% (Section 9.4) and ought to be handled as an error. A sender MUST
            %% remove the received Content-Length field prior to forwarding such
            %% a message downstream.
            case ResponseHeaderRcord#http_response_h.'transfer-encoding' of
                undefined ->
                    {ok, list_to_tuple(
                           lists:reverse([Body, ResponseHeaderRcord | Result]))};
                Value ->
                    TransferEncoding = string:lowercase(Value),
                    ContentLength = ResponseHeaderRcord#http_response_h.'content-length',
                    if
                        %% Respond without error but remove Content-Length field in relaxed mode
                        (Relaxed =:= true)
                        andalso (TransferEncoding =:= "chunked")
                        andalso (ContentLength =/= "-1") ->
                            ResponseHeaderRcordFixed =
                                ResponseHeaderRcord#http_response_h{'content-length' = "-1"},
                            {ok, list_to_tuple(
                                   lists:reverse([Body, ResponseHeaderRcordFixed | Result]))};
                        %% Respond with error in default (not relaxed) mode
                        (Relaxed =:= false)
                        andalso (TransferEncoding =:= "chunked")
                        andalso (ContentLength =/= "-1") ->
                            throw({error, {headers_conflict, {'content-length',
                                                              'transfer-encoding'}}});
                        true  ->
                            {ok, list_to_tuple(
                                   lists:reverse([Body, ResponseHeaderRcord | Result]))}
                    end
            end;
 	false ->
	    throw({error, {header_too_long, MaxHeaderSize, 
			   MaxHeaderSize-Length}})
    end;
parse_headers(<<?CR,?LF,?CR>> = Data, Header, Headers, 
	      MaxHeaderSize, Result, Relaxed) ->
    {?MODULE, parse_headers, [Data, Header, Headers, 
			      MaxHeaderSize, Result, Relaxed]};
parse_headers(<<?CR,?LF>> = Data, Header, Headers, 
	      MaxHeaderSize, Result, Relaxed) ->
    {?MODULE, parse_headers, [Data, Header, Headers, MaxHeaderSize, 
			      Result, Relaxed]};
parse_headers(<<?CR,?LF, Octet, Rest/binary>>, Header, Headers,
	      MaxHeaderSize, Result, Relaxed) ->
    parse_headers(Rest, [Octet], 
		  [lists:reverse(Header) | Headers], MaxHeaderSize, 
		  Result, Relaxed);
parse_headers(<<?CR>> = Data, Header, Headers, 
	      MaxHeaderSize, Result, Relaxed) ->
    {?MODULE, parse_headers, [Data, Header, Headers, MaxHeaderSize, 
			      Result, Relaxed]};

parse_headers(<<?LF>>, Header, Headers, 
	      MaxHeaderSize, Result, Relaxed) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR, ?LF>>, Header, Headers, 
		  MaxHeaderSize, Result, Relaxed);

parse_headers(<<Octet, Rest/binary>>, Header, Headers,
	      MaxHeaderSize, Result, Relaxed) ->
    parse_headers(Rest, [Octet | Header], Headers, MaxHeaderSize, 
		  Result, Relaxed).


%% RFC2616, Section 10.1.1
%% Note:
%% - Only act on the 100 status if the request included the
%%   "Expect:100-continue" header, otherwise just ignore this response.
status_continue(_, #request{headers = 
			    #http_request_h{expect = "100-continue"}}) ->  
    continue;

status_continue({_,_, Data}, _) ->
    %% The data in the body in this case is actually part of the real
    %% response sent after the "fake" 100-continue.
    {ignore, Data}.

status_service_unavailable(Response = {_, Headers, _}, Request) ->
    case Headers#http_response_h.'retry-after' of 
	undefined ->
	    status_server_error_50x(Response, Request);
	Time when (length(Time) < 3) -> % Wait only 99 s or less 
	    NewTime = list_to_integer(Time) * 1000, % time in ms
	    {_, Data} =  format_response(Response),
	    {retry, {NewTime, Request}, Data};
	_ ->
	    status_server_error_50x(Response, Request)
    end.

status_server_error_50x(Response, Request) ->
    {Msg, _} =  format_response(Response),
    {stop, {Request#request.id, Msg}}.


redirect(Response = {_, Headers, _}, Request) ->
    {_, Data} =  format_response(Response),
    case Headers#http_response_h.location of
        undefined ->
            transparent(Response, Request);
        RedirUrl ->
            Brackets = Request#request.ipv6_host_with_brackets,
            case uri_string:parse(RedirUrl) of
                {error, Reason, _} ->
                    {ok, error(Request, Reason), Data};
                %% Automatic redirection
                URI ->
                    {Host, Port0} = Request#request.address,
                    Port = maybe_to_integer(Port0),
                    Path = Request#request.path,
                    Scheme = atom_to_list(Request#request.scheme),
                    Query = Request#request.pquery,
                    URIMap = resolve_uri(Scheme, Host, Port, Path, Query, URI),
                    TScheme = list_to_atom(maps:get(scheme, URIMap)),
                    THost = http_util:maybe_add_brackets(maps:get(host, URIMap), Brackets),
                    TPort = maps:get(port, URIMap),
                    TPath = maps:get(path, URIMap),
                    TQuery = maps:get(query, URIMap, ""),
                    NewURI = uri_string:normalize(
                               uri_string:recompose(URIMap)),
                    HostPort = http_request:normalize_host(TScheme, THost, TPort),
                    NewHeaders =
                        (Request#request.headers)#http_request_h{host = HostPort},
                    NewRequest =
                        Request#request{redircount =
                                            Request#request.redircount+1,
                                        scheme = TScheme,
                                        headers = NewHeaders,
                                        address = {THost,TPort},
                                        path = TPath,
                                        pquery = TQuery,
                                        abs_uri = NewURI},
                    {redirect, NewRequest, Data}
            end
    end.


%% RFC3986 - 5.2.2.  Transform References
resolve_uri(Scheme, Host, Port, Path, Query, URI) ->
    resolve_uri(Scheme, Host, Port, Path, Query, URI, #{}).
%%
resolve_uri(Scheme, Host, Port, Path, Query, URI, Map0) ->
    case maps:is_key(scheme, URI) of
        true ->
            Port = get_port(URI),
            maybe_add_query(
              Map0#{scheme => maps:get(scheme, URI),
                   host => maps:get(host, URI),
                   port => Port,
                   path => maps:get(path, URI)},
              URI);
        false ->
            Map = Map0#{scheme => Scheme},
            resolve_authority(Host, Port, Path, Query, URI, Map)
    end.


get_port(URI) ->
    Scheme = maps:get(scheme, URI),
    case maps:get(port, URI, undefined) of
        undefined ->
            get_default_port(Scheme);
        Port ->
            Port
    end.


get_default_port("http") ->
    80;
get_default_port("https") ->
    443.


resolve_authority(Host, Port, Path, Query, RelURI, Map) ->
    case maps:is_key(host, RelURI) of
        true ->
            Port = get_port(RelURI),
            maybe_add_query(
              Map#{host => maps:get(host, RelURI),
                   port => Port,
                   path => maps:get(path, RelURI)},
              RelURI);
        false ->
            Map1 = Map#{host => Host,
                    port => Port},
            resolve_path(Path, Query, RelURI, Map1)
    end.


maybe_add_query(Map, RelURI) ->
     case maps:is_key(query, RelURI) of
         true ->
             Map#{query => maps:get(query, RelURI)};
         false ->
             Map
         end.


resolve_path(Path, Query, RelURI, Map) ->
    case maps:is_key(path, RelURI) of
        true ->
            Path1 = calculate_path(Path,  maps:get(path, RelURI)),
            maybe_add_query(
              Map#{path => Path1},
              RelURI);
        false ->
            Map1 = Map#{path => Path},
            resolve_query(Query, RelURI, Map1)
    end.


calculate_path(BaseP, RelP) ->
    case starts_with_slash(RelP) of
        true ->
            RelP;
        false ->
            merge_paths(BaseP, RelP)
    end.


starts_with_slash([$/|_]) ->
    true;
starts_with_slash(<<$/,_/binary>>) ->
    true;
starts_with_slash(_) ->
    false.


%% RFC3986 - 5.2.3.  Merge Paths
merge_paths("", RelP) ->
    [$/|RelP];
merge_paths(BaseP, RelP) when is_list(BaseP) ->
    do_merge_paths(lists:reverse(BaseP), RelP);
merge_paths(BaseP, RelP) when is_binary(BaseP) ->
    B = binary_to_list(BaseP),
    R = binary_to_list(RelP),
    Res = merge_paths(B, R),
    list_to_binary(Res).


do_merge_paths([$/|_] = L, RelP) ->
    lists:reverse(L) ++ RelP;
do_merge_paths([_|T], RelP) ->
    do_merge_paths(T, RelP).


resolve_query(Query, RelURI, Map) ->
    case maps:is_key(query, RelURI) of
        true ->
            Map#{query => maps:get(query, RelURI)};
        false ->
            Map#{query => Query}
    end.


maybe_to_integer(Port) when is_list(Port) ->
    {Port1, _} = string:to_integer(Port),
    Port1;
maybe_to_integer(Port) when is_integer(Port) ->
    Port.


error(#request{id = Id}, Reason) ->
    {Id, {error, Reason}}.

transparent(Response, Request) ->    
    {Msg, Data} =  format_response(Response),
    {ok, {Request#request.id, Msg}, Data}.

stream_start(Headers, Request, ignore) ->
    {Request#request.id, stream_start, http_response:header_list(Headers)};

stream_start(Headers, Request, Pid) ->
    {Request#request.id, stream_start, 
     http_response:header_list(Headers), Pid}.

stream_end(Response, Request = #request{stream = Self}) 
  when (Self =:= self) orelse (Self =:= {self, once}) -> 
    {{_, Headers, _}, Data} =  format_response(Response),
    {ok, {Request#request.id, stream_end, Headers}, Data};

stream_end(Response, Request) ->
    {_, Data} =  format_response(Response),
    {ok, {Request#request.id, saved_to_file}, Data}.

is_server_closing(Headers) when is_record(Headers, http_response_h) ->
    case Headers#http_response_h.connection of
	"close" ->
	    true;
	_ ->
	    false
    end.

format_response({{"HTTP/0.9", _, _} = StatusLine, _, Body}) ->
    {{StatusLine, [], Body}, <<>>};
format_response({StatusLine, Headers, Body = <<>>}) ->
    {{StatusLine, http_response:header_list(Headers), Body}, <<>>};

format_response({StatusLine, Headers, Body}) ->
    Length = list_to_integer(Headers#http_response_h.'content-length'),
    {NewBody, Data} = 
	case Length of
	    -1 -> % When no length indicator is provided
		{Body, <<>>};
	    Length when (Length =< size(Body)) ->
		<<BodyThisReq:Length/binary, Next/binary>> = Body,
		{BodyThisReq, Next};
	    _ -> %% Connection prematurely ended. 
		{Body, <<>>}
	end,
    {{StatusLine, http_response:header_list(Headers), NewBody}, Data}.
