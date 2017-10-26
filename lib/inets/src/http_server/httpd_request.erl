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

-module(httpd_request).

-include_lib("inets/src/http_lib/http_internal.hrl").
-include("httpd.hrl").
-include("httpd_internal.hrl").

-export([
	 parse/1, 
	 whole_body/2, 
	 validate/3, 
	 update_mod_data/5,
	 body_data/2
	]).

%% Callback API - used for example if the header/body is received a
%% little at a time on a socket. 
-export([
	 parse_method/1, parse_uri/1, parse_version/1, parse_headers/1,
	 whole_body/1, body_chunk_first/3, body_chunk/3, add_chunk/1
	]).


%%%=========================================================================
%%%  Internal application API
%%%=========================================================================
parse([Bin, Options]) ->
    ?hdrt("parse", [{bin, Bin}, {max_sizes, Options}]),    
    parse_method(Bin, [], 0, proplists:get_value(max_method, Options), Options, []);
parse(Unknown) ->
    ?hdrt("parse", [{unknown, Unknown}]),
    exit({bad_args, Unknown}).

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
parse_method([Bin, Method, Current, Max, Options, Result]) ->
    parse_method(Bin, Method, Current, Max, Options, Result).

parse_uri([Bin, URI, Current, Max, Options, Result]) ->
    parse_uri(Bin, URI, Current, Max, Options, Result).

parse_version([Bin, Rest, Version, Current, Max, Options, Result]) ->
    parse_version(<<Rest/binary, Bin/binary>>, Version, Current, Max, Options, 
		  Result).

parse_headers([Bin, Rest, Header, Headers, Current, Max, Options, Result]) ->
    parse_headers(<<Rest/binary, Bin/binary>>, 
		  Header, Headers, Current, Max, Options, Result).

whole_body([Bin, Body, Length])  ->
    whole_body(<<Body/binary, Bin/binary>>, Length).    


%% Separate the body for this request from a possible piplined new 
%% request and convert the body data to "string" format.
body_data(Headers, Body) ->    
    ContentLength = list_to_integer(Headers#http_request_h.'content-length'),
    case size(Body) - ContentLength of
 	0 ->
 	    {Body, <<>>};
 	_ ->
 	    <<BodyThisReq:ContentLength/binary, Next/binary>> = Body,   
 	    {BodyThisReq, Next}
    end.

%%-------------------------------------------------------------------------
%% validate(Method, Uri, Version) -> ok | {error, {bad_request, Reason} |
%%			     {error, {not_supported, {Method, Uri, Version}}
%%      Method = "HEAD" | "GET" | "POST" | "PATCH" | "TRACE" | "PUT"
%%               | "DELETE"
%%      Uri = uri()
%%      Version = "HTTP/N.M"      
%% Description: Checks that HTTP-request-line is valid.
%%------------------------------------------------------------------------- 
validate("HEAD", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("GET", Uri, []) -> %% Simple HTTP/0.9 
    validate_uri(Uri);
validate("GET", Uri, "HTTP/0.9") ->
    validate_uri(Uri);
validate("GET", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("PUT", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("DELETE", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("POST", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("PATCH", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("TRACE", Uri, "HTTP/1." ++ N) when hd(N) >= $1 ->
    validate_uri(Uri);
validate(Method, Uri, Version) ->
    case validate_version(Version) of
	true ->
	    {error, {not_supported, {Method, Uri, Version}}};
	false ->
	    {error, {bad_version, Version}}
    end.
%%----------------------------------------------------------------------
%% The request is passed through the server as a record of type mod 
%% create it.
%% ----------------------------------------------------------------------
update_mod_data(ModData, Method, RequestURI, HTTPVersion, Headers)-> 
    PersistentConn = get_persistens(HTTPVersion, Headers, 
				    ModData#mod.config_db),
    {ok, ModData#mod{data = [],
		     method = Method,
		     absolute_uri = format_absolute_uri(RequestURI, 
							Headers),
		     request_uri = format_request_uri(RequestURI),
		     http_version = HTTPVersion,
		     request_line = Method ++ " " ++ RequestURI ++ 
		     " " ++ HTTPVersion,
		     parsed_header = Headers,
		     connection = PersistentConn}}.

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_method(<<>>, Method, Current, Max, Options, Result) ->
    {?MODULE, parse_method, [Method, Current, Max, Options, Result]};
parse_method(<<?SP, Rest/binary>>, Method, _Current, _Max, Options, Result) ->
    parse_uri(Rest, [], 0,  proplists:get_value(max_uri, Options), Options,
	      [string:strip(lists:reverse(Method)) | Result]);
parse_method(<<Octet, Rest/binary>>, Method, Current, Max, Options, Result) when Current =< Max ->
    parse_method(Rest, [Octet | Method], Current + 1, Max, Options, Result);
parse_method(_, _, _, Max, _, _) ->
    %% We do not know the version of the client as it comes after the
    %% method send the lowest version in the response so that the client
    %% will be able to handle it.
    {error, {size_error, Max, 413, "Method unreasonably long"}, lowest_version()}.

parse_uri(_, _, Current, MaxURI, _, _) 
  when (Current > MaxURI) andalso (MaxURI =/= nolimit) -> 
    %% We do not know the version of the client as it comes after the
    %% uri send the lowest version in the response so that the client
    %% will be able to handle it.
    {error, {size_error, MaxURI, 414, "URI unreasonably long"},lowest_version()};
parse_uri(<<>>, URI, Current, Max, Options, Result) ->
    {?MODULE, parse_uri, [URI, Current, Max, Options, Result]};
parse_uri(<<?SP, Rest/binary>>, URI, _, _, Options, Result) -> 
    parse_version(Rest, [], 0, proplists:get_value(max_version, Options), Options, 
		  [string:strip(lists:reverse(URI)) | Result]);
%% Can happen if it is a simple HTTP/0.9 request e.i "GET /\r\n\r\n"
parse_uri(<<?CR, _Rest/binary>> = Data, URI, _, _, Options, Result) ->
    parse_version(Data, [], 0, proplists:get_value(max_version, Options), Options, 
		  [string:strip(lists:reverse(URI)) | Result]);
parse_uri(<<Octet, Rest/binary>>, URI, Current, Max, Options, Result) ->
    parse_uri(Rest, [Octet | URI], Current + 1, Max, Options, Result).

parse_version(<<>>, Version, Current, Max, Options, Result) ->
    {?MODULE, parse_version, [<<>>, Version, Current, Max, Options, Result]};
parse_version(<<?LF, Rest/binary>>, Version, Current, Max, Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_version(<<?CR, ?LF, Rest/binary>>, Version, Current, Max, Options, Result);
parse_version(<<?CR, ?LF, Rest/binary>>, Version, _, _,  Options, Result) ->
    parse_headers(Rest, [], [], 0, proplists:get_value(max_header, Options), Options, 
		  [string:strip(lists:reverse(Version)) | Result]);
parse_version(<<?CR>> = Data, Version, Current, Max, Options, Result) ->
    {?MODULE, parse_version, [Data, Version, Current, Max, Options, Result]};
parse_version(<<Octet, Rest/binary>>, Version, Current, Max, Options, Result)  when Current =< Max ->
    parse_version(Rest, [Octet | Version], Current + 1, Max, Options, Result);
parse_version(_, _, _, Max,_,_) ->
    {error, {size_error, Max, 413, "Version string unreasonably long"}, lowest_version()}.

parse_headers(_, _, _, Current, Max, _, Result) 
  when Max =/= nolimit andalso Current > Max -> 
    HttpVersion = lists:nth(3, lists:reverse(Result)),
    {error, {size_error, Max, 413, "Headers unreasonably long"}, HttpVersion};

parse_headers(<<>>, Header, Headers, Current, Max, Options, Result) ->
    {?MODULE, parse_headers, [<<>>, Header, Headers, Current, Max, 
			      Options, Result]};
parse_headers(<<?CR,?LF,?LF,Body/binary>>, [], [], Current, Max, Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, [], [], Current, Max,  
		  Options, Result);

parse_headers(<<?LF,?LF,Body/binary>>, [], [], Current, Max,  Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, [], [], Current, Max, 
		  Options, Result);

parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, [], [], _, _,  _, Result) ->
    NewResult = list_to_tuple(lists:reverse([Body, {#http_request_h{}, []} |
					     Result])),
    {ok, NewResult};
parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers, _, _,
	      Options, Result) ->
    Customize = proplists:get_value(customize, Options),
    case http_request:key_value(lists:reverse(Header)) of
	undefined -> %% Skip headers with missing :
	    FinalHeaders = lists:filtermap(fun(H) ->
						   httpd_custom:customize_headers(Customize, request_header, H)
					   end,
					   Headers),
	    {ok, list_to_tuple(lists:reverse([Body, {http_request:headers(FinalHeaders, #http_request_h{}), FinalHeaders} | Result]))};
	NewHeader ->
	    case check_header(NewHeader, Options) of 
		ok ->
		    FinalHeaders = lists:filtermap(fun(H) ->
							   httpd_custom:customize_headers(Customize, request_header, H)
						   end, [NewHeader | Headers]),
		    {ok, list_to_tuple(lists:reverse([Body, {http_request:headers(FinalHeaders, 
										  #http_request_h{}),  
							     FinalHeaders} | Result]))};
		
		{error, Reason} ->
		    HttpVersion = lists:nth(3, lists:reverse(Result)),
		    {error, Reason, HttpVersion}
	    end
    end;

parse_headers(<<?CR,?LF,?CR>> = Data, Header, Headers, Current, Max, 
	      Options, Result) ->
    {?MODULE, parse_headers, [Data, Header, Headers, Current, Max, 
			      Options, Result]};
parse_headers(<<?LF>>, [], [], Current, Max, Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF>>, [], [], Current, Max, Options, Result);

%% There where no headers, which is unlikely to happen.
parse_headers(<<?CR,?LF>>, [], [], _, _, _, Result) ->
     NewResult = list_to_tuple(lists:reverse([<<>>, {#http_request_h{}, []} |
					      Result])),
    {ok, NewResult};

parse_headers(<<?LF>>, Header, Headers, Current, Max,  
	      Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF>>, Header, Headers, Current, Max, Options, Result);

parse_headers(<<?CR,?LF>> = Data, Header, Headers, Current, Max, 
	      Options, Result) ->
    {?MODULE, parse_headers, [Data, Header, Headers, Current, Max, 
			      Options, Result]};
parse_headers(<<?LF, Octet, Rest/binary>>, Header, Headers, Current, Max,
	      Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR,?LF, Octet, Rest/binary>>, Header, Headers, Current, Max,
		  Options, Result); 
parse_headers(<<?CR,?LF, Octet, Rest/binary>>, Header, Headers, _, Max,
	      Options, Result) ->
    case http_request:key_value(lists:reverse(Header)) of
	undefined -> %% Skip headers with missing :
	    parse_headers(Rest, [Octet], Headers, 
			  0, Max, Options, Result);
	NewHeader ->
	    case check_header(NewHeader, Options) of 
		ok ->
		    parse_headers(Rest, [Octet], [NewHeader | Headers], 
				  0, Max, Options, Result);
		{error, Reason} ->
		    HttpVersion = lists:nth(3, lists:reverse(Result)),
		    {error, Reason, HttpVersion}
	    end
    end;
	
parse_headers(<<?CR>> = Data, Header, Headers, Current, Max,  
	      Options, Result) ->
    {?MODULE, parse_headers, [Data, Header, Headers, Current, Max, 
			      Options, Result]};
parse_headers(<<?LF>>, Header, Headers, Current,  Max, 
	      Options, Result) ->
    %% If ?CR is is missing RFC2616 section-19.3 
    parse_headers(<<?CR, ?LF>>, Header, Headers, Current, Max,  
		  Options, Result);

parse_headers(<<Octet, Rest/binary>>, Header, Headers, Current,
	      Max, Options, Result) ->
    parse_headers(Rest, [Octet | Header], Headers, Current + 1, Max,
		  Options, Result).

body_chunk_first(Body, 0 = Length, _) ->
    whole_body(Body, Length);
body_chunk_first(Body, Length, MaxChunk) ->
    case body_chunk(Body, Length, MaxChunk) of
        {ok, {last, NewBody}} ->
            {ok, NewBody};
        Other ->
            Other
    end.
%% Used to chunk non chunk decoded post/put data
add_chunk([<<>>, Body, Length, MaxChunk]) ->
    body_chunk(Body, Length, MaxChunk);
add_chunk([More, Body, Length, MaxChunk]) ->
    body_chunk(<<Body/binary, More/binary>>, Length, MaxChunk).

body_chunk(Body, Length, nolimit) ->
    whole_body(Body, Length); 
body_chunk(<<>> = Body, Length, MaxChunk) ->
    {ok, {continue, ?MODULE, add_chunk, [Body, Length, MaxChunk]}};

body_chunk(Body, Length, MaxChunk) when Length > MaxChunk ->
    case size(Body) >= MaxChunk of 
        true ->
            <<Chunk:MaxChunk/binary, Rest/binary>> = Body,
            {ok, {{continue, Chunk}, ?MODULE, add_chunk, [Rest, Length - MaxChunk, MaxChunk]}};
        false ->
            {ok, {continue, ?MODULE, add_chunk, [Body, Length, MaxChunk]}}
    end;
body_chunk(Body, Length, MaxChunk) ->
    case size(Body) of
        Length ->
            {ok, {last, Body}};
        _ ->
            {ok, {continue, ?MODULE, add_chunk, [Body, Length, MaxChunk]}}
    end.

whole_body(Body, Length) ->
    case size(Body) of
	N when N < Length, Length > 0 ->
	  {?MODULE, add_chunk, [Body, Length, nolimit]};
	N when N >= Length, Length >= 0 ->  
	    %% When a client uses pipelining trailing data
	    %% may be part of the next request!
	    %% Trailing data will be separated from 
	    %% the actual body in body_data/2.
	    {ok, Body}
	end.

%% Prevent people from trying to access directories/files
%% relative to the ServerRoot.
validate_uri(RequestURI) ->
    UriNoQueryNoHex = 
	case string:str(RequestURI, "?") of
	    0 ->
		(catch http_uri:decode(RequestURI));
	    Ndx ->
		(catch http_uri:decode(string:left(RequestURI, Ndx)))
	end,
    case UriNoQueryNoHex of
	{'EXIT', _Reason} ->
	    {error, {bad_request, {malformed_syntax, RequestURI}}};
	_ ->
	    Path  = format_request_uri(UriNoQueryNoHex),
	    Path2 = [X||X<-string:tokens(Path, "/"),X=/="."], %% OTP-5938
	    validate_path(Path2, 0, RequestURI)
    end.

validate_path([], _, _) ->
    ok;
validate_path([".." | _], 0, RequestURI) ->
    {error, {bad_request, {forbidden, RequestURI}}};
validate_path([".." | Rest], N, RequestURI) ->
    validate_path(Rest, N - 1, RequestURI);
validate_path([_ | Rest], N, RequestURI) ->
    validate_path(Rest, N + 1, RequestURI).

validate_version("HTTP/1.1") ->
    true;
validate_version("HTTP/1.0") ->
    true;
validate_version("HTTP/0.9") ->
    true;
validate_version(_) ->
    false.
%%----------------------------------------------------------------------
%% There are 3 possible forms of the reuqest URI 
%%
%%  1. * When the request is not for a special assset. is is instead
%%     to the server itself
%%
%%  2. absoluteURI the whole servername port and asset is in the request
%%
%%  3. The most common form that http/1.0 used abs path that is a path
%%     to the requested asset.
%%----------------------------------------------------------------------
format_request_uri("*")->
    "*";
format_request_uri("http://" ++ ServerAndPath) ->
   remove_server(ServerAndPath);

format_request_uri("HTTP://" ++ ServerAndPath) ->
    remove_server(ServerAndPath);

format_request_uri(ABSPath) ->
    ABSPath.

remove_server([]) ->
    "/";
remove_server([$\/|Url])->
    case Url of
	[]->
	    "/";
        _->
	    [$\/|Url]
    end;
remove_server([_|Url]) ->
    remove_server(Url).

format_absolute_uri("http://"++ Uri, _)->
    "HTTP://" ++ Uri;

format_absolute_uri(OrigUri = "HTTP://" ++ _, _)->
    OrigUri;

format_absolute_uri(Uri,ParsedHeader)->
    case proplists:get_value("host", ParsedHeader) of
	undefined ->
	    nohost;
	Host ->
	    Host++Uri
    end.

get_persistens(HTTPVersion,ParsedHeader,ConfigDB)->
    case httpd_util:lookup(ConfigDB, keep_alive, true) of
	true->
	    case HTTPVersion of
		%%If it is version prio to 1.1 kill the conneciton
		"HTTP/1." ++ NList ->
		    case proplists:get_value("connection", ParsedHeader,
					     "keep-alive") of  
			%%if the connection is not ordered to go down
			%%let it live The keep-alive value is the
			%%older http/1.1 might be older Clients that
			%%use it.
			"keep-alive" when hd(NList) >= 49 ->
			    ?DEBUG("CONNECTION MODE: ~p",[true]),  
			    true;
			"close" ->
			    ?DEBUG("CONNECTION MODE: ~p",[false]),  
			    false;
			_Connect ->
  			    ?DEBUG("CONNECTION MODE: ~p VALUE: ~p",
				   [false, _Connect]),  
			    false
		    end; 
		_ ->
		    ?DEBUG("CONNECTION MODE: ~p VERSION: ~p",
			   [false, HTTPVersion]),  
		    false
	    end;
	_ ->
	    false
    end.

lowest_version()->    
    "HTTP/0.9".

check_header({"content-length", Value}, Maxsizes) ->
    Max = proplists:get_value(max_content_length, Maxsizes),
    MaxLen = length(integer_to_list(Max)),
    case length(Value) =< MaxLen of
	true ->
	    try 
		list_to_integer(Value)
	    of
		I when I>= 0 ->
		    ok;
		_ ->
		    {error, {size_error, Max, 411, "negative content-length"}}
	    catch _:_ ->
		    {error, {size_error, Max, 411, "content-length not an integer"}}
	    end;
	false ->
	    {error, {size_error, Max, 413, "content-length unreasonably long"}}
    end;
check_header(_, _) ->
    ok.
