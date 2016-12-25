%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_parse.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
-module(httpd_parse).
-export([
	 request_header/1,
	 hsplit/2,
	 get_request_record/10,
	 split_lines/1,
	 tagup_header/1]).
-include("httpd.hrl").


%%----------------------------------------------------------------------
%% request_header
%%
%% Input: The request as sent from the client (list of characters)
%%        (may include part of the entity body)
%%
%% Returns:
%%   {ok, Info#mod}
%%   {not_implemented,Info#mod}
%%   {bad_request, Reason}
%%----------------------------------------------------------------------

request_header(Header)->
    [RequestLine|HeaderFields] = split_lines(Header),
    ?DEBUG("request ->"
	   "~n   RequestLine: ~p"
	   "~n   Header:      ~p",[RequestLine,Header]),
    ParsedHeader = tagup_header(HeaderFields),
    ?DEBUG("request ->"
	   "~n   ParseHeader: ~p",[ParsedHeader]),
    case verify_request(string:tokens(RequestLine," ")) of
	["HEAD", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
	    {ok, ["HEAD", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader]};
	["GET", RequestURI, "HTTP/0.9"] ->
	    {ok, ["GET", RequestURI, "HTTP/0.9", RequestLine, ParsedHeader]};
	["GET", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
	    {ok, ["GET", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader]};
	["POST", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
	    {ok, ["POST", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader]};
	%%HTTP must be 1.1 or higher
	["TRACE", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] when N>48->
	    {ok, ["TRACE", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader]};
	[Method, RequestURI] ->
	    {not_implemented, RequestLine, Method, RequestURI,ParsedHeader,"HTTP/0.9"};
	[Method, RequestURI, HTTPVersion] ->
	    {not_implemented, RequestLine, Method, RequestURI,ParsedHeader, HTTPVersion};
	{bad_request, Reason} ->
	    {bad_request, Reason};
	Reason ->
	    {bad_request, "Unknown request method"}
    end.






%%----------------------------------------------------------------------
%% The request is passed through the server as a record of type mod get it
%% ----------------------------------------------------------------------

get_request_record(Socket,SocketType,ConfigDB,Method,RequestURI,
		 HTTPVersion,RequestLine,ParsedHeader,EntityBody,InitData)->
    PersistentConn=get_persistens(HTTPVersion,ParsedHeader,ConfigDB),
    Info=#mod{init_data=InitData,
	      data=[],
	      socket_type=SocketType,
	      socket=Socket,
	      config_db=ConfigDB,
	      method=Method,
	      absolute_uri=formatAbsoluteURI(RequestURI,ParsedHeader),
	      request_uri=formatRequestUri(RequestURI),
	      http_version=HTTPVersion,
	      request_line=RequestLine,
	      parsed_header=ParsedHeader,
	      entity_body=maybe_remove_nl(ParsedHeader,EntityBody),
	      connection=PersistentConn},
    {ok,Info}.

%%----------------------------------------------------------------------
%% Conmtrol wheater we shall maintain a persistent connection or not
%%----------------------------------------------------------------------
get_persistens(HTTPVersion,ParsedHeader,ConfigDB)->
    case httpd_util:lookup(ConfigDB,persistent_conn,true) of
	true->
	    case HTTPVersion of
		%%If it is version prio to 1.1 kill the conneciton
		[$H, $T, $T, $P, $\/, $1, $.,N] ->
		    case httpd_util:key1search(ParsedHeader,"connection","keep-alive")of
			%%if the connection isn't ordered to go down let it live
			%%The keep-alive value is the older http/1.1 might be older
			%%Clients that use it.
			"keep-alive" when N >= 49 ->
			    ?DEBUG("CONNECTION MODE: ~p",[true]),
			    true;
			"close" ->
			    ?DEBUG("CONNECTION MODE: ~p",[false]),
			    false;
			Connect ->
			    ?DEBUG("CONNECTION MODE: ~p VALUE: ~p",[false,Connect]),
			    false
		    end;
		_ ->
		    ?DEBUG("CONNECTION MODE: ~p VERSION: ~p",[false,HTTPVersion]),
		    false

	    end;
	_ ->
	    false
    end.




%%----------------------------------------------------------------------
%% Control whether the last newline of the body is a part of the message or
%%it is a part of the multipart message.
%%----------------------------------------------------------------------
maybe_remove_nl(Header,Rest) ->
    case find_content_type(Header) of
	false ->
	    {ok,EntityBody,_}=regexp:sub(Rest,"\r\n\$",""),
	    EntityBody;
	{ok, Value} ->
	    case string:str(Value, "multipart/form-data") of
		0 ->
		    {ok,EntityBody,_}=regexp:sub(Rest,"\r\n\$",""),
		    EntityBody;
		_ ->
		    Rest
	    end
    end.

%%----------------------------------------------------------------------
%% Cet the content type of the incomming request
%%----------------------------------------------------------------------


find_content_type([]) ->
    false;
find_content_type([{Name,Value}|Tail]) ->
    case httpd_util:to_lower(Name) of
	"content-type" ->
	    {ok, Value};
	_ ->
	    find_content_type(Tail)
    end.

%%----------------------------------------------------------------------
%% Split the header to a list of strings where each string represents a
%% HTTP header-field
%%----------------------------------------------------------------------
split_lines(Request) ->
    split_lines(Request, [], []).
split_lines([], CAcc, Acc) ->
    lists:reverse([lists:reverse(CAcc)|Acc]);

%%White space in the header fields are allowed but the new line must begin with LWS se
%%rfc2616 chap 4.2. The rfc do not say what to
split_lines([$\r, $\n, $\t |Rest], CAcc, Acc) ->
    split_lines(Rest, [$\r, $\n |CAcc], Acc);

split_lines([$\r, $\n, $\s |Rest], CAcc, Acc) ->
    split_lines(Rest, [$\r, $\n |CAcc], Acc);

split_lines([$\r, $\n|Rest], CAcc, Acc) ->
    split_lines(Rest, [], [lists:reverse(CAcc)|Acc]);
split_lines([Chr|Rest], CAcc, Acc) ->
    split_lines(Rest, [Chr|CAcc], Acc).


%%----------------------------------------------------------------------
%% This is a 'hack' to stop people from trying to access directories/files
%% relative to the ServerRoot.
%%----------------------------------------------------------------------


verify_request([Request, RequestURI]) ->
    verify_request([Request, RequestURI, "HTTP/0.9"]);

verify_request([Request, RequestURI, Protocol]) ->
    NewRequestURI =
	case string:str(RequestURI, "?") of
	    0 ->
		RequestURI;
	    Ndx ->
		string:left(RequestURI, Ndx)
	end,
   case string:str(NewRequestURI, "..") of
	0 ->
	    [Request, RequestURI, Protocol];
	_ ->
	    {bad_request, {forbidden, RequestURI}}
    end;
verify_request(Request) ->
    Request.

%%----------------------------------------------------------------------
%% tagup_header
%%
%% Parses the header of a HTTP request and returns a key,value tuple
%% list containing Name and Value of each header directive as of:
%%
%% Content-Type: multipart/mixed -> {"Content-Type", "multipart/mixed"}
%%
%% But in http/1.1 the field-names are case insencitive so now it must be
%% Content-Type: multipart/mixed -> {"content-type", "multipart/mixed"}
%% The standard furthermore says that leading and traling white space
%% is not a part of the fieldvalue and shall therefore be removed.
%%----------------------------------------------------------------------

tagup_header([]) ->          [];
tagup_header([Line|Rest]) -> [tag(Line, [])|tagup_header(Rest)].

tag([], Tag) ->
    {httpd_util:to_lower(lists:reverse(Tag)), ""};
tag([$:|Rest], Tag) ->
    {httpd_util:to_lower(lists:reverse(Tag)), httpd_util:strip(Rest)};
tag([Chr|Rest], Tag) ->
    tag(Rest, [Chr|Tag]).


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
%5----------------------------------------------------------------------
formatRequestUri("*")->
    "*";
formatRequestUri([$h,$t,$t,$p,$:,$\/,$\/|ServerAndPath]) ->
   removeServer(ServerAndPath);

formatRequestUri([$H,$T,$T,$P,$:,$\/,$\/|ServerAndPath]) ->
    removeServer(ServerAndPath);

formatRequestUri(ABSPath) ->
    ABSPath.

removeServer([$\/|Url])->
    case Url of
	[]->
	    "/";
        _->
	    [$\/|Url]
    end;
removeServer([N|Url]) ->
    removeServer(Url).


formatAbsoluteURI([$h,$t,$t,$p,$:,$\/,$\/|Uri],ParsedHeader)->
    [$H,$T,$T,$P,$:,$\/,$\/|Uri];

formatAbsoluteURI([$H,$T,$T,$P,$:,$\/,$\/|Uri],ParsedHeader)->
    [$H,$T,$T,$P,$:,$\/,$\/|Uri];

formatAbsoluteURI(Uri,ParsedHeader)->
    case httpd_util:key1search(ParsedHeader,"host") of
	undefined ->
	    nohost;
	Host ->
	    Host++Uri
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Code below is crap from an older version shall be removed when
%%transformation to http/1.1 is finished
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%request(Request) ->
%    ?DEBUG("request -> entry with:"
%	   "~n   Request: ~s",[Request]),
 %   {BeforeEntityBody, Rest} = hsplit([], Request),
 %   ?DEBUG("request ->"
%	   "~n   BeforeEntityBody: ~p"
%	   "~n   Rest:             ~p",[BeforeEntityBody, Rest]),
%    [RequestLine|Header] = split_lines(BeforeEntityBody),
%    ?DEBUG("request ->"
%	   "~n   RequestLine: ~p"
%	   "~n   Header:      ~p",[RequestLine,Header]),
%    ParsedHeader = tagup_header(Header),
%    ?DEBUG("request ->"
%	   "~n   ParseHeader: ~p",[ParsedHeader]),
%    EntityBody = maybe_remove_nl(ParsedHeader,Rest),
%    ?DEBUG("request ->"
%	   "~n   EntityBody: ~p",[EntityBody]),
%    case verify_request(string:tokens(RequestLine," ")) of
%	["HEAD", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
%	    {ok, ["HEAD", formatRequestUri(RequestURI), [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
%		 ParsedHeader, EntityBody]};
%	["GET", RequestURI, "HTTP/0.9"] ->
%	    {ok, ["GET", RequestURI, "HTTP/0.9", RequestLine, ParsedHeader,
%		 EntityBody]};
%	["GET", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
%	    {ok, ["GET", formatRequestUri(RequestURI), [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
%		 ParsedHeader,EntityBody]};
%%	["POST", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
%	    {ok, ["POST", formatRequestUri(RequestURI), [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
%		 ParsedHeader, EntityBody]};
%	[Method, RequestURI] ->
%	    {not_implemented, RequestLine, Method, RequestURI,ParsedHeader,"HTTP/0.9"};
%	[Method, RequestURI, HTTPVersion] ->
%	    {not_implemented, RequestLine, Method, RequestURI,ParsedHeader, HTTPVersion};
%	{bad_request, Reason} ->
%	    {bad_request, Reason};
%	Reason ->
%	    {bad_request, "Unknown request method"}
%    end.

hsplit(Accu,[]) ->
    {lists:reverse(Accu), []};
hsplit(Accu, [ $\r, $\n, $\r, $\n | Tail]) ->
    {lists:reverse(Accu), Tail};
hsplit(Accu, [H|T]) ->
    hsplit([H|Accu],T).
