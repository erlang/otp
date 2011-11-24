%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%

-module(httpd_1_1).

-include("test_server.hrl").
-include("test_server_line.hrl").
-include_lib("kernel/include/file.hrl").

-export([host/4, chunked/4, expect/4, range/4, if_test/5, http_trace/4,
	 head/4, mod_cgi_chunked_encoding_test/5]).

%% -define(all_keys_lower_case,true).
-ifndef(all_keys_lower_case).
-define(CONTENT_LENGTH, "Content-Length: ").
-define(CONTENT_RANGE,  "Content-Range: ").
-define(CONTENT_TYPE,   "Content-Type: ").
-else.
-define(CONTENT_LENGTH, "content-length:").
-define(CONTENT_RANGE,  "content-range:").
-define(CONTENT_TYPE,   "content-type:").
-endif.


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
host(Type, Port, Host, Node) ->
    %% No host needed for HTTP/1.0
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    %% No host must generate an error
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.1\r\n\r\n",
				       [{statuscode, 400}]),
    
    %% If it is a fully qualified URL no host is needed
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET HTTP://"++ Host ++ ":" ++ 
				       integer_to_list(Port)++
				       "/ HTTP/1.1\r\n\r\n",
				       [{statuscode, 200}]),

    %% If both look at the url.
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET HTTP://"++ Host ++ ":"++ 
				       integer_to_list(Port) ++ 
				       "/ HTTP/1.1\r\nHost:"++ Host ++
				       "\r\n\r\n",[{statuscode, 200}]),
    
    %% Allow the request if its a Host field  
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.1\r\nHost:"++ 
				       Host ++ "\r\n\r\n",
				       [{statuscode, 200}]),
    ok.
    
chunked(Type, Port, Host, Node)->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.1\r\n" 
				       "Host:"++ Host ++"\r\n"
				       "Transfer-Encoding:chunked\r\n"
				       "\r\n"
				       "A\r\n"
				       "1234567890\r\n"
				       "4\r\n"
				       "HEJ!\r\n"
				       "0\r\n\r\n",
				       [{statuscode, 200}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.1\r\n" 
				       "Host:"++ Host ++"\r\n"
				       "Transfer-Encoding:chunked\r\n"
				       "Trailer:Content-Type\r\n"
				       "\r\n" 
				       "A\r\n" 
				       "1234567890\r\n"
				       "4\r\n" 
				       "HEJ!\r\n"
				       "0\r\n"
				       "Content-Type:text/plain\r\n\r\n",
				       [{statuscode, 200}]),
    ok.

expect(Type, Port, Host, Node)->
    Request="GET / HTTP/1.1\r\nHost:" ++ Host ++ 
	"\r\nContent-Length:22\r\nExpect:100-continue\r\n\r\n",
   
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       Request, 
				       [{statuscode, 100}]).
range(Type, Port, Host, Node)->
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /range.txt HTTP/1.1\r\nHost:"
				       ++ Host
				       ++ "\r\nRange:bytes=110-120\r\n\r\n",
				       [{statuscode,416}]),
    %%The simples of all range request a range
    Request1="GET /range.txt HTTP/1.1\r\nHost:" ++ Host ++
	"\r\nRange:bytes=0-9\r\n\r\n",    
    {ok, Socket1} = inets_test_lib:connect_byte(Type, Host, Port),
    inets_test_lib:send(Type, Socket1,Request1),
    ok = validateRangeRequest(Socket1,[],"1234567890",$2,$0,$6),
    inets_test_lib:close(Type,Socket1),
    
    %% Request the end of the file
    Request2 =
	"GET /range.txt HTTP/1.1\r\nHost:" ++ Host ++
	"\r\nRange:bytes=90-\r\n\r\n",    
    
    {ok, Socket2} = inets_test_lib:connect_byte(Type, Host, Port),
    inets_test_lib:send(Type, Socket2, Request2),
    ok = validateRangeRequest(Socket2,[],"1234567890",$2,$0,$6),
    inets_test_lib:close(Type,Socket2),
    
    %% The last byte in the file
    Request3 =
	"GET /range.txt HTTP/1.1\r\nHost:"++
	Host ++ "\r\nRange:bytes=-1\r\n\r\n",    
    {ok, Socket3} = inets_test_lib:connect_byte(Type, Host, Port),
    inets_test_lib:send(Type, Socket3,Request3),
     ok = validateRangeRequest(Socket3,[],"0",$2,$0,$6),
    inets_test_lib:close(Type, Socket3),

    %%Multi Range
    Request4 = "GET /range.txt HTTP/1.1\r\nHost:" ++ Host ++
	"\r\nRange:bytes=0-0,2-2,-1\r\n\r\n",    
    {ok, Socket4} = inets_test_lib:connect_byte(Type, Host, Port),
    inets_test_lib:send(Type, Socket4, Request4),
    ok = validateRangeRequest(Socket4,[],"130",$2,$0,$6),
    inets_test_lib:close(Type, Socket4).

if_test(Type, Port, Host, Node, DocRoot)->
    {ok, FileInfo} = 
	file:read_file_info(filename:join([DocRoot,"index.html"])),
    CreatedSec = 
	calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime),
    
    Mod = httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(
    				      CreatedSec-1)),
    
    %% Test that we get the data when the file is modified
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
    				       "GET / HTTP/1.1\r\nHost:" ++ Host ++
    				       "\r\nIf-Modified-Since:" ++
    				       Mod ++ "\r\n\r\n",
    				       [{statuscode, 200}]),
    Mod1 = httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(
    				     CreatedSec+100)),
    ok = httpd_test_lib:verify_request(Type,Host,Port,Node, 
    				       "GET / HTTP/1.1\r\nHost:"
    				       ++ Host ++"\r\nIf-Modified-Since:"
    				       ++ Mod1 ++"\r\n\r\n",
    				       [{statuscode, 304}]),
    

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.1\r\nHost:" ++ Host ++
				       "\r\nIf-Modified-Since:" ++
				       "AAA[...]AAAA" ++ "\r\n\r\n",
				       [{statuscode, 400}]),


     Mod2 =  httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(
    				      CreatedSec+1)),
     %% Control that the If-Unmodified-Header lmits the response
     ok = httpd_test_lib:verify_request(Type,Host,Port,Node, 
    					  "GET / HTTP/1.1\r\nHost:"
    					  ++ Host ++ 
    					  "\r\nIf-Unmodified-Since:" ++ Mod2 
    					  ++ "\r\n\r\n",
    					  [{statuscode, 200}]),
     Mod3 = httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(
    				     CreatedSec-1)),
    
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
    					  "GET / HTTP/1.1\r\nHost:"
    					  ++ Host ++ 
    					  "\r\nIf-Unmodified-Since:"++ Mod3 
    					  ++"\r\n\r\n",
    					  [{statuscode, 412}]),
    
     %% Control that we get the body when the etag match
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
    					  "GET / HTTP/1.1\r\nHost:" ++ Host 
    					  ++"\r\n"++
    					  "If-Match:"++ 
    					  httpd_util:create_etag(FileInfo)++
    					  "\r\n\r\n",
    					  [{statuscode, 200}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
    					  "GET / HTTP/1.1\r\nHost:" ++ 
    					  Host ++ "\r\n"++
    					  "If-Match:NotEtag\r\n\r\n",
    					  [{statuscode, 412}]),
    
     %% Control the response when the if-none-match header is there
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
    					  "GET / HTTP/1.1\r\nHost:"
    					  ++ Host ++"\r\n"++
    					  "If-None-Match:NoTaag," ++ 
    					  httpd_util:create_etag(FileInfo) ++
    					  "\r\n\r\n",
    					  [{statuscode, 304}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
    					  "GET / HTTP/1.1\r\nHost:"
    					  ++ Host ++ "\r\n"++
    					  "If-None-Match:NotEtag,"
    					  "NeihterEtag\r\n\r\n",
    					  [{statuscode,200}]),
    ok.
    
http_trace(Type, Port, Host, Node)->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
					  "TRACE / HTTP/1.1\r\n" ++
					  "Host:" ++ Host ++ "\r\n" ++
					  "Max-Forwards:2\r\n\r\n",
					  [{statuscode, 200}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "TRACE / HTTP/1.0\r\n\r\n",
				       [{statuscode, 501}, 
					{version, "HTTP/1.0"}]).
head(Type, Port, Host, Node)->
    %% mod_include 
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "HEAD /fsize.shtml HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
			"HEAD /fsize.shtml HTTP/1.1\r\nhost:" ++ 
			Host  ++ "\r\n\r\n", [{statuscode, 200}]),
    %% mod_esi
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
			"HEAD /cgi-bin/erl/httpd_example/newformat"
			" HTTP/1.0\r\n\r\n", [{statuscode, 200},
					      {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
			"HEAD /cgi-bin/erl/httpd_example/newformat "
			"HTTP/1.1\r\nhost:" ++ Host  ++ "\r\n\r\n", 
			 [{statuscode, 200}]),
    %% mod_cgi
    Script =
	case test_server:os_type() of
	    {win32, _} ->
		"printenv.bat";
	    _ ->
		"printenv.sh"
	end,
    ok = httpd_test_lib:verify_request(Type,Host,Port,Node,"HEAD /cgi-bin/" 
				       ++ Script ++ " HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type,Host,Port,Node, "HEAD /cgi-bin/"
				       ++ Script ++ " HTTP/1.1\r\nhost:" ++ 
				       Host  ++ "\r\n\r\n", 
				       [{statuscode, 200}]).

mod_cgi_chunked_encoding_test(_, _, _, _, [])->
    ok;
mod_cgi_chunked_encoding_test(Type, Port, Host, Node, [Request| Rest])->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, Request, 
				       [{statuscode, 200}]),
    mod_cgi_chunked_encoding_test(Type, Port, Host, Node, Rest).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
validateRangeRequest(Socket,Response,ValidBody,C,O,DE)->
    receive
	{tcp,Socket,Data} ->
	    case string:str(Data,"\r\n") of
		0->
		    validateRangeRequest(Socket,
					 Response ++ Data,
					 ValidBody, C, O, DE);
		_N ->
		    case Response ++ Data of
			[$H,$T,$T,$P,$/,$1,$.,$1,$ ,C,O,DE | _Rest]->
			    case [C,O,DE] of
				"206" ->
				    validateRangeRequest1(Socket,
							  Response ++ Data,
							  ValidBody);
				_ ->
				    bad_code
			    end;
			_->
			    error
		    end
	    end;
	_Error ->
	    error
    end.

validateRangeRequest1(Socket, Response, ValidBody) ->
    case end_of_header(Response) of
	false ->
	    receive
		{tcp,Socket,Data} ->
		    validateRangeRequest1(Socket, Response ++ Data, 
					  ValidBody);
		_->
		    error
	    end;
	{true, Head1, Body, _Size} ->
	    %% In this case size will be 0 if it is a multipart so
	    %% don't use it.
	    validateRangeRequest2(Socket, Head1, Body, ValidBody,
				  getRangeSize(Head1))
    end.

validateRangeRequest2(Socket, Head, Body, ValidBody, {multiPart,Boundary})->
    case endReached(Body,Boundary) of
	true ->
	    validateMultiPartRangeRequest(Body, ValidBody, Boundary);
	false->
	    receive
		{tcp, Socket, Data} ->
		    validateRangeRequest2(Socket, Head, Body ++ Data,
					  ValidBody, {multiPart, Boundary});
		{tcp_closed, Socket} ->
		    error;
		_ ->
		    error
	    end
    end;

validateRangeRequest2(Socket, Head, Body, ValidBody, BodySize) 
  when is_integer(BodySize) ->
    case length(Body)  of
	Size when Size =:= BodySize ->
	    case Body of
		ValidBody ->
		    ok;
		Body ->
		    error
	    end;	
	Size when Size < BodySize ->
	    receive
		{tcp, Socket, Data} ->
		    validateRangeRequest2(Socket, Head,
					  Body ++ Data, ValidBody, BodySize);
		_ ->
		    error
	    end;
	_ ->
	    error
    end.


validateMultiPartRangeRequest(Body, ValidBody, Boundary)->
    case inets_regexp:split(Body,"--"++Boundary++"--") of
	%%Last is the epilogue and must be ignored 
	{ok,[First | _Last]}->
	    %%First is now the actuall http request body.
	    case inets_regexp:split(First, "--" ++ Boundary) of
		%%Parts is now a list of ranges and the heads for each range
		%%Gues we try to split out the body
		{ok,Parts}->
		    case lists:flatten(lists:map(fun splitRange/1,Parts)) of
			ValidBody->
			    ok;
		       ParsedBody->
			    error = ParsedBody
		    end
	    end;
	_ ->
	    error
    end.


splitRange(Part)->	    
    case inets_regexp:split(Part, "\r\n\r\n") of
	{ok,[_, Body]} ->
	    string:substr(Body, 1, length(Body) - 2);
	_ ->
	    []
    end.

endReached(Body, Boundary)->
    EndBound = "--" ++ Boundary ++ "--",
    case string:str(Body, EndBound) of
	0 -> 
	    false;
	_ ->
	    true
    end.
    	    
getRangeSize(Head)->
    case controlMimeType(Head) of
	{multiPart, BoundaryString}->
	    {multiPart, BoundaryString};
	_X1 ->
	    case inets_regexp:match(Head, ?CONTENT_RANGE "bytes=.*\r\n") of
		{match, Start, Lenght} ->
		    %% Get the range data remove the fieldname and the
		    %% end of line.
		    RangeInfo = string:substr(Head, Start + 20, 
					      Lenght - (20 - 2)),
		    rangeSize(RangeInfo);
		_X2 ->
		    error
	    end
    end.
%%RangeInfo is NNN1-NNN2/NNN3
%%NNN1=RangeStartByte
%%NNN2=RangeEndByte
%%NNN3=total amount of bytes in file 
rangeSize([$=|RangeInfo]) ->
    rangeSize(RangeInfo);
rangeSize(RangeInfo) ->
    StartByte = lists:takewhile(fun(X)->
					num(X, true)
				end, RangeInfo),
    RangeInfo2 = string:substr(RangeInfo, length(StartByte) + 2),
    EndByte = lists:takewhile(fun(X)->
				      num(X,true)
			      end, RangeInfo2),
    case list_to_integer(EndByte) - list_to_integer(StartByte) of
	Val when is_number(Val) ->
	    %%Add one since it is startByte to endbyte ie 0-0 is 1
	    %%byte 0-99 is 100 bytes
	    Val + 1; 
	_Val ->
	    error
    end.
    
num(CharVal, RetVal) when (CharVal >= 48) andalso (CharVal =< 57) ->
    RetVal;
num(_CharVal, true) ->
    false;
num(_CharVal, false) ->
    true.

controlMimeType(Head)->
    case inets_regexp:match(Head,?CONTENT_TYPE "multipart/byteranges.*\r\n") of
	{match,Start,Length}->
	    FieldNameLen = length(?CONTENT_TYPE "multipart/byteranges"),
	    case clearBoundary(string:substr(Head, Start + FieldNameLen,
					     Length - (FieldNameLen+2))) of
		error ->
		    error;
		BoundaryStr ->
		    {multiPart,BoundaryStr}
	    end;
	nomatch->
	    0;
	_ ->
	    error
    end.

clearBoundary(Boundary)->
    case inets_regexp:match(Boundary, "boundary=.*\$") of
	{match, Start1, Length1}->
	    BoundLen = length("boundary="),
	    string:substr(Boundary, Start1 + BoundLen, Length1 - BoundLen);
	_ ->
	    error
    end.


end_of_header(HeaderPart) ->
    case httpd_util:split(HeaderPart,"\r\n\r\n",2) of
 	{ok, [Head, Body]} ->	
 	    {true, Head, Body, get_body_size(Head)};
 	_Pos ->
	    false
    end.

get_body_size(Head) ->
    case inets_regexp:match(Head,?CONTENT_LENGTH ".*\r\n") of
 	{match, Start, Length} ->
 	    %% 15 is length of Content-Length, 
 	    %% 17 Is length of Content-Length and \r\
 	    S = list_to_integer(
 		  string:strip(string:substr(Head, Start + 15, Length-17))),
 	    S;
 	_->
 	    0
     end.
