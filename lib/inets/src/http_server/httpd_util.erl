%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(httpd_util).
-export([ip_address/2, lookup/2, lookup/3, multi_lookup/2,
	 lookup_mime/2, lookup_mime/3, lookup_mime_default/2,
	 lookup_mime_default/3, reason_phrase/1, message/3, rfc1123_date/0,
	 rfc1123_date/1, day/1, month/1,
	 flatlength/1, split_path/1, split_script_path/1, 
	 suffix/1, split/3, uniq/1,
	 make_name/2,make_name/3,make_name/4,strip/1,
	 hexlist_to_integer/1,integer_to_hexlist/1,
	 convert_request_date/1,create_etag/1,create_etag/2,
	 convert_netscapecookie_date/1, enable_debug/1, valid_options/3,
	 modules_validate/1, module_validate/1, 
	 dir_validate/2, file_validate/2, mime_type_validate/1, 
	 mime_types_validate/1, custom_date/0, error_log/2]).

-export([encode_hex/1, decode_hex/1]).
-include_lib("kernel/include/file.hrl").
-include_lib("inets/include/httpd.hrl").

ip_address({_,_,_,_} = Address, _IpFamily) ->
    {ok, Address};
ip_address({_,_,_,_,_,_,_,_} = Address, _IpFamily) ->
    {ok, Address};
ip_address(Host, IpFamily) 
  when ((IpFamily =:= inet) orelse (IpFamily =:= inet6)) ->
    inet:getaddr(Host, IpFamily).

%% lookup

lookup(Table,Key) ->
    lookup(Table,Key,undefined).

lookup(Table,Key,Undefined) ->
    case catch ets:lookup(Table,Key) of
	[{Key,Value}|_] ->
	    Value;
	_->
	    Undefined
    end.

%% multi_lookup

multi_lookup(Table,Key) ->
    remove_key(ets:lookup(Table,Key)).

remove_key([]) ->
    [];
remove_key([{_Key, Value}| Rest]) ->
    [Value | remove_key(Rest)].

%% lookup_mime

lookup_mime(ConfigDB,Suffix) ->
    lookup_mime(ConfigDB,Suffix,undefined).

lookup_mime(ConfigDB,Suffix,Undefined) ->
    [{mime_types,MimeTypesDB}|_]=ets:lookup(ConfigDB,mime_types),
    case ets:lookup(MimeTypesDB,Suffix) of
	[] ->
	    Undefined;
	[{Suffix,MimeType}|_] ->
	    MimeType
    end.

%% lookup_mime_default

lookup_mime_default(ConfigDB,Suffix) ->
    lookup_mime_default(ConfigDB,Suffix,undefined).

lookup_mime_default(ConfigDB,Suffix,Undefined) ->
    [{mime_types,MimeTypesDB}|_]=ets:lookup(ConfigDB,mime_types),
    case ets:lookup(MimeTypesDB,Suffix) of
	[] ->
	    case ets:lookup(ConfigDB,default_type) of
		[] ->
		    Undefined;
		[{default_type,DefaultType}|_] ->
		    DefaultType
	    end;
	[{Suffix,MimeType}|_] ->
	    MimeType
    end.

%%% RFC 2616, HTTP 1.1 Status codes
reason_phrase(100) ->   "Continue";
reason_phrase(101) ->   "Switching Protocols" ;
reason_phrase(200) ->   "OK" ;
reason_phrase(201) ->   "Created" ;
reason_phrase(202) ->   "Accepted" ;
reason_phrase(203) ->   "Non-Authoritative Information" ;
reason_phrase(204) ->   "No Content" ;
reason_phrase(205) ->   "Reset Content" ;
reason_phrase(206) ->   "Partial Content" ;
reason_phrase(300) ->   "Multiple Choices" ;
reason_phrase(301) ->   "Moved Permanently" ;
reason_phrase(302) ->   "Moved Temporarily" ;
reason_phrase(303) ->   "See Other" ;
reason_phrase(304) ->   "Not Modified" ;
reason_phrase(305) ->   "Use Proxy" ;
reason_phrase(306) ->   "(unused)" ;
reason_phrase(307) ->   "Temporary Redirect" ;
reason_phrase(400) ->   "Bad Request";
reason_phrase(401) ->   "Unauthorized";
reason_phrase(402) ->   "Payment Required";
reason_phrase(403) ->   "Forbidden" ;
reason_phrase(404) ->   "Object Not Found" ;
reason_phrase(405) ->   "Method Not Allowed" ;
reason_phrase(406) ->   "Not Acceptable" ;
reason_phrase(407) ->   "Proxy Authentication Required" ;
reason_phrase(408) ->   "Request Time-out" ;
reason_phrase(409) ->   "Conflict" ;
reason_phrase(410) ->   "Gone" ;
reason_phrase(411) ->   "Length Required" ;
reason_phrase(412) ->   "Precondition Failed" ;
reason_phrase(413) ->   "Request Entity Too Large" ;
reason_phrase(414) ->   "Request-URI Too Large" ;
reason_phrase(415) ->   "Unsupported Media Type" ;
reason_phrase(416) ->   "Requested Range Not Satisfiable" ;
reason_phrase(417) ->   "Expectation Failed" ;
reason_phrase(500) ->   "Internal Server Error" ;
reason_phrase(501) ->   "Not Implemented" ;
reason_phrase(502) ->   "Bad Gateway" ;
reason_phrase(503) ->   "Service Unavailable" ;
reason_phrase(504) ->   "Gateway Time-out" ;
reason_phrase(505) ->   "HTTP Version not supported";

%%% RFC 2518, HTTP Extensions for Distributed Authoring -- WEBDAV
reason_phrase(102) ->   "Processing";
reason_phrase(207) ->   "Multi-Status";
reason_phrase(422) ->   "Unprocessable Entity";
reason_phrase(423) ->   "Locked";
reason_phrase(424) ->   "Failed Dependency";
reason_phrase(507) ->   "Insufficient Storage";

%%% (Work in Progress) WebDAV Advanced Collections
reason_phrase(425) ->   "";

%%% RFC 2817, HTTP Upgrade to TLS
reason_phrase(426) ->   "Upgrade Required";

%%% RFC 3229, Delta encoding in HTTP
reason_phrase(226) ->   "IM Used";

reason_phrase(_) -> "Internal Server Error".


%% message

message(301,URL,_) ->
    "The document has moved <A HREF=\""++ maybe_encode(URL) ++"\">here</A>.";
message(304, _URL,_) ->
    "The document has not been changed.";
message(400, none, _) ->
    "Your browser sent a query that this server could not understand. ";
message(400, Msg, _) ->
    "Your browser sent a query that this server could not understand. " ++ 
	html_encode(Msg);
message(401, none, _) ->
    "This server could not verify that you
are authorized to access the document you
	requested.  Either you supplied the wrong
credentials (e.g., bad password), or your
browser doesn't understand how to supply
the credentials required.";
message(403,RequestURI,_) ->
    "You don't have permission to access " ++ 
	html_encode(RequestURI) ++ 
	" on this server.";
message(404,RequestURI,_) ->
    "The requested URL " ++ 
	html_encode(RequestURI) ++ 
	" was not found on this server.";
message(408, Timeout, _) ->
    Timeout;
message(412,none,_) ->
    "The requested preconditions were false";
message(413, Reason,_) ->
    "Entity: " ++ html_encode(Reason);
message(414,ReasonPhrase,_) ->
    "Message " ++ html_encode(ReasonPhrase) ++ ".";
message(500,_,ConfigDB) ->
    ServerAdmin=lookup(ConfigDB,server_admin,"unknown@unknown"),
    "The server encountered an internal error or "
	"misconfiguration and was unable to complete "
	"your request.<P>Please contact the server administrator "
	++ html_encode(ServerAdmin) ++ 
	", and inform them of the time the error occurred "
	"and anything you might have done that may have caused the error.";

message(501,{Method, RequestURI, HTTPVersion}, _ConfigDB) ->
    if
	is_atom(Method) ->
	    atom_to_list(Method) ++
		" to " ++ 
		html_encode(RequestURI) ++ 
		" (" ++ HTTPVersion ++ ") not supported.";
	is_list(Method) ->
	    Method ++
		" to " ++ 
		html_encode(RequestURI) ++ 
		" (" ++ HTTPVersion ++ ") not supported."
    end;

message(503, String, _ConfigDB) ->
    "This service in unavailable due to: " ++ html_encode(String);
message(_, ReasonPhrase, _) ->
    html_encode(ReasonPhrase).

maybe_encode(URI) ->
    Decoded = try http_uri:decode(URI) of
	N -> N
    catch
	error:_ -> URI
    end,
    http_uri:encode(Decoded).

html_encode(String) ->
    try http_uri:decode(String) of
	Decoded when is_list(Decoded) ->
	    http_util:html_encode(Decoded)
    catch 
	_:_ ->
	    http_util:html_encode(String)
    end.

%%convert_rfc_date(Date)->{{YYYY,MM,DD},{HH,MIN,SEC}}

convert_request_date([D,A,Y,DateType| Rest])->
    Func=case DateType of
	     $\, ->
		 fun convert_rfc1123_date/1;
	     $\  ->
		 fun convert_ascii_date/1;
	     _ ->
		 fun convert_rfc850_date/1
	 end,
    case catch Func([D,A,Y,DateType| Rest]) of
	{ok, Date} ->
	    Date;
	_Error->
	    bad_date
    end.
convert_rfc850_date(DateStr) ->
    [_WeekDay,Date,Time,_TimeZone|_Rest] = string:tokens(DateStr," "), 
    convert_rfc850_date(Date,Time).

convert_rfc850_date([D1,D2,_,
		     M,O,N,_,
		     Y1,Y2|_Rest],[H1,H2,_Col,M1,M2,_Col,S1,S2|_Rest2])->    
    Year=list_to_integer([50,48,Y1,Y2]),
    Day=list_to_integer([D1,D2]),
    Month = http_util:convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {ok,{{Year,Month,Day},{Hour,Min,Sec}}}.

convert_ascii_date([_D,_A,_Y,_SP,
		    M,O,N,_SP,
		    D1,D2,_SP,
		    H1,H2,_Col,
		    M1,M2,_Col,
		    S1,S2,_SP,
		    Y1,Y2,Y3,Y4| _Rest])->
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=case D1 of 
	    $\ ->
		list_to_integer([D2]);
	    _->
		list_to_integer([D1,D2])
	end,
    Month=http_util:convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {ok,{{Year,Month,Day},{Hour,Min,Sec}}}.

convert_rfc1123_date([_D,_A,_Y,_C,_SP,
		      D1,D2,_SP,
		      M,O,N,_SP,
		      Y1,Y2,Y3,Y4,_SP,
		      H1,H2,_Col,
		      M1,M2,_Col,
		      S1,S2|_Rest]) -> 
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1,D2]),
    Month=http_util:convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {ok,{{Year,Month,Day},{Hour,Min,Sec}}}.

convert_netscapecookie_date(Date)->
    case (catch http_util:convert_netscapecookie_date(Date)) of
	Ret = {ok, _} ->
	    Ret;
	_ ->
	    {error,bad_date}
    end.


%% rfc1123_date

rfc1123_date() ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} = calendar:universal_time(),
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
		    [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

rfc1123_date(undefined) ->
    undefined;
rfc1123_date(LocalTime) ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} = 
	case calendar:local_time_to_universal_time_dst(LocalTime) of
	    [Gmt]   -> Gmt;
	    [_,Gmt] -> Gmt;
        % Should not happen, but handle the empty list to prevent an error.
        [] -> LocalTime
	end,
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
		    [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

custom_date() ->
    LocalTime     = calendar:local_time(),
    UniversalTime = calendar:universal_time(),
    Minutes       = round(diff_in_minutes(LocalTime,UniversalTime)),
    {{YYYY,MM,DD},{Hour,Min,Sec}} = LocalTime,
    Date = 
	io_lib:format("~.2.0w/~.3s/~.4w:~.2.0w:~.2.0w:~.2.0w ~c~.2.0w~.2.0w",
		      [DD,httpd_util:month(MM),YYYY,Hour,Min,Sec,
		       sign(Minutes), abs(Minutes) div 60,
		       abs(Minutes) rem 60]),  
    lists:flatten(Date).

diff_in_minutes(L,U) ->
    (calendar:datetime_to_gregorian_seconds(L) -
     calendar:datetime_to_gregorian_seconds(U))/60.

sign(Minutes) when Minutes > 0 ->
    $+;
sign(_Minutes) ->
    $-.

%% uniq

uniq([]) ->
    [];
uniq([First,First|Rest]) ->
    uniq([First|Rest]);
uniq([First|Rest]) ->
    [First|uniq(Rest)].


%% day

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat"; 
day(7) -> "Sun".

%% month

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% decode_hex

decode_hex(URI) ->
    http_uri:decode(URI).

encode_hex(URI) ->
    http_uri:encode(URI).

%% flatlength
flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T],L) when is_list(H) ->
    flatlength(H,flatlength(T,L));
flatlength([H|T],L) when is_binary(H) ->
    flatlength(T,L+size(H));
flatlength([_H|T],L) ->
    flatlength(T,L+1);
flatlength([],L) ->
    L.

%% split_path

split_path(Path) ->
    case re:run(Path,"[\?].*\$", [{capture, first}]) of
	%% A QUERY_STRING exists!
	{match,[{Start,Length}]} ->
	    {http_uri:decode(string:substr(Path,1,Start)),
	     string:substr(Path,Start+1,Length)};
	%% A possible PATH_INFO exists!
	nomatch ->
	    split_path(Path,[])
    end.

split_path([],SoFar) ->
    {http_uri:decode(lists:reverse(SoFar)),[]};
split_path([$/|Rest],SoFar) ->
    Path=http_uri:decode(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok,FileInfo} when FileInfo#file_info.type =:= regular ->
	    {Path,[$/|Rest]};
	{ok, _FileInfo} ->
	    split_path(Rest,[$/|SoFar]);
	{error, _Reason} ->
	    split_path(Rest,[$/|SoFar])
    end;
split_path([C|Rest],SoFar) ->
    split_path(Rest,[C|SoFar]).

%% split_script_path

split_script_path(Path) ->
    case split_script_path(Path, []) of
	{Script, AfterPath} ->
	    {PathInfo, QueryString} = pathinfo_querystring(AfterPath),
	    {Script, {PathInfo, QueryString}};
	not_a_script ->
	    not_a_script
    end.

pathinfo_querystring(Str) ->
    pathinfo_querystring(Str, []).
pathinfo_querystring([], SoFar) ->
    {lists:reverse(SoFar), []};
pathinfo_querystring([$?|Rest], SoFar) ->
    {lists:reverse(SoFar), Rest};
pathinfo_querystring([C|Rest], SoFar) ->
    pathinfo_querystring(Rest, [C|SoFar]).

split_script_path([$?|QueryString], SoFar) ->
    Path = http_uri:decode(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok,FileInfo} when FileInfo#file_info.type =:= regular ->
	    {Path, [$?|QueryString]};
	{ok, _FileInfo} ->
	    not_a_script;
	{error, _Reason} ->
	    not_a_script
    end;
split_script_path([], SoFar) ->
    Path = http_uri:decode(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok,FileInfo} when FileInfo#file_info.type =:= regular ->
	    {Path, []};
	{ok, _FileInfo} ->
	    not_a_script;
	{error, _Reason} ->
	    not_a_script
    end;
split_script_path([$/|Rest], SoFar) ->
    Path = http_uri:decode(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok, FileInfo} when FileInfo#file_info.type =:= regular ->
	    {Path, [$/|Rest]};
	{ok, _FileInfo} ->
	    split_script_path(Rest, [$/|SoFar]);
	{error, _Reason} ->
	    split_script_path(Rest, [$/|SoFar])
    end;
split_script_path([C|Rest], SoFar) ->
    split_script_path(Rest,[C|SoFar]).

%% suffix

suffix(Path) ->
    case filename:extension(Path) of
	[] ->
	    [];
	Extension ->
	    tl(Extension)
    end.


%% strip
strip(Value)->
    lists:reverse(remove_ws(lists:reverse(remove_ws(Value)))).
	
remove_ws([$\s|Rest])->
    remove_ws(Rest);
remove_ws([$\t|Rest]) ->
    remove_ws(Rest);
remove_ws(Rest) ->
    Rest.

%% split

split(String,RegExp,N) ->
    {ok, re:split(String, RegExp, [{parts, N}, {return, list}])}.

%% make_name/2, make_name/3
%% Prefix  -> string()
%%            First part of the name, e.g. "httpd"
%% Addr    -> {A,B,C,D} | string() | undefined
%%            The address part of the name. 
%%            e.g. "123.234.55.66" or {123,234,55,66} or "otp.ericsson.se" 
%%            for a host address or undefined if local host.
%% Port    -> integer()
%%            Last part of the name, such as the HTTPD server port 
%%            number (80).
%% Postfix -> Any string that will be added last to the name
%%
%% Example:
%% make_name("httpd","otp.ericsson.se",80) => httpd__otp_ericsson_se__80
%% make_name("httpd",undefined,8088)       => httpd_8088

make_name(Prefix,Port) ->
    make_name(Prefix,undefined,Port,"").

make_name(Prefix,Addr,Port) ->
    make_name(Prefix,Addr,Port,"").

make_name(Prefix, Addr,Port,Postfix) when is_atom(Postfix)->
    make_name(Prefix, Addr,Port, atom_to_list(Postfix));

make_name(Prefix,"*",Port,Postfix) ->
    make_name(Prefix,undefined,Port,Postfix);

make_name(Prefix,any,Port,Postfix) ->
    make_name1(io_lib:format("~s_~w~s",[Prefix,Port,Postfix]));

make_name(Prefix,undefined,Port,Postfix) ->
    make_name1(io_lib:format("~s_~w~s",[Prefix,Port,Postfix]));

make_name(Prefix,Addr,Port,Postfix) ->
    NameString = 
        Prefix ++ "__" ++ make_name2(Addr) ++ "__" ++ 
	integer_to_list(Port) ++ Postfix,
    make_name1(NameString).
    
make_name1(String) ->
    list_to_atom(lists:flatten(String)).

make_name2({A,B,C,D}) ->
    io_lib:format("~w_~w_~w_~w", [A,B,C,D]);

make_name2({A, B, C, D, E, F, G, H}) ->
    io_lib:format("~w_~w_~w_~w_~w_~w_~w_~w", [A,B,C,D,E,F,G,H]);
make_name2(Addr) ->
    search_and_replace(Addr,$.,$_).

search_and_replace(S,A,B) ->
    Fun = fun(What) -> 
                  case What of
                      A -> B;
                      O -> O
                  end
          end,
    lists:map(Fun,S).



%%----------------------------------------------------------------------
%% Converts  a string that constists of 0-9,A-F,a-f to a 
%% integer
%%----------------------------------------------------------------------

hexlist_to_integer(List)->
    http_util:hexlist_to_integer(List).

%%----------------------------------------------------------------------
%%Converts an integer to an hexlist
%%----------------------------------------------------------------------
integer_to_hexlist(Num) when is_integer(Num) ->
    http_util:integer_to_hexlist(Num).
	    	      
create_etag(FileInfo) ->
    create_etag(FileInfo#file_info.mtime,FileInfo#file_info.size).

create_etag({{Year,Month,Day},{Hour,Min,Sec}},Size)->
    create_part([Year,Month,Day,Hour,Min,Sec])++io_lib:write(Size);

create_etag(FileInfo,Size)->
    create_etag(FileInfo#file_info.mtime,Size).

create_part(Values)->
    lists:map(fun(Val0)->
		      Val=Val0 rem 60,
			  if
			      Val=<25 ->
				  65+Val;  % A-Z
			      Val=<50 ->
				  72+Val;  % a-z
			      %%Since no date s
			      true ->
				  Val-3
			  end
	      end,Values).

%%----------------------------------------------------------------------
%% Validate httpd options
%%----------------------------------------------------------------------
modules_validate([]) ->
    ok;
modules_validate([Head | Tail]) ->
    ok = module_validate(Head),
    modules_validate(Tail).

module_validate(Module) when is_atom(Module) ->
    case code:which(Module) of
	non_existing ->
	    throw({module_does_not_exist, Module});
	_ -> 
	    ok
    end;

module_validate(Module) ->
    throw({module_name_not_atom, Module}).

dir_validate(ConfDir, Dir) ->
    case filelib:is_dir(Dir) of
	true ->
	    ok;
	false when is_list(Dir) ->
	    throw({non_existing, {ConfDir, Dir}});
	false ->
	    throw({ConfDir, Dir})
    end.
    
file_validate(ConfFile, File) ->
    case filelib:is_file(File) of
	true ->
	    ok;
	false when is_list(File) ->
	    throw({non_existing, {ConfFile, File}});	      
	false ->
	    throw({ConfFile, File})
    end.

mime_type_validate({Value1, Value2}) 
  when is_list(Value1) andalso is_list(Value2) ->
    ok;
mime_type_validate({_, _} = Value) ->
    throw({mime_type, Value});
mime_type_validate(MimeFile) ->
    file_validate(mime_types, MimeFile).

mime_types_validate([{_, _} = Value | Rest]) ->
    ok = mime_types_validate(Value),
    mime_types_validate(Rest);
mime_types_validate([]) ->
    ok;
mime_types_validate(MimeFile) ->
    mime_type_validate(MimeFile).


valid_options(Debug, AcceptTimeout, Config) ->
    valid_debug(Debug),
    valid_accept_timeout(AcceptTimeout),
    valid_config(Config).

valid_debug([]) ->
    ok;
valid_debug(disable) ->
    ok;
valid_debug(L) when is_list(L) ->
    valid_debug2(L);
valid_debug(D) ->
    throw({error, {bad_debug_option,D}}).

valid_debug2([{all_functions,L}|Rest]) when is_list(L) ->
    try modules_validate(L) of
	ok ->
	    valid_debug2(Rest)
    catch
	throw:Error ->
	    throw({error, Error})
    end;
valid_debug2([{exported_functions,L}|Rest]) when is_list(L) ->
    modules_validate(L),
    valid_debug2(Rest);
valid_debug2([{disable,L}|Rest]) when is_list(L) ->
    modules_validate(L),
    valid_debug2(Rest);
valid_debug2([H|_T]) ->
    throw({error,{bad_debug_option,H}});
valid_debug2([]) ->
    ok.

valid_accept_timeout(I) when is_integer(I) ->
    ok;
valid_accept_timeout(A) ->
    throw({error,{bad_debug_option,A}}).

valid_config(_) ->
    ok.

%%----------------------------------------------------------------------
%% Enable debugging, 
%%----------------------------------------------------------------------

enable_debug([]) ->
    ok;
enable_debug(Debug) ->
    dbg:tracer(),
    dbg:p(all, [call]),
    do_enable_debug(Debug).

do_enable_debug(disable) ->
    dbg:stop();
do_enable_debug([]) ->
    ok;
do_enable_debug([{Level,Modules}|Rest]) 
  when is_atom(Level) andalso is_list(Modules) ->
    case Level of
	all_functions ->
	    lists:foreach(
	      fun(X) -> 
		      dbg:tpl(X, [{'_', [], [{return_trace}]}]) 
	      end, Modules);
	exported_functions -> 
	    lists:foreach(
	      fun(X) ->
		      dbg:tp(X, [{'_', [], [{return_trace}]}]) 
	      end, Modules);
	disable ->
	    lists:foreach(
	      fun(X) -> 
		      dbg:ctp(X) 
	      end, Modules);
	_ ->
	    ok
    end,
    do_enable_debug(Rest).


error_log(ConfigDB, Report) ->
    case lookup(ConfigDB, logger) of
        undefined ->
            mod_error_logging(mod_log, ConfigDB, Report),
            mod_error_logging(mod_disk_log, ConfigDB, Report);
        Logger  ->
            Domain = proplists:get_value(error, Logger),
            httpd_logger:log(error, Report, Domain),
            %% Backwards compat
            mod_error_logging(mod_log, ConfigDB, Report),
            mod_error_logging(mod_disk_log, ConfigDB, Report)
    end.

mod_error_logging(Mod, ConfigDB, Report) ->
    Modules = httpd_util:lookup(ConfigDB, modules,
				[mod_get, mod_head, mod_log]),
    case lists:member(Mod, Modules) of
	true ->
            %% Make it oneline string for backwards compatibility
            Msg = httpd_logger:format(Report),
            ErrorStr = lists:flatten(logger_formatter:format(#{level => error,
                                                               msg => Msg,
                                                               meta => #{}
                                                              },
                                                             #{template => [msg]})),
            Mod:report_error(ConfigDB, ErrorStr);
	_ ->
	    ok
    end.
