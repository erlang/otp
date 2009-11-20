%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
%% Description: Cookie handling according to RFC 2109

-module(http_cookie).

-include("httpc_internal.hrl").

-export([header/4, cookies/3, open_cookie_db/1, close_cookie_db/1, insert/2]). 

%%%=========================================================================
%%%  API
%%%=========================================================================
header(Scheme, {Host, _}, Path, CookieDb) ->
    case lookup_cookies(Host, Path, CookieDb) of
	[] ->
	    {"cookie", ""};
	Cookies ->
	    {"cookie", cookies_to_string(Scheme, Cookies)}
    end.

cookies(Headers, RequestPath, RequestHost) ->
    Cookies = parse_set_cookies(Headers, {RequestPath, RequestHost}),
    accept_cookies(Cookies, RequestPath, RequestHost).
	
open_cookie_db({{_, only_session_cookies}, SessionDbName}) ->
    EtsDb = ets:new(SessionDbName, [protected, bag,
				    {keypos, #http_cookie.domain}]),
    {undefined, EtsDb};

open_cookie_db({{DbName, Dbdir}, SessionDbName}) ->
    File = filename:join(Dbdir, atom_to_list(DbName)),
    {ok, DetsDb} = dets:open_file(DbName, [{keypos, #http_cookie.domain},
					   {type, bag},
					   {file, File},
					   {ram_file, true}]),
    EtsDb = ets:new(SessionDbName, [protected, bag,
				    {keypos, #http_cookie.domain}]),
    {DetsDb, EtsDb}.

close_cookie_db({undefined, EtsDb}) ->
    ets:delete(EtsDb);

close_cookie_db({DetsDb, EtsDb}) ->
    dets:close(DetsDb),
    ets:delete(EtsDb).

%% If no persistent cookie database is defined we
%% treat all cookies as if they where session cookies. 
insert(Cookie = #http_cookie{max_age = Int}, 
       Dbs = {undefined, _}) when is_integer(Int) ->
    insert(Cookie#http_cookie{max_age = session}, Dbs);

insert(Cookie = #http_cookie{domain = Key, name = Name, 
		    path = Path, max_age = session},
       Db = {_, CookieDb}) ->
    case ets:match_object(CookieDb, #http_cookie{domain = Key,
						 name = Name, 
						 path = Path,
						 _ = '_'}) of
	[] ->
	    ets:insert(CookieDb, Cookie);
	[NewCookie] ->
	    delete(NewCookie, Db),
	    ets:insert(CookieDb, Cookie)
    end,
    ok;
insert(#http_cookie{domain = Key, name = Name, 
		    path = Path, max_age = 0},
       Db = {CookieDb, _}) ->
    case dets:match_object(CookieDb, #http_cookie{domain = Key,
						  name = Name, 
						  path = Path,
						  _ = '_'}) of
	[] ->
	    ok;
	[NewCookie] ->
	    delete(NewCookie, Db)
    end,
    ok;
insert(Cookie = #http_cookie{domain = Key, name = Name, path = Path},
       Db = {CookieDb, _}) ->
    case dets:match_object(CookieDb, #http_cookie{domain = Key,
						  name = Name, 
						  path = Path,
						  _ = '_'}) of
	[] ->
	    dets:insert(CookieDb, Cookie);
	[NewCookie] ->
	    delete(NewCookie, Db),
	    dets:insert(CookieDb, Cookie)
    end,
    ok.

%%%========================================================================
%%% Internal functions
%%%========================================================================
lookup_cookies(Key, {undefined, Ets}) ->
    ets:match_object(Ets, #http_cookie{domain = Key,
				       _ = '_'});
lookup_cookies(Key, {Dets,Ets}) ->
    SessionCookies = ets:match_object(Ets, #http_cookie{domain = Key,
							_ = '_'}),
    Cookies = dets:match_object(Dets, #http_cookie{domain = Key,
						   _ = '_'}),
    Cookies ++ SessionCookies.

delete(Cookie = #http_cookie{max_age = session}, {_, CookieDb}) ->
    ets:delete_object(CookieDb, Cookie);
delete(Cookie, {CookieDb, _}) ->
    dets:delete_object(CookieDb, Cookie).

lookup_cookies(Host, Path, Db) ->
    Cookies = 
	case http_util:is_hostname(Host) of 
	    true ->  
		HostCookies = lookup_cookies(Host, Db),
		[_| DomainParts] = string:tokens(Host, "."),
		lookup_domain_cookies(DomainParts, Db, HostCookies);
	    false -> % IP-adress
		lookup_cookies(Host, Db)
	end,
    ValidCookies = valid_cookies(Cookies, [], Db),
    lists:filter(fun(Cookie) -> 
			 lists:prefix(Cookie#http_cookie.path, Path) 
		 end, ValidCookies).

%% For instance if Host=localhost 
lookup_domain_cookies([], _, AccCookies) ->
    lists:flatten(AccCookies);
%% Top domains can not have cookies
lookup_domain_cookies([_], _, AccCookies) ->
    lists:flatten(AccCookies);
lookup_domain_cookies([Next | DomainParts], CookieDb, AccCookies) ->    
    Domain = merge_domain_parts(DomainParts, [Next ++ "."]),
    lookup_domain_cookies(DomainParts, CookieDb,
			  [lookup_cookies(Domain, CookieDb) 
			   | AccCookies]).

merge_domain_parts([Part], Merged) ->
    lists:flatten(["." | lists:reverse([Part | Merged])]);
merge_domain_parts([Part| Rest], Merged) ->
    merge_domain_parts(Rest, [".", Part | Merged]).

cookies_to_string(Scheme, Cookies = [Cookie | _]) ->
    Version = "$Version=" ++ Cookie#http_cookie.version ++ "; ", 
    cookies_to_string(Scheme, path_sort(Cookies), [Version]).

cookies_to_string(_, [], CookieStrs) ->
    case length(CookieStrs) of
	1 ->
	    "";
	_ ->
	    lists:flatten(lists:reverse(CookieStrs))
    end;

cookies_to_string(https, [Cookie = #http_cookie{secure = true}| Cookies], 
		  CookieStrs) ->
    Str = case Cookies of
	      [] ->
		  cookie_to_string(Cookie);
	      _ ->
		  cookie_to_string(Cookie) ++ "; "
	  end,
    cookies_to_string(https, Cookies, [Str | CookieStrs]);

cookies_to_string(Scheme, [#http_cookie{secure = true}| Cookies],  
		  CookieStrs) ->
    cookies_to_string(Scheme, Cookies, CookieStrs);

cookies_to_string(Scheme, [Cookie | Cookies], CookieStrs) ->
    Str = case Cookies of
	      [] ->
		  cookie_to_string(Cookie);
	      _ ->
		  cookie_to_string(Cookie) ++ "; "
	  end,
    cookies_to_string(Scheme, Cookies, [Str | CookieStrs]).

cookie_to_string(Cookie = #http_cookie{name = Name, value = Value}) ->
    Str = Name ++ "=" ++ Value,
    add_domain(add_path(Str, Cookie), Cookie).
    
add_path(Str, #http_cookie{path_default = true}) ->
    Str;
add_path(Str, #http_cookie{path = Path}) ->
    Str ++ "; $Path=" ++  Path.

add_domain(Str, #http_cookie{domain_default = true}) ->
    Str;
add_domain(Str, #http_cookie{domain = Domain}) ->
    Str ++ "; $Domain=" ++  Domain.

parse_set_cookies(OtherHeaders, DefaultPathDomain) ->
    SetCookieHeaders = lists:foldl(fun({"set-cookie", Value}, Acc) ->  
					   [string:tokens(Value, ",")| Acc];
				      (_, Acc) ->
					   Acc
				   end, [], OtherHeaders),
    
    lists:flatten(lists:map(fun(CookieHeader) ->
				    NewHeader = 
					fix_netscape_cookie(CookieHeader, 
							    []),
				    parse_set_cookie(NewHeader, [], 
						     DefaultPathDomain) end,
			    SetCookieHeaders)).

parse_set_cookie([], AccCookies, _) ->    
    AccCookies;
parse_set_cookie([CookieHeader | CookieHeaders], AccCookies, 
		 Defaults = {DefaultPath, DefaultDomain}) -> 
    [CookieStr | Attributes] = case string:tokens(CookieHeader, ";") of
				   [CStr] ->
				       [CStr, ""];
				   [CStr | Attr] ->
				       [CStr, Attr]
			       end,
    Pos = string:chr(CookieStr, $=),
    Name = string:substr(CookieStr, 1, Pos - 1),
    Value = string:substr(CookieStr, Pos + 1),
    Cookie = #http_cookie{name = string:strip(Name), 
			  value = string:strip(Value)},
    NewAttributes = parse_set_cookie_attributes(Attributes),
    TmpCookie = cookie_attributes(NewAttributes, Cookie),
    %% Add runtime defult values if necessary
    NewCookie = domain_default(path_default(TmpCookie, DefaultPath), 
			       DefaultDomain),
    parse_set_cookie(CookieHeaders, [NewCookie | AccCookies], Defaults).

parse_set_cookie_attributes([]) ->
    [];
parse_set_cookie_attributes([Attributes]) ->
    lists:map(fun(Attr) -> 
		      [AttrName, AttrValue] = 
			  case string:tokens(Attr, "=") of
			      %% All attributes have the form
			      %% Name=Value except "secure"!
			      [Name] -> 
				  [Name, ""];
			      [Name, Value] ->
				  [Name, Value];
			      %% Anything not expected will be
			      %% disregarded
			      _ -> 
				  ["Dummy",""]
			  end,
		      {http_util:to_lower(string:strip(AttrName)), 
		       string:strip(AttrValue)}
	      end, Attributes).

cookie_attributes([], Cookie) ->
    Cookie;
cookie_attributes([{"comment", Value}| Attributes], Cookie) ->
    cookie_attributes(Attributes, 
				Cookie#http_cookie{comment = Value});
cookie_attributes([{"domain", Value}| Attributes], Cookie) ->
    cookie_attributes(Attributes, 
				Cookie#http_cookie{domain = Value});
cookie_attributes([{"max-age", Value}| Attributes], Cookie) ->
    ExpireTime = cookie_expires(list_to_integer(Value)),
    cookie_attributes(Attributes, 
				Cookie#http_cookie{max_age = ExpireTime});
%% Backwards compatibility with netscape cookies
cookie_attributes([{"expires", Value}| Attributes], Cookie) ->
    Time = http_util:convert_netscapecookie_date(Value),
    ExpireTime = calendar:datetime_to_gregorian_seconds(Time),
    cookie_attributes(Attributes, 
				Cookie#http_cookie{max_age = ExpireTime});
cookie_attributes([{"path", Value}| Attributes], Cookie) ->
    cookie_attributes(Attributes, 
				Cookie#http_cookie{path = Value});
cookie_attributes([{"secure", _}| Attributes], Cookie) ->
    cookie_attributes(Attributes, 
				Cookie#http_cookie{secure = true});
cookie_attributes([{"version", Value}| Attributes], Cookie) ->
    cookie_attributes(Attributes, 
				Cookie#http_cookie{version = Value});
%% Disregard unknown attributes.
cookie_attributes([_| Attributes], Cookie) ->
    cookie_attributes(Attributes, Cookie).
   
domain_default(Cookie = #http_cookie{domain = undefined}, 
	       DefaultDomain) ->
    Cookie#http_cookie{domain = DefaultDomain, domain_default = true};
domain_default(Cookie, _) ->
    Cookie.

path_default(Cookie = #http_cookie{path = undefined}, 
	     DefaultPath) ->
    Cookie#http_cookie{path = skip_right_most_slash(DefaultPath),
		       path_default = true};
path_default(Cookie, _) ->
    Cookie.

%% Note: if the path is only / that / will be keept
skip_right_most_slash("/") ->
    "/";
skip_right_most_slash(Str) ->
    string:strip(Str, right, $/).

accept_cookies(Cookies, RequestPath, RequestHost) ->
    lists:filter(fun(Cookie) ->
			 accept_cookie(Cookie, RequestPath, RequestHost)
		 end, Cookies).

accept_cookie(Cookie, RequestPath, RequestHost) ->
    accept_path(Cookie, RequestPath) and accept_domain(Cookie, RequestHost).

accept_path(#http_cookie{path = Path}, RequestPath) ->
    lists:prefix(Path, RequestPath).

accept_domain(#http_cookie{domain = RequestHost}, RequestHost) ->
    true;

accept_domain(#http_cookie{domain = Domain}, RequestHost) ->
    HostCheck = case http_util:is_hostname(RequestHost) of 
		    true ->  		
			(lists:suffix(Domain, RequestHost) andalso
			 (not 
			  lists:member($., 
				       string:substr(RequestHost, 1,
						     (length(RequestHost) -
						      length(Domain))))));
		    false -> 
			false
		end,
    HostCheck andalso (hd(Domain) == $.) 
	andalso (length(string:tokens(Domain, ".")) > 1).

cookie_expires(0) ->
    0;
cookie_expires(DeltaSec) ->
    NowSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    NowSec + DeltaSec.

is_cookie_expired(#http_cookie{max_age = session}) ->
    false;
is_cookie_expired(#http_cookie{max_age = ExpireTime}) ->
    NowSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    ExpireTime - NowSec =< 0.

valid_cookies([], Valid, _) ->
    Valid;

valid_cookies([Cookie | Cookies], Valid, Db) ->
    case is_cookie_expired(Cookie) of
	true ->
	    delete(Cookie, Db),
	    valid_cookies(Cookies, Valid, Db);
	false ->
	    valid_cookies(Cookies, [Cookie | Valid], Db)
    end.
    
path_sort(Cookies)->
    lists:reverse(lists:keysort(#http_cookie.path, Cookies)).


%%  Informally, the Set-Cookie response header comprises the token
%%  Set-Cookie:, followed by a comma-separated list of one or more
%%  cookies. Netscape cookies expires attribute may also have a
%% , in this case the header list will have been incorrectly split
%% in parse_set_cookies/2 this functions fixs that problem.
fix_netscape_cookie([Cookie1, Cookie2 | Rest], Acc) ->
    case inets_regexp:match(Cookie1, "expires=") of
	{_, _, _} ->
	    fix_netscape_cookie(Rest, [Cookie1 ++ Cookie2 | Acc]);
	nomatch ->
	    fix_netscape_cookie([Cookie2 |Rest], [Cookie1| Acc])
    end;
fix_netscape_cookie([Cookie | Rest], Acc) ->
    fix_netscape_cookie(Rest, [Cookie | Acc]);

fix_netscape_cookie([], Acc) ->
    Acc.
