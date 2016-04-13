%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%% Description: Cookie handling according to RFC 2109

%% The syntax for the Set-Cookie response header is
%% 
%% set-cookie      =       "Set-Cookie:" cookies
%% cookies         =       1#cookie
%% cookie          =       NAME "=" VALUE *(";" cookie-av)
%% NAME            =       attr
%% VALUE           =       value
%% cookie-av       =       "Comment" "=" value
%%                 |       "Domain" "=" value
%%                 |       "Max-Age" "=" value
%%                 |       "Path" "=" value
%%                 |       "Secure"
%%                 |       "Version" "=" 1*DIGIT


%% application:start(inets).
%% httpc:set_options([{cookies, enabled}, {proxy, {{"www-proxy.ericsson.se",8080}, ["*.ericsson.se"]}}]).
%% (catch httpc:request("http://www.expedia.com")).

-module(httpc_cookie).

-include("httpc_internal.hrl").

-export([open_db/3, close_db/1, insert/2, header/4, cookies/3]). 
-export([reset_db/1, which_cookies/1]). 
-export([image_of/2, print/2]). 

-record(cookie_db, {db, session_db}).


%%%=========================================================================
%%%  API
%%%=========================================================================

%%--------------------------------------------------------------------
%% Func: open_db(DbName, DbDir, SessionDbName) -> #cookie_db{}
%% Purpose: Create the cookie db
%%--------------------------------------------------------------------

open_db(_, only_session_cookies, SessionDbName) ->
    ?hcrt("open (session cookies only) db", 
	  [{session_db_name, SessionDbName}]),
    SessionDb = ets:new(SessionDbName, 
			[protected, bag, {keypos, #http_cookie.domain}]),
    #cookie_db{session_db = SessionDb};

open_db(Name, Dir, SessionDbName) ->
    ?hcrt("open db", 
	  [{name, Name}, {dir, Dir}, {session_db_name, SessionDbName}]),
    File = filename:join(Dir, atom_to_list(Name)),
    case dets:open_file(Name, [{keypos, #http_cookie.domain},
			       {type, bag},
			       {file, File},
			       {ram_file, true}]) of
	{ok, Db} ->
	    SessionDb = ets:new(SessionDbName, 
				[protected, bag, 
				 {keypos, #http_cookie.domain}]),
	    #cookie_db{db = Db, session_db = SessionDb};
	{error, Reason} ->
	    throw({error, {failed_open_file, Name, File, Reason}})
    end.


%%--------------------------------------------------------------------
%% Func: reset_db(CookieDb) -> void()
%% Purpose: Reset (empty) the cookie database
%% 
%%--------------------------------------------------------------------

reset_db(#cookie_db{db = undefined, session_db = SessionDb}) ->
    ets:delete_all_objects(SessionDb),
    ok;
reset_db(#cookie_db{db = Db, session_db = SessionDb}) ->
    dets:delete_all_objects(Db),
    ets:delete_all_objects(SessionDb),
    ok.


%%--------------------------------------------------------------------
%% Func: close_db(CookieDb) -> ok
%% Purpose: Close the cookie db
%%--------------------------------------------------------------------

close_db(#cookie_db{db = Db, session_db = SessionDb}) ->
    ?hcrt("close db", []),
    maybe_dets_close(Db), 
    ets:delete(SessionDb),
    ok.

maybe_dets_close(undefined) ->
    ok;
maybe_dets_close(Db) ->
    dets:close(Db).
    

%%--------------------------------------------------------------------
%% Func: insert(CookieDb, Cookie) -> ok
%% Purpose: insert cookies into the cookie db
%%--------------------------------------------------------------------

%% If no persistent cookie database is defined we
%% treat all cookies as if they where session cookies. 
insert(#cookie_db{db = undefined} = CookieDb,
       #http_cookie{max_age = Int} = Cookie) when is_integer(Int) ->
    insert(CookieDb, Cookie#http_cookie{max_age = session});

insert(#cookie_db{session_db = SessionDb} = CookieDb, 
       #http_cookie{domain  = Key, 
		    name    = Name, 
		    path    = Path, 
		    max_age = session} = Cookie) ->
    ?hcrt("insert session cookie", [{cookie, Cookie}]),
    Pattern = #http_cookie{domain = Key, name = Name, path = Path, _ = '_'}, 
    case ets:match_object(SessionDb, Pattern) of
	[] ->
	    ets:insert(SessionDb, Cookie);
	[NewCookie] ->
	    delete(CookieDb, NewCookie),
	    ets:insert(SessionDb, Cookie)
    end,
    ok;
insert(#cookie_db{db = Db} = CookieDb,
       #http_cookie{domain  = Key, 
		    name    = Name, 
		    path    = Path, 
		    max_age = 0}) ->
    ?hcrt("insert cookie", [{domain, Key}, {name, Name}, {path, Path}]),
    Pattern = #http_cookie{domain = Key, name = Name, path = Path, _ = '_'}, 
    case dets:match_object(Db, Pattern) of
	[] ->
	    ok;
	[NewCookie] ->
	    delete(CookieDb, NewCookie)
    end,
    ok;
insert(#cookie_db{db = Db} = CookieDb,
       #http_cookie{domain = Key, name = Name, path = Path} = Cookie) ->
    ?hcrt("insert cookie", [{cookie, Cookie}]),
    Pattern = #http_cookie{domain = Key,
			   name = Name, 
			   path = Path,
			   _ = '_'}, 
    case dets:match_object(Db, Pattern) of
	[] ->
	    dets:insert(Db, Cookie);
	[OldCookie] ->
	    delete(CookieDb, OldCookie),
	    dets:insert(Db, Cookie)
    end,
    ok.



%%--------------------------------------------------------------------
%% Func: header(CookieDb) -> ok
%% Purpose: Cookies
%%--------------------------------------------------------------------

header(CookieDb, Scheme, {Host, _}, Path) ->
    ?hcrd("header", [{scheme, Scheme}, {host, Host}, {path, Path}]),
    case lookup_cookies(CookieDb, Host, Path) of
	[] ->
	    {"cookie", ""};
	Cookies ->
	    %% print_cookies("Header Cookies", Cookies), 
	    {"cookie", cookies_to_string(Scheme, Cookies)}
    end.


%%--------------------------------------------------------------------
%% Func: cookies(Headers, RequestPath, RequestHost) -> [cookie()]
%% Purpose: Which cookies are stored
%%--------------------------------------------------------------------

cookies(Headers, RequestPath, RequestHost) ->

    ?hcrt("cookies", [{headers,      Headers}, 
		      {request_path, RequestPath}, 
		      {request_host, RequestHost}]),

    Cookies = parse_set_cookies(Headers, {RequestPath, RequestHost}),

    %% print_cookies("Parsed Cookies", Cookies),

    AcceptedCookies = accept_cookies(Cookies, RequestPath, RequestHost),

    %% print_cookies("Accepted Cookies", AcceptedCookies),

    AcceptedCookies.
	

%%--------------------------------------------------------------------
%% Func: which_cookies(CookieDb) -> [cookie()]
%% Purpose: For test and debug purpose, 
%%          dump the entire cookie database
%%--------------------------------------------------------------------

which_cookies(#cookie_db{db = undefined, session_db = SessionDb}) ->
    SessionCookies = ets:tab2list(SessionDb),
    [{session_cookies, SessionCookies}];
which_cookies(#cookie_db{db = Db, session_db = SessionDb}) ->
    Cookies        = dets:match_object(Db, '_'), 
    SessionCookies = ets:tab2list(SessionDb),
    [{cookies, Cookies}, {session_cookies, SessionCookies}].


%%%========================================================================
%%% Internal functions
%%%========================================================================

delete(#cookie_db{session_db = SessionDb}, 
       #http_cookie{max_age = session} = Cookie) ->
    ets:delete_object(SessionDb, Cookie);
delete(#cookie_db{db = Db}, Cookie) ->
    dets:delete_object(Db, Cookie).


lookup_cookies(#cookie_db{db = undefined, session_db = SessionDb}, Key) ->
    Pattern = #http_cookie{domain = Key, _ = '_'}, 
    Cookies = ets:match_object(SessionDb, Pattern),
    ?hcrt("lookup cookies", [{cookies, Cookies}]),    
    Cookies;

lookup_cookies(#cookie_db{db = Db, session_db = SessionDb}, Key) ->
    Pattern = #http_cookie{domain = Key, _ = '_'}, 
    SessionCookies = ets:match_object(SessionDb, Pattern),
    ?hcrt("lookup cookies", [{session_cookies, SessionCookies}]),    
    Cookies = dets:match_object(Db, Pattern),
    ?hcrt("lookup cookies", [{cookies, Cookies}]),    
    Cookies ++ SessionCookies.


lookup_cookies(CookieDb, Host, Path) ->
    Cookies = 
	case http_util:is_hostname(Host) of 
	    true ->  
		HostCookies = lookup_cookies(CookieDb, Host),
		[_| DomainParts] = string:tokens(Host, "."),
		lookup_domain_cookies(CookieDb, DomainParts, HostCookies);
	    false -> % IP-adress
		lookup_cookies(CookieDb, Host)
	end,
    ValidCookies = valid_cookies(CookieDb, Cookies),
    lists:filter(fun(Cookie) -> 
			 lists:prefix(Cookie#http_cookie.path, Path) 
		 end, ValidCookies).

%% For instance if Host=localhost 
lookup_domain_cookies(_CookieDb, [], AccCookies) ->
    lists:flatten(AccCookies);

%% Top domains can not have cookies
lookup_domain_cookies(_CookieDb, [_], AccCookies) ->
    lists:flatten(AccCookies);

lookup_domain_cookies(CookieDb, [Next | DomainParts], AccCookies) ->    
    Domain = merge_domain_parts(DomainParts, [Next ++ "."]),
    lookup_domain_cookies(CookieDb, DomainParts, 
			  [lookup_cookies(CookieDb, Domain) | AccCookies]).

merge_domain_parts([Part], Merged) ->
    lists:flatten(["." | lists:reverse([Part | Merged])]);
merge_domain_parts([Part| Rest], Merged) ->
    merge_domain_parts(Rest, [".", Part | Merged]).

cookies_to_string(Scheme, [Cookie | _] = Cookies) ->
    Version = "$Version=" ++ Cookie#http_cookie.version ++ "; ", 
    cookies_to_string(Scheme, path_sort(Cookies), [Version]).

cookies_to_string(_, [], CookieStrs) ->
    case length(CookieStrs) of
	1 ->
	    "";
	_ ->
	    lists:flatten(lists:reverse(CookieStrs))
    end;

cookies_to_string(https = Scheme, 
		  [#http_cookie{secure = true} = Cookie| Cookies], 
		  CookieStrs) ->
    Str = case Cookies of
	      [] ->
		  cookie_to_string(Cookie);
	      _ ->
		  cookie_to_string(Cookie) ++ "; "
	  end,
    cookies_to_string(Scheme, Cookies, [Str | CookieStrs]);

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

cookie_to_string(#http_cookie{name = Name, value = Value} = Cookie) ->
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

is_set_cookie_valid("") ->
    %% an empty Set-Cookie header is not valid
    false;
is_set_cookie_valid([$=|_]) ->
    %% a Set-Cookie header without name is not valid
    false;
is_set_cookie_valid(SetCookieHeader) ->
    %% a Set-Cookie header without name/value is not valid
    case string:chr(SetCookieHeader, $=) of
        0 -> false;
        _ -> true
    end.

parse_set_cookies(CookieHeaders, DefaultPathDomain) ->
    %% filter invalid Set-Cookie headers
    SetCookieHeaders = [Value || {"set-cookie", Value} <- CookieHeaders,
                                 is_set_cookie_valid(Value)],
    Cookies = [parse_set_cookie(SetCookieHeader, DefaultPathDomain) || 
		  SetCookieHeader <- SetCookieHeaders],
    %% print_cookies("Parsed Cookies", Cookies),
    Cookies.

parse_set_cookie(CookieHeader, {DefaultPath, DefaultDomain}) ->
    %% io:format("Raw Cookie: ~s~n", [CookieHeader]), 
    Pos             = string:chr(CookieHeader, $=),
    Name            = string:substr(CookieHeader, 1, Pos - 1),
    {Value, Attrs}  = 
	case string:substr(CookieHeader, Pos + 1) of
	    [] ->
		{"", ""};
	    [$;|ValueAndAttrs] ->
		{"", string:tokens(ValueAndAttrs, ";")};
	    ValueAndAttrs ->
		[V | A] = string:tokens(ValueAndAttrs, ";"), 
		{V, A}
	end,
    Cookie          = #http_cookie{name  = string:strip(Name), 
				   value = string:strip(Value)},
    Attributes      = parse_set_cookie_attributes(Attrs),
    TmpCookie       = cookie_attributes(Attributes, Cookie),
    %% Add runtime defult values if necessary
    NewCookie       = domain_default(path_default(TmpCookie, DefaultPath), 
				     DefaultDomain),
    NewCookie.
    
parse_set_cookie_attributes(Attributes) when is_list(Attributes) ->
    [parse_set_cookie_attribute(A) || A <- Attributes].

parse_set_cookie_attribute(Attribute) ->
    {AName, AValue} = 
	case string:tokens(Attribute, "=") of
	    %% All attributes have the form
	    %% Name=Value except "secure"!
	    [Name] -> 
		{Name, ""};
	    [Name, Value] ->
		{Name, Value};
	    %% Anything not expected will be
	    %% disregarded
	    _ -> 
		{"Dummy", ""}
	end,
    StrippedName  = http_util:to_lower(string:strip(AName)),
    StrippedValue = string:strip(AValue),
    {StrippedName, StrippedValue}.

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
    try http_util:convert_netscapecookie_date(Value) of
	Time ->
	    ExpireTime = calendar:datetime_to_gregorian_seconds(Time),
	    cookie_attributes(Attributes, 
			      Cookie#http_cookie{max_age = ExpireTime})
    catch 
	_:_ ->
	    cookie_attributes(Attributes, Cookie)
    end;
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

path_default(#http_cookie{path = undefined} = Cookie, DefaultPath) ->
    Cookie#http_cookie{path = skip_right_most_slash(DefaultPath),
		       path_default = true};
path_default(Cookie, _) ->
    Cookie.

%% Note: if the path is only / that / will be kept
skip_right_most_slash("/") ->
    "/";
skip_right_most_slash(Str) ->
    string:strip(Str, right, $/).

accept_cookies(Cookies, RequestPath, RequestHost) ->
    lists:filter(fun(Cookie) ->
			 accept_cookie(Cookie, RequestPath, RequestHost)
		 end, Cookies).

accept_cookie(Cookie, RequestPath, RequestHost) ->
    Accepted = 
	accept_path(Cookie, RequestPath) andalso 
	accept_domain(Cookie, RequestHost),
    Accepted.

accept_path(#http_cookie{path = Path}, RequestPath) ->
    lists:prefix(Path, RequestPath).

accept_domain(#http_cookie{domain = RequestHost}, RequestHost) ->
    true;

accept_domain(#http_cookie{domain = Domain}, RequestHost) ->
    HostCheck = 
	case http_util:is_hostname(RequestHost) of 
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
    HostCheck 
	andalso (hd(Domain) =:= $.) 
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


valid_cookies(Db, Cookies) ->
    valid_cookies(Db, Cookies, []).

valid_cookies(_Db, [], Valid) ->
    Valid;

valid_cookies(Db, [Cookie | Cookies], Valid) ->
    case is_cookie_expired(Cookie) of
	true ->
	    delete(Db, Cookie),
	    valid_cookies(Db, Cookies, Valid);
	false ->
	    valid_cookies(Db, Cookies, [Cookie | Valid])
    end.
    
path_sort(Cookies)->
    lists:reverse(lists:keysort(#http_cookie.path, Cookies)).


%% print_cookies(Header, Cookies) ->
%%     io:format("~s:~n", [Header]),
%%     Prefix = "   ", 
%%     lists:foreach(fun(Cookie) -> print(Prefix, Cookie) end, Cookies).

image_of(Prefix, 
	 #http_cookie{domain         = Domain,
		      domain_default = DomainDef,
		      name           = Name,
		      value          = Value,
		      comment        = Comment,
		      max_age        = MaxAge,
		      path           = Path, 
		      path_default   = PathDef,
		      secure         = Sec,
		      version        = Version}) ->
    lists:flatten(
      io_lib:format("~sCookie ~s: "
		    "~n~s   Value:     ~p"
		    "~n~s   Domain:    ~p"
		    "~n~s   DomainDef: ~p"
		    "~n~s   Comment:   ~p"
		    "~n~s   MaxAge:    ~p"
		    "~n~s   Path:      ~p"
		    "~n~s   PathDef:   ~p"
		    "~n~s   Secure:    ~p"
		    "~n~s   Version:   ~p", 
		    [Prefix, Name, 
		     Prefix, Value, 
		     Prefix, Domain, 
		     Prefix, DomainDef, 
		     Prefix, Comment, 
		     Prefix, MaxAge, 
		     Prefix, Path, 
		     Prefix, PathDef, 
		     Prefix, Sec, 
		     Prefix, Version])).

print(Prefix, Cookie) when is_record(Cookie, http_cookie) ->
    io:format("~s~n", [image_of(Prefix, Cookie)]).
