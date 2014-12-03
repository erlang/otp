%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2014. All Rights Reserved.
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

-module(eldap_basic_SUITE).

-compile(export_all).

%%-include_lib("common_test/include/ct.hrl").
-include_lib("test_server/include/test_server.hrl").
-include_lib("eldap/include/eldap.hrl").

-define(TIMEOUT, 120000). % 2 min

all() ->
    [app,
     appup,
     {group, plain_api},
     {group, ssl_api},
     {group, start_tls_api}
    ].

groups() ->
    [{plain_api,     [], [{group,api}]},
     {ssl_api,       [], [{group,api}, start_tls_on_ssl_should_fail]},
     {start_tls_api, [], [{group,api}, start_tls_twice_should_fail]},

     {api, [], [{group,api_not_bound},
		{group,api_bound}]},

     {api_not_bound, [], [elementary_search, search_non_existant, 
			  add_when_not_bound,
			  bind]},
     {api_bound, [], [add_when_bound, 
		      add_already_exists,
		      more_add, 
		      search_filter_equalityMatch,
		      search_filter_substring_any,
		      search_filter_initial,
		      search_filter_final,
		      search_filter_and,
		      search_filter_or,
		      search_filter_and_not,
		      search_two_hits,
		      modify,
		      delete,
		      modify_dn_delete_old,
		      modify_dn_keep_old]}
    ].

init_per_suite(Config) ->
    SSL_started =
	try ssl:start()
	of
	    ok -> true;
	    {error,{already_started,ssl}} -> true
	catch
	    Error:Reason ->
		ct:comment("ssl failed to start"),
		ct:log("init_per_suite failed to start ssl Error=~p Reason=~p", [Error, Reason]),
		false
	end,

    case SSL_started of
	true -> make_certs:all("/dev/null", 
			       filename:join(?config(data_dir,Config), "certs"));
	false -> ok
    end,

    LDAP_server =  find_first_server(false, [{config,eldap_server}, {config,ldap_server}, {"localhost",9876}]),
    LDAPS_server = 
	case SSL_started of
	    true ->
		find_first_server(true,  [{config,ldaps_server}, {"localhost",9877}]);
	    false ->
		undefined
	end,
    [{ssl_available, SSL_started},
     {ldap_server,   LDAP_server},
     {ldaps_server,  LDAPS_server} | Config].

end_per_suite(_Config) ->
    ssl:stop().


init_per_group(plain_api, Config0) -> 
    case ?config(ldap_server,Config0) of
	undefined -> 
	    {skip, "LDAP server not availble"};
	Server = {Host,Port} -> 
	    ct:comment("ldap://~s:~p",[Host,Port]),
	    initialize_db([{server,Server}, {ssl_flag,false}, {start_tls,false} | Config0])
    end;
init_per_group(ssl_api, Config0) -> 
    case ?config(ldaps_server,Config0) of
	undefined -> 
	    {skip, "LDAPS server not availble"};
	Server = {Host,Port} ->
	    ct:comment("ldaps://~s:~p",[Host,Port]),
	    initialize_db([{server,Server}, {ssl_flag,true}, {start_tls,false} | Config0])
    end;
init_per_group(start_tls_api, Config0) -> 
    case {?config(ldap_server,Config0), ?config(ssl_available,Config0)} of
	{undefined,true} -> 
	    {skip, "LDAP server not availble"};
	{_,false} -> 
	    {skip, "TLS not availble"};
	{Server={Host,Port}, true} ->
	    ct:comment("ldap://~s:~p + start_tls",[Host,Port]),
	    Config = [{server,Server}, {ssl_flag,false} | Config0],
	    case supported_extension("1.3.6.1.4.1.1466.20037", Config) of
		true -> initialize_db([{start_tls,true} | Config]);
		false -> {skip, "start_tls not supported by server"}
	    end
    end;
init_per_group(_, Config) -> 
    Config.

end_per_group(plain_api,     Config) -> clear_db(Config);
end_per_group(ssl_api,       Config) -> clear_db(Config);
end_per_group(start_tls_api, Config) -> clear_db(Config);
end_per_group(_Group, Config) -> Config.


init_per_testcase(_, Config) ->
    case proplists:get_value(name,?config(tc_group_properties, Config)) of
	api_not_bound ->
	    {ok,H} = open(Config),
	    [{handle,H} | Config];
	api_bound ->
	    {ok,H} = open(Config),
	    ok = eldap:simple_bind(H,
				   "cn=Manager,dc=ericsson,dc=se",
				   "hejsan"),
	    [{handle,H} | Config];
	_Name ->
	    Config
    end.

end_per_testcase(_, Config) ->
    case ?config(handle,Config) of
	undefined ->
	    Config;
	H ->
	    eldap:close(H)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Test cases
%%%

%%%----------------------------------------------------------------
%%% Test that the eldap app file is ok
app(Config) when is_list(Config) ->
    ok = test_server:app_test(eldap).

%%%----------------------------------------------------------------
%%% Test that the eldap appup file is ok
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(eldap).

%%%----------------------------------------------------------------
%%% Basic test that all api functions works as expected

%%%----------------------------------------------------------------
elementary_search(Config) ->
    {ok, #eldap_search_result{entries=[_]}} = 
	eldap:search(?config(handle,Config),
		     #eldap_search{base  = ?config(eldap_path, Config),
				   filter= eldap:present("objectclass"),
				   scope = eldap:wholeSubtree()}).

%%%----------------------------------------------------------------
search_non_existant(Config) ->
    {error, noSuchObject} = 
	eldap:search(?config(handle,Config),
		     #eldap_search{base  = "cn=Bar," ++ ?config(eldap_path, Config),
				   filter= eldap:present("objectclass"),
				   scope = eldap:wholeSubtree()}).

%%%----------------------------------------------------------------
add_when_not_bound(Config) ->
    {error, _} = eldap:add(?config(handle,Config),
			   "cn=Jonas Jonsson," ++ ?config(eldap_path, Config),
			   [{"objectclass", ["person"]},
			    {"cn", ["Jonas Jonsson"]}, 
			    {"sn", ["Jonsson"]}]).

%%%----------------------------------------------------------------
bind(Config) ->
    ok = eldap:simple_bind(?config(handle,Config),
			   "cn=Manager,dc=ericsson,dc=se",
			   "hejsan").

%%%----------------------------------------------------------------
add_when_bound(Config) ->
    ok = eldap:add(?config(handle, Config),
		   "cn=Jonas Jonsson," ++  ?config(eldap_path, Config),
		   [{"objectclass", ["person"]},
		    {"cn", ["Jonas Jonsson"]}, 
		    {"sn", ["Jonsson"]}]).

%%%----------------------------------------------------------------
add_already_exists(Config) ->
    {error, entryAlreadyExists} = 
	eldap:add(?config(handle, Config),
		  "cn=Jonas Jonsson," ++ ?config(eldap_path, Config),
		  [{"objectclass", ["person"]},
		   {"cn", ["Jonas Jonsson"]}, 
		   {"sn", ["Jonsson"]}]).

%%%----------------------------------------------------------------
more_add(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    ok = eldap:add(H, "cn=Foo Bar," ++ BasePath,
		   [{"objectclass", ["person"]},
		    {"cn", ["Foo Bar"]}, 
		    {"sn", ["Bar"]}, 
		    {"telephoneNumber", ["555-1232", "555-5432"]}]),
    ok = eldap:add(H, "ou=Team," ++ BasePath,
		   [{"objectclass", ["organizationalUnit"]},
		    {"ou", ["Team"]}]).


%%%----------------------------------------------------------------
search_filter_equalityMatch(Config) ->
    BasePath = ?config(eldap_path, Config),
    ExpectedDN = "cn=Jonas Jonsson," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} = 
	eldap:search(?config(handle, Config),
		     #eldap_search{base = BasePath,
				   filter = eldap:equalityMatch("sn", "Jonsson"),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_substring_any(Config) ->
    BasePath = ?config(eldap_path, Config),
    ExpectedDN = "cn=Jonas Jonsson," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} = 
	eldap:search(?config(handle, Config),
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "ss"}]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_initial(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    ExpectedDN = "cn=Foo Bar," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{initial, "B"}]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_final(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    ExpectedDN = "cn=Foo Bar," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{final, "r"}]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_and(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    ExpectedDN = "cn=Foo Bar," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:'and'([eldap:substrings("sn", [{any, "a"}]),
							 eldap:equalityMatch("cn","Foo Bar")]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_or(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    ExpectedDNs = lists:sort(["cn=Foo Bar," ++ BasePath,
			      "ou=Team," ++ BasePath]),
    {ok, #eldap_search_result{entries=Es}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:'or'([eldap:substrings("sn", [{any, "a"}]),
							eldap:equalityMatch("ou","Team")]),
				   scope=eldap:singleLevel()}),
    ExpectedDNs = lists:sort([DN || #eldap_entry{object_name=DN} <- Es]).

%%%----------------------------------------------------------------
search_filter_and_not(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    {ok, #eldap_search_result{entries=[]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:'and'([eldap:substrings("sn", [{any, "a"}]),
							 eldap:'not'(
							   eldap:equalityMatch("cn","Foo Bar")
							  )]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_two_hits(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    DN1 = "cn=Santa Claus," ++ BasePath,
    DN2 = "cn=Jultomten," ++ BasePath,
    %% Add two objects:
    ok = eldap:add(H, DN1,
		   [{"objectclass", ["person"]},
		    {"cn", ["Santa Claus"]}, 
		    {"sn", ["Santa"]},
		    {"description", ["USA"]}]),
    ok = eldap:add(H, DN2,
		   [{"objectclass", ["person"]},
		    {"cn", ["Jultomten"]}, 
		    {"sn", ["Tomten"]},
		    {"description", ["Sweden"]}]),

    %% Search for them:
    {ok, #eldap_search_result{entries=Es}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:present("description"),
				   scope=eldap:singleLevel()}),

    %% And check that they are the expected ones:
    ExpectedDNs = lists:sort([DN1, DN2]),
    ExpectedDNs = lists:sort([D || #eldap_entry{object_name=D} <- Es]),

    %% Restore the database:
    [ok=eldap:delete(H,DN) || DN <- ExpectedDNs].

%%%----------------------------------------------------------------
modify(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    %% The object to modify
    DN = "cn=Foo Bar," ++ BasePath,

    %% Save a copy to restore later:
    {ok,OriginalAttrs} = attributes(H, DN),
    
    %% Do a change
    Mod = [eldap:mod_replace("telephoneNumber", ["555-12345"]),
	   eldap:mod_add("description", ["Nice guy"])],
    ok = eldap:modify(H, DN, Mod),

    %% Check that the object was changed
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:equalityMatch("telephoneNumber", "555-12345"),
				   scope=eldap:singleLevel()}),

    %% Do another type of change
    ok = eldap:modify(H, DN, [eldap:mod_delete("telephoneNumber", [])]),
    %% and check that it worked by repeating the test above
    {ok, #eldap_search_result{entries=[]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:equalityMatch("telephoneNumber", "555-12345"),
				   scope=eldap:singleLevel()}),
    %% restore the orignal version:
    restore_original_object(H, DN, OriginalAttrs).

%%%----------------------------------------------------------------
delete(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    %% The element to play with:
    DN = "cn=Jonas Jonsson," ++ BasePath,

    %% Prove that the element is present before deletion
    {ok,OriginalAttrs} = attributes(H, DN),

    %% Do what the test has to do:
    ok = eldap:delete(H, DN),
    %% check that it really was deleted:
    {error, noSuchObject} = eldap:delete(H, DN),

    %% And restore the object for subsequent tests
    restore_original_object(H, DN, OriginalAttrs).

%%%----------------------------------------------------------------
modify_dn_delete_old(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    OrigCN = "Foo Bar",
    OriginalRDN = "cn="++OrigCN,
    DN = OriginalRDN ++ "," ++ BasePath,
    NewCN = "Niclas Andre",
    NewRDN = "cn="++NewCN,
    NewDN = NewRDN ++ "," ++BasePath,

    %% Check that the object to modify_dn of exists:
    {ok,OriginalAttrs} = attributes(H, DN),
    CN_orig = lists:sort(proplists:get_value("cn",OriginalAttrs)),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}),
    
    %% Modify and delete the old one:
    ok = eldap:modify_dn(H, DN, NewRDN, true, ""),
    
    %% Check that DN was modified and the old one was deleted:
    {ok,NewAttrs} = attributes(H, NewDN),
    CN_new = lists:sort(proplists:get_value("cn",NewAttrs)),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=NewDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}),
    %% What we expect:
    CN_new = lists:sort([NewCN | CN_orig -- [OrigCN]]),
    
    %% Change back:
    ok = eldap:modify_dn(H, NewDN, OriginalRDN, true, ""),

    %% Check that DN was modified and the new one was deleted:
    {ok,SameAsOriginalAttrs} = attributes(H, DN),
    CN_orig = lists:sort(proplists:get_value("cn",SameAsOriginalAttrs)),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}).
    
%%%----------------------------------------------------------------
modify_dn_keep_old(Config) ->
    H = ?config(handle, Config),
    BasePath = ?config(eldap_path, Config),
    OriginalRDN = "cn=Foo Bar",
    DN = OriginalRDN ++ "," ++ BasePath,
    NewCN = "Niclas Andre",
    NewRDN = "cn="++NewCN,
    NewDN = NewRDN ++ "," ++BasePath,

    %% Check that the object to modify_dn of exists but the new one does not:
    {ok,OriginalAttrs} = attributes(H, DN),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}),
    
    %% Modify but keep the old "cn" attr:
    ok = eldap:modify_dn(H, DN, NewRDN, false, ""),
    
    %% Check that DN was modified and the old CN entry is not deleted:
    {ok,NewAttrs} = attributes(H, NewDN),    
    CN_orig = proplists:get_value("cn",OriginalAttrs),
    CN_new = proplists:get_value("cn",NewAttrs),
    Expected = lists:sort([NewCN|CN_orig]),
    Expected = lists:sort(CN_new),
    
    %% Restore db:
    ok = eldap:delete(H, NewDN),
    restore_original_object(H, DN, OriginalAttrs).

%%%----------------------------------------------------------------
%%% Test that start_tls on an already upgraded connection makes no noise
start_tls_twice_should_fail(Config) ->
    {ok,H} = open_bind(Config),
    {error,tls_already_started} = eldap:start_tls(H, []),
    _Ok = eldap:close(H),
    ok.

%%%----------------------------------------------------------------
%%% Test that start_tls on an ldaps connection fails
start_tls_on_ssl_should_fail(Config) ->
    {ok,H} = open_bind(Config),
    {error,tls_already_started} = eldap:start_tls(H, []),
    _Ok = eldap:close(H),
    ok.

%%%****************************************************************
%%% Private

attributes(H, DN) ->
    case eldap:search(H,
		     #eldap_search{base  = DN,
				   filter= eldap:present("objectclass"),
				   scope = eldap:wholeSubtree()}) of
	{ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN,
							attributes=OriginalAttrs}]}} ->
	    {ok, OriginalAttrs};
	Other ->
	    Other
    end.

restore_original_object(H, DN, Attrs) ->
    eldap:delete(H, DN),
    ok = eldap:add(H, DN, Attrs).


find_first_server(UseSSL, [{config,Key}|Ss]) ->
    case ct:get_config(Key) of
	{Host,Port} ->
	    find_first_server(UseSSL, [{Host,Port}|Ss]);
	undefined ->
	    find_first_server(UseSSL, Ss)
    end;
find_first_server(UseSSL, [{Host,Port}|Ss]) ->
    case eldap:open([Host],[{port,Port},{ssl,UseSSL}]) of
	{ok,H} when UseSSL==false, Ss=/=[] ->
	    case eldap:start_tls(H,[]) of
		ok -> 
		    _Ok = eldap:close(H),
		    {Host,Port};
		_ ->
		    _Ok = eldap:close(H),
		    find_first_server(UseSSL, Ss++[{Host,Port}])
	    end;
	{ok,H} ->
	    _Ok = eldap:close(H),
	    {Host,Port};
	_ ->
	    find_first_server(UseSSL, Ss)
    end;
find_first_server(_, []) ->
    undefined.

initialize_db(Config) ->
    case {open_bind(Config), inet:gethostname()} of
	{{ok,H}, {ok,MyHost}} ->
	    Path = "dc="++MyHost++",dc=ericsson,dc=se",
	    delete_old_contents(H, Path),
	    add_new_contents(H, Path, MyHost),
	    _Ok = eldap:close(H),
	    [{eldap_path,Path}|Config];
	Other ->
	    ct:fail("initialize_db failed: ~p",[Other])
    end.

clear_db(Config) ->
    {ok,H} = open_bind(Config),
    Path = ?config(eldap_path, Config),
    delete_old_contents(H, Path),
    _Ok = eldap:close(H),
    Config.

delete_old_contents(H, Path) ->
    case eldap:search(H, [{base,  Path},
			  {filter, eldap:present("objectclass")},
			  {scope,  eldap:wholeSubtree()}])
    of
	{ok, #eldap_search_result{entries=Entries}} ->
	    [ok = eldap:delete(H,DN) || #eldap_entry{object_name=DN} <- Entries];
	_Res -> 
	    ignore
    end.

add_new_contents(H, Path, MyHost) ->
    eldap:add(H,"dc=ericsson,dc=se",
	      [{"objectclass", ["dcObject", "organization"]},
	       {"dc", ["ericsson"]}, 
	       {"o", ["Testing"]}]),
    eldap:add(H,Path,
	      [{"objectclass", ["dcObject", "organization"]},
	       {"dc", [MyHost]}, 
	       {"o", ["Test machine"]}]).



cond_start_tls(H, Config) ->
    case ?config(start_tls,Config) of
	true -> start_tls(H,Config);
	_ -> Config
    end.
	    
start_tls(H, Config) ->
    KeyFile = filename:join([?config(data_dir,Config),
			     "certs/client/key.pem"
			    ]),
    case eldap:start_tls(H, [{keyfile, KeyFile}]) of
	ok ->
	    [{start_tls_success,true} | Config];
	Error ->
	    ct:log("Start_tls on ~p failed: ~p",[?config(url,Config) ,Error]),
	    ct:fail("start_tls failed")
    end.


%%%----------------------------------------------------------------
open_bind(Config) ->
    {ok,H} = open(Config),
    ok = eldap:simple_bind(H, "cn=Manager,dc=ericsson,dc=se", "hejsan"),
    {ok,H}.

open(Config) ->
    {Host,Port} = ?config(server,Config),
    SSLflag = ?config(ssl_flag,Config),
    {ok,H} = eldap:open([Host], [{port,Port},{ssl,SSLflag}]),
    cond_start_tls(H, Config),
    {ok,H}.

%%%----------------------------------------------------------------
supported_extension(OID, Config) ->
    {ok,H} = open_bind(Config),
    case eldap:search(H, [{scope,  eldap:baseObject()},
			  {filter, eldap:present("objectclass")},
			  {deref,  eldap:neverDerefAliases()},
			  {attributes, ["+"]}]) of
	{ok,R=#eldap_search_result{}} ->
	    _Ok = eldap:close(H),
	    lists:member(OID,
			 [SE || EE <- R#eldap_search_result.entries,
				{"supportedExtension",SEs} <- EE#eldap_entry.attributes,
				SE<-SEs]);
	_ ->
	    _Ok = eldap:close(H),
	    false
    end.
