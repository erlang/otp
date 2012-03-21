%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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

init_per_suite(Config0) ->
    {{EldapHost,Port}, Config1} =
	case catch ct:get_config(eldap_server, undefined) of
	    undefined -> %% Dev test only
		Server = {"localhost", 9876},
		{Server, [{eldap_server, {"localhost", 9876}}|Config0]};
	    {'EXIT', _} -> %% Dev test only
		Server = {"localhost", 9876},
		{Server, [{eldap_server, {"localhost", 9876}}|Config0]};
	    Server ->
		{Server, [{eldap_server, Server}|Config0]}
	end,
    %% Add path for this test run
    try
	{ok, Handle} = eldap:open([EldapHost], [{port, Port}]),
	ok = eldap:simple_bind(Handle, "cn=Manager,dc=ericsson,dc=se", "hejsan"),
	{ok, MyHost} = inet:gethostname(),
	Path = "dc="++MyHost++",dc=ericsson,dc=se",
	Config = [{eldap_path,Path}|Config1],
	eldap:add(Handle,"dc=ericsson,dc=se",
		  [{"objectclass", ["dcObject", "organization"]},
		   {"dc", ["ericsson"]}, {"o", ["Testing"]}]),
	eldap:add(Handle,Path,
		  [{"objectclass", ["dcObject", "organization"]},
		   {"dc", [MyHost]}, {"o", ["Test machine"]}]),
	Config
    catch error:{badmatch,Error} ->
	    io:format("Eldap init error ~p~n ~p~n",[Error, erlang:get_stacktrace()]),
	    {skip, lists:flatten(io_lib:format("Ldap init failed with host ~p", [EldapHost]))}
    end.
end_per_suite(Config) ->
    %% Cleanup everything
    {EHost, Port} = proplists:get_value(eldap_server, Config),
    Path = proplists:get_value(eldap_path, Config),
    {ok, H} = eldap:open([EHost], [{port, Port}]),
    ok = eldap:simple_bind(H, "cn=Manager,dc=ericsson,dc=se", "hejsan"),
    case eldap:search(H, [{base, Path},
			  {filter, eldap:present("objectclass")},
			  {scope,  eldap:wholeSubtree()}])
    of
	{ok, {eldap_search_result, Entries, _}} ->
	    [ok = eldap:delete(H, Entry) || {eldap_entry, Entry, _} <- Entries];
	_ -> ignore
    end,
    ok.

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%% suite() ->

all() ->
    [app,
     api].

app(doc) ->  "Test that the eldap app file is ok";
app(suite) -> [];
app(Config) when is_list(Config) ->
    ok = test_server:app_test(public_key).

api(doc) -> "Basic test that all api functions works as expected";
api(suite) -> [];
api(Config) ->
    {Host,Port} = proplists:get_value(eldap_server, Config),
    {ok, H} = eldap:open([Host], [{port,Port}]),
    %% {ok, H} = eldap:open([Host], [{port,Port+1}, {ssl, true}]),
    BasePath = proplists:get_value(eldap_path, Config),
    All = fun(Where) ->
		  eldap:search(H, #eldap_search{base=Where,
						filter=eldap:present("objectclass"),
						scope= eldap:wholeSubtree()})
	  end,
    Search = fun(Filter) ->
		     eldap:search(H, #eldap_search{base=BasePath,
						   filter=Filter,
						   scope=eldap:singleLevel()})
	     end,
    {ok, #eldap_search_result{entries=[_]}} = All(BasePath),
    {error, noSuchObject} = All("cn=Bar,"++BasePath),

    {error, _} = eldap:add(H, "cn=Jonas Jonsson," ++ BasePath,
			   [{"objectclass", ["person"]},
			    {"cn", ["Jonas Jonsson"]}, {"sn", ["Jonsson"]}]),
    eldap:simple_bind(H, "cn=Manager,dc=ericsson,dc=se", "hejsan"),

    %% Add
    ok = eldap:add(H, "cn=Jonas Jonsson," ++ BasePath,
		   [{"objectclass", ["person"]},
		    {"cn", ["Jonas Jonsson"]}, {"sn", ["Jonsson"]}]),
    ok = eldap:add(H, "cn=Foo Bar," ++ BasePath,
		   [{"objectclass", ["person"]},
		    {"cn", ["Foo Bar"]}, {"sn", ["Bar"]}, {"telephoneNumber", ["555-1232", "555-5432"]}]),
    ok = eldap:add(H, "ou=Team," ++ BasePath,
		   [{"objectclass", ["organizationalUnit"]},
		    {"ou", ["Team"]}]),

    %% Search
    JJSR = {ok, #eldap_search_result{entries=[#eldap_entry{}]}} = Search(eldap:equalityMatch("sn", "Jonsson")),
    JJSR = Search(eldap:substrings("sn", [{any, "ss"}])),
    FBSR = {ok, #eldap_search_result{entries=[#eldap_entry{object_name=FB}]}} =
	Search(eldap:substrings("sn", [{any, "a"}])),
    FBSR = Search(eldap:substrings("sn", [{initial, "B"}])),
    FBSR = Search(eldap:substrings("sn", [{final, "r"}])),

    F_AND = eldap:'and'([eldap:present("objectclass"), eldap:present("ou")]),
    {ok, #eldap_search_result{entries=[#eldap_entry{}]}} = Search(F_AND),
    F_NOT = eldap:'and'([eldap:present("objectclass"), eldap:'not'(eldap:present("ou"))]),
    {ok, #eldap_search_result{entries=[#eldap_entry{}, #eldap_entry{}]}} = Search(F_NOT),

    %% MODIFY
    Mod = [eldap:mod_replace("telephoneNumber", ["555-12345"]),
	   eldap:mod_add("description", ["Nice guy"])],
    %% io:format("MOD ~p ~p ~n",[FB, Mod]),
    ok = eldap:modify(H, FB, Mod),
    %% DELETE ATTR
    ok = eldap:modify(H, FB, [eldap:mod_delete("telephoneNumber", [])]),

    %% DELETE
    {error, entryAlreadyExists} = eldap:add(H, "cn=Jonas Jonsson," ++ BasePath,
					    [{"objectclass", ["person"]},
					     {"cn", ["Jonas Jonsson"]}, {"sn", ["Jonsson"]}]),
    ok = eldap:delete(H, "cn=Jonas Jonsson," ++ BasePath),
    {error, noSuchObject} = eldap:delete(H, "cn=Jonas Jonsson," ++ BasePath),

    %% MODIFY_DN
    ok = eldap:modify_dn(H, FB, "cn=Niclas Andre", true, ""),
    %%io:format("Res ~p~n ~p~n",[R, All(BasePath)]),

    eldap:close(H),
    ok.

add(H,  Attr, Value, Path0, Attrs, Class) ->
    Path = case Path0 of
	       [] -> Attr ++ "=" ++ Value;
	       _ -> Attr ++ "=" ++ Value ++ "," ++ Path0
	   end,
    case eldap:add(H, Path, [{"objectclass", Class}, {Attr, [Value]}] ++ Attrs)
    of
	ok -> {ok, Path};
	{error, E = entryAlreadyExists} -> {E, Path};
	R = {error, Reason} ->
	    io:format("~p:~p: ~s,~s =>~n ~p~n",
		      [?MODULE,?LINE, Attr, Value, R]),
	    exit({ldap, add, Reason})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Develop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    run().

run() ->
    Cases = all(),
    run(Cases).

run(Case) when is_atom(Case) ->
    run([Case]);
run(Cases) when is_list(Cases) ->
    Run = fun(Test, Config0) ->
		  Config = init_per_testcase(Test, Config0),
		  try
		      io:format("~nTest ~p ... ",[Test]),
		      ?MODULE:Test(Config),
		      end_per_testcase(Test, Config),
		      io:format("ok~n",[])
		  catch _:Reason ->
			  io:format("~n   FAIL (~p): ~p~n ~p~n",
				    [Test, Reason, erlang:get_stacktrace()])
		  end
	  end,
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() ->
			     case init_per_suite([]) of
				 {skip, Reason} -> io:format("Skip ~s~n",[Reason]);
				 Config ->
				     try
					 [Run(Test, Config) || Test <- Cases]
				     catch _:Err ->
					     io:format("Error ~p in ~p~n",[Err, erlang:get_stacktrace()])
				     end,
				     end_per_suite(Config)
			     end
		     end),
    receive
	{'EXIT', Pid, normal} -> ok;
	Msg -> io:format("Received ~p (~p)~n",[Msg, Pid])
    after 100 -> ok end,
    process_flag(trap_exit, false),
    ok.
