%%%-------------------------------------------------------------------
%%% File    : mylib.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 20 Sep 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(mylib).
-export([fetch/2,path_locate/3]).
-include("xmerl_xsd.hrl").
-include("xmerl.hrl").
-include_lib("kernel/include/file.hrl").

fetch(URI,S) ->
    Split = filename:split(URI),
    Filename = fun([])->[];(X)->lists:last(X) end (Split),
    Fullname = 
	case Split of %% how about Windows systems?
	    ["file:"|Name]-> %% absolute path, see RFC2396 sect 3
		%% file:/dtd_name 
		filename:join(["/"|Name]);
	    ["/"|Rest] when Rest /= [] ->
		%% absolute path name
		URI;
	    ["http:"|_Rest] ->
		{http,URI};
	    [] -> %% empty systemliteral
		[];
	    _ ->
		filename:join(S#xsd_state.xsd_base, URI)
	end,
    Path = ?MODULE:path_locate(S#xsd_state.fetch_path, Filename, Fullname),
    {ok, Path, S}.

path_locate(_, _, {http,URI}) ->
    application:start(inets),
    http:set_options([{proxy, {{"www-proxy.ericsson.se", 8080},["localhost"]}}]),
    {ok,{{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	http:request(get, {URI, []}, [], []),
    {string,Body};
path_locate(_, _, []) ->
    [];
path_locate([Dir|Dirs], FN, FullName) ->
    F = filename:join(Dir, FN),
    case file:read_file_info(F) of
	{ok, #file_info{type = regular}} ->
	    {file,F};
	_ ->
	    path_locate(Dirs, FN, FullName)
    end;
path_locate([], _FN, FullName) ->
    {file,FullName}.


