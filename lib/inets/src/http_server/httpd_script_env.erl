%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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

-module(httpd_script_env).

-export([create_env/3]).

-include("httpd.hrl").

%%%=========================================================================
%%%  Internal application API 
%%%=========================================================================
%%--------------------------------------------------------------------------
%% create_env(ScriptType, ModData, ScriptElements) -> [{EnvVariable, Value}]
%%                           
%%	ScriptType = cgi | esi   
%%	ModData = #mod{}
%%      ScriptElements = [{Element, Value}]
%%      Element = path_info | query_string | entity_body
%%      Value = term()
%%      EnvVariable = string() - cgi | atom() - esi
%%
%% Description: Creates a list of cgi/esi environment variables and
%% there values.
%%--------------------------------------------------------------------------
create_env(ScriptType, ModData, ScriptElements) ->
    create_basic_elements(ScriptType, ModData) 
	++ create_http_header_elements(ScriptType, ModData#mod.parsed_header)
	++ create_script_elements(ScriptType, ModData, ScriptElements)
	++ create_mod_interaction_elements(ScriptType, ModData).

%%%========================================================================
%%% Internal functions
%%%========================================================================
create_basic_elements(esi, ModData) ->
    {_, RemoteAddr} = (ModData#mod.init_data)#init_data.peername,
    [{server_software, ?SERVER_SOFTWARE},
     {server_name, (ModData#mod.init_data)#init_data.resolve},
     {gateway_interface,?GATEWAY_INTERFACE},
     {server_protocol, ?SERVER_PROTOCOL},
     {server_port, httpd_util:lookup(ModData#mod.config_db,port,80)},
     {request_method, ModData#mod.method},
     {remote_addr, RemoteAddr},
     {script_name, ModData#mod.request_uri}];

create_basic_elements(cgi, ModData) ->
    {_, RemoteAddr} = (ModData#mod.init_data)#init_data.peername,
    [{"SERVER_SOFTWARE",?SERVER_SOFTWARE},
     {"SERVER_NAME", (ModData#mod.init_data)#init_data.resolve},
     {"GATEWAY_INTERFACE",?GATEWAY_INTERFACE},
     {"SERVER_PROTOCOL",?SERVER_PROTOCOL},
     {"SERVER_PORT",
      integer_to_list(httpd_util:lookup(
			ModData#mod.config_db, port, 80))},
     {"REQUEST_METHOD", ModData#mod.method},
     {"REMOTE_ADDR", RemoteAddr},
     {"SCRIPT_NAME", ModData#mod.request_uri}].

create_http_header_elements(ScriptType, Headers) ->
    create_http_header_elements(ScriptType, Headers, []).

create_http_header_elements(_, [], Acc) ->
    Acc;
create_http_header_elements(ScriptType, [{Name, [Value | _] = Values } | 
					     Headers], Acc) 
  when is_list(Value) ->
    NewName = lists:map(fun(X) -> if X == $- -> $_; true -> X end end, Name),
    Element = http_env_element(ScriptType, NewName, multi_value(Values)),
    create_http_header_elements(ScriptType, Headers, [Element | Acc]);

create_http_header_elements(ScriptType, [{Name, Value} | Headers], Acc) 
  when is_list(Value) ->
    {ok, NewName, _} = inets_regexp:gsub(Name,"-","_"),
    Element = http_env_element(ScriptType, NewName, Value),
    create_http_header_elements(ScriptType, Headers, [Element | Acc]).

http_env_element(cgi, VarName, Value)  ->
    {"HTTP_"++ http_util:to_upper(VarName), Value};
http_env_element(esi, VarName, Value)  ->
    {list_to_atom("http_"++ http_util:to_lower(VarName)), Value}.

multi_value([]) ->
  [];
multi_value([Value]) ->
  Value;
multi_value([Value | Rest]) ->
  Value ++ ", " ++ multi_value(Rest).

create_script_elements(ScriptType, ModData, ScriptElements) ->
    lists:flatmap(fun({Element, Data}) ->
			  create_script_elements(ScriptType,
						 Element,
						 Data, ModData)
		  end, ScriptElements).

create_script_elements(esi, query_string, QueryString, _) ->
    [{query_string, QueryString}];
create_script_elements(cgi, query_string, QueryString, _) ->
    [{"QUERY_STRING", QueryString}];
create_script_elements(esi, path_info, PathInfo, ModData) ->
    Aliases = httpd_util:multi_lookup(ModData#mod.config_db, alias),
    {_,PathTranslated,_} = 
	mod_alias:real_name(ModData#mod.config_db, PathInfo,
			    Aliases),
    [{path_info, PathInfo},
     {path_translated, PathTranslated}];
create_script_elements(cgi, path_info, PathInfo, ModData) ->
    Aliases = httpd_util:multi_lookup(ModData#mod.config_db, alias),
    {_,PathTranslated,_} = 
	mod_alias:real_name(ModData#mod.config_db, PathInfo,
			    Aliases),
    [{"PATH_INFO", PathInfo},
     {"PATH_TRANSLATED", PathTranslated}];
create_script_elements(esi, entity_body, Body, _) ->
    [{content_length, httpd_util:flatlength(Body)}]; 
create_script_elements(cgi, entity_body, Body, _) ->
    [{"CONTENT_LENGTH", httpd_util:flatlength(Body)}]; 
create_script_elements(_, _, _, _) ->
    [].

create_mod_interaction_elements(_, ModData)->
    case proplists:get_value(remote_user, ModData#mod.data) of
	undefined ->
	    [];
	RemoteUser ->
	    [{remote_user, RemoteUser}]
    end.
