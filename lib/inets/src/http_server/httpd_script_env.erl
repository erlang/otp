%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(httpd_script_env).

-export([create_env/3]).

-include("httpd.hrl").
-include("httpd_internal.hrl").

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

which_server(#mod{config_db = ConfigDb}) ->
    httpd_util:lookup(ConfigDb, server, ?SERVER_SOFTWARE).

which_port(#mod{config_db = ConfigDb}) ->
    httpd_util:lookup(ConfigDb, port, 80).

which_peername(#mod{init_data = #init_data{peername = {_, RemoteAddr}}}) ->
    RemoteAddr.

which_peercert(#mod{socket_type = {Type, _}, socket = Socket}) when Type == essl;
								    Type == ssl ->
    case ssl:peercert(Socket) of
	{ok, Cert} ->
	    Cert;
	{error, no_peercert} -> 
	    no_peercert;
	_  ->
	    undefined
    end;
which_peercert(_) -> %% Not an ssl connection
    undefined.

which_resolve(#mod{init_data = #init_data{resolve = Resolve}}) ->
    Resolve.

which_method(#mod{method = Method}) ->
    Method.

which_request_uri(#mod{request_uri = RUri}) ->
    RUri.

create_basic_elements(esi, ModData) ->
    [{server_software,   which_server(ModData)},
     {server_name,       which_resolve(ModData)},
     {gateway_interface, ?GATEWAY_INTERFACE},
     {server_protocol,   ?SERVER_PROTOCOL},
     {server_port,       which_port(ModData)},
     {request_method,    which_method(ModData)},
     {remote_addr,       which_peername(ModData)},
     {peer_cert,         which_peercert(ModData)},
     {script_name,       which_request_uri(ModData)}];

create_basic_elements(cgi, ModData) ->
    [{"SERVER_SOFTWARE",   which_server(ModData)},
     {"SERVER_NAME",       which_resolve(ModData)},
     {"GATEWAY_INTERFACE", ?GATEWAY_INTERFACE},
     {"SERVER_PROTOCOL",   ?SERVER_PROTOCOL},
     {"SERVER_PORT",       integer_to_list(which_port(ModData))},
     {"REQUEST_METHOD",    which_method(ModData)},
     {"REMOTE_ADDR",       which_peername(ModData)},
     {"SCRIPT_NAME",       which_request_uri(ModData)}].

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
    NewName = re:replace(Name,"-","_", [{return,list}, global]),
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
