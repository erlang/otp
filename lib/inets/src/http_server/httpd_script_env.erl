%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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
-moduledoc false.

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

which_peercert(#mod{socket_type = {ssl, _}, socket = Socket}) ->
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

which_name(#mod{config_db = ConfigDB}) ->
    httpd_util:lookup(ConfigDB, server_name).

which_method(#mod{method = Method}) ->
    Method.

which_request_uri(#mod{request_uri = RUri}) ->
    RUri.

which_connect_addr(#mod{socket = Socket, socket_type = SocketType}) when
      SocketType == ip_comm;
      element(1, SocketType) == ip_comm ->
    maybe
        {ok, {Addr, _Port}} ?= inet:sockname(Socket),
        inet:ntoa(Addr)
    end;
which_connect_addr(#mod{socket = Socket, socket_type = SocketType}) when
      SocketType == ssl;
      element(1, SocketType) == ssl ->
    maybe
        {ok, {Addr, _Port}} ?= ssl:sockname(Socket),
        inet:ntoa(Addr)
    end.

create_basic_elements(esi, ModData) ->
    [{server_software,   which_server(ModData)},
     {server_name,       which_name(ModData)},
     {host_name,         which_resolve(ModData)},
     {gateway_interface, ?GATEWAY_INTERFACE},
     {server_protocol,   ?SERVER_PROTOCOL},
     {server_port,       which_port(ModData)},
     {request_method,    which_method(ModData)},
     {remote_addr,       which_peername(ModData)},
     {connect_addr,      which_connect_addr(ModData)},
     {peer_cert,         which_peercert(ModData)},
     {script_name,       which_request_uri(ModData)}];

create_basic_elements(cgi, ModData) ->
    [{"SERVER_SOFTWARE",   which_server(ModData)},
     {"SERVER_NAME",       which_name(ModData)},
     {"HOST_NAME",         which_resolve(ModData)},
     {"GATEWAY_INTERFACE", ?GATEWAY_INTERFACE},
     {"SERVER_PROTOCOL",   ?SERVER_PROTOCOL},
     {"SERVER_PORT",       integer_to_list(which_port(ModData))},
     {"REQUEST_METHOD",    which_method(ModData)},
     {"REMOTE_ADDR",       which_peername(ModData)},
     {"CONNECT_ADDR",      which_connect_addr(ModData)},
     {"SCRIPT_NAME",       which_request_uri(ModData)}].

create_http_header_elements(ScriptType, Headers) ->
    create_http_header_elements(ScriptType, Headers, [], []).

create_http_header_elements(esi, [], Acc, OtherAcc) ->
    [{http_other, OtherAcc} | Acc];
create_http_header_elements(_, [], Acc, _OtherAcc) ->
    Acc;
create_http_header_elements(ScriptType, [{Name, [Value | _] = Values } | 
					 Headers], Acc, OtherAcc) 
  when is_list(Value) ->
    try http_env_element(ScriptType, Name, multi_value(Values)) of
        Element ->
            create_http_header_elements(ScriptType, Headers, [Element | Acc],
                                        OtherAcc)
    catch
        _:_ ->
            create_http_header_elements(ScriptType, Headers, Acc,
                                        [{Name, Values} | OtherAcc])
    end;
create_http_header_elements(ScriptType, [{Name, Value} | Headers], Acc, OtherAcc) 
  when is_list(Value) ->
    try http_env_element(ScriptType, Name, Value) of
        Element ->
            create_http_header_elements(ScriptType, Headers, [Element | Acc],
                                       OtherAcc)
    catch
        _:_ ->
            create_http_header_elements(ScriptType, Headers, Acc,
                                       [{Name, Value} | OtherAcc])
    end.

http_env_element(cgi, VarName0, Value)  ->
    VarName = re:replace(VarName0,"-","_", [{return,list}, global]),
    {"HTTP_"++ http_util:to_upper(VarName), Value};
http_env_element(esi, VarName0, Value)  ->
    list_to_existing_atom(VarName0),
    VarName = re:replace(VarName0,"-","_", [{return,list}, global]),
    HeaderName = http_util:to_lower(VarName),
    {list_to_atom("http_"++ HeaderName), Value}.

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
    [{content_length, integer_to_list(erlang:iolist_size(Body))}];
create_script_elements(cgi, entity_body, Body, _) ->
    [{"CONTENT_LENGTH", integer_to_list(erlang:iolist_size(Body))}];
create_script_elements(_, _, _, _) ->
    [].

create_mod_interaction_elements(_, ModData)->
    case proplists:get_value(remote_user, ModData#mod.data) of
	undefined ->
	    [];
	RemoteUser ->
	    [{remote_user, RemoteUser}]
    end.
