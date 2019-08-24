%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : ct_config_xml.erl
%% Description : CT callback module for reading configs from XML files
%%
%% Created : 16 February 2010
%%----------------------------------------------------------------------
-module(ct_config_xml).
-export([read_config/1, check_parameter/1]).

% read config file
read_config(ConfigFile) ->
    case catch do_read_xml_config(ConfigFile) of
	{ok,Config} ->
	    {ok,Config};
	Error = {error,_} ->
	    Error
    end.

% check file exists
check_parameter(File) ->
    case filelib:is_file(File) of
	true ->
	    {ok,{file,File}};
	false ->
	    {error,{nofile,File}}
    end.

% actual reading of the config
do_read_xml_config(ConfigFile) ->
    case catch xmerl_sax_parser:file(ConfigFile,
	    [{event_fun,fun event/3},
	     {event_state,[]}]) of
	{ok,EntityList,_} ->
	    {ok,lists:reverse(transform_entity_list(EntityList))};
	Oops ->
	    {error,{parsing_failed,Oops}}
    end.

% event callback for xmerl_sax_parser
event(Event, _LineNo, State) ->
    tag(Event, State).

% document start
tag(startDocument, State) ->
    State;

% start of the config
tag({startElement, _Uri, "config", _QName, _Attributes}, []) ->
	[{"config", []}];

% start tag
tag({startElement, _Uri, Name, _QName, _Attributes}, Tags) ->
	[{Name, []}|Tags];

% value
tag({characters, String}, [{Tag, _Value}|Tags]) ->
	[{Tag, String}|Tags];

% end tag
tag({endElement, _Uri, _Name, _QName},
        [Entity, {PrevEntityTag, PrevEntityValue}|Tags]) ->
	NewHead = {PrevEntityTag, [Entity|PrevEntityValue]},
	[NewHead|Tags];

% end of the config
tag({endElement, _Uri, "config", _QName}, [{"config", Config}]) ->
	Config;

% end of document, return result
tag(endDocument,  {_Tags, Result}) ->
    Result;

% default
tag(_El, State) ->
	State.

% transform of the ugly deeply nested entity list to the key-value "tree"
transform_entity_list(EntityList) ->
    lists:map(fun transform_entity/1, EntityList).

% transform entity from {list(), list()} to {atom(), term()}
transform_entity({Tag, [Value|Rest]}) when
    is_tuple(Value) ->
	{list_to_atom(Tag), transform_entity_list(lists:reverse([Value|Rest]))};
transform_entity({Tag, String}) ->
    case list_to_term(String) of
	{ok, Value} ->
	    {list_to_atom(Tag), Value};
	Error ->
	     throw(Error)
    end.

% transform a string with Erlang terms
list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case catch erl_parse:parse_term(T) of
        {ok,Term} ->
            {ok,Term};
        Error ->
            {error,{Error,String}}
    end.
