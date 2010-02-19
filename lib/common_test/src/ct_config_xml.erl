%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : ct_config_xml.erl
%% Description : CT callback module for reading configs from XML files
%%
%% Created : 16 February 2010
%%----------------------------------------------------------------------
-module(ct_config_xml).
-export([read_config_file/1, list_to_term/1]).

read_config_file(ConfigFile) ->
    case catch do_read_xml_config(ConfigFile) of
	{ok, Config}->
	    {ok, Config};
	{error, Error, ErroneousString}->
	    {error, Error, ErroneousString}
    end.

do_read_xml_config(ConfigFile)->
    {ok, EntityList, _}=
	xmerl_sax_parser:file(ConfigFile,
	    [{event_fun, fun event/3},
	    {event_state, initial_state()}]),
    {ok, transform_entity_list(EntityList)}.


initial_state() ->
    [].

event(Event, _LineNo, State) ->
    tag(Event, State).

tag(startDocument, State) ->
    State;

tag(endDocument,  {_Tags, Result}) ->
    Result;

tag({startElement, _Uri, "config", _QName, _Attributes}, []) ->
	[{"config", []}];

tag({endElement, _Uri, "config", _QName}, [{"config", Config}]) ->
	Config;

tag({startElement, _Uri, Name, _QName, _Attributes}, Tags) ->
	[{Name, []}|Tags];

tag({endElement, _Uri, _Name, _QName},
        [Entity, {PrevEntityTag, PrevEntityValue}|Tags]) ->
	NewHead = {PrevEntityTag, [Entity|PrevEntityValue]},
	[NewHead|Tags];

tag({characters, String}, [{Tag, _Value}|Tags]) ->
	[{Tag, String}|Tags];

tag(_El, State) ->
	State.

transform_entity_list(EntityList)->
    lists:map(fun transform_entity/1, EntityList).

transform_entity({Tag, [Value|Rest]}) when
    is_tuple(Value)->
	{list_to_atom(Tag), transform_entity_list([Value|Rest])};
transform_entity({Tag, String})->
    case list_to_term(String) of
	{ok, Value}->
	    {list_to_atom(Tag), Value};
	Error->
	     throw(Error)
    end.

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            {ok, Term};
        Error ->
            {error, Error, String}
    end.
