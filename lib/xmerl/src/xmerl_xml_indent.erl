%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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

%% Description  : Callback module for exporting complete or simple forms to indented XML.
%%
%% This module indents the xml with 2 spaces and a newline \n.
%% Currently the implementation does not allow it to be configured.
%% The implementation is based on the same Elixir implementation.
%% https://hexdocs.pm/xmerl_xml_indent/readme.html

-module(xmerl_xml_indent).
-moduledoc false.

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, empty_tag/2, export_text/1]).

-include("xmerl.hrl").
-include("xmerl_internal.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V,Data];
'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>\n", Data].


%% The '#element#' function is the default handler for XML elements.

'#element#'(Tag, [], Attrs, _Parents, _E) ->
    empty_tag(Tag, Attrs);
'#element#'(Tag, Data, Attrs, Parents, _E) ->
    IsCharData = is_char(Data),
    NewData =
        case IsCharData of
            true ->
                LengthParents = length(Parents),
                %% Push all the data over Lvl spaces.
                [
                    indent(LengthParents + 1) ++ DataEntry
                 || DataEntry <- Data
                ] ++ indent(LengthParents);
            false ->
                Data
        end,
    markup(Tag, Attrs, NewData).

is_char([[X|_]|_]) ->
    not is_integer(X);
is_char(Data) when is_list(Data) ->
    false.

indent(Level) ->
    [$\n | lists:duplicate(2 * Level, $\s)].
