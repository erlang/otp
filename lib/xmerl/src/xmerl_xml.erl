%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%% Description  : Callback module for exporting 
%%		   complete or simple forms to XML.

-module(xmerl_xml).

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
%?dbg("Text=~p~n",[Text]),
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V,Data];
'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>", Data].


%% The '#element#' function is the default handler for XML elements.

'#element#'(Tag, [], Attrs, _Parents, _E) ->
%?dbg("Empty Tag=~p~n",[Tag]),
    empty_tag(Tag, Attrs);
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
%?dbg("Tag=~p~n",[Tag]),
    markup(Tag, Attrs, Data).
