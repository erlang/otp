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

%%% Description  : Callback module for exporting XHTML to HTML.

-module(xmerl_html).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XHTML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1,
	 p/4]).

-import(xmerl_lib, [start_tag/2, end_tag/1, is_empty_data/1,
		    find_attribute/2, export_text/1]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, Attrs, [], _E) ->
    Ver = case find_attribute(version, Attrs) of
	      {value, V} ->
		  V;
	      false ->
%%% 		  "-//W3C//DTD HTML 4.01//EN"		     % strict
		  "-//W3C//DTD HTML 4.01 Transitional//EN"   % loose
	  end,
    URI = case find_attribute(uri, Attrs) of
	      {value, U} ->
		  [" \"", U, "\""];
	      false ->
%%%		  " \"http://www.w3.org/TR/html4/strict.dtd\""
%%%		  " \"http://www.w3.org/TR/html4/loose.dtd\""
		  ""
	  end,
    ["<!DOCTYPE HTML PUBLIC \"", Ver, "\"", URI, ">\n", Data].


%% HTML does not have the <Tag/> empty-element form of XML/XHTML.
markup(Tag, Attrs, Data) ->
    [start_tag(Tag, Attrs), Data, end_tag(Tag)].

%% Some HTML elements must not have an end tag at all.
markup_noend(Tag, Attrs, Data) ->
    [start_tag(Tag, Attrs), Data].


%% For some element types, the end tag is forbidden. (In all other
%% cases, we always generate the end tag, to make sure that the scope of
%% a markup is not extended by mistake.)

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    case forbid_end(Tag) of
	false ->
	    markup(Tag, Attrs, Data);
	true ->
	    markup_noend(Tag, Attrs, Data)
    end.


%% HTML tags with special handling

p(Data, Attrs, _Parents, _E) ->
    %% In general, we cannot drop the end tag for paragraph elements;
    %% that is only allowed if we know that it is immediately followed
    %% by some other block-level tag.
    case is_empty_data(Data) of
	true ->
	    %% Paragraph elements should never be completely empty.
	    markup(p, Attrs, "\s");
	false ->
	    markup(p, Attrs, Data)
    end.


%% Utility functions

forbid_end(area) -> true; 
forbid_end(base) -> true; 
forbid_end(basefont) -> true; 
forbid_end(br) -> true; 
forbid_end(col) -> true; 
forbid_end(frame) -> true; 
forbid_end(hr) -> true; 
forbid_end(img) -> true; 
forbid_end(input) -> true; 
forbid_end(isindex) -> true; 
forbid_end(link) -> true; 
forbid_end(meta) -> true; 
forbid_end(param) -> true; 
forbid_end(_) -> false.
