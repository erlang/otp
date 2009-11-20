%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

 %% Description  : Callback module for exporting XML to SGML.

-module(xmerl_sgml).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, find_attribute/2, export_text/1]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, Attrs, [], _E) -> 
    case find_attribute(header, Attrs) of
	{value, Hdr} ->
	    [Hdr, Data];
	false ->
	    Data
    end.


%% Note that SGML does not have the <Tag/> empty-element form.
%% Furthermore, for some element types, the end tag may be forbidden -
%% this can be handled by extending this module - see xmerl_otpsgml.erl
%% for an example. (By default, we always generate the end tag, to make
%% sure that the scope of a markup is not extended by mistake.)

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    markup(Tag, Attrs, Data).
