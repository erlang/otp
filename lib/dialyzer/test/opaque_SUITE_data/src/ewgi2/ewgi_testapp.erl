%%%-------------------------------------------------------------------
%%% File    : ewgi_testapp.erl
%%% Authors : Hunter Morris <huntermorris@gmail.com>
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% @doc
%%% <p>ewgi test applications</p>
%%%
%%% @end
%%%
%%% Created : 05 July 2009 by Hunter Morris <huntermorris@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_testapp).

-export([htmlise/1]).

-include_lib("ewgi.hrl").

htmlise(C) ->
    iolist_to_binary(
      ["<dl class=\"request\">",
       io_lib:format("<dt>other http headers</dt><dd>~s</dd>", [htmlise_data("http_headers", ewgi_api:get_all_headers(C))]),
       io_lib:format("<dt>ewgi extra data</dt><dd>~s</dd>", [htmlise_data("request_data", ewgi_api:get_all_data(C))]),
       "</dl>"]).

htmlise_data(Name, L) when is_list(L) ->
    ["<dl class=\"", Name, "\">",
     [io_lib:format("<dt>~s</dt><dd><pre>~p</pre><dd>", [K, V]) || {K, V} <- L],
     "</dl>"];
htmlise_data(Name, T) ->
    case gb_trees:to_list(T) of
        [] -> [];
        L -> htmlise_data(Name, L)
    end.
