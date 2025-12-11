%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: MPL-1.1
%%
%% SPDX-FileCopyrightText: Copyright 2007 S.G. Consulting s.r.l. Hunter Morris <huntermorris@gmail.com>
%% Copyright Ericsson AB 2025. All Rights Reserved.
%%
%% %CopyrightEnd%

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
