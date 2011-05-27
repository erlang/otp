%%%-------------------------------------------------------------------
%%% File    : ewgi_api.erl
%%% Authors : Filippo Pacini <filippo.pacini@gmail.com>
%%%           Hunter Morris <huntermorris@gmail.com>
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
%%% <p>ewgi API. Defines a low level CGI like API.</p>
%%%
%%% @end
%%%
%%% Created : 10 Oct 2007 by Filippo Pacini <filippo.pacini@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_api).

-include_lib("ewgi.hrl").

-export([get_all_headers/1, get_all_data/1]).

-spec request(ewgi_context()) -> ewgi_request().
request(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_REQUEST(Ctx).

-spec headers(ewgi_context()) -> ewgi_http_headers().
headers(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_HEADERS(request(Ctx)).

get_header_value(Hdr0, Ctx) when is_list(Hdr0), ?IS_EWGI_CONTEXT(Ctx) ->
    Hdr = string:to_lower(Hdr0),
    get_header1(Hdr, Ctx).

get_header1("accept", Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_ACCEPT(headers(Ctx)).

unzip_header_value([{_,_}|_]=V) ->
	{_, V1} = lists:unzip(V),
	string:join(V1, ", ");
unzip_header_value(V) ->
	V.

get_all_headers(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    H = headers(Ctx),
    Other = gb_trees:to_list(?GET_HTTP_OTHER(H)),
    Acc = [{K, unzip_header_value(V)} || {K, V} <- Other],
    L = [{"accept", get_header_value("accept", Ctx)}|Acc],
    lists:filter(fun({_, undefined}) -> false; (_) -> true end, L).

-spec ewgi_spec(ewgi_context()) -> ewgi_spec().
ewgi_spec(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI(request(Ctx)).

get_all_data(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_DATA(ewgi_spec(Ctx)).
