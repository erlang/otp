%  Copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%%%-------------------------------------------------------------------
%%% File    : pubsub_publish.erl
%%% Author  : Thorsten Schuett <schuett@zib.de>
%%% Description : Publish function
%%%
%%% Created : 26 Mar 2008 by Thorsten Schuett <schuett@zib.de>
%%%-------------------------------------------------------------------
%% @author Thorsten Schuett <schuett@zib.de>
%% @copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%% @version $Id $
-module(pubsub_dir.pubsub_publish).

-author('schuett@zib.de').
-vsn('$Id: pubsub_publish.erl,v 1.1 2009/11/06 12:39:55 maria Exp $ ').

-export([publish/3, publish_internal/3]).

-import(json).
-import(io).
-import(http).
-import(jsonrpc).

%%====================================================================
%% public functions
%%====================================================================

%% @doc publishs an event to a given url.
%% @spec publish(string(), string(), string()) -> ok
%% @todo use pool:pspawn
publish(URL, Topic, Content) ->
    spawn(fun () -> pubsub_publish:publish_internal(URL, Topic, Content) end),
    ok.

publish_internal(URL, Topic, Content) ->
    Res = jsonrpc:call(URL, [], {call, notify, [Topic, Content]}),
    io:format("~p ~p~n", [Res, URL]).
