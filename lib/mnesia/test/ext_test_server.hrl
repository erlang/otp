%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-ifdef(DEBUG).
-define(DBG(), io:format("~p:~p:~p: ~p~n",[node(), ?MODULE, ?LINE, ?FUNCTION_NAME])).
-define(DBG(DATA), io:format("~p:~p:~p: ~p, ~p~n",[node(), ?MODULE, ?LINE, ?FUNCTION_NAME, DATA])).
-define(DBG(FORMAT, ARGS), io:format("~p:~p:~p: ~p," ++ FORMAT,[node(), ?MODULE, ?LINE, ?FUNCTION_NAME] ++ ARGS)).
-else.
-define(DBG(), ok).
-define(DBG(DATA), ok).
-define(DBG(FORMAT, ARGS), ok).
-endif.

-record(exception, {c, r, st}).