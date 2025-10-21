%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Bartłomiej Puzoń 1996-2025 <bartlomiej.puzon@erlang-solutions.com>.
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : ttb_autostart.erl
%%% Author  : Bartłomiej Puzoń <bartlomiej.puzon@erlang-solutions.com>
%%% Description : This supervisor is used to resume ttb tracing
%%% Users are able to provide custom restart modules for *_config, as
%%% file:write/read/delete may not be possible on diskless nodes.
%%%
%%% Created : 31 Jul 2010 by <bartlomiej.puzon@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ttb_autostart).
-moduledoc false.

-behaviour(gen_server).

%% API
-export([start_link/0,
         read_config/0,
         write_config/1,
         delete_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEF_AUTOSTART_MODULE, ?MODULE).
-define(AUTOSTART_FILENAME, "ttb_autostart.bin").

start_link() ->
    gen_server:start_link(?MODULE, no_args, []).

delete_config() ->
    file:delete(?AUTOSTART_FILENAME).

read_config() ->
    case file:read_file(?AUTOSTART_FILENAME) of
        {ok, Data} -> {ok, binary_to_term(Data)};
        Error      -> Error
    end.

write_config(Data) ->
    file:write_file(?AUTOSTART_FILENAME, term_to_binary(Data)).

init(no_args) ->
    case application:get_env(runtime_tools, ttb_autostart_module) of
        {ok, _} ->  ok;
        undefined -> application:set_env(runtime_tools, ttb_autostart_module, ?DEF_AUTOSTART_MODULE)
    end,
    observer_backend:ttb_resume_trace(),
    %%As the process is not needed any more, it will shut itself down
    {ok, no_args, 10000}.

handle_call(_,_,_)       -> {noreply, no_args}.
handle_cast(_,_)         -> {noreply, no_args}.
handle_info(timeout,_)   -> {stop, normal, no_args}.
terminate(_,_)           -> ok.
code_change(_,_,_)       -> {ok, no_args}.
