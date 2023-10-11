%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2023. All Rights Reserved.
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
-module(logger_handler).

%%%
%%% Types
-type config() :: #{id => id(),
                    config => term(),
                    level => logger:level() | all | none,
                    module => module(),
                    filter_default => log | stop,
                    filters => [{logger:filter_id(),logger:filter()}],
                    formatter => {module(),logger:formatter_config()}}.
-type id() :: atom().

-export_type([config/0, id/0]).

%%%-----------------------------------------------------------------
%%% Callbacks
-callback adding_handler(Config1) -> {ok, Config2} | {error, Reason} when
      Config1 :: config(),
      Config2 :: config(),
      Reason :: term().

-callback changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    {ok, Config} | {error, Reason} when
      SetOrUpdate :: set | update,
      OldConfig :: config(),
      NewConfig :: config(),
      Config :: config(),
      Reason :: term().

-callback filter_config(Config) -> FilteredConfig when
      Config :: config(),
      FilteredConfig :: config().

-callback log(LogEvent, Config) -> term() when
      LogEvent :: logger:log_event(), Config :: config().

-callback removing_handler(Config) -> ok when
      Config :: config().

-optional_callbacks([adding_handler/1, changing_config/3,
                     filter_config/1, removing_handler/1]).
