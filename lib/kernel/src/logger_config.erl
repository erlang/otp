%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2020. All Rights Reserved.
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
-module(logger_config).

-export([new/1,delete/2,
         exist/2,
         allow/1,allow/2,
         get/2, get/3,
         create/3, set/3,
         set_module_level/2,unset_module_level/1,
         get_module_level/0,
         level_to_int/1]).

-include("logger_internal.hrl").

-compile({inline,[get_primary_level/0,level_to_int/1]}).

-define(LEVEL_TO_CACHE(Level),Level + 16#10).
-define(PRIMARY_TO_CACHE(Level),Level).
-define(IS_CACHED(Level),(Level =< ?LOG_ALL)).
-define(CACHE_TO_LEVEL(Level),if ?IS_CACHED(Level) -> Level; true -> Level - 16#10 end).

-define(IS_MODULE(Module),is_atom(Module) andalso Module =/= ?PRIMARY_KEY).

new(Name) ->
    _ = ets:new(Name,[set,protected,named_table,
                      {read_concurrency,true},
                      {write_concurrency,true}]),
    ets:whereis(Name).

delete(Tid,What) ->
    persistent_term:put({?MODULE,table_key(What)},undefined),
    ets:delete(Tid,table_key(What)).

%% Optimized for speed.
allow(Level,Module) ->
    ModLevel =
        case persistent_term:get({?MODULE,Module},undefined) of
            undefined ->
                %% This is where the module cache takes place. We insert the module level
                %% plus 16 into the pt and then when looking it up we need to do a check
                %% and subtraction.
                %% The reason why we do this dance and not just wrap it all in a tuple
                %% is because updates of immediates (i.e. small ints in this case)
                %% is cheap even in pt, so we cannot put any complex terms in there.
                IntLevel = get_primary_level(),
                persistent_term:put({?MODULE,Module},?PRIMARY_TO_CACHE(IntLevel)),
                IntLevel;
            IntLevel ->
                ?CACHE_TO_LEVEL(IntLevel)
        end,
    less_or_equal_level(Level,ModLevel).

allow(Level) ->
    PrimaryLevelInt = get_primary_level(),
    less_or_equal_level(Level,PrimaryLevelInt).

less_or_equal_level(emergency,ModLevel) -> ?EMERGENCY =< ModLevel;
less_or_equal_level(alert,ModLevel) -> ?ALERT =< ModLevel;
less_or_equal_level(critical,ModLevel) -> ?CRITICAL =< ModLevel;
less_or_equal_level(error,ModLevel) -> ?ERROR =< ModLevel;
less_or_equal_level(warning,ModLevel) -> ?WARNING =< ModLevel;
less_or_equal_level(notice,ModLevel) -> ?NOTICE =< ModLevel;
less_or_equal_level(info,ModLevel) -> ?INFO =< ModLevel;
less_or_equal_level(debug,ModLevel) -> ?DEBUG =< ModLevel.

exist(Tid,What) ->
    ets:member(Tid,table_key(What)).

get_primary_level() ->
    persistent_term:get({?MODULE,?PRIMARY_KEY},?NOTICE).

get(Tid,What) ->
    case ets:lookup(Tid,table_key(What)) of
        [{_,Config}] ->
            {ok,Config};
        [] ->
            {error,{not_found,What}}
    end.

get(Tid,What,Level) ->
    TableKey = table_key(What),
    case persistent_term:get({?MODULE,TableKey},undefined) of
        undefined ->
            %% The handler is not installed at the moment
            {error,{not_found,What}};
        ConfLevel ->
            case less_or_equal_level(Level,ConfLevel) of
                true ->
                    get(Tid, What);
                false ->
                    error
            end
    end.

create(Tid,proxy,Config) ->
    ets:insert(Tid,{table_key(proxy),Config});
create(Tid,What,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    ok = persistent_term:put({?MODULE,table_key(What)}, LevelInt),
    ets:insert(Tid,{table_key(What),Config}).

set(Tid,proxy,Config) ->
    ets:insert(Tid,{table_key(proxy),Config}),
    ok;
set(Tid,What,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    %% Put either primary or handler level in pt cache
    ok = persistent_term:put({?MODULE,table_key(What)}, LevelInt),
    case What of
        primary ->
            %% If we change primary level, then we need to flush
            %% the module level cache.
            [persistent_term:put(Key,?PRIMARY_TO_CACHE(LevelInt))
             || {{?MODULE,Module} = Key,Level} <- persistent_term:get(),
                ?IS_MODULE(Module), ?IS_CACHED(Level)],
            ok;
        _ ->
            ok
    end,
    ets:insert(Tid,{table_key(What),Config}),
    ok.

set_module_level(Modules,Level) ->
    LevelInt = level_to_int(Level),
    [persistent_term:put({?MODULE,Module},?LEVEL_TO_CACHE(LevelInt)) || Module <- Modules],
    ok.

%% We overwrite instead of delete because that is more efficient
%% when using persistent_term
unset_module_level(all) ->
    PrimaryLevel = get_primary_level(),
    [persistent_term:put(Key, ?PRIMARY_TO_CACHE(PrimaryLevel))
     || {{?MODULE, Module} = Key,_} <- persistent_term:get(), ?IS_MODULE(Module)],
    ok;
unset_module_level(Modules) ->
    PrimaryLevel = get_primary_level(),
    [persistent_term:put({?MODULE,Module}, ?PRIMARY_TO_CACHE(PrimaryLevel)) || Module <- Modules],
    ok.

get_module_level() ->
    lists:sort(
      [{Module,int_to_level(?CACHE_TO_LEVEL(Level))}
       || {{?MODULE, Module},Level} <- persistent_term:get(),
          ?IS_MODULE(Module), not ?IS_CACHED(Level)]).

level_to_int(none) -> ?LOG_NONE;
level_to_int(emergency) -> ?EMERGENCY;
level_to_int(alert) -> ?ALERT;
level_to_int(critical) -> ?CRITICAL;
level_to_int(error) -> ?ERROR;
level_to_int(warning) -> ?WARNING;
level_to_int(notice) -> ?NOTICE;
level_to_int(info) -> ?INFO;
level_to_int(debug) -> ?DEBUG;
level_to_int(all) -> ?LOG_ALL.

int_to_level(?LOG_NONE) -> none;
int_to_level(?EMERGENCY) -> emergency;
int_to_level(?ALERT) -> alert;
int_to_level(?CRITICAL) -> critical;
int_to_level(?ERROR) -> error;
int_to_level(?WARNING) -> warning;
int_to_level(?NOTICE) -> notice;
int_to_level(?INFO) -> info;
int_to_level(?DEBUG) -> debug;
int_to_level(?LOG_ALL) -> all.

%%%-----------------------------------------------------------------
%%% Internal

table_key(proxy) -> ?PROXY_KEY;
table_key(primary) -> ?PRIMARY_KEY;
table_key(HandlerId) -> {?HANDLER_KEY,HandlerId}.
