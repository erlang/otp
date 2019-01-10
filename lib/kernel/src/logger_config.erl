%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
         allow/2,allow/3,
         get/2, get/3,
         create/3, set/3,
         set_module_level/3,unset_module_level/2,
         get_module_level/1,cache_module_level/2,
         level_to_int/1]).

-include("logger_internal.hrl").

new(Name) ->
    _ = ets:new(Name,[set,protected,named_table,
                      {read_concurrency,true},
                      {write_concurrency,true}]),
    ets:whereis(Name).

delete(Tid,Id) ->
    ets:delete(Tid,table_key(Id)).

allow(Tid,Level,Module) ->
    LevelInt = level_to_int(Level),
    case ets:lookup(Tid,Module) of
        [{Module,{ModLevel,cached}}] when is_integer(ModLevel),
                                          LevelInt =< ModLevel ->
            true;
        [{Module,ModLevel}] when is_integer(ModLevel),
                                 LevelInt =< ModLevel ->
            true;
        [] ->
            logger_server:cache_module_level(Module),
            allow(Tid,Level);
        _ ->
            false
    end.

allow(Tid,Level) ->
    GlobalLevelInt = ets:lookup_element(Tid,?PRIMARY_KEY,2),
    level_to_int(Level) =< GlobalLevelInt.

exist(Tid,What) ->
    ets:member(Tid,table_key(What)).

get(Tid,What) ->
    case ets:lookup(Tid,table_key(What)) of
        [{_,_,Config}] ->
            {ok,Config};
        [{_,Config}] when What=:=proxy ->
            {ok,Config};
        [] ->
            {error,{not_found,What}}
    end.

get(Tid,What,Level) ->
    MS = [{{table_key(What),'$1','$2'},
           [{'>=','$1',level_to_int(Level)}],
           ['$2']}],
    case ets:select(Tid,MS) of
        [] -> error;
        [Data] -> {ok,Data}
    end.

create(Tid,proxy,Config) ->
    ets:insert(Tid,{table_key(proxy),Config});
create(Tid,What,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    ets:insert(Tid,{table_key(What),LevelInt,Config}).

set(Tid,proxy,Config) ->
    ets:insert(Tid,{table_key(proxy),Config}),
    ok;
set(Tid,What,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    %% Should do this only if the level has actually changed. Possibly
    %% overwrite instead of delete?
    case What of
        primary ->
            _ = ets:select_delete(Tid,[{{'_',{'$1',cached}},
                                        [{'=/=','$1',LevelInt}],
                                        [true]}]),
            ok;
        _ ->
            ok
    end,
    ets:update_element(Tid,table_key(What),[{2,LevelInt},{3,Config}]),
    ok.

set_module_level(Tid,Modules,Level) ->
    LevelInt = level_to_int(Level),
    [ets:insert(Tid,{Module,LevelInt}) || Module <- Modules],
    ok.

%% should possibly overwrite instead of delete?
unset_module_level(Tid,all) ->
    MS = [{{'$1','$2'},[{is_atom,'$1'},{is_integer,'$2'}],[true]}],
    _ = ets:select_delete(Tid,MS),    
    ok;
unset_module_level(Tid,Modules) ->
    [ets:delete(Tid,Module) || Module <- Modules],
    ok.

get_module_level(Tid) ->
    MS = [{{'$1','$2'},[{is_atom,'$1'},{is_integer,'$2'}],[{{'$1','$2'}}]}],
    Modules = ets:select(Tid,MS),
    lists:sort([{M,int_to_level(L)} || {M,L} <- Modules]).

cache_module_level(Tid,Module) ->
    GlobalLevelInt = ets:lookup_element(Tid,?PRIMARY_KEY,2),
    ets:insert_new(Tid,{Module,{GlobalLevelInt,cached}}),
    ok.

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
