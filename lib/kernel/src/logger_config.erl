%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
         get/2, get/3, get/1,
         create/3, create/4, set/3,
         set_module_level/3,reset_module_level/2,
         cache_module_level/2,
         level_to_int/1]).

-include("logger_internal.hrl").

new(Name) ->
    _ = ets:new(Name,[set,protected,named_table]),
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
    GlobalLevelInt = ets:lookup_element(Tid,?LOGGER_KEY,2),
    level_to_int(Level) =< GlobalLevelInt.

exist(Tid,What) ->
    ets:member(Tid,table_key(What)).

get(Tid,What) ->
    case ets:lookup(Tid,table_key(What)) of
        [{_,_,Config}] ->
            {ok,Config};
        [{_,_,Config,Module}] ->
            {ok,{Module,Config}};
        [] ->
            {error,{not_found,What}}
    end.

get(Tid,What,Level) ->
    MS = [{{table_key(What),'$1','$2'}, % logger config
           [{'>=','$1',level_to_int(Level)}],
           ['$2']},
          {{table_key(What),'$1','$2','$3'}, % handler config
           [{'>=','$1',level_to_int(Level)}],
           [{{'$3','$2'}}]}],
    case ets:select(Tid,MS) of
        [] -> error;
        [Data] -> {ok,Data}
    end.

create(Tid,What,Module,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    ets:insert(Tid,{table_key(What),LevelInt,Config,Module}).
create(Tid,What,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    ets:insert(Tid,{table_key(What),LevelInt,Config}).

set(Tid,What,Config) ->
    LevelInt = level_to_int(maps:get(level,Config)),
    %% Should do this only if the level has actually changed. Possibly
    %% overwrite instead of delete?
    case What of
        logger ->
            _ = ets:select_delete(Tid,[{{'_',{'$1',cached}},
                                        [{'=/=','$1',LevelInt}],
                                        [true]}]),
            ok;
        _ ->
            ok
    end,
    ets:update_element(Tid,table_key(What),[{2,LevelInt},{3,Config}]),
    ok.

set_module_level(Tid,Module,Level) ->
    ets:insert(Tid,{Module,level_to_int(Level)}),
    ok.

reset_module_level(Tid,Module) ->
    ets:delete(Tid,Module), % should possibley overwrite instead of delete?
    ok.

cache_module_level(Tid,Module) ->
    GlobalLevelInt = ets:lookup_element(Tid,?LOGGER_KEY,2),
    ets:insert_new(Tid,{Module,{GlobalLevelInt,cached}}),
    ok.

get(Tid) ->
    {ok,Logger} = get(Tid,logger),
    HMS = [{{table_key('$1'),'_','$2','$3'},[],[{{'$1','$3','$2'}}]}],
    Handlers = ets:select(Tid,HMS),
    MMS = [{{'$1','$2'},[{is_atom,'$1'},{is_integer,'$2'}],[{{'$1','$2'}}]}],
    Modules = ets:select(Tid,MMS),
    {Logger,Handlers,[{M,int_to_level(L)} || {M,L} <- Modules]}.

level_to_int(emergency) -> ?EMERGENCY;
level_to_int(alert) -> ?ALERT;
level_to_int(critical) -> ?CRITICAL;
level_to_int(error) -> ?ERROR;
level_to_int(warning) -> ?WARNING;
level_to_int(notice) -> ?NOTICE;
level_to_int(info) -> ?INFO;
level_to_int(debug) -> ?DEBUG.

int_to_level(?EMERGENCY) -> emergency;
int_to_level(?ALERT) -> alert;
int_to_level(?CRITICAL) -> critical;
int_to_level(?ERROR) -> error;
int_to_level(?WARNING) -> warning;
int_to_level(?NOTICE) -> notice;
int_to_level(?INFO) -> info;
int_to_level(?DEBUG) -> debug.

%%%-----------------------------------------------------------------
%%% Internal

table_key(logger) -> ?LOGGER_KEY;
table_key(HandlerId) -> {?HANDLER_KEY,HandlerId}.
