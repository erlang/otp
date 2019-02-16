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
-module(logger_backend).

-export([log_allowed/2]).

-include("logger_internal.hrl").

-define(OWN_KEYS,[level,filters,filter_default,handlers]).

%%%-----------------------------------------------------------------
%%% The default logger backend
log_allowed(Log, Tid) ->
    {ok,Config} = logger_config:get(Tid,primary),
    Filters = maps:get(filters,Config,[]),
    case apply_filters(primary,Log,Filters,Config) of
        stop ->
            ok;
        Log1 ->
            Handlers = maps:get(handlers,Config,[]),
            call_handlers(Log1,Handlers,Tid)
    end,
    ok.

call_handlers(#{level:=Level}=Log,[Id|Handlers],Tid) ->
    case logger_config:get(Tid,Id,Level) of
        {ok,#{module:=Module}=Config} ->
            Filters = maps:get(filters,Config,[]),
            case apply_filters(Id,Log,Filters,Config) of
                stop ->
                    ok;
                Log1 ->
                    Config1 = maps:without(?OWN_KEYS,Config),
                    try Module:log(Log1,Config1)
                    catch C:R:S ->
                            case logger:remove_handler(Id) of
                                ok ->
                                    logger:internal_log(
                                      error,{removed_failing_handler,Id}),
                                    ?LOG_INTERNAL(
                                       debug,
                                       [{logger,removed_failing_handler},
                                        {handler,{Id,Module}},
                                        {log_event,Log1},
                                        {config,Config1},
                                        {reason,{C,R,filter_stacktrace(S)}}]);
                                {error,{not_found,_}} ->
                                    %% Probably already removed by other client
                                    %% Don't report again
                                    ok;
                                {error,Reason} ->
                                    ?LOG_INTERNAL(
                                       debug,
                                       [{logger,remove_handler_failed},
                                        {reason,Reason}])
                            end
                    end
            end;
        _ ->
            ok
    end,
    call_handlers(Log,Handlers,Tid);
call_handlers(_Log,[],_Tid) ->
    ok.

apply_filters(Owner,Log,Filters,Config) ->
    case do_apply_filters(Owner,Log,Filters,ignore) of
        stop ->
            stop;
        ignore ->
            case maps:get(filter_default,Config) of
                log ->
                    Log;
                stop ->
                    stop
            end;
        Log1 ->
            Log1
    end.

do_apply_filters(Owner,Log,[{_Id,{FilterFun,FilterArgs}}=Filter|Filters],State) ->
    try FilterFun(Log,FilterArgs) of
        stop ->
            stop;
        ignore ->
            do_apply_filters(Owner,Log,Filters,State);
        Log1=#{level:=Level,msg:=Msg,meta:=Meta}
          when is_atom(Level), ?IS_MSG(Msg), is_map(Meta) ->
            do_apply_filters(Owner,Log1,Filters,log);
        Bad ->
            handle_filter_failed(Filter,Owner,Log,{bad_return_value,Bad})
    catch C:R:S ->
            handle_filter_failed(Filter,Owner,Log,{C,R,filter_stacktrace(S)})
    end;
do_apply_filters(_Owner,_Log,[],ignore) ->
    ignore;
do_apply_filters(_Owner,Log,[],log) ->
    Log.

handle_filter_failed({Id,_}=Filter,Owner,Log,Reason) ->
    case logger_server:remove_filter(Owner,Id) of
        ok ->
            logger:internal_log(error,{removed_failing_filter,Id}),
            ?LOG_INTERNAL(debug,
                          [{logger,removed_failing_filter},
                           {filter,Filter},
                           {owner,Owner},
                           {log_event,Log},
                           {reason,Reason}]);
        _ ->
            ok
    end,
    ignore.

filter_stacktrace(Stacktrace) ->
    logger:filter_stacktrace(?MODULE,Stacktrace).
