%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(logger_bench_SUITE).

-compile(export_all).

%%%-----------------------------------------------------------------
%%% To include lager tests, add paths to lager and goldrush
%%% (goldrush is a dependency inside the lager repo)
%%% 
%%% To print data to .csv files, add the following to a config file:
%%%   {print_csv,[{console_handler,[{path,"/some/dir/"}]}]}.
%%%
%%%-----------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

-define(msg,lists:flatten(string:pad("Log from "++atom_to_list(?FUNCTION_NAME)++
                                         ":"++integer_to_list(?LINE),
                                      80,trailing,$*))).
-define(meta,#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
               pid=>self()}).

-define(NO_COMPARE,[profile]).

-define(TIMES,100000).

suite() ->
    [{timetrap,{seconds,120}}].

init_per_suite(Config) ->
    DataDir = ?config(data_dir,Config),
    have_lager() andalso make(DataDir),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) ->
    H = remove_all_handlers(),
    do_init_per_group(Group),
    [{handlers,H}|Config].

do_init_per_group(minimal_handler) ->
    ok = logger:add_handler(?MODULE,?MODULE,#{level=>error,filter_default=>log});
do_init_per_group(console_handler) ->
    ok = logger:add_handler(?MODULE,logger_std_h,
                            #{filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS,
                              logger_std_h=>#{type=>standard_io,
                                              toggle_sync_qlen => ?TIMES+1,
                                              drop_new_reqs_qlen => ?TIMES+2,
                                              flush_reqs_qlen => ?TIMES+3,
                                              enable_burst_limit => false}}),
    have_lager() andalso lager_helper:start(),
    ok.

end_per_group(Group, Config) ->
    case ?config(saved_config,Config) of
        {_,[{bench,Bench}]} ->
            print_compare_chart(Group,Bench);
        _ ->
            ok
    end,
    add_all_handlers(?config(handlers,Config)),
    do_end_per_group(Group).

do_end_per_group(minimal_handler) ->
    ok = logger:remove_handler(?MODULE);
do_end_per_group(console_handler) ->
    ok = logger:remove_handler(?MODULE),
    have_lager() andalso lager_helper:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    wait_for_handlers(),
    ok.

wait_for_handlers() ->
    wait_for_handler(?MODULE),
    wait_for_handler(lager_event).

wait_for_handler(Handler) ->
    case whereis(Handler) of
        undefined ->
            io:format("~p: noproc1",[Handler]),
            ok;
        Pid ->
            case process_info(Pid,message_queue_len) of
                {_,0} ->
                    io:format("~p: queue=~p",[Handler,0]),
                    ok;
                {_,Q} ->
                    io:format("~p: queue=~p",[Handler,Q]),
                   timer:sleep(2000),
                    wait_for_handler(Handler);
                undefined ->
                    io:format("~p: noproc2",[Handler]),
                    ok
            end
    end.

groups() ->
    [{minimal_handler,[],[log,
                          log_drop,
                          log_drop_by_handler,
                          macro,
                          macro_drop,
                          macro_drop_by_handler,
                          error_logger,
                          error_logger_drop,
                          error_logger_drop_by_handler
                         ]},
     {console_handler,[],[%profile,
                          log,
                          log_drop,
                          log_drop_by_handler,
                          %% log_handler_complete,
                          macro,
                          macro_drop,
                          macro_drop_by_handler,
                          %% macro_handler_complete,
                          error_logger,
                          error_logger_drop,
                          error_logger_drop_by_handler%% ,
                          %% error_logger_handler_complete
                         ] ++ lager_cases()}
    ].

lager_cases() ->
    case have_lager() of
        true ->
            [lager_log,
             lager_log_drop,
             lager_log_drop_by_handler,
             %% lager_log_handler_complete,
             lager_parsetrans,
             lager_parsetrans_drop,
             lager_parsetrans_drop_by_handler%% ,
             %% lager_parsetrans_handler_complete
            ];
        false ->
            []
    end.


all() -> 
    [{group,minimal_handler},
     {group,console_handler}
    ].

log(Config) ->
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_func/2, [error,?msg], Times).

log_drop(Config) ->
    Times = ?TIMES*100,
    ok = logger:set_primary_config(level,error),
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_func/2, [info,?msg], Times).

log_drop(cleanup,_Config) ->
    ok = logger:set_primary_config(level,info).

log_drop_by_handler(Config) ->
    Times = ?TIMES,
    %% just ensure correct levels
    ok = logger:set_primary_config(level,info),
    ok = logger:set_handler_config(?MODULE,level,error),
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_func/2, [info,?msg], Times).

log_handler_complete(Config) ->
    ok = logger:set_handler_config(?MODULE,formatter,
                                   {?MODULE,?DEFAULT_FORMAT_CONFIG}),
    handler_complete(Config, ?FUNCTION_NAME, fun do_log_func/2, [error,?msg]).

log_handler_complete(cleanup,_Config) ->
    ok=logger:set_handler_config(?MODULE,formatter,
                                 {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}).
    
macro(Config) ->
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_macro/2,[error,?msg], Times).
    
macro_drop(Config) ->
    Times = ?TIMES*100,
    ok = logger:set_primary_config(level,error),
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_macro/2,[info,?msg], Times).

macro_drop(cleanup,_Config) ->
    ok = logger:set_primary_config(level,info).

macro_drop_by_handler(Config) ->
    Times = ?TIMES,
    %% just ensure correct levels
    ok = logger:set_primary_config(level,info),
    ok = logger:set_handler_config(?MODULE,level,error),
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_macro/2, [info,?msg], Times).
    
macro_handler_complete(Config) ->
    ok = logger:set_handler_config(?MODULE,formatter,
                                   {?MODULE,?DEFAULT_FORMAT_CONFIG}),
    handler_complete(Config, ?FUNCTION_NAME, fun do_log_macro/2, [error,?msg]).

macro_handler_complete(cleanup,_Config) ->
    ok=logger:set_handler_config(?MODULE,formatter,
                                 {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}).
    
error_logger(Config) ->
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun do_error_logger/2, [error,?msg], Times).
    
error_logger_drop(Config) ->
    Times = ?TIMES*100,
    ok = logger:set_primary_config(level,error),
    run_benchmark(Config,?FUNCTION_NAME,fun do_error_logger/2, [info,?msg], Times).

error_logger_drop(cleanup,_Config) ->
    ok = logger:set_primary_config(level,info).

error_logger_drop_by_handler(Config) ->
    Times = ?TIMES,
    %% just ensure correct levels
    ok = logger:set_primary_config(level,info),
    ok = logger:set_handler_config(?MODULE,level,error),
    run_benchmark(Config,?FUNCTION_NAME,fun do_log_func/2, [info,?msg], Times).

error_logger_handler_complete(Config) ->
    ok = logger:set_handler_config(?MODULE,formatter,
                                   {?MODULE,?DEFAULT_FORMAT_CONFIG}),
    handler_complete(Config, ?FUNCTION_NAME, fun do_error_logger/2, [error,?msg]).

error_logger_handler_complete(cleanup,_Config) ->
    ok=logger:set_handler_config(?MODULE,formatter,
                                 {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}).
    
lager_log(Config) ->
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun lager_helper:do_func/2, [error,?msg], Times).

lager_log_drop(Config) ->
    Times = ?TIMES*100,
    run_benchmark(Config,?FUNCTION_NAME,fun lager_helper:do_func/2, [info,?msg], Times).

lager_log_drop_by_handler(Config) ->
    %% This concept does not exist, so doing same as lager_log_drop/1
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun lager_helper:do_func/2, [info,?msg], Times).

lager_log_handler_complete(Config) ->
    handler_complete(Config, ?FUNCTION_NAME, fun lager_helper:do_func/2, [error,?msg]).
    
lager_parsetrans(Config) ->
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun lager_helper:do_parsetrans/2, [error,?msg], Times).

lager_parsetrans_drop(Config) ->
    Times = ?TIMES*100,
    run_benchmark(Config,?FUNCTION_NAME,fun lager_helper:do_parsetrans/2, [info,?msg], Times).

lager_parsetrans_drop_by_handler(Config) ->
    %% This concept does not exist, so doing same as lager_parsetrans_drop/1
    Times = ?TIMES,
    run_benchmark(Config,?FUNCTION_NAME,fun lager_helper:do_parsetrans/2, [info,?msg], Times).

lager_parsetrans_handler_complete(Config) ->
    handler_complete(Config, ?FUNCTION_NAME, fun lager_helper:do_parsetrans/2, [error,?msg]).


profile(Config) ->
    Times = ?TIMES,
    %% fprof:apply(fun repeated_apply/3,[fun lager_helper:do_func/2,[error,?msg],Times]),
    fprof:apply(fun repeated_apply/3,[fun do_log_func/2,[error,?msg],Times]),
    ok = fprof:profile(),
    ok = fprof:analyse(dest,"../fprof.analyse"),
    ok.
        
%%%-----------------------------------------------------------------
%%% Internal
%% Handler
log(_Log,_Config) ->
    ok.

format(Log=#{meta:=#{pid:=Pid}},Config) when is_pid(Pid) ->
    String = ?DEFAULT_FORMATTER:format(Log,Config),
    Pid ! done,
    String;
format(Log=#{meta:=#{pid:=PidStr}},Config) when is_list(PidStr) ->
    String = ?DEFAULT_FORMATTER:format(Log,Config),
    list_to_pid(PidStr) ! done,
    String.

handler_complete(Config, TC, Fun, Args) ->
    Times = ?TIMES,
    Start = os:perf_counter(microsecond),
    repeated_apply(Fun, Args, Times),
    MSecs = wait_for_done(Start,Times),
    calc_and_report(Config,TC,MSecs,Times).

wait_for_done(Start,0) ->
    os:perf_counter(microsecond) - Start;
wait_for_done(Start,N) ->
    receive
        done ->
            wait_for_done(Start,N-1)
    after 20000 ->
            ct:fail("missing " ++ integer_to_list(N) ++ " replys")
    end.

%%%-----------------------------------------------------------------
%%% Benchmark stuff
run_benchmark(Config,Tag,Fun,Args,Times) ->
    _ = erlang:apply(Fun, Args), % apply once to ensure level is cached
    MSecs = measure_repeated_op(Fun, Args, Times),
    %% fprof:profile(),
    %% fprof:analyse(dest,"../"++atom_to_list(Tag)++".prof"),
    calc_and_report(Config,Tag,MSecs,Times).

measure_repeated_op(Fun, Args, Times) ->
    Start = os:perf_counter(microsecond),
    %% fprof:apply(fun repeated_apply/3, [Fun, Args, Times]),
    repeated_apply(Fun, Args, Times),
    os:perf_counter(microsecond) - Start.

repeated_apply(_F, _Args, Times) when Times =< 0 ->
    ok;
repeated_apply(F, Args, Times) ->
    erlang:apply(F, Args),
    repeated_apply(F, Args, Times - 1).

calc_and_report(Config,Tag,MSecs,Times) ->
    IOPS = trunc(Times * (1000000 / MSecs)),
    ct_event:notify(#event{ name = benchmark_data, data = [{value,IOPS}] }),
    ct:print("~p:~n~p IOPS, ~p us", [Tag, IOPS, MSecs]),
    ct:comment("~p IOPS, ~p us", [IOPS, MSecs]),
    Bench = case ?config(saved_config,Config) of
                {_,[{bench,B}]} -> B;
                undefined -> []
            end,
    {save_config,[{bench,[{Tag,IOPS,MSecs}|Bench]}]}.

remove_all_handlers() ->
    #{handlers:=Hs} = logger:i(),
    [logger:remove_handler(Id) || {Id,_,_} <- Hs],
    Hs.

add_all_handlers(Hs) ->
    [logger:add_handler(Id,Mod,Config) || {Id,Mod,Config} <- Hs],
    ok.

%%%-----------------------------------------------------------------
%%% Call logger in different ways
do_log_func(Level,Msg) ->
    logger:Level(Msg,[],?meta).

do_log_macro(error,Msg) ->
    ?LOG_ERROR(Msg,[]);
do_log_macro(info,Msg) ->
    ?LOG_INFO(Msg,[]);
do_log_macro(debug,Msg) ->
    ?LOG_DEBUG(Msg,[]).

do_error_logger(error,Msg) ->
    error_logger:error_msg(Msg,[]);
do_error_logger(info,Msg) ->
    error_logger:info_msg(Msg,[]).

%%%-----------------------------------------------------------------
%%% 
print_compare_chart(Group,Bench) ->
    io:format("~-20s~12s~12s~12s~12s",
              ["Microseconds:","Log","Drop","HDrop","Complete"]),
    io:format(user,"~-20s~12s~12s~12s~12s~n",
              ["Microseconds:","Log","Drop","HDrop","Complete"]),
    {Log,Drop,HDrop,Comp} = sort_bench(Bench,[],[],[],[]),
    print_compare_chart(Log,Drop,HDrop,Comp),
    io:format(user,"~n",[]),
    maybe_print_csv_files(Group,
                          [{log,Log},{drop,Drop},{hdrop,HDrop},{comp,Comp}]).

print_compare_chart([{What,LIOPS,LMSecs}|Log],
                    [{What,DIOPS,DMSecs}|Drop],
                    [{What,HIOPS,HMSecs}|HDrop],
                    [{What,CIOPS,CMSecs}|Comp]) ->
    io:format("~-20w~12w~12w~12w~12w",[What,LMSecs,DMSecs,HMSecs,CMSecs]),
    io:format(user,"~-20w~12w~12w~12w~12w~n",[What,LMSecs,DMSecs,HMSecs,CMSecs]),
    print_compare_chart(Log,Drop,HDrop,Comp);
print_compare_chart([{What,LIOPS,LMSecs}|Log],
                    [{What,DIOPS,DMSecs}|Drop],
                    [{What,HIOPS,HMSecs}|HDrop],
                    []=Comp) ->
    io:format("~-20w~12w~12w~12w",[What,LMSecs,DMSecs,HMSecs]),
    io:format(user,"~-20w~12w~12w~12w~n",[What,LMSecs,DMSecs,HMSecs]),
    print_compare_chart(Log,Drop,HDrop,Comp);
print_compare_chart([],[],[],[]) ->
    ok;
print_compare_chart(Log,Drop,HDrop,Comp) ->
    ct:fail({Log,Drop,HDrop,Comp}).

sort_bench([{TC,IOPS,MSecs}|Bench],Log,Drop,HDrop,Comp) ->
    case lists:member(TC,?NO_COMPARE) of
        true ->
            sort_bench(Bench,Log,Drop,HDrop,Comp);
        false ->
            TCStr = atom_to_list(TC),
            {What,Type} =
                case re:run(TCStr,"(.*)_(drop.*)",
                            [{capture,all_but_first,list}]) of
                    {match,[WhatStr,TypeStr]} ->
                        {list_to_atom(WhatStr),list_to_atom(TypeStr)};
                    nomatch ->
                        case re:run(TCStr,"(.*)_(handler_complete.*)",
                                    [{capture,all_but_first,list}]) of
                            {match,[WhatStr,TypeStr]} ->
                                {list_to_atom(WhatStr),list_to_atom(TypeStr)};
                            nomatch ->
                                {TC,log}
                        end
                end,
            case Type of
                log ->
                    sort_bench(Bench,[{What,IOPS,MSecs}|Log],Drop,HDrop,Comp);
                drop ->
                    sort_bench(Bench,Log,[{What,IOPS,MSecs}|Drop],HDrop,Comp);
                drop_by_handler ->
                    sort_bench(Bench,Log,Drop,[{What,IOPS,MSecs}|HDrop],Comp);
                handler_complete ->
                    sort_bench(Bench,Log,Drop,HDrop,[{What,IOPS,MSecs}|Comp])
            end
    end;
sort_bench([],Log,Drop,HDrop,Comp) ->
    {lists:keysort(1,Log),
     lists:keysort(1,Drop),
     lists:keysort(1,HDrop),
     lists:keysort(1,Comp)}.

maybe_print_csv_files(Group,Data) ->
    case ct:get_config({print_csv,Group}) of
        undefined ->
            ok;
        Cfg ->
            Path = proplists:get_value(path,Cfg,".."),
            Files = [begin
                         File = filename:join(Path,F)++".csv",
                         case filelib:is_regular(File) of
                             true ->
                                 {ok,Fd} = file:open(File,[append]),
                                 Fd;
                             false ->
                                 {ok,Fd} = file:open(File,[write]),
                                 ok = file:write(Fd,
                                                 "error_logger,lager_log,"
                                                 "lager_parsetrans,logger_log,"
                                                 "logger_macro\n"),
                                 Fd
                         end
                     end || {F,_} <- Data],
            [print_csv_file(F,D) || {F,D} <- lists:zip(Files,Data)],
            [file:close(Fd) || Fd <- Files],
            ok
    end.

print_csv_file(Fd,{_,Data}) ->
    AllIOPS = [integer_to_list(IOPS) || {_,IOPS,_} <- Data],
    ok = file:write(Fd,lists:join(",",AllIOPS)++"\n").

have_lager() ->
    code:ensure_loaded(lager) == {module,lager}.

make(Dir) ->
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    up_to_date = make:all([load]),
    ok = file:set_cwd(Cwd),
    code:add_path(Dir).
