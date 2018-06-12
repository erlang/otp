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
-module(logger_legacy_SUITE).

-compile(export_all).
-compile({nowarn_deprecated_function,[{gen_fsm,start,3},
                                      {gen_fsm,send_all_state_event,2}]}).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

%%%-----------------------------------------------------------------
%%% This test suite test that log events from within OTP can be
%%% delivered to legacy error_logger event handlers on the same format
%%% as before 'logger' was introduced.
%%%
%%% Before changing the expected format of any of the log events in
%%% this suite, please make sure that the backwards incompatibility it
%%% introduces is ok.
%%% -----------------------------------------------------------------

-define(check(Expected),
        receive Expected ->
                [] = test_server:messages_get()
        after 1000 ->
                ct:fail({report_not_received,
                         {line,?LINE},
                         {got,test_server:messages_get()}})
        end).
-define(check_no_flush(Expected),
        receive Expected ->
                ok
        after 1000 ->
                ct:fail({report_not_received,
                         {line,?LINE},
                         {got,test_server:messages_get()}})
        end).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    logger:add_handler(error_logger,error_logger,
                       #{level=>info,filter_default=>stop}),
    Config.

end_per_suite(_Config) ->
    logger:remove_handler(error_logger),
    ok.

init_per_group(std, Config) ->
    ok = logger:set_handler_config(
           error_logger,filters,
           [{domain,{fun logger_filters:domain/2,{log,super,[otp]}}}]),
    Config;
init_per_group(sasl, Config) ->
    %% Since default level is notice, and progress reports are info,
    %% we need to raise the global logger level to info in order to
    %% receive these.
    ok = logger:set_primary_config(level,info),
    ok = logger:set_handler_config(
           error_logger,filters,
           [{domain,{fun logger_filters:domain/2,{log,super,[otp,sasl]}}}]),

    %% cth_log_redirect checks if sasl is started before displaying
    %% any sasl reports - so just to see the real sasl reports in tc
    %% log:
    {ok,Apps} = application:ensure_all_started(sasl),
    [{stop_apps,Apps}|Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(sasl, Config) ->
    Apps = ?config(stop_apps,Config),
    [application:stop(App) || App <- Apps],
    ok = logger:set_primary_config(level,notice),
    ok;
end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    error_logger:add_report_handler(?MODULE,{event_handler,self()}),
    Config.

end_per_testcase(Case, Config) ->
    %% Using gen_event directly here, instead of
    %% error_logger:delete_report_handler. This is to avoid
    %% automatically stopping the error_logger process due to removing
    %% the last handler.
    gen_event:delete_handler(error_logger,?MODULE,[]),
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [{std,[],[gen_server,
              gen_event,
              gen_fsm,
              gen_statem]},
     {sasl,[],[sasl_reports,
               supervisor_handle_info]}].

all() ->
    [{group,std},
     {group,sasl}].

gen_server(_Config) ->
    {ok,Pid} = gen_server:start(?MODULE,gen_server,[]),
    Msg = fun() -> erlang:error({badmatch,b}) end,
    Pid ! Msg,
    ?check({warning_msg,"** Undefined handle_info in ~p"++_,[?MODULE,Msg]}),
    ok = gen_server:cast(Pid,Msg),
    ?check({error,"** Generic server ~tp terminating"++_,
            [Pid,{'$gen_cast',Msg},gen_server,{{badmatch,b},_}]}).

gen_event(_Config) ->
    {ok,Pid} = gen_event:start(),
    ok = gen_event:add_handler(Pid,?MODULE,gen_event),
    Msg = fun() -> erlang:error({badmatch,b}) end,
    Pid ! Msg,
    ?check({warning_msg,"** Undefined handle_info in ~tp"++_,[?MODULE,Msg]}),
    gen_event:notify(Pid,Msg),
    ?check({error,"** gen_event handler ~p crashed."++_,
            [?MODULE,Pid,Msg,gen_event,{{badmatch,b},_}]}).

gen_fsm(_Config) ->
    {ok,Pid} = gen_fsm:start(?MODULE,gen_fsm,[]),
    Msg = fun() -> erlang:error({badmatch,b}) end,
    Pid ! Msg,
    ?check({warning_msg,"** Undefined handle_info in ~p"++_,[?MODULE,Msg]}),
    gen_fsm:send_all_state_event(Pid,Msg),
    ?check({error,"** State machine ~tp terminating"++_,
            [Pid,Msg,mystate,gen_fsm,{{badmatch,b},_}]}).

gen_statem(_Config) ->
    {ok,Pid} = gen_statem:start(?MODULE,gen_statem,[]),
    Msg = fun() -> erlang:error({badmatch,b}) end,
    Pid ! Msg,
    ?check({error,"** State machine ~tp terminating"++_,
            [Pid,{info,Msg},{mystate,gen_statem},error,{badmatch,b}|_]}).

sasl_reports(Config) ->
    App = {application,?MODULE,[{description, ""},
                                {vsn, "1.0"},
                                {modules, [?MODULE]},
                                {registered, []},
                                {applications, []},
                                {mod, {?MODULE, []}}]},
    AppStr = io_lib:format("~p.",[App]),
    Dir = ?config(priv_dir,Config),
    AppFile = filename:join(Dir,?MODULE_STRING++".app"),
    ok = file:write_file(AppFile,AppStr),
    true = code:add_patha(Dir),
    ok = application:start(?MODULE),
    SupName = sup_name(),
    Pid = whereis(SupName),
    [{ch,ChPid,_,_}] = supervisor:which_children(Pid),
    Node = node(),
    ?check_no_flush({info_report,progress,[{application,?MODULE},
                                           {started_at,Node}]}),
    ?check({info_report,progress,[{supervisor,{local,SupName}},
                                  {started,[{pid,ChPid}|_]}]}),
    ok = gen_server:cast(ChPid, fun() ->
                                        spawn_link(fun() -> receive x->ok end end)
                                end),
    Msg = fun() -> erlang:error({badmatch,b}) end,
    ok = gen_server:cast(ChPid,Msg),
    ?check_no_flush({error,"** Generic server ~tp terminating"++_,
                     [ChPid,{'$gen_cast',Msg},gen_server,{{badmatch,b},_}]}),
    ?check_no_flush({error_report,crash_report,
                     [[{initial_call,_},
                       {pid,ChPid},
                       {registered_name,[]},
                       {error_info,{error,{badmatch,b},_}},
                       {ancestors,_},
                       {message_queue_len,_},
                       {messages,_},
                       {links,[Pid,Neighbour]},
                       {dictionary,_},
                       {trap_exit,_},
                       {status,_},
                       {heap_size,_},
                       {stack_size,_},
                       {reductions,_}],
                      [{neighbour,[{pid,Neighbour},
                                   {registered_name,_},
                                   {initial_call,_},
                                   {current_function,_},
                                   {ancestors,_},
                                   {message_queue_len,_},
                                   {links,[ChPid]},
                                   {trap_exit,_},
                                   {status,_},
                                   {heap_size,_},
                                   {stack_size,_},
                                   {reductions,_},
                                   {current_stacktrace,_}]}]]}),
    ?check_no_flush({error_report,supervisor_report,
                     [{supervisor,{local,SupName}},
                      {errorContext,child_terminated},
                      {reason,{{badmatch,b},_}},
                      {offender,[{pid,ChPid}|_]}]}),
    ?check({info_report,progress,[{supervisor,{local,SupName}},
                                  {started,_}]}),

    ok = application:stop(?MODULE),
    ?check({info_report,std_info,[{application,?MODULE},
                                  {exited,stopped},
                                  {type,temporary}]}).

sasl_reports(cleanup,_Config) ->
    application:stop(?MODULE).

supervisor_handle_info(_Config) ->
    {ok,Pid} = supervisor:start_link({local,sup_name()},?MODULE,supervisor),
    ?check({info_report,progress,[{supervisor,_},{started,_}]}),
    Pid ! msg,
    ?check({error,"Supervisor received unexpected message: ~tp~n",[msg]}).

supervisor_handle_info(cleanup,_Config) ->
    Pid = whereis(sup_name()),
    unlink(Pid),
    exit(Pid,shutdown).

%%%-----------------------------------------------------------------
%%% Callbacks for error_logger event handler, gen_server, gen_statem,
%%% gen_fsm, gen_event, supervisor and application.
start(_,_) ->
    supervisor:start_link({local,sup_name()},?MODULE,supervisor).

init(supervisor) ->
    {ok,{#{},[#{id=>ch,start=>{gen_server,start_link,[?MODULE,gen_server,[]]}}]}};
init(StateMachine) when StateMachine==gen_statem; StateMachine==gen_fsm ->
    {ok,mystate,StateMachine};
init(State) ->
    {ok,State}.

%% error_logger event handler
handle_event({Tag,_Gl,{_Pid,Type,Report}},{_,Pid}=State) ->
    Pid ! {Tag,Type,Report},
    {ok,State};
%% other gen_event
handle_event(Fun,State) when is_function(Fun) ->
    Fun(),
    {next_state,State}.

%% gen_fsm
handle_event(Fun,State,Data) when is_function(Fun)  ->
    Fun(),
    {next_state,State,Data}.

%% gen_statem
handle_event(info,Fun,State,Data) when is_function(Fun)  ->
    Fun(),
    {next_state,State,Data}.

%% gen_server
handle_cast(Fun,State) when is_function(Fun)  ->
    Fun(),
    {noreply,State}.

%% gen_statem
callback_mode() ->
    handle_event_function.

%%%-----------------------------------------------------------------
%%% Internal
sup_name() ->
    list_to_atom(?MODULE_STRING++"_sup").
