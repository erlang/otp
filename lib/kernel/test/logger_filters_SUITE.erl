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
-module(logger_filters_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

-define(ndlog,
        #{level=>info,msg=>{"Line: ~p",[?LINE]},meta=>#{}}).
-define(dlog(Domain),
        #{level=>info,msg=>{"Line: ~p",[?LINE]},meta=>#{domain=>Domain}}).
-define(llog(Level),
        #{level=>Level,msg=>{"Line: ~p",[?LINE]},meta=>#{}}).
-define(plog,
        #{level=>info,
          msg=>{report,#{label=>{?MODULE,progress}}},
          meta=>#{line=>?LINE}}).
-define(rlog(Node),
        #{level=>info,
          msg=>{"Line: ~p",[?LINE]},
          meta=>#{gl=>rpc:call(Node,erlang,whereis,[user])}}).

-define(TRY(X), my_try(fun() -> X end)).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [].

all() -> 
    [domain,
     level,
     progress,
     remote_gl].

domain(_Config) ->
    L1 = logger_filters:domain(L1=?dlog([]),{log,super,[]}),
    stop = logger_filters:domain(?dlog([]),{stop,super,[]}),
    L2 = logger_filters:domain(L2=?dlog([]),{log,sub,[]}),
    stop = logger_filters:domain(?dlog([]),{stop,sub,[]}),
    L3 = logger_filters:domain(L3=?dlog([]),{log,equal,[]}),
    stop = logger_filters:domain(?dlog([]),{stop,equal,[]}),
    ignore = logger_filters:domain(?dlog([]),{log,not_equal,[]}),
    ignore = logger_filters:domain(?dlog([]),{stop,not_equal,[]}),
    ignore = logger_filters:domain(?dlog([]),{log,undefined,[]}),
    ignore = logger_filters:domain(?dlog([]),{stop,undefined,[]}),

    L4 = logger_filters:domain(L4=?dlog([a]),{log,super,[a,b]}),
    stop = logger_filters:domain(?dlog([a]),{stop,super,[a,b]}),
    ignore = logger_filters:domain(?dlog([a]),{log,sub,[a,b]}),
    ignore = logger_filters:domain(?dlog([a]),{stop,sub,[a,b]}),
    ignore = logger_filters:domain(?dlog([a]),{log,equal,[a,b]}),
    ignore = logger_filters:domain(?dlog([a]),{stop,equal,[a,b]}),
    L5 = logger_filters:domain(L5=?dlog([a]),{log,not_equal,[a,b]}),
    stop = logger_filters:domain(?dlog([a]),{stop,not_equal,[a,b]}),
    ignore = logger_filters:domain(?dlog([a]),{log,undefined,[a,b]}),
    ignore = logger_filters:domain(?dlog([a]),{stop,undefined,[a,b]}),

    ignore = logger_filters:domain(?dlog([a,b]),{log,super,[a]}),
    ignore = logger_filters:domain(?dlog([a,b]),{stop,super,[a]}),
    L6 = logger_filters:domain(L6=?dlog([a,b]),{log,sub,[a]}),
    stop = logger_filters:domain(?dlog([a,b]),{stop,sub,[a]}),
    ignore = logger_filters:domain(?dlog([a,b]),{log,equal,[a]}),
    ignore = logger_filters:domain(?dlog([a,b]),{stop,equal,[a]}),
    L7 = logger_filters:domain(L7=?dlog([a,b]),{log,not_equal,[a]}),
    stop = logger_filters:domain(?dlog([a,b]),{stop,not_equal,[a]}),
    ignore = logger_filters:domain(?dlog([a,b]),{log,undefined,[a]}),
    ignore = logger_filters:domain(?dlog([a,b]),{stop,undefined,[a]}),

    ignore = logger_filters:domain(?ndlog,{log,super,[a]}),
    ignore = logger_filters:domain(?ndlog,{stop,super,[a]}),
    ignore = logger_filters:domain(?ndlog,{log,sub,[a]}),
    ignore = logger_filters:domain(?ndlog,{stop,sub,[a]}),
    ignore = logger_filters:domain(?ndlog,{log,equal,[a]}),
    ignore = logger_filters:domain(?ndlog,{stop,equal,[a]}),
    L8 = logger_filters:domain(L8=?ndlog,{log,not_equal,[a]}),
    stop = logger_filters:domain(?ndlog,{stop,not_equal,[a]}),
    L9 = logger_filters:domain(L9=?ndlog,{log,undefined,[a]}),
    stop = logger_filters:domain(?ndlog,{stop,undefined,[a]}),

    L10 = logger_filters:domain(L10=?dlog([a,b,c,d]),{log,super,[a,b,c,d]}),
    stop = logger_filters:domain(?dlog([a,b,c,d]),{stop,super,[a,b,c,d]}),
    L11 = logger_filters:domain(L11=?dlog([a,b,c,d]),{log,sub,[a,b,c,d]}),
    stop = logger_filters:domain(?dlog([a,b,c,d]),{stop,sub,[a,b,c,d]}),
    L12 = logger_filters:domain(L12=?dlog([a,b,c,d]),{log,equal,[a,b,c,d]}),
    stop = logger_filters:domain(?dlog([a,b,c,d]),{stop,equal,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog([a,b,c,d]),{log,not_equal,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog([a,b,c,d]),{stop,not_equal,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog([a,b,c,d]),{log,undefined,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog([a,b,c,d]),{stop,undefined,[a,b,c,d]}),

    %% A domain field in meta which is not a list is allowed by the
    %% filter, but since MatchDomain is always a list of atoms, only
    %% Action=not_equal can ever match.
    ignore = logger_filters:domain(?dlog(dummy),{log,super,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{stop,super,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{log,sub,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{stop,sub,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{log,equal,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{stop,equal,[a,b,c,d]}),
    L13 = logger_filters:domain(L13=?dlog(dummy),{log,not_equal,[a,b,c,d]}),
    stop = logger_filters:domain(?dlog(dummy),{stop,not_equal,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{log,undefined,[a,b,c,d]}),
    ignore = logger_filters:domain(?dlog(dummy),{stop,undefined,[a,b,c,d]}),

    {error,badarg} = ?TRY(logger_filters:domain(?ndlog,bad)),
    {error,badarg} = ?TRY(logger_filters:domain(?ndlog,{bad,super,[]})),
    {error,badarg} = ?TRY(logger_filters:domain(?ndlog,{log,bad,[]})),
    {error,badarg} = ?TRY(logger_filters:domain(?ndlog,{log,super,bad})),

    ok.

level(_Config) ->
    ignore = logger_filters:level(?llog(info),{log,lt,info}),
    ignore = logger_filters:level(?llog(info),{stop,lt,info}),
    ignore = logger_filters:level(?llog(info),{log,gt,info}),
    ignore = logger_filters:level(?llog(info),{stop,gt,info}),
    L1 = logger_filters:level(L1=?llog(info),{log,lteq,info}),
    stop = logger_filters:level(?llog(info),{stop,lteq,info}),
    L2 = logger_filters:level(L2=?llog(info),{log,gteq,info}),
    stop = logger_filters:level(?llog(info),{stop,gteq,info}),
    L3 = logger_filters:level(L3=?llog(info),{log,eq,info}),
    stop = logger_filters:level(?llog(info),{stop,eq,info}),
    ignore = logger_filters:level(?llog(info),{log,neq,info}),
    ignore = logger_filters:level(?llog(info),{stop,neq,info}),

    ignore = logger_filters:level(?llog(error),{log,lt,info}),
    ignore = logger_filters:level(?llog(error),{stop,lt,info}),
    L4 = logger_filters:level(L4=?llog(error),{log,gt,info}),
    stop = logger_filters:level(?llog(error),{stop,gt,info}),
    ignore = logger_filters:level(?llog(error),{log,lteq,info}),
    ignore = logger_filters:level(?llog(error),{stop,lteq,info}),
    L5 = logger_filters:level(L5=?llog(error),{log,gteq,info}),
    stop = logger_filters:level(?llog(error),{stop,gteq,info}),
    ignore = logger_filters:level(?llog(error),{log,eq,info}),
    ignore = logger_filters:level(?llog(error),{stop,eq,info}),
    L6 = logger_filters:level(L6=?llog(error),{log,neq,info}),
    stop = logger_filters:level(?llog(error),{stop,neq,info}),

    L7 = logger_filters:level(L7=?llog(info),{log,lt,error}),
    stop = logger_filters:level(?llog(info),{stop,lt,error}),
    ignore = logger_filters:level(?llog(info),{log,gt,error}),
    ignore = logger_filters:level(?llog(info),{stop,gt,error}),
    L8 = logger_filters:level(L8=?llog(info),{log,lteq,error}),
    stop = logger_filters:level(?llog(info),{stop,lteq,error}),
    ignore = logger_filters:level(?llog(info),{log,gteq,error}),
    ignore = logger_filters:level(?llog(info),{stop,gteq,error}),
    ignore = logger_filters:level(?llog(info),{log,eq,error}),
    ignore = logger_filters:level(?llog(info),{stop,eq,error}),
    L9 = logger_filters:level(L9=?llog(info),{log,neq,error}),
    stop = logger_filters:level(?llog(info),{stop,neq,error}),

    {error,badarg} = ?TRY(logger_filters:level(?llog(info),bad)),
    {error,badarg} = ?TRY(logger_filters:level(?llog(info),{bad,eq,info})),
    {error,badarg} = ?TRY(logger_filters:level(?llog(info),{log,bad,info})),
    {error,badarg} = ?TRY(logger_filters:level(?llog(info),{log,eq,bad})),

    ok.

progress(_Config) ->
    L1 = logger_filters:progress(L1=?plog,log),
    stop = logger_filters:progress(?plog,stop),
    ignore = logger_filters:progress(?ndlog,log),
    ignore = logger_filters:progress(?ndlog,stop),
    
    {error,badarg} = ?TRY(logger_filters:progress(?plog,bad)),
    
    ok.

remote_gl(_Config) ->
    {ok,Node} = test_server:start_node(?FUNCTION_NAME,slave,[]),
    L1 = logger_filters:remote_gl(L1=?rlog(Node),log),
    stop = logger_filters:remote_gl(?rlog(Node),stop),
    ignore = logger_filters:remote_gl(?ndlog,log),
    ignore = logger_filters:remote_gl(?ndlog,stop),

    {error,badarg} = ?TRY(logger_filters:remote_gl(?rlog(Node),bad)),
    ok.

remote_gl(cleanup,_Config) ->
    [test_server:stop_node(N) || N<-nodes()].

%%%-----------------------------------------------------------------
%%% Called by macro ?TRY(X)
my_try(Fun) ->
    try Fun() catch C:R -> {C,R} end.
