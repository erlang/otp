%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

%%% @doc Common Test Framework callback module.
%%%
%%% <p>This module exports framework callback functions which are
%%% called from the test_server.</p>

-module(ct_framework).

-export([init_tc/3, end_tc/3, end_tc/4, get_suite/2, get_all_cases/1]).
-export([report/2, warn/1, error_notification/4]).

-export([get_log_dir/0, get_logopts/0, format_comment/1, get_html_wrapper/4]).

-export([error_in_suite/1, init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2]).

-include("ct.hrl").
-include("ct_event.hrl").
-include("ct_util.hrl").

-define(val(Key, List), proplists:get_value(Key, List)). 
-define(val(Key, List, Def), proplists:get_value(Key, List, Def)).
-define(rev(L), lists:reverse(L)).

%%%-----------------------------------------------------------------
%%% @spec init_tc(Mod,Func,Args) -> {ok,NewArgs} | {error,Reason} |
%%%         {skip,Reason} | {auto_skip,Reason}
%%%      Mod = atom()
%%%      Func = atom()
%%%      Args = list()
%%%      NewArgs = list()
%%%      Reason = term()
%%%
%%% @doc Test server framework callback, called by the test_server
%%% when a new test case is started.
init_tc(_,{end_per_testcase_not_run,_},[Config]) ->
    %% Testcase is completed (skipped or failed), but end_per_testcase
    %% is not run - don't call pre-hook.
    {ok,[Config]};
init_tc(Mod,EPTC={end_per_testcase,_},[Config]) ->
    %% in case Mod == ct_framework, lookup the suite name
    Suite = get_suite_name(Mod, Config),
    case ct_hooks:init_tc(Suite,EPTC,Config) of
	NewConfig when is_list(NewConfig) ->
	    {ok,[NewConfig]};
	Other->
	    Other
    end;

init_tc(Mod,Func0,Args) ->
    %% in case Mod == ct_framework, lookup the suite name
    Suite = get_suite_name(Mod, Args),
    {Func,HookFunc} = case Func0 of
			  {init_per_testcase,F} -> {F,Func0};
			  _                     -> {Func0,Func0}
		      end,

    %% check if previous testcase was interpreted and has left
    %% a "dead" trace window behind - if so, kill it
    case ct_util:get_testdata(interpret) of
	{What,kill,{TCPid,AttPid}} ->
	    ct_util:kill_attached(TCPid,AttPid),
	    ct_util:set_testdata({interpret,{What,kill,{undefined,undefined}}});
	_ ->
	    ok
    end,

    case Func=/=end_per_suite
	andalso Func=/=end_per_group
	andalso ct_util:get_testdata(skip_rest) of
	true ->
            initialize(false,Mod,Func,Args),
	    {auto_skip,"Repeated test stopped by force_stop option"};
	_ ->
	    case ct_util:get_testdata(curr_tc) of
		{Suite,{suite0_failed,{require,Reason}}} ->
                    initialize(false,Mod,Func,Args),
		    {auto_skip,{require_failed_in_suite0,Reason}};
		{Suite,{suite0_failed,_}=Failure} ->
                    initialize(false,Mod,Func,Args),
		    {fail,Failure};
		_ ->
		    ct_util:update_testdata(curr_tc,
					    fun(undefined) ->
						    [{Suite,Func}];
					       (Running) ->
						    [{Suite,Func}|Running]
					    end, [create]),
		    case ct_util:read_suite_data({seq,Suite,Func}) of
			undefined ->
			    init_tc1(Mod,Suite,Func,HookFunc,Args);
			Seq when is_atom(Seq) ->
			    case ct_util:read_suite_data({seq,Suite,Seq}) of
				[Func|TCs] -> % this is the 1st case in Seq
				    %% make sure no cases in this seq are
				    %% marked as failed from an earlier execution
				    %% in the same suite
				    lists:foreach(
				      fun(TC) ->
					      ct_util:save_suite_data(
						{seq,Suite,TC},
						Seq)
				      end, TCs);
				_ ->
				    ok
			    end,
			    init_tc1(Mod,Suite,Func,HookFunc,Args);
			{failed,Seq,BadFunc} ->
                            initialize(false,Mod,Func,Args),
                            {auto_skip,{sequence_failed,Seq,BadFunc}}
		    end
	    end
    end.

init_tc1(?MODULE,_,error_in_suite,_,[Config0]) when is_list(Config0) ->
    initialize(false,?MODULE,error_in_suite),
    _ = ct_suite_init(?MODULE,error_in_suite,[],Config0),
    case ?val(error,Config0) of
	undefined ->
	    {fail,"unknown_error_in_suite"};
	Reason ->
	    {fail,Reason}
    end;

init_tc1(Mod,Suite,Func,HookFunc,[Config0]) when is_list(Config0) ->
    Config1 = 
	case ct_util:read_suite_data(last_saved_config) of
	    {{Suite,LastFunc},SavedConfig} ->	% last testcase
		[{saved_config,{LastFunc,SavedConfig}} | 
		 lists:keydelete(saved_config,1,Config0)];
	    {{LastSuite,InitOrEnd},
	     SavedConfig} when InitOrEnd == init_per_suite ;
			       InitOrEnd == end_per_suite ->
		%% last suite
		[{saved_config,{LastSuite,SavedConfig}} | 
		 lists:keydelete(saved_config,1,Config0)];
	    undefined ->
		lists:keydelete(saved_config,1,Config0)
	end,
    ct_util:delete_suite_data(last_saved_config),
    Config = lists:keydelete(watchdog,1,Config1),

    if Func == init_per_suite ->
	    %% delete all default values used in previous suite
	    ct_config:delete_default_config(suite),
	    %% release all name -> key bindings (once per suite)
	    ct_config:release_allocated();
       Func /= init_per_suite ->
	    ok
    end,

    GroupPath = ?val(tc_group_path, Config, []),
    AllGroups =	[?val(tc_group_properties, Config, []) | GroupPath],

    %% clear all config data default values set by previous
    %% testcase info function (these should only survive the
    %% testcase, not the whole suite)
    FuncSpec = group_or_func(Func,Config0),
    HookFunc1 =
	if is_tuple(FuncSpec) ->		% group
	    FuncSpec;
	   true ->
		ct_config:delete_default_config(testcase),
		HookFunc
	end,
    case add_defaults(Mod,Func,AllGroups) of
	Error = {suite0_failed,_} ->
	    initialize(false,Mod,FuncSpec),
	    ct_util:set_testdata({curr_tc,{Suite,Error}}),
	    {error,Error};
	Error = {group0_failed,_} ->
	    initialize(false,Mod,FuncSpec),
	    {auto_skip,Error};
	Error = {testcase0_failed,_} ->
	    initialize(false,Mod,FuncSpec),
	    {auto_skip,Error};
	{SuiteInfo,MergeResult} ->
	    case MergeResult of
		{error,Reason} ->
		    initialize(false,Mod,FuncSpec),
		    {fail,Reason};
		_ ->
		    init_tc2(Mod,Suite,Func,HookFunc1,
			     SuiteInfo,MergeResult,Config)
	    end
    end;

init_tc1(_Mod,_Suite,_Func,_HookFunc,Args) ->
    {ok,Args}.

init_tc2(Mod,Suite,Func,HookFunc,SuiteInfo,MergeResult,Config) ->
    %% timetrap must be handled before require
    MergedInfo = timetrap_first(MergeResult, [], []),
    %% tell logger to use specified style sheet
    _ = case lists:keysearch(stylesheet,1,MergeResult++Config) of
	{value,{stylesheet,SSFile}} ->
	    ct_logs:set_stylesheet(Func,add_data_dir(SSFile,Config));
	_ ->
	    case ct_util:get_testdata(stylesheet) of
		undefined ->
		    ct_logs:clear_stylesheet(Func);
		SSFile ->
		    ct_logs:set_stylesheet(Func,SSFile)
	    end
    end,
    %% suppress output for connections (Conns is a 
    %% list of {Type,Bool} tuples, e.g. {telnet,true}),		
    case ct_util:get_overridden_silenced_connections() of
	undefined ->
	    case lists:keysearch(silent_connections,1,MergeResult++Config) of
		{value,{silent_connections,Conns}} ->
		    ct_util:silence_connections(Conns);
		_ ->
		    ok
	    end;
	Conns ->
	    ct_util:silence_connections(Conns)
    end,
    FuncSpec = group_or_func(Func,Config),
    initialize((Func==init_per_suite),Mod,FuncSpec),

    case catch configure(MergedInfo,MergedInfo,SuiteInfo,
			 FuncSpec,[],Config) of
	{suite0_failed,Reason} ->
	    ct_util:set_testdata({curr_tc,{Mod,{suite0_failed,
						{require,Reason}}}}),
	    {auto_skip,{require_failed_in_suite0,Reason}};
	{error,Reason} ->
	    {auto_skip,{require_failed,Reason}};
	{'EXIT',Reason} ->
	    {fail,Reason};
	{ok,PostInitHook,Config1} ->
	    case get('$test_server_framework_test') of
		undefined ->
		    ct_suite_init(Suite,HookFunc,PostInitHook,Config1);
		Fun ->
		    PostInitHookResult = do_post_init_hook(PostInitHook,
							   Config1),
		    case Fun(init_tc, [PostInitHookResult ++ Config1]) of
			NewConfig when is_list(NewConfig) ->
			    {ok,NewConfig};
			Else ->
			    Else
		    end
	    end
    end.

initialize(RefreshLogs,Mod,Func,[Config]) when is_list(Config) ->
    initialize(RefreshLogs,Mod,group_or_func(Func,Config));
initialize(RefreshLogs,Mod,Func,_) ->
    initialize(RefreshLogs,Mod,Func).

initialize(RefreshLogs,Mod,FuncSpec) ->
    ct_logs:init_tc(RefreshLogs),
    ct_event:notify(#event{name=tc_start,
			   node=node(),
			   data={Mod,FuncSpec}}).


ct_suite_init(Suite,HookFunc,PostInitHook,Config) when is_list(Config) ->
    case ct_hooks:init_tc(Suite,HookFunc,Config) of
	NewConfig when is_list(NewConfig) ->
	    PostInitHookResult = do_post_init_hook(PostInitHook,NewConfig),
	    {ok, [PostInitHookResult ++ NewConfig]};
	Else ->
	    Else
    end.

do_post_init_hook(PostInitHook,Config) ->
    lists:flatmap(fun({Tag,Fun}) ->
			  case lists:keysearch(Tag,1,Config) of
			      {value,_} ->
				  [];
			      false -> 
				  case Fun() of
				      {error,_} -> [];
				      Result    -> [{Tag,Result}]
				  end
			  end
		  end, PostInitHook).

add_defaults(Mod,Func, GroupPath) ->
    Suite = get_suite_name(Mod, GroupPath),
    case (catch Suite:suite()) of
	{'EXIT',{undef,_}} ->
	    SuiteInfo = merge_with_suite_defaults(Suite,[]),
	    SuiteInfoNoCTH = [I || I <- SuiteInfo, element(1,I) =/= ct_hooks],
	    case add_defaults1(Mod,Func, GroupPath, SuiteInfoNoCTH) of
		Error = {group0_failed,_} -> Error;
		Error = {testcase0_failed,_} -> Error;
		Error = {error,_} -> {SuiteInfo,Error};
		MergedInfo -> {SuiteInfo,MergedInfo}
	    end;
	{'EXIT',Reason} ->
	    ErrStr = io_lib:format("~n*** ERROR *** "
				   "~w:suite/0 failed: ~tp~n",
				   [Suite,Reason]),
	    io:format(ErrStr, []),
	    io:format(?def_gl, ErrStr, []),
	    {suite0_failed,{exited,Reason}};
	SuiteInfo when is_list(SuiteInfo) ->
	    case lists:all(fun(E) when is_tuple(E) -> true;
			      (_) -> false
			   end, SuiteInfo) of
		true ->
		    SuiteInfo1 = merge_with_suite_defaults(Suite, SuiteInfo),
		    SuiteInfoNoCTH = [I || I <- SuiteInfo1,
					   element(1,I) =/= ct_hooks],
		    case add_defaults1(Mod,Func, GroupPath,
				       SuiteInfoNoCTH) of
			Error = {group0_failed,_} -> Error;
			Error = {testcase0_failed,_} -> Error;
			Error = {error,_} -> {SuiteInfo1,Error};
			MergedInfo -> {SuiteInfo1,MergedInfo}
		    end;
		false ->
		    ErrStr = io_lib:format("~n*** ERROR *** "
					   "Invalid return value from "
					   "~w:suite/0: ~tp~n",
					   [Suite,SuiteInfo]),
		    io:format(ErrStr, []),
		    io:format(?def_gl, ErrStr, []),
		    {suite0_failed,bad_return_value}
	    end;
	SuiteInfo ->
	    ErrStr = io_lib:format("~n*** ERROR *** "
				   "Invalid return value from "
				   "~w:suite/0: ~tp~n", [Suite,SuiteInfo]),
	    io:format(ErrStr, []),
	    io:format(?def_gl, ErrStr, []),
	    {suite0_failed,bad_return_value}
    end.

add_defaults1(Mod,Func, GroupPath, SuiteInfo) ->
    Suite = get_suite_name(Mod, GroupPath),
    %% GroupPathInfo (for subgroup on level X) =
    %%     [LevelXGroupInfo, LevelX-1GroupInfo, ..., TopLevelGroupInfo]
    GroupPathInfo =  
	lists:map(fun(GroupProps) ->
			  case ?val(name, GroupProps) of
			      undefined ->
				  [];
			      Name ->
				  case catch Suite:group(Name) of
				      GrInfo when is_list(GrInfo) -> GrInfo;
				      {'EXIT',{undef,_}} -> [];
				      BadGr0 -> {error,BadGr0,Name}
				  end
			  end
		  end, GroupPath),
    case lists:keysearch(error, 1, GroupPathInfo) of
	{value,{error,BadGr0Val,GrName}} ->
	    Gr0ErrStr = io_lib:format("~n*** ERROR *** "
				      "Invalid return value from "
				      "~w:group(~tw): ~tp~n",
				      [Mod,GrName,BadGr0Val]),
	    io:format(Gr0ErrStr, []),
	    io:format(?def_gl, Gr0ErrStr, []),
	    {group0_failed,bad_return_value};
	_ ->
	    Args = if Func == init_per_group ; Func == end_per_group ->
			   [?val(name, hd(GroupPath))];
		      true ->
			   []
		   end,
	    TestCaseInfo =
		case catch apply(Mod,Func,Args) of
		    TCInfo when is_list(TCInfo) -> TCInfo;
		    {'EXIT',{undef,_}} -> [];
		    BadTC0 -> {error,BadTC0}
		end,
	    
	    case TestCaseInfo of
		{error,BadTC0Val} ->
		    TC0ErrStr = io_lib:format("~n*** ERROR *** "
					      "Invalid return value from "
					      "~w:~tw/0: ~tp~n",
					      [Mod,Func,BadTC0Val]),
		    io:format(TC0ErrStr, []),
		    io:format(?def_gl, TC0ErrStr, []),
		    {testcase0_failed,bad_return_value};
		_ ->
		    %% let test case info (also for all config funcs) override
		    %% group info, and lower level group info override higher
		    %% level info
		    TCAndGroupInfo =
			[TestCaseInfo | remove_info_in_prev(TestCaseInfo,
							    GroupPathInfo)],
		    %% find and save require terms found in suite info
		    SuiteReqs = 
			[SDDef || SDDef <- SuiteInfo,
				  ((require == element(1,SDDef))
				   or (default_config == element(1,SDDef)))],
		    case check_for_clashes(TestCaseInfo, GroupPathInfo,
					   SuiteReqs) of
			[] ->
			    add_defaults2(Mod,Func, TCAndGroupInfo,
					  SuiteInfo,SuiteReqs);
			Clashes ->
			    {error,{config_name_already_in_use,Clashes}}
		    end
	    end
    end.

get_suite_name(?MODULE, [Cfg|_]) when is_list(Cfg), Cfg /= [] ->
    get_suite_name(?MODULE, Cfg);

get_suite_name(?MODULE, Cfg) when is_list(Cfg), Cfg /= [] ->
    case ?val(tc_group_properties, Cfg) of
	undefined ->
	    case ?val(suite, Cfg) of
		undefined -> ?MODULE;
		Suite -> Suite
	    end;
	GrProps ->
	    case ?val(suite, GrProps) of
		undefined -> ?MODULE;
		Suite -> Suite
	    end
    end;
get_suite_name(Mod, _) ->
    Mod.

%% Check that alias names are not already in use
check_for_clashes(TCInfo, [CurrGrInfo|Path], SuiteInfo) ->
    ReqNames = fun(Info) -> [element(2,R) || R <- Info,
					     size(R) == 3,
					     require == element(1,R)]
	       end,
    ExistingNames = lists:flatten([ReqNames(L)  || L <- [SuiteInfo|Path]]),
    CurrGrReqNs = ReqNames(CurrGrInfo),
    GrClashes = [Name || Name <- CurrGrReqNs,
			 true == lists:member(Name, ExistingNames)],
    AllReqNs = CurrGrReqNs ++ ExistingNames,
    TCClashes = [Name || Name <- ReqNames(TCInfo),
			 true == lists:member(Name, AllReqNs)],
    TCClashes ++ GrClashes.

%% Delete the info terms in Terms from all following info lists
remove_info_in_prev(Terms, [[] | Rest]) ->
    [[] | remove_info_in_prev(Terms, Rest)];
remove_info_in_prev(Terms, [Info | Rest]) ->
    UniqueInInfo = [U || U <- Info,
			  ((timetrap == element(1,U)) and
			   (not lists:keymember(timetrap,1,Terms))) or 
			  ((require == element(1,U)) and
			   (not lists:member(U,Terms))) or
			  ((default_config == element(1,U)) and
                           (not keysmember([default_config,1,
					    element(2,U),2], Terms)))],
    OtherTermsInInfo = [T || T <- Info,
			     timetrap /= element(1,T),
			     require /= element(1,T),
			     default_config /= element(1,T),
			     false == lists:keymember(element(1,T),1,
						      Terms)],
    KeptInfo = UniqueInInfo ++ OtherTermsInInfo,
    [KeptInfo | remove_info_in_prev(Terms ++ KeptInfo, Rest)];
remove_info_in_prev(_, []) ->
    [].

keysmember([Key,Pos|Next], List) ->
    case [Elem || Elem <- List, Key == element(Pos,Elem)] of
	[]    -> false;
	Found -> keysmember(Next, Found)
    end;
keysmember([], _) -> true.


add_defaults2(_Mod,init_per_suite, IPSInfo, SuiteInfo,SuiteReqs) ->
    Info = lists:flatten([IPSInfo, SuiteReqs]),
    lists:flatten([Info,remove_info_in_prev(Info, [SuiteInfo])]);

add_defaults2(_Mod,init_per_group, IPGAndGroupInfo, SuiteInfo,SuiteReqs) ->
    SuiteInfo1 =
	remove_info_in_prev(lists:flatten([IPGAndGroupInfo,
					   SuiteReqs]), [SuiteInfo]),
    %% don't require terms in prev groups (already processed)
    case IPGAndGroupInfo of
	[IPGInfo] ->
	    lists:flatten([IPGInfo,SuiteInfo1]);
	[IPGInfo | [CurrGroupInfo | PrevGroupInfo]] ->
	    PrevGroupInfo1 = delete_require_terms(PrevGroupInfo),
	    lists:flatten([IPGInfo,CurrGroupInfo,PrevGroupInfo1,
			   SuiteInfo1])
    end;

add_defaults2(_Mod,_Func, TCAndGroupInfo, SuiteInfo,SuiteReqs) ->
    %% Include require elements from test case info and current group,
    %% but not from previous groups or suite/0 (since we've already required
    %% those vars). Let test case info elements override group and suite
    %% info elements.
    SuiteInfo1 = remove_info_in_prev(lists:flatten([TCAndGroupInfo,
						    SuiteReqs]), [SuiteInfo]),
    %% don't require terms in prev groups (already processed)
    case TCAndGroupInfo of
	[TCInfo] ->
	    lists:flatten([TCInfo,SuiteInfo1]);
	[TCInfo | [CurrGroupInfo | PrevGroupInfo]] ->
	    PrevGroupInfo1 = delete_require_terms(PrevGroupInfo),
	    lists:flatten([TCInfo,CurrGroupInfo,PrevGroupInfo1,
			   SuiteInfo1])
    end.

delete_require_terms([Info | Prev]) ->
    Info1 = [T || T <- Info, 
		  require /= element(1,T),
		  default_config /= element(1,T)],
    [Info1 | delete_require_terms(Prev)];
delete_require_terms([]) ->
    [].

merge_with_suite_defaults(Mod,SuiteInfo) ->
    case lists:keysearch(suite_defaults,1,Mod:module_info(attributes)) of
	{value,{suite_defaults,Defaults}} ->
	    SDReqs =
		[SDDef || SDDef <- Defaults,
			  require == element(1,SDDef),
			  false == lists:keymember(element(2,SDDef),2,
						   SuiteInfo)],
	    SuiteInfo ++ SDReqs ++
		[SDDef || SDDef <- Defaults,
			  require /= element(1,SDDef),
			  false == lists:keymember(element(1,SDDef),1,
						   SuiteInfo)];		    
	false ->
	    SuiteInfo
    end.

timetrap_first([Trap = {timetrap,_} | Rest],Info,Found) ->
    timetrap_first(Rest,Info,[Trap | Found]);
timetrap_first([Other | Rest],Info,Found) ->
    timetrap_first(Rest,[Other | Info],Found);
timetrap_first([],Info,[]) ->
    [{timetrap,{minutes,30}} | ?rev(Info)];
timetrap_first([],Info,Found) ->
    ?rev(Found) ++ ?rev(Info).

configure([{require,Required}|Rest],
	  Info,SuiteInfo,Scope,PostInitHook,Config) ->
    case ct:require(Required) of
	ok ->
	    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,Config);
	Error = {error,Reason} ->
	    case required_default('_UNDEF',Required,Info,
				  SuiteInfo,Scope) of
		ok ->
		    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,Config);
		_ ->
		    case lists:keymember(Required,2,SuiteInfo) of
			true ->
			    {suite0_failed,Reason};
			false ->
			    Error
		    end
	    end
    end;
configure([{require,Name,Required}|Rest],
	  Info,SuiteInfo,Scope,PostInitHook,Config) ->
    case ct:require(Name,Required) of
	ok ->
	    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,Config);
	Error = {error,Reason} ->
	    case required_default(Name,Required,Info,SuiteInfo,Scope) of
		ok ->
		    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,Config);
		_ ->
		    case lists:keymember(Name,2,SuiteInfo) of
			true -> 
			    {suite0_failed,Reason};
			false ->
			    Error
		    end
	    end
    end;
configure([{timetrap,off}|Rest],Info,SuiteInfo,Scope,PostInitHook,Config) ->
    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,Config);
configure([{timetrap,Time}|Rest],Info,SuiteInfo,Scope,PostInitHook,Config) ->
    PostInitHook1 = 
	[{watchdog,fun() -> case test_server:get_timetrap_info() of
				undefined ->
				    test_server:timetrap(Time);
				_ ->
				    {error,already_set}
			    end
		   end} | PostInitHook],
    configure(Rest,Info,SuiteInfo,Scope,PostInitHook1,Config);
configure([{ct_hooks,Hook}|Rest],Info,SuiteInfo,Scope,PostInitHook,Config) ->
    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,[{ct_hooks,Hook}|Config]);
configure([_|Rest],Info,SuiteInfo,Scope,PostInitHook,Config) ->
    configure(Rest,Info,SuiteInfo,Scope,PostInitHook,Config);
configure([],_,_,_,PostInitHook,Config) ->
    {ok,PostInitHook,Config}.

%% the require element in Info may come from suite/0 and
%% should be scoped 'suite', or come from the group info
%% function and be scoped 'group', or come from the testcase
%% info function and then be scoped 'testcase'

required_default(Name,Key,Info,_,init_per_suite) ->
    try_set_default(Name,Key,Info,suite);
required_default(Name,Key,Info,_,{init_per_group,GrName,_}) ->
    try_set_default(Name,Key,Info,{group,GrName});
required_default(Name,Key,Info,_,_FuncSpec) ->
    try_set_default(Name,Key,Info,testcase).

try_set_default(Name,Key,Info,Where) ->
    CfgElems = 
	case lists:keysearch(Name,1,Info) of
	    {value,{Name,Val}} ->
		[Val];
	    false ->
		case catch [{Key,element(3,Elem)} || Elem <- Info,
						     element(1,Elem)==default_config,
						     element(2,Elem)==Key] of
		    {'EXIT',_} -> [];
		    Result -> Result
		end
	end,
    case {Name,CfgElems} of
	{_,[]} -> 
	    no_default;
	{'_UNDEF',_} ->
	    _ = [ct_config:set_default_config([CfgVal],Where) || CfgVal <- CfgElems],
	    ok;
	_ ->
	    _ = [ct_config:set_default_config(Name,[CfgVal],Where) || CfgVal <- CfgElems],
	    ok
    end.
	    

%%%-----------------------------------------------------------------
%%% @spec end_tc(Mod,Func,Args) -> {ok,NewArgs}| {error,Reason} |
%%%         {skip,Reason} | {auto_skip,Reason}
%%%      Mod = atom()
%%%      Func = atom()
%%%      Args = list()
%%%      NewArgs = list()
%%%      Reason = term()
%%%
%%% @doc Test server framework callback, called by the test_server
%%% when a test case is finished.
end_tc(Mod, Fun, Args) ->
    %% Have to keep end_tc/3 for backwards compatibility issues
    end_tc(Mod, Fun, Args, '$end_tc_dummy').
end_tc(?MODULE,error_in_suite,{Result,[Args]},Return) ->
    %% this clause gets called if CT has encountered a suite that
    %% can't be executed
    FinalNotify =
	case ct_hooks:end_tc(?MODULE, error_in_suite, Args, Result, Return) of
	    '$ct_no_change' ->
		Result;
	    HookResult ->
		HookResult
	end,
    Event = #event{name=tc_done,
		   node=node(),
		   data={?MODULE,error_in_suite,tag(FinalNotify)}},
    ct_event:sync_notify(Event),
    ok;
end_tc(Mod,Func,{TCPid,Result,[Args]}, Return) when is_pid(TCPid) ->
    end_tc(Mod,Func,TCPid,Result,Args,Return);
end_tc(Mod,Func,{Result,[Args]}, Return) ->
    end_tc(Mod,Func,self(),Result,Args,Return).

end_tc(Mod,IPTC={init_per_testcase,_Func},_TCPid,Result,Args,Return) ->
    case end_hook_func(IPTC,Return,IPTC) of
        undefined -> ok;
        _ ->
            %% in case Mod == ct_framework, lookup the suite name
            Suite = get_suite_name(Mod, Args),
            case ct_hooks:end_tc(Suite,IPTC,Args,Result,Return) of
                '$ct_no_change' ->
                    ok;
                HookResult ->
                    HookResult
            end
    end;

end_tc(Mod,Func0,TCPid,Result,Args,Return) ->
    %% in case Mod == ct_framework, lookup the suite name
    Suite = get_suite_name(Mod, Args),
    {Func,FuncSpec,HookFunc} =
        case Func0 of
            {end_per_testcase_not_run,F} ->
                %% Testcase is completed (skipped or failed), but
                %% end_per_testcase is not run - don't call post-hook.
                {F,F,undefined};
            {end_per_testcase,F} ->
                {F,F,Func0};
            _ ->
                FS = group_or_func(Func0,Args),
                HF = end_hook_func(Func0,Return,FS),
                {Func0,FS,HF}
        end,

    test_server:timetrap_cancel(),

    %% save the testcase process pid so that it can be used
    %% to look up the attached trace window later
    case ct_util:get_testdata(interpret) of
	{What,kill,_} ->
	    AttPid = ct_util:get_attached(self()),
	    ct_util:set_testdata({interpret,{What,kill,{self(),AttPid}}});
	_ ->
	    ok
    end,
    if Func == end_per_group; Func == end_per_suite ->
	    %% clean up any saved comments
	    ct_util:match_delete_testdata({comment,'_'});
       true ->
	    %% attemp to delete any saved comment for this TC
	    case process_info(TCPid, group_leader) of
		{group_leader,TCGL} ->
		    ct_util:delete_testdata({comment,TCGL});
		_ ->
		    ok
	    end
    end,
    ct_util:delete_suite_data(last_saved_config),

    {Result1,FinalNotify} =
        case HookFunc of
            undefined ->
                {ok,Result};
            _ ->
                case ct_hooks:end_tc(Suite,HookFunc,Args,Result,Return) of
                    '$ct_no_change' ->
                        {ok,Result};
                    HookResult ->
                        {HookResult,HookResult}
                end
        end,
    FinalResult =
	case get('$test_server_framework_test') of
	    undefined ->
		%% send sync notification so that event handlers may print
		%% in the log file before it gets closed
		Event = #event{name=tc_done,
			       node=node(),
			       data={Mod,FuncSpec,
				     tag(FinalNotify)}},
		ct_event:sync_notify(Event),
		Result1;
	    Fun ->
		%% send sync notification so that event handlers may print
		%% in the log file before it gets closed
		Event = #event{name=tc_done,
			       node=node(),
			       data={Mod,FuncSpec,
				     tag({'$test_server_framework_test',
					  FinalNotify})}},
		ct_event:sync_notify(Event),
		Fun(end_tc, Return)
	end,    

    case FuncSpec of
	{_,GroupName,_Props} ->
	    if Func == end_per_group ->
		    ct_config:delete_default_config({group,GroupName});
	       true -> ok
	    end,
	    case lists:keysearch(save_config,1,Args) of
		{value,{save_config,SaveConfig}} ->
		    ct_util:save_suite_data(last_saved_config,
					    {Suite,{group,GroupName}},
					    SaveConfig);
		false ->
		    ok
	    end;
	_ ->
	    case lists:keysearch(save_config,1,Args) of
		{value,{save_config,SaveConfig}} ->
		    ct_util:save_suite_data(last_saved_config,
					    {Suite,Func},SaveConfig);
		false ->
		    ok
	    end
    end,

    ct_util:reset_silent_connections(),

    %% reset the curr_tc state, or delete this TC from the list of
    %% executing cases (if in a parallel group)
    ClearCurrTC = fun(Running = [_,_|_]) ->
			  lists:keydelete(Func,2,Running);
		     ({_,{suite0_failed,_}}) ->
			  undefined;
		     ([{_,CurrTC}]) when CurrTC == Func ->
			  undefined;
		     (undefined) ->
			  undefined;
		     (Unexpected) ->
			  {error,{reset_curr_tc,{Mod,Func},Unexpected}}
		  end,
    case ct_util:update_testdata(curr_tc, ClearCurrTC) of
	{error,_} = ClearError ->
	    exit(ClearError);
	_ ->
	    ok
    end,

    case FinalResult of
	{auto_skip,{sequence_failed,_,_}} ->
	    %% ct_logs:init_tc is never called for a skipped test case
	    %% in a failing sequence, so neither should end_tc	    
	    ok;
	_ ->
	    case ct_logs:end_tc(TCPid) of
		{error,Reason} ->
		    exit({error,{logger,Reason}});
		_ ->
		    ok
	    end
    end,
    case Func of
	end_per_suite -> 
	    ct_util:match_delete_suite_data({seq,Suite,'_'});
	_ -> 
	    ok
    end,
    FinalResult.	    

%% This is to make sure that no post_init_per_* is ever called if the
%% corresponding pre_init_per_* was not called.
%% The skip or fail reasons are those that can be returned from
%% init_tc above in situations where we never came to call
%% ct_hooks:init_tc/3, e.g. if suite/0 fails, then we never call
%% ct_hooks:init_tc for init_per_suite, and thus we must not call
%% ct_hooks:end_tc for init_per_suite either.
end_hook_func({init_per_testcase,_},{auto_skip,{sequence_failed,_,_}},_) ->
    undefined;
end_hook_func({init_per_testcase,_},{auto_skip,"Repeated test stopped by force_stop option"},_) ->
    undefined;
end_hook_func({init_per_testcase,_},{fail,{config_name_already_in_use,_}},_) ->
    undefined;
end_hook_func({init_per_testcase,_},{auto_skip,{InfoFuncError,_}},_)
  when InfoFuncError==testcase0_failed;
       InfoFuncError==require_failed ->
    undefined;
end_hook_func(init_per_group,{auto_skip,{InfoFuncError,_}},_)
  when InfoFuncError==group0_failed;
       InfoFuncError==require_failed ->
    undefined;
end_hook_func(init_per_suite,{auto_skip,{require_failed_in_suite0,_}},_) ->
    undefined;
end_hook_func(init_per_suite,{auto_skip,{failed,{error,{suite0_failed,_}}}},_) ->
    undefined;
end_hook_func(_,_,Default) ->
    Default.

%% {error,Reason} | {skip,Reason} | {timetrap_timeout,TVal} | 
%% {testcase_aborted,Reason} | testcase_aborted_or_killed | 
%% {'EXIT',Reason} | {fail,Reason} | {failed,Reason} |
%% {user_timetrap_error,Reason} |
%% Other (ignored return value, e.g. 'ok')
tag({'$test_server_framework_test',Result}) ->
    case tag(Result) of
	ok      -> Result;
	Failure -> Failure
    end;	    
tag({skipped,Reason={failed,{_,init_per_testcase,_}}}) ->
    {auto_skipped,Reason};
tag({STag,Reason}) when STag == skip; STag == skipped -> 
    case Reason of
	{failed,{_,init_per_testcase,_}} -> {auto_skipped,Reason};
	_ -> {skipped,Reason}
    end;
tag({auto_skip,Reason}) ->
    {auto_skipped,Reason};
tag({fail,Reason}) ->
    {failed,{error,Reason}};
tag(Failed = {failed,_Reason}) ->
    Failed;
tag(E = {ETag,_}) when ETag == error; ETag == 'EXIT'; 
			   ETag == timetrap_timeout;
			   ETag == testcase_aborted -> 
    {failed,E};
tag(E = testcase_aborted_or_killed) ->
    {failed,E};
tag(UserTimetrap = {user_timetrap_error,_Reason}) ->
    UserTimetrap;
tag(_Other) ->
    ok.

%%%-----------------------------------------------------------------
%%% @spec error_notification(Mod,Func,Args,Error) -> ok
%%%      Mod = atom()
%%%      Func = atom()
%%%      Args = list()
%%%      Error = term()
%%%
%%% @doc This function is called as the result of testcase 
%%% <code>Func</code> in suite <code>Mod</code> crashing. 
%%% <code>Error</code> specifies the reason for failing.
error_notification(Mod,Func,_Args,{Error,Loc}) ->
    ErrorSpec = case Error of
		 {What={_E,_R},Trace} when is_list(Trace) ->
		      What;
		  What ->
		      What
	      end,
    ErrorStr = case ErrorSpec of
		 {badmatch,Descr} ->
                     Descr1 = io_lib:format("~tP",[Descr,10]),
                     DescrLength = string:length(Descr1),
                     if DescrLength > 50 ->
			     Descr2 = string:slice(Descr1,0,50),
			     io_lib:format("{badmatch,~ts...}",[Descr2]);
			true ->
			     io_lib:format("{badmatch,~ts}",[Descr1])
		     end;
		 {test_case_failed,Reason} ->
		     case (catch io_lib:format("{test_case_failed,~ts}", [Reason])) of
			 {'EXIT',_} ->
			     io_lib:format("{test_case_failed,~tp}", [Reason]);
			 Result -> Result
		     end;
		 {'EXIT',_Reason} = EXIT ->
		     io_lib:format("~tP", [EXIT,5]);
		 {Spec,_Reason} when is_atom(Spec) ->
		     io_lib:format("~tw", [Spec]);
		 Other ->
		     io_lib:format("~tP", [Other,5])
	     end,
    ErrorHtml =
	"<font color=\"brown\">" ++ ct_logs:escape_chars(ErrorStr) ++ "</font>",
    case {Mod,Error} of
	%% some notifications come from the main test_server process
	%% and for these cases the existing comment may not be modified
	{_,{timetrap_timeout,_TVal}} ->
	    ok;
	{_,{testcase_aborted,_Info}} ->
	    ok;
	{_,testcase_aborted_or_killed} ->
	    ok;
	{undefined,_OtherError} ->
	    ok;
	_ ->			     
	    %% this notification comes from the test case process, so
	    %% we can add error info to comment with test_server:comment/1
	    case ct_util:get_testdata({comment,group_leader()}) of
		undefined ->
		    test_server:comment(ErrorHtml);
		Comment ->
		    CommentHtml = 
			"<font color=\"green\">" ++ "(" ++ "</font>"
			++ Comment ++ 
			"<font color=\"green\">" ++ ")" ++ "</font>",
		    Str = io_lib:format("~ts   ~ts", [ErrorHtml,CommentHtml]),
		    test_server:comment(Str)
	    end
    end,

    PrintError = fun(ErrorFormat, ErrorArgs) ->
		       Div = "~n- - - - - - - - - - - - - - - - - - - "
			     "- - - - - - - - - - - - - - - - - - - - -~n",
		       ErrorStr2 = io_lib:format(ErrorFormat, ErrorArgs),
		       io:format(?def_gl, lists:concat([Div,ErrorStr2,Div,"~n"]),
				 []),
		       Link =
			   "\n\n<a href=\"#end\">"
			   "Full error description and stacktrace"
			   "</a>",
		       ErrorHtml2 = ct_logs:escape_chars(ErrorStr2),
		       ct_logs:tc_log(ct_error_notify,
				      ?MAX_IMPORTANCE,
				      "CT Error Notification",
				      ErrorHtml2++Link, [], [])
	       end,
    case Loc of
	[{?MODULE,error_in_suite}] ->
	    PrintError("Error in suite detected: ~ts", [ErrorStr]);

	R when R == unknown; R == undefined ->
	    PrintError("Error detected: ~ts", [ErrorStr]);

	%% if a function specified by all/0 does not exist, we
	%% pick up undef here
	[{LastMod,LastFunc}|_] when ErrorStr == "undef" ->
	    PrintError("~w:~tw could not be executed~nReason: ~ts",
		     [LastMod,LastFunc,ErrorStr]);

	[{LastMod,LastFunc}|_] ->
	    PrintError("~w:~tw failed~nReason: ~ts", [LastMod,LastFunc,ErrorStr]);
	    
	[{LastMod,LastFunc,LastLine}|_] ->
	    %% print error to console, we are only
	    %% interested in the last executed expression
	    PrintError("~w:~tw failed on line ~w~nReason: ~ts",
		     [LastMod,LastFunc,LastLine,ErrorStr]),
	    
	    case ct_util:read_suite_data({seq,Mod,Func}) of
		undefined ->
		    ok;
		Seq ->
		    SeqTCs = ct_util:read_suite_data({seq,Mod,Seq}),
		    mark_as_failed(Seq,Mod,Func,SeqTCs)
	    end	    
    end,
    ok.

%% cases in seq that have already run
mark_as_failed(Seq,Mod,Func,[Func|TCs]) ->
    mark_as_failed1(Seq,Mod,Func,TCs);
mark_as_failed(Seq,Mod,Func,[_TC|TCs]) ->
    mark_as_failed(Seq,Mod,Func,TCs);
mark_as_failed(_,_,_,[]) ->
    ok;
mark_as_failed(_,_,_,undefined) ->
    ok.

%% mark rest of cases in seq to be skipped
mark_as_failed1(Seq,Mod,Func,[TC|TCs]) ->
    ct_util:save_suite_data({seq,Mod,TC},{failed,Seq,Func}),
    mark_as_failed1(Seq,Mod,Func,TCs);
mark_as_failed1(_,_,_,[]) ->
    ok.

group_or_func(Func, Config) when Func == init_per_group; 
				 Func == end_per_group ->
    case ?val(tc_group_properties, Config) of
	undefined ->
	    {Func,unknown,[]};
	GrProps ->
	    GrName = ?val(name,GrProps),
	    {Func,GrName,proplists:delete(name,GrProps)}
    end;	  
group_or_func(Func, _Config) ->
    Func.

%%%-----------------------------------------------------------------
%%% @spec get_suite(Mod, Func) -> Tests
%%%
%%% @doc Called from test_server for every suite (<code>Func==all</code>)
%%%      and every test case. If the former, all test cases in the suite
%%%      should be returned. 

get_suite(Mod, all) ->
    case catch apply(Mod, groups, []) of
	{'EXIT',_} ->
	    get_all(Mod, []);
	GroupDefs when is_list(GroupDefs) ->
	    case catch ct_groups:find_groups(Mod, all, all, GroupDefs) of
		{error,_} = Error ->
		    %% this makes test_server call error_in_suite as first
		    %% (and only) test case so we can report Error properly
		    [{?MODULE,error_in_suite,[[Error]]}];
		ConfTests ->
		    get_all(Mod, ConfTests)
	    end;
	_ ->
	    E = "Bad return value from "++atom_to_list(Mod)++":groups/0",
	    [{?MODULE,error_in_suite,[[{error,list_to_atom(E)}]]}]
    end;

%%!============================================================
%%! Note: The handling of sequences in get_suite/2 and get_all/2
%%! is deprecated and should be removed at some point...
%%!============================================================

%% group
get_suite(Mod, Group={conf,Props,_Init,TCs,_End}) ->
    Name = ?val(name, Props),
    case catch apply(Mod, groups, []) of
	{'EXIT',_} ->
	    [Group];
	GroupDefs when is_list(GroupDefs) ->
	    case catch ct_groups:find_groups(Mod, Name, TCs, GroupDefs) of
		{error,_} = Error ->
		    %% this makes test_server call error_in_suite as first
		    %% (and only) test case so we can report Error properly
		    [{?MODULE,error_in_suite,[[Error]]}];
		[] ->
		    [];
		ConfTests ->
		    case lists:member(skipped, Props) of
			true ->
			    %% a *subgroup* specified *only* as skipped (and not
			    %% as an explicit test) should not be returned, or
			    %% init/end functions for top groups will be executed
			    case catch ?val(name, element(2, hd(ConfTests))) of
				Name ->		% top group
				    ct_groups:delete_subs(ConfTests, ConfTests);
				_ ->
				    []
			    end;
			false ->
			    ConfTests1 = ct_groups:delete_subs(ConfTests,
							       ConfTests),
			    case ?val(override, Props) of
				undefined ->
				    ConfTests1;
				[] ->
				    ConfTests1;
				ORSpec ->
				    ORSpec1 = if is_tuple(ORSpec) -> [ORSpec];
						 true -> ORSpec end,
				    ct_groups:search_and_override(ConfTests1,
								  ORSpec1, Mod)
			    end
		    end
	    end;
	_ ->
	    E = "Bad return value from "++atom_to_list(Mod)++":groups/0",
	    [{?MODULE,error_in_suite,[[{error,list_to_atom(E)}]]}]
    end;

%% testcase
get_suite(Mod, Name) ->
     get_seq(Mod, Name).

%%%-----------------------------------------------------------------

get_all_cases(Suite) ->
    case get_suite(Suite, all) of
	[{?MODULE,error_in_suite,[[{error,_}=Error]]}] ->
		Error;
	[{?MODULE,error_in_suite,[[Error]]}] ->
	    {error,Error};
	Tests ->
	    Cases = get_all_cases1(Suite, Tests),
	    ?rev(lists:foldl(fun(TC, TCs) ->
				     case lists:member(TC, TCs) of
				      true  -> TCs;
					 false -> [TC | TCs]
				     end
			     end, [], Cases))
    end.

get_all_cases1(Suite, [{conf,_Props,_Init,GrTests,_End} | Tests]) ->
    get_all_cases1(Suite, GrTests) ++ get_all_cases1(Suite, Tests);

get_all_cases1(Suite, [Test | Tests]) when is_atom(Test) ->
    [{Suite,Test} | get_all_cases1(Suite, Tests)];

get_all_cases1(Suite, [Test | Tests]) ->
    [Test | get_all_cases1(Suite, Tests)];

get_all_cases1(_, []) ->
    [].

%%%-----------------------------------------------------------------

get_all(Mod, ConfTests) ->	
    case catch apply(Mod, all, []) of
	{'EXIT',{undef,[{Mod,all,[],_} | _]}} ->
	    Reason =
		case code:which(Mod) of
		    non_existing ->
			list_to_atom(atom_to_list(Mod)++
					 " can not be compiled or loaded");
		    _ ->
			list_to_atom(atom_to_list(Mod)++":all/0 is missing")
		end,
	    %% this makes test_server call error_in_suite as first
	    %% (and only) test case so we can report Reason properly
	    [{?MODULE,error_in_suite,[[{error,Reason}]]}];
	{'EXIT',ExitReason} ->
	    case ct_util:get_testdata({error_in_suite,Mod}) of
		undefined ->
		    ErrStr = io_lib:format("~n*** ERROR *** "
					   "~w:all/0 failed: ~tp~n",
					   [Mod,ExitReason]),
		    io:format(?def_gl, ErrStr, []),
		    %% save the error info so it doesn't get printed twice
		    ct_util:set_testdata_async({{error_in_suite,Mod},
						ExitReason});
		_ExitReason ->
		    ct_util:delete_testdata({error_in_suite,Mod})
	    end,
	    Reason = list_to_atom(atom_to_list(Mod)++":all/0 failed"),
	    %% this makes test_server call error_in_suite as first
	    %% (and only) test case so we can report Reason properly
	    [{?MODULE,error_in_suite,[[{error,Reason}]]}];
	AllTCs when is_list(AllTCs) ->
	    case catch save_seqs(Mod,AllTCs) of
		{error,What} ->
		    [{?MODULE,error_in_suite,[[{error,What}]]}];
		SeqsAndTCs ->
		    %% expand group references in all() using ConfTests
		    case catch ct_groups:expand_groups(SeqsAndTCs,
						       ConfTests,
						       Mod) of
			{error,_} = Error ->
			    [{?MODULE,error_in_suite,[[Error]]}];
			Tests ->
			    ct_groups:delete_subs(Tests, Tests)
		    end
	    end;
	Skip = {skip,_Reason} ->
	    Skip;
	_ ->
	    Reason = 
		list_to_atom("Bad return value from "++
				 atom_to_list(Mod)++":all/0"),
	    [{?MODULE,error_in_suite,[[{error,Reason}]]}]
    end.

%%!============================================================
%%! The support for sequences by means of using sequences/0
%%! will be removed in OTP R15. The code below is only kept 
%%! for backwards compatibility. From OTP R13 groups with
%%! sequence property should be used instead!
%%!============================================================
%%!============================================================
%%! START OF DEPRECATED SUPPORT FOR SEQUENCES --->

get_seq(Mod, Func) ->
    case ct_util:read_suite_data({seq,Mod,Func}) of
	undefined ->
	    case catch apply(Mod,sequences,[]) of
		{'EXIT',_} ->
		    [];
		Seqs ->
		    case lists:keysearch(Func,1,Seqs) of
			{value,{Func,SeqTCs}} ->			    
			    case catch save_seq(Mod,Func,SeqTCs) of
				{error,What} ->
				    [{?MODULE,error_in_suite,[[{error,What}]]}];
				_ ->
				    SeqTCs
			    end;
			false ->
			    []
		    end
	    end;
	TCs when is_list(TCs) ->
	    TCs;
	_ ->
	    []
    end.

save_seqs(Mod,AllTCs) ->
    case lists:keymember(sequence,1,AllTCs) of
	true ->
	    case catch apply(Mod,sequences,[]) of
		{'EXIT',_} -> 
		    Reason = list_to_atom(atom_to_list(Mod)++
					  ":sequences/0 is missing"),
		    throw({error,Reason});
		Seqs ->
		    save_seqs(Mod,AllTCs,Seqs,AllTCs)
	    end;
	false ->
	    AllTCs
    end.
    
save_seqs(Mod,[{sequence,Seq}|TCs],Seqs,All) ->
    case lists:keysearch(Seq,1,Seqs) of
	{value,{Seq,SeqTCs}} ->
	    save_seq(Mod,Seq,SeqTCs,All),
	    [Seq|save_seqs(Mod,TCs,Seqs,All)];
	false ->
	    Reason = list_to_atom(
		       atom_to_list(Seq)++" is missing in "++
		       atom_to_list(Mod)),
	    throw({error,Reason})
    end;
save_seqs(Mod,[TC|TCs],Seqs,All) ->
    [TC|save_seqs(Mod,TCs,Seqs,All)];
save_seqs(_,[],_,_) ->
    [].

save_seq(Mod,Seq,SeqTCs) ->
    save_seq(Mod,Seq,SeqTCs,apply(Mod,all,[])).
    
save_seq(Mod,Seq,SeqTCs,All) ->
    check_private(Seq,SeqTCs,All),
    check_multiple(Mod,Seq,SeqTCs),
    ct_util:save_suite_data({seq,Mod,Seq},SeqTCs),
    lists:foreach(fun(TC) -> 
			  ct_util:save_suite_data({seq,Mod,TC},Seq)
		  end, SeqTCs).

check_private(Seq,TCs,All) ->    
    Bad = lists:filter(fun(TC) -> lists:member(TC,All) end, TCs),
    if Bad /= [] ->
	    Reason = io_lib:format("regular test cases not allowed in sequence ~tp: "
				   "~tp",[Seq,Bad]),
	    throw({error,list_to_atom(lists:flatten(Reason))});
       true ->
	    ok
    end.

check_multiple(Mod,Seq,TCs) ->
    Bad = lists:filter(fun(TC) ->
			       case ct_util:read_suite_data({seq,Mod,TC}) of
				   Seq1 when Seq1 /= undefined, Seq1 /= Seq -> 
				       true;

				   _ -> false
			       end
		       end,TCs),
    if Bad /= [] ->
	    Reason = io_lib:format("test cases found in multiple sequences: "
				   "~tp",[Bad]),
	    throw({error,list_to_atom(lists:flatten(Reason))});
       true ->
	    ok
    end.

%%! <---  END OF DEPRECATED SUPPORT FOR SEQUENCES
%%!============================================================

%% let test_server call this function as a testcase only so that
%% the user may see info about what's missing in the suite
error_in_suite(Config) ->
    Reason = test_server:lookup_config(error,Config),
    exit(Reason).

%% if init_per_suite and end_per_suite are missing in the suite,
%% these will be called instead (without any trace of them in the
%% log files), only so that it's possible to call hook functions
%% for configuration
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% if the group config functions are missing in the suite,
%% use these instead
init_per_group(GroupName, Config) ->
    ct:comment(io_lib:format("start of ~tp", [GroupName])),
    ct_logs:log("TEST INFO", "init_per_group/2 for ~tw missing "
		"in suite, using default.",
		[GroupName]),
    Config.

end_per_group(GroupName, _) ->
    ct:comment(io_lib:format("end of ~tp", [GroupName])),
    ct_logs:log("TEST INFO", "end_per_group/2 for ~tw missing "
		"in suite, using default.",
		[GroupName]),
    ok.
    
%%%-----------------------------------------------------------------
%%% @spec report(What,Data) -> ok
report(What,Data) ->
    case What of
	loginfo ->
	    %% logfiles and direcories have been created for a test and the
	    %% top level test index page needs to be refreshed
	    TestName = filename:basename(?val(topdir, Data), ".logs"),
	    RunDir = ?val(rundir, Data),
	    _ = ct_logs:make_all_suites_index({TestName,RunDir}),
	    ok;
	tests_start ->
	    ok;
	tests_done ->
	    ok;
	severe_error ->
	    ct_event:sync_notify(#event{name=What,
					node=node(),
					data=Data}),
	    ct_util:set_testdata({What,Data}),
	    ok;
	tc_start ->
	    %% Data = {{Suite,{Func,GroupName}},LogFileName}
	    Data1 = case Data of
			{{Suite,{Func,undefined}},LFN} -> {{Suite,Func},LFN};
			_ -> Data
		    end,
	    ct_event:sync_notify(#event{name=tc_logfile,
					node=node(),
					data=Data1}),
	    ok;
	tc_done ->
	    {Suite,{Func,GrName},Result} = Data,
            FuncSpec = if GrName == undefined -> Func;
                          true                -> {Func,GrName}
                       end,
	    %% Register the group leader for the process calling the report
	    %% function, making it possible for a hook function to print
	    %% in the test case log file
	    ReportingPid = self(),
	    ct_logs:register_groupleader(ReportingPid, group_leader()),
	    case Result of
		{failed, Reason} ->
		    ct_hooks:on_tc_fail(What, {Suite,FuncSpec,Reason});
		{skipped,{failed,{_,init_per_testcase,_}}=Reason} ->
		    ct_hooks:on_tc_skip(tc_auto_skip,  {Suite,FuncSpec,Reason});
		{skipped,{require_failed,_}=Reason} ->
		    ct_hooks:on_tc_skip(tc_auto_skip, {Suite,FuncSpec,Reason});
		{skipped,Reason} ->
		    ct_hooks:on_tc_skip(tc_user_skip, {Suite,FuncSpec,Reason});
		{auto_skipped,Reason} ->
		    ct_hooks:on_tc_skip(tc_auto_skip, {Suite,FuncSpec,Reason});
		_Else ->
		    ok
	    end,
	    ct_logs:unregister_groupleader(ReportingPid),
	    case {Func,Result} of
		{error_in_suite,_} when Suite == ?MODULE ->
		    ok;
		{init_per_suite,_} ->
		    ok;
		{end_per_suite,_} ->
		    ok;
		{init_per_group,_} ->
		    ok;
		{end_per_group,_} ->
		    ok;
		{_,ok} ->
		    add_to_stats(ok);
		{_,{skipped,{failed,{_,init_per_testcase,_}}}} ->
		    add_to_stats(auto_skipped);
		{_,{skipped,{require_failed,_}}} ->
		    add_to_stats(auto_skipped);
		{_,{skipped,{timetrap_error,_}}} ->
		    add_to_stats(auto_skipped);
		{_,{skipped,{invalid_time_format,_}}} ->
		    add_to_stats(auto_skipped);
		{_,{skipped,_}} ->
		    add_to_stats(user_skipped);
		{_,{auto_skipped,_}} ->
		    add_to_stats(auto_skipped);
		{_,{SkipOrFail,_Reason}} ->
		    add_to_stats(SkipOrFail)
	    end;
	tc_user_skip ->
	    %% test case or config function specified as skipped in testspec,
	    %% or init config func for suite/group has returned {skip,Reason}
	    %% Data = {Suite,{Func,GroupName},Comment}
	    {Func,Data1} = case Data of
			       {Suite,{F,undefined},Comment} ->
				   {F,{Suite,F,Comment}};
			       D = {_,{F,_},_} ->
				   {F,D}
			   end,
	    ct_event:sync_notify(#event{name=tc_user_skip,
					node=node(),
					data=Data1}),
	    ct_hooks:on_tc_skip(What, Data1),
	    if Func /= init_per_suite, Func /= init_per_group,
	       Func /= end_per_suite, Func /= end_per_group ->
		    add_to_stats(user_skipped);
	       true ->
		    ok
	    end;
	tc_auto_skip ->
	    %% test case skipped because of error in config function, or
	    %% config function skipped because of error in info function
	    %% Data = {Suite,{Func,GroupName},Comment}
	    {Func,Data1} = case Data of
			       {Suite,{F,undefined},Comment} ->
				   {F,{Suite,F,Comment}};
			       D = {_,{F,_},_} ->
				   {F,D}
			   end,
	    %% this test case does not have a log, so printouts
	    %% from event handlers should end up in the main log
	    ct_event:sync_notify(#event{name=tc_auto_skip,
					node=node(),
					data=Data1}),
	    ct_hooks:on_tc_skip(What, Data1),
	    if Func /= end_per_suite, 
	       Func /= end_per_group ->
		    add_to_stats(auto_skipped);
	       true -> 
		    ok
	    end;
	framework_error ->
	    case Data of
		{{M,F},E} ->
		    ct_event:sync_notify(#event{name=tc_done,
						node=node(),
						data={M,F,{framework_error,E}}});
		_ ->
		    ct_event:sync_notify(#event{name=tc_done,
						node=node(),
						data=Data})
	    end;
	_ ->
	    ok
    end,
    catch vts:report(What,Data).

add_to_stats(Result) ->
    Update = fun({Ok,Failed,Skipped={UserSkipped,AutoSkipped}}) ->
		     Stats =
			 case Result of
			     ok ->
				 {Ok+1,Failed,Skipped};
			     failed ->
				 {Ok,Failed+1,Skipped};
			     skipped ->
				 {Ok,Failed,{UserSkipped+1,AutoSkipped}};
			     user_skipped ->
				 {Ok,Failed,{UserSkipped+1,AutoSkipped}};
			     auto_skipped ->
				 {Ok,Failed,{UserSkipped,AutoSkipped+1}}
			 end,
		     ct_event:sync_notify(#event{name=test_stats,
						 node=node(),
						 data=Stats}),
		     Stats
	     end,
    ct_util:update_testdata(stats, Update).

%%%-----------------------------------------------------------------
%%% @spec warn(What) -> true | false
warn(What) when What==nodes; What==processes ->
    false;
warn(_What) ->
    true.

%%%-----------------------------------------------------------------
%%% @spec add_data_dir(File0, Config) -> File1
add_data_dir(File,Config) when is_atom(File) ->
    add_data_dir(atom_to_list(File),Config);

add_data_dir(File,Config) when is_list(File) ->
    case filename:split(File) of
	[File] ->
	    %% no user path, add data dir
	    case lists:keysearch(data_dir,1,Config) of
		{value,{data_dir,DataDir}} ->
		    filename:join(DataDir,File);
		_ ->
		    File
	    end;
	_ ->
	    File
    end.

%%%-----------------------------------------------------------------
%%% @spec get_logopts() -> [LogOpt]
get_logopts() ->
    case ct_util:get_testdata(logopts) of
	undefined ->
	    [];
	LogOpts ->
	    LogOpts
    end.

%%%-----------------------------------------------------------------
%%% @spec format_comment(Comment) -> HtmlComment
format_comment(Comment) ->
    "<font color=\"green\">" ++ Comment ++ "</font>".

%%%-----------------------------------------------------------------
%%% @spec get_html_wrapper(TestName, PrintLabel, Cwd) -> Header
get_html_wrapper(TestName, PrintLabel, Cwd, TableCols) ->
    get_html_wrapper(TestName, PrintLabel, Cwd, TableCols, utf8).

get_html_wrapper(TestName, PrintLabel, Cwd, TableCols, Encoding) ->
    ct_logs:get_ts_html_wrapper(TestName, PrintLabel, Cwd, TableCols, Encoding).

%%%-----------------------------------------------------------------
%%% @spec get_log_dir() -> {ok,LogDir}
get_log_dir() ->
    ct_logs:get_log_dir(true).
