%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% @doc Common Test Framework callback module.
%%%
%%% <p>This module exports framework callback functions which are
%%% called from the test_server.</p>

-module(ct_framework).

-export([init_tc/3, end_tc/3, get_suite/2, report/2, warn/1]).
-export([error_notification/4]).

-export([error_in_suite/1, ct_init_per_group/2, ct_end_per_group/2]).

-include("ct_event.hrl").
-include("ct_util.hrl").

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
init_tc(Mod,Func,Config) ->
    %% check if previous testcase was interpreted and has left
    %% a "dead" trace window behind - if so, kill it
    case ct_util:get_testdata(interpret) of
	{What,kill,{TCPid,AttPid}} ->
	    ct_util:kill_attached(TCPid,AttPid),
	    ct_util:set_testdata({interpret,{What,kill,{undefined,undefined}}});
	_ ->
	    ok
    end,
	    
    %% check if we need to add defaults explicitly because
    %% there's no init_per_suite exported from Mod
    {InitFailed,DoInit} =
	case ct_util:get_testdata(curr_tc) of
	    {Mod,{suite0_failed,_}=Failure} ->
		{Failure,false};
	    {Mod,_} ->
		{false,false};
	    _ when Func == init_per_suite ->
		{false,false};
	    _ ->
		{false,true}
	end,
    case InitFailed of
	false ->
	    ct_util:set_testdata({curr_tc,{Mod,Func}}),
	    case ct_util:read_suite_data({seq,Mod,Func}) of
		undefined ->
		    init_tc1(Mod,Func,Config,DoInit);
		Seq when is_atom(Seq) ->
		    case ct_util:read_suite_data({seq,Mod,Seq}) of
			[Func|TCs] ->		% this is the 1st case in Seq
			    %% make sure no cases in this seq are marked as failed
			    %% from an earlier execution in the same suite
			    lists:foreach(fun(TC) ->
						  ct_util:save_suite_data({seq,Mod,TC},Seq)
					  end, TCs);
			_ ->
			    ok
		    end,
		    init_tc1(Mod,Func,Config,DoInit);
		{failed,Seq,BadFunc} ->
		    {skip,{sequence_failed,Seq,BadFunc}}
	    end;
	{_,{require,Reason}} ->
	    {skip,{require_failed_in_suite0,Reason}};
	_ ->
	    {skip,InitFailed}
    end.	    

init_tc1(Mod,Func,[Config0],DoInit) when is_list(Config0) ->
    Config1 = 
	case ct_util:read_suite_data(last_saved_config) of
	    {{Mod,LastFunc},SavedConfig} ->	% last testcase
		[{saved_config,{LastFunc,SavedConfig}} | 
		 lists:keydelete(saved_config,1,Config0)];
	    {{LastSuite,InitOrEnd},SavedConfig} when InitOrEnd == init_per_suite ;
						     InitOrEnd == end_per_suite -> % last suite
		[{saved_config,{LastSuite,SavedConfig}} | 
		 lists:keydelete(saved_config,1,Config0)];
	    undefined ->
		lists:keydelete(saved_config,1,Config0)
	end,
    ct_util:delete_suite_data(last_saved_config),
    Config = lists:keydelete(watchdog,1,Config1),
    if Func /= init_per_suite, DoInit /= true ->
	    ok;
	true ->
	    %% delete all default values used in previous suite
	    ct_config:delete_default_config(suite),
	    %% release all name -> key bindings (once per suite)
	    ct_config:release_allocated()
    end,
    TestCaseInfo =
	case catch apply(Mod,Func,[]) of
	    Result when is_list(Result) -> Result;
	    _ -> []
	end,
    %% clear all config data default values set by previous
    %% testcase info function (these should only survive the
    %% testcase, not the whole suite)
    ct_config:delete_default_config(testcase),
    case add_defaults(Mod,Func,TestCaseInfo,DoInit) of
	Error = {suite0_failed,_} ->
	    ct_logs:init_tc(),
	    FuncSpec = group_or_func(Func,Config0),
	    ct_event:notify(#event{name=tc_start,
				   node=node(),
				   data={Mod,FuncSpec}}),
	    ct_util:set_testdata({curr_tc,{Mod,Error}}),
	    {error,Error};
	{SuiteInfo,MergeResult} ->
	    case MergeResult of
		{error,Reason} when DoInit == false ->
		    ct_logs:init_tc(),
		    FuncSpec = group_or_func(Func,Config0),
		    ct_event:notify(#event{name=tc_start,
					   node=node(),
					   data={Mod,FuncSpec}}),
		    {skip,Reason};
		_ ->
		    init_tc2(Mod,Func,SuiteInfo,MergeResult,Config,DoInit)
	    end
    end;
init_tc1(_Mod,_Func,Args,_DoInit) ->
    {ok,Args}.

init_tc2(Mod,Func,SuiteInfo,MergeResult,Config,DoInit) ->
    %% if first testcase fails when there's no init_per_suite
    %% we must do suite/0 configurations before skipping test
    MergedInfo =
	case MergeResult of
	    {error,_} when DoInit == true ->
		SuiteInfo;
	    _ ->
		MergeResult
	end,
    %% timetrap must be handled before require
    MergedInfo1 = timetrap_first(MergedInfo, [], []),
    %% tell logger to use specified style sheet
    case lists:keysearch(stylesheet,1,MergedInfo++Config) of
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
	    case lists:keysearch(silent_connections,1,MergedInfo++Config) of
		{value,{silent_connections,Conns}} ->
		    ct_util:silence_connections(Conns);
		_ ->
		    ok
	    end;
	Conns ->
	    ct_util:silence_connections(Conns)
    end,
    
    ct_logs:init_tc(),
    FuncSpec = group_or_func(Func,Config),
    ct_event:notify(#event{name=tc_start,
			   node=node(),
			   data={Mod,FuncSpec}}),
    
    case configure(MergedInfo1,MergedInfo1,SuiteInfo,{Func,DoInit},Config) of
	{suite0_failed,Reason} ->
	    ct_util:set_testdata({curr_tc,{Mod,{suite0_failed,{require,Reason}}}}),
	    {skip,{require_failed_in_suite0,Reason}};
	{error,Reason} ->
	    {auto_skip,{require_failed,Reason}};
	FinalConfig ->
	    case MergeResult of
		{error,Reason} ->
		    %% suite0 configure finished now, report that 
		    %% first test case actually failed		    
		    {skip,Reason};
		_ ->
		    case get('$test_server_framework_test') of
			undefined ->
			    FinalConfig;
			Fun ->
			    Fun(init_tc, FinalConfig)
		    end
	    end
    end.
	    

add_defaults(Mod,Func,FuncInfo,DoInit) ->
    case (catch Mod:suite()) of
	{'EXIT',{undef,_}} ->
	    SuiteInfo = merge_with_suite_defaults(Mod,[]),
	    case add_defaults1(Mod,Func,FuncInfo,SuiteInfo,DoInit) of
		Error = {error,_} -> {SuiteInfo,Error};
		MergedInfo -> {SuiteInfo,MergedInfo}
	    end;
	{'EXIT',Reason} -> 
	    {suite0_failed,{exited,Reason}};
	SuiteInfo when is_list(SuiteInfo) ->
	    case lists:all(fun(E) when is_tuple(E) -> true;
			      (_) -> false
			   end, SuiteInfo) of
		true ->
		    SuiteInfo1 = merge_with_suite_defaults(Mod,SuiteInfo),
		    case add_defaults1(Mod,Func,FuncInfo,SuiteInfo1,DoInit) of
			Error = {error,_} -> {SuiteInfo1,Error};
			MergedInfo -> {SuiteInfo1,MergedInfo}
		    end;
		false ->
		    {suite0_failed,bad_return_value}
	    end;
	_ ->
	    {suite0_failed,bad_return_value}
    end.
    
add_defaults1(_Mod,init_per_suite,[],SuiteInfo,_) ->
    SuiteInfo;

add_defaults1(Mod,Func,FuncInfo,SuiteInfo,DoInit) ->
    %% mustn't re-require suite variables in test case info function,
    %% can result in weird behaviour (suite values get overwritten)
    SuiteReqs = 
	[SDDef || SDDef <- SuiteInfo,
		  require == element(1,SDDef)],
    case [element(2,Clash) || Clash <- SuiteReqs, 
	  true == lists:keymember(element(2,Clash),2,FuncInfo)] of
	[] ->
	    add_defaults2(Mod,Func,FuncInfo,SuiteInfo,SuiteReqs,DoInit);
	Clashes ->
	    {error,{config_name_already_in_use,Clashes}}
    end.

add_defaults2(_Mod,_Func,FuncInfo,SuiteInfo,_,false) ->
    %% include require elements from test case info, but not from suite/0
    %% (since we've already required those vars)
    FuncInfo ++
	[SFDef || SFDef <- SuiteInfo,
		  require /= element(1,SFDef),
		  false == lists:keymember(element(1,SFDef),1,FuncInfo)];

add_defaults2(_Mod,_Func,FuncInfo,SuiteInfo,SuiteReqs,true) ->
    %% We must include require elements from suite/0 here since
    %% there's no init_per_suite call before this first test case.
    %% Let other test case info elements override those from suite/0.
    FuncInfo ++ SuiteReqs ++
	[SDDef || SDDef <- SuiteInfo,
		  require /= element(1,SDDef),
		  false == lists:keymember(element(1,SDDef),1,FuncInfo)].    

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
    [{timetrap,{minutes,30}} | lists:reverse(Info)];
timetrap_first([],Info,Found) ->
    lists:reverse(Found) ++ lists:reverse(Info).

configure([{require,Required}|Rest],Info,SuiteInfo,Scope,Config) ->
    case ct:require(Required) of
	ok ->
	    configure(Rest,Info,SuiteInfo,Scope,Config);
	Error = {error,Reason} ->
	    case required_default('_UNDEF',Required,Info,SuiteInfo,Scope) of
		ok ->
		    configure(Rest,Info,SuiteInfo,Scope,Config);
		_ ->
		    case lists:keymember(Required,2,SuiteInfo) of
			true ->
			    {suite0_failed,Reason};
			false ->
			    Error
		    end
	    end
    end;
configure([{require,Name,Required}|Rest],Info,SuiteInfo,Scope,Config) ->
    case ct:require(Name,Required) of
	ok ->
	    configure(Rest,Info,SuiteInfo,Scope,Config);
	Error = {error,Reason} ->
	    case required_default(Name,Required,Info,SuiteInfo,Scope) of
		ok ->
		    configure(Rest,Info,SuiteInfo,Scope,Config);
		_ ->
		    case lists:keymember(Name,2,SuiteInfo) of
			true -> 
			    {suite0_failed,Reason};
			false ->
			    Error
		    end
	    end
    end;
configure([{timetrap,off}|Rest],Info,SuiteInfo,Scope,Config) ->
    configure(Rest,Info,SuiteInfo,Scope,Config);
configure([{timetrap,Time}|Rest],Info,SuiteInfo,Scope,Config) ->
    Dog = test_server:timetrap(Time),
    configure(Rest,Info,SuiteInfo,Scope,[{watchdog,Dog}|Config]);
configure([_|Rest],Info,SuiteInfo,Scope,Config) ->
    configure(Rest,Info,SuiteInfo,Scope,Config);
configure([],_,_,_,Config) ->
    {ok,[Config]}.

%% the require element in Info may come from suite/0 and
%% should be scoped 'suite', or come from the testcase info
%% function and should then be scoped 'testcase'
required_default(Name,Key,Info,SuiteInfo,{Func,true}) ->
    case try_set_default(Name,Key,SuiteInfo,suite) of
	ok ->
	    ok;
	_ ->
	    required_default(Name,Key,Info,[],{Func,false})
    end;
required_default(Name,Key,Info,_,{init_per_suite,_}) ->
    try_set_default(Name,Key,Info,suite);
required_default(Name,Key,Info,_,_) ->
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
	    [ct_config:set_default_config([CfgVal],Where) || CfgVal <- CfgElems],
	    ok;
	_ ->
	    [ct_config:set_default_config(Name,[CfgVal],Where) || CfgVal <- CfgElems],
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
end_tc(?MODULE,error_in_suite,_) ->		% bad start!
    ok;
end_tc(Mod,Func,{TCPid,Result,[Args]}) when is_pid(TCPid) ->
    end_tc(Mod,Func,TCPid,Result,Args);
end_tc(Mod,Func,{Result,[Args]}) ->
    end_tc(Mod,Func,self(),Result,Args).

end_tc(Mod,Func,TCPid,Result,Args) ->
    case lists:keysearch(watchdog,1,Args) of
	{value,{watchdog,Dog}} -> test_server:timetrap_cancel(Dog);
	false -> ok
    end,

    %% save the testcase process pid so that it can be used
    %% to look up the attached trace window later
    case ct_util:get_testdata(interpret) of
	{What,kill,_} ->
	    AttPid = ct_util:get_attached(self()),
	    ct_util:set_testdata({interpret,{What,kill,{self(),AttPid}}});
	_ ->
	    ok
    end,

    ct_util:delete_testdata(comment),
    ct_util:delete_suite_data(last_saved_config),
    FuncSpec =
	case group_or_func(Func,Args) of
	    {_,GroupName,_Props} = Group ->			       
		case lists:keysearch(save_config,1,Args) of
		    {value,{save_config,SaveConfig}} ->
			ct_util:save_suite_data(last_saved_config,
						{Mod,{group,GroupName}},SaveConfig),
			Group;
		    false ->
			Group
		end;
	    _ ->
		case lists:keysearch(save_config,1,Args) of
		    {value,{save_config,SaveConfig}} ->
			ct_util:save_suite_data(last_saved_config,
						{Mod,Func},SaveConfig),
			Func;
		    false ->
			Func
		end
	end,
    ct_util:reset_silent_connections(),

    %% send sync notification so that event handlers may print
    %% in the log file before it gets closed
    ct_event:sync_notify(#event{name=tc_done,
				node=node(),
				data={Mod,FuncSpec,tag(Result)}}),
    case Result of
	{skip,{sequence_failed,_,_}} ->
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
	    ct_util:match_delete_suite_data({seq,Mod,'_'});
	_ -> 
	    ok
    end,
    case get('$test_server_framework_test') of
	undefined ->
	    ok;
	Fun ->
	    Fun(end_tc, ok)
    end.

%% {error,Reason} | {skip,Reason} | {timetrap_timeout,TVal} | 
%% {testcase_aborted,Reason} | testcase_aborted_or_killed | 
%% {'EXIT',Reason} | Other (ignored return value, e.g. 'ok')
tag({STag,Reason}) when STag == skip; STag == skipped -> 
    {skipped,Reason};
tag(E = {ETag,_}) when ETag == error; ETag == 'EXIT'; 
                       ETag == timetrap_timeout;
                       ETag == testcase_aborted -> 
    {failed,E};
tag(E = testcase_aborted_or_killed) ->
    {failed,E};
tag(Other) ->
    Other.

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
    ErrSpec = case Error of
		 {What={_E,_R},Trace} when is_list(Trace) ->
		      What;
		  What ->
		      What
	      end,
    ErrStr = case ErrSpec of
		 {badmatch,Descr} ->
		     Descr1 = lists:flatten(io_lib:format("~P",[Descr,10])),
		     if length(Descr1) > 50 ->
			     Descr2 = string:substr(Descr1,1,50),
			     io_lib:format("{badmatch,~s...}",[Descr2]);
			true ->
			     io_lib:format("{badmatch,~s}",[Descr1])
		     end;
		 {test_case_failed,Reason} ->
		     case (catch io_lib:format("{test_case_failed,~s}", [Reason])) of
			 {'EXIT',_} ->
			     io_lib:format("{test_case_failed,~p}", [Reason]);
			 Result -> Result
		     end;
		 {Spec,_Reason} when is_atom(Spec) ->
		     io_lib:format("~w", [Spec]);
		 Other ->
		     io_lib:format("~P", [Other,5])
	     end,
    ErrorHtml = "<font color=\"brown\">" ++ ErrStr ++ "</font>",
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
	    case ct_util:get_testdata(comment) of
		undefined ->
		    test_server:comment(ErrorHtml);
		Comment ->
		    CommentHtml = 
			"<font color=\"green\">" ++ "(" ++ "</font>"
			++ Comment ++ 
			"<font color=\"green\">" ++ ")" ++ "</font>",
		    Str = io_lib:format("~s   ~s", [ErrorHtml,CommentHtml]),
		    test_server:comment(Str)
	    end
    end,

    io:format(user, "~n- - - - - - - - - - - - - - - - "
	      "- - - - - - - - - -~n", []),
    case Loc of
	%% we don't use the line parse transform as we compile this 
	%% module so location will be on form {M,F}
	[{?MODULE,error_in_suite}] ->
	    io:format(user, "Error in suite detected: ~s", [ErrStr]);

	unknown ->
	    io:format(user, "Error detected: ~s", [ErrStr]);

	%% if a function specified by all/0 does not exist, we
	%% pick up undef here
	[{LastMod,LastFunc}] ->
	    io:format(user, "~w:~w could not be executed~n", 
		      [LastMod,LastFunc]),
	    io:format(user, "Reason: ~s", [ErrStr]);
	    
	[{LastMod,LastFunc,LastLine}|_] ->
	    %% print error to console, we are only
	    %% interested in the last executed expression
	    io:format(user, "~w:~w failed on line ~w~n", 
		      [LastMod,LastFunc,LastLine]),
	    io:format(user, "Reason: ~s", [ErrStr]),
    
	    case ct_util:read_suite_data({seq,Mod,Func}) of
		undefined ->
		    ok;
		Seq ->
		    SeqTCs = ct_util:read_suite_data({seq,Mod,Seq}),
		    mark_as_failed(Seq,Mod,Func,SeqTCs)
	    end	    
    end,
    io:format(user, "~n- - - - - - - - - - - - - - - - "
	      "- - - - - - - - - -~n~n", []),
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
    case proplists:get_value(tc_group_properties,Config) of
	undefined ->
	    {Func,unknown,[]};
	GrProps ->
	    GrName = proplists:get_value(name,GrProps),
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
	    case catch check_groups(Mod, GroupDefs) of
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
%%! is deprecated and should be removed after OTP R13!
%%!============================================================

get_suite(Mod, Name) ->
    %% Name may be name of a group or a test case. If it's a group,
    %% it should be expanded to list of cases (in a conf term)
    case catch apply(Mod, groups, []) of
	{'EXIT',_} ->
	    get_seq(Mod, Name);
	GroupDefs when is_list(GroupDefs) ->
	    case catch check_groups(Mod, GroupDefs) of
		{error,_} = Error ->
		    %% this makes test_server call error_in_suite as first
		    %% (and only) test case so we can report Error properly
		    [{?MODULE,error_in_suite,[[Error]]}];
		ConfTests ->
		    FindConf = fun({conf,Props,_,_,_}) ->
				       case proplists:get_value(name, Props) of
					   Name -> true;
					   _    -> false
				       end
			       end,					 
		    case lists:filter(FindConf, ConfTests) of
			[] ->			% must be a test case
			    get_seq(Mod, Name);
			[ConfTest|_] ->
			    ConfTest
		    end
	    end;
	_ ->
	    E = "Bad return value from "++atom_to_list(Mod)++":groups/0",
	    [{?MODULE,error_in_suite,[[{error,list_to_atom(E)}]]}]
    end.

check_groups(_Mod, []) ->
    [];
check_groups(Mod, Defs) ->
    check_groups(Mod, Defs, Defs, []).

check_groups(Mod, [TC | Gs], Defs, Levels) when is_atom(TC), length(Levels)>0 ->
    [TC | check_groups(Mod, Gs, Defs, Levels)];

check_groups(Mod, [{group,SubName} | Gs], Defs, Levels) when is_atom(SubName) ->
    case lists:member(SubName, Levels) of
	true ->
	    E = "Cyclic reference to group "++atom_to_list(SubName)++
		" in "++atom_to_list(Mod)++":groups/0",
	    throw({error,list_to_atom(E)});
	false ->	    
	    case find_group(Mod, SubName, Defs) of
		{error,_} = Error ->
		    throw(Error);
		G ->
		    [check_groups(Mod, [G], Defs, Levels) | 
		     check_groups(Mod, Gs, Defs, Levels)]
	    end
    end;

check_groups(Mod, [{Name,Tests} | Gs], Defs, Levels) when is_atom(Name),
							  is_list(Tests) ->
    check_groups(Mod, [{Name,[],Tests} | Gs], Defs, Levels);

check_groups(Mod, [{Name,Props,Tests} | Gs], Defs, Levels) when is_atom(Name),
								is_list(Props),
								is_list(Tests) ->
    {TestSpec,Levels1} = 
	case Levels of
	    [] ->
		{check_groups(Mod, Tests, Defs, [Name]),[]};
	    _ ->
		{check_groups(Mod, Tests, Defs, [Name|Levels]),Levels}
	end,
    [make_conf(Mod, Name, Props, TestSpec) | 
     check_groups(Mod, Gs, Defs, Levels1)];

check_groups(Mod, [BadTerm | _Gs], _Defs, Levels) ->
    Where = if length(Levels) == 0 ->
		    atom_to_list(Mod)++":groups/0";
	       true ->
		    "group "++atom_to_list(lists:last(Levels))++
			" in "++atom_to_list(Mod)++":groups/0"
	    end,		 
    Term = io_lib:format("~p", [BadTerm]),
    E = "Bad term "++lists:flatten(Term)++" in "++Where,
    throw({error,list_to_atom(E)});

check_groups(_Mod, [], _Defs, _) ->
    [].

find_group(Mod, Name, Defs) ->
    case lists:keysearch(Name, 1, Defs) of
	{value,Def} -> 
	    Def;
	false ->
	    E = "Invalid group "++atom_to_list(Name)++
		" in "++atom_to_list(Mod)++":groups/0",
	    throw({error,list_to_atom(E)})
    end.

make_conf(Mod, Name, Props, TestSpec) ->
    {InitConf,EndConf} =
	case erlang:function_exported(Mod,init_per_group,2) of
	    true ->
		{{Mod,init_per_group},{Mod,end_per_group}};
	    false ->
		{{?MODULE,ct_init_per_group},
		 {?MODULE,ct_end_per_group}}
	end,
    {conf,[{name,Name}|Props],InitConf,TestSpec,EndConf}.


get_all(Mod, ConfTests) ->	
    case catch apply(Mod, all, []) of
	{'EXIT',_} ->
	    Reason = 
		list_to_atom(atom_to_list(Mod)++":all/0 is missing"),
	    %% this makes test_server call error_in_suite as first
	    %% (and only) test case so we can report Reason properly
	    [{?MODULE,error_in_suite,[[{error,Reason}]]}];
	AllTCs when is_list(AllTCs) ->
	    case catch save_seqs(Mod,AllTCs) of
		{error,What} ->
		    [{?MODULE,error_in_suite,[[{error,What}]]}];
		SeqsAndTCs ->
		    %% expand group references in all() using ConfTests
		    Expand =
			fun({group,Name}) ->
				FindConf = 
				    fun({conf,Props,_,_,_}) ->
					    case proplists:get_value(name, Props) of
						Name -> true;
						_    -> false
					    end
					   end,					 
				case lists:filter(FindConf, ConfTests) of
				    [ConfTest|_] ->
					ConfTest;
				    [] ->
					E = "Invalid reference to group "++
					    atom_to_list(Name)++" in "++
					    atom_to_list(Mod)++":all/0",
					throw({error,list_to_atom(E)})
				end;
			   (SeqOrTC) -> SeqOrTC
			end,
		    case catch lists:map(Expand, SeqsAndTCs) of
			{error,_} = Error ->
			    [{?MODULE,error_in_suite,[[Error]]}];
			Tests ->
			    Tests
		    end
	    end;
	Skip = {skip,_Reason} ->
	    Skip;
	_ ->
	    Reason = 
		list_to_atom("Bad return value from "++atom_to_list(Mod)++":all/0"),
	    [{?MODULE,error_in_suite,[[{error,Reason}]]}]
    end.


%%!============================================================
%%! The support for sequences by means of using sequences/0
%%! will be removed in OTP R14. The code below is only kept 
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
	    Reason = io_lib:format("regular test cases not allowed in sequence ~p: "
				   "~p",[Seq,Bad]),
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
				   "~p",[Bad]),
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

%% if the group config functions are missing in the suite,
%% use these instead
ct_init_per_group(GroupName, Config) ->
    ct_logs:log("WARNING", "init_per_group/2 for ~w missing in suite, using default.", 
		[GroupName]),
    Config.

ct_end_per_group(GroupName, _) ->
    ct_logs:log("WARNING", "end_per_group/2 for ~w missing in suite, using default.", 
		[GroupName]),
    ok.

    
%%%-----------------------------------------------------------------
%%% @spec report(What,Data) -> ok
report(What,Data) ->
    case What of
	tests_start ->
	    case ct_util:get_testdata(cover) of
		undefined -> 
		    ok;
		{_CovFile,_CovNodes,CovImport,CovExport,_CovAppData} ->
		    %% Always import cover data from files specified by CovImport 
		    %% if no CovExport defined. If CovExport is defined, only
		    %% import from CovImport files initially, then use CovExport
		    %% to pass coverdata between proceeding tests (in the same run).
		    Imps =
			case CovExport of
			    [] ->  % don't export data between tests
				CovImport;
			    _ ->
				case filelib:is_file(CovExport) of
				    true ->
					[CovExport];
				    false ->
					CovImport
				end
			end,
		    lists:foreach(
		      fun(Imp) ->
			      case cover:import(Imp) of
				  ok -> 
				      ok;
				  {error,Reason} ->
				      ct_logs:log("COVER INFO",
						  "Importing cover data from: ~s fails! "
						  "Reason: ~p", [Imp,Reason])
			      end
		      end, Imps)
	    end;
	tests_done ->
	    ok;
	tc_start ->
	    ok;
	tc_done ->
	    {_Suite,Case,Result} = Data,
	    case {Case,Result} of
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
		{_,{skipped,_}} ->
		    add_to_stats(user_skipped);
		{_,{FailOrSkip,_Reason}} ->
		    add_to_stats(FailOrSkip)
	    end;
	tc_user_skip ->	    
	    %% test case specified as skipped in testspec
	    %% Data = {Suite,Case,Comment}
	    ct_event:sync_notify(#event{name=tc_user_skip,
					node=node(),
					data=Data}),
	    add_to_stats(user_skipped);
	tc_auto_skip ->
	    %% test case skipped because of error in init_per_suite
	    %% Data = {Suite,Case,Comment}

	    {_Suite,Case,_Result} = Data,

	    %% this test case does not have a log, so printouts
	    %% from event handlers should end up in the main log
	    ct_event:sync_notify(#event{name=tc_auto_skip,
					node=node(),
					data=Data}),
	    if Case /= end_per_suite, Case /= end_per_group -> 
		    add_to_stats(auto_skipped);
	       true -> 
		    ok
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
%%% @spec add_data_dir(File0) -> File1
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


