%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(vts).

-export([start/0,
	 init_data/5,
	 stop/0,
	 report/2]).

-export([config_data/0,
	 start_link/0]).

-export([start_page/2,
	 title_frame/2,
	 menu_frame/2,
	 welcome_frame/2,
	 config_frame/2,
	 browse_config_file/2,
	 add_config_file/2,
	 remove_config_file/2,
	 run_frame/2,
	 add_test_dir/2,
	 remove_test_dir/2,
	 select_case/2,
	 select_suite/2,
	 run_test/2,
	 result_frameset/2,
	 result_summary_frame/2,
	 no_result_log_frame/2,
	 redirect_to_result_log_frame/2]).

-export([test_info/3]).

-define(START_PAGE,"/vts_erl/vts/start_page").

-define(tests,vts_tests).

%% Colors
-define(INFO_BG_COLOR,"#C0C0EA").

-record(state,{tests=[],config=[],event_handler=[],test_runner,
	       running=0,reload_results=false,start_dir,current_log_dir,
	       logopts=[],total=0,ok=0,fail=0,skip=0,testruns=[]}).


%%%-----------------------------------------------------------------
%%% User API
start() ->
    {ok, _} = ct_webtool:start(),
    ct_webtool:start_tools([],"app=vts").

init_data(ConfigFiles,EvHandlers,LogDir,LogOpts,Tests) ->
    call({init_data,ConfigFiles,EvHandlers,LogDir,LogOpts,Tests}).

stop() ->
    ct_webtool:stop_tools([],"app=vts"),
    ct_webtool:stop().

report(What,Data) ->
    call({report,What,Data}).

%%%-----------------------------------------------------------------
%%% Return config data used by ct_webtool
config_data() ->
    {ok,LogDir} =
	case lists:keysearch(logdir,1,init:get_arguments()) of
	    {value,{logdir,[LogD]}} -> {ok,filename:absname(LogD)};
	    false -> file:get_cwd()
	end,
    {vts,
     [{web_data,{"VisualTestServer",?START_PAGE}},
      {alias,{erl_alias,"/vts_erl",[?MODULE]}},
      {alias,{"/log_dir",LogDir}},
      {start,{child,{{local,?MODULE},
		     {?MODULE,start_link,[]},
		     permanent,100,worker,[?MODULE]}}}
	      ]}.

start_link() ->
    case whereis(?MODULE) of
	undefined ->
	    Self = self(),
	    Pid = spawn_link(fun() -> init(Self) end),
	    MRef = erlang:monitor(process,Pid),
	    receive
		{Pid,started} -> 
		    erlang:demonitor(MRef, [flush]),
		    {ok,Pid};
		{'DOWN',MRef,process,_,Reason} -> 
		    {error,{vts,died,Reason}}
	    end;
	Pid ->
	    {ok,Pid}
    end.

start_page(_Env,_Input) ->
    call(start_page).
title_frame(_Env,_Input) ->
    call(title_frame).
welcome_frame(_Env,_Input) ->
    call(welcome_frame).
menu_frame(_Env,_Input) ->
    call(menu_frame).
config_frame(_Env,_Input) ->
    call(config_frame).
browse_config_file(_Env,Input) ->
    call({browse_config_file,Input}).
add_config_file(_Env,Input) ->
    call({add_config_file,Input}).
remove_config_file(_Env,Input) ->
    call({remove_config_file,Input}).
run_frame(_Env,_Input) ->
    call(run_frame).
add_test_dir(_Env,Input) ->
    call({add_test_dir,Input}).
remove_test_dir(_Env,Input) ->
    call({remove_test_dir,Input}).
select_suite(_Env,Input) ->
    call({select_suite,Input}).
select_case(_Env,Input) ->
    call({select_case,Input}).
run_test(_Env,_Input) ->
    call(run_test).
result_frameset(_Env,_Input) ->
    call(result_frameset).
redirect_to_result_log_frame(_Env,_Input) ->
    call(redirect_to_result_log_frame).
result_summary_frame(_Env,_Input) ->
    call(result_summary_frame).
no_result_log_frame(_Env,_Input) ->
    call(no_result_log_frame).

aborted() ->
    call(aborted).

test_info(_VtsPid,Type,Data) ->
    call({test_info,Type,Data}).

init(Parent) ->
    register(?MODULE,self()),
    process_flag(trap_exit,true),
    Parent ! {self(),started},
    {ok,Cwd} = file:get_cwd(),
    InitState = #state{start_dir=Cwd},
    loop(InitState).

loop(State) ->
    receive
	{{init_data,Config,EvHandlers,LogDir,LogOpts,Tests},From} ->
	    %% ct:pal("State#state.current_log_dir=~p", [State#state.current_log_dir]),
	    NewState = State#state{config=Config,event_handler=EvHandlers,
				   current_log_dir=LogDir,
				   logopts=LogOpts,tests=Tests},
	    _ = ct_install(NewState),
	    return(From,ok),
	    loop(NewState);
	{start_page,From} ->
	    return(From,start_page1()),
	    loop(State);
	{title_frame,From} ->
	    return(From,title_frame1()),
	    loop(State);
	{welcome_frame,From} ->
	    return(From,welcome_frame1()),
	    loop(State);
	{menu_frame,From} ->
	    return(From,menu_frame1()),
	    loop(State);
	{config_frame,From} ->
	    return(From,config_frame1(State)),
	    loop(State);
	{{browse_config_file,_Input},From} ->
	    return(From,ok),
	    loop(State);
	{{add_config_file,Input},From} ->
	    {Return,State1} = add_config_file1(Input,State),
	    _ = ct_install(State1),
	    return(From,Return),
	    loop(State1);
	{{remove_config_file,Input},From} ->
	    {Return,State1} = remove_config_file1(Input,State),
	    _ = ct_install(State1),
	    return(From,Return),
	    loop(State1);
	{run_frame,From} ->
	    return(From,run_frame1(State)),
	    loop(State);
	{{add_test_dir,Input},From} ->
	    {Return,State1} = add_test_dir1(Input,State),
	    return(From,Return),
	    loop(State1);
	{{remove_test_dir,Input},From} ->
	    {Return,State1} = remove_test_dir1(Input,State),
	    return(From,Return),
	    loop(State1);
	{{select_suite,Input},From} ->
	    {Return,State1} = select_suite1(Input,State),
	    return(From,Return),
	    loop(State1);
	{{select_case,Input},From} ->
	    {Return,State1} = select_case1(Input,State),
	    return(From,Return),
	    loop(State1);
	{run_test,From} ->
	    State1 = run_test1(State),
	    return(From,redirect_to_result_frameset1()),
	    loop(State1);
	{result_frameset,From} ->
	    return(From,result_frameset1(State)),
	    loop(State);
	{redirect_to_result_log_frame,From} ->
	    return(From,redirect_to_result_log_frame1(State)),
	    loop(State);
	{result_summary_frame,From} ->
	    return(From,result_summary_frame1(State)),
	    loop(State);
	stop_reload_results ->
	    ok = file:set_cwd(State#state.start_dir),
	    loop(State#state{reload_results=false});
	{no_result_log_frame,From} ->
	    return(From,no_result_log_frame1()),
	    loop(State);
	{aborted,From} ->
	    return(From,ok),
	    loop(State#state{test_runner=undefined,running=0});
	{{report,What,Data},From} ->
	    State1 = report1(What,Data,State),
	    return(From,ok),
	    loop(State1);
	{stop,From} -> 
	    return(From,ok);
	{'EXIT',Pid,Reason} ->
	    case State#state.test_runner of
		Pid ->
		    io:format("Test run error: ~p\n",[Reason]),
		    loop(State);
		_ ->
		    loop(State)
	    end;
	{{test_info,_Type,_Data},From} ->
	    return(From,ok),
	    loop(State)
    end.

call(Msg) ->
    case whereis(?MODULE) of
	undefined -> {error,no_proc};
	Pid ->
	    MRef = erlang:monitor(process,Pid),
	    Ref = make_ref(),
	    Pid ! {Msg,{self(),Ref}},
	    receive
		{Ref, Result} -> 
		    erlang:demonitor(MRef, [flush]),
		    Result;
		{'DOWN',MRef,process,_,Reason}  -> 
		    {error,{process_down,Pid,Reason}}
	    end
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result},
    ok.

run_test1(State=#state{tests=Tests,current_log_dir=LogDir,
		       logopts=LogOpts}) ->
    Self=self(),
    RunTest = fun() ->
		      case ct_run:do_run(Tests,[],LogDir,LogOpts) of
			  {error,_Reason} ->
			      aborted();
			  _ ->
			      ok
		      end,
		      unlink(Self)
	      end,
    Pid = spawn_link(RunTest),
    {Total,Tests1} =
	receive 
	    {{test_info,start_info,{_,_,Cases}},From} ->
		return(From,ok),
		{Cases,Tests};
	    EXIT = {'EXIT',_,_} ->
		self() ! EXIT,
		{0,[]}
	after 30000 ->
		{0,[]}
	end,
    State#state{test_runner=Pid,running=length(Tests1),
		total=Total,ok=0,fail=0,skip=0,testruns=[]}.

 
ct_install(#state{config=Config,event_handler=EvHandlers,
		  current_log_dir=LogDir}) ->
    ct_run:install([{config,Config},{event_handler,EvHandlers}],LogDir).
%%%-----------------------------------------------------------------
%%% HTML
start_page1() ->
    header("Visual Test Server Start Page",start_page_frameset()).

start_page_frameset() ->
    frameset(
      "ROWS=\"60,*\"",
      [frame(["NAME=\"title\" SRC=\"./title_frame\""]),
       frameset(
	 "COLS=\"150,*\"",
	 [frame(["NAME=\"menu\" SRC=\"./menu_frame\""]),
	  frame(["NAME=\"main\" SRC=\"./welcome_frame\""])])]).


title_frame1() ->
    header(body("BGCOLOR=lightgrey TEXT=darkgreen",title_body())).

title_body() ->
    p("ALIGN=center",font("SIZE=\"+3\"",b("Visual Test Server"))).

welcome_frame1() ->
    header(body(welcome_body())).

welcome_body() ->
    table(
      "WIDTH=100% HEIGHT=60%",
      [tr("VALIGN=middle",
	  td("ALIGN=center",
	     font("SIZE=6",
		  ["Welcome to the",br(),
		   "Visual Test Server"])))]).

menu_frame1() ->
    header(body(menu_body())).

menu_body() ->
    [h2("Content"),
     ul([
	 li(href(["TARGET=\"main\""],"./config_frame","Config")),
	 li(href(["TARGET=\"main\""],"./run_frame","Run")),
	 li(href(["TARGET=\"main\""],"./result_frameset","Result"))
	]),
     h2("Logs"),
     ul([
	 li(href(["TARGET=\"new\""],"/log_dir/index.html","Last Runs")),
	 li(href(["TARGET=\"new\""],"/log_dir/all_runs.html","All Runs"))
	])
    ].

config_frame1(State) ->
    header("Config",body(config_body(State))).
    
config_body(State) ->
    Entry = [input("TYPE=file NAME=browse SIZE=40"),
	     input("TYPE=hidden NAME=file")],
    BrowseForm =
	form(
	  "NAME=read_file_form METHOD=post ACTION=\"./browse_config_file\"",
	  table(
	    "BORDER=0",
	    [tr(td("1. Locate config file")),
	     tr(td(Entry))])),
    AddForm = 
	form(
	  "NAME=add_file_form METHOD=post ACTION=\"./add_config_file\"",
	  table(
	    "BORDER=0",
	    [tr(td("2. Paste full config file name here")),
	     tr(
	       [td(input("TYPE=text NAME=file SIZE=40")),
		td("ALIGN=center",
		   input("TYPE=submit onClick=\"file.value=browse.value;\""
			 " VALUE=\"Add\""))])])),

    {Text,RemoveForm} = 
	case State#state.config of
	    [] ->
		T = "Before running the tests, one or more configuration "
		    "files may be added. Locate the config file, copy its "
		    "full name, paste this into the text field below, then "
		    "click the \"Add\" button.",
		R = "",
		{T,R};
	    Files ->
		T = "The currently known configuration files are listed below. "
		    "To add a file, type the filename in the entry and "
		    "click the \"Add\" button. "
		    "To remove a file, select it and click the \"Remove\" "
		    "button.",
		ConfigFiles = [option(File) || File <- Files],
		Select = select("NAME=file TITLE=\"Select Config File\""
				" MULTIPLE=true",
				ConfigFiles),
		R = 
		    form(["NAME=remove_config METHOD=post ",
			  "ACTION=\"./remove_config_file\""],
			 table(
			   "BORDER=0",
			   [tr(td("ALIGN=center",Select)),
			    tr(td("ALIGN=center",
				  input("TYPE=submit VALUE=\"Remove\"")))])),
		{T,R}
	end,

    [h1("ALIGN=center","Config"),
     table(
       "WIDTH=450 ALIGN=center CELLPADDING=5",
       [tr(td(["BGCOLOR=",?INFO_BG_COLOR],Text)),
	tr(td("")),
	tr(td("")),
	tr(td("ALIGN=left",BrowseForm)),
	tr(td("ALIGN=left",AddForm)),
	tr(td("ALIGN=left",RemoveForm))])].

add_config_file1(Input,State) ->
    State1 = 
	case get_input_data(Input,"file") of
	    "" ->
		State;
	    File ->
		State#state{config=[File|State#state.config]}
	end,
    Return = config_frame1(State1),
    {Return,State1}.

remove_config_file1(Input,State) ->
    Files = get_all_input_data(Input,"file"),
    State1 = State#state{config=State#state.config--Files},
    Return = config_frame1(State1),
    {Return,State1}.
    


run_frame1(State) ->
    header("Run Test",body(run_body(State))).
    
run_body(#state{running=Running}) when Running>0 ->
    [h1("ALIGN=center","Run Test"),
     p(["Test are ongoing: ",href("./result_frameset","Results")])];
run_body(State) ->
    ConfigList =
	case State#state.config of
	    [] ->
		ul(["none"]);
	    CfgFiles ->
		ul([li(File) || File <- CfgFiles])
	end,
    ConfigFiles = [h3("Config Files"),
		   ConfigList],
    {ok,CWD} = file:get_cwd(),
    CurrWD = [h3("Current Working Directory"), ul(CWD)],
    AddDirForm = 
	form(
	  "NAME=add_dir_form METHOD=post ACTION=\"./add_test_dir\"",
	  table(
	    "BORDER=0",
	    [tr(td("COLSPAN=2","Enter test directory")),
	     tr(
	       [td(input("TYPE=text NAME=dir SIZE=40")),
		td("ALIGN=center",
		   input("TYPE=submit onClick=\"dir.value=browse.value;\""
			 " VALUE=\"Add Test Dir\""))])])),
    {LoadedTestsTable,Submit} = 
	case create_testdir_entries(State#state.tests,1) of
	    [] -> {"",""};
	    TestDirs ->
		Heading = tr([th(""),
			      th("ALIGN=left","Directory"),
			      th("ALIGN=left","Suite"),
			      th("ALIGN=left","Case")]),
		{table("CELLPADDING=5",[Heading,TestDirs]),
		submit_button()} 
	end,
    Body = 
	table(
	  "WIDTH=450 ALIGN=center",
	  [tr(td("")),
	   tr(td("")),
	   tr(td(ConfigFiles)),
	   tr(td("")),
	   tr(td(CurrWD)),
	   tr(td("")),
	   tr(td(AddDirForm)),
	   tr(td("")),
	   tr(td(LoadedTestsTable)),
	   tr(td(Submit))
	  ]),
    [h1("ALIGN=center","Run Test"), Body].

create_testdir_entries([{Dir,Suite,Case}|Tests],N) ->
    [testdir_entry(Dir,Suite,Case,N)|create_testdir_entries(Tests,N+1)];
create_testdir_entries([],_N) ->
    [].

testdir_entry(Dir,Suite,Case,N) ->
    NStr = vts_integer_to_list(N),
    tr([td(delete_button(NStr)),
	td(Dir),
	td(suite_select(Dir,Suite,NStr)),
	td(case_select(Dir,Suite,Case,NStr))]).

delete_button(N) ->
    form(["NAME=remove_dir_form METHOD=post ACTION=\"./remove_test_dir\""],
	 [input(["TYPE=hidden NAME=dir VALUE=\'",N,"\'"]),
	  input(["TYPE=submit VALUE=X"])]).

suite_select(Dir,Suite,N) ->
    case filelib:wildcard(filename:join(Dir,"*_SUITE.erl")) of
	[] ->
	    select("NAME=suite TITLE=\"Select suite\"","");
	Suites0 ->
	    Suites = [filename:basename(filename:rootname(S)) ||  S <- Suites0],
	    select("NAME=suite TITLE=\"Select suite\"",
		   options(["all"|Suites],atom_to_list(Suite),N,"select_suite"))
    end.

case_select(_Dir,all,_,N) ->
    select("NAME=case TITLE=\"Select case\"",
	   options(["all"],"all",N,"select_case"));
case_select(Dir,Suite,Case,N) ->
    MakeResult =
	case application:get_env(common_test, auto_compile) of
	    {ok,false} ->
		ok;
	    _ ->	    
		UserInclude =
		    case application:get_env(common_test, include) of
			{ok,UserInclDirs} when length(UserInclDirs) > 0 ->
			    [{i,UserInclDir} || UserInclDir <- UserInclDirs];
			_ ->
			    []
		    end,
		ct_run:run_make(Dir,Suite,UserInclude)
	end,
    case MakeResult of
	ok ->
	    true = code:add_pathz(Dir),
	    case catch apply(Suite,all,[]) of
		{'EXIT',Reason} ->
		    io:format("\n~p\n",[Reason]),
		    red(["COULD NOT READ TESTCASES!!",br(),
			 "See erlang shell for info"]);
		{skip,_Reason} ->
		    select("NAME=case TITLE=\"Select case\"",
			   options(["all"],"all",N,"select_case"));
		AllCasesAtoms ->
		    AllCases = [atom_to_list(C) || C <- AllCasesAtoms,
						   is_atom(C)],
		    select("NAME=case TITLE=\"Select case\"",
			   options(["all"|AllCases],atom_to_list(Case),
				   N,"select_case"))
	    end;
	_Error ->
	    red(["COMPILATION ERROR!!",br(),
		 "See erlang shell for info",br(),
		 "Reload this page when errors are fixed"])
    end.
	    

options([Selected|Elements],Selected,N,Func) ->
    [option(["SELECTED ",
	     "onClick=\"document.location.href=\'./",Func,"?n=",N,
	     "&selected=",Selected,"\';\""],
	    Selected)|
     options(Elements,Selected,N,Func)];
options([Element|Elements],Selected,N,Func) ->
    [option(["onClick=\"document.location.href=\'./",Func,"?n=",N,
	     "&selected=",Element,"\';\""],
	    Element)|
     options(Elements,Selected,N,Func)];
options([],_Selected,_N,_Func) ->
    [].

add_test_dir1(Input, State) ->
    State1 = 
	case get_input_data(Input,"dir") of
	    "" -> State;
	    Dir0 ->
		Dir = case ct_util:is_test_dir(Dir0) of
			  true  -> Dir0;
			  false -> ct_util:get_testdir(Dir0, all)
		      end,
		case filelib:is_dir(Dir) of
		    true  ->
			Test = ct_run:tests(Dir),
			State#state{tests=State#state.tests++Test};
		    false ->
			State
		end
	end,
    Return = run_frame1(State1),
    {Return,State1}.

remove_test_dir1(Input,State) ->
    N = list_to_integer(get_input_data(Input,"dir")),
    State1 = State#state{tests=delete_test(N,State#state.tests)},
    Return = run_frame1(State1),
    {Return,State1}.

delete_test(1,[_|T]) ->
    T;
delete_test(N,[H|T]) ->
    [H|delete_test(N-1,T)].

select_suite1(Input,State) ->
    N = list_to_integer(get_input_data(Input,"n")),
    Suite = list_to_atom(get_input_data(Input,"selected")),
    Tests1 = replace_suite(N,Suite,State#state.tests),
    State1 = State#state{tests=Tests1},
    Return = run_frame1(State1),
    {Return,State1}.

replace_suite(1,Suite,[{Dir,_,_}|T]) ->
    [Test] = ct_run:tests(Dir,Suite),
    [Test|T];
replace_suite(N,Suite,[H|T]) ->	    
    [H|replace_suite(N-1,Suite,T)].

select_case1(Input,State) ->
    N = list_to_integer(get_input_data(Input,"n")),
    Case = list_to_atom(get_input_data(Input,"selected")),
    Tests1 = replace_case(N,Case,State#state.tests),
    State1 = State#state{tests=Tests1},
    Return = run_frame1(State1),
    {Return,State1}.

replace_case(1,Case,[{Dir,Suite,_}|T]) ->
    [Test] = ct_run:tests(Dir,Suite,Case),
    [Test|T];
replace_case(N,Case,[H|T]) ->	    
    [H|replace_case(N-1,Case,T)].


submit_button() ->
    form(["NAME=run_test_form METHOD=post ACTION=\"./run_test\""],
	 [input("TYPE=submit VALUE=\"Run Test\"")]).
	    

redirect_to_result_frameset1() ->
    Head = 
	["<META HTTP-EQUIV=\"refresh\" CONTENT=\"1; URL=./result_frameset\">"],
    [header("",Head,body("Please wait..."))].

result_frameset1(State) ->
     header("Results",result_frameset2(State)).

result_frameset2(State) ->
    ResultLog = 
	case {State#state.current_log_dir,State#state.running} of
	    {undefined,0} ->
		"./no_result_log_frame";
	    {undefined,_} ->
		"./redirect_to_result_log_frame";
	    {_Dir,0} ->
		filename:join(["/log_dir","index.html"]);
	    {_Dir,_} when State#state.testruns == [] ->
		%% crash before first test
		"./no_result_log_frame";
	    {_Dir,_} ->
		{_,CurrentLog} = hd(State#state.testruns),
		CurrentLog
	end,
    frameset(
      "COLS=\"200,*\"",
     [frame(["NAME=\"result_summary\" SRC=\"./result_summary_frame\""]),
      frame(["NAME=\"result_log\" SRC=\"",ResultLog,"\""])]).

redirect_to_result_log_frame1(State) ->
    ResultLog = 
	case {State#state.testruns,State#state.running} of
	    {[],0} ->
		"./no_result_log_frame";
	    {[],_} ->
		"./redirect_to_result_log_frame";
	    {[{_,CurrentLog}|_],_} ->
		CurrentLog
	end,
    Head = ["<META HTTP-EQUIV=\"refresh\" CONTENT=\"1; URL=",ResultLog,"\">"],
    [header("",Head,body("Please wait..."))].

result_summary_frame1(State) ->
    case {State#state.running,State#state.reload_results} of
	{0,false} ->
	    header("Result Summary",body(result_summary_body(State)));
	_ ->
 	    Head = 
		"<SCRIPT LANGUAGE=\"JavaScript1.2\">\n"
		"\n"
		"function startReloadInterval() {\n"
		"    intervalId = setInterval(\"reloadPage()\",5000)\n"
		"}\n"
		"\n"
		"function reloadPage() {\n"
		"    location.reload()\n"
		"    parent.result_log.location.reload()\n"
%		"    parent.result_log.scrollBy(0, window.innerHeight)\n"
		"}\n"
		"</SCRIPT>\n",
 	    header("Result Summary",Head,
		   body("onLoad=\"startReloadInterval()\" BGCOLOR=\"#FFFFFF\"",
			result_summary_body(State)))
	end.
    
result_summary_body(State) ->
    N = State#state.ok + State#state.fail + State#state.skip,
    [h2("Result Summary"),
     p([b(vts_integer_to_list(N))," cases executed (of ",
	b(vts_integer_to_list(State#state.total)),")"]),
     p([green([b(vts_integer_to_list(State#state.ok))," successful"]),br(),
	red([b(vts_integer_to_list(State#state.fail))," failed"]),br(),
	orange([b(vts_integer_to_list(State#state.skip))," skipped"])]),
    executed_test_list(State)].

executed_test_list(#state{testruns=[]}) ->
    [];
executed_test_list(#state{testruns=TestRuns}) ->
    [h2("Executed Tests"),
     table(
       "",
       [tr(td(href("TARGET=\"result_log\"",Log,Name))) || 
	   {Name,Log} <- lists:reverse(TestRuns)])].


no_result_log_frame1() ->
    header("Test Results",body(no_result_log_body())).

no_result_log_body() ->
    [h1("ALIGN=center","Test Results"),
     p(["There are currently no test results available. ",
	br(),href("TARGET=\"main\"","./run_frame","You can run tests here")])].

report1(tests_start,{TestName,_N},State) ->
    {ok,LogDir} = ct_logs:get_log_dir(),
    TestRuns = 
	case State#state.testruns of
	    [{TestName,_}|_]=TR -> 
		TR;
	    TR -> 
		[{TestName,get_test_log(TestName,LogDir)}|TR]
	end,
    State#state{testruns=TestRuns};
report1(tests_done,{_Ok,_Fail,_Skip},State) ->
    {ok, _}  = timer:send_after(5000, self(),stop_reload_results),
    State#state{running=State#state.running-1,reload_results=true};
report1(tc_start,{_Suite,_Case},State) ->
    State;
report1(tc_done,{_Suite,init_per_suite,_},State) ->
    State;
report1(tc_done,{_Suite,end_per_suite,_},State) ->
    State;
report1(tc_done,{_Suite,init_per_group,_},State) ->
    State;
report1(tc_done,{_Suite,end_per_group,_},State) ->
    State;
report1(tc_done,{_Suite,_Case,ok},State) ->
    State#state{ok=State#state.ok+1};
report1(tc_done,{_Suite,_Case,{failed,_Reason}},State) ->
    State#state{fail=State#state.fail+1};
report1(tc_done,{_Suite,_Case,{skipped,_Reason}},State) ->
    State#state{skip=State#state.skip+1};
report1(tc_user_skip,{_Suite,_Case,_Reason},State) ->
    State#state{skip=State#state.skip+1};
report1(tc_auto_skip,{_Suite,_Case,_Reason},State) ->
    State#state{skip=State#state.skip+1};
report1(loginfo,_,State) ->
    State.

get_test_log(TestName,LogDir) ->
    [Log] = 
	filelib:wildcard(
	  filename:join([TestName++".logs","run*","suite.log.html"])),
    filename:join(["/log_dir",LogDir,Log]).



%get_description(Suite,Case) ->
%    case erlang:function_exported(Suite,Case,0) of
%	true ->
%	    case catch apply(Suite,Case,[]) of
%		{'EXIT',_Reason} ->
%		    "-";
%		Info ->
%		    case lists:keysearch(doc,1,Info) of
%			{value,{doc,Doc}} when is_list(Doc) ->
%			    Doc;
%			_ ->
%			    "-"
%		    end
%	    end;
%	false ->
%	    "-"
%    end.

%%%-----------------------------------------------------------------
%%% Internal library
header(Body) ->
    header("","",Body).
header(Title,Body) ->
    header(Title,"",Body).
header(Title,Head,Body) ->
    ["Pragma:no-cache\r\n",
     "Content-type: text/html\r\n\r\n",
     html_header(Title,Head,Body)].

html_header(Title,Head,Body) ->    
    ["<HTML>\n",
     "<HEAD>\n",
     "<TITLE>", Title,  "</TITLE>\n",
     Head,
     "</HEAD>\n",
     Body,
     "</HTML>"].

body(Text) ->
    ["<BODY BGCOLOR=\"#FFFFFF\">\n",Text,"<\BODY>\n"].
body(Args,Text) ->
    ["<BODY ", Args, ">\n", Text,"<\BODY>\n"].


frameset(Args,Frames) ->
    ["<FRAMESET ",Args,">\n", Frames, "\n</FRAMESET>\n"].
frame(Args) ->
    ["<FRAME ",Args, ">\n"].

table(Args,Text) ->
    ["<TABLE ", Args, ">\n", Text, "\n</TABLE>\n"].
tr(Text) ->
    ["<TR>\n", Text, "\n</TR>\n"].
tr(Args,Text) ->
    ["<TR ", Args, ">\n", Text, "\n</TR>\n"].
th(Text) ->
    ["<TH>", Text, "</TH>"].
th(Args,Text) ->
    ["<TH ", Args, ">\n", Text, "\n</TH>\n"].
td(Text) ->
    ["<TD>", Text, "</TD>"].
td(Args,Text) ->
    ["<TD ", Args, ">", Text, "</TD>"].

b(Text) ->
    ["<B>",Text,"</B>"].
%em(Text) ->    
%    ["<EM>",Text,"</EM>\n"].
%pre(Text) ->
%    ["<PRE>",Text,"</PRE>"].
href(Link,Text) ->
    ["<A HREF=\"",Link,"\">",Text,"</A>"].
href(Args,Link,Text) ->
    ["<A HREF=\"",Link,"\" ",Args,">",Text,"</A>"].
form(Args,Text) ->
    ["<FORM ",Args,">\n",Text,"\n</FORM>\n"].
input(Args) ->
    ["<INPUT ", Args, ">\n"].
select(Args,Text) ->
    ["<SELECT ", Args, ">\n", Text, "\n</SELECT>\n"].
option(Text) ->
    ["<OPTION>\n", Text, "\n</OPTION>\n"].
option(Args,Text) ->
    ["<OPTION ", Args, ">\n", Text, "\n</OPTION>\n"].
h1(Args,Text) ->
    ["<H1 ", Args, ">",Text,"</H1>\n"].
h2(Text) ->
    ["<H2>",Text,"</H2>\n"].
h3(Text) ->
    ["<H3>",Text,"</H3>\n"].
%%h4(Text) ->
%%    ["<H4>",Text,"</H4>\n"].
font(Args,Text) ->
    ["<FONT ",Args,">\n",Text,"\n</FONT>\n"].
p(Text) ->    
    ["<P>",Text,"</P>\n"].
p(Args, Text) ->    
    ["<P ", Args, ">",Text,"</P>\n"].
ul(Text) ->
    ["<UL>", Text, "</UL>\n"].
li(Text) ->
    ["<LI>", Text, "</LI>\n"].
br() ->
    "<BR>\n".

red(Text) -> color(red,Text).
green(Text) -> color(green,Text).
orange(Text) -> color(orange,Text).
color(Color,Text) when is_atom(Color) ->
    font(["COLOR=",atom_to_list(Color)],Text).

get_all_input_data(Input,Key)->
    List = parse(Input),
    get_all_input_data(List,Key,[]).
get_all_input_data([{Key,Value}|List],Key,Acc) ->
    get_all_input_data(List,Key,[Value|Acc]);
get_all_input_data([{_OtherKey,_Value}|List],Key,Acc) ->
    get_all_input_data(List,Key,Acc);
get_all_input_data([],_Key,Acc) ->
    Acc.

get_input_data(Input,Key)->
    case lists:keysearch(Key,1,parse(Input)) of
	{value,{Key,Value}} ->
	    Value;
	false ->
	    undefined
    end.

parse(Input) ->
    httpd:parse_query(Input).

vts_integer_to_list(X) when is_atom(X) ->
    atom_to_list(X);
vts_integer_to_list(X) when is_integer(X) ->
    integer_to_list(X).
