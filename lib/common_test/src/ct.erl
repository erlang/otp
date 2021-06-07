%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

-module(ct).

-include("ct.hrl").
-include("ct_util.hrl").

%% Command line user interface for running tests
-export([install/1, run/1, run/2, run/3,
	 run_test/1, run_testspec/1, step/3, step/4,
	 start_interactive/0, stop_interactive/0]).

%% Test suite API
-export([require/1, require/2,
	 get_config/1, get_config/2, get_config/3,
	 reload_config/1,
	 escape_chars/1, escape_chars/2,
	 log/1, log/2, log/3, log/4, log/5,
	 print/1, print/2, print/3, print/4, print/5,
	 pal/1, pal/2, pal/3, pal/4, pal/5,
         set_verbosity/2, get_verbosity/1,
	 capture_start/0, capture_stop/0, capture_get/0, capture_get/1,
	 fail/1, fail/2, comment/1, comment/2, make_priv_dir/0,
	 testcases/2, userdata/2, userdata/3,
	 timetrap/1, get_timetrap_info/0, sleep/1,
	 notify/2, sync_notify/2,
	 break/1, break/2, continue/0, continue/1]).

%% New API for manipulating with config handlers
-export([add_config/2, remove_config/2]).

%% Other interface functions
-export([get_status/0, abort_current_testcase/1,
	 get_event_mgr_ref/0,
	 get_testspec_terms/0, get_testspec_terms/1,
	 encrypt_config_file/2, encrypt_config_file/3,
	 decrypt_config_file/2, decrypt_config_file/3]).

-export([get_target_name/1]).
-export([get_progname/0]).
-export([parse_table/1, listenv/1]).

-export([remaining_test_procs/0]).

%%----------------------------------------------------------------------
%% Exported types
%%----------------------------------------------------------------------
%% For ct_gen_conn
-export_type([config_key/0,
	      target_name/0,
	      key_or_name/0]).

%% For cth_conn_log
-export_type([conn_log_options/0,
	      conn_log_type/0,
	      conn_log_mod/0]).

%%------------------------------------------------------------------
%% Type declarations
%% ------------------------------------------------------------------
-type config_key() :: atom(). % Config key which exists in a config file
-type target_name() :: atom().% Name associated to a config_key() though 'require'
-type key_or_name() :: config_key() | target_name().

%% Types used when logging connections with the 'cth_conn_log' hook
-type conn_log_options() :: [conn_log_option()].
-type conn_log_option() :: {log_type,conn_log_type()} |
                           {hosts,[key_or_name()]}.
-type conn_log_type() :: raw | pretty | html | silent.
-type conn_log_mod() :: ct_netconfc | ct_telnet.
%%----------------------------------------------------------------------


install(Opts) ->
    ct_run:install(Opts).

run(TestDir,Suite,Cases) ->
    ct_run:run(TestDir,Suite,Cases).

run(TestDir,Suite) ->
    ct_run:run(TestDir,Suite).

run(TestDirs) ->
    ct_run:run(TestDirs).

run_test(Opts) ->
    ct_run:run_test(Opts).

run_testspec(TestSpec) ->
    ct_run:run_testspec(TestSpec).
    
step(TestDir,Suite,Case) ->
    ct_run:step(TestDir,Suite,Case).

step(TestDir,Suite,Case,Opts) ->
    ct_run:step(TestDir,Suite,Case,Opts).

start_interactive() ->
    _ = ct_util:start(interactive),
    ok.

stop_interactive() ->
    ct_util:stop(normal),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MISC INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require(Required) ->
    ct_config:require(Required).

require(Name,Required) ->
    ct_config:require(Name,Required).

get_config(Required) ->
    ct_config:get_config(Required,undefined,[]).

get_config(Required,Default) ->
    ct_config:get_config(Required,Default,[]).

get_config(Required,Default,Opts) ->
    ct_config:get_config(Required,Default,Opts).

reload_config(Required)->
    ct_config:reload_config(Required).

get_testspec_terms() ->
    case ct_util:get_testdata(testspec) of
	undefined ->
	    undefined;
	CurrSpecRec ->
	    ct_testspec:testspec_rec2list(CurrSpecRec)
    end.

get_testspec_terms(Tags) ->
    case ct_util:get_testdata(testspec) of
	undefined ->
	    undefined;
	CurrSpecRec ->
	    ct_testspec:testspec_rec2list(Tags, CurrSpecRec)
    end.

escape_chars(IoList) ->
    ct_logs:escape_chars(IoList).

escape_chars(Format, Args) ->
    try io_lib:format(Format, Args) of
	IoList ->
	    ct_logs:escape_chars(IoList)
    catch
	_:Reason ->
	    {error,Reason}
    end.

log(Format) ->
    log(default,?STD_IMPORTANCE,Format,[],[]).

log(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    log(Category,Importance,Format,Args,[]).

log(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    log(Category,Importance,Format,Args,Opts).

log(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    log(Category,Importance,Format,Args,Opts).

log(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_log(Category,Importance,Format,Args,Opts).

print(Format) ->
    print(default,?STD_IMPORTANCE,Format,[],[]).

print(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    print(Category,Importance,Format,Args,[]).

print(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    print(Category,Importance,Format,Args,Opts).

print(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    print(Category,Importance,Format,Args,Opts).

print(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_print(Category,Importance,Format,Args,Opts).

pal(Format) ->
    pal(default,?STD_IMPORTANCE,Format,[]).

pal(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    pal(Category,Importance,Format,Args,[]).

pal(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    pal(Category,Importance,Format,Args,Opts).

pal(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    pal(Category,Importance,Format,Args,Opts).

pal(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_pal(Category,Importance,Format,Args,Opts).

set_verbosity(Category, Level) ->
    ct_util:set_verbosity({Category,Level}).

get_verbosity(Category) ->
    ct_util:get_verbosity(Category).

capture_start() ->
    test_server:capture_start().

capture_stop() ->
    test_server:capture_stop().

capture_get() ->
    %% remove default log printouts (e.g. ct:log/2 printouts)
    capture_get([default]).

capture_get([ExclCat | ExclCategories]) ->
    Strs = test_server:capture_get(),
    CatsStr = [atom_to_list(ExclCat) | 
	       [[$| | atom_to_list(EC)] || EC <- ExclCategories]],
    {ok,MP} = re:compile("<div class=\"(" ++ lists:flatten(CatsStr) ++ ")\">.*",
                         [unicode]),
    lists:flatmap(fun(Str) ->
			  case re:run(Str, MP) of
			      {match,_} -> [];
			      nomatch -> [Str]
			  end
		  end, Strs);

capture_get([]) ->
    test_server:capture_get().

fail(Reason) ->
    try
	exit({test_case_failed,Reason})
    catch
	Class:R:S ->
	    case S of
		[{?MODULE,fail,1,_}|Stk] -> ok;
		Stk -> ok
	    end,
	    erlang:raise(Class, R, Stk)
    end.

fail(Format, Args) ->
    try io_lib:format(Format, Args) of
	Str ->
	    try
		exit({test_case_failed,lists:flatten(Str)})
	    catch
		Class:R:S ->
		    case S of
			[{?MODULE,fail,2,_}|Stk] -> ok;
			Stk -> ok
		    end,
		    erlang:raise(Class, R, Stk)
	    end
    catch
	_:BadArgs ->
	    exit({BadArgs,{?MODULE,fail,[Format,Args]}})
    end.

comment(Comment) when is_list(Comment) ->
    Formatted =
	case (catch io_lib:format("~ts",[Comment])) of
	    {'EXIT',_} ->  % it's a list not a string
		io_lib:format("~tp",[Comment]);
	    String ->
		String
	end,
    send_html_comment(lists:flatten(Formatted));
comment(Comment) ->
    Formatted = io_lib:format("~tp",[Comment]),
    send_html_comment(lists:flatten(Formatted)).

comment(Format, Args) when is_list(Format), is_list(Args) ->
    Formatted =
	case (catch io_lib:format(Format, Args)) of
	    {'EXIT',Reason} ->  % bad args
		exit({Reason,{?MODULE,comment,[Format,Args]}});
	    String ->
		lists:flatten(String)
	end,
    send_html_comment(Formatted).

send_html_comment(Comment) ->
    Html = "<font color=\"green\">" ++ Comment ++ "</font>",
    ct_util:set_testdata({{comment,group_leader()},Html}),
    test_server:comment(Html).

make_priv_dir() ->
    test_server:make_priv_dir().

get_target_name(Handle) ->
    ct_util:get_target_name(Handle).

-spec get_progname() -> string().

get_progname() ->
    case init:get_argument(progname) of
	{ok, [[Prog]]} ->
	    Prog;
	_Other ->
	    "no_prog_name"
    end.

parse_table(Data) ->
    ct_util:parse_table(Data).

listenv(Telnet) ->
    ct_util:listenv(Telnet).

testcases(TestDir, Suite) ->
    case make_and_load(TestDir, Suite) of
	E = {error,_} ->
	    E;
	_ ->
	    case (catch Suite:all()) of
		{'EXIT',Reason} ->
		    {error,Reason};
		TCs ->
		    TCs
	    end
    end.

make_and_load(Dir, Suite) ->
    EnvInclude = string:lexemes(os:getenv("CT_INCLUDE_PATH", ""), [$:,$ ,$,]),
    StartInclude =
	case init:get_argument(include) of
	    {ok,[Dirs]} -> Dirs;
	    _ -> []
	end,
    UserInclude = EnvInclude ++ StartInclude,
    case ct_run:run_make(Dir, Suite, UserInclude) of
	MErr = {error,_} ->
	    MErr;
	_ ->
	    TestDir = ct_util:get_testdir(Dir, Suite),
	    File = filename:join(TestDir, atom_to_list(Suite)),
	    case code:soft_purge(Suite) of
		true ->
		    code:load_abs(File);
		false ->			% will use loaded
		    {module,Suite}
	    end
    end.

userdata(TestDir, Suite) ->
    case make_and_load(TestDir, Suite) of
	E = {error,_} ->
	    E;
	_ ->
	    Info = (catch Suite:suite()),
	    get_userdata(Info, "suite/0")
    end.

get_userdata({'EXIT',{Undef,_}}, Spec) when Undef == undef;
					     Undef == function_clause ->
    {error,list_to_atom(Spec ++ " is not defined")};
get_userdata({'EXIT',Reason}, Spec) ->
    {error,{list_to_atom("error in " ++ Spec),Reason}};
get_userdata(List, _) when is_list(List) ->
    Fun = fun({userdata,Data}, Acc) -> [Data | Acc];
	     (_, Acc) -> Acc
	  end,
    case lists:foldl(Fun, [], List) of
	Terms ->
	    lists:flatten(lists:reverse(Terms))
    end;
get_userdata(_BadTerm, Spec) ->
    {error,list_to_atom(Spec ++ " must return a list")}.
   
userdata(TestDir, Suite, {group,GroupName}) ->
    case make_and_load(TestDir, Suite) of
	E = {error,_} ->
	    E;
	_ ->
	    Info = (catch apply(Suite, group, [GroupName])),
	    get_userdata(Info, "group("++atom_to_list(GroupName)++")")
    end;

userdata(TestDir, Suite, Case) when is_atom(Case) ->
    case make_and_load(TestDir, Suite) of
	E = {error,_} ->
	    E;
	_ ->
	    Info = (catch apply(Suite, Case, [])),
	    get_userdata(Info, atom_to_list(Case)++"/0")
    end.

get_status() ->
    case get_testdata(curr_tc) of
	{ok,TestCase} ->
	    case get_testdata(stats) of
		{ok,{Ok,Failed,Skipped={UserSkipped,AutoSkipped}}} ->
		    [{current,TestCase},
		     {successful,Ok},
		     {failed,Failed},
		     {skipped,Skipped},
		     {total,Ok+Failed+UserSkipped+AutoSkipped}];
		Err1 -> Err1
	    end;
	Err2 -> Err2
    end.
	    
get_testdata(Key) ->
    case catch ct_util:get_testdata(Key) of
	{error,ct_util_server_not_running} ->
	    no_tests_running;
	Error = {error,_Reason} ->
	    Error;
	{'EXIT',_Reason} ->
	    no_tests_running;
	undefined ->
	    {error,no_testdata};
	[CurrTC] when Key == curr_tc ->
	    {ok,CurrTC};
	Data ->
	    {ok,Data}
    end.

abort_current_testcase(Reason) ->
    test_server_ctrl:abort_current_testcase(Reason).

get_event_mgr_ref() ->
    ?CT_EVMGR_REF.

encrypt_config_file(SrcFileName, EncryptFileName) ->
    ct_config:encrypt_config_file(SrcFileName, EncryptFileName).

encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile) ->
    ct_config:encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile).

decrypt_config_file(EncryptFileName, TargetFileName) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName).

decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile).

add_config(Callback, Config)->
    ct_config:add_config(Callback, Config).

remove_config(Callback, Config) ->
    ct_config:remove_config(Callback, Config).

timetrap(Time) ->
    test_server:timetrap_cancel(),
    test_server:timetrap(Time).

get_timetrap_info() ->
    test_server:get_timetrap_info().

sleep({hours,Hs}) ->
    sleep(trunc(Hs * 1000 * 60 * 60));
sleep({minutes,Ms}) ->
    sleep(trunc(Ms * 1000 * 60));
sleep({seconds,Ss}) ->
    sleep(trunc(Ss * 1000));
sleep(Time) ->
    test_server:adjusted_sleep(Time).

notify(Name,Data) ->
    ct_event:notify(Name, Data).

sync_notify(Name,Data) ->
    ct_event:sync_notify(Name, Data).

break(Comment) ->
    case {ct_util:get_testdata(starter),
	  ct_util:get_testdata(release_shell)} of
	{ct,ReleaseSh} when ReleaseSh /= true ->
	    Warning = "ct:break/1 can only be used if release_shell == true.\n",
	    ct_logs:log("Warning!", Warning, []),
	    io:format(user, "Warning! " ++ Warning, []),
	    {error,'enable break with release_shell option'};
	_ ->
	    case get_testdata(curr_tc) of
		{ok,{_,_TestCase}} ->
		    test_server:break(?MODULE, Comment);
		{ok,Cases} when is_list(Cases) ->
		    {error,{'multiple cases running',
			    [TC || {_,TC} <- Cases]}};
		Error = {error,_} -> 
		    Error;
		Error ->
		    {error,Error}
	    end
    end.

break(TestCase, Comment) ->
    case {ct_util:get_testdata(starter),
	  ct_util:get_testdata(release_shell)} of
	{ct,ReleaseSh} when ReleaseSh /= true ->
	    Warning = "ct:break/2 can only be used if release_shell == true.\n",
	    ct_logs:log("Warning!", Warning, []),
	    io:format(user, "Warning! " ++ Warning, []),
	    {error,'enable break with release_shell option'};
	_ ->
	    case get_testdata(curr_tc) of
		{ok,Cases} when is_list(Cases) ->
		    case lists:keymember(TestCase, 2, Cases) of
			true ->
			    test_server:break(?MODULE, TestCase, Comment);
			false ->
			    {error,'test case not running'}
		    end;
		{ok,{_,TestCase}} ->
		    test_server:break(?MODULE, TestCase, Comment);
		Error = {error,_} -> 
		    Error;
		Error ->
		    {error,Error}
	    end
    end.

continue() -> 
    test_server:continue().

continue(TestCase) -> 
    test_server:continue(TestCase).


remaining_test_procs() ->
    ct_util:remaining_test_procs().
