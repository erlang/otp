%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

%%% @doc Main user interface for the Common Test framework.
%%%
%%% <p> This module implements the command line interface for running
%%% tests and some basic functions for common test case issues
%%% such as configuration and logging. </p>
%%%
%%% <p><strong>Test Suite Support Macros</strong></p>
%%%
%%% <p>The <code>config</code> macro is defined in <code>ct.hrl</code>. This
%%% macro should be used to retrieve information from the
%%% <code>Config</code> variable sent to all test cases. It is used with two
%%% arguments, where the first is the name of the configuration
%%% variable you wish to retrieve, and the second is the <code>Config</code>
%%% variable supplied to the test case.</p>
%%%
%%% <p>Possible configuration variables include:</p>
%%% <ul>
%%%   <li><code>data_dir</code>  - Data file directory.</li>
%%%   <li><code>priv_dir</code>  - Scratch file directory.</li>
%%%   <li>Whatever added by <code>init_per_suite/1</code> or
%%%   <code>init_per_testcase/2</code> in the test suite.</li>
%%% </ul>

%%% @type var_name() = atom(). A variable name which is specified when
%%% <code>ct:require/2</code> is called,
%%% e.g. <code>ct:require(mynodename,{node,[telnet]})</code>
%%%
%%% @type target_name() = var_name(). The name of a target.
%%%
%%% @type handle() = ct_gen_conn:handle() | term(). The identity of a
%%% specific connection.

-module(ct).

%% Command line user interface for running tests
-export([install/1, run/1, run/2, run/3,
	 run_test/1, run_testspec/1, step/3, step/4,
	 start_interactive/0, stop_interactive/0]).

%% Test suite API
-export([require/1, require/2,
	 get_config/1, get_config/2, get_config/3,
	 log/1, log/2, log/3,
	 print/1, print/2, print/3,
	 pal/1, pal/2, pal/3,
	 fail/1, comment/1,
	 testcases/2, userdata/2, userdata/3]).

%% Other interface functions
-export([get_status/0, abort_current_testcase/1,
	 encrypt_config_file/2, encrypt_config_file/3,
	 decrypt_config_file/2, decrypt_config_file/3]).


-export([get_target_name/1]).
-export([parse_table/1, listenv/1]).

%%%-----------------------------------------------------------------
%%% @spec install(Opts) -> ok | {error,Reason}
%%%    Opts = [Opt]
%%%    Opt = {config,ConfigFiles} | {event_handler,Modules} | 
%%%          {decrypt,KeyOrFile}
%%%    ConfigFiles = [ConfigFile]
%%%    ConfigFile = string()
%%%    Modules = [atom()]
%%%    KeyOrFile = {key,Key} | {file,KeyFile}
%%%    Key = string()
%%%    KeyFile = string()
%%% @doc Install config files and event handlers.
%%%
%%% <p>Run this function once before first test.</p>
%%%
%%% <p>Example:<br/>
%%% <code>install([{config,["config_node.ctc","config_user.ctc"]}])</code>.</p>
%%%
%%% <p>Note that this function is automatically run by the
%%% <code>run_test</code> script.</p>
install(Opts) ->
    ct_run:install(Opts).

%%%-----------------------------------------------------------------
%%% @spec run(TestDir,Suite,Cases) -> Result
%%%   TestDir = string()
%%%   Suite = atom()
%%%   Cases = atom() | [atom()]
%%%   Result = [TestResult] | {error,Reason}
%%%
%%% @doc Run the given testcase(s).
%%%
%%% <p>Requires that <code>ct:install/1</code> has been run first.</p>
%%%
%%% <p>Suites (*_SUITE.erl) files must be stored in
%%% <code>TestDir</code> or <code>TestDir/test</code>.  All suites
%%% will be compiled when test is run.</p>
run(TestDir,Suite,Cases) ->
    ct_run:run(TestDir,Suite,Cases).

%%%-----------------------------------------------------------------
%%% @spec run(TestDir,Suite) -> Result
%%%
%%% @doc Run all testcases in the given suite.
%%% @see run/3.
run(TestDir,Suite) ->
    ct_run:run(TestDir,Suite).

%%%-----------------------------------------------------------------
%%% @spec run(TestDirs) -> Result
%%%   TestDirs = TestDir | [TestDir]
%%%
%%% @doc Run all testcases in all suites in the given directories.
%%% @see run/3.
run(TestDirs) ->
    ct_run:run(TestDirs).

%%%-----------------------------------------------------------------
%%% @spec run_test(Opts) -> Result
%%%   Opts = [OptTuples]
%%%   OptTuples = {config,CfgFiles} | {dir,TestDirs} | {suite,Suites} |
%%%               {userconfig, Callback, CfgFiles} |
%%%               {testcase,Cases} | {group,Groups} | {spec,TestSpecs} |
%%%               {allow_user_terms,Bool} | {logdir,LogDir} | 
%%%               {silent_connections,Conns} | {cover,CoverSpecFile} | 
%%%               {step,StepOpts} | {event_handler,EventHandlers} | {include,InclDirs} | 
%%%               {auto_compile,Bool} | {repeat,N} | {duration,DurTime} | 
%%%               {until,StopTime} | {force_stop,Bool} | {decrypt,DecryptKeyOrFile} |
%%%               {refresh_logs,LogDir} | {basic_html,Bool}
%%%   CfgFiles = [string()] | string()
%%%   TestDirs = [string()] | string()
%%%   Suites = [string()] | string()
%%%   Cases = [atom()] | atom()
%%%   Groups = [atom()] | atom()
%%%   TestSpecs = [string()] | string()
%%%   LogDir = string()
%%%   Conns = all | [atom()]
%%%   CoverSpecFile = string()
%%%   StepOpts = [StepOpt] | []
%%%   StepOpt = config | keep_inactive
%%%   EventHandlers = EH | [EH]
%%%   EH = atom() | {atom(),InitArgs} | {[atom()],InitArgs}
%%%   InitArgs = [term()]
%%%   InclDirs = [string()] | string()
%%%   N = integer()
%%%   DurTime = string(HHMMSS)
%%%   StopTime = string(YYMoMoDDHHMMSS) | string(HHMMSS)
%%%   DecryptKeyOrFile = {key,DecryptKey} | {file,DecryptFile}
%%%   DecryptKey = string()
%%%   DecryptFile = string()
%%%   Result = [TestResult] | {error,Reason}
%%% @doc Run tests as specified by the combination of options in <code>Opts</code>.
%%% The options are the same as those used with the <code>run_test</code> script.
%%% Note that here a <code>TestDir</code> can be used to point out the path to 
%%% a <code>Suite</code>. Note also that the option <code>testcase</code>
%%% corresponds to the <code>-case</code> option in the <code>run_test</code> 
%%% script. Configuration files specified in <code>Opts</code> will be 
%%% installed automatically at startup.
run_test(Opts) ->
    ct_run:run_test(Opts).

%%%-----------------------------------------------------------------
%%% @spec run_testspec(TestSpec) -> Result
%%%   TestSpec = [term()]
%%% @doc Run test specified by <code>TestSpec</code>. The terms are
%%% the same as those used in test specification files.
run_testspec(TestSpec) ->
    ct_run:run_testspec(TestSpec).
    
%%%-----------------------------------------------------------------
%%% @spec step(TestDir,Suite,Case) -> Result
%%%   Case = atom()
%%%
%%% @doc Step through a test case with the debugger.
%%% @see run/3
step(TestDir,Suite,Case) ->
    ct_run:step(TestDir,Suite,Case).

%%%-----------------------------------------------------------------
%%% @spec step(TestDir,Suite,Case,Opts) -> Result
%%%   Case = atom()
%%%   Opts = [Opt] | []
%%%   Opt = config | keep_inactive
%%%
%%% @doc Step through a test case with the debugger. If the
%%% <code>config</code> option has been given, breakpoints will
%%% be set also on the configuration functions in <code>Suite</code>.
%%% @see run/3
step(TestDir,Suite,Case,Opts) ->
    ct_run:step(TestDir,Suite,Case,Opts).

%%%-----------------------------------------------------------------
%%% @spec start_interactive() -> ok
%%%
%%% @doc Start CT in interactive mode.
%%%
%%% <p>From this mode all test case support functions can be executed
%%% directly from the erlang shell. The interactive mode can also be
%%% started from the unix command line with <code>run_test -shell
%%% [-config File...]</code>.</p>
%%%
%%% <p>If any functions using "required config data" (e.g. telnet or
%%% ftp functions) are to be called from the erlang shell, config data
%%% must first be required with <code>ct:require/2</code>.</p>
%%%
%%% <p>Example:<br/>
%%% <code>&gt; ct:require(unix_telnet, unix).</code><br/>
%%% <code>ok</code><br/>
%%% <code>&gt; ct_telnet:open(unix_telnet).</code><br/>
%%% <code>{ok,&lt;0.105.0&gt;}</code><br/>
%%% <code>&gt; ct_telnet:cmd(unix_telnet, "ls .").</code><br/>
%%% <code>{ok,["ls","file1  ...",...]}</code></p>
start_interactive() ->
    ct_util:start(interactive).

%%%-----------------------------------------------------------------
%%% @spec stop_interactive() -> ok
%%%
%%% @doc Exit the interactive mode.
%%% @see start_interactive/0
stop_interactive() ->
    ct_util:stop(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MISC INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------
%%% @spec require(Required) -> ok | {error,Reason}
%%%      Required = Key | {Key,SubKeys}
%%%      Key = atom()
%%%      SubKeys = SubKey | [SubKey]
%%%      SubKey = atom()
%%%
%%% @doc Check if the required configuration is available.
%%%
%%% <p>Example: require the variable <code>myvar</code>:<br/>
%%% <code>ok = ct:require(myvar)</code></p>
%%%
%%% <p>In this case the config file must at least contain:</p>
%%% <pre>
%%% {myvar,Value}.</pre>
%%% 
%%% <p>Example: require the variable <code>myvar</code> with
%%% subvariable <code>sub1</code>:<br/>
%%% <code>ok = ct:require({myvar,sub1})</code></p>
%%%
%%% <p>In this case the config file must at least contain:</p>
%%% <pre>
%%% {myvar,[{sub1,Value}]}.</pre>
%%%
%%% @see require/2
%%% @see get_config/1
%%% @see get_config/2
%%% @see get_config/3
require(Required) ->
    ct_config:require(Required).

%%%-----------------------------------------------------------------
%%% @spec require(Name,Required) -> ok | {error,Reason}
%%%      Name = atom()
%%%      Required = Key | {Key,SubKeys}
%%%      Key = atom()
%%%      SubKeys = SubKey | [SubKey]
%%%      SubKey = atom()
%%%
%%% @doc Check if the required configuration is available, and give it
%%% a name.
%%%
%%% <p>If the requested data is available, the main entry will be
%%% associated with <code>Name</code> so that the value of the element
%%% can be read with <code>get_config/1,2</code> provided
%%% <code>Name</code> instead of the <code>Key</code>.</p>
%%%
%%% <p>Example: Require one node with a telnet connection and an
%%% ftp connection. Name the node <code>a</code>:<br/> <code>ok =
%%% ct:require(a,{node,[telnet,ftp]}).</code><br/> All references
%%% to this node may then use the node name. E.g. you can fetch a
%%% file over ftp like this:<br/>
%%% <code>ok = ct:ftp_get(a,RemoteFile,LocalFile).</code></p>
%%%
%%% <p>For this to work, the config file must at least contain:</p>
%%% <pre>
%%% {node,[{telnet,IpAddr},
%%%        {ftp,IpAddr}]}.</pre>
%%%
%%% @see require/1
%%% @see get_config/1
%%% @see get_config/2
%%% @see get_config/3
require(Name,Required) ->
    ct_config:require(Name,Required).

%%%-----------------------------------------------------------------
%%% @spec get_config(Required) -> Value
%%% @equiv get_config(Required,undefined,[])
get_config(Required) ->
    ct_config:get_config(Required,undefined,[]).

%%%-----------------------------------------------------------------
%%% @spec get_config(Required,Default) -> Value
%%% @equiv get_config(Required,Default,[])
get_config(Required,Default) ->
    ct_config:get_config(Required,Default,[]).

%%%-----------------------------------------------------------------
%%% @spec get_config(Required,Default,Opts) -> ValueOrElement
%%%      Required = KeyOrName | {KeyOrName,SubKey}
%%%      KeyOrName = atom()
%%%      SubKey = atom()
%%%      Default = term()
%%%      Opts = [Opt] | []
%%%      Opt = element | all
%%%      ValueOrElement = term() | Default
%%%
%%% @doc Read config data values.
%%%
%%% <p>This function returns the matching value(s) or config element(s),
%%% given a config variable key or its associated name
%%% (if one has been specified with <code>require/2</code> or a 
%%% require statement).</p>
%%%
%%% <p>Example, given the following config file:</p>
%%% <pre>
%%% {unix,[{telnet,IpAddr},
%%%        {username,Username},
%%%        {password,Password}]}.</pre>
%%% <p><code>get_config(unix,Default) -> 
%%%                          [{telnet,IpAddr},
%%%                           {username,Username},
%%%                           {password,Password}]</code><br/>
%%% <code>get_config({unix,telnet},Default) -> IpAddr</code><br/>
%%% <code>get_config({unix,ftp},Default) -> Default</code><br/>
%%% <code>get_config(unknownkey,Default) -> Default</code></p>
%%%
%%% <p>If a config variable key has been associated with a name (by
%%% means of <code>require/2</code> or a require statement), the name 
%%% may be used instead of the key to read the value:</p>
%%%
%%% <p><code>require(myhost,unix) -> ok</code><br/>
%%% <code>get_config(myhost,Default) -> 
%%%                          [{telnet,IpAddr},
%%%                           {username,Username},
%%%                           {password,Password}]</code></p>
%%%
%%% <p>If a config variable is defined in multiple files and you want to
%%% access all possible values, use the <code>all</code> option. The
%%% values will be returned in a list and the order of the elements
%%% corresponds to the order that the config files were specified at 
%%% startup.</p>
%%%
%%% <p>If you want config elements (key-value tuples) returned as result 
%%% instead of values, use the <code>element</code> option. 
%%% The returned elements will then be on the form <code>{KeyOrName,Value}</code>, 
%%% or (in case a subkey has been specified)
%%% <code>{{KeyOrName,SubKey},Value}</code></p>
%%%
%%% @see get_config/1
%%% @see get_config/2
%%% @see require/1
%%% @see require/2
get_config(Required,Default,Opts) ->
    ct_config:get_config(Required,Default,Opts).

%%%-----------------------------------------------------------------
%%% @spec log(Format) -> ok
%%% @equiv log(default,Format,[])
log(Format) ->
    log(default,Format,[]).

%%%-----------------------------------------------------------------
%%% @spec log(X1,X2) -> ok
%%%      X1 = Category | Format
%%%      X2 = Format | Args
%%% @equiv log(Category,Format,Args)
log(X1,X2) ->
    {Category,Format,Args} = 
	if is_atom(X1) -> {X1,X2,[]};
	   is_list(X1) -> {default,X1,X2}
	end,
    log(Category,Format,Args).

%%%-----------------------------------------------------------------
%%% @spec log(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Printout from a testcase to the log. 
%%%
%%% <p>This function is meant for printing stuff directly from a
%%% testcase (i.e. not from within the CT framework) in the test
%%% log.</p>
%%%
%%% <p>Default <code>Category</code> is <code>default</code> and
%%% default <code>Args</code> is <code>[]</code>.</p>
log(Category,Format,Args) ->
    ct_logs:tc_log(Category,Format,Args).


%%%-----------------------------------------------------------------
%%% @spec print(Format) -> ok
%%% @equiv print(default,Format,[])
print(Format) ->
    print(default,Format,[]).

%%%-----------------------------------------------------------------
%%% @equiv print(Category,Format,Args)
print(X1,X2) ->
    {Category,Format,Args} = 
	if is_atom(X1) -> {X1,X2,[]};
	   is_list(X1) -> {default,X1,X2}
	end,
    print(Category,Format,Args).

%%%-----------------------------------------------------------------
%%% @spec print(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Printout from a testcase to the console. 
%%%
%%% <p>This function is meant for printing stuff from a testcase on
%%% the console.</p>
%%%
%%% <p>Default <code>Category</code> is <code>default</code> and
%%% default <code>Args</code> is <code>[]</code>.</p>
print(Category,Format,Args) ->
    ct_logs:tc_print(Category,Format,Args).


%%%-----------------------------------------------------------------
%%% @spec pal(Format) -> ok
%%% @equiv pal(default,Format,[])
pal(Format) ->
    pal(default,Format,[]).

%%%-----------------------------------------------------------------
%%% @spec pal(X1,X2) -> ok
%%%      X1 = Category | Format
%%%      X2 = Format | Args
%%% @equiv pal(Category,Format,Args)
pal(X1,X2) ->
    {Category,Format,Args} = 
	if is_atom(X1) -> {X1,X2,[]};
	   is_list(X1) -> {default,X1,X2}
	end,
    pal(Category,Format,Args).

%%%-----------------------------------------------------------------
%%% @spec pal(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Print and log from a testcase. 
%%%
%%% <p>This function is meant for printing stuff from a testcase both
%%% in the log and on the console.</p>
%%%
%%% <p>Default <code>Category</code> is <code>default</code> and
%%% default <code>Args</code> is <code>[]</code>.</p>
pal(Category,Format,Args) ->
    ct_logs:tc_pal(Category,Format,Args).


%%%-----------------------------------------------------------------
%%% @spec fail(Reason) -> void()
%%%      Reason = term()
%%%
%%% @doc Terminate a test case with the given error
%%% <code>Reason</code>.
fail(Reason) ->
    exit({test_case_failed,Reason}).

%%%-----------------------------------------------------------------
%%% @spec comment(Comment) -> void()
%%%      Comment = term()
%%%
%%% @doc Print the given <code>Comment</code> in the comment field of
%%% the table on the test suite result page.
%%%
%%% <p>If called several times, only the last comment is printed.
%%% <code>comment/1</code> is also overwritten by the return value
%%% <code>{comment,Comment}</code> or by the function
%%% <code>fail/1</code> (which prints <code>Reason</code> as a
%%% comment).</p>
comment(Comment) when is_list(Comment) ->
    Formatted =
	case (catch io_lib:format("~s",[Comment])) of
	    {'EXIT',_} ->  % it's a list not a string
		io_lib:format("~p",[Comment]);
	    String ->
		String
	end,
    send_html_comment(lists:flatten(Formatted));
comment(Comment) ->
    Formatted = io_lib:format("~p",[Comment]),
    send_html_comment(lists:flatten(Formatted)).

send_html_comment(Comment) ->
    Html = "<font color=\"green\">" ++ Comment ++ "</font>",
    ct_util:set_testdata({comment,Html}),
    test_server:comment(Html).


%%%-----------------------------------------------------------------
%%% @spec get_target_name(Handle) -> {ok,TargetName} | {error,Reason}
%%%      Handle = handle()
%%%      TargetName = target_name()
%%% @doc Return the name of the target that the given connection
%%% belongs to.
get_target_name(Handle) ->
    ct_util:get_target_name(Handle).
    
%%%-----------------------------------------------------------------
%%% @spec parse_table(Data) -> {Heading,Table}
%%%       Data = [string()]
%%%       Heading = tuple()
%%%       Table = [tuple()]
%%%
%%% @doc Parse the printout from an SQL table and return a list of tuples.
%%%
%%% <p>The printout to parse would typically be the result of a
%%% <code>select</code> command in SQL. The returned
%%% <code>Table</code> is a list of tuples, where each tuple is a row
%%% in the table.</p>
%%% 
%%% <p><code>Heading</code> is a tuple of strings representing the
%%% headings of each column in the table.</p>
parse_table(Data) ->
    ct_util:parse_table(Data).

%%%-----------------------------------------------------------------
%%% @spec listenv(Telnet) -> [Env]
%%%       Telnet = term()
%%%       Env = {Key,Value}
%%%       Key = string()
%%%       Value = string()
%%%
%%% @doc Performs the listenv command on the given telnet connection
%%% and returns the result as a list of Key-Value pairs.
listenv(Telnet) ->
    ct_util:listenv(Telnet).


%%%-----------------------------------------------------------------
%%% @spec testcases(TestDir, Suite) -> Testcases | {error,Reason}
%%%       TestDir = string()
%%%       Suite = atom()
%%%       Testcases = list()
%%%       Reason = term()
%%%
%%% @doc Returns all testcases in the specified suite.
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
    EnvInclude =
	case os:getenv("CT_INCLUDE_PATH") of
	    false -> [];
	    CtInclPath -> string:tokens(CtInclPath, [$:,$ ,$,])
	end,
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

%%%-----------------------------------------------------------------
%%% @spec userdata(TestDir, Suite) -> SuiteUserData | {error,Reason}
%%%       TestDir = string()
%%%       Suite = atom()
%%%       SuiteUserData = [term()]
%%%       Reason = term()
%%%
%%% @doc Returns any data specified with the tag <code>userdata</code> 
%%% in the list of tuples returned from <code>Suite:suite/0</code>.
userdata(TestDir, Suite) ->
    case make_and_load(TestDir, Suite) of
	E = {error,_} ->
	    E;
	_ ->
	    Info = (catch Suite:suite()),
	    get_userdata(Info, "suite/0")
    end.

get_userdata({'EXIT',{undef,_}}, Spec) ->
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
   
%%%-----------------------------------------------------------------
%%% @spec userdata(TestDir, Suite, Case) -> TCUserData | {error,Reason}
%%%       TestDir = string()
%%%       Suite = atom()
%%%       Case = atom()
%%%       TCUserData = [term()]
%%%       Reason = term()
%%%
%%% @doc Returns any data specified with the tag <code>userdata</code>
%%% in the list of tuples returned from <code>Suite:Case/0</code>.
userdata(TestDir, Suite, Case) ->
    case make_and_load(TestDir, Suite) of
	E = {error,_} ->
	    E;
	_ ->
	    Info = (catch apply(Suite, Case, [])),
	    get_userdata(Info, atom_to_list(Case)++"/0")
    end.


%%%-----------------------------------------------------------------
%%% @spec get_status() -> TestStatus | {error,Reason}
%%%       TestStatus = [StatusElem]
%%%       StatusElem = {current,{Suite,TestCase}} | {successful,Successful} |
%%%                    {failed,Failed} | {skipped,Skipped} | {total,Total}
%%%       Suite = atom()
%%%       TestCase = atom()
%%%       Successful = integer()
%%%       Failed = integer()
%%%       Skipped = {UserSkipped,AutoSkipped}
%%%       UserSkipped = integer()
%%%       AutoSkipped = integer()
%%%       Total = integer()
%%%       Reason = term()
%%%
%%% @doc Returns status of ongoing test. The returned list contains info about
%%%      which test case is currently executing, as well as counters for 
%%%      successful, failed, skipped, and total test cases so far.
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
	Error = {error,_Reason} ->
	    Error;
	{'EXIT',_Reason} ->
	    no_tests_running;
	Data ->
	    {ok,Data}
    end.

%%%-----------------------------------------------------------------
%%% @spec abort_current_testcase(Reason) -> ok | {error,no_testcase_running}
%%%       Reason = term()
%%%
%%% @doc <p>When calling this function, the currently executing test case will be aborted.
%%%      It is the user's responsibility to know for sure which test case is currently
%%%	 executing. The function is therefore only safe to call from a function which
%%%	 has been called (or synchronously invoked) by the test case.</p>
%%%
%%%      <p><code>Reason</code>, the reason for aborting the test case, is printed
%%%      in the test case log.</p>
abort_current_testcase(Reason) ->
    test_server_ctrl:abort_current_testcase(Reason).

%%%-----------------------------------------------------------------
%%% @spec encrypt_config_file(SrcFileName, EncryptFileName) -> 
%%%                                                  ok | {error,Reason}
%%%       SrcFileName = string()
%%%       EncryptFileName = string()
%%%       Reason = term()
%%%
%%% @doc <p>This function encrypts the source config file with DES3 and 
%%%      saves the result in file <code>EncryptFileName</code>. The key,
%%%      a string, must be available in a text file named 
%%%      <code>.ct_config.crypt</code> in the current directory, or the 
%%%      home directory of the user (it is searched for in that order).</p>
%%%      <p>See the Common Test User's Guide for information about using
%%%      encrypted config files when running tests.</p>
%%%      <p>See the <code>crypto</code> application for details on DES3
%%%      encryption/decryption.</p>
encrypt_config_file(SrcFileName, EncryptFileName) ->
    ct_config:encrypt_config_file(SrcFileName, EncryptFileName).

%%%-----------------------------------------------------------------
%%% @spec encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile) -> 
%%%                                                  ok | {error,Reason}
%%%       SrcFileName = string()
%%%       EncryptFileName = string()
%%%       KeyOrFile = {key,string()} | {file,string()}
%%%       Reason = term()
%%%
%%% @doc <p>This function encrypts the source config file with DES3 and
%%%      saves the result in the target file <code>EncryptFileName</code>. 
%%%      The encryption key to use is either the value in 
%%%      <code>{key,Key}</code> or the value stored in the file specified 
%%%      by <code>{file,File}</code>.</p>
%%%      <p>See the Common Test User's Guide for information about using
%%%      encrypted config files when running tests.</p>
%%%      <p>See the <code>crypto</code> application for details on DES3
%%%      encryption/decryption.</p>
encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile) ->
    ct_config:encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile).

%%%-----------------------------------------------------------------
%%% @spec decrypt_config_file(EncryptFileName, TargetFileName) -> 
%%%                                                   ok | {error,Reason}
%%%       EncryptFileName = string()
%%%       TargetFileName = string()
%%%       Reason = term()
%%%
%%% @doc <p>This function decrypts <code>EncryptFileName</code>, previously 
%%%      generated with <code>encrypt_config_file/2/3</code>. The original
%%%      file contents is saved in the target file. The encryption key, a
%%%      string, must be available in a text file named
%%%      <code>.ct_config.crypt</code> in the current directory, or the
%%%      home directory of the user (it is searched for in that order).</p>
decrypt_config_file(EncryptFileName, TargetFileName) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName).

%%%-----------------------------------------------------------------
%%% @spec decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile) -> 
%%%                                                   ok | {error,Reason}
%%%       EncryptFileName = string()
%%%       TargetFileName = string()
%%%       KeyOrFile = {key,string()} | {file,string()}
%%%       Reason = term()
%%%
%%% @doc <p>This function decrypts <code>EncryptFileName</code>, previously 
%%%      generated with <code>encrypt_config_file/2/3</code>. The original
%%%      file contents is saved in the target file. The key must have the
%%%      the same value as that used for encryption.</p>
decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile).

