%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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

%%% @doc Main user interface for the Common Test framework.
%%%
%%% <p> This module implements the command line interface for running
%%% tests and some basic functions for common test case issues
%%% such as configuration and logging. </p>
%%%
%%% <p><strong>Test Suite Support Macros</strong></p>
%%%
%%% <p>The <c>config</c> macro is defined in <c>ct.hrl</c>. This
%%% macro should be used to retrieve information from the
%%% <c>Config</c> variable sent to all test cases. It is used with two
%%% arguments, where the first is the name of the configuration
%%% variable you wish to retrieve, and the second is the <c>Config</c>
%%% variable supplied to the test case.</p>
%%%
%%% <p>Possible configuration variables include:</p>
%%% <ul>
%%%   <li><c>data_dir</c>  - Data file directory.</li>
%%%   <li><c>priv_dir</c>  - Scratch file directory.</li>
%%%   <li>Whatever added by <c>init_per_suite/1</c> or
%%%   <c>init_per_testcase/2</c> in the test suite.</li>
%%% </ul>

%%% @type var_name() = atom(). A variable name which is specified when
%%% <c>ct:require/2</c> is called,
%%% e.g. <c>ct:require(mynodename,{node,[telnet]})</c>
%%%
%%% @type target_name() = var_name(). The name of a target.
%%%
%%% @type handle() = ct_gen_conn:handle() | term(). The identity of a
%%% specific connection.

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
%%% <c>install([{config,["config_node.ctc","config_user.ctc"]}])</c>.</p>
%%%
%%% <p>Note that this function is automatically run by the
%%% <c>ct_run</c> program.</p>
install(Opts) ->
    ct_run:install(Opts).

%%%-----------------------------------------------------------------
%%% @spec run(TestDir,Suite,Cases) -> Result
%%%   TestDir = string()
%%%   Suite = atom()
%%%   Cases = atom() | [atom()]
%%%   Result = [TestResult] | {error,Reason}
%%%
%%% @doc Run the given test case(s).
%%%
%%% <p>Requires that <c>ct:install/1</c> has been run first.</p>
%%%
%%% <p>Suites (*_SUITE.erl) files must be stored in
%%% <c>TestDir</c> or <c>TestDir/test</c>.  All suites
%%% will be compiled when test is run.</p>
run(TestDir,Suite,Cases) ->
    ct_run:run(TestDir,Suite,Cases).

%%%-----------------------------------------------------------------
%%% @spec run(TestDir,Suite) -> Result
%%%
%%% @doc Run all test cases in the given suite.
%%% @see run/3.
run(TestDir,Suite) ->
    ct_run:run(TestDir,Suite).

%%%-----------------------------------------------------------------
%%% @spec run(TestDirs) -> Result
%%%   TestDirs = TestDir | [TestDir]
%%%
%%% @doc Run all test cases in all suites in the given directories.
%%% @see run/3.
run(TestDirs) ->
    ct_run:run(TestDirs).

%%%-----------------------------------------------------------------
%%% @spec run_test(Opts) -> Result
%%%   Opts = [OptTuples]
%%%   OptTuples = {dir,TestDirs} | {suite,Suites} | {group,Groups} |
%%%               {testcase,Cases} | {spec,TestSpecs} | {join_specs,Bool} |
%%%               {label,Label} | {config,CfgFiles} | {userconfig, UserConfig} |
%%%               {allow_user_terms,Bool} | {logdir,LogDir} |
%%%               {silent_connections,Conns} | {stylesheet,CSSFile} |
%%%               {cover,CoverSpecFile} | {cover_stop,Bool} | {step,StepOpts} |
%%%               {event_handler,EventHandlers} | {include,InclDirs} |
%%%               {auto_compile,Bool} | {abort_if_missing_suites,Bool} |
%%%               {create_priv_dir,CreatePrivDir}  |
%%%               {multiply_timetraps,M} | {scale_timetraps,Bool} |
%%%               {repeat,N} | {duration,DurTime} | {until,StopTime} |
%%%               {force_stop,ForceStop} | {decrypt,DecryptKeyOrFile} |
%%%               {refresh_logs,LogDir} | {logopts,LogOpts} | 
%%%               {verbosity,VLevels} | {basic_html,Bool} |
%%%               {esc_chars,Bool} | {ct_hooks, CTHs} |
%%%               {enable_builtin_hooks,Bool} | {release_shell,Bool}
%%%   TestDirs = [string()] | string()
%%%   Suites = [string()] | [atom()] | string() | atom()
%%%   Cases = [atom()] | atom()
%%%   Groups = GroupNameOrPath | [GroupNameOrPath]
%%%   GroupNameOrPath = [atom()] | atom() | all
%%%   TestSpecs = [string()] | string()
%%%   Label = string() | atom()
%%%   CfgFiles = [string()] | string()
%%%   UserConfig = [{CallbackMod,CfgStrings}] | {CallbackMod,CfgStrings}
%%%   CallbackMod = atom()
%%%   CfgStrings = [string()] | string()
%%%   LogDir = string()
%%%   Conns = all | [atom()]
%%%   CSSFile = string()
%%%   CoverSpecFile = string()
%%%   StepOpts = [StepOpt] | []
%%%   StepOpt = config | keep_inactive
%%%   EventHandlers = EH | [EH]
%%%   EH = atom() | {atom(),InitArgs} | {[atom()],InitArgs}
%%%   InitArgs = [term()]
%%%   InclDirs = [string()] | string()
%%%   CreatePrivDir = auto_per_run | auto_per_tc | manual_per_tc
%%%   M = integer()
%%%   N = integer()
%%%   DurTime = string(HHMMSS)
%%%   StopTime = string(YYMoMoDDHHMMSS) | string(HHMMSS)
%%%   ForceStop = skip_rest | Bool
%%%   DecryptKeyOrFile = {key,DecryptKey} | {file,DecryptFile}
%%%   DecryptKey = string()
%%%   DecryptFile = string()
%%%   LogOpts = [LogOpt]
%%%   LogOpt = no_nl | no_src
%%%   VLevels = VLevel | [{Category,VLevel}]
%%%   VLevel = integer()
%%%   Category = atom()
%%%   CTHs = [CTHModule | {CTHModule, CTHInitArgs}]
%%%   CTHModule = atom()
%%%   CTHInitArgs = term()
%%%   Result = {Ok,Failed,{UserSkipped,AutoSkipped}} | TestRunnerPid | {error,Reason}
%%%   Ok = integer()
%%%   Failed = integer()
%%%   UserSkipped = integer()
%%%   AutoSkipped = integer()
%%%   TestRunnerPid = pid()
%%%   Reason = term()
%%% @doc <p>Run tests as specified by the combination of options in <c>Opts</c>.
%%% The options are the same as those used with the
%%% <seealso marker="ct_run#ct_run"><c>ct_run</c></seealso> program.
%%% Note that here a <c>TestDir</c> can be used to point out the path to 
%%% a <c>Suite</c>. Note also that the option <c>testcase</c>
%%% corresponds to the <c>-case</c> option in the <c>ct_run</c> 
%%% program. Configuration files specified in <c>Opts</c> will be
%%% installed automatically at startup.</p>
%%% <p><c>TestRunnerPid</c> is returned if <c>release_shell == true</c>
%%% (see <c>break/1</c> for details).</p>
%%% <p><c>Reason</c> indicates what type of error has been encountered.</p>
run_test(Opts) ->
    ct_run:run_test(Opts).

%%%-----------------------------------------------------------------
%%% @spec run_testspec(TestSpec) -> Result
%%%   TestSpec = [term()]
%%%   Result = {Ok,Failed,{UserSkipped,AutoSkipped}} | {error,Reason}
%%%   Ok = integer()
%%%   Failed = integer()
%%%   UserSkipped = integer()
%%%   AutoSkipped = integer()
%%%   Reason = term()
%%% @doc Run test specified by <c>TestSpec</c>. The terms are
%%% the same as those used in test specification files.
%%% <p><c>Reason</c> indicates what type of error has been encountered.</p>
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
%%% <c>config</c> option has been given, breakpoints will
%%% be set also on the configuration functions in <c>Suite</c>.
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
%%% started from the OS command line with <c>ct_run -shell
%%% [-config File...]</c>.</p>
%%%
%%% <p>If any functions using "required config data" (e.g. telnet or
%%% ftp functions) are to be called from the erlang shell, config data
%%% must first be required with <c>ct:require/2</c>.</p>
%%%
%%% <p>Example:<br/>
%%% <c>&gt; ct:require(unix_telnet, unix).</c><br/>
%%% <c>ok</c><br/>
%%% <c>&gt; ct_telnet:open(unix_telnet).</c><br/>
%%% <c>{ok,&lt;0.105.0&gt;}</c><br/>
%%% <c>&gt; ct_telnet:cmd(unix_telnet, "ls .").</c><br/>
%%% <c>{ok,["ls","file1  ...",...]}</c></p>
start_interactive() ->
    _ = ct_util:start(interactive),
    ok.

%%%-----------------------------------------------------------------
%%% @spec stop_interactive() -> ok
%%%
%%% @doc Exit the interactive mode.
%%% @see start_interactive/0
stop_interactive() ->
    ct_util:stop(normal),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MISC INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------
%%% @spec require(Required) -> ok | {error,Reason}
%%%      Required = Key | {Key,SubKeys} | {Key,SubKey,SubKeys}
%%%      Key = atom()
%%%      SubKeys = SubKey | [SubKey]
%%%      SubKey = atom()
%%%
%%% @doc Check if the required configuration is available. It is possible
%%% to specify arbitrarily deep tuples as <c>Required</c>. Note that it is
%%% only the last element of the tuple which can be a list of <c>SubKey</c>s.
%%%
%%% <p>Example 1: require the variable <c>myvar</c>:</p>
%%% <pre>ok = ct:require(myvar).</pre>
%%%
%%% <p>In this case the config file must at least contain:</p>
%%% <pre>{myvar,Value}.</pre>
%%% 
%%% <p>Example 2: require the key <c>myvar</c> with
%%% subkeys <c>sub1</c> and <c>sub2</c>:</p>
%%% <pre>ok = ct:require({myvar,[sub1,sub2]}).</pre>
%%%
%%% <p>In this case the config file must at least contain:</p>
%%% <pre>{myvar,[{sub1,Value},{sub2,Value}]}.</pre>
%%%
%%% <p>Example 3: require the key <c>myvar</c> with
%%% subkey <c>sub1</c> with <c>subsub1</c>:</p>
%%% <pre>ok = ct:require({myvar,sub1,sub2}).</pre>
%%%
%%% <p>In this case the config file must at least contain:</p>
%%% <pre>{myvar,[{sub1,[{sub2,Value}]}]}.</pre>
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
%%%      Required = Key | {Key,SubKey} | {Key,SubKey,SubKey}
%%%      SubKey = Key
%%%      Key = atom()
%%%
%%% @doc Check if the required configuration is available, and give it
%%% a name. The semantics for <c>Required</c> is the same as in
%%% <c>required/1</c> except that it is not possible to specify a list
%%% of <c>SubKey</c>s.
%%%
%%% <p>If the requested data is available, the sub entry will be
%%% associated with <c>Name</c> so that the value of the element
%%% can be read with <c>get_config/1,2</c> provided
%%% <c>Name</c> instead of the whole <c>Required</c> term.</p>
%%%
%%% <p>Example: Require one node with a telnet connection and an
%%% ftp connection. Name the node <c>a</c>:
%%% <pre>ok = ct:require(a,{machine,node}).</pre>
%%% All references to this node may then use the node name.
%%% E.g. you can fetch a file over ftp like this:</p>
%%% <pre>ok = ct:ftp_get(a,RemoteFile,LocalFile).</pre>
%%%
%%% <p>For this to work, the config file must at least contain:</p>
%%% <pre>{machine,[{node,[{telnet,IpAddr},{ftp,IpAddr}]}]}.</pre>
%%%
%%% <note><p>The behaviour of this function changed radically in common_test
%%% 1.6.2. In order too keep some backwards compatability it is still possible
%%% to do: <br/><c>ct:require(a,{node,[telnet,ftp]}).</c><br/>
%%% This will associate the name <c>a</c> with the top level <c>node</c> entry.
%%% For this to work, the config file must at least contain:<br/>
%%% <c>{node,[{telnet,IpAddr},{ftp,IpAddr}]}.</c></p></note>
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
%%%      Required = KeyOrName | {KeyOrName,SubKey} | {KeyOrName,SubKey,SubKey}
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
%%% (if one has been specified with <c>require/2</c> or a 
%%% require statement).</p>
%%%
%%% <p>Example, given the following config file:</p>
%%% <pre>
%%% {unix,[{telnet,IpAddr},
%%%        {user,[{username,Username},
%%%               {password,Password}]}]}.</pre>
%%% <p><c>ct:get_config(unix,Default) ->
%%%                          [{telnet,IpAddr},
%%%                           {user, [{username,Username},
%%%                                   {password,Password}]}]</c><br/>
%%% <c>ct:get_config({unix,telnet},Default) -> IpAddr</c><br/>
%%% <c>ct:get_config({unix,user,username},Default) -> Username</c><br/>
%%% <c>ct:get_config({unix,ftp},Default) -> Default</c><br/>
%%% <c>ct:get_config(unknownkey,Default) -> Default</c></p>
%%%
%%% <p>If a config variable key has been associated with a name (by
%%% means of <c>require/2</c> or a require statement), the name 
%%% may be used instead of the key to read the value:</p>
%%%
%%% <p><c>ct:require(myuser,{unix,user}) -> ok.</c><br/>
%%% <c>ct:get_config(myuser,Default) ->
%%%          [{username,Username},
%%%           {password,Password}]</c></p>
%%%
%%% <p>If a config variable is defined in multiple files and you want to
%%% access all possible values, use the <c>all</c> option. The
%%% values will be returned in a list and the order of the elements
%%% corresponds to the order that the config files were specified at 
%%% startup.</p>
%%%
%%% <p>If you want config elements (key-value tuples) returned as result 
%%% instead of values, use the <c>element</c> option. 
%%% The returned elements will then be on the form <c>{Required,Value}</c></p>
%%%
%%% @see get_config/1
%%% @see get_config/2
%%% @see require/1
%%% @see require/2
get_config(Required,Default,Opts) ->
    ct_config:get_config(Required,Default,Opts).

%%%-----------------------------------------------------------------
%%% @spec reload_config(Required) -> ValueOrElement
%%%      Required = KeyOrName | {KeyOrName,SubKey} | {KeyOrName,SubKey,SubKey}
%%%      KeyOrName = atom()
%%%      SubKey = atom()
%%%      ValueOrElement = term()
%%%
%%% @doc Reload config file which contains specified configuration key.
%%%
%%% <p>This function performs updating of the configuration data from which the
%%% given configuration variable was read, and returns the (possibly) new
%%% value of this variable.</p>
%%% <p>Note that if some variables were present in the configuration but are not loaded
%%% using this function, they will be removed from the configuration table together
%%% with their aliases.</p>
%%%
reload_config(Required)->
    ct_config:reload_config(Required).

%%%-----------------------------------------------------------------
%%% @spec get_testspec_terms() -> TestSpecTerms | undefined
%%%      TestSpecTerms = [{Tag,Value}]
%%%      Value = [term()]
%%%
%%% @doc Get a list of all test specification terms used to
%%% configure and run this test.
%%%
get_testspec_terms() ->
    case ct_util:get_testdata(testspec) of
	undefined ->
	    undefined;
	CurrSpecRec ->
	    ct_testspec:testspec_rec2list(CurrSpecRec)
    end.

%%%-----------------------------------------------------------------
%%% @spec get_testspec_terms(Tags) -> TestSpecTerms | undefined
%%%      Tags = [Tag] | Tag
%%%      Tag = atom()
%%%      TestSpecTerms = [{Tag,Value}] | {Tag,Value}
%%%      Value = [{Node,term()}] | [term()]
%%%      Node = atom()
%%%
%%% @doc Read one or more terms from the test specification used
%%% to configure and run this test. Tag is any valid test specification
%%% tag, such as e.g. <c>label</c>, <c>config</c>, <c>logdir</c>.
%%% User specific terms are also available to read if the
%%% <c>allow_user_terms</c> option has been set. Note that all value tuples
%%% returned, except user terms, will have the node name as first element.
%%% Note also that in order to read test terms, use <c>Tag = tests</c>
%%% (rather than <c>suites</c>, <c>groups</c> or <c>cases</c>). Value is
%%% then the list of *all* tests on the form:
%%% <c>[{Node,Dir,[{TestSpec,GroupsAndCases1},...]},...], where
%%% GroupsAndCases = [{Group,[Case]}] | [Case]</c>.
get_testspec_terms(Tags) ->
    case ct_util:get_testdata(testspec) of
	undefined ->
	    undefined;
	CurrSpecRec ->
	    ct_testspec:testspec_rec2list(Tags, CurrSpecRec)
    end.


%%%-----------------------------------------------------------------
%%% @spec escape_chars(IoList1) -> IoList2 | {error,Reason}
%%%      IoList1 = iolist()
%%%      IoList2 = iolist()
%%%
%%% @doc Escape special characters to be printed in html log
%%%
escape_chars(IoList) ->
    ct_logs:escape_chars(IoList).

%%%-----------------------------------------------------------------
%%% @spec escape_chars(Format, Args) -> IoList | {error,Reason}
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Escape special characters to be printed in html log
%%%
escape_chars(Format, Args) ->
    try io_lib:format(Format, Args) of
	IoList ->
	    ct_logs:escape_chars(IoList)
    catch
	_:Reason ->
	    {error,Reason}
    end.

%%%-----------------------------------------------------------------
%%% @spec log(Format) -> ok
%%% @equiv log(default,50,Format,[],[])
log(Format) ->
    log(default,?STD_IMPORTANCE,Format,[],[]).

%%%-----------------------------------------------------------------
%%% @spec log(X1,X2) -> ok
%%%      X1 = Category | Importance | Format
%%%      X2 = Format | Args
%%% @equiv log(Category,Importance,Format,Args,[])
log(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    log(Category,Importance,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec log(X1,X2,X3) -> ok
%%%      X1 = Category | Importance | Format
%%%      X2 = Importance | Format | Args
%%%      X3 = Format | Args | Opts
%%% @equiv log(Category,Importance,Format,Args,Opts)
log(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    log(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec log(X1,X2,X3,X4) -> ok
%%%      X1 = Category | Importance
%%%      X2 = Importance | Format
%%%      X3 = Format | Args
%%%      X4 = Args | Opts
%%% @equiv log(Category,Importance,Format,Args,Opts)
log(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    log(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec log(Category,Importance,Format,Args,Opts) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Format = string()
%%%      Args = list()
%%%      Opts = [Opt]
%%%      Opt = {heading,string()} | esc_chars | no_css
%%%
%%% @doc Printout from a test case to the log file. 
%%%
%%% <p>This function is meant for printing a string directly from a
%%% test case to the test case log file.</p>
%%%
%%% <p>Default <c>Category</c> is <c>default</c>,
%%% default <c>Importance</c> is <c>?STD_IMPORTANCE</c>,
%%% and default value for <c>Args</c> is <c>[]</c>.</p>
%%% <p>Please see the User's Guide for details on <c>Category</c>
%%% and <c>Importance</c>.</p>
log(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_log(Category,Importance,Format,Args,Opts).


%%%-----------------------------------------------------------------
%%% @spec print(Format) -> ok
%%% @equiv print(default,50,Format,[],[])
print(Format) ->
    print(default,?STD_IMPORTANCE,Format,[],[]).

%%%-----------------------------------------------------------------
%%% @spec print(X1,X2) -> ok
%%%      X1 = Category | Importance | Format
%%%      X2 = Format | Args
%%% @equiv print(Category,Importance,Format,Args,[])
print(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    print(Category,Importance,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec print(X1,X2,X3) -> ok
%%%      X1 = Category | Importance | Format
%%%      X2 = Importance | Format | Args
%%%      X3 = Format | Args | Opts
%%% @equiv print(Category,Importance,Format,Args,Opts)
print(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    print(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec print(X1,X2,X3,X4) -> ok
%%%      X1 = Category | Importance
%%%      X2 = Importance | Format
%%%      X3 = Format | Args
%%%      X4 = Args | Opts
%%% @equiv print(Category,Importance,Format,Args,Opts)
print(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    print(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec print(Category,Importance,Format,Args,Opts) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Format = string()
%%%      Args = list()
%%%      Opts = [Opt]
%%%      Opt = {heading,string()}
%%%
%%% @doc Printout from a test case to the console. 
%%%
%%% <p>This function is meant for printing a string from a test case
%%% to the console.</p>
%%%
%%% <p>Default <c>Category</c> is <c>default</c>,
%%% default <c>Importance</c> is <c>?STD_IMPORTANCE</c>,
%%% and default value for <c>Args</c> is <c>[]</c>.</p>
%%% <p>Please see the User's Guide for details on <c>Category</c>
%%% and <c>Importance</c>.</p>
print(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_print(Category,Importance,Format,Args,Opts).


%%%-----------------------------------------------------------------
%%% @spec pal(Format) -> ok
%%% @equiv pal(default,50,Format,[],[])
pal(Format) ->
    pal(default,?STD_IMPORTANCE,Format,[]).

%%%-----------------------------------------------------------------
%%% @spec pal(X1,X2) -> ok
%%%      X1 = Category | Importance | Format
%%%      X2 = Format | Args
%%% @equiv pal(Category,Importance,Format,Args,[])
pal(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    pal(Category,Importance,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec pal(X1,X2,X3) -> ok
%%%      X1 = Category | Importance | Format
%%%      X2 = Importance | Format | Args
%%%      X3 = Format | Args | Opts
%%% @equiv pal(Category,Importance,Format,Args,Opts)
pal(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    pal(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec pal(X1,X2,X3,X4) -> ok
%%%      X1 = Category | Importance
%%%      X2 = Importance | Format
%%%      X3 = Format | Args
%%%      X4 = Args | Opts
%%% @equiv pal(Category,Importance,Format,Args,Opts)
pal(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    pal(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec pal(Category,Importance,Format,Args,Opts) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Format = string()
%%%      Args = list()
%%%      Opts = [Opt]
%%%      Opt = {heading,string()} | no_css
%%%
%%% @doc Print and log from a test case. 
%%%
%%% <p>This function is meant for printing a string from a test case,
%%% both to the test case log file and to the console.</p>
%%%
%%% <p>Default <c>Category</c> is <c>default</c>,
%%% default <c>Importance</c> is <c>?STD_IMPORTANCE</c>,
%%% and default value for <c>Args</c> is <c>[]</c>.</p>
%%% <p>Please see the User's Guide for details on <c>Category</c>
%%% and <c>Importance</c>.</p>
pal(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_pal(Category,Importance,Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec set_verbosity(Category, Level) -> ok
%%%      Category = default | atom()
%%%      Level = integer()
%%%
%%% @doc Set the verbosity level for a category
set_verbosity(Category, Level) ->
    ct_util:set_verbosity({Category,Level}).

%%%-----------------------------------------------------------------
%%% @spec get_verbosity(Category) -> Level | undefined
%%%      Category = default | atom()
%%%      Level = integer()
%%%
%%% @doc Read the verbosity level for a category
get_verbosity(Category) ->
    ct_util:get_verbosity(Category).

%%%-----------------------------------------------------------------
%%% @spec capture_start() -> ok
%%%
%%% @doc Start capturing all text strings printed to stdout during
%%% execution of the test case.
%%%
%%% @see capture_stop/0
%%% @see capture_get/1
capture_start() ->
    test_server:capture_start().

%%%-----------------------------------------------------------------
%%% @spec capture_stop() -> ok
%%%
%%% @doc Stop capturing text strings (a session started with
%%% <c>capture_start/0</c>).
%%%
%%% @see capture_start/0
%%% @see capture_get/1
capture_stop() ->
    test_server:capture_stop().

%%%-----------------------------------------------------------------
%%% @spec capture_get() -> ListOfStrings
%%%      ListOfStrings = [string()]
%%%
%%% @equiv capture_get([default])
capture_get() ->
    %% remove default log printouts (e.g. ct:log/2 printouts)
    capture_get([default]).

%%%-----------------------------------------------------------------
%%% @spec capture_get(ExclCategories) -> ListOfStrings
%%%      ExclCategories = [atom()]
%%%      ListOfStrings = [string()]
%%%
%%% @doc Return and purge the list of text strings buffered
%%% during the latest session of capturing printouts to stdout.
%%% With <c>ExclCategories</c> it's possible to specify
%%% log categories that should be ignored in <c>ListOfStrings</c>.
%%% If <c>ExclCategories = []</c>, no filtering takes place.
%%%
%%% @see capture_start/0
%%% @see capture_stop/0
%%% @see log/3
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

%%%-----------------------------------------------------------------
%%% @spec fail(Reason) -> ok
%%%      Reason = term()
%%%
%%% @doc Terminate a test case with the given error
%%% <c>Reason</c>.
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

%%%-----------------------------------------------------------------
%%% @spec fail(Format, Args) -> ok
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Terminate a test case with an error message specified
%%% by a format string and a list of values (used as arguments to
%%% <c>io_lib:format/2</c>).
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

%%%-----------------------------------------------------------------
%%% @spec comment(Comment) -> ok
%%%      Comment = term()
%%%
%%% @doc Print the given <c>Comment</c> in the comment field in
%%% the table on the test suite result page.
%%%
%%% <p>If called several times, only the last comment is printed.
%%% The test case return value <c>{comment,Comment}</c>
%%% overwrites the string set by this function.</p>
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

%%%-----------------------------------------------------------------
%%% @spec comment(Format, Args) -> ok
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Print the formatted string in the comment field in
%%% the table on the test suite result page.
%%% 
%%% <p>The <c>Format</c> and <c>Args</c> arguments are
%%% used in call to <c>io_lib:format/2</c> in order to create
%%% the comment string. The behaviour of <c>comment/2</c> is
%%% otherwise the same as the <c>comment/1</c> function (see
%%% above for details).</p>
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

%%%-----------------------------------------------------------------
%%% @spec make_priv_dir() -> ok | {error,Reason}
%%%      Reason = term()
%%% @doc If the test has been started with the create_priv_dir 
%%% option set to manual_per_tc, in order for the test case to use
%%% the private directory, it must first create it by calling
%%% this function.
make_priv_dir() ->
    test_server:make_priv_dir().

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
%%% <c>select</c> command in SQL. The returned
%%% <c>Table</c> is a list of tuples, where each tuple is a row
%%% in the table.</p>
%%% 
%%% <p><c>Heading</c> is a tuple of strings representing the
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
%%% @doc Returns all test cases in the specified suite.
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
	    CtInclPath -> string:lexemes(CtInclPath, [$:,$ ,$,])
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
%%% @doc Returns any data specified with the tag <c>userdata</c> 
%%% in the list of tuples returned from <c>Suite:suite/0</c>.
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
   
%%%-----------------------------------------------------------------
%%% @spec userdata(TestDir, Suite, GroupOrCase) -> TCUserData | {error,Reason}
%%%       TestDir = string()
%%%       Suite = atom()
%%%       GroupOrCase = {group,GroupName} | atom()
%%%       GroupName = atom()
%%%       TCUserData = [term()]
%%%       Reason = term()
%%%
%%% @doc Returns any data specified with the tag <c>userdata</c>
%%% in the list of tuples returned from <c>Suite:group(GroupName)</c>
%%% or <c>Suite:Case()</c>.
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


%%%-----------------------------------------------------------------
%%% @spec get_status() -> TestStatus | {error,Reason} | no_tests_running
%%%       TestStatus = [StatusElem]
%%%       StatusElem = {current,TestCaseInfo} | {successful,Successful} |
%%%                    {failed,Failed} | {skipped,Skipped} | {total,Total}
%%%       TestCaseInfo = {Suite,TestCase} | [{Suite,TestCase}]
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
%%%      which test case is currently executing (a list of cases when a
%%%      parallel test case group is executing), as well as counters for 
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

%%%-----------------------------------------------------------------
%%% @spec abort_current_testcase(Reason) -> ok | {error,ErrorReason}
%%%       Reason = term()
%%%       ErrorReason = no_testcase_running | parallel_group
%%%
%%% @doc <p>When calling this function, the currently executing test case will be aborted.
%%%      It is the user's responsibility to know for sure which test case is currently
%%%	 executing. The function is therefore only safe to call from a function which
%%%	 has been called (or synchronously invoked) by the test case.</p>
%%%
%%%      <p><c>Reason</c>, the reason for aborting the test case, is printed
%%%      in the test case log.</p>
abort_current_testcase(Reason) ->
    test_server_ctrl:abort_current_testcase(Reason).

%%%-----------------------------------------------------------------
%%% @spec get_event_mgr_ref() -> EvMgrRef
%%%       EvMgrRef = atom()
%%%
%%% @doc <p>Call this function in order to get a reference to the
%%%         CT event manager. The reference can be used to e.g. add
%%%         a user specific event handler while tests are running.
%%%         Example:
%%%         <c>gen_event:add_handler(ct:get_event_mgr_ref(), my_ev_h, [])</c></p>
get_event_mgr_ref() ->
    ?CT_EVMGR_REF.

%%%-----------------------------------------------------------------
%%% @spec encrypt_config_file(SrcFileName, EncryptFileName) -> 
%%%                                                  ok | {error,Reason}
%%%       SrcFileName = string()
%%%       EncryptFileName = string()
%%%       Reason = term()
%%%
%%% @doc <p>This function encrypts the source config file with DES3 and 
%%%      saves the result in file <c>EncryptFileName</c>. The key,
%%%      a string, must be available in a text file named 
%%%      <c>.ct_config.crypt</c> in the current directory, or the 
%%%      home directory of the user (it is searched for in that order).</p>
%%%      <p>See the Common Test User's Guide for information about using
%%%      encrypted config files when running tests.</p>
%%%      <p>See the <c>crypto</c> application for details on DES3
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
%%%      saves the result in the target file <c>EncryptFileName</c>. 
%%%      The encryption key to use is either the value in 
%%%      <c>{key,Key}</c> or the value stored in the file specified 
%%%      by <c>{file,File}</c>.</p>
%%%      <p>See the Common Test User's Guide for information about using
%%%      encrypted config files when running tests.</p>
%%%      <p>See the <c>crypto</c> application for details on DES3
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
%%% @doc <p>This function decrypts <c>EncryptFileName</c>, previously 
%%%      generated with <c>encrypt_config_file/2/3</c>. The original
%%%      file contents is saved in the target file. The encryption key, a
%%%      string, must be available in a text file named
%%%      <c>.ct_config.crypt</c> in the current directory, or the
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
%%% @doc <p>This function decrypts <c>EncryptFileName</c>, previously 
%%%      generated with <c>encrypt_config_file/2/3</c>. The original
%%%      file contents is saved in the target file. The key must have the
%%%      the same value as that used for encryption.</p>
decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile).


%%%-----------------------------------------------------------------
%%% @spec add_config(Callback, Config) -> ok | {error, Reason}
%%%       Callback = atom()
%%%       Config = string()
%%%       Reason = term()
%%%
%%% @doc <p>This function loads configuration variables using the
%%% 	 given callback module and configuration string. Callback module
%%%	 should be either loaded or present in the code part. Loaded
%%%	 configuration variables can later be removed using
%%%	 <c>remove_config/2</c> function.</p>
add_config(Callback, Config)->
    ct_config:add_config(Callback, Config).

%%%-----------------------------------------------------------------
%%% @spec remove_config(Callback, Config) -> ok
%%%       Callback = atom()
%%%       Config = string()
%%%       Reason = term()
%%%
%%% @doc <p>This function removes configuration variables (together with
%%%	 their aliases) which were loaded with specified callback module and
%%%	 configuration string.</p>
remove_config(Callback, Config) ->
    ct_config:remove_config(Callback, Config).

%%%-----------------------------------------------------------------
%%% @spec timetrap(Time) -> ok
%%%       Time = {hours,Hours} | {minutes,Mins} | {seconds,Secs} | Millisecs | infinity | Func
%%%       Hours = integer()
%%%       Mins = integer()
%%%       Secs = integer()
%%%       Millisecs = integer() | float()
%%%       Func = {M,F,A} | fun()
%%%       M = atom()
%%%       F = atom()
%%%       A = list()
%%%
%%% @doc <p>Use this function to set a new timetrap for the running test case.
%%%      If the argument is <c>Func</c>, the timetrap will be triggered
%%%      when this function returns. <c>Func</c> may also return a new
%%%      <c>Time</c> value, which in that case will be the value for the
%%%      new timetrap.</p>
timetrap(Time) ->
    test_server:timetrap_cancel(),
    test_server:timetrap(Time).

%%%-----------------------------------------------------------------
%%% @spec get_timetrap_info() -> {Time,Scale}
%%%       Time = integer() | infinity
%%%       Scale = true | false
%%%
%%% @doc <p>Read info about the timetrap set for the current test case.
%%%      <c>Scale</c> indicates if Common Test will attempt to automatically
%%%      compensate timetraps for runtime delays introduced by e.g. tools like
%%%      cover.</p>
get_timetrap_info() ->
    test_server:get_timetrap_info().

%%%-----------------------------------------------------------------
%%% @spec sleep(Time) -> ok
%%%       Time = {hours,Hours} | {minutes,Mins} | {seconds,Secs} | Millisecs | infinity
%%%       Hours = integer()
%%%       Mins = integer()
%%%       Secs = integer()
%%%       Millisecs = integer() | float()
%%%
%%% @doc <p>This function, similar to <c>timer:sleep/1</c>, suspends the test
%%%      case for specified time. However, this function also multiplies
%%%      <c>Time</c> with the 'multiply_timetraps' value (if set) and under
%%%      certain circumstances also scales up the time automatically
%%%      if 'scale_timetraps' is set to true (default is false).</p>
sleep({hours,Hs}) ->
    sleep(trunc(Hs * 1000 * 60 * 60));
sleep({minutes,Ms}) ->
    sleep(trunc(Ms * 1000 * 60));
sleep({seconds,Ss}) ->
    sleep(trunc(Ss * 1000));
sleep(Time) ->
    test_server:adjusted_sleep(Time).

%%%-----------------------------------------------------------------
%%% @spec notify(Name,Data) -> ok
%%%       Name = atom()
%%%       Data = term()
%%%
%%% @doc <p>Sends a asynchronous notification of type <c>Name</c> with
%%%      <c>Data</c>to the common_test event manager. This can later be
%%%      caught by any installed event manager. </p>
%%% @see //stdlib/gen_event
notify(Name,Data) ->
    ct_event:notify(Name, Data).

%%%-----------------------------------------------------------------
%%% @spec sync_notify(Name,Data) -> ok
%%%       Name = atom()
%%%       Data = term()
%%%
%%% @doc <p>Sends a synchronous notification of type <c>Name</c> with
%%%      <c>Data</c>to the common_test event manager. This can later be
%%%      caught by any installed event manager. </p>
%%% @see //stdlib/gen_event
sync_notify(Name,Data) ->
    ct_event:sync_notify(Name, Data).

%%%-----------------------------------------------------------------
%%% @spec break(Comment) -> ok | {error,Reason}
%%%       Comment = string()
%%%       Reason = {multiple_cases_running,TestCases} |
%%%                'enable break with release_shell option'
%%%       TestCases = [atom()]
%%%
%%% @doc <p>This function will cancel any active timetrap and pause the
%%%       execution of the current test case until the user calls the
%%%       <c>continue/0</c> function. It gives the user the opportunity
%%%       to interact with the erlang node running the tests, e.g. for
%%%       debugging purposes or for manually executing a part of the
%%%       test case. If a parallel group is executing, <c>break/2</c>
%%%       should be called instead.</p>
%%%      <p>A cancelled timetrap will not be automatically
%%%       reactivated after the break, but must be started exlicitly with
%%%       <c>ct:timetrap/1</c></p>
%%%      <p>In order for the break/continue functionality to work,
%%%       Common Test must release the shell process controlling stdin.
%%%       This is done by setting the <c>release_shell</c> start option
%%%       to <c>true</c>. See the User's Guide for more information.</p>

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

%%%-----------------------------------------------------------------
%%% @spec break(TestCase, Comment) -> ok | {error,Reason}
%%%       TestCase = atom()
%%%       Comment = string()
%%%       Reason = 'test case not running' |
%%%                'enable break with release_shell option'
%%%
%%% @doc <p>This function works the same way as <c>break/1</c>,
%%%       only the <c>TestCase</c> argument makes it possible to
%%%       pause a test case executing in a parallel group. The
%%%       <c>continue/1</c> function should be used to resume
%%%       execution of <c>TestCase</c>.</p>
%%%      <p>See <c>break/1</c> for more details.</p>
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

%%%-----------------------------------------------------------------
%%% @spec continue() -> ok
%%%
%%% @doc <p>This function must be called in order to continue after a
%%%      test case (not executing in a parallel group) has called
%%%      <c>break/1</c>.</p>
continue() -> 
    test_server:continue().

%%%-----------------------------------------------------------------
%%% @spec continue(TestCase) -> ok
%%%       TestCase = atom()
%%%
%%% @doc <p>This function must be called in order to continue after a
%%%      test case has called <c>break/2</c>. If the paused test case,
%%%      <c>TestCase</c>, executes in a parallel group, this
%%%      function - rather than <c>continue/0</c> - must be used
%%%      in order to let the test case proceed.</p>
continue(TestCase) -> 
    test_server:continue(TestCase).


%%%-----------------------------------------------------------------
%%% @spec remaining_test_procs() -> {TestProcs,SharedGL,OtherGLs}
%%%       TestProcs = [{pid(),GL}]
%%%       GL = SharedGL = pid()
%%%       OtherGLs = [pid()]
%%%
%%% @doc <p>This function will return the identity of test- and group
%%%      leader processes that are still running at the time of this call.
%%%      TestProcs are processes in the system that have a Common Test IO
%%%      process as group leader. SharedGL is the central Common Test
%%%      IO process, responsible for printing to log files for configuration
%%%      functions and sequentially executing test cases. OtherGLs are
%%%      Common Test IO processes that print to log files for test cases
%%%      in parallel test case groups.</p>
%%%      <p>The process information returned by this function may be
%%%      used to locate and terminate remaining processes after tests have
%%%      finished executing. The function would typically by called from
%%%      Common Test Hook functions.</p>
%%%      <p>Note that processes that execute configuration functions or
%%%      test cases are never included in TestProcs. It is therefore safe
%%%      to use post configuration hook functions (such as post_end_per_suite,
%%%      post_end_per_group, post_end_per_testcase) to terminate all processes
%%%      in TestProcs that have the current group leader process as its group
%%%      leader.</p>
%%%      <p>Note also that the shared group leader (SharedGL) must never be
%%%      terminated by the user, only by Common Test. Group leader processes
%%%      for parallel test case groups (OtherGLs) may however be terminated
%%%      in post_end_per_group hook functions.</p>
%%%      
remaining_test_procs() ->
    ct_util:remaining_test_procs().
