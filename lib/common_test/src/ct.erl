%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
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
-moduledoc """
Main user interface for the `Common Test` framework.

This module implements the command-line interface for running tests and basic
functions for `Common Test` case issues, such as configuration and logging.

The framework stores configuration values in a property list usually named
`Config`. The list contains information about the test run added by the
framework itself and may also contain user-provided values. The configuration is
passed into individual test cases as well as support functions if defined.

Possible configuration variables include:

- `data_dir` \- Data file directory
- `priv_dir` \- Scratch file directory
- Whatever added by [`init_per_suite/1`](`c:ct_suite:init_per_suite/1`) or
  [`init_per_testcase/2`](`c:ct_suite:init_per_testcase/2`) in the test suite.

> #### Warning {: .warning }
>
> The `?config` macro, used to receive individual config values from the
> `Config` property list, is deprecated. Please use `proplists:get_value/2-3`
> instead.
""".

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
	      key_or_name/0,
          handle/0]).

%% For cth_conn_log
-export_type([conn_log_option/0,
          conn_log_options/0,
	      conn_log_type/0,
	      conn_log_mod/0]).

%%------------------------------------------------------------------
%% Type declarations
%% ------------------------------------------------------------------
-doc "A configuration key which exists in a configuration file".
-type config_key() :: atom(). % Config key which exists in a config file
-doc """
A name and association to configuration data introduced through a require
statement, or a call to [`ct:require/2`](`require/2`), for example,
`ct:require(mynodename,{node,[telnet]})`.
""".
-type target_name() :: atom().% Name associated to a config_key() though 'require'
-type key_or_name() :: config_key() | target_name().
-doc "The identity (handle) of a connection.".
-type handle() :: pid().

%% Types used when logging connections with the 'cth_conn_log' hook
-doc """
Options that can be given to the `cth_conn_log` hook, which is used for logging
of NETCONF and Telnet connections. See [ct_netconfc](`m:ct_netconfc#Logging`) or
[ct_telnet](`m:ct_telnet#module-logging`) for description and examples of how to use
this hook.
""".
-type conn_log_options() :: [conn_log_option()].
-type conn_log_option() :: {log_type,conn_log_type()} |
                           {hosts,[key_or_name()]}.
-type conn_log_type() :: raw | pretty | html | silent.
-type conn_log_mod() :: ct_netconfc | ct_telnet.
%%----------------------------------------------------------------------


-doc """
Installs configuration files and event handlers.

Run this function once before the first test.

_Example:_

```erlang
install([{config,["config_node.ctc","config_user.ctc"]}])
```

This function is automatically run by program `ct_run`.
""".
-spec install(Opts) -> ok | {error, Reason}
      when Opts :: [Opt],
           Opt :: {config, ConfigFiles} | {event_handler, Modules} | {decrypt, KeyOrFile},
           ConfigFiles :: [ConfigFile],
           ConfigFile :: string(),
           Modules :: [atom()],
           KeyOrFile :: {key, Key} | {file, KeyFile},
           Key :: string(),
           KeyFile :: string(),
           Reason :: term().
install(Opts) ->
    ct_run:install(Opts).

-doc """
Runs the specified test cases.

Requires that [`ct:install/1`](`install/1`) has been run first.

Suites (`*_SUITE.erl`) files must be stored in `TestDir` or `TestDir/test`. All
suites are compiled when the test is run.
""".
-spec run(TestDir, Suite, Cases) -> Result
      when TestDir :: string(),
           Suite :: atom(),
           Cases :: atom() | [atom()],
           Result :: [TestResult] | {error, Reason},
           TestResult :: term(),
           Reason :: term().
run(TestDir,Suite,Cases) ->
    ct_run:run(TestDir,Suite,Cases).

-doc """
Runs all test cases in the specified suite.

See also [`ct:run/3`](`run/3`).
""".
-spec run(TestDir, Suite) -> Result
      when TestDir :: string(),
           Suite :: atom(),
           Result :: [TestResult] | {error, Reason},
           TestResult :: term(),
           Reason :: term().
run(TestDir,Suite) ->
    ct_run:run(TestDir,Suite).

-doc """
Runs all test cases in all suites in the specified directories.

See also [`ct:run/3`](`run/3`).
""".
-spec run(TestDirs) -> Result
      when TestDirs :: TestDir | [TestDir],
           TestDir :: string(),
           Result :: [TestResult] | {error, Reason},
           TestResult :: term(),
           Reason :: term().
run(TestDirs) ->
    ct_run:run(TestDirs).

-doc """
Runs tests as specified by the combination of options in `Opts`. The options are
the same as those used with program `ct_run`, see
[Run Tests from Command Line](ct_run_cmd.md#ct_run) in the `ct_run` manual page.

Here a `TestDir` can be used to point out the path to a `Suite`. Option
`testcase` corresponds to option `-case` in program `ct_run`. Configuration
files specified in `Opts` are installed automatically at startup.

`TestRunnerPid` is returned if `release_shell == true`. For details, see
[`ct:break/1`](`break/1`).

`Reason` indicates the type of error encountered.
""".
-spec run_test(Opts) -> Result
      when Opts :: [OptTuples],
           OptTuples :: {dir, TestDirs}
                      | {suite, Suites}
                      | {group, Groups}
                      | {testcase, Cases}
                      | {spec, TestSpecs}
                      | {join_specs, boolean()}
                      | {label, Label}
                      | {config, CfgFiles}
                      | {userconfig, UserConfig}
                      | {allow_user_terms, boolean()}
                      | {logdir, LogDir}
                      | {silent_connections, Conns}
                      | {stylesheet, CSSFile}
                      | {cover, CoverSpecFile}
                      | {cover_stop, boolean()}
                      | {step, StepOpts}
                      | {event_handler, EventHandlers}
                      | {include, InclDirs}
                      | {auto_compile, boolean()}
                      | {abort_if_missing_suites, boolean()}
                      | {create_priv_dir, CreatePrivDir}
                      | {multiply_timetraps, M}
                      | {scale_timetraps, boolean()}
                      | {repeat, N}
                      | {duration, DurTime}
                      | {until, StopTime}
                      | {force_stop, ForceStop}
                      | {decrypt, DecryptKeyOrFile}
                      | {refresh_logs, LogDir}
                      | {logopts, LogOpts}
                      | {verbosity, VLevels}
                      | {basic_html, boolean()}
                      | {esc_chars, boolean()}
                      | {keep_logs,KeepSpec}
                      | {ct_hooks, CTHs}
                      | {ct_hooks_order, CTHsOrder}
                      | {enable_builtin_hooks, boolean()}
                      | {release_shell, boolean()},
           TestDirs :: [string()] | string(),
           Suites :: [string()] | [atom()] | string() | atom(),
           Cases :: [atom()] | atom(),
           Groups :: GroupNameOrPath | [GroupNameOrPath],
           GroupNameOrPath :: [atom()] | atom() | all,
           TestSpecs :: [string()] | string(),
           Label :: string() | atom(),
           CfgFiles :: [string()] | string(),
           UserConfig :: [{CallbackMod, CfgStrings}] | {CallbackMod, CfgStrings},
           CallbackMod :: atom(),
           CfgStrings :: [string()] | string(),
           LogDir :: string(),
           Conns :: all | [atom()],
           CSSFile :: string(),
           CoverSpecFile :: string(),
           StepOpts :: [StepOpt],
           StepOpt :: config | keep_inactive,
           EventHandlers :: EH | [EH],
           EH :: atom() | {atom(), InitArgs} | {[atom()], InitArgs},
           InitArgs :: [term()],
           InclDirs :: [string()] | string(),
           CreatePrivDir :: auto_per_run | auto_per_tc | manual_per_tc,
           M :: integer(),
           N :: integer(),
           DurTime :: HHMMSS,
           HHMMSS :: string(),
           StopTime :: YYMoMoDDHHMMSS | HHMMSS,
           YYMoMoDDHHMMSS :: string(),
           ForceStop :: skip_rest | boolean(),
           DecryptKeyOrFile :: {key, DecryptKey} | {file, DecryptFile},
           DecryptKey :: string(),
           DecryptFile :: string(),
           LogOpts :: [LogOpt],
           LogOpt :: no_nl | no_src,
           VLevels :: VLevel | [{Category, VLevel}],
           VLevel :: integer(),
           Category :: atom(),
           KeepSpec :: all | pos_integer(),
           CTHs :: [CTHModule | {CTHModule, CTHInitArgs}],
           CTHsOrder :: atom(),
           CTHModule :: atom(),
           CTHInitArgs :: term(),
           Result :: {Ok, Failed, {UserSkipped, AutoSkipped}} | TestRunnerPid | {error, Reason},
           Ok :: integer(),
           Failed :: integer(),
           UserSkipped :: integer(),
           AutoSkipped :: integer(),
           TestRunnerPid :: pid(),
           Reason :: term().
run_test(Opts) ->
    ct_run:run_test(Opts).

-doc """
Runs a test specified by `TestSpec`. The same terms are used as in test
specification files.

`Reason` indicates the type of error encountered.
""".
-spec run_testspec(TestSpec) -> Result
      when TestSpec :: [term()],
           Result :: {Ok, Failed, {UserSkipped, AutoSkipped}} | {error, Reason},
           Ok :: integer(),
           Failed :: integer(),
           UserSkipped :: integer(),
           AutoSkipped :: integer(),
           Reason :: term().
run_testspec(TestSpec) ->
    ct_run:run_testspec(TestSpec).
    
-doc """
Steps through a test case with the debugger.

See also [`ct:run/3`](`run/3`).
""".
-spec step(TestDir, Suite, Case) -> Result
      when TestDir :: string(),
           Suite :: atom(),
           Case :: atom(),
           Result :: term().
step(TestDir,Suite,Case) ->
    ct_run:step(TestDir,Suite,Case).

-doc """
Steps through a test case with the debugger. If option `config` has been
specified, breakpoints are also set on the configuration functions in `Suite`.

See also [`ct:run/3`](`run/3`).
""".
-spec step(TestDir, Suite, Case, Opts) -> Result
      when TestDir :: string(),
           Suite :: atom(),
           Case :: atom(),
           Opts :: [Opt],
           Opt :: config | keep_inactive,
           Result :: term().
step(TestDir,Suite,Case,Opts) ->
    ct_run:step(TestDir,Suite,Case,Opts).

-doc """
Starts `Common Test` in interactive mode.

From this mode, all test case support functions can be executed directly from
the Erlang shell. The interactive mode can also be started from the OS command
line with `ct_run -shell [-config File...]`.

If any functions (for example, Telnet or FTP) using "required configuration
data" are to be called from the Erlang shell, configuration data must first be
required with [`ct:require/2`](`require/2`).

_Example:_

```erlang
> ct:require(unix_telnet, unix).
ok
> ct_telnet:open(unix_telnet).
{ok,<0.105.0>}
> ct_telnet:cmd(unix_telnet, "ls .").
{ok,["ls","file1  ...",...]}
```
""".
-spec start_interactive() -> ok.
start_interactive() ->
    _ = ct_util:start(interactive),
    ok.

-doc """
Exits the interactive mode.

See also [`ct:start_interactive/0`](`start_interactive/0`).
""".
-spec stop_interactive() -> ok.
stop_interactive() ->
    ct_util:stop(normal),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MISC INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Checks if the required configuration is available. Arbitrarily deep tuples can
be specified as `Required`. Only the last element of the tuple can be a list of
`SubKey`s.

_Example 1._ Require the variable `myvar`:

```erlang
ok = ct:require(myvar).
```

In this case the configuration file must at least contain:

```erlang
{myvar,Value}.
```

_Example 2._ Require key `myvar` with subkeys `sub1` and `sub2`:

```erlang
ok = ct:require({myvar,[sub1,sub2]}).
```

In this case the configuration file must at least contain:

```erlang
{myvar,[{sub1,Value},{sub2,Value}]}.
```

_Example 3._ Require key `myvar` with subkey `sub1` with `subsub1`:

```erlang
ok = ct:require({myvar,sub1,sub2}).
```

In this case the configuration file must at least contain:

```erlang
{myvar,[{sub1,[{sub2,Value}]}]}.
```

See also [`ct:get_config/1`](`get_config/1`),
[`ct:get_config/2`](`get_config/2`), [`ct:get_config/3`](`get_config/3`),
[`ct:require/2`](`require/2`).
""".
-spec require(Required) -> ok | {error, Reason}
      when Required :: Key | {Key, SubKeys} | {Key, SubKey, SubKeys},
           Key :: atom(),
           SubKeys :: SubKey | [SubKey],
           SubKey :: atom(),
           Reason :: term().
require(Required) ->
    ct_config:require(Required).

-doc """
Checks if the required configuration is available and gives it a name. The
semantics for `Required` is the same as in [`ct:require/1`](`require/1`) except
that a list of `SubKey`s cannot be specified.

If the requested data is available, the subentry is associated with `Name` so
that the value of the element can be read with
[`ct:get_config/1,2`](`get_config/1`) provided `Name` is used instead of the
whole `Required` term.

_Example:_

Require one node with a Telnet connection and an FTP connection. Name the node
`a`:

```erlang
ok = ct:require(a,{machine,node}).
```

All references to this node can then use the node name. For example, a file over
FTP is fetched like follows:

```erlang
ok = ct:ftp_get(a,RemoteFile,LocalFile).
```

For this to work, the configuration file must at least contain:

```erlang
{machine,[{node,[{telnet,IpAddr},{ftp,IpAddr}]}]}.
```

> #### Note {: .info }
>
> The behavior of this function changed radically in `Common Test` 1\.6.2. To
> keep some backwards compatibility, it is still possible to do:
> `ct:require(a,{node,[telnet,ftp]}).` This associates the name `a` with the
> top-level `node` entry. For this to work, the configuration file must at least
> contain: `{node,[{telnet,IpAddr},{ftp,IpAddr}]}.`

See also [`ct:get_config/1`](`get_config/1`),
[`ct:get_config/2`](`get_config/2`), [`ct:get_config/3`](`get_config/3`),
[`ct:require/1`](`require/1`).
""".
-spec require(Name, Required) -> ok | {error, Reason}
      when Name :: atom(),
           Required :: Key | {Key, SubKey} | {Key, SubKey, SubKey},
           SubKey :: Key,
           Key :: atom(),
           Reason :: term().
require(Name,Required) ->
    ct_config:require(Name,Required).

-doc(#{equiv => get_config(Required, undefined, [])}).
-spec get_config(Required) -> Value
      when Required :: KeyOrName | {KeyOrName, SubKey} | {KeyOrName, SubKey, SubKey},
           KeyOrName :: key_or_name(),
           SubKey :: atom(),
           Value :: term().
get_config(Required) ->
    ct_config:get_config(Required,undefined,[]).

-doc(#{equiv => get_config(Required, Default, [])}).
-spec get_config(Required, Default) -> Value
      when Required :: KeyOrName | {KeyOrName, SubKey} | {KeyOrName, SubKey, SubKey},
           KeyOrName :: key_or_name(),
           SubKey :: atom(),
           Default :: term(),
           Value :: term().
get_config(Required,Default) ->
    ct_config:get_config(Required,Default,[]).

-doc """
Reads configuration data values.

Returns the matching values or configuration elements, given a configuration
variable key or its associated name (if one has been specified with
[`ct:require/2`](`require/2`) or a `require` statement).

_Example:_

Given the following configuration file:

```erlang
{unix,[{telnet,IpAddr},
       {user,[{username,Username},
              {password,Password}]}]}.
```

Then:

```erlang
ct:get_config(unix,Default) -> [{telnet,IpAddr},
 {user, [{username,Username}, {password,Password}]}]
ct:get_config({unix,telnet},Default) -> IpAddr
ct:get_config({unix,user,username},Default) -> Username
ct:get_config({unix,ftp},Default) -> Default
ct:get_config(unknownkey,Default) -> Default
```

If a configuration variable key has been associated with a name (by
[`ct:require/2`](`require/2`) or a `require` statement), the name can be used
instead of the key to read the value:

```erlang
ct:require(myuser,{unix,user}) -> ok.
ct:get_config(myuser,Default) -> [{username,Username}, {password,Password}]
```

If a configuration variable is defined in multiple files, use option `all` to
access all possible values. The values are returned in a list. The order of the
elements corresponds to the order that the configuration files were specified at
startup.

If configuration elements (key-value tuples) are to be returned as result
instead of values, use option `element`. The returned elements are then on the
form `{Required,Value}`.

See also [`ct:get_config/1`](`get_config/1`),
[`ct:get_config/2`](`get_config/2`), [`ct:require/1`](`require/1`),
[`ct:require/2`](`require/2`).
""".
-spec get_config(Required, Default, Opts) -> ValueOrElement
      when Required :: KeyOrName | {KeyOrName, SubKey} | {KeyOrName, SubKey, SubKey},
           KeyOrName :: key_or_name(),
           SubKey :: atom(),
           Default :: term(),
           Opts :: [Opt],
           Opt :: element | all,
           ValueOrElement :: term() | Default.
get_config(Required,Default,Opts) ->
    ct_config:get_config(Required,Default,Opts).

-doc """
Reloads configuration file containing specified configuration key.

This function updates the configuration data from which the specified
configuration variable was read, and returns the (possibly) new value of this
variable.

If some variables were present in the configuration, but are not loaded using
this function, they are removed from the configuration table together with their
aliases.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec reload_config(Required) -> ValueOrElement | {error, Reason}
      when Required :: KeyOrName | {KeyOrName, SubKey} | {KeyOrName, SubKey, SubKey},
           KeyOrName :: key_or_name(),
           SubKey :: atom(),
           ValueOrElement :: term(),
           Reason :: term().
reload_config(Required)->
    ct_config:reload_config(Required).

-doc "Gets a list of all test specification terms used to configure and run this test.".
-doc(#{since => <<"OTP 18.0">>}).
-spec get_testspec_terms() -> TestSpecTerms | undefined
      when TestSpecTerms :: [{Tag, Value}],
           Tag :: atom(),
           Value :: [term()].
get_testspec_terms() ->
    case ct_util:get_testdata(testspec) of
	undefined ->
	    undefined;
	CurrSpecRec ->
	    ct_testspec:testspec_rec2list(CurrSpecRec)
    end.

-doc """
Reads one or more terms from the test specification used to configure and run
this test. `Tag` is any valid test specification tag, for example, `label`,
`config`, or `logdir`. User-specific terms are also available to read if option
`allow_user_terms` is set.

All value tuples returned, except user terms, have the node name as first
element.

To read test terms, use `Tag = tests` (rather than `suites`, `groups`, or
`cases`). `Value` is then the list of _all_ tests on the form
`[{Node,Dir,[{TestSpec,GroupsAndCases1},...]},...]`, where
`GroupsAndCases = [{Group,[Case]}] | [Case]`.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec get_testspec_terms(Tags) -> TestSpecTerms | undefined
      when Tags :: [Tag] | Tag,
           Tag :: atom(),
           TestSpecTerms :: [{Tag, Value}] | {Tag, Value},
           Value :: [{Node, term()}] | [term()],
           Node :: atom().
get_testspec_terms(Tags) ->
    case ct_util:get_testdata(testspec) of
	undefined ->
	    undefined;
	CurrSpecRec ->
	    ct_testspec:testspec_rec2list(Tags, CurrSpecRec)
    end.

-doc false.
escape_chars(IoList) ->
    ct_logs:escape_chars(IoList).

-doc false.
escape_chars(Format, Args) ->
    try io_lib:format(Format, Args) of
	IoList ->
	    ct_logs:escape_chars(IoList)
    catch
	_:Reason ->
	    {error,Reason}
    end.

-doc "Equivalent to [`log(default, ?STD_IMPORTANCE, Format, [], [])`](`log/5`).".
-spec log(Format) -> ok
      when Format :: string().
log(Format) ->
    log(default,?STD_IMPORTANCE,Format,[],[]).

-doc(#{equiv => log(Category, Importance, Format, FormatArgs, [])}).
-spec log(X1, X2) -> ok
      when X1 :: Category | Importance | Format,
           X2 :: Format | FormatArgs,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list().
log(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    log(Category,Importance,Format,Args,[]).

-doc(#{equiv => log(Category, Importance, Format, FormatArgs, Opts)}).
-spec log(X1, X2, X3) -> ok
      when X1 :: Category | Importance,
           X2 :: Importance | Format,
           X3 :: Format | FormatArgs | Opts,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()} | no_css | esc_chars.
log(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    log(Category,Importance,Format,Args,Opts).

-doc(#{equiv => log(Category, Importance, Format, FormatArgs, Opts), since => <<"OTP R15B02">>}).
-spec log(X1, X2, X3, X4) -> ok
      when X1 :: Category | Importance,
           X2 :: Importance | Format,
           X3 :: Format | FormatArgs,
           X4 :: FormatArgs | Opts,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()} | no_css | esc_chars.
log(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    log(Category,Importance,Format,Args,Opts).

-doc """
Prints from a test case to the log file.

This function is meant for printing a string directly from a test case to the
test case log file.

Default `Category` is `default`, default `Importance` is `?STD_IMPORTANCE`, and
default value for `FormatArgs` is `[]`.

For details on `Category`, `Importance` and the `no_css` option, see section
[Logging - Categories and Verbosity Levels](write_test_chapter.md#logging) in
the User's Guide.

Common Test will not escape special HTML characters (<, > and &) in the text
printed with this function, unless the `esc_chars` option is used.
""".
-doc(#{since => <<"OTP 18.3">>}).
-spec log(Category, Importance, Format, FormatArgs, Opts) -> ok
      when Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()} | no_css | esc_chars.
log(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_log(Category,Importance,Format,Args,Opts).

-doc "Equivalent to [`print(default, ?STD_IMPORTANCE, Format, [], [])`](`print/5`).".
-spec print(Format) -> ok
      when Format :: string().
print(Format) ->
    print(default,?STD_IMPORTANCE,Format,[],[]).

-doc(#{equiv => print(Category, Importance, Format, FormatArgs, []), since => <<"OTP R15B02">>}).
-spec print(X1, X2) -> ok
      when X1 :: Category | Importance | Format,
           X2 :: Format | FormatArgs,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list().
print(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    print(Category,Importance,Format,Args,[]).

-doc(#{equiv => print(Category, Importance, Format, FormatArgs, Opts)}).
-spec print(X1, X2, X3) -> ok
      when X1 :: Category | Importance,
           X2 :: Importance | Format,
           X3 :: Format | FormatArgs | Opts,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()}.
print(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    print(Category,Importance,Format,Args,Opts).

-doc(#{equiv => print(Category, Importance, Format, FormatArgs, Opts), since => <<"OTP R15B02">>}).
-spec print(X1, X2, X3, X4) -> ok
      when X1 :: Category | Importance,
           X2 :: Importance | Format,
           X3 :: Format | FormatArgs,
           X4 :: FormatArgs | Opts,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()}.
print(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    print(Category,Importance,Format,Args,Opts).

-doc """
Prints from a test case to the console.

This function is meant for printing a string from a test case to the console.

Default `Category` is `default`, default `Importance` is `?STD_IMPORTANCE`, and
default value for `FormatArgs` is `[]`.

For details on `Category` and `Importance`, see section
[Logging - Categories and Verbosity Levels](write_test_chapter.md#logging) in
the User's Guide.
""".
-doc(#{since => <<"OTP 19.2">>}).
-spec print(Category, Importance, Format, FormatArgs, Opts) -> ok
      when Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()}.
print(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_print(Category,Importance,Format,Args,Opts).

-doc "Equivalent to [`pal(default, ?STD_IMPORTANCE, Format, [])`](`pal/4`).".
-spec pal(Format) -> ok
      when Format :: string().
pal(Format) ->
    pal(default,?STD_IMPORTANCE,Format,[]).

-doc(#{equiv => pal(Category, Importance, Format, FormatArgs, [])}).
-spec pal(X1, X2) -> ok
      when X1 :: Category | Importance | Format,
           X2 :: Format | FormatArgs,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list().
pal(X1,X2) ->
    {Category,Importance,Format,Args} = 
	if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
	   is_integer(X1) -> {default,X1,X2,[]};
	   is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
	end,
    pal(Category,Importance,Format,Args,[]).

-doc(#{equiv => pal(Category, Importance, Format, FormatArgs, Opts)}).
-spec pal(X1, X2, X3) -> ok
      when X1 :: Category | Importance,
           X2 :: Importance | Format,
           X3 :: Format | FormatArgs | Opt,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opt :: {heading,string()} | no_css.
pal(X1,X2,X3) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[],[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,[]};
	   is_integer(X1)              -> {default,X1,X2,X3,[]};
	   is_list(X1), is_list(X2)    -> {default,?STD_IMPORTANCE,X1,X2,X3}
	end,
    pal(Category,Importance,Format,Args,Opts).

-doc(#{equiv => pal(Category, Importance, Format, FormatArgs, Opts), since => <<"OTP R15B02">>}).
-spec pal(X1, X2, X3, X4) -> ok
      when X1 :: Category | Importance,
           X2 :: Importance | Format,
           X3 :: Format | FormatArgs,
           X4 :: FormatArgs | Opts,
           Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()} | no_css.
pal(X1,X2,X3,X4) ->
    {Category,Importance,Format,Args,Opts} = 
	if is_atom(X1), is_integer(X2) -> {X1,X2,X3,X4,[]};
	   is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3,X4};
	   is_integer(X1)              -> {default,X1,X2,X3,X4}
	end,
    pal(Category,Importance,Format,Args,Opts).

-doc """
Prints and logs from a test case.

This function is meant for printing a string from a test case, both to the test
case log file and to the console.

Default `Category` is `default`, default `Importance` is `?STD_IMPORTANCE`, and
default value for `FormatArgs` is `[]`.

For details on `Category` and `Importance`, see section
[Logging - Categories and Verbosity Levels](write_test_chapter.md#logging) in
the User's Guide.

Note that special characters in the text (<, > and &) will be escaped by Common
Test before the text is printed to the log file.
""".
-doc(#{since => <<"OTP 19.2">>}).
-spec pal(Category, Importance, Format, FormatArgs, Opts) -> ok
      when Category :: atom() | integer() | string(),
           Importance :: integer(),
           Format :: string(),
           FormatArgs :: list(),
           Opts :: [Opt],
           Opt :: {heading,string()} | no_css.
pal(Category,Importance,Format,Args,Opts) ->
    ct_logs:tc_pal(Category,Importance,Format,Args,Opts).

-doc """
Use this function to set, or modify, the verbosity level for a logging category.
See the [User's Guide](write_test_chapter.md#logging) for details. Use the value
`default` to set the general verbosity level.
""".
-doc(#{since => <<"OTP 19.1">>}).
-spec set_verbosity(Category, Level) -> ok
      when Category :: default | atom(),
           Level :: integer().
set_verbosity(Category, Level) ->
    ct_util:set_verbosity({Category,Level}).

-doc """
This function returns the verbosity level for the specified logging category.
See the [User's Guide](write_test_chapter.md#logging) for details. Use the value
`default` to read the general verbosity level.
""".
-doc(#{since => <<"OTP 19.1">>}).
-spec get_verbosity(Category) -> Level | undefined
      when Category :: default | atom(),
           Level :: integer().
get_verbosity(Category) ->
    ct_util:get_verbosity(Category).

-doc """
Starts capturing all text strings printed to `stdout` during execution of the
test case.

See also [`ct:capture_get/1`](`capture_get/1`),
[`ct:capture_stop/0`](`capture_stop/0`).
""".
-doc(#{since => <<"OTP R15B">>}).
-spec capture_start() -> ok.
capture_start() ->
    test_server:capture_start().

-doc """
Stops capturing text strings (a session started with `capture_start/0`).

See also [`ct:capture_get/1`](`capture_get/1`),
[`ct:capture_start/0`](`capture_start/0`).
""".
-doc(#{since => <<"OTP R15B">>}).
-spec capture_stop() -> ok.
capture_stop() ->
    test_server:capture_stop().

-doc(#{equiv => capture_get([default]), since => <<"OTP R15B">>}).
-spec capture_get() -> ListOfStrings
      when ListOfStrings :: [string()].
capture_get() ->
    %% remove default log printouts (e.g. ct:log/2 printouts)
    capture_get([default]).

-doc """
Returns and purges the list of text strings buffered during the latest session
of capturing printouts to `stdout`. Log categories that are to be ignored in
`ListOfStrings` can be specified with `ExclCategories`. If
`ExclCategories = []`, no filtering takes place.

See also [`ct:capture_start/0`](`capture_start/0`),
[`ct:capture_stop/0`](`capture_stop/0`), [`ct:log/3`](`log/3`).
""".
-doc(#{since => <<"OTP R15B">>}).
-spec capture_get(ExclCategories) -> ListOfStrings
      when ExclCategories :: [atom()],
           ListOfStrings :: [string()].
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


-doc "Terminates a test case with the specified error `Reason`.".
-spec fail(Reason) -> no_return()
      when Reason :: term().
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


-doc """
Terminates a test case with an error message specified by a format string and a
list of values (used as arguments to `io_lib:format/2`).
""".
-doc(#{since => <<"OTP R15B">>}).
-spec fail(Format, Args) -> no_return()
      when Format :: io:format(),
           Args :: [term()].
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

-doc """
Prints the specified `Comment` in the comment field in the table on the test
suite result page.

If called several times, only the last comment is printed. The test case return
value `{comment,Comment}` overwrites the string set by this function.
""".
-spec comment(Comment) -> ok
      when Comment :: term().
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

-doc """
Prints the formatted string in the comment field in the table on the test suite
result page.

Arguments `Format` and `Args` are used in a call to `io_lib:format/2` to create
the comment string. The behavior of [`comment/2`](`comment/2`) is otherwise the
same as function [`ct:comment/1`](`comment/1`).
""".
-doc(#{since => <<"OTP R15B">>}).
-spec comment(Format, Args) -> ok
      when Format :: string(),
           Args :: list().
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

-doc """
If the test is started with option `create_priv_dir` set to `manual_per_tc`, in
order for the test case to use the private directory, it must first create it by
calling this function.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec make_priv_dir() -> ok | {error, Reason}
      when Reason :: term().
make_priv_dir() ->
    test_server:make_priv_dir().

-doc "Returns the name of the target that the specified connection belongs to.".
-spec get_target_name(Handle) -> {ok, TargetName} | {error, Reason}
      when Handle :: handle(),
           TargetName :: target_name(),
           Reason :: term().
get_target_name(Handle) ->
    ct_util:get_target_name(Handle).

-doc """
Returns the command used to start this Erlang instance. If this information
could not be found, the string `"no_prog_name"` is returned.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec get_progname() -> string().
get_progname() ->
    case init:get_argument(progname) of
	{ok, [[Prog]]} ->
	    Prog;
	_Other ->
	    "no_prog_name"
    end.

-doc """
Parses the printout from an SQL table and returns a list of tuples.

The printout to parse is typically the result of a `select` command in SQL. The
returned `Table` is a list of tuples, where each tuple is a row in the table.

`Heading` is a tuple of strings representing the headings of each column in the
table.
""".
-spec parse_table(Data) -> {Heading, Table}
      when Data :: [string()],
           Heading :: tuple(),
           Table :: [tuple()].
parse_table(Data) ->
    ct_util:parse_table(Data).

-doc """
Performs command `listenv` on the specified Telnet connection and returns the
result as a list of key-value pairs.
""".
-spec listenv(Telnet) -> {'ok', [Env]} | {'error', Reason}
              when Telnet :: ct_telnet:connection(),
                   Env :: {Key, Value},
                   Key :: string(),
                   Value :: string(),
                   Reason :: term().
listenv(Telnet) ->
    ct_util:listenv(Telnet).

-doc "Returns all test cases in the specified suite.".
-spec testcases(TestDir, Suite) -> Testcases | {error, Reason}
      when TestDir :: string(),
           Suite :: atom(),
           Testcases :: list(),
           Reason :: term().
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

-doc """
Returns any data specified with tag `userdata` in the list of tuples returned
from [`suite/0`](`c:ct_suite:suite/0`).
""".
-spec userdata(TestDir, Suite) -> SuiteUserData | {error, Reason}
      when TestDir :: string(),
           Suite :: atom(),
           SuiteUserData :: [term()],
           Reason :: term().
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
   
-doc """
Returns any data specified with tag `userdata` in the list of tuples returned
from `Suite:group(GroupName)` or `Suite:Case()`.
""".
-spec userdata(TestDir, Suite, Case::GroupOrCase) -> TCUserData | {error, Reason}
      when TestDir :: string(),
           Suite :: atom(),
           GroupOrCase :: {group, GroupName} | atom(),
           GroupName :: atom(),
           TCUserData :: [term()],
           Reason :: term().
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

-doc """
Returns status of ongoing test. The returned list contains information about
which test case is executing (a list of cases when a parallel test case group is
executing), as well as counters for successful, failed, skipped, and total test
cases so far.
""".
-spec get_status() -> TestStatus | {error, Reason} | no_tests_running
      when TestStatus :: [StatusElem],
           StatusElem :: {current, TestCaseInfo} | {successful, Successful} | {failed, Failed} | {skipped, Skipped} | {total, Total},
           TestCaseInfo :: {Suite, TestCase} | [{Suite, TestCase}],
           Suite :: atom(),
           TestCase :: atom(),
           Successful :: integer(),
           Failed :: integer(),
           Skipped :: {UserSkipped, AutoSkipped},
           UserSkipped :: integer(),
           AutoSkipped :: integer(),
           Total :: integer(),
           Reason :: term().
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

-doc """
Aborts the currently executing test case. The user must know with certainty
which test case is currently executing. The function is therefore only safe to
call from a function that has been called (or synchronously invoked) by the test
case.

`Reason`, the reason for aborting the test case, is printed in the test case
log.
""".
-spec abort_current_testcase(Reason) -> ok | {error, ErrorReason}
      when Reason :: term(),
           ErrorReason :: no_testcase_running | parallel_group. 
abort_current_testcase(Reason) ->
    test_server_ctrl:abort_current_testcase(Reason).

-doc """
Gets a reference to the `Common Test` event manager. The reference can be used
to, for example, add a user-specific event handler while tests are running.

_Example:_

```erlang
gen_event:add_handler(ct:get_event_mgr_ref(), my_ev_h, [])
```
""".
-doc(#{since => <<"OTP 17.5">>}).
-spec get_event_mgr_ref() -> EvMgrRef
      when EvMgrRef :: atom().
get_event_mgr_ref() ->
    ?CT_EVMGR_REF.

-doc """
Encrypts the source configuration file with DES3 and saves the result in file
`EncryptFileName`. The key, a string, must be available in a text file named
`.ct_config.crypt`, either in the current directory, or the home directory of
the user (it is searched for in that order).

For information about using encrypted configuration files when running tests,
see section
[Encrypted Configuration Files](config_file_chapter.md#encrypted_config_files)
in the User's Guide.

For details on DES3 encryption/decryption, see application
[`Crypto`](`e:crypto:index.html`).
""".
-spec encrypt_config_file(SrcFileName, EncryptFileName) -> ok | {error, Reason}
      when SrcFileName :: string(),
           EncryptFileName :: string(),
           Reason :: term().
encrypt_config_file(SrcFileName, EncryptFileName) ->
    ct_config:encrypt_config_file(SrcFileName, EncryptFileName).

-doc """
Encrypts the source configuration file with DES3 and saves the result in the
target file `EncryptFileName`. The encryption key to use is either the value in
`{key,Key}` or the value stored in the file specified by `{file,File}`.

For information about using encrypted configuration files when running tests,
see section
[Encrypted Configuration Files](config_file_chapter.md#encrypted_config_files)
in the User's Guide.

For details on DES3 encryption/decryption, see application
[`Crypto`](`e:crypto:index.html`).
""".
-spec encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile) -> ok | {error, Reason}
      when SrcFileName :: string(),
           EncryptFileName :: string(),
           KeyOrFile :: {key, string()} | {file, string()},
           Reason :: term().
encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile) ->
    ct_config:encrypt_config_file(SrcFileName, EncryptFileName, KeyOrFile).

-doc """
Decrypts `EncryptFileName`, previously generated with
[`ct:encrypt_config_file/2,3`](`encrypt_config_file/2`). The original file
contents is saved in the target file. The encryption key, a string, must be
available in a text file named `.ct_config.crypt`, either in the current
directory, or the home directory of the user (it is searched for in that order).
""".
-spec decrypt_config_file(EncryptFileName, TargetFileName) -> ok | {error, Reason}
     when EncryptFileName :: string(),
          TargetFileName :: string(),
          Reason :: term().
decrypt_config_file(EncryptFileName, TargetFileName) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName).

-doc """
Decrypts `EncryptFileName`, previously generated with
[`ct:encrypt_config_file/2,3`](`encrypt_config_file/2`). The original file
contents is saved in the target file. The key must have the same value as that
used for encryption.
""".
-spec decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile) -> ok | {error, Reason}
      when EncryptFileName :: string(),
           TargetFileName :: string(),
           KeyOrFile :: {key, string()} | {file, string()},
           Reason :: term().
decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile) ->
    ct_config:decrypt_config_file(EncryptFileName, TargetFileName, KeyOrFile).

-doc """
Loads configuration variables using the specified callback module and
configuration string. The callback module is to be either loaded or present in
the code path. Loaded configuration variables can later be removed using
function [`ct:remove_config/2`](`remove_config/2`).
""".
-doc(#{since => <<"OTP R14B">>}).
-spec add_config(Callback, Config) -> ok | {error, Reason}
      when Callback :: atom(),
           Config :: string(),
           Reason :: term().
add_config(Callback, Config)->
    ct_config:add_config(Callback, Config).

-doc """
Removes configuration variables (together with their aliases) that were loaded
with specified callback module and configuration string.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec remove_config(Callback, Config) -> ok
      when Callback :: atom(),
           Config :: string().
remove_config(Callback, Config) ->
    ct_config:remove_config(Callback, Config).

-doc """
Sets a new timetrap for the running test case.

If the argument is `Func`, the timetrap is triggered when this function returns.
`Func` can also return a new `Time` value, which in that case is the value for
the new timetrap.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec timetrap(Time) -> infinity | pid()
      when Time :: {hours, Hours} | {minutes, Mins} | {seconds, Secs} | timeout() | Func,
           Hours :: integer(),
           Mins :: integer(),
           Secs :: integer(),
           Func :: {M, F, A} | function(),
           M :: atom(),
           F :: atom(),
           A :: list().
timetrap(Time) ->
    test_server:timetrap_cancel(),
    test_server:timetrap(Time).

-doc """
Reads information about the timetrap set for the current test case. `Scaling`
indicates if `Common Test` will attempt to compensate timetraps automatically
for runtime delays introduced by, for example, tools like cover. `ScaleVal` is
the value of the current scaling multiplier (always 1 if scaling is disabled).
Note the `Time` is not the scaled result.
""".
-doc(#{since => <<"OTP R15B">>}).
-spec get_timetrap_info() -> {Time, {Scaling,ScaleVal}}
      when Time :: timeout(),
           Scaling :: boolean(),
           ScaleVal :: integer().
get_timetrap_info() ->
    test_server:get_timetrap_info().

-doc """
This function, similar to `timer:sleep/1` in STDLIB, suspends the test case for
a specified time. However, this function also multiplies `Time` with the
`multiply_timetraps` value (if set) and under certain circumstances also scales
up the time automatically if `scale_timetraps` is set to `true` (default is
`false`).
""".
-doc(#{since => <<"OTP R14B">>}).
-spec sleep(Time) -> ok
      when Time :: {hours, Hours} | {minutes, Mins} | {seconds, Secs} | Millisecs,
           Hours :: integer(),
           Mins :: integer(),
           Secs :: integer(),
           Millisecs :: timeout() | float().
sleep({hours,Hs}) ->
    sleep(trunc(Hs * 1000 * 60 * 60));
sleep({minutes,Ms}) ->
    sleep(trunc(Ms * 1000 * 60));
sleep({seconds,Ss}) ->
    sleep(trunc(Ss * 1000));
sleep(Time) ->
    test_server:adjusted_sleep(Time).

-doc """
Sends an asynchronous notification of type `Name` with `Data`to the Common Test
event manager. This can later be caught by any installed event manager.

See also `m:gen_event`.
""".
-doc(#{since => <<"OTP R15B02">>}).
-spec notify(Name, Data) -> ok
      when Name :: atom(),
           Data :: term().
notify(Name,Data) ->
    ct_event:notify(Name, Data).

-doc """
Sends a synchronous notification of type `Name` with `Data` to the `Common Test`
event manager. This can later be caught by any installed event manager.

See also `m:gen_event`.
""".
-doc(#{since => <<"OTP R15B02">>}).
-spec sync_notify(Name, Data) -> ok
      when Name :: atom(),
           Data :: term().
sync_notify(Name,Data) ->
    ct_event:sync_notify(Name, Data).

-doc """
Cancels any active timetrap and pauses the execution of the current test case
until the user calls function `continue/0`. The user can then interact with the
Erlang node running the tests, for example, for debugging purposes or for
manually executing a part of the test case. If a parallel group is executing,
[`ct:break/2`](`break/2`) is to be called instead.

A cancelled timetrap is not automatically reactivated after the break, but must
be started explicitly with [`ct:timetrap/1`](`timetrap/1`).

In order for the break/continue functionality to work, `Common Test` must
release the shell process controlling `stdin`. This is done by setting start
option `release_shell` to `true`. For details, see section
[Running Tests from the Erlang Shell or from an Erlang Program](run_test_chapter.md#erlang_shell_or_program)
in the User's Guide.
""".
-doc(#{since => <<"OTP R15B02">>}).
-spec break(Comment) -> ok | {error, Reason}
      when Comment :: string(),
           Reason :: {multiple_cases_running, TestCases} | 'enable break with release_shell option',
           TestCases :: [atom()].
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

-doc """
Works the same way as [`ct:break/1`](`break/1`), only argument `TestCase` makes
it possible to pause a test case executing in a parallel group. Function
[`ct:continue/1`](`continue/1`) is to be used to resume execution of `TestCase`.

For details, see [`ct:break/1`](`break/1`).
""".
-doc(#{since => <<"OTP R15B02">>}).
-spec break(TestCase, Comment) -> ok | {error, Reason}
      when TestCase :: atom(),
           Comment :: string(),
           Reason :: 'test case not running' | 'enable break with release_shell option'.
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

-doc """
This function must be called to continue after a test case (not executing in a
parallel group) has called function [`ct:break/1`](`break/1`).
""".
-doc(#{since => <<"OTP R15B02">>}).
-spec continue() -> ok.
continue() -> 
    test_server:continue().

-doc """
This function must be called to continue after a test case has called
[`ct:break/2`](`break/2`). If the paused test case, `TestCase`, executes in a
parallel group, this function, rather than `continue/0`, must be used to let the
test case proceed.
""".
-doc(#{since => <<"OTP R15B02">>}).
-spec continue(TestCase) -> ok
      when TestCase :: atom().
continue(TestCase) -> 
    test_server:continue(TestCase).


-doc """
This function will return the identity of test- and group leader processes that
are still running at the time of this call. `TestProcs` are processes in the
system that have a Common Test IO process as group leader. `SharedGL` is the
central Common Test IO process, responsible for printing to log files for
configuration functions and sequentially executing test cases. `OtherGLs` are
Common Test IO processes that print to log files for test cases in parallel test
case groups.

The process information returned by this function may be used to locate and
terminate remaining processes after tests have finished executing. The function
would typically by called from Common Test Hook functions.

Note that processes that execute configuration functions or test cases are never
included in `TestProcs`. It is therefore safe to use post configuration hook
functions (such as post_end_per_suite, post_end_per_group,
post_end_per_testcase) to terminate all processes in `TestProcs` that have the
current group leader process as its group leader.

Note also that the shared group leader (`SharedGL`) must never be terminated by
the user, only by Common Test. Group leader processes for parallel test case
groups (`OtherGLs`) may however be terminated in post_end_per_group hook
functions.
""".
-doc(#{since => <<"OTP 20.2">>}).
-spec remaining_test_procs() -> {TestProcs,SharedGL,OtherGLs}
      when TestProcs :: [{pid(),GL}],
           GL :: pid(),
           SharedGL :: pid(),
           OtherGLs :: [pid()].
remaining_test_procs() ->
    ct_util:remaining_test_procs().
