<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# External Configuration Data

[](){: #top }

## General

To avoid hard-coding data values related to the test and/or System Under Test
(SUT) in the test suites, the data can instead be specified through
configuration files or strings that `Common Test` reads before the start of a
test run. External configuration data makes it possible to change test
properties without modifying the test suites using the data. Examples of
configuration data follows:

- Addresses to the test plant or other instruments
- User login information
- Names of files needed by the test
- Names of programs to be executed during the test
- Any other variable needed by the test

## Syntax

A configuration file can contain any number of elements of the type:

```text
 {CfgVarName,Value}.
```

where

```erlang
 CfgVarName = atom()
 Value = term() | [{CfgVarName,Value}]
```

## Requiring and Reading Configuration Data

[](){: #require_config_data }

In a test suite, one must _require_ that a configuration variable (`CfgVarName`
in the previous definition) exists before attempting to read the associated
value in a test case or configuration function.

`require` is an assert statement, which can be part of the
[Test Suite Information Function](write_test_chapter.md#suite) or
[Test Case Information Function](write_test_chapter.md#info_function). If the
required variable is unavailable, the test is skipped (unless a default value
has been specified, see section
[Test Case Information Function](write_test_chapter.md#info_function) for
details). Also, function [`ct:require/1/2`](`ct:require/1`) can be called from a
test case to check if a specific variable is available. The return value from
this function must be checked explicitly and appropriate action be taken
depending on the result (for example, to skip the test case if the variable in
question does not exist).

A `require` statement in the test suite information case or test case
information-list is to look like `{require,CfgVarName}` or
`{require,AliasName,CfgVarName}`. The arguments `AliasName` and `CfgVarName` are
the same as the arguments to [`ct:require/1,2`](`ct:require/1`). `AliasName`
becomes an alias for the configuration variable, and can be used as reference to
the configuration data value. The configuration variable can be associated with
any number of alias names, but each name must be unique within the same test
suite. The two main uses for alias names follows:

- To identify connections (described later).
- To help adapt configuration data to a test suite (or test case) and improve
  readability.

To read the value of a configuration variable, use function
[`get_config/1,2,3`](`ct:get_config/1`).

_Example:_

```erlang
 suite() ->
     [{require, domain, 'CONN_SPEC_DNS_SUFFIX'}].

 ...

 testcase(Config) ->
     Domain = ct:get_config(domain),
     ...
```

## Using Configuration Variables Defined in Multiple Files

If a configuration variable is defined in multiple files and you want to access
all possible values, use function `ct:get_config/3` and specify `all` in the
options list. The values are then returned in a list and the order of the
elements corresponds to the order that the configuration files were specified at
startup.

## Encrypted Configuration Files

[](){: #encrypted_config_files }

Configuration files containing sensitive data can be encrypted if they must be
stored in open and shared directories.

To have `Common Test` encrypt a specified file using function `DES3` in
application `Crypto`, call
[`ct:encrypt_config_file/2,3`](`ct:encrypt_config_file/2`) The encrypted file
can then be used as a regular configuration file in combination with other
encrypted files or normal text files. However, the key for decrypting the
configuration file must be provided when running the test. This can be done with
flag/option `decrypt_key` or `decrypt_file`, or a key file in a predefined
location.

`Common Test` also provides decryption functions,
[`ct:decrypt_config_file/2,3`](`ct:decrypt_config_file/2`), for recreating the
original text files.

## Opening Connections Using Configuration Data

Two different methods for opening a connection using the support functions in,
for example, `m:ct_ssh`, `m:ct_ftp`, and `m:ct_telnet` follows:

- Using a configuration target name (an alias) as reference.
- Using the configuration variable as reference.

When a target name is used for referencing the configuration data (that
specifies the connection to be opened), the same name can be used as connection
identity in all subsequent calls related to the connection (also for closing
it). Only one open connection per target name is possible. If you attempt to
open a new connection using a name already associated with an open connection,
`Common Test` returns the already existing handle so the previously opened
connection is used. This feature makes it possible to call the function for
opening a particular connection whenever useful. An action like this does not
necessarily open any new connections unless it is required (which could be the
case if, for example, the previous connection has been closed unexpectedly by
the server). Using named connections also removes the need to pass handle
references around in the suite for these connections.

When a configuration variable name is used as reference to the data specifying
the connection, the handle returned as a result of opening the connection must
be used in all subsequent calls (also for closing the connection). Repeated
calls to the open function with the same variable name as reference results in
multiple connections being opened. This can be useful, for example, if a test
case needs to open multiple connections to the same server on the target node
(using the same configuration data for each connection).

## User-Specific Configuration Data Formats

The user can specify configuration data on a different format than key-value
tuples in a text file, as described so far. The data can, for example, be read
from any files, fetched from the web over HTTP, or requested from a
user-specific process. To support this, `Common Test` provides a callback module
plugin mechanism to handle configuration data.

### Default Callback Modules for Handling Configuration Data

`Common Test` includes default callback modules for handling configuration data
specified in standard configuration files (described earlier) and in XML files
as follows:

- `ct_config_plain` \- for reading configuration files with key-value tuples
  (standard format). This handler is used to parse configuration files if no
  user callback is specified.
- `ct_config_xml` \- for reading configuration data from XML files.

### Using XML Configuration Files

An example of an XML configuration file follows:

```c

 <config>
    <ftp_host>
        <ftp>"targethost"</ftp>
        <username>"tester"</username>
        <password>"letmein"</password>
    </ftp_host>
    <lm_directory>"/test/loadmodules"</lm_directory>
 </config>
```

Once read, this file produces the same configuration variables as the following
text file:

```erlang
 {ftp_host, [{ftp,"targethost"},
             {username,"tester"},
             {password,"letmein"}]}.

 {lm_directory, "/test/loadmodules"}.
```

### Implement a User-Specific Handler

The user-specific handler can be written to handle special configuration file
formats. The parameter can be either file names or configuration strings (the
empty list is valid).

The callback module implementing the handler is responsible for checking the
correctness of configuration strings.

To validate the configuration strings, the callback module is to have function
`Callback:check_parameter/1` exported.

The input argument is passed from `Common Test`, as defined in the test
specification, or specified as an option to `ct_run` or `ct:run_test`.

The return value is to be any of the following values, indicating if the
specified configuration parameter is valid:

- `{ok, {file, FileName}}` \- the parameter is a file name and the file exists.
- `{ok, {config, ConfigString}}` \- the parameter is a configuration string and
  it is correct.
- `{error, {nofile, FileName}}` \- there is no file with the specified name in
  the current directory.
- `{error, {wrong_config, ConfigString}}` \- the configuration string is wrong.

The function `Callback:read_config/1` is to be exported from the callback module
to read configuration data, initially before the tests start, or as a result of
data being reloaded during test execution. The input argument is the same as for
function `check_parameter/1`.

The return value is to be either of the following:

- `{ok, Config}` \- if the configuration variables are read successfully.
- `{error, {Error, ErrorDetails}}` \- if the callback module fails to proceed
  with the specified configuration parameters.

`Config` is the proper Erlang key-value list, with possible key-value sublists
as values, like the earlier configuration file example:

```erlang
 [{ftp_host, [{ftp, "targethost"}, {username, "tester"}, {password, "letmein"}]},
  {lm_directory, "/test/loadmodules"}]
```

## Examples of Configuration Data Handling

A configuration file for using the FTP client to access files on a remote host
can look as follows:

```erlang
 {ftp_host, [{ftp,"targethost"},
	     {username,"tester"},
	     {password,"letmein"}]}.

 {lm_directory, "/test/loadmodules"}.
```

The XML version shown earlier can also be used, but it is to be explicitly
specified that the `ct_config_xml` callback module is to be used by
`Common Test`.

The following is an example of how to assert that the configuration data is
available and can be used for an FTP session:

```erlang
 init_per_testcase(ftptest, Config) ->
     {ok,_} = ct_ftp:open(ftp),
     Config.

 end_per_testcase(ftptest, _Config) ->
     ct_ftp:close(ftp).

 ftptest() ->
     [{require,ftp,ftp_host},
      {require,lm_directory}].

 ftptest(Config) ->
     Remote = filename:join(ct:get_config(lm_directory), "loadmodX"),
     Local = filename:join(proplists:get_value(priv_dir,Config), "loadmodule"),
     ok = ct_ftp:recv(ftp, Remote, Local),
     ...
```

The following is an example of how the functions in the previous example can be
rewritten if it is necessary to open multiple connections to the FTP server:

```erlang
 init_per_testcase(ftptest, Config) ->
     {ok,Handle1} = ct_ftp:open(ftp_host),
     {ok,Handle2} = ct_ftp:open(ftp_host),
     [{ftp_handles,[Handle1,Handle2]} | Config].

 end_per_testcase(ftptest, Config) ->
     lists:foreach(fun(Handle) -> ct_ftp:close(Handle) end,
                   proplists:get_value(ftp_handles,Config)).

 ftptest() ->
     [{require,ftp_host},
      {require,lm_directory}].

 ftptest(Config) ->
     Remote = filename:join(ct:get_config(lm_directory), "loadmodX"),
     Local = filename:join(proplists:get_value(priv_dir,Config), "loadmodule"),
     [Handle | MoreHandles] = proplists:get_value(ftp_handles,Config),
     ok = ct_ftp:recv(Handle, Remote, Local),
     ...
```

## Example of User-Specific Configuration Handler

A simple configuration handling driver, asking an external server for
configuration data, can be implemented as follows:

```erlang
 -module(config_driver).
 -export([read_config/1, check_parameter/1]).

 read_config(ServerName)->
     ServerModule = list_to_atom(ServerName),
     ServerModule:start(),
     ServerModule:get_config().

 check_parameter(ServerName)->
     ServerModule = list_to_atom(ServerName),
     case code:is_loaded(ServerModule) of
         {file, _}->
             {ok, {config, ServerName}};
         false->
             case code:load_file(ServerModule) of
                 {module, ServerModule}->
                     {ok, {config, ServerName}};
                 {error, nofile}->
                     {error, {wrong_config, "File not found: " ++ ServerName ++ ".beam"}}
             end
     end.
```

The configuration string for this driver can be `config_server`, if the
`config_server.erl` module that follows is compiled and exists in the code path
during test execution:

```erlang
 -module(config_server).
 -export([start/0, stop/0, init/1, get_config/0, loop/0]).

 -define(REGISTERED_NAME, ct_test_config_server).

 start()->
     case whereis(?REGISTERED_NAME) of
         undefined->
             spawn(?MODULE, init, [?REGISTERED_NAME]),
             wait();
         _Pid->
         ok
     end,
     ?REGISTERED_NAME.

 init(Name)->
     register(Name, self()),
     loop().

 get_config()->
     call(self(), get_config).

 stop()->
     call(self(), stop).

 call(Client, Request)->
     case whereis(?REGISTERED_NAME) of
         undefined->
             {error, {not_started, Request}};
         Pid->
             Pid ! {Client, Request},
             receive
                 Reply->
                     {ok, Reply}
             after 4000->
                 {error, {timeout, Request}}
             end
     end.

 loop()->
     receive
         {Pid, stop}->
             Pid ! ok;
         {Pid, get_config}->
             {D,T} = erlang:localtime(),
             Pid !
                 [{localtime, [{date, D}, {time, T}]},
                  {node, erlang:node()},
                  {now, erlang:now()},
                  {config_server_pid, self()},
                  {config_server_vsn, ?vsn}],
             ?MODULE:loop()
     end.

 wait()->
     case whereis(?REGISTERED_NAME) of
         undefined->
             wait();
         _Pid->
             ok
     end.
```

Here, the handler also provides for dynamically reloading of configuration
variables. If [`ct:reload_config(localtime)`](`ct:reload_config/1`) is called
from the test case function, all variables loaded with
`config_driver:read_config/1` are updated with their latest values, and the new
value for variable `localtime` is returned.
