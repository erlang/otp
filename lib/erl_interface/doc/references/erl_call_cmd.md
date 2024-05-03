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
# erl_call

Call/start a distributed Erlang node.

## Description

`erl_call` makes it possible to start and/or communicate with a distributed
Erlang node. It is built upon the `Erl_Interface` library as an example
application. Its purpose is to use a Unix shell script to interact with a
distributed Erlang node. It performs all communication with the Erlang _rex
server_, using the standard Erlang RPC facility. It does not require any special
software to be run at the Erlang target node.

The main use is to either start a distributed Erlang node or to make an ordinary
function call. However, it is also possible to pipe an Erlang module to
`erl_call` and have it compiled, or to pipe a sequence of Erlang expressions to
be evaluated (similar to the Erlang shell).

Options, which cause `stdin` to be read, can be used with advantage, as scripts
from within (Unix) shell scripts. Another nice use of `erl_call` could be from
(HTTP) CGI-bin scripts.

## erl_call <options>

Starts/calls Erlang.

Each option flag is described below with its name, type, and meaning.

- **`-a [Mod [Fun [Args]]]]`** - (_Optional._) Applies the specified function
  and returns the result. `Mod` must be specified. However, `start` and `[]` are
  assumed for unspecified `Fun` and `Args`, respectively. `Args` is to be in the
  same format as for `erlang:apply/3` in `ERTS` except only a subset of all
  terms are allowed. The allowed term types are: `list` (and `string`
  representation of list, that is "example"), `tuple`, `atom` and `number`.

  Notice that this flag takes exactly one argument, so quoting can be necessary
  to group `Mod`, `Fun`, and `Args` in a manner dependent on the behavior of
  your command shell.

- **`-address [Hostname:]Port`** - (One of `-n`, `-name`, `-sname` or `-address`
  is required.) `Hostname` is the hostname of the machine that is running the
  peer node that `erl_call` shall communicate with. The default hostname is the
  hostname of the local machine. `Port` is the port number of the node that
  `erl_call` shall communicate with. The `-address` flag cannot be combined with
  any of the flags `-n`, `-name`, `-sname` or `-s`.

  The `-address` flag is typically useful when one wants to call a node that is
  running on machine without an accessible [epmd](`e:erts:epmd_cmd.md`)
  instance.

- **`-c Cookie`** - (_Optional._) Use this option to specify a certain cookie.
  If no cookie is specified, the `~/.erlang.cookie` file is read and its content
  is used as cookie. The Erlang node we want to communicate with must have the
  same cookie.

- **`-d`** - (_Optional._) Debug mode. This causes all I/O to be output to the
  `~/.erl_call.out.Nodename` file, where `Nodename` is the node name of the
  Erlang node in question.

- **`-e`** - (_Optional._) Reads a sequence of Erlang expressions, separated by
  comma (,) and ended with a full stop (.), from `stdin` until EOF (Control-D).
  Evaluates the expressions and returns the result from the last expression.
  Returns `{ok,Result}` on success.

- **`-fetch_stdout`** - (_Optional._) Executes the code, specified with the `-a`
  or `-e` option, in a new process that has a
  [group leader](`erlang:group_leader/0`) that forwards all stdout (standard
  output) data so that it is printed to stdout of the `erl_call` process. This
  means that stdout data that are written during the execution of the called
  code, by the code and by descendant processes, will be forwarded (given that
  the group leader has not been changed by a call to `erlang:group_leader/2`).

  The printed data is UTF-8 encoded.

  This option is only relevant together with the option `-a` or `-e`.

  See the documentation of [the I/O protocol](`e:stdlib:io_protocol.md`), for
  more information about the group leader concept.

  > #### Note {: .info }
  >
  > This option only works when `erl_call` is interacting with a node with a
  > version greater or equal to OTP-24.

- **`-h HiddenName`** - (_Optional._) Specifies the name of the hidden node that
  `erl_call` represents.

- **`-m`** - (_Optional._) Reads an Erlang module from `stdin` and compiles it.

- **`-n Node`** - (One of `-n`, `-name`, `-sname` or `-address` is required.)
  Has the same meaning as `-name` and can still be used for backward
  compatibility reasons.

- **`-name Node`** - (One of `-n`, `-name`, `-sname` or `-address` is required.)
  `Node` is the name of the peer node to be started or communicated with. It is
  assumed that `Node` is started with `erl -name`, which means that fully
  qualified long node names are used. If option `-s` is specified, an Erlang
  node will (if necessary) be started with `erl -name`.

- **`-no_result_term`** - (_Optional._) Do not print the result term. This
  option is only relevant together with the options `-a` and `-e`.

- **`-q`** - (_Optional._) Halts the Erlang node specified with switch `-n`.
  This switch overrides switch `-s`.

- **`-r`** - (_Optional._) Generates a random name of the hidden node that
  `erl_call` represents.

- **`-R`** - (_Optional._) Request a dynamic random name, of the hidden node
  that `erl_call` represents, from the peer node. Supported since OTP 23. Prefer
  `-R` over `-r` when doing repeated requests toward the same peer node.

- **`-s`** - (_Optional._) Starts a distributed Erlang node if necessary. This
  means that in a sequence of calls, where '`-s`' and '`-n Node`' are constant,
  only the first call starts the Erlang node. This makes the rest of the
  communication very fast. This flag is currently only available on Unix-like
  platforms (Linux, Mac OS X, Solaris, and so on).

- **`-sname Node`** - (One of `-n`, `-name`, `-sname` or `-address` is
  required.) `Node` is the name of the peer node to be started or communicated
  with. It is assumed that `Node` is started with `erl -sname`, which means that
  short node names are used. If option `-s` is specified, an Erlang node is
  started (if necessary) with `erl -sname`.

- **`-timeout Seconds`** - (_Optional._) Aborts the `erl_call` process after the
  timeout expires. Note that this does not abort commands that have already been
  started with `-a`, `-e`, or similar.

- **`-v`** - (_Optional._) Prints a lot of `verbose` information. This is only
  useful for the developer and maintainer of `erl_call`.

- **`-x ErlScript`** - (_Optional._) Specifies another name of the Erlang
  startup script to be used. If not specified, the standard `erl` startup script
  is used.

## Examples

To start an Erlang node and call `erlang:time/0`:

```text
erl_call -s -a 'erlang time' -n madonna
{18,27,34}
```

To terminate an Erlang node by calling `erlang:halt/0`:

```text
erl_call -s -a 'erlang halt' -n madonna
```

To apply with many arguments:

```text
erl_call -s -a 'lists seq [1,10]' -n madonna
```

To evaluate some expressions (_the input ends with EOF (Control-D)_):

```text
erl_call -s -e -n madonna
statistics(runtime),
X=1,
Y=2,
{_,T}=statistics(runtime),
{X+Y,T}.
^D
{ok,{3,0}}
```

To compile a module and run it (_again, the input ends with EOF (Control-D)_):

(In the example, the output has been formatted afterwards.)

```text
erl_call -s -m -a procnames -n madonna
-module(procnames).
-compile(export_all).
start() ->
        P = processes(),
        F = fun(X) -> {X,process_info(X,registered_name)} end,
        lists:map(F,[],P).
^D
[{<madonna@chivas.du.etx.ericsson.se,0,0>,
                  {registered_name,init}},
 {<madonna@chivas.du.etx.ericsson.se,2,0>,
                  {registered_name,erl_prim_loader}},
 {<madonna@chivas.du.etx.ericsson.se,4,0>,
                  {registered_name,error_logger}},
 {<madonna@chivas.du.etx.ericsson.se,5,0>,
                  {registered_name,application_controller}},
 {<madonna@chivas.du.etx.ericsson.se,6,0>,
                  {registered_name,kernel}},
 {<madonna@chivas.du.etx.ericsson.se,7,0>,
                  []},
 {<madonna@chivas.du.etx.ericsson.se,8,0>,
                  {registered_name,kernel_sup}},
 {<madonna@chivas.du.etx.ericsson.se,9,0>,
                  {registered_name,net_sup}},
 {<madonna@chivas.du.etx.ericsson.se,10,0>,
                  {registered_name,net_kernel}},
 {<madonna@chivas.du.etx.ericsson.se,11,0>,
                  []},
 {<madonna@chivas.du.etx.ericsson.se,12,0>,
                  {registered_name,global_name_server}},
 {<madonna@chivas.du.etx.ericsson.se,13,0>,
                  {registered_name,auth}},
 {<madonna@chivas.du.etx.ericsson.se,14,0>,
                  {registered_name,rex}},
 {<madonna@chivas.du.etx.ericsson.se,15,0>,
                  []},
 {<madonna@chivas.du.etx.ericsson.se,16,0>,
                  {registered_name,file_server}},
 {<madonna@chivas.du.etx.ericsson.se,17,0>,
                  {registered_name,code_server}},
 {<madonna@chivas.du.etx.ericsson.se,20,0>,
                  {registered_name,user}},
 {<madonna@chivas.du.etx.ericsson.se,38,0>,
                  []}]
```

To forward standard output without printing the result term (_again, the input
ends with EOF (Control-D)_):

```erlang
erl_call -s -e -sname madonna -fetch_stdout -no_result_term
io:format("Number of schedulers: ~p~n", [erlang:system_info(schedulers)]),
io:format("Number of logical cores: ~p~n", [erlang:system_info(logical_processors_available)]).
^D
Number of schedulers: 8
Number of logical cores: 8
```
