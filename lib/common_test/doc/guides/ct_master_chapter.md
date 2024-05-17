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
# Using Common Test for Large-Scale Testing

## General

Large-scale automated testing requires running multiple independent test
sessions in parallel. This is accomplished by running some `Common Test` nodes
on one or more hosts, testing different target systems. Configuring, starting,
and controlling the test nodes independently can be a cumbersome operation. To
aid this kind of automated large-scale testing, `Common Test` offers a master
test node component, `Common Test` Master, which handles central configuration
and control in a system of distributed `Common Test` nodes.

The `Common Test` Master server runs on one dedicated Erlang node and uses
distributed Erlang to communicate with any number of `Common Test` test nodes,
each hosting a regular `Common Test` server. Test specifications are used as
input to specify what to test on which test nodes, using what configuration.

The `Common Test` Master server writes progress information to HTML log files
similarly to the regular `Common Test` server. The logs contain test statistics
and links to the log files written by each independent `Common Test` server.

The `Common Test` Master API is exported by module `m:ct_master`.

## Use

`Common Test` Master requires all test nodes to be on the same network and share
a common file system. `Common Test` Master cannot start test nodes
automatically. The nodes must be started in advance for `Common Test` Master to
be able to start test sessions on them.

Tests are started by calling [`ct_master:run(TestSpecs)`](`ct_master:run/1`) or
[`ct_master:run(TestSpecs, InclNodes, ExclNodes)`](`ct_master:run/3`)

`TestSpecs` is either the name of a test specification file (string) or a list
of test specifications. If it is a list, the specifications are handled (and the
corresponding tests executed) in sequence. An element in a `TestSpecs` list can
also be list of test specifications. The specifications in such a list are
merged into one combined specification before test execution.

_Example:_

```erlang
ct_master:run(["ts1","ts2",["ts3","ts4"]])
```

Here, the tests specified by "ts1" run first, then the tests specified by "ts2",
and finally the tests specified by both "ts3" and "ts4".

The `InclNodes` argument to `run/3` is a list of node names. Function `run/3`
runs the tests in `TestSpecs` just like `run/1`, but also takes any test in
`TestSpecs`, which is not explicitly tagged with a particular node name, and
execute it on the nodes listed in `InclNodes`. By using `run/3` this way, any
test specification can be used, with or without node information, in a
large-scale test environment.

`ExclNodes` is a list of nodes to be excluded from the test. That is, tests that
are specified in the test specification to run on a particular node are not
performed if that node is listed in `ExclNodes` at runtime.

If `Common Test` Master fails initially to connect to any of the test nodes
specified in a test specification or in the `InclNodes` list, the operator is
prompted with the option to either start over again (after manually checking the
status of the nodes in question), to run without the missing nodes, or to abort
the operation.

When tests start, `Common Test` Master displays information to console about the
involved nodes. `Common Test` Master also reports when tests finish,
successfully or unsuccessfully. If connection is lost to a node, the test on
that node is considered finished. `Common Test` Master does not attempt to
re-establish contact with the failing node.

At any time, to get the current status of the test nodes, call function
[`ct_master:progress()`](`ct_master:progress/0`).

To stop one or more tests, use function
[`ct_master:abort()`](`ct_master:abort/0`) (to stop all) or
[`ct_master:abort(Nodes)`](`ct_master:abort/1`).

For details about the `Common Test` Master API, see module `m:ct_master`.

[](){: #test_specifications }

## Test Specifications

The test specifications used as input to `Common Test` Master are fully
compatible with the specifications used as input to the regular `Common Test`
server. The syntax is described in section
[Test Specifications](run_test_chapter.md#test_specifications) in section
Running Tests and Analyzing Results.

All test specification terms can have a `NodeRefs` element. This element
specifies which node or nodes a configuration operation or a test is to be
executed on. `NodeRefs` is defined as follows:

`NodeRefs = all_nodes | [NodeRef] | NodeRef`

`NodeRef = NodeAlias | node() | master`

A `NodeAlias` (`t:atom/0`) is used in a test specification as a reference to a
node name (so the node name only needs to be declared once, which also can be
achieved using constants). The alias is declared with a `node` term as follows:

`{node, NodeAlias, NodeName}`

If `NodeRefs` has the value `all_nodes`, the operation or test is performed on
all specified test nodes. (Declaring a term without a `NodeRefs` element has the
same effect). If `NodeRefs` has the value `master`, the operation is only
performed on the `Common Test` Master node (namely set the log directory or
install an event handler).

Consider the example in section
[Test Specifications](run_test_chapter.md#test_specifications) in section
Running Tests and Analysing Results, now extended with node information and
intended to be executed by `Common Test` Master:

```erlang
{define, 'Top', "/home/test"}.
{define, 'T1', "'Top'/t1"}.
{define, 'T2', "'Top'/t2"}.
{define, 'T3', "'Top'/t3"}.
{define, 'CfgFile', "config.cfg"}.
{define, 'Node', ct_node}.

{node, node1, 'Node@host_x'}.
{node, node2, 'Node@host_y'}.

{logdir, master, "'Top'/master_logs"}.
{logdir, "'Top'/logs"}.

{config, node1, "'T1'/'CfgFile'"}.
{config, node2, "'T2'/'CfgFile'"}.
{config, "'T3'/'CfgFile'"}.

{suites, node1, 'T1', all}.
{skip_suites, node1, 'T1', [t1B_SUITE,t1D_SUITE], "Not implemented"}.
{skip_cases, node1, 'T1', t1A_SUITE, [test3,test4], "Irrelevant"}.
{skip_cases, node1, 'T1', t1C_SUITE, [test1], "Ignore"}.

{suites, node2, 'T2', [t2B_SUITE,t2C_SUITE]}.
{cases, node2, 'T2', t2A_SUITE, [test4,test1,test7]}.

{skip_suites, 'T3', all, "Not implemented"}.
```

This example specifies the same tests as the original example. But now if
started with a call to `ct_master:run(TestSpecName)`, test `t1` is executed on
node `ct_node@host_x` (`node1`), test `t2` on `ct_node@host_y` (`node2`) and
test `t3` on both `node1` and `node2`. Configuration file `t1` is only read on
`node1` and configuration file `t2` only on `node2`, while the configuration
file `t3` is read on both `node1` and `node2`. Both test nodes write log files
to the same directory. (However, the `Common Test` Master node uses a different
log directory than the test nodes.)

If the test session is instead started with a call to
`ct_master:run(TestSpecName, [ct_node@host_z], [ct_node@host_x])`, the result is
that test `t1` does not run on `ct_node@host_x` (or any other node) while test
`t3` runs on both `ct_node@host_y` and `ct_node@host_z`.

A nice feature is that a test specification that includes node information can
still be used as input to the regular `Common Test` server (as described in
section [Test Specifications](run_test_chapter.md#test_specifications)). The
result is that any test specified to run on a node with the same name as the
`Common Test` node in question (typically `ct@somehost` if started with the
`ct_run` program), is performed. Tests without explicit node association are
always performed too, of course.

## Automatic Startup of Test Target Nodes

[](){: #ct_slave }

Initial actions can be started and performed automatically on test target nodes
using test specification term `init`.

Two subterms are supported, `node_start` and `eval`.

_Example:_

```erlang
{node, node1, node1@host1}.
{node, node2, node1@host2}.
{node, node3, node2@host2}.
{node, node4, node1@host3}.
{init, node1, [{node_start, [{callback_module, my_slave_callback}]}]}.
{init, [node2, node3], {node_start, [{username, "ct_user"}, {password, "ct_password"}]}}.
{init, node4, {eval, {module, function, []}}}.
```

This test specification declares that `node1@host1` is to be started using the
user callback function `callback_module:my_slave_callback/0`, and nodes
`node1@host2` and `node2@host2` are to be started with the default callback
module `ct_slave`. The specified username and password are used to log on to
remote host `host2`. Also, function `module:function/0` is evaluated on
`node1@host3`, and the result of this call is printed to the log.

The default callback module `m:ct_slave`, has the following features:

- Starting Erlang target nodes on local or remote hosts (application `SSH` is
  used for communication).
- Ability to start an Erlang emulator with more flags (any flags supported by
  `erl` are supported).
- Supervision of a node being started using internal callback functions. Used to
  prevent hanging nodes. (Configurable.)
- Monitoring of the master node by the slaves. A slave node can be stopped if
  the master node terminates. (Configurable.)
- Execution of user functions after a slave node is started. Functions can be
  specified as a list of `{Module, Function, Arguments}` tuples.

> #### Note {: .info }
>
> An `eval` term for the node and `startup_functions` in the `node_start`
> options list can be specified. In this case, the node is started first, then
> the `startup_functions` are executed, and finally functions specified with
> `eval` are called.
