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
# Advanced Agent Topics

The chapter _Advanced Agent Topics_ describes the more advanced agent related
features of the SNMP development tool. The following topics are covered:

- When to use a Sub-agent
- Agent semantics
- Sub-agents and dependencies
- Distributed tables
- Fault tolerance
- Using Mnesia tables as SNMP tables
- Audit Trail Logging
- Deviations from the standard

## When to use a Sub-agent

The section _When to use a Sub-agent_ describes situations where the mechanism
of loading and unloading MIBs is insufficient. In these cases a sub-agent is
needed.

### Special Set Transaction Mechanism

Each sub-agent can implement its own mechanisms for `set`, `get` and `get-next`.
For example, if the application requires the `get` mechanism to be asynchronous,
or needs a N-phase `set` mechanism, a specialized sub-agent should be used.

The toolkit allows different kinds of sub-agents at the same time. Accordingly,
different MIBs can have different `set` or `get` mechanisms.

### Process Communication

A simple distributed agent can be managed without sub-agents. The
instrumentation functions can use distributed Erlang to communicate with other
parts of the application. However, a sub-agent can be used on each node if this
generates too much unnecessary traffic. A sub-agent processes requests per
incoming SNMP request, not per variable. Therefore the network traffic is
minimized.

If the instrumentation functions communicate with UNIX processes, it might be a
good idea to use a special sub-agent. This sub-agent sends the SNMP request to
the other process in one packet in order to minimize context switches. For
example, if a whole MIB is implemented on the C level in UNIX, but you still
want to use the Erlang SNMP tool, then you may have one special sub-agent, which
sends the variables in the request as a single operation down to C.

### Frequent Loading of MIBs

Loading and unloading of MIBs are quite cheap operations. However, if the
application does this very often, perhaps several times per minute, it should
load the MIBs once and for all in a sub-agent. This sub-agent only registers and
unregisters itself under another agent instead of loading the MIBs each time.
This is cheaper than loading an MIB.

### Interaction With Other SNMP Agent Toolkits

If the SNMP agent needs to interact with sub-agents constructed in another
package, a special sub-agent should be used, which communicates through a
protocol specified by the other package.

## Agent Semantics

The agent can be configured to be multi-threaded, to process one incoming
request at a time, or to have a request limit enabled (this can be used for load
control or to limit the effect of DoS attacks). If it is multi-threaded, read
requests (`get`, `get-next` and `get-bulk`) and traps are processed in parallel
with each other and `set` requests. However, all `set` requests are serialized,
which means that if the agent is waiting for the application to complete a
complicated write operation, it will not process any new write requests until
this operation is finished. It processes read requests and sends traps,
concurrently. The reason for not handle write requests in parallel is that a
complex locking mechanism would be needed even in the simplest cases. Even with
the scheme described above, the user must be careful not to violate that the
`set` requests are atoms. If this is hard to do, do not use the multi-threaded
feature.

The order within an request is undefined and variables are not processed in a
defined order. Do not assume that the first variable in the PDU will be
processed before the second, even if the agent processes variables in this
order. It cannot even be assumed that requests belonging to different sub-agents
have any order.

If the manager tries to set the same variable many times in the same PDU, the
agent is free to improvise. There is no definition which determines if the
instrumentation will be called once or twice. If called once only, there is no
definition that determines which of the new values is going to be supplied.

When the agent receives a request, it keeps the request ID for one second after
the response is sent. If the agent receives another request with the same
request ID during this time, from the same IP address and UDP port, that request
will be discarded. This mechanism has nothing to do with the function
`snmpa:current_request_id/0`.

## Sub-agents and Dependencies

The toolkit supports the use of different types of sub-agents, but not the
construction of sub-agents.

Also, the toolkit does not support dependencies between sub-agents. A sub-agent
should by definition be stand alone and it is therefore not good design to
create dependencies between them.

## Distributed Tables

A common situation in more complex systems is that the data in a table is
distributed. Different table rows are implemented in different places. Some SNMP
tool-kits dedicate an SNMP sub-agent for each part of the table and load the
corresponding MIB into all sub-agents. The Master Agent is responsible for
presenting the distributed table as a single table to the manager. The toolkit
supplied uses a different method.

The method used to implement distributed tables with this SNMP tool is to
implement a table coordinator process responsible for coordinating the
processes, which hold the table data and they are called table holders. All
table holders must in some way be known by the coordinator; the structure of the
table data determines how this is achieved. The coordinator may require that the
table holders explicitly register themselves and specify their information. In
other cases, the table holders can be determined once at compile time.

When the instrumentation function for the distributed table is called, the
request should be forwarded to the table coordinator. The coordinator finds the
requested information among the table holders and then returns the answer to the
instrumentation function. The SNMP toolkit contains no support for coordination
of tables since this must be independent of the implementation.

The advantages of separating the table coordinator from the SNMP tool are:

- We do not need a sub-agent for each table holder. Normally, the sub-agent is
  needed to take care of communication, but in Distributed Erlang we use
  ordinary message passing.
- Most likely, some type of table coordinator already exists. This process
  should take care of the instrumentation for the table.
- The method used to present a distributed table is strongly application
  dependent. The use of different masking techniques is only valid for a small
  subset of problems and registering every row in a distributed table makes it
  non-distributed.

## Fault Tolerance

The SNMP agent toolkit gets input from three different sources:

- UDP packets from the network
- return values from the user defined instrumentation functions
- return values from the MIB.

The agent is highly fault tolerant. If the manager gets an unexpected response
from the agent, it is possible that some instrumentation function has returned
an erroneous value. The agent will not crash even if the instrumentation does.
It should be noted that if an instrumentation function enters an infinite loop,
the agent will also be blocked forever. The supervisor ,or the application,
specifies how to restart the agent.

### Using the SNMP Agent in a Distributed Environment

The normal way to use the agent in a distributed environment is to use one
master agent located at one node, and zero or more sub-agents located on other
nodes. However, this configuration makes the master agent node a single point of
failure. If that node goes down, the agent will not work.

One solution to this problem is to make the snmp application a distributed
Erlang application, and that means, the agent may be configured to run on one of
several nodes. If the node where it runs goes down, another node restarts the
agent. This is called _failover_. When the node starts again, it may _takeover_
the application. This solution to the problem adds another problem. Generally,
the new node has another IP address than the first one, which may cause problems
in the communication between the SNMP managers and the agent.

If the snmp agent is configured as a distributed Erlang application, it will
during takeover try to load the same MIBs that were loaded at the old node. It
uses the same filenames as the old node. If the MIBs are not located in the same
paths at the different nodes, the MIBs must be loaded explicitly after takeover.

## Using Mnesia Tables as SNMP Tables

The Mnesia DBMS can be used for storing data of SNMP tables. This means that an
SNMP table can be implemented as a Mnesia table, and that a Mnesia table can be
made visible via SNMP. This mapping is largely automated.

There are three main reasons for using this mapping:

- We get all features of Mnesia, such as fault tolerance, persistent data
  storage, replication, and so on.
- Much of the work involved is automated. This includes `get-next` processing
  and `RowStatus` handling.
- The table may be used as an ordinary Mnesia table, using the Mnesia API
  internally in the application at the same time as it is visible through SNMP.

When this mapping is used, insertion and deletion in the original Mnesia table
is slower, with a factor O(log n). The read access is not affected.

A drawback with implementing an SNMP table as a Mnesia table is that the
internal resource is forced to use the table definition from the MIB, which
means that the external data model must be used internally. Actually, this is
only partially true. The Mnesia table may extend the SNMP table, which means
that the Mnesia table may have columns which are use internally and are not seen
by SNMP. Still, the data model from SNMP must be maintained. Although this is
undesirable, it is a pragmatic compromise in many situations where simple and
efficient implementation is preferable to abstraction.

### Creating the Mnesia Table

The table must be created in Mnesia before the manager can use it. The table
must be declared as type `snmp`. This makes the table ordered in accordance with
the lexicographical ordering rules of SNMP. The name of the Mnesia table must be
identical to the SNMP table name. The types of the INDEX fields in the
corresponding SNMP table must be specified.

If the SNMP table has more than one INDEX column, the corresponding Mnesia row
is a tuple, where the first element is a tuple with the INDEX columns.
Generally, if the SNMP table has _N_ INDEX columns and _C_ data columns, the
Mnesia table is of arity _(C-N)+1_, where the key is a tuple of arity _N_ if
_N > 1_, or a single term if _N = 1_.

Refer to the Mnesia User's Guide for information on how to declare a Mnesia
table as an SNMP table.

The following example illustrates a situation in which we have an SNMP table
that we wish to implement as a Mnesia table. The table stores information about
employees at a company. Each employee is indexed with the department number and
the name.

```text
       empTable OBJECT-TYPE
              SYNTAX      SEQUENCE OF EmpEntry
              ACCESS      not-accessible
              STATUS      mandatory
              DESCRIPTION
                      "A table with information about employees."
       ::= { emp 1}
       empEntry OBJECT-TYPE
              SYNTAX      EmpEntry
              ACCESS      not-accessible
              STATUS      mandatory
              DESCRIPTION
                 ""
              INDEX      { empDepNo, empName }
       ::= { empTable 1 }
       EmpEntry ::=
              SEQUENCE {
                  empDepNo         INTEGER,
                  empName          DisplayString,
                  empTelNo         DisplayString,
                  empStatus        RowStatus
              }
```

The corresponding Mnesia table is specified as follows:

```erlang
mnesia:create_table([{name, employees},
                     {snmp, [{key, {integer, string}}]},
                     {attributes, [key, telno, row_status]}]).
```

> #### Note {: .info }
>
> In the Mnesia tables, the two key columns are stored as a tuple with two
> elements. Therefore, the arity of the table is 3.

### Instrumentation Functions

The MIB table shown in the previous section can be compiled as follows:

```erlang
1> snmpc:compile("EmpMIB", [{db, mnesia}]).
```

This is all that has to be done\! Now the manager can read, add, and modify
rows. Also, you can use the ordinary Mnesia API to access the table from your
programs. The only explicit action is to create the Mnesia table, an action the
user has to perform in order to create the required table schemas.

### Adding Own Actions

It is often necessary to take some specific action when a table is modified.
This is accomplished with an instrumentation function. It executes some specific
code when the table is set, and passes all other requests down to the
pre-defined function.

The following example illustrates this idea:

```erlang
emp_table(set, RowIndex, Cols) ->
    notify_internal_resources(RowIndex, Cols),
    snmp_generic:table_func(set, RowIndex, Cols, {empTable, mnesia});
emp_table(Op, RowIndex, Cols) ->
    snmp_generic:table_func(Op, RowIndex, Cols, {empTable, mnesia}).
```

The default instrumentation functions are defined in the module `snmp_generic`.
Refer to the Reference Manual, section SNMP, module `snmp_generic` for details.

### Extending the Mnesia Table

A table may contain columns that are used internally, but should not be visible
to a manager. These internal columns must be the last columns in the table. The
`set` operation will not work with this arrangement, because there are columns
that the agent does not know about. This situation is handled by adding values
for the internal columns in the `set` function.

To illustrate this, suppose we extend our Mnesia `empTable` with one internal
column. We create it as before, but with an arity of 4, by adding another
attribute.

```erlang
mnesia:create_table([{name, employees},
                     {snmp, [{key, {integer, string}}]},
                     {attributes, {key, telno, row_status, internal_col}}]).
```

The last column is the internal column. When performing a `set` operation, which
creates a row, we must give a value to the internal column. The instrumentation
functions will now look as follows:

```erlang
-define(createAndGo, 4).
-define(createAndWait, 5).

emp_table(set, RowIndex, Cols) ->
  notify_internal_resources(RowIndex, Cols),
  NewCols =
    case is_row_created(empTable, Cols) of
      true -> Cols ++ [{4, "internal"}]; % add internal column
      false -> Cols                      % keep original cols
  end,
  snmp_generic:table_func(set, RowIndex, NewCols, {empTable, mnesia});
emp_table(Op, RowIndex, Cols) ->
  snmp_generic:table_func(Op, RowIndex, Cols, {empTable, mnesia}).

is_row_created(Name, Cols) ->
  case snmp_generic:get_status_col(Name, Cols) of
    {ok, ?createAndGo} -> true;
    {ok, ?createAndWait} -> true;
    _ -> false
  end.
```

If a row is created, we always set the internal column to `"internal"`.

## Deviations from the Standard

In some aspects the agent does not implement SNMP fully. Here are the
differences:

- The default functions and `snmp_generic` cannot handle an object of type
  `NetworkAddress` as INDEX (SNMPv1 only\!). Use `IpAddress` instead.
- The agent does not check complex ranges specified for INTEGER objects. In
  these cases it just checks that the value lies within the minimum and maximum
  values specified. For example, if the range is specified as `1..10 | 12..20`
  the agent would let 11 through, but not 0 or 21. The instrumentation functions
  must check the complex ranges itself.
- The agent will never generate the `wrongEncoding` error. If a variable binding
  is erroneous encoded, the `asn1ParseError` counter will be incremented.
- A `tooBig` error in an SNMPv1 packet will always use the `'NULL'` value in all
  variable bindings.
- The default functions and `snmp_generic` do not check the range of each OCTET
  in textual conventions derived from OCTET STRING, e.g. `DisplayString` and
  `DateAndTime`. This must be checked in an overloaded `is_set_ok` function.
