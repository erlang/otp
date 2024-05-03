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
# Build a Mnesia Database

This section describes the basic steps when designing a `Mnesia` database and
the programming constructs that make different solutions available to the
programmer. The following topics are included:

- Define a schema
- Data model
- Start `Mnesia`
- Create tables

[](){: #def_schema }

## Define a Schema

The configuration of a `Mnesia` system is described in a schema. The schema is a
special table that includes information such as the table names and the storage
type of each table (that is, whether a table is to be stored in RAM, on disc, or
on both, as well as its location).

Unlike data tables, information in schema tables can only be accessed and
modified by using the schema-related functions described in this section.

`Mnesia` has various functions for defining the database schema. Tables can be
moved or deleted, and the table layout can be reconfigured.

An important aspect of these functions is that the system can access a table
while it is being reconfigured. For example, it is possible to move a table and
simultaneously perform write operations to the same table. This feature is
essential for applications that require continuous service.

This section describes the functions available for schema management, all which
return either of the following tuples:

- `{atomic, ok}` if successful
- `{aborted, Reason}` if unsuccessful

### Schema Functions

The schema functions are as follows:

- [mnesia:create_schema(NodeList)](`mnesia:create_schema/1`) initializes a new,
  empty schema. This is a mandatory requirement before `Mnesia` can be started.
  `Mnesia` is a truly distributed DBMS and the schema is a system table that is
  replicated on all nodes in a `Mnesia` system. This function fails if a schema
  is already present on any of the nodes in `NodeList`. The function requires
  `Mnesia` to be stopped on the all `db_nodes` contained in parameter
  `NodeList`. Applications call this function only once, as it is usually a
  one-time activity to initialize a new database.
- [mnesia:delete_schema(DiscNodeList)](`mnesia:delete_schema/1`) erases any old
  schemas on the nodes in `DiscNodeList`. It also removes all old tables
  together with all data. This function requires `Mnesia` to be stopped on all
  `db_nodes`.
- [mnesia:delete_table(Tab)](`mnesia:delete_table/1`) permanently deletes all
  replicas of table `Tab`.
- [mnesia:clear_table(Tab)](`mnesia:clear_table/1`) permanently deletes all
  entries in table `Tab`.
- [mnesia:move_table_copy(Tab, From, To)](`mnesia:move_table_copy/3`) moves the
  copy of table `Tab` from node `From` to node `To`. The table storage type
  `{type}` is preserved, so if a RAM table is moved from one node to another, it
  remains a RAM table on the new node. Other transactions can still perform read
  and write operation to the table while it is being moved.
- [mnesia:add_table_copy(Tab, Node, Type)](`mnesia:add_table_copy/3`) creates a
  replica of table `Tab` at node `Node`. Argument `Type` must be either of the
  atoms `ram_copies`, `disc_copies`, or `disc_only_copies`. If you add a copy of
  the system table `schema` to a node, you want the `Mnesia` schema to reside
  there as well. This action extends the set of nodes that comprise this
  particular `Mnesia` system.
- [mnesia:del_table_copy(Tab, Node)](`mnesia:del_table_copy/2`) deletes the
  replica of table `Tab` at node `Node`. When the last replica of a table is
  removed, the table is deleted.
- [mnesia:transform_table(Tab, Fun, NewAttributeList, NewRecordName)](`mnesia:transform_table/4`)
  changes the format on all records in table `Tab`. It applies argument `Fun` to
  all records in the table. `Fun` must be a function that takes a record of the
  old type, and returns the record of the new type. The table key must not be
  changed.

  _Example:_

  ```erlang
  -record(old, {key, val}).
  -record(new, {key, val, extra}).

  Transformer =
     fun(X) when record(X, old) ->
        #new{key = X#old.key,
             val = X#old.val,
             extra = 42}
     end,
  {atomic, ok} = mnesia:transform_table(foo, Transformer,
                                        record_info(fields, new),
                                        new),
  ```

  Argument `Fun` can also be the atom `ignore`, which indicates that only the
  metadata about the table is updated. Use of `ignore` is not recommended (as it
  creates inconsistencies between the metadata and the actual data) but it is
  included as a possibility for the user do to an own (offline) transform.

- `change_table_copy_type(Tab, Node, ToType)` changes the storage type of a
  table. For example, a RAM table is changed to a `disc_table` at the node
  specified as `Node`.

## Data Model

The data model employed by `Mnesia` is an extended relational data model. Data
is organized as a set of tables and relations between different data records can
be modeled as more tables describing the relationships. Each table contains
instances of Erlang records. The records are represented as Erlang tuples.

Each Object Identifier (OID) is made up of a table name and a key. For example,
if an employee record is represented by the tuple
`{employee, 104732, klacke, 7, male, 98108, {221, 015}}`, this record has an
OID, which is the tuple `{employee, 104732}`.

Thus, each table is made up of records, where the first element is a record name
and the second element of the table is a key, which identifies the particular
record in that table. The combination of the table name and a key is an arity
two tuple `{Tab, Key}` called the OID. For more information about the
relationship between the record name and the table name, see
[Record Names versus Table Names](mnesia_chap4.md#recordnames_tablenames).

What makes the `Mnesia` data model an extended relational model is the ability
to store arbitrary Erlang terms in the attribute fields. One attribute value
can, for example, be a whole tree of OIDs leading to other terms in other
tables. This type of record is difficult to model in traditional relational
DBMSs.

[](){: #start_mnesia }

## Start Mnesia

Before starting `Mnesia`, the following must be done:

- An empty schema must be initialized on all the participating nodes.
- The Erlang system must be started.
- Nodes with disc database schema must be defined and implemented with the
  function [mnesia:create_schema(NodeList)](`mnesia:create_schema/1`).

When running a distributed system with two or more participating nodes, the
function [mnesia:start()](`mnesia:start/0`) must be executed on each
participating node. This would typically be part of the boot script in an
embedded environment. In a test environment or an interactive environment,
`mnesia:start()` can also be used either from the Erlang shell or another
program.

### Initialize a Schema and Start Mnesia

Let us use the example database `Company`, described in
[Getting Started](mnesia_chap2.md#getting_started) to illustrate how to run a
database on two separate nodes, called `a@gin` and `b@skeppet`. Each of these
nodes must have a `Mnesia` directory and an initialized schema before `Mnesia`
can be started. There are two ways to specify the `Mnesia` directory to be used:

- Specify the `Mnesia` directory by providing an application parameter either
  when starting the Erlang shell or in the application script. Previously, the
  following example was used to create the directory for the `Company` database:

  ```text
  %erl -mnesia dir '"/ldisc/scratch/Mnesia.Company"'
  ```

- If no command-line flag is entered, the `Mnesia` directory becomes the current
  working directory on the node where the Erlang shell is started.

To start the `Company` database and get it running on the two specified nodes,
enter the following commands:

1. On the node `a@gin`:

```text
 gin %erl -sname a  -mnesia dir '"/ldisc/scratch/Mnesia.company"'
```

1. On the node `b@skeppet`:

```text
skeppet %erl -sname b -mnesia dir '"/ldisc/scratch/Mnesia.company"'
```

1. On one of the two nodes:

```text
(a@gin)1>mnesia:create_schema([a@gin, b@skeppet]).
```

1. The function [mnesia:start()](`mnesia:start/0`) is called on both nodes.
1. To initialize the database, execute the following code on one of the two
   nodes:

```erlang


dist_init() ->
    mnesia:create_table(employee,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields,
						   employee)}]),
    mnesia:create_table(dept,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields, dept)}]),
    mnesia:create_table(project,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields, project)}]),
    mnesia:create_table(manager, [{type, bag},
                                  {ram_copies, [a@gin, b@skeppet]},
                                  {attributes, record_info(fields,
							   manager)}]),
    mnesia:create_table(at_dep,
                         [{ram_copies, [a@gin, b@skeppet]},
                          {attributes, record_info(fields, at_dep)}]),
    mnesia:create_table(in_proj,
                        [{type, bag},
                         {ram_copies, [a@gin, b@skeppet]},
                         {attributes, record_info(fields, in_proj)}]).
```

As illustrated, the two directories reside on different nodes, because
`/ldisc/scratch` (the "local" disc) exists on the two different nodes.

By executing these commands, two Erlang nodes are configured to run the
`Company` database, and therefore, initialize the database. This is required
only once when setting up. The next time the system is started,
[mnesia:start()](`mnesia:start/0`) is called on both nodes, to initialize the
system from disc.

In a system of `Mnesia` nodes, every node is aware of the current location of
all tables. In this example, data is replicated on both nodes and functions that
manipulate the data in the tables can be executed on either of the two nodes.
Code that manipulate `Mnesia` data behaves identically regardless of where the
data resides.

The function [mnesia:stop()](`mnesia:stop/0`) stops `Mnesia` on the node where
the function is executed. The functions `mnesia:start/0` and `mnesia:stop/0`
work on the "local" `Mnesia` system. No functions start or stop a set of nodes.

### Startup Procedure

Start `Mnesia` by calling the following function:

```text
          mnesia:start().
```

This function initiates the DBMS locally.

The choice of configuration alters the location and load order of the tables.
The alternatives are as follows:

1. Tables that are only stored locally are initialized from the local `Mnesia`
   directory.
1. Replicated tables that reside locally as well as somewhere else are either
   initiated from disc or by copying the entire table from the other node,
   depending on which of the different replicas are the most recent. `Mnesia`
   determines which of the tables are the most recent.
1. Tables that reside on remote nodes are available to other nodes as soon as
   they are loaded.

Table initialization is asynchronous. The function call
[mnesia:start()](`mnesia:start/0`) returns the atom `ok` and then starts to
initialize the different tables. Depending on the size of the database, this can
take some time, and the application programmer must wait for the tables that the
application needs before they can be used. This is achieved by using the
function [mnesia:wait_for_tables(TabList, Timeout)](`mnesia:wait_for_tables/2`),
which suspends the caller until all tables specified in `TabList` are properly
initiated.

A problem can arise if a replicated table on one node is initiated, but `Mnesia`
deduces that another (remote) replica is more recent than the replica existing
on the local node, and the initialization procedure does not proceed. In this
situation, a call to `mnesia:wait_for_tables/2`, suspends the caller until the
remote node has initialized the table from its local disc and the node has
copied the table over the network to the local node.

However, this procedure can be time-consuming, the shortcut function
[mnesia:force_load_table(Tab)](`mnesia:force_load_table/1`) loads all the tables
from disc at a faster rate. The function forces tables to be loaded from disc
regardless of the network situation.

Thus, it can be assumed that if an application wants to use tables `a` and `b`,
the application must perform some action similar to following before it can use
the tables:

```erlang
          case mnesia:wait_for_tables([a, b], 20000) of
            {timeout,   RemainingTabs} ->
              panic(RemainingTabs);
            ok ->
              synced
          end.
```

> #### Warning {: .warning }
>
> When tables are forcefully loaded from the local disc, all operations that
> were performed on the replicated table while the local node was down, and the
> remote replica was alive, are lost. This can cause the database to become
> inconsistent.

If the startup procedure fails, the function [mnesia:start()](`mnesia:start/0`)
returns the cryptic tuple
`{error,{shutdown, {mnesia_sup,start_link,[normal,[]]}}}`. To get more
information about the start failure, use command-line arguments
`-boot start_sasl` as argument to the `erl` script.

[](){: #create_tables }

## Create Tables

The function [mnesia:create_table(Name, ArgList)](`mnesia:create_table/2`)
creates tables. When executing this function, it returns one of the following
responses:

- `{atomic, ok}` if the function executes successfully
- `{aborted, Reason}` if the function fails

The function arguments are as follows:

- `Name` is the name of the table. It is usually the same name as the name of
  the records that constitute the table. For details, see `record_name`.
- `ArgList` is a list of `{Key,Value}` tuples. The following arguments are
  valid:

  - `{type, Type}`, where `Type` must be either of the atoms `set`,
    `ordered_set`, or `bag`. Default is `set`.

    Notice that currently `ordered_set` is not supported for `disc_only_copies`
    tables.

    A table of type `set` or `ordered_set` has either zero or one record per
    key, whereas a table of type `bag` can have an arbitrary number of records
    per key. The key for each record is always the first attribute of the
    record.

    The following example illustrates the difference between type `set` and
    `bag`:

    ```erlang
     f() ->
        F = fun() ->
              mnesia:write({foo, 1, 2}),
              mnesia:write({foo, 1, 3}),
              mnesia:read({foo, 1})
            end,
        mnesia:transaction(F).
    ```

    This transaction returns the list `[{foo,1,3}]` if table `foo` is of type
    `set`. However, the list `[{foo,1,2}, {foo,1,3}]` is returned if the table
    is of type `bag`.

    `Mnesia` tables can never contain duplicates of the same record in the same
    table. Duplicate records have attributes with the same contents and key.

  - `{disc_copies, NodeList}`, where `NodeList` is a list of the nodes where
    this table is to reside on disc.

    Write operations to a table replica of type `disc_copies` write data to the
    disc copy and to the RAM copy of the table.

    It is possible to have a replicated table of type `disc_copies` on one node,
    and the same table stored as a different type on another node. Default is
    `[]`. This arrangement is desirable if the following operational
    characteristics are required:

    1. Read operations must be fast and performed in RAM.
    1. All write operations must be written to persistent storage.

    A write operation on a `disc_copies` table replica is performed in two
    steps. First the write operation is appended to a log file, then the actual
    operation is performed in RAM.

  - `{ram_copies, NodeList}`, where `NodeList` is a list of the nodes where this
    table is stored in RAM. Default is `[node()]`. If the default value is used
    to create a table, it is located on the local node only.

    Table replicas of type `ram_copies` can be dumped to disc with the function
    [mnesia:dump_tables(TabList)](`mnesia:dump_tables/1`).

  - `{disc_only_copies, NodeList}`. These table replicas are stored on disc only
    and are therefore slower to access. However, a disc-only replica consumes
    less memory than a table replica of the other two storage types.
  - `{index, AttributeNameList}`, where `AttributeNameList` is a list of atoms
    specifying the names of the attributes `Mnesia` is to build and maintain. An
    index table exists for every element in the list. The first field of a
    `Mnesia` record is the key and thus need no extra index.

    The first field of a record is the second element of the tuple, which is the
    representation of the record.

  - `{snmp, SnmpStruct}`. `SnmpStruct` is described in the
    [SNMP](`e:snmp:index.html`) User's Guide. Basically, if this attribute is
    present in `ArgList` of `mnesia:create_table/2`, the table is immediately
    accessible the SNMP.

    It is easy to design applications that use SNMP to manipulate and control
    the system. `Mnesia` provides a direct mapping between the logical tables
    that make up an SNMP control application and the physical data that makes up
    a `Mnesia` table. The default value is `[]`.

  - `{local_content, true}`. When an application needs a table whose contents is
    to be locally unique on each node, `local_content` tables can be used. The
    name of the table is known to all `Mnesia` nodes, but its contents is unique
    for each node. Access to this type of table must be done locally.
  - `{attributes, AtomList}` is a list of the attribute names for the records
    that are supposed to populate the table. Default is the list `[key, val]`.
    The table must at least have one extra attribute besides the key. When
    accessing single attributes in a record, it is not recommended to hard code
    the attribute names as atoms. Use the construct
    `record_info(fields, record_name)` instead.

    The expression `record_info(fields, record_name)` is processed by the Erlang
    preprocessor and returns a list of the record field names. With the record
    definition `-record(foo, {x,y,z}).`, the expression
    `record_info(fields,foo)` is expanded to the list `[x,y,z]`. It is therefore
    possible for you to provide the attribute names or to use the
    `record_info/2` notation.

    It is recommended to use the `record_info/2` notation, as it becomes easier
    to maintain the program and the program becomes more robust with regards to
    future record changes.

  - `{record_name, Atom}` specifies the common name of all records stored in the
    table. All records stored in the table must have this name as their first
    element. `record_name` defaults to the name of the table. For more
    information, see
    [Record Names versus Table Names](mnesia_chap4.md#recordnames_tablenames).

As an example, consider the following record definition:

```erlang
      -record(funky, {x, y}).
```

The following call would create a table that is replicated on two nodes, has an
extra index on attribute `y`, and is of type `bag`.

```erlang
      mnesia:create_table(funky, [{disc_copies, [N1, N2]}, {index,
      [y]}, {type, bag}, {attributes, record_info(fields, funky)}]).
```

Whereas a call to the following default code values would return a table with a
RAM copy on the local node, no extra indexes, and the attributes defaulted to
the list `[key,val]`.

```text
mnesia:create_table(stuff, [])
```
