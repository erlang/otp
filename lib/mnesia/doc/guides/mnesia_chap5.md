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
# Miscellaneous Mnesia Features

The previous sections describe how to get started with `Mnesia` and how to build
a `Mnesia` database. This section describes the more advanced features available
when building a distributed, fault-tolerant `Mnesia` database. The following
topics are included:

- Indexing
- Distribution and fault tolerance
- Table fragmentation
- Local content tables
- Disc-less nodes
- More about schema management
- `Mnesia` event handling
- Debugging `Mnesia` applications
- Concurrent processes in `Mnesia`
- Prototyping
- Object-based programming with `Mnesia`

## Indexing

Data retrieval and matching can be performed efficiently if the key for the
record is known. Conversely, if the key is unknown, all records in a table must
be searched. The larger the table, the more time consuming it becomes. To remedy
this problem, `Mnesia` indexing capabilities are used to improve data retrieval
and matching of records.

The following two functions manipulate indexes on existing tables:

- [`mnesia:add_table_index(Tab, AttributeName) -> {aborted, R} | {atomic, ok}`](`mnesia:add_table_index/2`)
- [`mnesia:del_table_index(Tab, AttributeName) -> {aborted, R} | {atomic, ok}`](`mnesia:del_table_index/2`)

These functions create or delete a table index on a field defined by
`AttributeName`. To illustrate this, add an index to the table definition
`(employee, {emp_no, name, salary, sex, phone, room_no})`, which is the example
table from the `Company` database. The function that adds an index on element
`salary` can be expressed as `mnesia:add_table_index(employee, salary)`.

The indexing capabilities of `Mnesia` are used with the following three
functions, which retrieve and match records based on index entries in the
database:

- [`mnesia:index_read(Tab, SecondaryKey, AttributeName) -> transaction abort | RecordList`](`mnesia:index_read/3`)
  avoids an exhaustive search of the entire table, by looking up `SecondaryKey`
  in the index to find the primary keys.
- [`mnesia:index_match_object(Pattern, AttributeName) -> transaction abort | RecordList`](`mnesia:index_match_object/2`)
  avoids an exhaustive search of the entire table, by looking up the secondary
  key in the index to find the primary keys. The secondary key is found in field
  `AttributeName` of `Pattern`. The secondary key must be bound.
- [`mnesia:match_object(Pattern) -> transaction abort | RecordList`](`mnesia:match_object/1`)
  uses indexes to avoid exhaustive search of the entire table. Unlike the
  previous functions, this function can use any index as long as the secondary
  key is bound.

These functions are further described and exemplified in
[Pattern Matching](mnesia_chap4.md#matching).

## Distribution and Fault Tolerance

`Mnesia` is a distributed, fault-tolerant DBMS. Tables can be replicated on
different Erlang nodes in various ways. The `Mnesia` programmer does not need to
state where the different tables reside, only the names of the different tables
need to be specified in the program code. This is known as "location
transparency" and is an important concept. In particular:

- A program works regardless of the data location. It makes no difference
  whether the data resides on the local node or on a remote node.

  Notice that the program runs slower if the data is located on a remote node.

- The database can be reconfigured, and tables can be moved between nodes. These
  operations do not affect the user programs.

It has previously been shown that each table has a number of system attributes,
such as `index` and `type`.

Table attributes are specified when the table is created. For example, the
following function creates a table with two RAM replicas:

```erlang
mnesia:create_table(foo,
                    [{ram_copies, [N1, N2]},
                     {attributes, record_info(fields, foo)}]).
```

Tables can also have the following properties, where each attribute has a list
of Erlang nodes as its value:

- `ram_copies`. The value of the node list is a list of Erlang nodes, and a RAM
  replica of the table resides on each node in the list.

  Notice that no disc operations are performed when a program executes write
  operations to these replicas. However, if permanent RAM replicas are required,
  the following alternatives are available:

  1. The function `mnesia:dump_tables/1` can be used to dump RAM table replicas
     to disc.
  1. The table replicas can be backed up, either from RAM, or from disc if
     dumped there with this function.

- `disc_copies`. The value of the attribute is a list of Erlang nodes, and a
  replica of the table resides both in RAM and on disc on each node in the list.
  Write operations addressed to the table address both the RAM and the disc copy
  of the table.
- `disc_only_copies`. The value of the attribute is a list of Erlang nodes, and
  a replica of the table resides only as a disc copy on each node in the list.
  The major disadvantage of this type of table replica is the access speed. The
  major advantage is that the table does not occupy space in memory.

In addition, table properties can be set and changed. For details, see
[Define a Schema](mnesia_chap3.md#def_schema).

There are basically two reasons for using more than one table replica: fault
tolerance and speed. Notice that table replication provides a solution to both
of these system requirements.

If there are two active table replicas, all information is still available if
one replica fails. This can be an important property in many applications.
Furthermore, if a table replica exists at two specific nodes, applications that
execute at either of these nodes can read data from the table without accessing
the network. Network operations are considerably slower and consume more
resources than local operations.

It can be advantageous to create table replicas for a distributed application
that reads data often, but writes data seldom, to achieve fast read operations
on the local node. The major disadvantage with replication is the increased time
to write data. If a table has two replicas, every write operation must access
both table replicas. Since one of these write operations must be a network
operation, it is considerably more expensive to perform a write operation to a
replicated table than to a non-replicated table.

## Table Fragmentation

### Concept

A concept of table fragmentation has been introduced to cope with large tables.
The idea is to split a table into several manageable fragments. Each fragment is
implemented as a first class `Mnesia` table and can be replicated, have indexes,
and so on, as any other table. But the tables cannot have `local_content` or
have the `snmp` connection activated.

To be able to access a record in a fragmented table, `Mnesia` must determine to
which fragment the actual record belongs. This is done by module `mnesia_frag`,
which implements the `mnesia_access` callback behavior. It is recommended to
read the documentation about the function `mnesia:activity/4` to see how
`mnesia_frag` can be used as a `mnesia_access` callback module.

At each record access, `mnesia_frag` first computes a hash value from the record
key. Second, the name of the table fragment is determined from the hash value.
Finally the actual table access is performed by the same functions as for
non-fragmented tables. When the key is not known beforehand, all fragments are
searched for matching records.

Notice that in `ordered_set` tables, the records are ordered per fragment, and
the order is undefined in results returned by `select` and `match_object`, as
well as `first`, `next`, `prev` and `last`.

The following code illustrates how a `Mnesia` table is converted to be a
fragmented table and how more fragments are added later:

```erlang
Eshell V4.7.3.3  (abort with ^G)
(a@sam)1> mnesia:start().
ok
(a@sam)2> mnesia:system_info(running_db_nodes).
[b@sam,c@sam,a@sam]
(a@sam)3> Tab = dictionary.
dictionary
(a@sam)4> mnesia:create_table(Tab, [{ram_copies, [a@sam, b@sam]}]).
{atomic,ok}
(a@sam)5> Write = fun(Keys) -> [mnesia:write({Tab,K,-K}) || K <- Keys], ok end.
#Fun<erl_eval>
(a@sam)6> mnesia:activity(sync_dirty, Write, [lists:seq(1, 256)], mnesia_frag).
ok
(a@sam)7> mnesia:change_table_frag(Tab, {activate, []}).
{atomic,ok}
(a@sam)8> mnesia:table_info(Tab, frag_properties).
[{base_table,dictionary},
 {foreign_key,undefined},
 {n_doubles,0},
 {n_fragments,1},
 {next_n_to_split,1},
 {node_pool,[a@sam,b@sam,c@sam]}]
(a@sam)9> Info = fun(Item) -> mnesia:table_info(Tab, Item) end.
#Fun<erl_eval>
(a@sam)10> Dist = mnesia:activity(sync_dirty, Info, [frag_dist], mnesia_frag).
[{c@sam,0},{a@sam,1},{b@sam,1}]
(a@sam)11> mnesia:change_table_frag(Tab, {add_frag, Dist}).
{atomic,ok}
(a@sam)12> Dist2 = mnesia:activity(sync_dirty, Info, [frag_dist], mnesia_frag).
[{b@sam,1},{c@sam,1},{a@sam,2}]
(a@sam)13> mnesia:change_table_frag(Tab, {add_frag, Dist2}).
{atomic,ok}
(a@sam)14> Dist3 = mnesia:activity(sync_dirty, Info, [frag_dist], mnesia_frag).
[{a@sam,2},{b@sam,2},{c@sam,2}]
(a@sam)15> mnesia:change_table_frag(Tab, {add_frag, Dist3}).
{atomic,ok}
(a@sam)16> Read = fun(Key) -> mnesia:read({Tab, Key}) end.
#Fun<erl_eval>
(a@sam)17> mnesia:activity(transaction, Read, [12], mnesia_frag).
[{dictionary,12,-12}]
(a@sam)18> mnesia:activity(sync_dirty, Info, [frag_size], mnesia_frag).
[{dictionary,64},
 {dictionary_frag2,64},
 {dictionary_frag3,64},
 {dictionary_frag4,64}]
(a@sam)19>
```

### Fragmentation Properties

The table property `frag_properties` can be read with the function
[`mnesia:table_info(Tab, frag_properties)`](`mnesia:table_info/2`). The
fragmentation properties are a list of tagged tuples with arity 2. By default
the list is empty, but when it is non-empty it triggers `Mnesia` to regard the
table as fragmented. The fragmentation properties are as follows:

- **`{n_fragments, Int}`** - `n_fragments` regulates how many fragments that the
  table currently has. This property can explicitly be set at table creation and
  later be changed with `{add_frag, NodesOrDist}` or `del_frag`. `n_fragments`
  defaults to `1`.

- **`{node_pool, List}`** - The node pool contains a list of nodes and can
  explicitly be set at table creation and later be changed with
  `{add_node, Node}` or `{del_node, Node}`. At table creation `Mnesia` tries to
  distribute the replicas of each fragment evenly over all the nodes in the node
  pool. Hopefully all nodes end up with the same number of replicas. `node_pool`
  defaults to the return value from the function
  [`mnesia:system_info(db_nodes)`](`mnesia:system_info/1`).

- **`{n_ram_copies, Int}`** - Regulates how many `ram_copies` replicas that each
  fragment is to have. This property can explicitly be set at table creation.
  Defaults is `0`, but if `n_disc_copies` and `n_disc_only_copies` also are `0`,
  `n_ram_copies` defaults to `1`.

- **`{n_disc_copies, Int}`** - Regulates how many `disc_copies` replicas that
  each fragment is to have. This property can explicitly be set at table
  creation. Default is `0`.

- **`{n_disc_only_copies, Int}`** - Regulates how many `disc_only_copies`
  replicas that each fragment is to have. This property can explicitly be set at
  table creation. Defaults is `0`.

- **`{foreign_key, ForeignKey}`** - `ForeignKey` can either be the atom
  `undefined` or the tuple `{ForeignTab, Attr}`, where `Attr` denotes an
  attribute that is to be interpreted as a key in another fragmented table named
  `ForeignTab`. `Mnesia` ensures that the number of fragments in this table and
  in the foreign table are always the same.

  When fragments are added or deleted, `Mnesia` automatically propagates the
  operation to all fragmented tables that have a foreign key referring to this
  table. Instead of using the record key to determine which fragment to access,
  the value of field `Attr` is used. This feature makes it possible to colocate
  records automatically in different tables to the same node. `foreign_key`
  defaults to `undefined`. However, if the foreign key is set to something else,
  it causes the default values of the other fragmentation properties to be the
  same values as the actual fragmentation properties of the foreign table.

- **`{hash_module, Atom}`** - Enables definition of an alternative hashing
  scheme. The module must implement the `m:mnesia_frag_hash` callback behavior.
  This property can explicitly be set at table creation. Default is
  `mnesia_frag_hash`.

- **`{hash_state, Term}`** - Enables a table-specific parameterization of a
  generic hash module. This property can explicitly be set at table creation.
  Default is `undefined`.

  ```erlang
  Eshell V4.7.3.3  (abort with ^G)
  (a@sam)1> mnesia:start().
  ok
  (a@sam)2> PrimProps = [{n_fragments, 7}, {node_pool, [node()]}].
  [{n_fragments,7},{node_pool,[a@sam]}]
  (a@sam)3> mnesia:create_table(prim_dict,
                                [{frag_properties, PrimProps},
                                 {attributes,[prim_key,prim_val]}]).
  {atomic,ok}
  (a@sam)4> SecProps = [{foreign_key, {prim_dict, sec_val}}].
  [{foreign_key,{prim_dict,sec_val}}]
  (a@sam)5> mnesia:create_table(sec_dict,
                                [{frag_properties, SecProps},
  (a@sam)5>                      {attributes, [sec_key, sec_val]}]).
  {atomic,ok}
  (a@sam)6> Write = fun(Rec) -> mnesia:write(Rec) end.
  #Fun<erl_eval>
  (a@sam)7> PrimKey = 11.
  11
  (a@sam)8> SecKey = 42.
  42
  (a@sam)9> mnesia:activity(sync_dirty, Write,
                            [{prim_dict, PrimKey, -11}], mnesia_frag).
  ok
  (a@sam)10> mnesia:activity(sync_dirty, Write,
                             [{sec_dict, SecKey, PrimKey}], mnesia_frag).
  ok
  (a@sam)11> mnesia:change_table_frag(prim_dict, {add_frag, [node()]}).
  {atomic,ok}
  (a@sam)12> SecRead = fun(PrimKey, SecKey) ->
                 mnesia:read({sec_dict, PrimKey}, SecKey, read) end.
  #Fun<erl_eval>
  (a@sam)13> mnesia:activity(transaction, SecRead,
                             [PrimKey, SecKey], mnesia_frag).
  [{sec_dict,42,11}]
  (a@sam)14> Info = fun(Tab, Item) -> mnesia:table_info(Tab, Item) end.
  #Fun<erl_eval>
  (a@sam)15> mnesia:activity(sync_dirty, Info,
                             [prim_dict, frag_size], mnesia_frag).
  [{prim_dict,0},
   {prim_dict_frag2,0},
   {prim_dict_frag3,0},
   {prim_dict_frag4,1},
   {prim_dict_frag5,0},
   {prim_dict_frag6,0},
   {prim_dict_frag7,0},
   {prim_dict_frag8,0}]
  (a@sam)16> mnesia:activity(sync_dirty, Info,
                             [sec_dict, frag_size], mnesia_frag).
  [{sec_dict,0},
   {sec_dict_frag2,0},
   {sec_dict_frag3,0},
   {sec_dict_frag4,1},
   {sec_dict_frag5,0},
   {sec_dict_frag6,0},
   {sec_dict_frag7,0},
   {sec_dict_frag8,0}]
  (a@sam)17>
  ```

### Management of Fragmented Tables

The function `mnesia:change_table_frag(Tab, Change)` is intended to be used for
reconfiguration of fragmented tables. Argument `Change` is to have one of the
following values:

- **`{activate, FragProps}`** - Activates the fragmentation properties of an
  existing table. `FragProps` is either to contain `{node_pool, Nodes}` or be
  empty.

- **`deactivate`** - Deactivates the fragmentation properties of a table. The
  number of fragments must be `1`. No other table can refer to this table in its
  foreign key.

- **`{add_frag, NodesOrDist}`** - Adds a fragment to a fragmented table. All
  records in one of the old fragments are rehashed and about half of them are
  moved to the new (last) fragment. All other fragmented tables, which refer to
  this table in their foreign key, automatically get a new fragment. Also, their
  records are dynamically rehashed in the same manner as for the main table.

  Argument `NodesOrDist` can either be a list of nodes or the result from the
  function [`mnesia:table_info(Tab, frag_dist)`](`mnesia:table_info/2`). Argument
  `NodesOrDist` is assumed to be a sorted list with the best nodes to host new
  replicas first in the list. The new fragment gets the same number of replicas
  as the first fragment (see `n_ram_copies`, `n_disc_copies`, and
  `n_disc_only_copies`). The `NodesOrDist` list must at least contain one
  element for each replica that needs to be allocated.

- **`del_frag`** - Deletes a fragment from a fragmented table. All records in
  the last fragment are moved to one of the other fragments. All other
  fragmented tables, which refer to this table in their foreign key,
  automatically lose their last fragment. Also, their records are dynamically
  rehashed in the same manner as for the main table.

- **`{add_node, Node}`** - Adds a node to `node_pool`. The new node pool affects
  the list returned from the function
  [`mnesia:table_info(Tab, frag_dist)`](`mnesia:table_info/2`).

- **`{del_node, Node}`** - Deletes a node from `node_pool`. The new node pool
  affects the list returned from the function
  [`mnesia:table_info(Tab, frag_dist)`](`mnesia:table_info/2`).

### Extensions of Existing Functions

The function `mnesia:create_table/2` creates a brand new fragmented table, by
setting table property `frag_properties` to some proper values.

The function `mnesia:delete_table/1` deletes a fragmented table including all
its fragments. There must however not exist any other fragmented tables that
refer to this table in their foreign key.

The function `mnesia:table_info/2` now understands item `frag_properties`.

If the function `mnesia:table_info/2` is started in the activity context of
module `mnesia_frag`, information of several new items can be obtained:

- **`base_table`** - The name of the fragmented table

- **`n_fragments`** - The actual number of fragments

- **`node_pool`** - The pool of nodes

- **`n_ram_copies`**

- **`n_disc_copies`**

- **`n_disc_only_copies`** - The number of replicas with storage type
  `ram_copies`, `disc_copies`, and `disc_only_copies`, respectively. The actual
  values are dynamically derived from the first fragment. The first fragment
  serves as a protype. When the actual values need to be computed (for example,
  when adding new fragments) they are determined by counting the number of each
  replica for each storage type. This means that when the functions
  `mnesia:add_table_copy/3`, `mnesia:del_table_copy/2`, and
  [`mnesia:change_table_copy_type/2`](`mnesia:change_table_copy_type/3`) are
  applied on the first fragment, it affects the settings on `n_ram_copies`,
  `n_disc_copies`, and `n_disc_only_copies`.

- **`foreign_key`** - The foreign key

- **`foreigners`** - All other tables that refer to this table in their foreign
  key

- **`frag_names`** - The names of all fragments

- **`frag_dist`** - A sorted list of `{Node, Count}` tuples that are sorted in
  increasing `Count` order. `Count` is the total number of replicas that this
  fragmented table hosts on each `Node`. The list always contains at least all
  nodes in `node_pool`. Nodes that do not belong to `node_pool` are put last in
  the list even if their `Count` is lower.

- **`frag_size`** - A list of `{Name, Size}` tuples, where `Name` is a fragment
  `Name`, and `Size` is how many records it contains

- **`frag_memory`** - A list of `{Name, Memory}` tuples, where `Name` is a
  fragment `Name`, and `Memory` is how much memory it occupies

- **`size`** - Total size of all fragments

- **`memory`** - Total memory of all fragments

### Load Balancing

There are several algorithms for distributing records in a fragmented table
evenly over a pool of nodes. No one is best, it depends on the application
needs. The following examples of situations need some attention:

- `permanent change of nodes`. When a new permanent `db_node` is introduced or
  dropped, it can be time to change the pool of nodes and redistribute the
  replicas evenly over the new pool of nodes. It can also be time to add or
  delete a fragment before the replicas are redistributed.
- `size/memory threshold`. When the total size or total memory of a fragmented
  table (or a single fragment) exceeds some application-specific threshold, it
  can be time to add a new fragment dynamically to obtain a better distribution
  of records.
- `temporary node down`. When a node temporarily goes down, it can be time to
  compensate some fragments with new replicas to keep the desired level of
  redundancy. When the node comes up again, it can be time to remove the
  superfluous replica.
- `overload threshold`. When the load on some node exceeds some
  application-specific threshold, it can be time to either add or move some
  fragment replicas to nodes with lower load. Take extra care if the table has a
  foreign key relation to some other table. To avoid severe performance
  penalties, the same redistribution must be performed for all the related
  tables.

Use the function `mnesia:change_table_frag/2` to add new fragments and apply the
usual schema manipulation functions (such as `mnesia:add_table_copy/3`,
`mnesia:del_table_copy/2`, and
[`mnesia:change_table_copy_type/2`](`mnesia:change_table_copy_type/3`)) on each
fragment to perform the actual redistribution.

## Local Content Tables

Replicated tables have the same content on all nodes where they are replicated.
However, it is sometimes advantageous to have tables, but different content on
different nodes.

If attribute `{local_content, true}` is specified when you create the table, the
table resides on the nodes where you specify the table to exist, but the write
operations on the table are only performed on the local copy.

Furthermore, when the table is initialized at startup, the table is only
initialized locally, and the table content is not copied from another node.

## Disc-Less Nodes

`Mnesia` can be run on nodes that do not have a disc. Replicas of `disc_copies`
or `disc_only_copies` are not possible on such nodes. This is especially
troublesome for the `schema` table, as `Mnesia` needs the schema to initialize
itself.

The schema table can, as other tables, reside on one or more nodes. The storage
type of the schema table can either be `disc_copies` or `ram_copies` (but not
`disc_only_copies`). At startup, `Mnesia` uses its schema to determine with
which nodes it is to try to establish contact. If any other node is started
already, the starting node merges its table definitions with the table
definitions brought from the other nodes. This also applies to the definition of
the schema table itself. Application parameter `extra_db_nodes` contains a list
of nodes that `Mnesia` also is to establish contact with besides those found in
the schema. Default is `[]` (empty list).

Hence, when a disc-less node needs to find the schema definitions from a remote
node on the network, this information must be supplied through application
parameter `-mnesia extra_db_nodes NodeList`. Without this configuration
parameter set, `Mnesia` starts as a single node system. Also, the function
`mnesia:change_config/2` can be used to assign a value to `extra_db_nodes` and
force a connection after `Mnesia` has been started, that is,
`mnesia:change_config(extra_db_nodes, NodeList)`.

Application parameter `schema_location` controls where `Mnesia` searches for its
schema. The parameter can be one of the following atoms:

- **`disc`** - Mandatory disc. The schema is assumed to be located in the
  `Mnesia` directory. If the schema cannot be found, `Mnesia` refuses to start.

- **`ram`** - Mandatory RAM. The schema resides in RAM only. At startup, a tiny
  new schema is generated. This default schema contains only the definition of
  the schema table and resides on the local node only. Since no other nodes are
  found in the default schema, configuration parameter `extra_db_nodes` must be
  used to let the node share its table definitions with other nodes. (Parameter
  `extra_db_nodes` can also be used on disc-full nodes.)

- **`opt_disc`** - Optional disc. The schema can reside on either disc or RAM.
  If the schema is found on disc, `Mnesia` starts as a disc-full node (the
  storage type of the schema table is disc_copies). If no schema is found on
  disc, `Mnesia` starts as a disc-less node (the storage type of the schema
  table is `ram_copies`). The default for the application parameter is
  `opt_disc`.

When `schema_location` is set to `opt_disc`, the function
`mnesia:change_table_copy_type/3` can be used to change the storage type of the
schema. This is illustrated as follows:

```erlang
1> mnesia:start().
ok
2> mnesia:change_table_copy_type(schema, node(), disc_copies).
{atomic, ok}
```

Assuming that the call to `mnesia:start/0` does not find any schema to read on
the disc, `Mnesia` starts as a disc-less node, and then change it to a node that
use the disc to store the schema locally.

## More about Schema Management

Nodes can be added to and removed from a `Mnesia` system. This can be done by
adding a copy of the schema to those nodes.

The functions `mnesia:add_table_copy/3` and `mnesia:del_table_copy/2` can be
used to add and delete replicas of the schema table. Adding a node to the list
of nodes where the schema is replicated affects the following:

- It allows other tables to be replicated to this node.
- It causes `Mnesia` to try to contact the node at startup of disc-full nodes.

The function call `mnesia:del_table_copy(schema, mynode@host)` deletes node
`mynode@host` from the `Mnesia` system. The call fails if `Mnesia` is running on
`mynode@host`. The other `Mnesia` nodes never try to connect to that node again.
Notice that if there is a disc resident schema on node `mynode@host`, the entire
`Mnesia` directory is to be deleted. This is done with the function
`mnesia:delete_schema/1`. If `Mnesia` is started again on node `mynode@host` and
the directory has not been cleared, the behavior of `Mnesia` is undefined.

If the storage type of the schema is `ram_copies`, that is, a disc-less node,
`Mnesia` does not use the disc on that particular node. The disc use is enabled
by changing the storage type of table `schema` to `disc_copies`.

New schemas are created explicitly with the function `mnesia:create_schema/1` or
implicitly by starting `Mnesia` without a disc resident schema. Whenever a table
(including the schema table) is created, it is assigned its own unique cookie.
The schema table is not created with the function `mnesia:create_table/2` as
normal tables.

At startup, `Mnesia` connects different nodes to each other, then they exchange
table definitions with each other, and the table definitions are merged. During
the merge procedure, `Mnesia` performs a sanity test to ensure that the table
definitions are compatible with each other. If a table exists on several nodes,
the cookie must be the same, otherwise `Mnesia` shut down one of the nodes. This
unfortunate situation occurs if a table has been created on two nodes
independently of each other while they were disconnected. To solve this, one of
the tables must be deleted (as the cookies differ, it is regarded to be two
different tables even if they have the same name).

Merging different versions of the schema table does not always require the
cookies to be the same. If the storage type of the schema table is
`disc_copies`, the cookie is immutable, and all other `db_nodes` must have the
same cookie. When the schema is stored as type `ram_copies`, its cookie can be
replaced with a cookie from another node (`ram_copies` or `disc_copies`). The
cookie replacement (during merge of the schema table definition) is performed
each time a RAM node connects to another node.

Further, the following applies:

- [`mnesia:system_info(schema_location)`](`mnesia:system_info/1`) and
  [`mnesia:system_info(extra_db_nodes)`](`mnesia:system_info/1`) can be used to
  determine the actual values of `schema_location` and `extra_db_nodes`,
  respectively.
- [`mnesia:system_info(use_dir)`](`mnesia:system_info/1`) can be used to determine
  whether `Mnesia` is actually using the `Mnesia` directory.
- `use_dir` can be determined even before `Mnesia` is started.

The function `mnesia:info/0` can now be used to print some system information
even before `Mnesia` is started. When `Mnesia` is started, the function prints
more information.

Transactions that update the definition of a table requires that `Mnesia` is
started on all nodes where the storage type of the schema is `disc_copies`. All
replicas of the table on these nodes must also be loaded. There are a few
exceptions to these availability rules:

- Tables can be created and new replicas can be added without starting all the
  disc-full nodes.
- New replicas can be added before all other replicas of the table have been
  loaded, provided that at least one other replica is active.

[](){: #event_handling }

## Mnesia Event Handling

System events and table events are the two event categories that `Mnesia`
generates in various situations.

A user process can subscribe on the events generated by `Mnesia`. The following
two functions are provided:

- [`mnesia:subscribe(Event-Category)`](`mnesia:subscribe/1`) - Ensures that a
  copy of all events of type `Event-Category` are sent to the calling process

- [`mnesia:unsubscribe(Event-Category)`](`mnesia:unsubscribe/1`) - Removes the
  subscription on events of type `Event-Category`

`Event-Category` can be either of the following:

- The atom `system`
- The atom `activity`
- The tuple `{table, Tab, simple}`
- The tuple `{table, Tab, detailed}`

The old event category `{table, Tab}` is the same event category as
`{table, Tab, simple}`.

The subscribe functions activate a subscription of events. The events are
delivered as messages to the process evaluating the function
`mnesia:subscribe/1` The syntax is as follows:

- `{mnesia_system_event, Event}` for system events
- `{mnesia_activity_event, Event}` for activity events
- `{mnesia_table_event, Event}` for table events

The event types are described in the next sections.

All system events are subscribed by the `Mnesia` `gen_event` handler. The
default `gen_event` handler is `mnesia_event`, but it can be changed by using
application parameter `event_module`. The value of this parameter must be the
name of a module implementing a complete handler, as specified by the
`m:gen_event` module in `STDLIB`.

[`mnesia:system_info(subscribers)`](`mnesia:system_info/1`) and
[`mnesia:table_info(Tab, subscribers)`](`mnesia:table_info/2`) can be used to
determine which processes are subscribed to various events.

### System Events

The system events are as follows:

- **`{mnesia_up, Node}`** - Mnesia is started on a node. `Node` is the node
  name. By default this event is ignored.

- **`{mnesia_down, Node}`** - Mnesia is stopped on a node. `Node` is the node
  name. By default this event is ignored.

- **`{mnesia_checkpoint_activated, Checkpoint}`** - A checkpoint with the name
  `Checkpoint` is activated and the current node is involved in the checkpoint.
  Checkpoints can be activated explicitly with the function
  `mnesia:activate_checkpoint/1` or implicitly at backup, when adding table
  replicas, at internal transfer of data between nodes, and so on. By default
  this event is ignored.

- **`{mnesia_checkpoint_deactivated, Checkpoint}`** - A checkpoint with the name
  `Checkpoint` is deactivated and the current node is involved in the
  checkpoint. Checkpoints can be deactivated explicitly with the function
  [`mnesia:deactivate/1`](`mnesia:deactivate_checkpoint/1`) or implicitly when the
  last replica of a table (involved in the checkpoint) becomes unavailable, for
  example, at node-down. By default this event is ignored.

- **`{mnesia_overload, Details}`** - `Mnesia` on the current node is overloaded
  and the subscriber is to take action.

  A typical overload situation occurs when the applications perform more updates
  on disc resident tables than `Mnesia` can handle. Ignoring this kind of
  overload can lead to a situation where the disc space is exhausted (regardless
  of the size of the tables stored on disc).

  Each update is appended to the transaction log and occasionally (depending on
  how it is configured) dumped to the tables files. The table file storage is
  more compact than the transaction log storage, especially if the same record
  is updated repeatedly. If the thresholds for dumping the transaction log are
  reached before the previous dump is finished, an overload event is triggered.

  Another typical overload situation is when the transaction manager cannot
  commit transactions at the same pace as the applications perform updates of
  disc resident tables. When this occurs, the message queue of the transaction
  manager continues to grow until the memory is exhausted or the load decreases.

  The same problem can occur for dirty updates. The overload is detected locally
  on the current node, but its cause can be on another node. Application
  processes can cause high load if any table resides on another node (replicated
  or not). By default this event is reported to `error_logger.`

- **`{inconsistent_database, Context, Node}`** - `Mnesia` regards the database
  as potential inconsistent and gives its applications a chance to recover from
  the inconsistency. For example, by installing a consistent backup as fallback
  and then restart the system. An alternative is to pick a `MasterNode` from
  [`mnesia:system_info(db_nodes)`](`mnesia:system_info/1`) and invoke
  [`mnesia:set_master_nodes([MasterNode])`](`mnesia:set_master_nodes/1`). By
  default an error is reported to `error_logger`.

- **`{mnesia_fatal, Format, Args, BinaryCore}`** - `Mnesia` detected a fatal
  error and terminates soon. The fault reason is explained in `Format` and
  `Args`, which can be given as input to `io:format/2` or sent to
  `error_logger`. By default it is sent to `error_logger`.

  `BinaryCore` is a binary containing a summary of the `Mnesia` internal state
  at the time when the fatal error was detected. By default the binary is
  written to a unique filename on the current directory. On RAM nodes, the core
  is ignored.

- **`{mnesia_info, Format, Args}`** - `Mnesia` detected something that can be of
  interest when debugging the system. This is explained in `Format` and `Args`,
  which can appear as input to `io:format/2` or sent to `error_logger`. By
  default this event is printed with `io:format/2`.

- **`{mnesia_error, Format, Args}`** - `Mnesia` has detected an error. The fault
  reason is explained in `Format` and `Args`, which can be given as input to
  `io:format/2` or sent to `error_logger`. By default this event is reported to
  `error_logger`.

- **`{mnesia_user, Event}`** - An application started the function
  [`mnesia:report_event(Event)`](`mnesia:report_event/1`). `Event` can be any
  Erlang data structure. When tracing a system of `Mnesia` applications, it is
  useful to be able to interleave own events of `Mnesia` with
  application-related events that give information about the application
  context. Whenever the application starts with a new and demanding `Mnesia`
  activity, or enters a new and interesting phase in its execution, it can be a
  good idea to use `mnesia:report_event/1`.

### Activity Events

Currently, there is only one type of activity event:

- **`{complete, ActivityID}`** - This event occurs when a transaction that
  caused a modification to the database is completed. It is useful for
  determining when a set of table events (see the next section), caused by a
  given activity, have been sent. Once this event is received, it is guaranteed
  that no further table events with the same `ActivityID` will be received.
  Notice that this event can still be received even if no table events with a
  corresponding `ActivityID` were received, depending on the tables to which the
  receiving process is subscribed.

  Dirty operations always contain only one update and thus no activity event is
  sent.

### Table Events

Table events are events related to table updates. There are two types of table
events, simple and detailed.

The _simple table events_ are tuples like `{Oper, Record, ActivityId}`, where:

- `Oper` is the operation performed.
- `Record` is the record involved in the operation.
- `ActivityId` is the identity of the transaction performing the operation.

Notice that the record name is the table name even when `record_name` has
another setting.

The table-related events that can occur are as follows:

- **`{write, NewRecord, ActivityId}`** - A new record has been written.
  `NewRecord` contains the new record value.

- **`{delete_object, OldRecord, ActivityId}`** - A record has possibly been
  deleted with `mnesia:delete_object/1`. `OldRecord` contains the value of the
  old record, as stated as argument by the application. Notice that other
  records with the same key can remain in the table if it is of type `bag`.

- **`{delete, {Tab, Key}, ActivityId}`** - One or more records have possibly
  been deleted. All records with the key `Key` in the table `Tab` have been
  deleted.

The _detailed table events_ are tuples like
`{Oper, Table, Data, [OldRecs], ActivityId}`, where:

- `Oper` is the operation performed.
- `Table` is the table involved in the operation.
- `Data` is the record/OID written/deleted.
- `OldRecs` is the contents before the operation.
- `ActivityId` is the identity of the transaction performing the operation.

The table-related events that can occur are as follows:

- **`{write, Table, NewRecord, [OldRecords], ActivityId}`** - A new record has
  been written. `NewRecord` contains the new record value and `OldRecords`
  contains the records before the operation is performed. Notice that the new
  content depends on the table type.

- **`{delete, Table, What, [OldRecords], ActivityId}`** - Records have possibly
  been deleted. `What` is either `{Table, Key}` or a record
  `{RecordName, Key, ...}` that was deleted. Notice that the new content depends
  on the table type.

## Debugging Mnesia Applications

Debugging a `Mnesia` application can be difficult for various reasons, primarily
related to difficulties in understanding how the transaction and table load
mechanisms work. Another source of confusion can be the semantics of nested
transactions.

The debug level of `Mnesia` is set by calling the function
[`mnesia:set_debug_level(Level)`](`mnesia:set_debug_level/1`), where `Level` is
one of the following:

- **`none`** - No trace outputs. This is the default.

- **`verbose`** - Activates tracing of important debug events. These events
  generate `{mnesia_info, Format, Args}` system events. Processes can subscribe
  to these events with the function `mnesia:subscribe/1`. The events are always
  sent to the `Mnesia` event handler.

- **`debug`** - Activates all events at the verbose level plus traces of all
  debug events. These debug events generate `{mnesia_info, Format, Args}` system
  events. Processes can subscribe to these events with `mnesia:subscribe/1`. The
  events are always sent to the `Mnesia` event handler. On this debug level, the
  `Mnesia` event handler starts subscribing to updates in the schema table.

- **`trace`** - Activates all events at the debug level. On this level, the
  `Mnesia` event handler starts subscribing to updates on all `Mnesia` tables.
  This level is intended only for debugging small toy systems, as many large
  events can be generated.

- **`false`** - An alias for none.

- **`true`** - An alias for debug.

The debug level of `Mnesia` itself is also an application parameter, making it
possible to start an Erlang system to turn on `Mnesia` debug in the initial
startup phase by using the following code:

```text
% erl -mnesia debug verbose
```

## Concurrent Processes in Mnesia

Programming concurrent Erlang systems is the subject of a separate book.
However, it is worthwhile to draw attention to the following features, which
permit concurrent processes to exist in a `Mnesia` system:

- A group of functions or processes can be called within a transaction. A
  transaction can include statements that read, write, or delete data from the
  DBMS. Many such transactions can run concurrently, and the programmer does not
  need to explicitly synchronize the processes that manipulate the data.

  All programs accessing the database through the transaction system can be
  written as if they had sole access to the data. This is a desirable property,
  as all synchronization is taken care of by the transaction handler. If a
  program reads or writes data, the system ensures that no other program tries
  to manipulate the same data at the same time.

- Tables can be moved or deleted, and the layout of a table can be reconfigured
  in various ways. An important aspect of the implementation of these functions
  is that user programs can continue to use a table while it is being
  reconfigured. For example, it is possible to move a table and perform write
  operations to the table at the same time. This is important for many
  applications that require continuously available services. For more
  information, see
  [Transactions and Other Access Contexts](mnesia_chap4.md#trans_prop).

## Prototyping

If and when you would like to start and manipulate `Mnesia`, it is often easier
to write the definitions and data into an ordinary text file. Initially, no
tables and no data exist, or which tables are required. At the initial stages of
prototyping, it is prudent to write all data into one file, process that file,
and have the data in the file inserted into the database. `Mnesia` can be
initialized with data read from a text file. The following two functions can be
used to work with text files.

- [`mnesia:load_textfile(Filename)`](`mnesia:load_textfile/1`) loads a series of
  local table definitions and data found in the file into `Mnesia`. This
  function also starts `Mnesia` and possibly creates a new schema. The function
  operates on the local node only.
- [`mnesia:dump_to_textfile(Filename)`](`mnesia:dump_to_textfile/1`) dumps all
  local tables of a `Mnesia` system into a text file, which can be edited (with
  a normal text editor) and later reloaded.

These functions are much slower than the ordinary store and load functions of
`Mnesia`. However, this is mainly intended for minor experiments and initial
prototyping. The major advantage of these functions is that they are easy to
use.

The format of the text file is as follows:

```erlang
{tables, [{Typename, [Options]},
{Typename2 ......}]}.

{Typename, Attribute1, Attribute2 ....}.
{Typename, Attribute1, Attribute2 ....}.
```

`Options` is a list of `{Key,Value}` tuples conforming to the options that you
can give to `mnesia:create_table/2`.

For example, to start playing with a small database for healthy foods, enter the
following data into file `FRUITS`:

```erlang
{tables,
 [{fruit, [{attributes, [name, color, taste]}]},
  {vegetable, [{attributes, [name, color, taste, price]}]}]}.


{fruit, orange, orange, sweet}.
{fruit, apple, green, sweet}.
{vegetable, carrot, orange, carrotish, 2.55}.
{vegetable, potato, yellow, none, 0.45}.
```

The following session with the Erlang shell shows how to load the `FRUITS`
database:

```erlang
% erl
Erlang (BEAM) emulator version 4.9

Eshell V4.9  (abort with ^G)
1> mnesia:load_textfile("FRUITS").
New table fruit
New table vegetable
{atomic,ok}
2> mnesia:info().
---> Processes holding locks <---
---> Processes waiting for locks <---
---> Pending (remote) transactions <---
---> Active (local) transactions <---
---> Uncertain transactions <---
---> Active tables <---
vegetable      : with 2 records occuping 299 words of mem
fruit          : with 2 records occuping 291 words of mem
schema         : with 3 records occuping 401 words of mem
===> System info in version "1.1", debug level = none <===
opt_disc. Directory "/var/tmp/Mnesia.nonode@nohost" is used.
use fallback at restart = false
running db nodes = [nonode@nohost]
stopped db nodes = []
remote           = []
ram_copies       = [fruit,vegetable]
disc_copies      = [schema]
disc_only_copies = []
[{nonode@nohost,disc_copies}] = [schema]
[{nonode@nohost,ram_copies}] = [fruit,vegetable]
3 transactions committed, 0 aborted, 0 restarted, 2 logged to disc
0 held locks, 0 in queue; 0 local transactions, 0 remote
0 transactions waits for other nodes: []
ok
3>
```

It can be seen that the DBMS was initiated from a regular text file.

## Object-Based Programming with Mnesia

The `Company` database, introduced in
[Getting Started](mnesia_chap2.md#getting_started), has three tables that store
records (`employee`, `dept`, `project`), and three tables that store
relationships (`manager`, `at_dep`, `in_proj`). This is a normalized data model,
which has some advantages over a non-normalized data model.

It is more efficient to do a generalized search in a normalized database. Some
operations are also easier to perform on a normalized data model. For example,
one project can easily be removed, as the following example illustrates:

```erlang
remove_proj(ProjName) ->
    F = fun() ->
                Ip = qlc:e(qlc:q([X || X <- mnesia:table(in_proj),
				       X#in_proj.proj_name == ProjName]
				)),
                mnesia:delete({project, ProjName}),
                del_in_projs(Ip)
        end,
    mnesia:transaction(F).

del_in_projs([Ip|Tail]) ->
    mnesia:delete_object(Ip),
    del_in_projs(Tail);
del_in_projs([]) ->
    done.
```

In reality, data models are seldom fully normalized. A realistic alternative to
a normalized database model would be a data model that is not even in first
normal form. `Mnesia` is suitable for applications such as telecommunications,
because it is easy to organize data in a flexible manner. A `Mnesia` database is
always organized as a set of tables. Each table is filled with rows, objects,
and records. What sets `Mnesia` apart is that individual fields in a record can
contain any type of compound data structures. An individual field in a record
can contain lists, tuples, functions, and even record code.

Many telecommunications applications have unique requirements on lookup times
for certain types of records. If the `Company` database had been a part of a
telecommunications system, it could be to minimize the lookup time of an
employee _together_ with a list of the projects the employee is working on. If
this is the case, a drastically different data model without direct
relationships can be chosen. You would then have only the records themselves,
and different records could contain either direct references to other records,
or contain other records that are not part of the `Mnesia` schema.

The following record definitions can be created:

```erlang
-record(employee, {emp_no,
		   name,
		   salary,
		   sex,
		   phone,
		   room_no,
		   dept,
		   projects,
		   manager}).


-record(dept, {id,
               name}).

-record(project, {name,
                  number,
                  location}).
```

A record that describes an employee can look as follows:

```erlang
Me = #employee{emp_no = 104732,
               name = klacke,
               salary = 7,
               sex = male,
               phone = 99586,
               room_no = {221, 015},
               dept = 'B/SFR',
               projects = [erlang, mnesia, otp],
               manager = 114872},
```

This model has only three different tables, and the employee records contain
references to other records. The record has the following references:

- `'B/SFR'` refers to a `dept` record.
- `[erlang, mnesia, otp]` is a list of three direct references to three
  different `projects` records.
- `114872` refers to another employee record.

The `Mnesia` record identifiers (`{Tab, Key}`) can also be used as references.
In this case, attribute `dept` would be set to value `{dept, 'B/SFR'}` instead
of `'B/SFR'`.

With this data model, some operations execute considerably faster than they do
with the normalized data model in the `Company` database. However, some other
operations become much more complicated. In particular, it becomes more
difficult to ensure that records do not contain dangling pointers to other
non-existent, or deleted, records.

The following code exemplifies a search with a non-normalized data model. To
find all employees at department `Dep` with a salary higher than `Salary`, use
the following code:

```erlang
get_emps(Salary, Dep) ->
    Q = qlc:q(
          [E || E <- mnesia:table(employee),
                E#employee.salary > Salary,
                E#employee.dept == Dep]
	 ),
    F = fun() -> qlc:e(Q) end,
    transaction(F).
```

This code is easier to write and to understand, and it also executes much
faster.

It is easy to show examples of code that executes faster if a non-normalized
data model is used, instead of a normalized model. The main reason is that fewer
tables are required. Therefore, data from different tables can more easily be
combined in join operations. In the previous example, the function `get_emps/2`
is transformed from a join operation into a simple query, which consists of a
selection and a projection on one single table.
