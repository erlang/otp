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
# Getting Started

[](){: #getting_started }

This section introduces `Mnesia` with an example database. This example is
referenced in the following sections, where the example is modified to
illustrate various program constructs. This section illustrates the following
mandatory procedures through examples:

- Starting the Erlang session.
- Specifying the `Mnesia` directory where the database is to be stored.
- Initializing a new database schema with an attribute that specifies on which
  node, or nodes, that database is to operate.
- Starting `Mnesia`.
- Creating and populating the database tables.

## Starting Mnesia for the First Time

This section provides a simplified demonstration of a `Mnesia` system startup.
The dialogue from the Erlang shell is as follows:

```erlang
        unix>  erl -mnesia dir '"/tmp/funky"'
        Erlang (BEAM) emulator version 4.9

        Eshell V4.9  (abort with ^G)
        1>
        1> mnesia:create_schema([node()]).
        ok
        2> mnesia:start().
        ok
        3> mnesia:create_table(funky, []).
        {atomic,ok}
        4> mnesia:info().
        ---> Processes holding locks <---
        ---> Processes waiting for locks <---
        ---> Pending (remote) transactions <---
        ---> Active (local) transactions <---
        ---> Uncertain transactions <---
        ---> Active tables <---
        funky          : with 0 records occupying 269 words of mem
        schema         : with 2 records occupying 353 words of mem
        ===> System info in version "1.0", debug level = none <===
        opt_disc. Directory "/tmp/funky" is used.
        use fall-back at restart = false
        running db nodes = [nonode@nohost]
        stopped db nodes = []
        remote           = []
        ram_copies       = [funky]
        disc_copies      = [schema]
        disc_only_copies = []
        [{nonode@nohost,disc_copies}] = [schema]
        [{nonode@nohost,ram_copies}] = [funky]
        1 transactions committed, 0 aborted, 0 restarted, 1 logged to disc
        0 held locks, 0 in queue; 0 local transactions, 0 remote
        0 transactions waits for other nodes: []
        ok
```

In this example, the following actions are performed:

- _Step 1:_ The Erlang system is started from the UNIX prompt with a flag
  `-mnesia dir '"/tmp/funky"'`, which indicates in which directory to store the
  data.
- _Step 2:_ A new empty schema is initialized on the local node by evaluating
  [mnesia:create_schema(\[node()])](`mnesia:create_schema/1`). The schema
  contains information about the database in general. This is explained in
  detail later.
- _Step 3:_ The DBMS is started by evaluating
  [mnesia:start()](`mnesia:start/0`).
- _Step 4:_ A first table is created, called `funky`, by evaluating the
  expression `mnesia:create_table(funky, [])`. The table is given default
  properties.
- _Step 5:_ [mnesia:info()](`mnesia:info/0`) is evaluated to display information
  on the terminal about the status of the database.

## Example

A `Mnesia` database is organized as a set of tables. Each table is populated
with instances (Erlang records). A table has also a number of properties, such
as location and persistence.

### Database

This example shows how to create a database called `Company` and the
relationships shown in the following diagram:

![Company Entity-Relation Diagram](assets/company.gif "Company Entity-Relation Diagram")

The database model is as follows:

- There are three entities: department, employee, and project.
- There are three relationships between these entities:

  1. A department is managed by an employee, hence the `manager` relationship.
  1. An employee works at a department, hence the `at_dep` relationship.
  1. Each employee works on a number of projects, hence the `in_proj`
     relationship.

### Defining Structure and Content

First the record definitions are entered into a text file named `company.hrl`.
This file defines the following structure for the example database:

```erlang

-record(employee, {emp_no,
                   name,
                   salary,
                   sex,
                   phone,
                   room_no}).

-record(dept, {id,
               name}).

-record(project, {name,
                  number}).


-record(manager, {emp,
                  dept}).

-record(at_dep, {emp,
                 dept_id}).

-record(in_proj, {emp,
                  proj_name}).
```

The structure defines six tables in the database. In `Mnesia`, the function
[mnesia:create_table(Name, ArgList)](`mnesia:create_table/2`) creates tables.
`Name` is the table name.

> #### Note {: .info }
>
> The current version of `Mnesia` does not require that the name of the table is
> the same as the record name, see
> [Record Names versus Table Names.](mnesia_chap4.md#recordnames_tablenames).

For example, the table for employees is created with the function
`mnesia:create_table(employee, [{attributes, record_info(fields, employee)}])`.
The table name `employee` matches the name for records specified in `ArgList`.
The expression `record_info(fields, RecordName)` is processed by the Erlang
preprocessor and evaluates to a list containing the names of the different
fields for a record.

### Program

The following shell interaction starts `Mnesia` and initializes the schema for
the `Company` database:

```erlang
        % erl -mnesia dir '"/ldisc/scratch/Mnesia.Company"'
         Erlang (BEAM) emulator version 4.9

          Eshell V4.9  (abort with ^G)
          1> mnesia:create_schema([node()]).
          ok
          2> mnesia:start().
          ok
```

The following program module creates and populates previously defined tables:

```erlang


-include_lib("stdlib/include/qlc.hrl").
-include("company.hrl").

init() ->
    mnesia:create_table(employee,
                        [{attributes, record_info(fields, employee)}]),
    mnesia:create_table(dept,
                        [{attributes, record_info(fields, dept)}]),
    mnesia:create_table(project,
                        [{attributes, record_info(fields, project)}]),
    mnesia:create_table(manager, [{type, bag},
                                  {attributes, record_info(fields, manager)}]),
    mnesia:create_table(at_dep,
                         [{attributes, record_info(fields, at_dep)}]),
    mnesia:create_table(in_proj, [{type, bag},
                                  {attributes, record_info(fields, in_proj)}]).
```

### Program Explained

The following commands and functions are used to initiate the `Company`
database:

- `% erl -mnesia dir '"/ldisc/scratch/Mnesia.Company"'`. This is a UNIX
  command-line entry that starts the Erlang system. The flag `-mnesia dir Dir`
  specifies the location of the database directory. The system responds and
  waits for further input with the prompt `1>`.
- [mnesia:create_schema(\[node()])](`mnesia:create_schema/1`). This function has
  the format `mnesia:create_schema(DiscNodeList)` and initiates a new schema. In
  this example, a non-distributed system using only one node is created. Schemas
  are fully explained in [Define a Schema](mnesia_chap3.md#def_schema).
- [mnesia:start()](`mnesia:start/0`). This function starts `Mnesia` and is fully
  explained in [Start Mnesia](mnesia_chap3.md#start_mnesia).

Continuing the dialogue with the Erlang shell produces the following:

```erlang
        3> company:init().
        {atomic,ok}
        4> mnesia:info().
        ---> Processes holding locks <---
        ---> Processes waiting for locks <---
        ---> Pending (remote) transactions <---
        ---> Active (local) transactions <---
        ---> Uncertain transactions <---
        ---> Active tables <---
        in_proj        : with 0 records occuping 269 words of mem
        at_dep         : with 0 records occuping 269 words of mem
        manager        : with 0 records occuping 269 words of mem
        project        : with 0 records occuping 269 words of mem
        dept           : with 0 records occuping 269 words of mem
        employee       : with 0 records occuping 269 words of mem
        schema         : with 7 records occuping 571 words of mem
        ===> System info in version "1.0", debug level = none <===
        opt_disc. Directory "/ldisc/scratch/Mnesia.Company" is used.
        use fall-back at restart = false
        running db nodes = [nonode@nohost]
        stopped db nodes = []
        remote           = []
        ram_copies       =
            [at_dep,dept,employee,in_proj,manager,project]
        disc_copies      = [schema]
        disc_only_copies = []
        [{nonode@nohost,disc_copies}] = [schema]
        [{nonode@nohost,ram_copies}] =
            [employee,dept,project,manager,at_dep,in_proj]
        6 transactions committed, 0 aborted, 0 restarted, 6 logged to disc
        0 held locks, 0 in queue; 0 local transactions, 0 remote
        0 transactions waits for other nodes: []
        ok
```

A set of tables is created. The function
[mnesia:create_table(Name, ArgList)](`mnesia:create_table/2`) creates the
required database tables. The options available with `ArgList` are explained in
[Create New Tables](mnesia_chap3.md#create_tables).

The function `company:init/0` creates the tables. Two tables are of type `bag`.
This is the `manager` relation as well the `in_proj` relation. This is
interpreted as: an employee can be manager over several departments, and an
employee can participate in several projects. However, the `at_dep` relation is
`set`, as an employee can only work in one department. In this data model, there
are examples of relations that are 1-to-1 (`set`) and 1-to-many (`bag`).

[mnesia:info()](`mnesia:info/0`) now indicates that a database has seven local
tables, where six are the user-defined tables and one is the schema. Six
transactions have been committed, as six successful transactions were run when
creating the tables.

To write a function that inserts an employee record into the database, there
must be an `at_dep` record and a set of `in_proj` records inserted. Examine the
following code used to complete this action:

```erlang


insert_emp(Emp, DeptId, ProjNames) ->
    Ename = Emp#employee.name,
    Fun = fun() ->
                  mnesia:write(Emp),
                  AtDep = #at_dep{emp = Ename, dept_id = DeptId},
                  mnesia:write(AtDep),
                  mk_projs(Ename, ProjNames)
          end,
    mnesia:transaction(Fun).


mk_projs(Ename, [ProjName|Tail]) ->
    mnesia:write(#in_proj{emp = Ename, proj_name = ProjName}),
    mk_projs(Ename, Tail);
mk_projs(_, []) -> ok.
```

- The `insert_emp/3` arguments are as follows:

  1. `Emp` is an employee record.
  1. `DeptId` is the identity of the department where the employee works.
  1. `ProjNames` is a list of the names of the projects where the employee
     works.

The function `insert_emp/3` creates a Functional Object (Fun). `Fun` is passed
as a single argument to the function
[mnesia:transaction(Fun)](`mnesia:transaction/1`). This means that `Fun` is run
as a transaction with the following properties:

- A `Fun` either succeeds or fails.
- Code that manipulates the same data records can be run concurrently without
  the different processes interfering with each other.

The function can be used as follows:

```erlang
          Emp  = #employee{emp_no= 104732,
                           name = klacke,
                           salary = 7,
                           sex = male,
                           phone = 98108,
                           room_no = {221, 015}},
        insert_emp(Emp, 'B/SFR', [Erlang, mnesia, otp]).
```

> #### Note {: .info }
>
> For information about Funs, see "Fun Expressions" in section
> `Erlang Reference Manual` in System Documentation..

### Initial Database Content

After the insertion of the employee named `klacke`, the database has the
following records:

| emp_no | name   | salary | sex  | phone | room_no      |
| ------ | ------ | ------ | ---- | ----- | ------------ |
| 104732 | klacke | 7      | male | 98108 | \{221, 015\} |

{: #table2_1 }

_Table: employee Database Record_

This `employee` record has the Erlang record/tuple representation
`{employee, 104732, klacke, 7, male, 98108, {221, 015}}`.

| emp    | dept_name |
| ------ | --------- |
| klacke | B/SFR     |

{: #table2_2 }

_Table: at_dep Database Record_

This `at_dep` record has the Erlang tuple representation
`{at_dep, klacke, 'B/SFR'}`.

| emp    | proj_name |
| ------ | --------- |
| klacke | Erlang    |
| klacke | otp       |
| klacke | mnesia    |

{: #table3_3 }

_Table: in_proj Database Record_

This `in_proj` record has the Erlang tuple representation
`{in_proj, klacke, 'Erlang', klacke, 'otp', klacke, 'mnesia'}`.

There is no difference between rows in a table and `Mnesia` records. Both
concepts are the same and are used interchangeably throughout this User's Guide.

A `Mnesia` table is populated by `Mnesia` records. For example, the tuple
`{boss, klacke, bjarne}` is a record. The second element in this tuple is the
key. To identify a table uniquely, both the key and the table name is needed.
The term Object Identifier (OID) is sometimes used for the arity two tuple
\{Tab, Key\}. The OID for the record `{boss, klacke, bjarne}` is the arity two
tuple `{boss, klacke}`. The first element of the tuple is the type of the record
and the second element is the key. An OID can lead to zero, one, or more records
depending on whether the table type is `set` or `bag`.

The record `{boss, klacke, bjarne}` can also be inserted. This record contains
an implicit reference to another employee that does not yet exist in the
database. `Mnesia` does not enforce this.

### Adding Records and Relationships to Database

After adding more records to the `Company` database, the result can be the
following records:

`employees`:

```erlang
        {employee, 104465, "Johnson Torbjorn",   1, male,  99184, {242,038}}.
        {employee, 107912, "Carlsson Tuula",     2, female,94556, {242,056}}.
        {employee, 114872, "Dacker Bjarne",      3, male,  99415, {221,035}}.
        {employee, 104531, "Nilsson Hans",       3, male,  99495, {222,026}}.
        {employee, 104659, "Tornkvist Torbjorn", 2, male,  99514, {222,022}}.
        {employee, 104732, "Wikstrom Claes",     2, male,  99586, {221,015}}.
        {employee, 117716, "Fedoriw Anna",       1, female,99143, {221,031}}.
        {employee, 115018, "Mattsson Hakan",     3, male,  99251, {203,348}}.
```

`dept`:

```erlang
        {dept, 'B/SF',  "Open Telecom Platform"}.
        {dept, 'B/SFP', "OTP - Product Development"}.
        {dept, 'B/SFR', "Computer Science Laboratory"}.
```

`projects`:

```erlang
        %% projects
        {project, erlang, 1}.
        {project, otp, 2}.
        {project, beam, 3}.
        {project, mnesia, 5}.
        {project, wolf, 6}.
        {project, documentation, 7}.
        {project, www, 8}.
```

These three tables, `employees`, `dept`, and `projects`, are made up of real
records. The following database content is stored in the tables and is built on
relationships. These tables are `manager`, `at_dep`, and `in_proj`.

`manager`:

```erlang
        {manager, 104465, 'B/SF'}.
        {manager, 104465, 'B/SFP'}.
        {manager, 114872, 'B/SFR'}.
```

`at_dep`:

```erlang
        {at_dep, 104465, 'B/SF'}.
        {at_dep, 107912, 'B/SF'}.
        {at_dep, 114872, 'B/SFR'}.
        {at_dep, 104531, 'B/SFR'}.
        {at_dep, 104659, 'B/SFR'}.
        {at_dep, 104732, 'B/SFR'}.
        {at_dep, 117716, 'B/SFP'}.
        {at_dep, 115018, 'B/SFP'}.
```

`in_proj`:

```erlang
        {in_proj, 104465, otp}.
        {in_proj, 107912, otp}.
        {in_proj, 114872, otp}.
        {in_proj, 104531, otp}.
        {in_proj, 104531, mnesia}.
        {in_proj, 104545, wolf}.
        {in_proj, 104659, otp}.
        {in_proj, 104659, wolf}.
        {in_proj, 104732, otp}.
        {in_proj, 104732, mnesia}.
        {in_proj, 104732, erlang}.
        {in_proj, 117716, otp}.
        {in_proj, 117716, documentation}.
        {in_proj, 115018, otp}.
        {in_proj, 115018, mnesia}.
```

The room number is an attribute of the employee record. This is a structured
attribute that consists of a tuple. The first element of the tuple identifies a
corridor, and the second element identifies the room in that corridor. An
alternative is to represent this as a record `-record(room, {corr, no}).`
instead of an anonymous tuple representation.

The `Company` database is now initialized and contains data.

### Writing Queries

Retrieving data from DBMS is usually to be done with the functions
`mnesia:read/3` or [mnesia:read/1](`mnesia:read/2`). The following function
raises the salary:

```erlang

raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read(employee, Eno, write),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                mnesia:write(New)
        end,
    mnesia:transaction(F).
```

Since it is desired to update the record using the function `mnesia:write/1`
after the salary has been increased, a write lock (third argument to `read`) is
acquired when the record from the table is read.

To read the values from the table directly is not always possible. It can be
needed to search one or more tables to get the wanted data, and this is done by
writing database queries. Queries are always more expensive operations than
direct lookups done with `mnesia:read`. Therefore, avoid queries in
performance-critical code.

Two methods are available for writing database queries:

- `Mnesia` functions
- QLC

#### Using Mnesia Functions

The following function extracts the names of the female employees stored in the
database:

```erlang
mnesia:select(employee, [{#employee{sex = female, name = '$1', _ = '_'},[], ['$1']}]).
```

`select` must always run within an activity, such as a transaction. The
following function can be constructed to call from the shell:

```erlang

all_females() ->
    F = fun() ->
		Female = #employee{sex = female, name = '$1', _ = '_'},
		mnesia:select(employee, [{Female, [], ['$1']}])
        end,
    mnesia:transaction(F).
```

The `select` expression matches all entries in table employee with the field
`sex` set to `female`.

This function can be called from the shell as follows:

```erlang
          (klacke@gin)1> company:all_females().
          {atomic,  ["Carlsson Tuula", "Fedoriw Anna"]}
```

For a description of `select` and its syntax, see
[Pattern Matching](mnesia_chap4.md#matching).

#### Using QLC

This section contains simple introductory examples only. For a full description
of the QLC query language, see the `m:qlc` manual page in `STDLIB`.

Using QLC can be more expensive than using `Mnesia` functions directly but
offers a nice syntax.

The following function extracts a list of female employees from the database:

```erlang
          Q = qlc:q([E#employee.name || E <- mnesia:table(employee),
                                E#employee.sex == female]),
          qlc:e(Q),
```

Accessing `Mnesia` tables from a QLC list comprehension must always be done
within a transaction. Consider the following function:

```erlang

females() ->
    F = fun() ->
		Q = qlc:q([E#employee.name || E <- mnesia:table(employee),
					      E#employee.sex == female]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).
```

This function can be called from the shell as follows:

```erlang
          (klacke@gin)1> company:females().
          {atomic, ["Carlsson Tuula", "Fedoriw Anna"]}
```

In traditional relational database terminology, this operation is called a
selection, followed by a projection.

The previous list comprehension expression contains a number of syntactical
elements:

- The first `[` bracket is read as "build the list".
- The `||` "such that" and the arrow `<-` is read as "taken from".

Hence, the previous list comprehension demonstrates the formation of the list
`E#employee.name` such that `E` is taken from the table of employees, and
attribute `sex` of each record is equal to the atom `female`.

The whole list comprehension must be given to the function `qlc:q/1`.

List comprehensions with low-level `Mnesia` functions can be combined in the
same transaction. To raise the salary of all female employees, execute the
following:

```erlang

raise_females(Amount) ->
    F = fun() ->
                Q = qlc:q([E || E <- mnesia:table(employee),
                                E#employee.sex == female]),
		Fs = qlc:e(Q),
                over_write(Fs, Amount)
        end,
    mnesia:transaction(F).

over_write([E|Tail], Amount) ->
    Salary = E#employee.salary + Amount,
    New = E#employee{salary = Salary},
    mnesia:write(New),
    1 + over_write(Tail, Amount);
over_write([], _) ->
    0.
```

The function `raise_females/1` returns the tuple `{atomic, Number}`, where
`Number` is the number of female employees who received a salary increase. If an
error occurs, the value `{aborted, Reason}` is returned, and `Mnesia` guarantees
that the salary is not raised for any employee.

_Example:_

```text
          33>company:raise_females(33).
          {atomic,2}
```
