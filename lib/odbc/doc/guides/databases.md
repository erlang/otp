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
# Databases

## Databases

If you need to access a relational database such as `sqlserver`, `mysql`,
`postgres`, `oracle`, `cybase` etc. from your erlang application using the
Erlang ODBC interface is a good way to go about it.

The Erlang ODBC application should work for any relational database that has an
ODBC driver. But currently it is only regularly tested for `sqlserver` and
`postgres`.

## Database independence

The Erlang ODBC interface is in principal database independent, e.i. an erlang
program using the interface could be run without changes towards different
databases. But as SQL is used it is alas possible to write database dependent
programs. Even though SQL is an ANSI-standard meant to be database independent,
different databases have proprietary extensions to SQL defining their own data
types. If you keep to the ANSI data types you will minimize the problem. But
unfortunately there is no guarantee that all databases actually treats the ANSI
data types equivalently. For instance an installation of
`Oracle Enterprise release 8.0.5.0.0 for unix` will accept that you create a
table column with the ANSI data type `integer`, but when retrieving values from
this column the driver reports that it is of type `SQL_DECIMAL(0, 38)` and not
`SQL_INTEGER` as you may have expected.

Another obstacle is that some drivers do not support scrollable cursors which
has the effect that the only way to traverse the result set is sequentially,
with next, from the first row to the last, and once you pass a row you cannot go
back. This means that some functions in the interface will not work together
with certain drivers. A similar problem is that not all drivers support "row
count" for select queries, hence resulting in that the function
`select_count/[3,4]` will return `{ok, undefined}` instead of `{ok, NrRows}`
where `NrRows` is the number of rows in the result set.

## Data types

The following is a list of the ANSI data types. For details turn to the ANSI
standard documentation. Usage of other data types is of course possible, but you
should be aware that this makes your application dependent on the database you
are using at the moment.

- CHARACTER (size), CHAR (size)
- NUMERIC (precision, scale), DECIMAL (precision, scale), DEC (precision, scale
  ) precision - total number of digits, scale - total number of decimal places
- INTEGER, INT, SMALLINT
- FLOAT (precision)
- REAL
- DOUBLE PRECISION
- CHARACTER VARYING(size), CHAR VARYING(size)

When inputting data using sql_query/\[2,3] the values will always be in string
format as they are part of an SQL-query. Example:

```text
      odbc:sql_query(Ref, "INSERT INTO TEST VALUES(1, 2, 3)").
```

> #### Note {: .info }
>
> Note that when the value of the data to input is a string, it has to be quoted
> with `'`. Example:
>
> ```text
> odbc:sql_query(Ref, "INSERT INTO EMPLOYEE VALUES(1, 'Jane', 'Doe', 'F')").
> ```

You may also input data using [param_query/\[3,4]](`m:odbc#param_query`) and
then the input data will have the Erlang type corresponding to the ODBC type of
the column.[See ODBC to Erlang mapping](databases.md#type)

[](){: #type } When selecting data from a table, all data types are returned
from the database to the ODBC driver as an ODBC data type. The tables below
shows the mapping between those data types and what is returned by the Erlang
API.

| ODBC Data Type                                                                | Erlang Data Type                               |
| ----------------------------------------------------------------------------- | ---------------------------------------------- |
| SQL_CHAR(size)                                                                | String \| Binary (configurable)                |
| SQL_WCHAR(size)                                                               | Unicode binary encoded as UTF16 little endian. |
| SQL_NUMERIC(p,s) when (p >= 0 and p <= 9 and s == 0)                          | Integer                                        |
| SQL_NUMERIC(p,s) when (p >= 10 and p <= 15 and s == 0) or (s <= 15 and s > 0) | Float                                          |
| SQL_NUMERIC(p,s) when p >= 16                                                 | String                                         |
| SQL_DECIMAL(p,s) when (p >= 0 and p <= 9 and s == 0)                          | Integer                                        |
| SQL_DECIMAL(p,s) when (p >= 10 and p <= 15 and s == 0) or (s <= 15 and s > 0) | Float                                          |
| SQL_DECIMAL(p,s) when p >= 16                                                 | String                                         |
| SQL_INTEGER                                                                   | Integer                                        |
| SQL_SMALLINT                                                                  | Integer                                        |
| SQL_FLOAT                                                                     | Float                                          |
| SQL_REAL                                                                      | Float                                          |
| SQL_DOUBLE                                                                    | Float                                          |
| SQL_VARCHAR(size)                                                             | String \| Binary (configurable)                |
| SQL_WVARCHAR(size)                                                            | Unicode binary encoded as UTF16 little endian. |

_Table: Mapping of ODBC data types to the Erlang data types returned to the
Erlang application._

| ODBC Data Type         | Erlang Data Type                               |
| ---------------------- | ---------------------------------------------- |
| SQL_TYPE_DATE          | String                                         |
| SQL_TYPE_TIME          | String                                         |
| SQL_TYPE_TIMESTAMP     | \{\{YY, MM, DD\}, \{HH, MM, SS\}\}             |
| SQL_LONGVARCHAR        | String \| Binary (configurable)                |
| SQL_WLONGVARCHAR(size) | Unicode binary encoded as UTF16 little endian. |
| SQL_BINARY             | String \| Binary (configurable)                |
| SQL_VARBINARY          | String \| Binary (configurable)                |
| SQL_LONGVARBINARY      | String \| Binary (configurable)                |
| SQL_TINYINT            | Integer                                        |
| SQL_BIT                | Boolean                                        |

_Table: Mapping of extended ODBC data types to the Erlang data types returned to
the Erlang application._

> #### Note {: .info }
>
> To find out which data types will be returned for the columns in a table use
> the function [describe_table/\[2,3]](`m:odbc#describe_table`)

## Batch handling

Grouping of SQL queries can be desirable in order to reduce network traffic.
Another benefit can be that the data source sometimes can optimize execution of
a batch of SQL queries.

Explicit batches an procedures described below will result in multiple results
being returned from sql_query/\[2,3]. while with parameterized queries only one
result will be returned from param_query/\[2,3].

### Explicit batches

The most basic form of a batch is created by semicolons separated SQL queries,
for example:

```text
"SELECT * FROM FOO; SELECT * FROM BAR" or
"INSERT INTO FOO VALUES(1,'bar'); SELECT * FROM FOO"
```

### Procedures

Different databases may also support creating of procedures that contains more
than one SQL query. For example, the following SQLServer-specific statement
creates a procedure that returns a result set containing information about
employees that work at the department and a result set listing the customers of
that department.

```text
 CREATE PROCEDURE DepartmentInfo (@DepartmentID INT) AS
 SELECT * FROM Employee WHERE department = @DepartmentID
 SELECT * FROM Customers WHERE department = @DepartmentID
```

### Parameterized queries

To effectively perform a batch of similar queries, you can use parameterized
queries. This means that you in your SQL query string will mark the places that
usually would contain values with question marks and then provide lists of
values for each parameter. For instance you can use this to insert multiple rows
into the `EMPLOYEE` table while executing only a single SQL statement, for
example code see ["Using the Erlang API"](getting_started.md#param_query)
section in the "Getting Started" chapter.
