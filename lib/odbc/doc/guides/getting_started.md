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
# Getting started

## Setting things up

As the Erlang ODBC application is dependent on third party products there are a
few administrative things that needs to be done before you can get things up and
running.

- The first thing you need to do, is to make sure you have an ODBC driver
  installed for the database that you want to access. Both the client machine
  where you plan to run your erlang node and the server machine running the
  database needs the the ODBC driver. (In some cases the client and the server
  may be the same machine).
- Secondly you might need to set environment variables and paths to appropriate
  values. This may differ a lot between different os's, databases and ODBC
  drivers. This is a configuration problem related to the third party product
  and hence we cannot give you a standard solution in this guide.
- The Erlang ODBC application consists of both `Erlang` and `C` code. The `C`
  code is delivered as a precompiled executable for windows, solaris and linux
  (SLES10) in the commercial build. In the open source distribution it is built
  the same way as all other application using configure and make. You may want
  to provide the the path to your ODBC libraries using --with-odbc=PATH.

> #### Note {: .info }
>
> The Erlang ODBC application should run on all Unix dialects including Linux,
> Windows 2000, Windows XP and NT. But currently it is only tested for Solaris,
> Windows 2000, Windows XP and NT.

## Using the Erlang API

The following dialog within the Erlang shell illustrates the functionality of
the Erlang ODBC interface. The table used in the example does not have any
relevance to anything that exist in reality, it is just a simple example. The
example was created using `sqlserver 7.0 with servicepack 1` as database and the
ODBC driver for `sqlserver` with version `2000.80.194.00`.

```text
 1 > odbc:start().
      ok
```

Connect to the database

```erlang
 2 > {ok, Ref} = odbc:connect("DSN=sql-server;UID=aladdin;PWD=sesame", []).
      {ok,<0.342.0>}
```

Create a table

```text
 3 > odbc:sql_query(Ref, "CREATE TABLE EMPLOYEE (NR integer,
      FIRSTNAME  char varying(20), LASTNAME  char varying(20), GENDER char(1),
      PRIMARY KEY(NR))").
      {updated,undefined}
```

Insert some data

```text
 4 > odbc:sql_query(Ref, "INSERT INTO EMPLOYEE VALUES(1, 'Jane', 'Doe', 'F')").
      {updated,1}
```

Check what data types the database assigned for the columns. Hopefully this is
not a surprise, some times it can be\! These are the data types that you should
use if you want to do a parameterized query.

```erlang
 5 > odbc:describe_table(Ref, "EMPLOYEE").
      {ok, [{"NR", sql_integer},
            {"FIRSTNAME", {sql_varchar, 20}},
            {"LASTNAME", {sql_varchar, 20}}
            {"GENDER", {sql_char, 1}}]}
```

[](){: #param_query } Use a parameterized query to insert many rows in one go.

```erlang
 6 > odbc:param_query(Ref,"INSERT INTO EMPLOYEE (NR, FIRSTNAME, "
                  "LASTNAME, GENDER) VALUES(?, ?, ?, ?)",
                   [{sql_integer,[2,3,4,5,6,7,8]},
                    {{sql_varchar, 20},
                             ["John", "Monica", "Ross", "Rachel",
                             "Piper", "Prue", "Louise"]},
                   {{sql_varchar, 20},
                             ["Doe","Geller","Geller", "Green",
                              "Halliwell", "Halliwell", "Lane"]},
                   {{sql_char, 1}, ["M","F","M","F","F","F","F"]}]).
      {updated, 7}
```

Fetch all data in the table employee

```erlang
 7> odbc:sql_query(Ref, "SELECT * FROM EMPLOYEE").
    {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],
          [{1,"Jane","Doe","F"},
           {2,"John","Doe","M"},
           {3,"Monica","Geller","F"},
           {4,"Ross","Geller","M"},
           {5,"Rachel","Green","F"},
           {6,"Piper","Halliwell","F"},
           {7,"Prue","Halliwell","F"},
           {8,"Louise","Lane","F"}]]}
```

Associate a result set containing the whole table `EMPLOYEE` to the connection.
The number of rows in the result set is returned.

```erlang
 8 > odbc:select_count(Ref, "SELECT * FROM EMPLOYEE").
      {ok,8}
```

You can always traverse the result set sequential by using next

```erlang
 9 > odbc:next(Ref).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{1,"Jane","Doe","F"}]}
```

```erlang
 10 > odbc:next(Ref).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{2,"John","Doe","M"}]}
```

If your driver supports scrollable cursors you have a little more freedom, and
can do things like this.

```erlang
 11 > odbc:last(Ref).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{8,"Louise","Lane","F"}]}
```

```erlang
 12 > odbc:prev(Ref).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{7,"Prue","Halliwell","F"}]}
```

```erlang
 13 > odbc:first(Ref).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{1,"Jane","Doe","F"}]}
```

```erlang
 14 > odbc:next(Ref).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{2,"John","Doe","M"}]}
```

Fetch the fields `FIRSTNAME `and `NR `for all female employees

```erlang
 15 > odbc:sql_query(Ref, "SELECT FIRSTNAME, NR FROM EMPLOYEE WHERE GENDER = 'F'").
     {selected,["FIRSTNAME","NR"],
          [{"Jane",1},
           {"Monica",3},
           {"Rachel",5},
           {"Piper",6},
           {"Prue",7},
           {"Louise",8}]}
```

Fetch the fields `FIRSTNAME `and `NR `for all female employees and sort them on
the field `FIRSTNAME `.

```erlang
 16 > odbc:sql_query(Ref, "SELECT FIRSTNAME, NR FROM EMPLOYEE WHERE GENDER = 'F'
      ORDER BY FIRSTNAME").
    {selected,["FIRSTNAME","NR"],
          [{"Jane",1},
           {"Louise",8},
           {"Monica",3},
           {"Piper",6},
           {"Prue",7},
           {"Rachel",5}]}
```

Associate a result set that contains the fields `FIRSTNAME` and `NR `for all
female employees to the connection. The number of rows in the result set is
returned.

```erlang
 17 > odbc:select_count(Ref, "SELECT FIRSTNAME, NR FROM EMPLOYEE WHERE GENDER = 'F'").
      {ok,6}
```

A few more ways of retrieving parts of the result set when the driver supports
scrollable cursors. Note that next will work even without support for scrollable
cursors.

```erlang
 18 > odbc:select(Ref, {relative, 2}, 3).
    {selected,["FIRSTNAME","NR"],[{"Monica",3},{"Rachel",5},{"Piper",6}]}
```

```erlang
 19 > odbc:select(Ref, next, 2).
      {selected,["FIRSTNAME","NR"],[{"Prue",7},{"Louise",8}]}
```

```erlang
 20 > odbc:select(Ref, {absolute, 1}, 2).
      {selected,["FIRSTNAME","NR"],[{"Jane",1},{"Monica",3}]}
```

```erlang
 21 > odbc:select(Ref, next, 2).
    {selected,["FIRSTNAME","NR"],[{"Rachel",5},{"Piper",6}]}
```

```erlang
 22 > odbc:select(Ref, {absolute, 1}, 4).
      {selected,["FIRSTNAME","NR"],
                [{"Jane",1},{"Monica",3},{"Rachel",5},{"Piper",6}]}
```

Select, using a parameterized query.

```erlang
 23 > odbc:param_query(Ref, "SELECT * FROM EMPLOYEE WHERE GENDER=?",
      [{{sql_char, 1}, ["M"]}]).
      {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],
                [{2,"John", "Doe", "M"},{4,"Ross","Geller","M"}]}
```

Delete the table `EMPLOYEE`.

```text
 24 > odbc:sql_query(Ref, "DROP TABLE EMPLOYEE").
      {updated,undefined}
```

Shut down the connection.

```erlang
 25 > odbc:disconnect(Ref).
      ok
```

Shut down the application.

```text
 26 > odbc:stop().
    =INFO REPORT==== 7-Jan-2004::17:00:59 ===
    application: odbc
    exited: stopped
    type: temporary

    ok
```
