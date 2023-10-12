{application, db,
   [{description, "ERICSSON NR FOR DB"},
    {vsn, "2.1"},
    {modules, [db1, db2, db3]},
    {registered, []},
    {applications, []},
    {env, []},
    {start, {db1, start, []}}]}.
