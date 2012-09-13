{application, db,
   [{description, "ERICSSON NR FOR DB"},
    {vsn, "2.1"},
    {modules, [db1, db2]},
    {registered, []},
    {applications, []},
    {env, []},
    {start, {db1, start, []}}]}.
