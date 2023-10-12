{application, db,
   [{description, "ERICSSON NR FOR DB"},
    {vsn, "1.0"},
    {modules, [db1, db2]},
    {registered, []},
    {applications, [fe]},
    {env, []},
    {start, {db1, start, []}}]}.
