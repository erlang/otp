{application, db,
   [{description, "ERICSSON NR FOR DB"},
    {vsn, "1.0"},
    {modules, [{db1, "1.0"}, {db2, "1.0"}]},
    {registered, []},
    {applications, []},
    {env, []},
    {start, {db1, start, []}}]}.
