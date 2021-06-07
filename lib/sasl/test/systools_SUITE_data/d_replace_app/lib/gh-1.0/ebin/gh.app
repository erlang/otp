{application, gh,
   [{description, "ERICSSON NR FOR GH"},
    {vsn, "1.0"},
    {modules, [gh1]},
    {registered, []},
    {applications, []},
    {env, []},
    {start, {gh1, start, []}}]}.
