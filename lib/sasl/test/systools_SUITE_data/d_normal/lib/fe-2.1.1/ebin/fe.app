{application, fe,
   [{description, "ERICSSON NR FOR FE"},
    {vsn, "2.1.1"},
    {modules, [{fe1, "1.0"}, {fe2, "1.0"}, {fe3, "2.0"}]},
    {registered, []},
    {applications, []},
    {env, []},
    {start, {fe2, start, []}}]}.
