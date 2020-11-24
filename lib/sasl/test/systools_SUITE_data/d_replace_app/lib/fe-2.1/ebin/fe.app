{application, fe,
   [{description, "ERICSSON NR FOR FE"},
    {vsn, "2.1"},
    {modules, [fe1, fe2, fe3]},
    {registered, []},
    {applications, []},
    {env, []},
    {start, {fe2, start, []}}]}.
