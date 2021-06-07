% -*-erlang-*-
{application, z,
 [{description, "Application in reltool sort app test - included applications"},
  {vsn, "1.0"},
  {modules,[]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, inets, unknown]},
  {optional_applications, [unknown]},
  {included_applications, [tools, mnesia]}]}.
