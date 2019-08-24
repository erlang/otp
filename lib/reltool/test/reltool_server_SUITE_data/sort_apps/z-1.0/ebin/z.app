% -*-erlang-*-
{application, z,
 [{description, "Application in reltool sort app test - included applications"},
  {vsn, "1.0"},
  {modules,[]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, inets]},
  {included_applications, [tools, mnesia]}]}.
