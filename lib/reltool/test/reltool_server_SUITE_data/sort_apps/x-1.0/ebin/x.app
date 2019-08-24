% -*-erlang-*-
{application, x,
 [{description, "Application in reltool sort app test - circular dependency"},
  {vsn, "1.0"},
  {modules,[]},
  {registered, []},
  {applications, [kernel, stdlib, y]}]}.
