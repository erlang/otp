% -*-erlang-*-
{application, a,
 [{description, "Application with duplicated module name in .app file"},
  {vsn, "1.0"},
  {modules, [a,a]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
