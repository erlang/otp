% -*-erlang-*-
{application, x,
 [{description, "Main application in reltool dependency test"},
  {vsn, "1.0"},
  {modules, [x1]},
  {registered, []},
  {applications, [kernel, stdlib, y]}]}.
