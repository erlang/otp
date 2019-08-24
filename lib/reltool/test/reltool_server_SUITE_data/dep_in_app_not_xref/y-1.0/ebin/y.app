% -*-erlang-*-
{application, y,
 [{description, "Library application in reltool dependency test"},
  {vsn, "1.0"},
  {modules, [y1]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {included_applications, [z]}]}.
