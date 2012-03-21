% -*-erlang-*-
{application, y,
 [{description, "Library application in reltool dependency test"},
  {vsn, "1.0"},
  {modules, [y1,y2]}, % y3 is skipped on purpose - to test module inclusion policy
  {registered, []},
  {applications, [kernel, stdlib]}]}.
