%% -*- erlang -*-
{application, b,
 [{description, "B  CXC 138 12"},
  {vsn, "1.0"},
  {modules, [{b_server, 1},{b_lib, 1}]},
  {registered, [b_server]},
  {applications, [kernel, stdlib]}]}.
