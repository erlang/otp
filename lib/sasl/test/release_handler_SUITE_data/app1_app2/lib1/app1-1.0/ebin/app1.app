{application, app1,
 [{description, "very simple example application"},
  {id, "app1"},
  {vsn, "1.0"},
  {modules, [app1, app1_sup, app1_server]},
  {registered, [harry]},
  {applications, [kernel, stdlib, sasl]},
  {env, [{var,val1}]},
  {mod, {app1, []}}]}.
