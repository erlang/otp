{application, app2,
 [{description, "very simple example application"},
  {id, "app2"},
  {vsn, "1.0"},
  {modules, [app2, app2_sup, app2_server]},
  {registered, [ginny]},
  {applications, [kernel, stdlib, sasl]},
  {env, []},
  {mod, {app2, []}}]}.
