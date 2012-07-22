{application, app_faulty,
 [{description, "very simple example faulty application"},
  {id, "app_faulty"},
  {vsn, "1.0"},
  {modules, [app_faulty, app_faulty_sup, app_faulty_server]},
  {registered, [app_faulty]},
  {applications, [kernel, stdlib]},
  {env, [{var,val1}]},
  {mod, {app_faulty, []}}
  ]}.
