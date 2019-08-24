{application, appinc1,
     [{description, "Test of new start, no inc file"},
      {id, "CXC 138 xx1"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {start_phases, [{go, [goArgs1]}]},
      {mod, {appinc1, [ch_sup, start, {app1, 55, 57}] }}]}. 
