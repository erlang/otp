{application, appinc2,
     [{description, "Test of new start, no inc file"},
      {id, "CXC 138 xx2"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {start_phases, [{init, [initArgs2]}, {go, [goArgs2]}]},
      {mod, {appinc2, [ch_sup, start, {app1, 55, 57}] }}]}. 
