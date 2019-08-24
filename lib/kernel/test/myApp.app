    {application, myApp,
     [{description, "Test of start phase"},
      {id, "CXC 138 38"},
      {applications, [kernel]},
      {included_applications, []},
      {start_phases, [{init, [initArgs]}, {go, [goArgs]}]},
      {mod, {myApp, {myApp, 1, 3}} }]}. 
