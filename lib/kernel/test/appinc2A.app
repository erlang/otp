{application, appinc2A,
     [{description, "Test of new start"},
      {id, "CXC 138 xx2"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {start_phases, [{some, [someArgs2A]}, {go, [goArgs2A]}]},
      {mod, {appinc2A, [arg1, arg2] }}]}. 
