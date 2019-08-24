{application, appinc2B,
     [{description, "Test of new start"},
      {id, "CXC 138 xx2"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {start_phases, [{init, [initArgs2B]}]},
      {mod, {appinc2B, [arg1, arg2] }}]}. 
