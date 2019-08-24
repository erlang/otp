{application, appinc1x,
     [{description, "Test of new start"},
      {id, "CXC 138 xx1"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {start_phases, [{spec, [specArgs1]}, {go, [goArgs1]}]},
      {mod, {appinc1x, [arg1, arg2, arg3] }}]}. 
