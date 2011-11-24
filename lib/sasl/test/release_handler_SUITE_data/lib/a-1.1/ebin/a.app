{application, a,
	     [{description, "A  CXC 138 11"},
              {vsn, "1.1"},
	      {modules, [{a, 2}, {a_sup,1}]},
	      {registered, [a_sup]},
	      {applications, [kernel, stdlib]},
	      {env, [{key1, val1}]},
	      {mod, {a_sup, []}}]}.
