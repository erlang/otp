{application, c,
	     [{description, "C  CXC 138 11"},
              {vsn, "1.0"},
	      {modules, [b, aa, c_sup]},
	      {registered, [cc,bb,c_sup]},
	      {applications, [kernel, stdlib]},
	      {env, [{key1, val1}]},
	      {mod, {c_sup, []}}]}.
