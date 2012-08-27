{application, a,
	     [{description, "A  CXC 138 11"},
              {vsn, "1.0"},
	      {modules, [a, a_sup]},
	      {registered, [a_sup]},
	      {applications, [kernel, stdlib]},
	      {mod, {a_sup, []}}]}.
