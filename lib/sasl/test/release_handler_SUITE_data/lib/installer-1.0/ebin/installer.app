{application, installer,
	     [{description, "Installer application"},
              {vsn, "1.0"},
	      {modules, [{installer, 1}]},
	      {registered, []},
	      {applications, [kernel, stdlib, sasl]}]}.
