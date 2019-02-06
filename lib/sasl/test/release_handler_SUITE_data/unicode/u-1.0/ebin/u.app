{application, u,
	     [{description, "This app shall test unicode handling αβ"},
              {vsn, "1.0"},
	      {modules, [u, u_sup]},
	      {registered, [u_sup]},
	      {applications, [kernel, stdlib]},
	      {env, [{'key_αβ', 'val_αβ'}]},
	      {mod, {u_sup, []}}]}.
