{application, installer,
	     [{description, "Installer application"},
              {vsn, "1.0"},
	      {modules, [installer,rh_test_lib,otp_vsns]},
	      {registered, []},
	      {applications, [kernel, stdlib, sasl]}]}.
