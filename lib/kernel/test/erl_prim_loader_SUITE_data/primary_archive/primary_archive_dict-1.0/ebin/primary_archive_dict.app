{application, primary_archive_dict,
 [{description, "primary_archive_dict"},
  {vsn, "1.0"},
  {modules, [
	     primary_archive_dict,
	     primary_archive_dict_sup
            ]},
  {registered, [
		primary_archive_dict_sup
	       ]},
  {applications, [kernel, stdlib]},
  {mod, {primary_archive_dict_app, [[]]}}]}.
