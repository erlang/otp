{application, archive_script_dict,
 [{description, "archive_script_dict"},
  {vsn, "0.1"},
  {modules, [
	     archive_script_dict,
	     archive_script_dict_sup
            ]},
  {registered, [
		archive_script_dict_sup
	       ]},
  {applications, [kernel, stdlib]},
  {mod, {archive_script_dict_app, [[]]}}]}.
