{application, code_archive_dict,
 [{description, "code_archive_dict"},
  {vsn, "1.0"},
  {modules, [
	     code_archive_dict,
	     code_archive_dict_sup
            ]},
  {registered, [
		code_archive_dict_sup
	       ]},
  {applications, [kernel, stdlib]},
  {mod, {code_archive_dict_app, [[]]}}]}.
