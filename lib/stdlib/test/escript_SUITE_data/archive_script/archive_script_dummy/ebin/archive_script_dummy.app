{application, archive_script_dummy,
 [{description, "archive_script_dummy"},
  {vsn, "0.1"},
  {modules, [
	     archive_script_main,
	     archive_script_main2
            ]},
  {registered, []},
  {applications, [kernel, stdlib, archive_script_dict]},
  {mod, {archive_script_dummy_app, [[]]}}]}.
