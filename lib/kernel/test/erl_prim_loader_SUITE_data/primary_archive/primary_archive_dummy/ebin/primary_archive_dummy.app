{application, code_archive_dummy,
 [{description, "primary_archive_dummy"},
  {vsn, "0.1"},
  {modules, [
	     primary_archive_dummy,
	     primary_archive_dummy_app,
	     primary_archive_dummy_sup
            ]},
  {registered, []},
  {applications, [kernel, stdlib, primary_archive_dict]},
  {mod, {primary_archive_dummy_app, [[]]}}]}.
