%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

-define(NS,ns). % netconf server module
-define(LOCALHOST, "127.0.0.1").
-define(SSH_PORT, 2060).

-define(DEFAULT_SSH_OPTS,[{ssh,?LOCALHOST},
			  {port,?SSH_PORT},
			  {user,"xxx"},
			  {password,"xxx"}]).
-define(DEFAULT_SSH_OPTS(Dir), ?DEFAULT_SSH_OPTS++[{user_dir,Dir}]).

-define(ok,ok).
