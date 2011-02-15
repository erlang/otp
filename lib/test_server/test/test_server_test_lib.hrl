-record(tc, {
	  name,
	  result,
	  elapsed,
	  logfile
	 }).

-record(suite, {
	  application,
	  n_cases = 0,
	  n_cases_failed = 0,
	  n_cases_expected = 0,
	  n_cases_succ,
	  n_cases_user_skip,
	  n_cases_auto_skip,
	  cases = [],
	  host,
	  emulator_vsn,
	  emulator,
	  otp_release,
	  started,
	  log_ok = false
	 }).
