-module(snmpm_error_report).

-callback config_err(Format, Args) -> snmp:void()
    when Format :: string(), Args :: [term()].

-callback user_err(Format, Args) -> snmp:void()
    when Format :: string(), Args :: [term()].
