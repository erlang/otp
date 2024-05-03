### erlang:now/0

New time functionality and a new time API was introduced. For more information
see the [Time and Time Correction](`e:erts:time_correction.md`) chapter in the
ERTS User's guide and specifically the
[Dos and Donts](`e:erts:time_correction.md#Dos_and_Donts`) section on how to
replace usage of `erlang:now/0`.

### httpd_conf module

API functions in the module `httpd_conf` was deprecated in favor of standard
modules such as `lists`, `string`, `filelib`, and `erlang`.
