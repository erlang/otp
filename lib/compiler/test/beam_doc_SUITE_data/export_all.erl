-module(export_all).

-compile(export_all).


-doc #{equiv => ok/0}.
-doc "
This is a test
".
all_ok() ->
    all_ok().

-doc #{equiv => main()}.
-spec main() -> ok.
-doc "
all_ok()

Calls all_ok/0
".
main() ->
    all_ok().

-doc #{equiv => main()}.
-doc "
main2()

Second main
".
-spec main2() -> ok.
main2() ->
    ok.
