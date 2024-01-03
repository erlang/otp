-module(singleton_meta).

-export([main/0, main1/0]).

-doc #{ authors => [<<"Beep Bop">>] }.
-doc #{ equiv => main/3 }.
main() ->
    main1().

-doc (#{ equiv => main(_) }).
-doc "
main1()

Returns always ok.
".
main1() ->
    ok.
