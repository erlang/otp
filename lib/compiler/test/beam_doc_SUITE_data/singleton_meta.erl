-module(singleton_meta).

-moduledoc(#{ since => ~"1.0" }).

-export([main/0, main1/0]).

-doc #{ authors => [<<"Beep Bop">>] }.
-doc #{ equiv => main/3 }.
main() ->
    main1().

-doc (#{ equiv => main(_),
         since => ~"1.1" }).
-doc "
main1()

Returns always ok.
".
main1() ->
    ok.
