%%% This is a behaviour with info about its calllbacks.

-module(correct_behaviour).

-callback foo() -> yes | no.
-callback bar({atom(),_},[_]) -> term().
