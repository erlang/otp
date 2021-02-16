-module(cover_messages).

foo() ->
    ok.

-file("cover_messages.erl", 99999).

bar() ->
    ok.
