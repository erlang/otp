-module(hide_moduledoc2).

-export([main/0, handle_call/1]).

-moduledoc hidden.

-doc hidden.
main() ->
    ok().

-doc #{since => "1.0"}.
-doc hidden.
ok() ->
    ok.

-doc false.
-spec handle_call('which' | {'add',atom()} | {'delete',atom()}) ->
        {'reply', 'ok' | [atom()]}.

handle_call({add,_Address}=A) -> A;
handle_call({delete,_Address}=D) -> D;
handle_call(which) -> which.
