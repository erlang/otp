%% coding:utf-8
-module(un_atom2).

-export(['\x{aaa}memory'/0, 'func-\x{400}'/1, func/1, äfunc/1]).

-record('rec-\x{400}', {'field-\x{400}'}).

-type cs() :: $\x{a}
            | $\x{aa}
            | $\x{aaa}
            | $\xaa.

-callback 'callback-\x{400}'() -> 'apa'.

-type 'type-\x{400}'() :: 'atom-\x{400}'
                        | cs()
                        | #'rec-\x{400}'{'field-\x{400}' :: 'type-\x{400}'()}.

-spec '\x{aaa}memory'() -> 'type-\x{400}'().

'\x{aaa}memory'() ->
    apa:foo().

%% @deprecated Please use {@link m:f/1}.
-spec 'func-\x{400}'(#'rec-\x{400}'{}) -> #'rec-\x{400}'{}.

'func-\x{400}'(_T) ->
    foo:bar(#'rec-\x{400}'{}).

-record(rec, {}).

-spec func(#rec{}) -> #rec{}.

func(#rec{}) -> #rec{}.

-record(ärec, {}).

-spec äfunc(#ärec{}) -> #ärec{}.

äfunc(#ärec{}) -> #ärec{}.
