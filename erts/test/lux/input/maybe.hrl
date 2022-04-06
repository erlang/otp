-if(?enable_maybe > 0).
-feature(while_expr, enable).
-record(conditional, {on=off, 'until'=none}).
-else.
-record(conditional, {on=on, until=until}).
-endif.

%% -record(foo, {bar=outer, baz}).

%% -record(conditional, {on, else}).
