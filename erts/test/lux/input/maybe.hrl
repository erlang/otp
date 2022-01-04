-if(?enable_maybe > 0).
-feature(enable, while_expr).
-record(conditional, {on=off, 'until'=none}).
-else.
-record(conditional, {on=on, until=until}).
-endif.

%% -record(foo, {bar=outer, baz}).

%% -record(conditional, {on, else}).
