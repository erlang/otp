-module(contracts_with_subtypes2).

-compile(export_all).

-behaviour(supervisor).

-spec t(Arg) -> ok when
      Arg :: {a, A},
      A :: {b, B},
      B :: {c, C},
      C :: {d, D},
      D :: {e, E},
      E :: {f, _}.

t(X) ->
    get(X).

t() ->
    t({a, {b, {c, {d, {e, {g, 3}}}}}}). % breaks the contract

%% This one should possibly result in warnings about unused variables.
-spec l() -> ok when
      X :: Y,
      Y :: X.

l() ->
    ok.

%% This is the example from seq12547 (ticket OTP-11798).
%% There used to be a warning.

-spec init(term()) -> Result when
      Result :: {ok, {{supervisor:strategy(),
                       non_neg_integer(),
                       pos_integer()},
                      [supervisor:child_spec()]}}
              | ignore.

init(_) ->
    foo:bar().
