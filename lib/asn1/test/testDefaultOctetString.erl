-module(testDefaultOctetString).

-export([dos/1]).

-include_lib("common_test/include/ct.hrl").

-record('Dos', {
  opt = asn1_NOVALUE,
  def = asn1_DEFAULT
}).

-define(def_DEFAULT, <<5>>).

dos(Rules) ->
    %% test roundtrip default
    E1 = roundtrip(#'Dos'{}, #'Dos'{def = ?def_DEFAULT}),
    %% test the value dos defined in the .asn file
    E2 = roundtrip('DefaultOctetString':dos()),
    %% sanity test a fully specified SEQUENCE
    E3 = roundtrip(#'Dos'{opt = <<1,2,3>>, def = <<6>>}),
    %% test def is/isn't encoded according to the value
    if Rules == ber ->
            <<48, 0>> = E1,
            <<48, 4, 16#82, 2, 16#12, 16#34>> = E2,
            <<48, 8, 16#82, 3, 1, 2, 3, 16#8A, 1, 6>> = E3;
       true ->
            ignore
    end,
    ok.

roundtrip(Value) ->
    roundtrip(Value, Value).
roundtrip(Value, Exp) ->
    asn1_test_lib:roundtrip('DefaultOctetString', 'Dos', Value, Exp).
