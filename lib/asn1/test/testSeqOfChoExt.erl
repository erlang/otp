-module(testSeqOfChoExt).

-export([main/1]).

%-record('Seq2', { octstr = asn1_NOVALUE }).
%-record('Seq1', { int, soc }).

main(_Rules) ->
    roundtrip('SeqOfChoExt':seq1()).

roundtrip(Value) ->
    roundtrip(Value, Value).
roundtrip(Value, Exp) ->
    Type = element(1,Value),
    asn1_test_lib:roundtrip('SeqOfChoExt', Type, Value, Exp).
