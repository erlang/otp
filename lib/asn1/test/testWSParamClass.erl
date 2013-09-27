-module(testWSParamClass).
-export([main/1]).

main(_) ->
    IF = 'InformationFramework',
    roundtrip({'Attribute',IF:'id-at-objectClass'(),
	       [IF:'id-at-objectClass'()],
	       asn1_NOVALUE}),
    roundtrip({'Attribute',IF:'id-at-objectClass'(),
	       [],[]}),
    ok.

roundtrip(Data) ->
    asn1_test_lib:roundtrip('InformationFramework', 'Attribute', Data).
