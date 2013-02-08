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
    IF = 'InformationFramework',
    {ok,Enc} = asn1_wrapper:encode(IF, 'Attribute', Data),
    {ok,Data} = IF:decode('Attribute', Enc),
    ok.
