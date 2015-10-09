% coding: latin-1

-module(tlatin).

-include_lib("eunit/include/eunit.hrl").

'foo_ä_test_'() ->
	[
	 {"1ö1", fun() -> io:format("1å1 ~s ~w",[<<"aö">>, 'Zök']), io:format([128,64,255,255]), ?assert("gö"=="gö") end}
	 ,{<<"2ö2">>, fun() -> io:format("2å2 ~s",[<<"bö">>]), io:format([128,64]), ?assert("gö"=="gö") end}
	 ,{<<"3ö3"/utf8>>, fun() -> io:format("3å3 ~ts",[<<"cö"/utf8>>]), io:format([128,64]), ?assert("gö"=="gö") end}
	 ,{"1ä1", fun() -> io:format("1ä1 ~s ~w",[<<"aä">>,'Zbäd']), io:format([128,64,255,255]), ?assert("wå"=="wä") end}
	 ,{<<"2ä2">>, fun() -> io:format("2ä2 ~s",[<<"bä">>]), io:format([128,64]), ?assert("wå"=="wä") end}
	 ,{<<"3ä3"/utf8>>, fun() -> io:format("3ä3 ~ts",[<<"cä"/utf8>>]), io:format([128,64]), ?assert("wå"=="wä") end}
	].
