%% coding: utf-8

-module(tutf8).

-include_lib("eunit/include/eunit.hrl").

'foo_ö_test_'() ->
	[
	 {"1ö汉1", fun() -> io:format("1å汉1 ~s ~w",[<<"aö汉">>, 'Zök']), io:format([128,64,255,255]), ?assert("gö汉"=="gö汉") end}
	 ,{<<"2ö汉2">>, fun() -> io:format("2å汉2 ~s",[<<"bö汉">>]), io:format([128,64]), ?assert("gö汉"=="gö汉") end}
	 ,{<<"3ö汉3"/utf8>>, fun() -> io:format("3å汉3 ~ts",[<<"cö汉"/utf8>>]), io:format([128,64]), ?assert("gö汉"=="gö汉") end}
	 ,{"1ä汉1", fun() -> io:format("1ä汉1 ~s ~w",[<<"aä汉">>, 'Zbäd']), io:format([128,64,255,255]), ?assert("wå汉"=="wä汉") end}
	 ,{<<"2ä汉2">>, fun() -> io:format("2ä汉2 ~s",[<<"bä汉">>]), io:format([128,64]), ?assert("wå汉"=="wä汉") end}
	 ,{<<"3ä汉"/utf8>>, fun() -> io:format("3ä汉3 ~ts",[<<"cä汉"/utf8>>]), io:format([128,64]), ?assert("wå汉"=="wä汉") end}
	].
