% coding: latin-1

-module(tlatin).

-include_lib("eunit/include/eunit.hrl").

'foo_�_test_'() ->
	[
	 {"1�1", fun() -> io:format("1�1 ~s ~w",[<<"a�">>, 'Z�k']), io:format([128,64,255,255]), ?assert("g�"=="g�") end}
	 ,{<<"2�2">>, fun() -> io:format("2�2 ~s",[<<"b�">>]), io:format([128,64]), ?assert("g�"=="g�") end}
	 ,{<<"3�3"/utf8>>, fun() -> io:format("3�3 ~ts",[<<"c�"/utf8>>]), io:format([128,64]), ?assert("g�"=="g�") end}
	 ,{"1�1", fun() -> io:format("1�1 ~s ~w",[<<"a�">>,'Zb�d']), io:format([128,64,255,255]), ?assert("w�"=="w�") end}
	 ,{<<"2�2">>, fun() -> io:format("2�2 ~s",[<<"b�">>]), io:format([128,64]), ?assert("w�"=="w�") end}
	 ,{<<"3�3"/utf8>>, fun() -> io:format("3�3 ~ts",[<<"c�"/utf8>>]), io:format([128,64]), ?assert("w�"=="w�") end}
	].
