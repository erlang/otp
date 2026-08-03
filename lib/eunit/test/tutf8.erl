%% coding: utf-8
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2026. All Rights Reserved.
%% Copyright Richard Carlsson 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-module(tutf8).

-compile(nowarn_latin1_binary).
-include_lib("eunit/include/eunit.hrl").

'foo_ö_test_'() ->
	[
         {"1ö汉1", fun() -> io:format("1å汉1 ~s ~w",[<<"aö">>, 'Zök']), io:format([128,64,255,255]), ?assert("gö汉"=="gö汉") end}
         ,{<<"2ö2">>, fun() -> io:format("2å汉2 ~s",[<<"bö">>]), io:format([128,64]), ?assert("gö汉"=="gö汉") end}
	 ,{<<"3ö汉3"/utf8>>, fun() -> io:format("3å汉3 ~ts",[<<"cö汉"/utf8>>]), io:format([128,64]), ?assert("gö汉"=="gö汉") end}
         ,{"1ä汉1", fun() -> io:format("1ä汉1 ~s ~w",[<<"aä">>, 'Zbäd']), io:format([128,64,255,255]), ?assert("wå汉"=="wä汉") end}
         ,{<<"2ä2">>, fun() -> io:format("2ä汉2 ~s",[<<"bä">>]), io:format([128,64]), ?assert("wå汉"=="wä汉") end}
	 ,{<<"3ä汉"/utf8>>, fun() -> io:format("3ä汉3 ~ts",[<<"cä汉"/utf8>>]), io:format([128,64]), ?assert("wå汉"=="wä汉") end}
	].
