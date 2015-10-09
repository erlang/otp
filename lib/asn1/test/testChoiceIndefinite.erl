%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
%%
%%
-module(testChoiceIndefinite).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

main(per) -> ok;
main(ber) ->
    %% Test case related to OTP-4358
    %% normal encoding
    B = [48,8,160,3,128,1,11,129,1,12],
    %% indefinite length encoding
    Bi = [48,128,160,128,128,1,11,0,0,129,1,12,0,0],
    %% the value which is encoded
    V = {'Seq',{ca,11},12},
    {ok,V} = 'ChoiceIndef':decode('Seq', B),
    {ok,V} = 'ChoiceIndef':decode('Seq', Bi),
    ok.
