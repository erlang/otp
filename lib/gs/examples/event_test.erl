%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% Demo for testing some events

-module(event_test).
-compile([{nowarn_deprecated_function,{gs,button,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,2}}]).

-export([start/0,init/0]).



start() ->
    spawn(event_test,init,[]).

init() ->
    S=gs:start(),
    W=gs:window(S,[{map,true},{keypress,true},{buttonpress,true}]),
    gs:button(W,[{label,{text,"Press Me"}},{enter,true},{leave,true}]),
    event_loop().


event_loop() ->
    receive
	X ->
	    io:format("Got event: ~p~n",[X]),
	    event_loop()
    end.


		     
    
