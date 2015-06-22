%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(otp_5244).
-export([?MODULE/0]).

?MODULE() ->
    L = [{stretch,0,0},
	 {bad,[]},
	 {bad,atom},
	 {bad,0},
	 {bad,16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA},
	 {bad,16#555555555555555555555555555555555555555555555555555}],
    remove_failure(L, unit, 0).

remove_failure([], _Unit, _MaxFailure) ->
    ok;
remove_failure([{bad,Bad}|_], _Unit, _MaxFailure) ->
    Bad;
remove_failure([{stretch,_,Mi}=Stretch | Specs], Unit, _MaxFailure) ->
    {MinMax,NewMaxFailure} = max_failure(),
    case {MinMax,remove_failure(Specs, Unit, NewMaxFailure)} of
	{min,{NewMaxFailure,Rest}} ->
	    {done,[{fixed,Mi} | Rest]};
	{min,_} when Specs =/= [] ->
	    remove_failure([Stretch|tl(Specs)], Unit, NewMaxFailure);
	{min,_} ->
	    ok
    end.

max_failure() ->
    {min,1}.
