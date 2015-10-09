%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module('rmod_random_impl').
-include("oe_random.hrl").
-export([init/1, terminate/2]).
-export([produce/1,init/2]).


init(Env) ->
    {ok, []}.

terminate(From, Reason) ->
    ok.


produce(_Random)  ->
    case catch random:uniform() of
	{'EXIT',_} ->
	    true;
	RUnif ->
            {reply,RUnif,[]}
     end.


init(_Random,IData)  ->
    S1 = IData#seed.seed1,
    S2 = IData#seed.seed2,
    S3 = IData#seed.seed3,
    case catch random:seed(S1,S2,S3) of
	{'EXIT',_} ->
	    true;
	_ ->
            {noreply,[]}
    end.

