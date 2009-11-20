%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

