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
-export([init/1, terminate/2, start/0]).
-export([produce/1,init/4]).


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


init(_Random,S1,S2,S3)  ->
    case catch random:seed(S1,S2,S3) of
	{'EXIT',_} ->
	    true;
	_ ->
            {noreply,[]}
    end.


%% This starts up the random number server
start() ->
    %% Start the gen server
    {ok,Pid} = rmod_random:oe_create([],{local,'rmod_random_impl'}),
    true.









