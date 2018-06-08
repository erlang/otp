%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

-module(ct_rpc).

%%% API
-export([app_node/2, app_node/3, app_node/4,
	 call/4, call/5, call/6, cast/4, cast/5]).

%%%=========================================================================
%%%  API
%%%=========================================================================
app_node(App, Candidates) ->
    app_node(App, Candidates, true, []).

app_node(App, Candidates, FailOnBadRPC) ->
    app_node(App, Candidates, FailOnBadRPC, []).

app_node(App, [], _, _) -> 
    ct:fail({application_not_running, App});

%% Variable _Candidates is a workaround for the strange edoc behavior
%% of creating the spec:  app_node(App, Nodes::Candidates) -> NodeName
%% if it does not exist.
app_node(App, _Candidates = [CandidateNode | Nodes], FailOnBadRPC, Cookie) -> 
    Cookie0 = set_the_cookie(Cookie),
    Result = rpc:call(CandidateNode, application, which_applications, []),
    _ = set_the_cookie(Cookie0),
    case Result of
	{badrpc,Reason} when FailOnBadRPC == true ->
	    ct:fail({Reason,CandidateNode});
	{badrpc,_} when FailOnBadRPC == false ->
	    app_node(App, Nodes, FailOnBadRPC);
	Apps ->    
	    case lists:keysearch(App, 1, Apps) of
		{value, _} ->
		    CandidateNode;
		_ ->
		    app_node(App, Nodes, FailOnBadRPC)
	    end
    end.

call(Node, Module, Function, Args) ->
    call(Node, Module, Function, Args, infinity, []). 

call(Node, Module, Function, Args, TimeOut) ->
    call(Node, Module, Function, Args, TimeOut, []).

call({Fun, FunArgs}, Module, Function, Args, TimeOut, Cookie) ->
    Node = Fun(FunArgs),
    call(Node, Module, Function, Args, TimeOut, Cookie);
call(Node, Module, Function, Args, TimeOut, Cookie) when is_atom(Node) ->
    Cookie0 = set_the_cookie(Cookie),
    Result = rpc:call(Node, Module, Function, Args, TimeOut),
    _ = set_the_cookie(Cookie0),
    Result.    

cast(Node, Module, Function, Args) ->
    cast(Node, Module, Function, Args, []).

cast({Fun, FunArgs}, Module, Function, Args, Cookie) ->
    Node = Fun(FunArgs),
    cast(Node, Module, Function, Args, Cookie);
cast(Node, Module, Function, Args, Cookie) when is_atom(Node) ->
    Cookie0 = set_the_cookie(Cookie),
    true = rpc:cast(Node, Module, Function, Args),
    _ = set_the_cookie(Cookie0),
    ok.


%%%---------- Internal -----------

set_the_cookie([]) ->
    [];
set_the_cookie(Cookie) ->
    Cookie0 = erlang:get_cookie(),
    erlang:set_cookie(node(),Cookie),
    Cookie0.
