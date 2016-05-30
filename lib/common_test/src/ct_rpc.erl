%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%% @doc Common Test specific layer on Erlang/OTP rpc. 

-module(ct_rpc).

%%% API
-export([app_node/2, app_node/3, app_node/4,
	 call/4, call/5, call/6, cast/4, cast/5]).

%%%=========================================================================
%%%  API
%%%=========================================================================
%%% @spec app_node(App, Candidates) -> NodeName
%%%
%%%      App = atom() 
%%%      Candidates = [NodeName]
%%%      NodeName = atom() 
%%%
%%% @doc From a set of candidate nodes determines which of them is
%%%      running the application App. If none of the candidate nodes
%%%      is running the application the function will make the test case
%%%      calling this function fail. This function is the same as calling
%%%      <code>app_node(App, Candidates, true)</code>.
%%%
app_node(App, Candidates) ->
    app_node(App, Candidates, true, []).

%%% @spec app_node(App, Candidates, FailOnBadRPC) -> NodeName
%%%
%%%      App = atom() 
%%%      Candidates = [NodeName]
%%%      NodeName = atom() 
%%%      FailOnBadRPC = true | false
%%%
%%% @doc Same as <code>app_node/2</code> only the <code>FailOnBadRPC</code>
%%%      argument will determine if the search for a candidate node should
%%%      stop or not if <code>badrpc</code> is received at some point. 
%%%
app_node(App, Candidates, FailOnBadRPC) ->
    app_node(App, Candidates, FailOnBadRPC, []).

%%% @spec app_node(App, Candidates, FailOnBadRPC, Cookie) -> NodeName
%%%
%%%      App = atom() 
%%%      Candidates = [NodeName]
%%%      NodeName = atom() 
%%%      FailOnBadRPC = true | false
%%%      Cookie = atom()
%%%
%%% @doc Same as <code>app_node/2</code> only the <code>FailOnBadRPC</code>
%%%      argument will determine if the search for a candidate node should
%%%      stop or not if <code>badrpc</code> is received at some point. 
%%%      The cookie on the client node will be set to <code>Cookie</code>
%%%      for this rpc operation (use to match the server node cookie).
%%%
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

%%% @spec call(Node, Module, Function, Args) -> term() | {badrpc, Reason}
%%% 
%%% @doc Same as call(Node, Module, Function, Args, infinity)
%%%
call(Node, Module, Function, Args) ->
    call(Node, Module, Function, Args, infinity, []). 

%%% @spec call(Node, Module, Function, Args, TimeOut) -> 
%%%                                              term() | {badrpc, Reason}
%%%      Node = NodeName | {Fun, FunArgs} 
%%%      Fun = fun()
%%%      FunArgs =  term()
%%%      NodeName = atom() 
%%%      Module = atom() 
%%%      Function = atom()  
%%%      Args = [term()]
%%%      Reason = timeout | term() 
%%%
%%% @doc Evaluates apply(Module, Function, Args) on the node Node.
%%% Returns whatever Function returns or {badrpc, Reason} if the
%%% remote procedure call fails. If Node is {Fun, FunArgs} applying
%%% Fun to FunArgs should return a node name.
call(Node, Module, Function, Args, TimeOut) ->
    call(Node, Module, Function, Args, TimeOut, []).

%%% @spec call(Node, Module, Function, Args, TimeOut, Cookie) -> 
%%%                                              term() | {badrpc, Reason}
%%%      Node = NodeName | {Fun, FunArgs} 
%%%      Fun = fun()
%%%      FunArgs =  term()
%%%      NodeName = atom() 
%%%      Module = atom() 
%%%      Function = atom()  
%%%      Args = [term()]
%%%      Reason = timeout | term() 
%%%      Cookie = atom()
%%%
%%% @doc Evaluates apply(Module, Function, Args) on the node Node.
%%% Returns whatever Function returns or {badrpc, Reason} if the
%%% remote procedure call fails. If Node is {Fun, FunArgs} applying
%%% Fun to FunArgs should return a node name. 
%%% The cookie on the client node will be set to <code>Cookie</code>
%%% for this rpc operation (use to match the server node cookie).
call({Fun, FunArgs}, Module, Function, Args, TimeOut, Cookie) ->
    Node = Fun(FunArgs),
    call(Node, Module, Function, Args, TimeOut, Cookie);
call(Node, Module, Function, Args, TimeOut, Cookie) when is_atom(Node) ->
    Cookie0 = set_the_cookie(Cookie),
    Result = rpc:call(Node, Module, Function, Args, TimeOut),
    _ = set_the_cookie(Cookie0),
    Result.    

%%% @spec cast(Node, Module, Function, Args) -> ok
%%%      Node = NodeName | {Fun, FunArgs} 
%%%      Fun = fun()
%%%      FunArgs =  term()
%%%      NodeName = atom() 
%%%      Module = atom()  
%%%      Function = atom() 
%%%      Args = [term()]
%%%      Reason = timeout | term() 
%%%
%%% @doc Evaluates apply(Module, Function, Args) on the node Node.
%%% No response is delivered and the process which makes the call is
%%% not suspended until the evaluation is compleated as in the case of
%%% call/[3,4]. If Node is {Fun, FunArgs} applying
%%% Fun to FunArgs should return a node name.
cast(Node, Module, Function, Args) ->
    cast(Node, Module, Function, Args, []).

%%% @spec cast(Node, Module, Function, Args, Cookie) -> ok
%%%      Node = NodeName | {Fun, FunArgs} 
%%%      Fun = fun()
%%%      FunArgs =  term()
%%%      NodeName = atom() 
%%%      Module = atom()  
%%%      Function = atom() 
%%%      Args = [term()]
%%%      Reason = timeout | term() 
%%%      Cookie = atom()
%%%
%%% @doc Evaluates apply(Module, Function, Args) on the node Node.
%%% No response is delivered and the process which makes the call is
%%% not suspended until the evaluation is compleated as in the case of
%%% call/[3,4]. If Node is {Fun, FunArgs} applying
%%% Fun to FunArgs should return a node name. 
%%% The cookie on the client node will be set to <code>Cookie</code>
%%% for this rpc operation (use to match the server node cookie).
cast({Fun, FunArgs}, Module, Function, Args, Cookie) ->
    Node = Fun(FunArgs),
    cast(Node, Module, Function, Args, Cookie);
cast(Node, Module, Function, Args, Cookie) when is_atom(Node) ->
    Cookie0 = set_the_cookie(Cookie),
    true = rpc:cast(Node, Module, Function, Args),
    _ = set_the_cookie(Cookie0),
    ok.


%%%---------- Internal -----------

%%% @hidden
set_the_cookie([]) ->
    [];
set_the_cookie(Cookie) ->
    Cookie0 = erlang:get_cookie(),
    erlang:set_cookie(node(),Cookie),
    Cookie0.
