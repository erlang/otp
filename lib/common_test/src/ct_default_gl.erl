%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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

-module(ct_default_gl).
-export([start_link/1, stop/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

%% start_link()
%% Start a new group leader process.
start_link(ParentGL) ->
    do_start(ParentGL, 3).

do_start(_ParentGL, 0) ->
    exit({?MODULE,startup});
do_start(ParentGL, Retries) ->
    case whereis(?MODULE) of
	undefined ->
	    case gen_server:start_link(?MODULE, [ParentGL], []) of
		{ok,Pid} ->
		    {ok,Pid};
		Other ->
		    Other
	    end;
	Pid ->
	    exit(Pid, kill),
	    timer:sleep(1000),
	    do_start(ParentGL, Retries-1)
    end.

%% stop(Pid)
%% Stop a group leader process.
stop() ->
    gen_server:cast(whereis(?MODULE), stop).


%%% Internal functions.

init([ParentGL]) ->
    register(?MODULE, self()),
    ct_util:mark_process(),
    {ok,#{parent_gl_pid => ParentGL,
	  parent_gl_monitor => erlang:monitor(process,ParentGL)}}.

handle_cast(stop, St) ->
    {stop,normal,St}.

%% If the parent group leader dies, fall back on using the local user process
handle_info({'DOWN',Ref,process,_,_Reason}, #{parent_gl_monitor := Ref} = St) ->
    User = whereis(user),
    {noreply,St#{parent_gl_pid => User,
		 parent_gl_monitor => erlang:monitor(process,User)}};

handle_info({io_request,_From,_ReplyAs,_Req} = IoReq,
	    #{parent_gl_pid := ParentGL} = St) ->
    ParentGL ! IoReq,
    {noreply,St};

handle_info(Msg, St) ->
    io:format(user, "Common Test Group Leader process got: ~tp~n", [Msg]),
    {noreply,St}.

handle_call(_Req, _From, St) ->
    {reply,ok,St}.

terminate(_, _) ->
    ok.
