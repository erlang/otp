%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(logger_handler_watcher).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register_handler/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {handlers}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_handler(Id::logger:handler_id(),Pid::pid()) -> ok.
register_handler(Id,Pid) ->
    gen_server:call(?SERVER,{register,Id,Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{handlers=[]}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({register,Id,Pid}, _From, #state{handlers=Hs}=State) ->
    Ref = erlang:monitor(process,Pid),
    Hs1 = lists:keystore(Id,1,Hs,{Id,Ref}),
    {reply, ok, State#state{handlers=Hs1}}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'DOWN',Ref,process,_,shutdown}, #state{handlers=Hs}=State) ->
    case lists:keytake(Ref,2,Hs) of
        {value,{Id,Ref},Hs1} ->
            %% Probably terminated by supervisor. Remove the handler to avoid
            %% error printouts due to failing handler.
            _ = case logger:get_handler_config(Id) of
                    {ok,_} ->
                        logger:remove_handler(Id);
                    _ ->
                        ok
                end,
            {noreply,State#state{handlers=Hs1}};
        false ->
            {noreply, State}
    end;
handle_info({'DOWN',Ref,process,_,_OtherReason}, #state{handlers=Hs}=State) ->
    {noreply,State#state{handlers=lists:keydelete(Ref,2,Hs)}};
handle_info(_Other,State) ->
    {noreply,State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.
