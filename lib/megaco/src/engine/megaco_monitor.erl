%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Monitor connections and timers
%%----------------------------------------------------------------------

-module(megaco_monitor).

-behaviour(gen_server).


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------

-include_lib("megaco/src/app/megaco_internal.hrl"). 


%% Application internal exports
-export([
	 start_link/0,
	 stop/0,

	 apply_after/4,
	 apply_after/5,
	 cancel_apply_after/1,

	 lookup_request/1,
	 lookup_request_field/2,
	 match_requests/1,
	 which_requests/1,
	 insert_request/1,
	 update_request_field/3, update_request_fields/2, 
	 delete_request/1, 

	 request_lockcnt_cre/1, 
	 request_lockcnt_del/1, 
	 request_lockcnt_inc/1, 
	 request_lockcnt_dec/1, 

	 lookup_reply/1,
	 lookup_reply_field/2,
	 match_replies/1,
	 which_replies/1,
	 insert_reply/1, insert_reply_new/1,
	 update_reply_field/3, update_reply_fields/2, 
	 delete_reply/1,

	 apply_at_exit/4,
	 cancel_apply_at_exit/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {parent_pid}).
-record(apply_at_exit, {ref, pid, module, function, arguments}).



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    ?d("start -> entry", []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

stop() ->
    call(stop).

lookup_request(Key) ->
    ets:lookup(megaco_requests, Key).

lookup_request_field(Key, Field) ->
    try
	begin
	    {ok, ets:lookup_element(megaco_requests, Key, Field)}
	end
    catch 
	error:badarg ->
	    {error, not_found}
    end.

match_requests(Pat) ->
    ets:match_object(megaco_requests, Pat).

which_requests(Pat) ->
    Spec = [{Pat, [], ['$$']}],
    ets:select(megaco_requests, Spec).

insert_request(Rec) ->
    ets:insert(megaco_requests, Rec).

update_request_field(Key, Field, NewValue) ->
    ets:update_element(megaco_requests, Key, {Field, NewValue}).

update_request_fields(Key, NewFields) when is_list(NewFields) ->
    ets:update_element(megaco_requests, Key, NewFields).

delete_request(Key) ->
    ets:delete(megaco_requests, Key).


request_lockcnt_cre(TransId) ->
    Key = {TransId, lockcnt},
    ets:insert_new(megaco_requests, {Key, 1}).

request_lockcnt_del(TransId) ->
    Key = {TransId, lockcnt},
    ets:delete(megaco_requests, Key).

request_lockcnt_inc(TransId) ->
    Key = {TransId, lockcnt},
    (catch ets:update_counter(megaco_requests, Key, 1)).

request_lockcnt_dec(TransId) ->
    Key = {TransId, lockcnt},
    (catch ets:update_counter(megaco_requests, Key, -1)).


lookup_reply(Key) ->
    ets:lookup(megaco_replies, Key).

lookup_reply_field(Key, Field) ->
    try
	begin
	    {ok, ets:lookup_element(megaco_replies, Key, Field)}
	end
    catch 
	error:badarg ->
	    {error, not_found}
    end.

match_replies(Pat) ->
    ets:match_object(megaco_replies, Pat).

which_replies(Pat) ->
    Spec = [{Pat, [], ['$$']}],
    ets:select(megaco_replies, Spec).

insert_reply(Rec) ->
    ets:insert(megaco_replies, Rec).

insert_reply_new(Rec) ->
    ets:insert_new(megaco_replies, Rec).

update_reply_field(Key, Field, NewValue) ->
    ets:update_element(megaco_replies, Key, {Field, NewValue}).

update_reply_fields(Key, NewFields) when is_list(NewFields) ->
    ets:update_element(megaco_replies, Key, NewFields).

delete_reply(Key) ->
    ets:delete(megaco_replies, Key).

apply_after(M, F, A, Time) ->
    apply_after(spawn_method, M, F, A, Time).

apply_after(Method, M, F, A, Time) 
  when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    if
	Time =:= infinity ->
	    apply_after_infinity;
	is_integer(Time) ->
	    Msg = {apply_after, Method, M, F, A},
	    Ref = erlang:send_after(Time, whereis(?SERVER), Msg),
	    {apply_after, Ref}
    end.

cancel_apply_after({apply_after, Ref}) ->
    case erlang:cancel_timer(Ref) of
	TimeLeft when is_integer(TimeLeft) ->
	    {ok, TimeLeft};
	_ ->
	    {error, {already_expired, Ref}}
    end;
cancel_apply_after(apply_after_infinity) ->
    ok;
cancel_apply_after(BadRef) ->
    {error, {bad_ref, BadRef}}.

%% Performs apply(M, F, [Reason | A]) when process Pid dies
apply_at_exit(M, F, A, Pid) 
  when is_atom(M) andalso is_atom(F) andalso is_list(A) andalso is_pid(Pid) ->
    Ref = call({apply_at_exit, M, F, A, Pid}),
    {apply_at_exit, Ref}.

cancel_apply_at_exit({apply_at_exit, Ref}) ->
    cast({cancel_apply_at_exit, Ref});
cancel_apply_at_exit(BadRef) ->
    {error, {bad_ref, BadRef}}.
    
call(Request) ->
    gen_server:call(?SERVER, Request, infinity).

cast(Msg) ->
    ?SERVER ! Msg, ok.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([Parent]) ->
    ?d("init -> entry", []),
    process_flag(trap_exit, true),
    ets:new(megaco_requests, [public, named_table, {keypos, 2}]),
    ets:new(megaco_replies,  [public, named_table, {keypos, 2}]),
    ?d("init -> done", []),
    {ok, #state{parent_pid = Parent}}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({apply_at_exit, M, F, A, Pid}, _From, S) ->
    Ref = erlang:monitor(process, Pid),
    AAE = #apply_at_exit{ref       = Ref,
			 pid       = Pid,
			 module    = M,
			 function  = F,
			 arguments = A},
    put({?MODULE, Ref}, AAE),
    Reply = Ref,
    {reply, Reply, S};

handle_call(stop, {Parent, _} = _From, #state{parent_pid = Parent} = S) ->
    {stop, normal, ok, S};

handle_call(Req, From, S) ->
    warning_msg("received unexpected request from ~p: "
		"~n~w",[From, Req]),
    {reply, {error, {bad_request, Req}}, S}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, S) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({cancel_apply_at_exit, Ref}, S) ->
    case erase({?MODULE, Ref}) of
	undefined ->
	    %% Reply = {error, {already_cancelled, {apply_at_exit, Ref}}},
	    {noreply, S};
	_AAE ->
	    erlang:demonitor(Ref),
	    {noreply, S}
    end;

handle_info({apply_after, Method, M, F, A}, S) ->
    handle_apply(Method, M, F, A, apply_after),
    {noreply, S};

%% Handle the old format also...
handle_info({apply_after, M, F, A}, S) ->
    handle_apply(M, F, A, apply_after),
    {noreply, S};

handle_info({'DOWN', Ref, process, _Pid, Reason}, S) ->
    case erase({?MODULE, Ref}) of
	undefined ->
	    {noreply, S};
	AAE ->
	    M = AAE#apply_at_exit.module,
	    F = AAE#apply_at_exit.function,
	    A = AAE#apply_at_exit.arguments,
	    handle_apply(M, F, [Reason | A], apply_at_exit),
	    {noreply, S}
    end;

handle_info({'EXIT', Pid, Reason}, S) when Pid == S#state.parent_pid ->
    %% [megaco_messenger:disconnect(CH, {stopped, Reason})
    %% 	|| CH <- megaco:lookup_system_info(connections)],
    {stop, Reason, S};

handle_info(Info, S) ->
    warning_msg("received unknown info: "
		"~n~w", [Info]),
    {noreply, S}.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_Vsn, S, _Extra) ->
    {ok, S}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

handle_apply(M, F, A, _ErrorTag) ->
    spawn(M, F, A).

handle_apply(spawn_method, M, F, A, _ErrorTag) ->
    spawn(M, F, A);
handle_apply(_Method, M, F, A, _ErrorTag) ->
    (catch apply(M, F, A)).


warning_msg(F, A) ->
    ?megaco_warning("Monitor server: " ++ F, A).


% d(F) ->
%     d(F,[]).

% d(F,A) ->
%     %% d(true,F,A).
%     d(get(dbg),F,A).

% d(true,F,A) ->
%     io:format("*** [~s] ~p:~p ***"
% 	      "~n   " ++ F ++ "~n", 
% 	      [format_timestamp(now()), self(),?MODULE|A]);
% d(_, _, _) ->
%     ok.

% format_timestamp(Now) ->
%     {N1, N2, N3}   = Now,
%     {Date, Time}   = calendar:now_to_datetime(Now),
%     {YYYY,MM,DD}   = Date,
%     {Hour,Min,Sec} = Time,
%     FormatDate = 
%         io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
%                       [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
%     lists:flatten(FormatDate).


