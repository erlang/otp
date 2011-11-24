%%%-------------------------------------------------------------------
%%% File    : gcpFlowControl.erl
%%% Author  : EAB/UPD/AV
%%% Description : Implements overload protection.
%%%-------------------------------------------------------------------
-module(gcpFlowControl).
-id('24/190 55-CNA 113 033 Ux').
-vsn('/main/R1A/14').
-date('2005-05-04').
-author('uabasve').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 16/002 01-FEA 202 714 Ux, Rev: /main/4 %
%%%
%%% Copyright (C) 2001-2005 by Ericsson Telecom AB
%%% SE-126 25  STOCKHOLM
%%% SWEDEN, tel int + 46 8 719 0000
%%%
%%% The program may be used and/or copied only with the written
%%% permission from Ericsson Telecom AB, or in accordance with
%%% the terms and conditions stipulated in the agreement/contract
%%% under which the program has been supplied.
%%%
%%% All rights reserved
%%%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% --------   --------   --------    ------------------------
%%% R1A/1-2    05-02-07   ejojmjn     Copied from EAS R7A/11.
%%% R1A/3-14   05-03-14   uabasve     Clean.
%%%--------------------------------------------------------------------

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("gcp.hrl").

-export([send_request/4,     %% user send from gcpInterface
         receive_reply/2,    %% from callback in gcpTransaction
         init_ets_tables/1,
         init_data/2]).

-define(PRIO_INFINITY, 16).
-define(MIN_WINDOW, 10).
-define(MAX_WINDOW, 100).

-define(BUCKET_MAX, 100).
-define(BUCKET_THRESH_HIGH, 80).
-define(BUCKET_THRESH_LOW, 20).

-define(ALLOW_TIMEOUT, 1000).

%% Holds counters for flow control in GCP
-record(gcpFlowControlTable,
	{key,
	 window        = 50,
	 available     = 50,
	 bucket        = 0,
	 q             = 0,
	 sent          = 0,   %% Counts all attempts
	 rejectable    = 0,   %% Counts rejectable attempts
	 t95,
	 errors        = 0,
	 rejects       = 0,
	 replies       = 0}).

-record(gcpFlowControlBitmap,
        {key,
         count = 0}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: send_request/4
%%
%% Output: ok | {error, Reason}
%%--------------------------------------------------------------------

send_request(ActiveLink, TimerOptions, ActionRequests, UserData) ->
    #gcpActiveLinkTable{key = Key,
                        conn_handle = ConnHandle}
        = ActiveLink,
    Prio = prio(ActionRequests),
    incr(Key, sent),
    case allow(Key, Prio) of
        {true, Timestamp} ->
            grant_request(user_data(ConnHandle),
                          Key,
                          Prio,
                          Timestamp,
                          ConnHandle,
                          TimerOptions,
                          ActionRequests,
                          UserData);
        false ->
            {error, rejected}
    end.

%%--------------------------------------------------------------------
%% Function: receive_reply/2
%% Description:
%%--------------------------------------------------------------------

receive_reply(Key, Timestamp) ->
    incr(Key, available),
    incr(Key, replies),
    release(Key),
    report_time(Key, Timestamp).

%%--------------------------------------------------------------------
%% Func: init_ets_tables/1
%%
%% Returns: ok
%%--------------------------------------------------------------------

init_ets_tables(Role) ->
    create_ets(Role, gcpFlowControlTable, #gcpFlowControlTable.key),
    create_ets(Role, gcpFlowControlBitmap, #gcpFlowControlBitmap.key),
    ok.

create_ets(Role, Table, Pos) when integer(Pos) ->
    create_ets(Role,
               Table,
               [named_table, ordered_set, public, {keypos, Pos}]);

create_ets(test, Table, ArgList) ->
    ets:new(Table, ArgList);
create_ets(Role, Table, ArgList) ->
    case ets:info(Table) of
        undefined ->
            sysCmd:ets_new(Table, ArgList);
        _ when Role == ch ->
            sysCmd:inherit_tables([Table]);
        _ when Role == om ->
            ok
    end.

%%--------------------------------------------------------------------
%% Func: init_data/2
%%--------------------------------------------------------------------

init_data(Key, T95) ->
    ets:insert(gcpFlowControlTable, #gcpFlowControlTable{key = Key,
                                                         t95 = T95}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%% ----------------------------------------------------------
%%%     incr
%%% ----------------------------------------------------------

cntr(Key, Field) ->
    incr(Key, Field, 0).

incr(Key, Field) ->
    incr(Key, Field, 1).

-define(INCR(Field),
        incr(Key, Field, X) -> upd_c(Key, {#gcpFlowControlTable.Field, X})).

?INCR(sent);
?INCR(replies);
?INCR(q);
?INCR(t95);
?INCR(errors);
?INCR(rejects);
?INCR(rejectable);
?INCR(window);
?INCR(available);

incr(Key, bucket, X)->
    upd_c(Key, {#gcpFlowControlTable.bucket, X, ?BUCKET_MAX, ?BUCKET_MAX}).

upd_c(Key, N) ->
    ets:update_counter(gcpFlowControlTable, Key, N).

%%% ----------------------------------------------------------
%%%     decr
%%%
%%%     Beware that decr is implemented as incr, care has to be taken
%%%     not to bungle things when max/min values are used.
%%% ----------------------------------------------------------

decr(Key, available, X) ->
    upd_c(Key, {#gcpFlowControlTable.available, -X});
decr(Key, window, X) ->
    upd_c(Key, {#gcpFlowControlTable.window, -X});
decr(Key, bucket, X) ->
    upd_c(Key, {#gcpFlowControlTable.bucket, -X, 0, 0}).

decr(Key, Field) ->
    decr(Key, Field, 1).

%%% ----------------------------------------------------------
%%%     allow
%%% ----------------------------------------------------------

allow(Key, ?PRIO_INFINITY) ->
    decr(Key, available),
    {true, now()};

allow(Key, Prio) ->
    incr(Key, rejectable),
    case decr(Key, available) of
        N when N > 0 ->
            {true, no_stamp};
        _ ->
            %% We did not send it, therefore incr available again
            incr(Key, available),
            queue(Key, Prio)
    end.

%%% ----------------------------------------------------------
%%%     queue
%%% ----------------------------------------------------------

queue(Key, Prio) ->
    incr(Key, q),
    T = {Key, Prio, now(), self()},
    ets:insert(gcpFlowControlBitmap, #gcpFlowControlBitmap{key = T}),
    wait(T).

%%% ----------------------------------------------------------
%%%     wait
%%% ----------------------------------------------------------

wait({Key, _Prio, _When, _Self} = T) ->
    receive
        allow ->
            ets:delete(gcpFlowControlBitmap, T),
            decr(Key, available),
            {true, no_stamp}
    after ?ALLOW_TIMEOUT ->
            timeout(T),
            adjust_window(Key),
            incr(Key, rejects),
            false
    end.

timeout(T) ->
    case ets:update_counter(gcpFlowControlBitmap, T, 1) of
        1 ->
            %% Got the lock: no one has released Key and sent 'allow'.
            ets:delete(gcpFlowControlBitmap, T),
            ok;
        _ ->
            %% A releasing process got the lock: 'allow' has been
            %% sent. Try to remove the message before proceeding.
            %% (This is to keep mdisp from complaining apparently.)
            ets:delete(gcpFlowControlBitmap, T),
            receive
                allow ->
                    ok
            after ?ALLOW_TIMEOUT ->
                    io:format("~p: errant allow: ~p~n", [?MODULE, T])
            end
    end.

%% Now, if we reject and our general response time is low
%% (i.e. low bucket) then we increase the window size.
adjust_window(Key) ->
    adjust_window(Key,
                  cntr(Key, bucket) < ?BUCKET_THRESH_LOW
                  andalso cntr(Key, window) < ?MAX_WINDOW).

adjust_window(Key, true) ->
    incr(Key, window),
    incr(Key, available),
    incr(Key, bucket, 20);
adjust_window(_, false) ->
    ok.

%%--------------------------------------------------------------------
%% Func: report_time/2
%%--------------------------------------------------------------------

report_time(_, no_stamp) ->
    ok;
report_time(Key, {MS, S, Ms})->
    {MegaSecs, Secs, MicroSecs} = now(),
    p(Key,
      MicroSecs - Ms + 1000000*(Secs - S + 1000000*(MegaSecs - MS)),
      cntr(Key, t95)).

%%% ----------------------------------------------------------
%%%     p
%%% ----------------------------------------------------------

p(Key, Time, T95) when Time =< T95 ->
    decr(Key, bucket);
p(Key, _Time, _T95) ->
    %% If we have a long response time, then increase the leaky
    %% bucket. If the bucket is over the high watermark and the window
    %% is not already at its minimum size, then decrease the window
    %% and available.
    case {cntr(Key, window), incr(Key, bucket, 20)} of
        {Window, Bucket} when Window > ?MIN_WINDOW,
                              Bucket > ?BUCKET_THRESH_HIGH ->
            decr(Key, window),
            decr(Key, available);
        _ ->
            ok
    end.

%%% ----------------------------------------------------------
%%%     release
%%% ----------------------------------------------------------

release(Key) ->
    %% The choice of the key below will cause ets:prev/2 to return
    %% the key with the highest priority which was queued most
    %% recently. This relies on the fact that integers sort before
    %% atoms, the atom 'prio' in this case. The atoms 'queued' and
    %% 'pid' are of no significance.
    release(Key, {Key, prio, queued, pid}).

%% This isn't a (FIFO) queue within each priority, but a (LIFO) stack.

release(Key, T) ->
    release(Key, cntr(Key, available), ets:prev(gcpFlowControlBitmap, T)).

%% Note that only keys on the same Key are matched.
release(Key, N, {Key, _Prio, _When, Pid} = T) when N > 0 ->
    case catch ets:update_counter(gcpFlowControlBitmap, T, 1) of
        1 ->
            Pid ! allow;
        _ ->
            %% Another process has released this key.
            release(Key, T)
    end;

release(_, _, _)->
    ok.

%%% ----------------------------------------------------------
%%%     user_data
%%% ----------------------------------------------------------

user_data(ConnHandle) ->
    case catch megaco:conn_info(ConnHandle, reply_data) of
        {'EXIT', _Reason} ->
            false;
        Rec ->
            {value, Rec}
    end.

%%% ----------------------------------------------------------
%%%     grant_request
%%% ----------------------------------------------------------

grant_request({value, Rec},
              Key, Prio, Time,
              ConnHandle, Options, ActionRequests, UserData) ->
    ReplyData = Rec#gcpReplyData{user_data = UserData,
                                 prio      = Prio,
                                 timestamp = Time},
    cast_rc(megaco:cast(ConnHandle,
                        ActionRequests,
                        [{reply_data, ReplyData} | Options]),
            Key,
            ActionRequests);

grant_request(false, Key, _, _, _, _, _, _) ->
    incr(Key, available),
    {error, reply_data}.

cast_rc(ok = Ok, _, _) ->
    Ok;
cast_rc({error, Reason}, Key, ActionRequests) ->
    incr(Key, available),
    gcpLib:error_report(?MODULE, send_request, [ActionRequests],
                        "send failed",
                        Reason),
    {error, {encode, Reason}}.

%%--------------------------------------------------------------------
%% Func: prio/1
%% Returns: The priority of the request
%%--------------------------------------------------------------------

prio([ActionRequest | _]) ->
    #'ActionRequest'{contextId = ContextId,
                     contextRequest = ContextRequest}
        = ActionRequest,
    prio(ContextId, ContextRequest).

prio(?megaco_choose_context_id, #'ContextRequest'{priority = Prio})
  when integer(Prio) ->
    Prio;
prio(_, _) ->
    ?PRIO_INFINITY.
