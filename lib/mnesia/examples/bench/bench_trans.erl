%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File    : bench_trans.hrl
%%% Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
%%% Purpose : Implement the transactions in Canadian database benchmark (LMC/UU-01:025)
%%% Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bench_trans).
-author('hakan@cslab.ericsson.se').

-include("bench.hrl").

-export([
	 update_current_location/5,
	 read_current_location/2,
	 read_session_details/4,
	 create_session_to_server/6,
	 delete_session_from_server/5,
	 number_to_suffix/1,
	 number_to_key/2
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The transactions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -------------------------------------------------------------------
%% T1
%% -------------------------------------------------------------------

update_current_location(Wlock, SubscrId, Location, ChangedBy, ChangedTime) ->
    Suffix   = number_to_suffix(SubscrId),
    [Subscr] = mnesia:read({subscriber, Suffix}, SubscrId, Wlock),
    Subscr2  = Subscr#subscriber{location     = Location,
                                 changed_by   = ChangedBy,
                                 changed_time = ChangedTime},
    mnesia:write(subscriber, Subscr2, Wlock),
    {do_commit, false, [ok]}.

%% -------------------------------------------------------------------
%% T2
%% -------------------------------------------------------------------

read_current_location(_Wlock, SubscrId) ->
    Suffix   = number_to_suffix(SubscrId),
    [Subscr] = mnesia:read({subscriber, Suffix}, SubscrId, read),

    Name          = Subscr#subscriber.subscriber_name,
    Location      = Subscr#subscriber.location,
    ChangedBy     = Subscr#subscriber.changed_by,
    ChangedTime   = Subscr#subscriber.changed_time,
    {do_commit, false, [Name, Location, ChangedBy, ChangedTime]}.

%% -------------------------------------------------------------------
%% T3
%% -------------------------------------------------------------------

read_session_details(Wlock, SubscrId, ServerBit, ServerId) ->
    Suffix   = number_to_suffix(SubscrId),
    [Subscr] = mnesia:read({subscriber, Suffix}, SubscrId, read),
    %%[Group]  = mnesia:read(group, Subscr#subscriber.group_id, read),
    [Group]  = mnesia:dirty_read(group, Subscr#subscriber.group_id),

    IsAllowed     = ((Group#group.allow_read band ServerBit) == ServerBit),
    IsActive      = ((Subscr#subscriber.active_sessions band ServerBit) == ServerBit),    
    ExecuteBranch = (IsAllowed and IsActive),

    case ExecuteBranch of
        true ->
            SessionKey = {SubscrId, ServerId},
            [Session] = mnesia:read({session, Suffix}, SessionKey, read),

            ServerKey = {ServerId, Suffix},
            [Server] = mnesia:read({server, Suffix}, ServerKey, Wlock),
            Server2 = Server#server{no_of_read = Server#server.no_of_read + 1},
            mnesia:write(server, Server2, Wlock),
            {do_commit, ExecuteBranch, [Session#session.session_details]};
        false  ->
            {do_commit, ExecuteBranch, []}
    end.

%% -------------------------------------------------------------------
%% T4
%% -------------------------------------------------------------------

create_session_to_server(Wlock, SubscrId, ServerBit, ServerId, Details, DoRollback) ->
    Suffix   = number_to_suffix(SubscrId),
    [Subscr] = mnesia:read({subscriber, Suffix}, SubscrId, Wlock),
    %%[Group]  = mnesia:read(group, Subscr#subscriber.group_id, read),
    [Group]  = mnesia:dirty_read(group, Subscr#subscriber.group_id),

    IsAllowed     = ((Group#group.allow_insert band ServerBit) == ServerBit),
    IsInactive    = ((Subscr#subscriber.active_sessions band ServerBit) == 0),
    ExecuteBranch = (IsAllowed and IsInactive),
    case ExecuteBranch of
        true ->
            SessionKey = {SubscrId, ServerId},
            Session = #session{session_key     = SessionKey,
                               session_details = Details,
                               suffix          = Suffix},
            mnesia:write(session, Session, Wlock),
            Active = (Subscr#subscriber.active_sessions bor ServerBit),
            Subscr2 = Subscr#subscriber{active_sessions = Active},
            mnesia:write(subscriber, Subscr2, Wlock),

            ServerKey = {ServerId, Suffix},
            [Server] = mnesia:read({server, Suffix}, ServerKey, Wlock),
            Server2 = Server#server{no_of_insert = Server#server.no_of_insert + 1},
            mnesia:write(server, Server2, Wlock);
        false ->
            ignore
    end,
    case DoRollback of
        true ->
            mnesia:abort({do_rollback, ExecuteBranch, []});
        false ->
            {do_commit, ExecuteBranch, []}
    end.

%% -------------------------------------------------------------------
%% T5
%% -------------------------------------------------------------------

delete_session_from_server(Wlock, SubscrId, ServerBit, ServerId, DoRollback) ->
    Suffix   = number_to_suffix(SubscrId),
    [Subscr] = mnesia:read({subscriber, Suffix}, SubscrId, Wlock),
    %%[Group]  = mnesia:read(group, Subscr#subscriber.group_id, read),
    [Group]  = mnesia:dirty_read(group, Subscr#subscriber.group_id),

    IsAllowed     = ((Group#group.allow_delete band ServerBit) == ServerBit),
    IsActive      = ((Subscr#subscriber.active_sessions band ServerBit) == ServerBit),    
    ExecuteBranch = (IsAllowed and IsActive),
    case ExecuteBranch of
        true ->
            SessionKey = {SubscrId, ServerId},
            mnesia:delete({session, Suffix}, SessionKey, Wlock),
            Active = (Subscr#subscriber.active_sessions bxor ServerBit),
            Subscr2 = Subscr#subscriber{active_sessions = Active},
            mnesia:write(subscriber, Subscr2, Wlock),

            ServerKey = {ServerId, Suffix},
            [Server] = mnesia:read({server, Suffix}, ServerKey, Wlock),
            Server2 = Server#server{no_of_delete = Server#server.no_of_delete + 1},
            mnesia:write(server, Server2, Wlock);
        false ->
            ignore
    end,
    case DoRollback of
        true ->
            mnesia:abort({do_rollback, ExecuteBranch, []});
        false ->
            {do_commit, ExecuteBranch, []}
    end.

number_to_suffix(SubscrId) when is_integer(SubscrId) ->
    SubscrId rem 100;
number_to_suffix(<<_:8/binary, TimesTen:8/integer, TimesOne:8/integer>>) ->
    ((TimesTen - $0) * 10) + (TimesOne - $0).

number_to_key(Id, C) when is_integer(Id) ->
    case C#config.use_binary_subscriber_key of
	true ->
	    list_to_binary(string:right(integer_to_list(Id), 10, $0));
	false ->
	    Id
    end.
    
