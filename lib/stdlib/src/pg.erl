%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
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
-module(pg).
-deprecated(module).

%% pg provides a process group facility. Messages 
%% can be multicasted to all members in the group

-export([create/1,
	 create/2,
	 standby/2,
	 join/2,
	 send/2,
	 esend/2,
	 members/1,
	 name_to_pid/1,
	 master/1]).


%% Create a brand new empty process group with the master residing 
%% at the local node

-spec create(PgName) -> 'ok' | {'error', Reason} when
      PgName :: term(),
      Reason :: 'already_created' | term().

create(PgName) -> 
    catch begin check(PgName),
    Pid = spawn(pg,master,[PgName]),
    global:register_name(PgName,Pid),
    ok end.

%% Create a brand new empty process group with the master 
%% residing at Node

-spec create(PgName, Node) -> 'ok' | {'error', Reason} when
      PgName :: term(),
      Node :: node(),
      Reason :: 'already_created' | term().

create(PgName, Node) ->
    catch begin check(PgName),
    Pid = spawn(Node,pg,master,[PgName]),
    global:register_name(PgName,Pid),
    ok end.

%% Have a process on Node that will act as a standby for the process
%% group manager. So if the node where the manager runs fails, the
%% process group will continue to function.

-spec standby(term(), node()) -> 'ok'.

standby(_PgName, _Node) ->
    ok.

%% Tell process group PgName that Pid is a new member of the group
%% synchronously return a list of all old members in the group

-spec join(PgName, Pid) -> Members when
      PgName :: term(),
      Pid :: pid(),
      Members :: [pid()].

join(PgName, Pid) when is_atom(PgName) -> 
    global:send(PgName, {join,self(),Pid}),
    receive
	{_P,{members,Members}} ->
	    Members
    end.

%% Multi cast Mess to all members in the group

-spec send(PgName, Msg) -> 'ok' when
      PgName :: term(),
      Msg :: term().

send(PgName, Mess) when is_atom(PgName) ->
    global:send(PgName, {send, self(), Mess}),
    ok;
send(Pg, Mess) when is_pid(Pg) ->
    Pg ! {send,self(),Mess},
    ok.

%% multi cast a message to all members in the group but ourselves
%% If we are a member

-spec esend(PgName, Msg) -> 'ok' when
      PgName :: term(),
      Msg :: term().

esend(PgName, Mess) when is_atom(PgName) ->
    global:send(PgName, {esend,self(),Mess}),
    ok;
esend(Pg, Mess) when is_pid(Pg) ->
    Pg ! {esend,self(),Mess},
    ok.

%% Return the members of the group

-spec members(PgName) -> Members when
      PgName :: term(),
      Members :: [pid()].

members(PgName) when is_atom(PgName) ->
    global:send(PgName, {self() ,members}),
    receive
	{_P,{members,Members}} ->
	    Members
    end;
members(Pg) when is_pid(Pg) ->
    Pg ! {self,members},
    receive
	{_P,{members,Members}} ->
	    Members
    end.

-spec name_to_pid(atom()) -> pid() | 'undefined'.

name_to_pid(PgName) when is_atom(PgName) ->
    global:whereis_name(PgName).

-spec master(term()) -> no_return().

master(PgName) ->
    process_flag(trap_exit, true),
    master_loop(PgName, []).

master_loop(PgName,Members) ->
    receive
	{send,From,Message} ->
	    send_all(Members,{pg_message,From,PgName,Message}),
	    master_loop(PgName,Members);
	{esend,From,Message} ->
	    send_all(lists:delete(From,Members),
		     {pg_message,From,PgName,Message}),
	    master_loop(PgName,Members);
	{join,From,Pid} ->
	    link(Pid),
	    send_all(Members,{new_member,PgName,Pid}),
	    From ! {self(),{members,Members}},
	    master_loop(PgName,[Pid|Members]);
	{From,members} ->
	    From ! {self(),{members,Members}},
	    master_loop(PgName,Members);
	{'EXIT',From,_} ->
	    L =
		case lists:member(From,Members) of
		    true ->
			NewMembers = lists:delete(From,Members),
			send_all(NewMembers, {crashed_member,PgName,From}),
			NewMembers;
		    false ->
			Members
		end,
	    master_loop(PgName,L)  
    end.

send_all([], _) -> ok;
send_all([P|Ps], M) ->
    P ! M,
    send_all(Ps, M).

%% Check if the process group already exists

check(PgName) ->
    case global:whereis_name(PgName) of
        Pid when is_pid(Pid) -> 
            throw({error,already_created});
        undefined ->
	    ok
    end.
