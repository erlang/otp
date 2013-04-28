%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
-module(snmpa_mib_data).

%%%-----------------------------------------------------------------
%%% This is the behaviour for the MIB server backend internal 
%%% data storage. 
%%%-----------------------------------------------------------------


-callback new(MibStorage :: mib_storage()) -> State :: term().

-callback close(State :: term()) -> ok.

-callback sync(State :: term()) -> ok.

-callback load_mib(State :: term(), FileName :: string(), 
		   MeOverride :: boolean(), 
		   TeOverride :: boolean()) -> 
    {ok, NewState :: term()} | {error, already_loaded | Reason :: term()}.

-callback unload_mib(State :: term(), FileName :: string(), 
		   MeOverride :: boolean(), 
		   TeOverride :: boolean()) -> 
    {ok, NewState :: term()} | {error, not_loaded | Reason :: term()}.

-callback lookup(State :: term(), Oid :: oid()) -> 
    {false, Reason :: term()} | 
    {variable, MibEntry :: me()} |
    {table_column, MibEntry :: me(), TableEntryOid :: oid()} |
    {subagent, SubAgentPid :: pid(), SAOid :: oid()}.

-callback next(State :: term(), Oid :: oid(), MibView :: mib_view()) -> 
    endOfView | false | 
    {subagent, SubAgentPid :: pid(), SAOid :: oid()} |
    {variable, MibEntry :: me(), VarOid :: oid()} |
    {table, TableOid :: oid(), TableRestOid :: oid(), MibEntry :: me()}

-callback register_subagent(State :: term(), Oid :: oid(), Pid :: pid()) -> 
    {error, Reason :: term()} | NewState :: term().

-callback unregister_subagent(State :: term(), 
			      Pid :: pid() | Oid :: oid()) -> 
    {ok, NewState :: term()}               | % When second arg wa a pid()
    {ok, NewState :: term(), Pid :: pid()} | % When second arg was a oid()
    {error, Reason :: term()}.

-callback dump(State :: term(), Destination :: io | filename()) -> 
    ok | {error, Reason :: term()}.

-callback which_mib(State :: term(), Oid :: oid()) -> 
    {ok, Mib :: string()} | {error, Reason :: term()}.

-callback which_mibs(State :: term()) -> 
    [{MibName :: atom(), Filename :: string()}].

-callback whereis_mib(State :: term(), MibName :: atom()) -> 
    {ok, Filename :: string()} | {error, Reason :: term()}.

-callback info(State :: term()) -> list().
-callback info(State :: term(), Item :: atom()) -> term().

-callback backup(State :: term(), BackupDir :: string()) -> 
    ok |  {error, Reason :: term()}.

-callback code_change(Direction :: up | down, State :: term()) -> 
    NewState :: term().



