%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%--------------------------------------------------------------------

-ifndef(ORBER_COSNAMING_HRL).
-define(ORBER_COSNAMING_HRL, true).

%%-----------------------------------------------------------------
%% Mnesia Table definition record
%%-----------------------------------------------------------------
-record('orber_CosNaming', {name_context, nameindex}).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------

-define(CREATE_OPTS, [{no_security, orber:partial_security()}]).

%%-define(dirty_query_context, true).

%% This macro returns a read fun suitable for evaluation in a transaction
-define(read_function(Objkey),
	fun() ->
		mnesia:read(Objkey)
	end).

%% This macro returns a write fun suitable for evaluation in a transaction
-define(write_function(R),
	fun() ->
		mnesia:write(R)
	end).

%% This macro returns a delete fun suitable for evaluation in a transaction
-define(delete_function(R),
	fun() ->
		mnesia:delete(R)
	end).

-ifdef(dirty_query_context).
-define(query_check(Q_res), Q_res).
-else.
-define(query_check(Q_res), {atomic, Q_res}).
-endif.

-endif.
