%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File        : cosEventApp.hrl
%% Description : 
%%
%%----------------------------------------------------------------------

%%--------------- INCLUDES -----------------------------------
%% External
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("================ CosEvent =================~n"
		       Txt
		       "===========================================~n",
		       Arg)).


-define(PULL_INTERVAL, pull_interval).
-define(TYPECHECK, typecheck).
-define(MAXEVENTS, maxEvents).
-define(BLOCKING, blocking).
-define(SERVER, server_options).
-define(DEFAULT_OPTIONS, [{?PULL_INTERVAL, 20}, 
			  {?BLOCKING, true},
			  {?TYPECHECK, false},
			  {?MAXEVENTS, 300},
			  {?SERVER, []}]).

-define(DEBUG_LEVEL, 3).

-ifdef(debug).
-define(DBG(F,A),
        io:format("[~p (~p)] "++F,[?MODULE, ?LINE]++A)).
-else.
-define(DBG(F,A), ok).
-endif.    




%%--------------- END OF MODULE ----------------------------------------
