%%----------------------------------------------------------------------
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
