%%----------------------------------------------------------------------
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
%% File    : cosProperty.hrl
%% Purpose : 
%%----------------------------------------------------------------------


%%--------------- INCLUDES -----------------------------------
%% External
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%-----------------------------------------------------------------
%% Mnesia Table definition record
%%-----------------------------------------------------------------
-record('oe_CosPropertyService', {key, properties}).
 
%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(PropertySet,    0).
-define(PropertySetDef, 1).
 
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
 
-define(query_check(Q_res), {atomic, Q_res}).
 

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("================ CosProperty ==============~n"
		       Txt
		       "===========================================~n",
		       Arg)).



-ifdef(debug).
-define(debug_print(F,A),
        io:format("[LINE: ~p MODULE: ~p] "++F,[?LINE, ?MODULE]++A)).
-define(property_TypeCheck(O,M), 'cosProperty':type_check(O,M)).
-else.
-define(debug_print(F,A), ok).
-define(property_TypeCheck(O,I), ok).
-endif.    

%%--------------- END OF MODULE ------------------------------
