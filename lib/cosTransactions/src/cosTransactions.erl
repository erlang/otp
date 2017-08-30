%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% File    : cosTransactions.erl
%% Purpose : Initialize the 'cosTransactions' application
%%----------------------------------------------------------------------

-module(cosTransactions).

%%--------------- INCLUDES -----------------------------------
%% Local
-include_lib("ETraP_Common.hrl").
-include_lib("CosTransactions.hrl").
%%--------------- EXPORTS-------------------------------------
%% cosTransactions API external
-export([start/0, stop/0, start_factory/1, start_factory/0, stop_factory/1]).

%% Application callbacks
-export([start/2, init/1, stop/1]).

%%------------------------------------------------------------
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosTRansaction application.
%%------------------------------------------------------------

start() ->
    application:start(cosTransactions).
stop() ->
    application:stop(cosTransactions).

%%------------------------------------------------------------
%% function : start_factory 
%% Arguments: none or an argumentlist which by default is defined
%%            in ETraP_Common.hrl, i.e., '?tr_FAC_DEF'
%% Returns  : ObjectRef | {'EXCEPTION', _} | {'EXIT', Reason}
%% Effect   : Starts a CosTransactions_TransactionFactory
%%------------------------------------------------------------

start_factory() ->
    ?tr_start_child(?SUP_FAC(?tr_FAC_DEF)).
    
start_factory(Args) when is_list(Args) ->
    ?tr_start_child(?SUP_FAC(Args));
start_factory(Args) ->
    ?tr_error_msg("applications:start( ~p ) failed. Bad parameters~n", [Args]),
    exit("applications:start failed. Bad parameters").

%%------------------------------------------------------------
%% function : stop_factory 
%% Arguments: Factory Object Reference
%% Returns  : ok | {'EXCEPTION', _}
%% Effect   : 
%%------------------------------------------------------------

stop_factory(Fac)->
    corba:dispose(Fac).

%%------------------------------------------------------------
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------

start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosTransactions, app_init).


%%------------------------------------------------------------
%% function : stop
%% Arguments: Arg - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------

stop(_) ->
    ok.

%%------------------------------------------------------------
%% function : init
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

%% Starting using create_factory/X
init(own_init) ->
    {ok,{?SUP_FLAG, [?SUP_CHILD]}};
%% When starting as an application.
init(app_init) ->
    {ok,{?SUP_FLAG, [?SUP_CHILD]}}.


%%--------------- END OF MODULE ------------------------------
