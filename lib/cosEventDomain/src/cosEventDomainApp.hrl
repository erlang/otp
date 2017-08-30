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
%% File    : cosEventDomainApp.hrl
%% Purpose : 
%%----------------------------------------------------------------------


%%--------------- INCLUDES -----------------------------------
%% External

%% Local

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("============== CosEventDomain =============~n"
		       Txt
		       "===========================================~n",
		       Arg)).

-define(DEBUG_LEVEL, 3).

-ifdef(debug).
-define(DBG(F,A),
        io:format("[LINE: ~p MODULE: ~p] "++F,[?LINE, ?MODULE]++A)).
-else.
-define(DBG(F,A), ok).
-endif.    

%%%%%% WARNING! These definitions are defined in the CosEventDomainAdmin.idl
%%%%%% file. If the specification is changed so must the definitions!!
%%%%%% We use this approach to be able to use them as guards.

%%%% Constant: 'CycleDetection'
-define(CycleDetection, "CycleDetection").

%%%% Constant: 'AuthorizeCycles'
-define(AuthorizeCycles, 0).

%%%% Constant: 'ForbidCycles'
-define(ForbidCycles, 1).

%%%% Constant: 'DiamondDetection'
-define(DiamondDetection, "DiamondDetection").

%%%% Constant: 'AuthorizeDiamonds'
-define(AuthorizeDiamonds, 0).

%%%% Constant: 'ForbidDiamonds'
-define(ForbidDiamonds, 1).

%%--------------- END OF MODULE ------------------------------
