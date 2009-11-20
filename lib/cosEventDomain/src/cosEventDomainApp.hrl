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
