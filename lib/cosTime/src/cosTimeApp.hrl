%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%% File    : cosTimeApp.hrl
%% Purpose : 
%%----------------------------------------------------------------------


%%--------------- INCLUDES -----------------------------------
%% External
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%% Local
-include("CosTimerEvent.hrl").
-include("CosTime.hrl").
-include("CosTimerEvent.hrl").
-include("TimeBase.hrl").

-define(CREATE_OPTS, [{no_security, orber:partial_security()}]).

-define(max_Inaccuracy, 281474976710655).
-define(max_TimeT, 18446744073709551616).

%% The calendar module uses year 0 as base for gregorian functions.
%% 'ABSOULTE_TIME_DIFF' is #seconfs from year 0 until 15 october 1582, 00:00.
-define(ABSOLUTE_TIME_DIFF, 49947926400).
%% As above but diff year 0 to 00:00 GMT, January 1, 1970
-define(STANDARD_TIME_DIFF, 62167219200).

-define(split_TimeT(T), {((T band 16#0000ffff00000000) bsr 32),
			 (T band 16#00000000ffffffff)}).

-define(high_TimeT(T), ((T band 16#0000ffff00000000) bsr 32)).			
-define(low_TimeT(T),  (T band 16#00000000ffffffff)).

-define(concat_TimeT(H,L), ((H bsl 32) + L)).

-define(convert_TimeT2TimerT(N), erlang:trunc(N*1.0e-4)).

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("================ CosTime ==================~n"
		       Txt
		       "===========================================~n",
		       Arg)).



-ifdef(debug).
-define(debug_print(F,A),
        io:format("[LINE: ~p MODULE: ~p] "++F,[?LINE, ?MODULE]++A)).
-define(time_TypeCheck(O,M), 'cosTime':type_check(O,M)).
-else.
-define(debug_print(F,A), ok).
-define(time_TypeCheck(O,I), ok).
-endif.    

%%--------------- END OF MODULE ------------------------------
