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
%% File    : cosFileTransferApp.hrl
%% Purpose : 
%% Created : 10 Feb 2000
%%----------------------------------------------------------------------


%%--------------- INCLUDES -----------------------------------
%% External
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("cosProperty/include/CosPropertyService.hrl").

%% Local
-include_lib("cosFileTransfer/include/CosFileTransfer.hrl").

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("============= CosFileTransfer =============~n"
		       Txt
		       "===========================================~n",
		       Arg)).

-define(FTP_PORT_INT,        21).
-define(FTP_PORT_STR,        "21").
-define(TCP_ID,              "TCP").
-define(FTP_ID,              "FTP").
-define(FTAM_ID,             "FTAM").
-define(NATIVE_ID,           "NATIVE").
-define(SUPPURTED_PROTOCOLS, ["TCP/IP", "SSL"]).

-define(DEFAULT_CONFIG, [{protocol, tcp}, {connect_timeout, 60}]).

-define(SEPARATOR,           "\r\n").

-define(DEFAULT_BUFSIZE, 64000).

-define(DEBUG_LEVEL, 3).

-ifdef(debug).
-define(debug_print(F,A),
        io:format("[LINE: ~p MODULE: ~p] "++F,[?LINE, ?MODULE]++A)).
-define(ft_TypeCheck(O,M), 'cosFileTransferApp':type_check(O,M)).
-else.
-define(debug_print(F,A), ok).
-define(ft_TypeCheck(O,I), ok).
-endif.    

%%--------------- END OF MODULE ------------------------------
