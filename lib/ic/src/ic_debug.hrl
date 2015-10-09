%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%% Debug macro
%%----------------------------------------------------------------------
-ifndef(ic_debug_hrl).
-define(ic_debug_hrl, true).

-ifdef(debug).
    -define(PRINTDEBUG(Msg),
            io:format("~p :~p ~p~n", [Msg, ?FILE, ?LINE])).
    -define(PRINTDEBUG2(F, A),
            io:format(F ++ ":~p ~p~n", A ++ [?FILE, ?LINE])).
-else.
    -define(PRINTDEBUG(Msg), ok).
    -define(PRINTDEBUG2(F, A), ok).
-endif.    

-endif.
