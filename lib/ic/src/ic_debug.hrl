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
