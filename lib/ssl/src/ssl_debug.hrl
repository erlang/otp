%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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


-ifndef(ssl_debug).
-define(ssl_debug, true).

-ifdef(SSL_DEBUG).
-define(DBG_HEX(V), ssl_debug:hex_data(??V, V, ?MODULE, ?LINE)).
-define(DBG_TERM(T), ssl_debug:term_data(??T, T, ?MODULE, ?LINE)).
-else.
-define(DBG_HEX(V), ok).
-define(DBG_TERM(T), ok).
-endif.

-endif. % -ifdef(ssl_debug).





