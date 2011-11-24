%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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

-ifndef(hdlt_logger_hrl).
-define(hdlt_logger_hrl, true).

%% Various log macros
-define(SET_LEVEL(N), hdlt_logger:set_level(N)).
-define(GET_LEVEL(),  hdlt_logger:get_level()).
-define(SET_NAME(N),  hdlt_logger:set_name(N)).

-define(INFO(F, A),   hdlt_logger:info(F, A)).
-define(LOG(F, A),    hdlt_logger:log(F, A)).
-define(DEBUG(F, A),  hdlt_logger:debug(F, A)).

-endif. % -ifdef(hdlt_logger_hrl).
