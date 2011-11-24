%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(os_mon_conf).
-export([init/1,fin/1]).

init(Conf) ->
    RetVal = application:start(os_mon,temporary),
    [{os_mon,RetVal}|Conf].

fin(Conf) ->
    application:stop(os_mon),
    lists:keydelete(os_mon,1,Conf).
