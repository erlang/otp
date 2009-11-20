%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
%% Description: This module uses the erlang shell and
%% ssh_cli to make an erlang sshd

-module(ssh_sshd).

%% API
-export([listen/0, listen/1, listen/2, listen/3, stop/1]).

-deprecated({listen, 0, next_major_release}).
-deprecated({listen, 1, next_major_release}).
-deprecated({listen, 2, next_major_release}).
-deprecated({listen, 3, next_major_release}).
-deprecated({stop, 1, next_major_release}).

listen() ->
    listen(22).

listen(Port) ->
    listen(Port, []).

listen(Port, Opts) ->
    listen(any, Port, Opts).

listen(Addr, Port, Opts) ->
    ssh:daemon(Addr, Port, Opts).

stop(Pid) ->
    ssh:stop_daemon(Pid).
