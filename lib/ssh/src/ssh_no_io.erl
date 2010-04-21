%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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

%%% Description: ssh_io replacement that throws on everything

-module(ssh_no_io).

-export([yes_no/1, read_password/1, read_line/1, format/2]).

yes_no(_Prompt) ->
    throw({no_io_allowed, yes_no}).

read_password(_Prompt) ->
    throw({no_io_allowed, read_password}).

read_line(_Prompt) ->
    throw({no_io_allowed, read_line}).

format(_Fmt, _Args) ->
    throw({no_io_allowed, format}).
    
