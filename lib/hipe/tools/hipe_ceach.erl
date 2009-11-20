%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Module   :	hipe_ceach
%%  Purpose  :  Compile each function in a module, possibly applying a
%%              fun between each compilation. Useful for bug hunting by
%%		pinpointing a function that when compiled causes a bug.
%%  Notes    : 
%%  History  :	* 2001-12-11 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_ceach).

-export([c/1, c/2, c/3]).

-include("../main/hipe.hrl").

%%---------------------------------------------------------------------

-spec c(atom()) -> 'ok'.

c(M) ->
  lists:foreach(fun({F,A}) -> comp(M, F, A) end,
		M:module_info(functions)).

-spec c(atom(), comp_options()) -> 'ok'.

c(M, Opts) ->
  lists:foreach(fun({F,A}) -> comp(M, F, A, Opts) end,
		M:module_info(functions)).

-spec c(atom(), comp_options(), fun(() -> any())) -> 'ok'.

c(M, Opts, Fn) ->
  lists:foreach(fun({F,A}) -> comp(M, F, A, Opts), Fn() end,
		M:module_info(functions)).

-spec comp(atom(), atom(), arity()) -> 'ok'.

comp(M, F, A) ->
  io:format("~w:~w/~w... ", [M, F, A]),
  MFA = {M, F, A},
  {ok, MFA} = hipe:c(MFA),
  io:format("OK\n").

-spec comp(atom(), atom(), arity(), comp_options()) -> 'ok'.

comp(M, F, A, Opts) ->
  io:format("~w:~w/~w... ", [M, F, A]),
  MFA = {M, F, A},
  {ok, MFA} = hipe:c(MFA, Opts),
  io:format("OK\n").
