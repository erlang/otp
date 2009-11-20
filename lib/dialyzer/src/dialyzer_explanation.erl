%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_explanation.erl
%%% Author  : Elli Fragkaki <ellifrag@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------

-module(dialyzer_explanation).

-export([expl_loop/3]).

-include("dialyzer.hrl").

-spec expl_loop(pid(), dialyzer_codeserver:codeserver(), dialyzer_plt:plt()) ->
	no_return().

expl_loop(Parent, CServer, Plt) ->
  receive
    {Parent, warning, _Warning} ->
      send_explanation(Parent, none),
      expl_loop(Parent, CServer, Plt);
    {Parent, further, _Explanation} -> 
      Parent ! {self(), further, none},
      expl_loop(Parent, CServer, Plt);
    Other ->
      io:format("Unknown message: ~p\n", [Other]),
      expl_loop(Parent, CServer, Plt)
  end.

send_explanation(Parent, Expl) ->
  Parent ! {self(), explanation, Expl},
  ok.

