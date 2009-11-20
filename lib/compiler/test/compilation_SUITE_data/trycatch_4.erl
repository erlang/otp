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
-module(trycatch_4).
-export([trycatch_4/0]).
-record(state, {foo}).

trycatch_4() ->
    handle_info({foo}, #state{}),
    ok.

handle_info({_}, State) ->
   foo(),
   State#state{foo = bar},
   case ok of
   _ ->
     case catch foo() of
     ok ->
       {stop, State}
     end
   end;
handle_info(_, State) ->
   (catch begin
     foo(),
     State#state{foo = bar}
   end),
   case ok of
   _ ->
     case catch foo() of
     ok ->
       {stop, State}
     end
   end.

foo() -> ok.
