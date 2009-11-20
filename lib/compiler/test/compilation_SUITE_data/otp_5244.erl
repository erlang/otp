%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(otp_5244).
-export([?MODULE/0]).

?MODULE() ->
    L = [{stretch,0,0},
	 {bad,[]},
	 {bad,atom},
	 {bad,0},
	 {bad,16#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA},
	 {bad,16#555555555555555555555555555555555555555555555555555}],
    remove_failure(L, unit, 0).

remove_failure([], _Unit, _MaxFailure) ->
    ok;
remove_failure([{bad,Bad}|_], _Unit, _MaxFailure) ->
    Bad;
remove_failure([{stretch,_,Mi}=Stretch | Specs], Unit, _MaxFailure) ->
    {MinMax,NewMaxFailure} = max_failure(),
    case {MinMax,remove_failure(Specs, Unit, NewMaxFailure)} of
	{min,{NewMaxFailure,Rest}} ->
	    {done,[{fixed,Mi} | Rest]};
	{min,_} when Specs =/= [] ->
	    remove_failure([Stretch|tl(Specs)], Unit, NewMaxFailure);
	{min,_} ->
	    ok
    end.

max_failure() ->
    {min,1}.
