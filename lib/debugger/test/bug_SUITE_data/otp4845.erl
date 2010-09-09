%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
%%% Purpose : Test OTP-4845

-module(otp4845).


%%-export([error/1]).
-export([test/0]).

%% OTP-4845 OTP 4859
-record(sune, {a,sd,g,s}).
-record(error, {a,sd,g,s}).

test() ->
    R1 = error(#sune{}),
    R2 = error(false),
    R3 = error(true),
    R4 = error(#error{}),
    {R1,R2,R3,R4}.

error(X) ->
    if
	is_record(X, sune) ->
	    sune;
	X ->
	    {true, X};
	not X ->
	    {false, X};
	not is_record(X, error) ->
	    error;
	true ->
	    ok
    end.
