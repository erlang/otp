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
-module(complex_guard).

-compile(export_all).

?MODULE() ->
    ok.

f(X1,Y1,Z1) ->
    if
	((X1 =:= 4) or (X1 =:= 5)) and ((Y1 =:= 4) or (Y1 =:= 5)) and ((Z1 =:= 4) or (Z1 =:= 5)) or ((X1 =:= 1) or (X1 =:= 2) or (X1 =:= 3)) and ((Y1 =:= 1) or (Y1 =:= 2) or (Y1 =:= 3)) and ((Z1 =:= 1) or (Z1 =:= 2) or (Z1 =:= 3)) ->
	    true
    end.

