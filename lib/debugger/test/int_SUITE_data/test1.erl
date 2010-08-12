%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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
-module(test1).

-compile(export_all).


foo() ->
	A = [1,2,3],
	B = [a,b,c],
	C = [d,e,f],
	D = my_lists:append(A,B),
	E = my_lists:append(D,C),
	F = [1],
	G = my_lists:append(E,F),
	{A,B,C,D,E,F,G}.
