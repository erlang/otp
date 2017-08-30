%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
