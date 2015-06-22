%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(complex_guard).

-compile(export_all).

?MODULE() ->
    ok.

f(X1,Y1,Z1) ->
    if
	((X1 =:= 4) or (X1 =:= 5)) and ((Y1 =:= 4) or (Y1 =:= 5)) and ((Z1 =:= 4) or (Z1 =:= 5)) or ((X1 =:= 1) or (X1 =:= 2) or (X1 =:= 3)) and ((Y1 =:= 1) or (Y1 =:= 2) or (Y1 =:= 3)) and ((Z1 =:= 1) or (Z1 =:= 2) or (Z1 =:= 3)) ->
	    true
    end.

