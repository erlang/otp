%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-ifdef(snmp_debug).
-define(d(F,A),
	io:format("~p:~p:~p:" ++ F ++ "~n",[self(),?MODULE,?LINE]++A)).

%% Same as 'd' but without the ending newline ('~n').
-define(d_b(F,A),
	io:format("~p:~p:~p:" ++ F,[self(),?MODULE,?LINE]++A)).
%% To be used together with 'd_b'. Note: NO ending newline ('~n')..
-define(d_e(F,A),
	io:format(F,A)).
-else.
-define(d(F,A),ok).
-define(d_b(F,A),ok).
-define(d_e(F,A),ok).
-endif.




