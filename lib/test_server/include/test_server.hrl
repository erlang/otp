%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

-ifdef(line_trace).
-line_trace(true).
-define(line,
	io:format(lists:concat([?MODULE,",",integer_to_list(?LINE),": ~p"]),
		  [erlang:monotonic_time()-erlang:system_info(start_time)]),).
-else.
-define(line,).
-endif.
-define(t,test_server).
-define(config,test_server:lookup_config).


