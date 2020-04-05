%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-ifndef(socket_test_ttest_client).
-define(socket_test_ttest_client, true).

-define(MSG_ID_DEFAULT,  2).
-define(RUNTIME_DEFAULT, ?SECS(10)).
-define(MAX_ID,          16#FFFFFFFF).

-define(MSG_DATA1, <<"This is test data 0123456789 0123456789 0123456789">>).
-define(MSG_DATA2, <<"This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789">>).
-define(MSG_DATA3, <<"This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
		     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789"
                     "This is test data 0123456789 0123456789 0123456789">>).


-endif. % -ifdef(socket_test_ttest_client).
