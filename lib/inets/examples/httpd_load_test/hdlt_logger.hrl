%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-ifndef(hdlt_logger_hrl).
-define(hdlt_logger_hrl, true).

%% Various log macros
-define(SET_LEVEL(N), hdlt_logger:set_level(N)).
-define(GET_LEVEL(),  hdlt_logger:get_level()).
-define(SET_NAME(N),  hdlt_logger:set_name(N)).

-define(INFO(F, A),   hdlt_logger:info(F, A)).
-define(LOG(F, A),    hdlt_logger:log(F, A)).
-define(DEBUG(F, A),  hdlt_logger:debug(F, A)).

-endif. % -ifdef(hdlt_logger_hrl).
