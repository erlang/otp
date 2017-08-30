%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : orber_ifr.hrl
%% Purpose : Macros for the Interface Repository
%%----------------------------------------------------------------------


-record(lightdata, {scope, id}).
-record(orber_light_ifr_ref, {data}).

%% "Type" checking
-define(tcheck(Type, Thing), when Type == Thing ; Thing == orber_light_ifr_ref).

-define(DEBUG_LEVEL, 9).

