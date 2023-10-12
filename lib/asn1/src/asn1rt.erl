%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
%% This module was removed entirely in OTP 20.0, it's only retained for its
%% removal warning attribute.
%%

-module(asn1rt).

-removed({decode,'_',"use Mod:decode/2 instead"}).
-removed({encode,'_',"use Mod:encode/2 instead"}).

-removed({utf8_binary_to_list,'_',
         "use unicode:characters_to_list/1 instead"}).
-removed({utf8_list_to_binary,'_',
         "use unicode:characters_to_binary/1 instead"}).
