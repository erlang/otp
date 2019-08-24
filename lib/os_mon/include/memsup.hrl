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
-ifndef(_memsup_hrl).
-define(_memsup_hrl,true).
%%% This file has to be kept consistent with ../c_src/memsup.h. 
%%% Keep consistence manually.

%% Defines

-define( SHOW_MEM , 1 ).
-define( SHOW_SYSTEM_MEM , 2 ).
-define( SHOW_SYSTEM_MEM_END , 8#0 ).
%% tags for extended statistics
-define( MEM_SYSTEM_TOTAL , 1 ).
-define( MEM_TOTAL , 2 ).
-define( MEM_FREE , 3 ).
-define( MEM_LARGEST_FREE , 4 ).
-define( MEM_NUMBER_OF_FREE , 5 ).
%% extensions 
-define( MEM_BUFFERS , 6 ).
-define( MEM_CACHED , 7 ).
-define( MEM_SHARED , 8 ).
-define( SWAP_TOTAL , 9 ).
-define( SWAP_FREE , 10 ).

-endif.

