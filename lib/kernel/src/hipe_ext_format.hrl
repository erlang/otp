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

%% hipe_x86_ext_format.hrl
%% Definitions for unified external object format
%% Currently: sparc, x86, amd64
%% Authors: Erik Johansson, Ulf Magnusson

-define(LOAD_ATOM,0).
-define(LOAD_ADDRESS,1).
-define(CALL_REMOTE,2).
-define(CALL_LOCAL,3).
-define(SDESC,4).
-define(X86ABSPCREL,5).

-define(TERM,0).
-define(BLOCK,1).
-define(SORTEDBLOCK,2).

-define(CONST_TYPE2EXT(T),
	case T of
	    term -> ?TERM;
	    sorted_block -> ?SORTEDBLOCK;
            block -> ?BLOCK
        end).

-define(EXT2CONST_TYPE(E),
	case E of
	    ?TERM -> term;
	    ?SORTEDBLOCK -> sorted_block;
	    ?BLOCK -> block
	end).

-define(EXT2PATCH_TYPE(E),
	case E of
	    ?LOAD_ATOM -> load_atom;
	    ?LOAD_ADDRESS -> load_address;
	    ?SDESC -> sdesc;
	    ?X86ABSPCREL -> x86_abs_pcrel;
	    ?CALL_REMOTE -> call_remote;
	    ?CALL_LOCAL -> call_local
	end).

-define(STACK_DESC(ExnRA, FSize, Arity, Live), {ExnRA, FSize, Arity, Live}).
