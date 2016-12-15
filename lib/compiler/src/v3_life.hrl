%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% This record contains variable life-time annotation for a
%% kernel expression.  Added by v3_life, used by v3_codegen.

-type vdb_entry() :: {atom(),non_neg_integer(),non_neg_integer()}.

-record(l, {ke,					%Kernel expression
	    i=0 :: non_neg_integer(),           %Op number
	    vdb=[] :: [vdb_entry()],            %Variable database
	    a=[] :: [term()]}).                 %Core annotation

