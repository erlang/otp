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
%%
%%    Module documentation:
%%    ---------------------
%%
%%    Header file for the Erlang IDL compiler. Contains all records
%%    used in the parse tree
%%
%%
%%------------------------------------------------------------



%%------------------------------------------------------------

-record(module,		{id, body}).
-record(interface,	{id, inherit, body, inherit_body, tk}).
-record(forward,	{id, tk}).
-record(constr_forward,	{id, tk}).
-record(const,		{type, id, val, tk}).
-record(type_dcl,	{type, tk}).
-record(typedef,	{type, id, tk}).
-record(struct,		{id, body, tk}).
-record(member,		{type, id}).
-record(union,		{id, type, body, tk}).
-record(case_dcl,	{label, id, type}).
-record(enum,		{id, body, tk}).
-record(enumerator,	{id}).
-record(sequence,	{type, length=0}).
-record(string,		{length=0}).
-record(wstring,	{length=0}).      %% WSTRING
-record(array,		{id, size}).
-record(attr,		{readonly, type, id, tk}).
-record(except,		{id, body, tk}).
-record(op,		{oneway, type, id, params, raises, ctx, tk}).
-record(param,		{inout, type, id, tk}).
-record(fixed,		{digits, scale, value}).

%% NON-STANDARD
-record(preproc,	{cat, id, aux}).
-record(pragma,	        {type, to, apply}).










