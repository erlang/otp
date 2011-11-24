%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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










