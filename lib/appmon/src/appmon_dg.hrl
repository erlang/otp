%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
%% The dg package record definitions
%%
%% This is the declarations of the datastructures used for the
%% application monitoring software. All fields are reserved for the
%% implementation except those stated otherwise
%%

-record(vdata, {ref,				%
		type,				%
		x,				%
		origx=-1,			%
		y,				%
		origy=-1,			%
		txt="",				% Set by user
		width=0,			% Set by user
		sym_obj=undefined,		%
		txt_obj}).			%



-record(edata, {ref,				%
		line,				%
		weight}).			%

