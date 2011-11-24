%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2011. All Rights Reserved.
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

 
% Default timetrap timeout (set in init_per_testcase).
% This should be set relatively high (10-15 times the expected
% max testcasetime).
-define(default_timeout, ?t:minutes(10)).

-define(RDBMS, case os:type() of
		   {unix, sunos} ->
		       mysql;
		   {unix,linux} ->
		       case  erlang:system_info({wordsize, external}) of
			   4 ->
			       mysql;
			   _ ->
			       postgres
		       end;
		   {unix, darwin} ->
		       mysql;
		   {win32, _} ->
		       sqlserver
	       end).

-define(TIMEOUT, 100000).


