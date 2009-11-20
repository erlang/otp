%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-define(fg_th, (0.25)).
-define(fg_damp, (0.75)).
-define(fg_kc, (1000.0)).
-define(fg_stretch, (0.005)).
-define(fg_grav, (9.82)).

%% Ke = 8.854187817e9 [N x M^2 x C^(-2)]
-define(fg_wind, (0.15)).

-record(fg_e,
	{
	  l = 10.0,
	  k = 10.0
	 }).
	
-record(fg_v,
	{
	  p = {0.0,0.0},
	  v = {0.0,0.0},
	  q = 5.0,
	  m = 1.0,
	  type = dynamic,
	  color = default,
	  resides = undefined,
	  selected = false
	 }).
