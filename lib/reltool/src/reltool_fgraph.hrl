%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
