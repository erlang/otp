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
-module(otp_2380).
-export([test/0, otp_2380/0]).

otp_2380() ->
    ok.

-define(FUNC(Name),
	case Name of
	    dpCh -> 5;
	    dpEvent -> 1;
	    dpc -> 4;
	    dpFm -> 6;
	    dpFm1 -> 6;
	    _ -> false
	end).

test() ->
    N = ?FUNC(dpCh).
