%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-module(otp_4790).

-export([?MODULE/0]).

?MODULE() ->
    pan_test().


% --------------------------- OTP Ticket --------------------------------
% *Id: 		OTP-4790
% *Notes:	In the code below, the compiler incorrectly assumes
% 		wings_pref:get_value(pan_speed) returns a float,
% 		causing a crash at run-time.

% 		The same error could cause tuple tests to be removed,
% 		but that would propbably only cause a crash if the
% 		Erlang code was incorrect or if it depended on a catch
% 		to catch exceptions. Therefore, I consider it unlikely
% 		that Erlang programs that don't use floating point
% 		arithmetic are likely to be bitten by this bug.
% -----------------------------------------------------------------------

-record(view, {pan_x,pan_y,distance}).

pan_test() ->
    pan(13, 3).

pan(Dx0, Dy0) ->
    #view{pan_x=PanX0,pan_y=PanY0,distance=D} = View = current(),
    S = D*(1/8)/(51-pref_get_value(pan_speed)),
    Dx = Dx0*S,
    Dy = Dy0*S,
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    set_current(View#view{pan_x=PanX,pan_y=PanY}).

current() ->
    #view{pan_x=2.0,pan_y=9.75,distance=25.3}.

set_current(#view{pan_x=X,pan_y=Y,distance=D})
  when is_float(X), is_float(Y), is_float(D) ->
    io:format("X=~p Y=~p D=~p\n", [X,Y,D]).

pref_get_value(pan_speed) ->
    32.
