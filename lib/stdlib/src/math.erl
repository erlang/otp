%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(math).

-export([pi/0]).

%%% BIFs

-export([sin/1, cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2, sinh/1,
         cosh/1, tanh/1, asinh/1, acosh/1, atanh/1, exp/1, log/1,
         log2/1, log10/1, pow/2, sqrt/1, erf/1, erfc/1]).

-spec acos(X) -> float() when
      X :: number().
acos(_) ->
    erlang:nif_error(undef).

-spec acosh(X) -> float() when
      X :: number().
acosh(_) ->
    erlang:nif_error(undef).

-spec asin(X) -> float() when
      X :: number().
asin(_) ->
    erlang:nif_error(undef).

-spec asinh(X) -> float() when
      X :: number().
asinh(_) ->
    erlang:nif_error(undef).

-spec atan(X) -> float() when
      X :: number().
atan(_) ->
    erlang:nif_error(undef).

-spec atan2(Y, X) -> float() when
      Y :: number(),
      X :: number().
atan2(_, _) ->
    erlang:nif_error(undef).

-spec atanh(X) -> float() when
      X :: number().
atanh(_) ->
    erlang:nif_error(undef).

-spec cos(X) -> float() when
      X :: number().
cos(_) ->
    erlang:nif_error(undef).

-spec cosh(X) -> float() when
      X :: number().
cosh(_) ->
    erlang:nif_error(undef).

-spec erf(X) -> float() when
      X :: number().
erf(_) ->
    erlang:nif_error(undef).

-spec erfc(X) -> float() when
      X :: number().
erfc(_) ->
    erlang:nif_error(undef).

-spec exp(X) -> float() when
      X :: number().
exp(_) ->
    erlang:nif_error(undef).

-spec log(X) -> float() when
      X :: number().
log(_) ->
    erlang:nif_error(undef).

-spec log2(X) -> float() when
      X :: number().
log2(_) ->
    erlang:nif_error(undef).

-spec log10(X) -> float() when
      X :: number().
log10(_) ->
    erlang:nif_error(undef).

-spec pow(X, Y) -> float() when
      X :: number(),
      Y :: number().
pow(_, _) ->
    erlang:nif_error(undef).

-spec sin(X) -> float() when
      X :: number().
sin(_) ->
    erlang:nif_error(undef).

-spec sinh(X) -> float() when
      X :: number().
sinh(_) ->
    erlang:nif_error(undef).

-spec sqrt(X) -> float() when
      X :: number().
sqrt(_) ->
    erlang:nif_error(undef).

-spec tan(X) -> float() when
      X :: number().
tan(_) ->
    erlang:nif_error(undef).

-spec tanh(X) -> float() when
      X :: number().
tanh(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

-spec pi() -> float().

pi() -> 3.1415926535897932.
