%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-moduledoc """
Mathematical functions.

This module provides an interface to a number of mathematical functions.

> #### Note {: .info }
>
> Not all functions are provided on all platforms. In particular, the `erf/1`
> and `erfc/1` functions are not provided on Windows.

## Limitations

As these are the C library, the same limitations apply.
""".

-export([pi/0,tau/0]).

%%% BIFs

-export([sin/1, cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2, sinh/1,
         cosh/1, tanh/1, asinh/1, acosh/1, atanh/1, exp/1, log/1,
         log2/1, log10/1, pow/2, sqrt/1, erf/1, erfc/1,
         ceil/1, floor/1,
         fmod/2]).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec acos(X) -> float() when
      X :: number().
acos(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec acosh(X) -> float() when
      X :: number().
acosh(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec asin(X) -> float() when
      X :: number().
asin(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec asinh(X) -> float() when
      X :: number().
asinh(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec atan(X) -> float() when
      X :: number().
atan(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec atan2(Y, X) -> float() when
      Y :: number(),
      X :: number().
atan2(_, _) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec atanh(X) -> float() when
      X :: number().
atanh(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec ceil(X) -> float() when
      X :: number().
ceil(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec cos(X) -> float() when
      X :: number().
cos(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec cosh(X) -> float() when
      X :: number().
cosh(_) ->
    erlang:nif_error(undef).

-doc """
Returns the error function of `X`, where:

```text
erf(X) = 2/sqrt(pi)*integral from 0 to X of exp(-t*t) dt.
```
""".
-spec erf(X) -> float() when
      X :: number().
erf(_) ->
    erlang:nif_error(undef).

-doc """
[`erfc(X)`](`erfc/1`) returns `1.0` \- [`erf(X)`](`erf/1`), computed by methods
that avoid cancellation for large `X`.
""".
-spec erfc(X) -> float() when
      X :: number().
erfc(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec exp(X) -> float() when
      X :: number().
exp(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec floor(X) -> float() when
      X :: number().
floor(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec fmod(X, Y) -> float() when
      X :: number(), Y :: number().
fmod(_, _) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec log(X) -> float() when
      X :: number().
log(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec log2(X) -> float() when
      X :: number().
log2(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec log10(X) -> float() when
      X :: number().
log10(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec pow(X, Y) -> float() when
      X :: number(),
      Y :: number().
pow(_, _) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec sin(X) -> float() when
      X :: number().
sin(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec sinh(X) -> float() when
      X :: number().
sinh(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec sqrt(X) -> float() when
      X :: number().
sqrt(_) ->
    erlang:nif_error(undef).

-doc(#{equiv => tanh/1}).
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec tan(X) -> float() when
      X :: number().
tan(_) ->
    erlang:nif_error(undef).

-doc """
A collection of mathematical functions that return floats. Arguments are
numbers.
""".
-doc(#{since => <<"OTP 18.0,OTP 20.0">>}).
-spec tanh(X) -> float() when
      X :: number().
tanh(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

-doc """
Ratio of the circumference of a circle to its diameter.

Floating point approximation of mathematical constant pi.
""".
-spec pi() -> float().
pi() -> 3.1415926535897932.

-doc """
Ratio of the circumference of a circle to its radius.

This constant is equivalent to a full turn when described in radians.

The same as `2 * pi()`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec tau() -> float().
tau() -> 6.2831853071795864.
