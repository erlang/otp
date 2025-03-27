%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
""".

-export([pi/0,tau/0]).

%%% BIFs

-export([sin/1, cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2, sinh/1,
         cosh/1, tanh/1, asinh/1, acosh/1, atanh/1, exp/1, log/1,
         log2/1, log10/1, pow/2, sqrt/1, erf/1, erfc/1,
         ceil/1, floor/1,
         fmod/2]).

-doc """
Returns the arc cosine of `X` in radians.

## Examples

```erlang
1> math:acos(1.0).
0.0
```
""".
-spec acos(X) -> float() when
      X :: number().
acos(_) ->
    erlang:nif_error(undef).

-doc """
Returns the inverse hyperbolic cosine of `X`.

## Examples

```erlang
1> math:acosh(1.0).
0.0
```
""".
-spec acosh(X) -> float() when
      X :: number().
acosh(_) ->
    erlang:nif_error(undef).

-doc """
Returns the arc cosine of `X` in radians.

## Examples

```erlang
1> math:asin(0.0).
0.0
```
""".
-spec asin(X) -> float() when
      X :: number().
asin(_) ->
    erlang:nif_error(undef).

-doc """
Returns the inverse hyperbolic sine of `X`.

## Examples

```erlang
1> math:asinh(0.0).
0.0
```
""".
-spec asinh(X) -> float() when
      X :: number().
asinh(_) ->
    erlang:nif_error(undef).

-doc """
Returns the arc tangent of `X` in radians.

## Examples

```erlang
1> math:atan(0.0).
0.0
```
""".
-spec atan(X) -> float() when
      X :: number().
atan(_) ->
    erlang:nif_error(undef).

-doc """
Returns the arc tangent of `Y`/`X` in radians, using the signs of both
arguments to determine the quadrant of the return value.

## Examples

```erlang
1> math:atan2(0.0, -10.0).
3.141592653589793
```
""".
-spec atan2(Y, X) -> float() when
      Y :: number(),
      X :: number().
atan2(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns the inverse hyperbolic tangent of `X`.

## Examples

```erlang
1> math:atanh(0.0).
0.0
```
""".
-spec atanh(X) -> float() when
      X :: number().
atanh(_) ->
    erlang:nif_error(undef).

-doc """
Returns the ceiling of `X`.

## Examples

```erlang
1> math:ceil(7.5).
8.0
2> math:ceil(-5.5).
-5.0
3> math:ceil(1.0).
1.0
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec ceil(X) -> float() when
      X :: number().
ceil(_) ->
    erlang:nif_error(undef).

-doc """
Returns the cosine of `X` in radians.

## Examples

```erlang
1> math:cos(0.0)
1.0
```
""".
-spec cos(X) -> float() when
      X :: number().
cos(_) ->
    erlang:nif_error(undef).

-doc """
Returns the hyperbolic cosine of `X`.

## Examples

```erlang
1> math:cosh(0.0)
1.0
```
""".
-spec cosh(X) -> float() when
      X :: number().
cosh(_) ->
    erlang:nif_error(undef).

-doc """
Returns the error function of `X`.

See [Error function](https://en.wikipedia.org/wiki/Error_function) (Wikipedia).

## Examples

```erlang
1> math:erf(0.0).
0.0
2> math:erf(10.0).
1.0
```
""".
-spec erf(X) -> float() when
      X :: number().
erf(_) ->
    erlang:nif_error(undef).

-doc """
Returns `1.0` - [`erf(X)`](`erf/1`), computed using methods
that avoid cancellation for large `X`.

## Examples

```erlang
1> math:erfc(0.0).
1.0
```
""".
-spec erfc(X) -> float() when
      X :: number().
erfc(_) ->
    erlang:nif_error(undef).

-doc """
Returns *e* raised to the power of `X`.

## Examples

```erlang
1> math:exp(0).
1.0
2> trunc(100 * math:exp(1)).
271
```
""".
-spec exp(X) -> float() when
      X :: number().
exp(_) ->
    erlang:nif_error(undef).

-doc """
Returns the floor of `X`.

## Examples

```erlang
1> math:floor(9.1).
9.0
2> math:floor(-1.5).
-2.0
3> math:floor(1.0)
1.0
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec floor(X) -> float() when
      X :: number().
floor(_) ->
    erlang:nif_error(undef).

-doc """
Returns the floating point remainder `X` divided by `Y`.

## Examples

```erlang
1> math:fmod(10.5, 8.0).
2.5
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec fmod(X, Y) -> float() when
      X :: number(), Y :: number().
fmod(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns the natural logarithm of `X`.

## Examples

```erlang
1> math:log(1.0).
0.0
2> math:log(2.718281828459045).
1.0
```
""".
-spec log(X) -> float() when
      X :: number().
log(_) ->
    erlang:nif_error(undef).

-doc """
Returns logarithm of `X` to base 2.

## Examples

```erlang
1> math:log2(1.0).
0.0
2> math:log2(2.0).
1.0
3> math:log2(64).
6.0
```
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec log2(X) -> float() when
      X :: number().
log2(_) ->
    erlang:nif_error(undef).

-doc """
Returns logarithm of `X` to base 10.

## Examples

```erlang
1> math:log10(1.0).
0.0
2> math:log10(10.0).
1.0
3> math:log10(100).
2.0
```
""".
-spec log10(X) -> float() when
      X :: number().
log10(_) ->
    erlang:nif_error(undef).

-doc """
Raise `X` to the power `N`.

## Examples

```erlang
1> math:pow(2, 6).
64.0
2> math:pow(10.0, 3.0).
1000.0
```
""".
-spec pow(X, N) -> float() when
      X :: number(),
      N :: number().
pow(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns the sine of `X` in radians.

## Examples

```erlang
1> math:sin(0.0)
0.0
```
""".
-spec sin(X) -> float() when
      X :: number().
sin(_) ->
    erlang:nif_error(undef).

-doc """
Returns the hyperbolic sine of `X`.

## Examples

```erlang
1> math:sinh(0.0)
0.0
```
""".
-spec sinh(X) -> float() when
      X :: number().
sinh(_) ->
    erlang:nif_error(undef).

-doc """
Returns the non-negative square root of `X`.

## Examples

```erlang
1> math:sqrt(2).
1.4142135623730951
2> math:sqrt(100.0).
10.0
```
""".
-spec sqrt(X) -> float() when
      X :: number().
sqrt(_) ->
    erlang:nif_error(undef).

-doc """
Returns the tangent of `X` in radians.

## Examples

```erlang
1> math:tan(0.0)
0.0
```
""".
-spec tan(X) -> float() when
      X :: number().
tan(_) ->
    erlang:nif_error(undef).

-doc """
Returns the hyperbolic tangent of `X`.

## Examples

```erlang
1> math:tan(0.0)
0.0
```
""".
-spec tanh(X) -> float() when
      X :: number().
tanh(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

-doc """
Returns the ratio of the circumference of a circle to its diameter.

## Examples

```erlang
1> math:pi().
3.141592653589793
```
""".
-spec pi() -> float().
pi() -> 3.1415926535897932.

-doc """
Returns the ratio of the circumference of a circle to its radius.

This constant is equivalent to a full turn when described in radians.

## Examples

```erlang
1> math:tau().
6.283185307179586
2> math:tau() == 2 * math:pi().
true
```
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec tau() -> float().
tau() -> 6.2831853071795864.
