<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Retired Myths

We believe that the truth finally has caught with the following, retired myths.

[](){: #retired_myths }

## Myth: Funs are Slow

Funs used to be very slow, slower than [`apply/3`](`apply/3`). Originally, funs
were implemented using nothing more than compiler trickery, ordinary tuples,
[`apply/3`](`apply/3`), and a great deal of ingenuity.

But that is history. Funs was given its own data type in R6B and was further
optimized in R7B. Now the cost for a fun call falls roughly between the cost for
a call to a local function and [`apply/3`](`apply/3`).

## Myth: List Comprehensions are Slow

List comprehensions used to be implemented using funs, and in the old days funs
were indeed slow.

Nowadays, the compiler rewrites list comprehensions into an ordinary recursive
function. Using a tail-recursive function with a reverse at the end would be
still faster. Or would it? That leads us to the myth that tail-recursive
functions are faster than body-recursive functions.

## Myth: List subtraction ("--" operator) is slow

List subtraction used to have a run-time complexity proportional to the product
of the length of its operands, so it was extremely slow when both lists were
long.

As of OTP 22 the run-time complexity is "n log n" and the operation will
complete quickly even when both lists are very long. In fact, it is faster and
uses less memory than the commonly used workaround to convert both lists to
ordered sets before subtracting them with `ordsets:subtract/2`.
