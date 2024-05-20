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
# Pattern Matching

## Pattern Matching

Variables are bound to values through the _pattern matching_ mechanism. Pattern
matching occurs when evaluating the `case`, `receive`, `try`, and
the match operator (`=`) expressions.

In pattern matching, a left-hand side [pattern](expressions.md#patterns) is
matched against a right-hand side [term](expressions.md#terms). If the matching
succeeds, any unbound variables in the pattern become bound. If the matching
fails, an exception is raised.

_Examples:_

```erlang
1> X.
** 1:1: variable 'X' is unbound **
2> X = 2.
2
3> X + 1.
3
4> {X, Y} = {1, 2}.
** exception error: no match of right hand side value {1,2}
5> {X, Y} = {2, 3}.
{2,3}
6> Y.
3
```
