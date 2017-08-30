%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(simple).

-export([test/0]).

-ifdef(need_foo).
-export([foo/0]).
-endif.

test() ->
    passed.

%% Conditional inclusion.
%% Compile with [{d, need_foo}, {d, foo_value, 42}].

-ifdef(need_foo).
-include("simple.hrl").

foo() ->
    {?included_value, ?foo_value}.

-endif.

-ifdef(include_generated).
-include("generated.hrl").
-endif.
