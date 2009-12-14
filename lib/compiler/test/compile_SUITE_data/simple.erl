%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
