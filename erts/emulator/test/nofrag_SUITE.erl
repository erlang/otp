%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(nofrag_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
         error_handler/1,error_handler_apply/1,
         error_handler_fixed_apply/1,error_handler_fun/1,
         debug_breakpoint/1]).

%% Exported functions for an error_handler module.
-export([undefined_function/3,undefined_lambda/3,breakpoint/3]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() -> 
    [error_handler, error_handler_apply,
     error_handler_fixed_apply, error_handler_fun,
     debug_breakpoint].

error_handler(Config) when is_list(Config) ->
    process_flag(error_handler, ?MODULE),
    %% The term_to_binary/1 - binary_to_term/1 roundtrip is a good way
    %% to traverse the entire term.
    Term = collect(1024),
    Term = binary_to_term(term_to_binary(Term)),
    1024 = length(Term),
    [[a,b,c,d,[e,f,g]]] = lists:usort(Term),
    ok.

collect(0) ->
    [];
collect(N) ->
    %% The next line calls the error handle function, which is
    %% ?MODULE:undefined_function/3 (it simply returns the list
    %% of args, i.e. [a,b,c,d,[e,f,g]]).
    C = fooblurf:x(a, b, c, d, [e,f,id(g)]),

    %% The variable C will be saved onto the stack frame; if C
    %% points into a heap fragment the garbage collector will reach
    %% it and the emulator will crash sooner or later (sooner if
    %% the emulator is debug-compiled).
    Res = collect(N-1),
    [C|Res].

collect_apply(0, _) ->
    [];
collect_apply(N, Mod) ->
    %% The next line calls the error handle function, which is
    %% ?MODULE:undefined_function/3 (it simply returns the list
    %% of args).

    C = apply(Mod, xyz, id([{a,id(42)},b,c,d,[e,f,id(g)]])),

    %% The variable C will be saved onto the stack frame; if C
    %% points into a heap fragment the garbage collector will reach
    %% it and the emulator will crash sooner or later (sooner if
    %% the emulator is debug-compiled).
    Res = collect_apply(N-1, Mod),
    [C|Res].

error_handler_apply(Config) when is_list(Config) ->
    process_flag(error_handler, ?MODULE),

    %% The term_to_binary/1 - binary_to_term/1 roundtrip is a good way
    %% to traverse the entire term.
    Term = collect_apply(1024, fooblurfbar),
    Term = binary_to_term(term_to_binary(Term)),
    1024 = length(Term),
    [[{a,42},b,c,d,[e,f,g]]] = lists:usort(Term),
    ok.

error_handler_fixed_apply(Config) when is_list(Config) ->
    process_flag(error_handler, ?MODULE),

    %% The term_to_binary/1 - binary_to_term/1 roundtrip is a good way
    %% to traverse the entire term.
    Term = collect_fixed_apply(1024, fooblurfbar),
    Term = binary_to_term(term_to_binary(Term)),
    1024 = length(Term),
    [[{a,2},b,c,d,[e,f,g]]] = lists:usort(Term),
    ok.

collect_fixed_apply(0, _) ->
    [];
collect_fixed_apply(N, Mod) ->
    %% The next line calls the error handle function, which is
    %% ?MODULE:undefined_function/3 (it simply returns the list
    %% of args).
    C = Mod:x({a,id(2)}, b, c, d, [e,f,id(g)]),

    %% The variable C will be saved onto the stack frame; if C
    %% points into a heap fragment the garbage collector will reach
    %% it and the emulator will crash sooner or later (sooner if
    %% the emulator is debug-compiled).
    Res = collect_fixed_apply(N-1, Mod),
    [C|Res].

undefined_function(_Mod, _Name, Args) ->
    Args.

error_handler_fun(Config) when is_list(Config) ->
    process_flag(error_handler, ?MODULE),

    %% fun(A, B, C) -> {A,B,C,X} end in module foobarblurf.
    B = <<131,112,0,0,0,84,3,109,96,69,208,5,175,207,75,36,93,112,218,232,222,22,251,0,
          0,0,0,0,0,0,1,100,0,11,102,111,111,98,97,114,98,108,117,114,102,97,0,98,5,
          244,197,144,103,100,0,13,110,111,110,111,100,101,64,110,111,104,111,115,116,
          0,0,0,46,0,0,0,0,0,104,3,97,1,97,2,97,3>>,
    Fun = binary_to_term(B),
    Term = collect_fun(1024, Fun),
    Term = binary_to_term(term_to_binary(Term)),
    1024 = length(Term),
    [[{foo,bar},{99,1.0},[e,f,g]]] = lists:usort(Term),
    {env,[{1,2,3}]} = erlang:fun_info(Fun, env),
    ok.

collect_fun(0, _) ->
    [];
collect_fun(N, Fun) ->
    %% The next line calls the error handle function, which is
    %% ?MODULE:undefined_lambda/3 (it simply returns the list
    %% of args).
    C = Fun({foo,id(bar)}, {99,id(1.0)}, [e,f,id(g)]),

    %% The variable C will be saved onto the stack frame; if C
    %% points into a heap fragment the garbage collector will reach
    %% it and the emulator will crash sooner or later (sooner if
    %% the emulator is debug-compiled).
    Res = collect_fun(N-1, Fun),
    [C|Res].

undefined_lambda(foobarblurf, Fun, Args) when is_function(Fun) ->
    Args.

debug_breakpoint(Config) when is_list(Config) ->
    process_flag(error_handler, ?MODULE),
    erts_debug:breakpoint({?MODULE,foobar,5}, true),
    Term = break_collect(1024),
    Term = binary_to_term(term_to_binary(Term)),
    1024 = length(Term),
    [[a,b,c,{d,e},[f,g,h]]] = lists:usort(Term),
    erts_debug:breakpoint({?MODULE,foobar,5}, false),
    ok.

break_collect(0) ->
    [];
break_collect(N) ->
    C = foobar(a, b, c, {id(d),e}, [f,g,id(h)]),
    Res = break_collect(N-1),
    [C|Res].

breakpoint(?MODULE, foobar, Args) ->
    Args.

foobar(_, _, _, _, _) ->
    exit(dont_execute_me).

id(I) -> I.
