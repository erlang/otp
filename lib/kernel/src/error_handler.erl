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
-module(error_handler).
-moduledoc """
Default system error handler.

This module defines what happens when certain types of errors occur.

You can change the error handler of a process by calling
[`erlang:process_flag(error_handler, NewErrorHandler)`](`erlang#process_flag_error_handler`).

## Notes

The code in `error_handler` is complex. Do not change it without fully
understanding the interaction between the error handler, the `init` process of
the code server, and the I/O mechanism of the code.

Code changes that seem small can cause a deadlock, as unforeseen consequences
can occur. The use of `input` is dangerous in this type of code.
""".

%% See the comment before the int/0 function for an explanation
%% why this option is needed.
-compile(no_module_opt).

%% Callbacks called from the run-time system.
-export([undefined_function/3,undefined_lambda/3,breakpoint/3]).

%% Exported utility functions.
-export([raise_undef_exception/3]).
-export([stub_function/3]).

-doc """
This function is called by the runtime system if a call is made to
`Module:Function(Arg1,.., ArgN)` and `Module:Function/N` is undefined. Notice
that this function is evaluated inside the process making the original call.

This function first attempts to autoload `Module`. If that is not possible, an
`undef` exception is raised.

If it is possible to load `Module` and function `Function/N` is exported, it is
called.

Otherwise, if function `'$handle_undefined_function'/2` is exported, it is
called as `'$handle_undefined_function'(`Function, Args).

> #### Warning {: .warning }
>
> Defining `'$handle_undefined_function'/2` in ordinary application code is
> highly discouraged. It is very easy to make subtle errors that can take a long
> time to debug. Furthermore, none of the tools for static code analysis (such
> as Dialyzer and Xref) supports the use of `'$handle_undefined_function'/2` and
> no such support will be added. Only use this function after having carefully
> considered other, less dangerous, solutions. One example of potential
> legitimate use is creating stubs for other sub-systems during testing and
> debugging.

Otherwise an `undef` exception is raised.
""".
-spec undefined_function(Module, Function, Args) ->
	any() when
      Module :: atom(),
      Function :: atom(),
      Args :: list().

undefined_function(Module, Func, Args) ->
    case ensure_loaded(Module) of
	{module, Module} ->
	    case erlang:function_exported(Module, Func, length(Args)) of
		true ->
		    apply(Module, Func, Args);
		false ->
		    call_undefined_function_handler(Module, Func, Args)
	    end;
	{module, _} ->
	    crash(Module, Func, Args);
	_Other ->
	    crash(Module, Func, Args)
    end.

-doc """
This function is evaluated if a call is made to `Fun(Arg1,.., ArgN)` when the
module defining the fun is not loaded. The function is evaluated inside the
process making the original call.

If `Module` is interpreted, the interpreter is invoked and the return value of
the interpreted `Fun(Arg1,.., ArgN)` call is returned.

Otherwise, it returns, if possible, the value of [`apply(Fun, Args)`](`apply/2`)
after an attempt is made to autoload `Module`. If this is not possible, the call
fails with exit reason `undef`.
""".
-spec undefined_lambda(Module, Fun, Args) -> term() when
      Module :: atom(),
      Fun :: fun(),
      Args :: list().

undefined_lambda(Module, Fun, Args) ->
    case ensure_loaded(Module) of
	{module, Module} ->
	    %% There is no need (and no way) to test if the fun is present.
	    %% apply/2 will not call us again if the fun is missing.
	    apply(Fun, Args);
	{module, _} ->
	    crash(Fun, Args);
	_Other ->
	    crash(Fun, Args)
    end.

-doc false.
-spec breakpoint(Module :: atom(), Function :: atom(), Args :: [_]) ->
	any().

breakpoint(Module, Func, Args) ->
    (int()):eval(Module, Func, Args).

-doc """
Raises an `undef` exception with a stacktrace, indicating that
`Module:Function/N` is undefined.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec raise_undef_exception(Module, Function, Args) -> no_return() when
      Module :: atom(),
      Function :: atom(),
      Args :: list().

raise_undef_exception(Module, Func, Args) ->
    crash({Module,Func,Args,[]}).

%% Used to make the call to the 'int' module a "weak" one, to avoid
%% making Kernel a visible dependency to Debugger in xref. (To ensure
%% that the call in breakpoint/3 is kept as an apply to an unknown
%% module, this module must be compiled with the 'no_module_opt'
%% option to turn off inter-function type analysis.)

int() -> int.

%%
%% Crash providing a beautiful stack backtrace.
%%
-spec crash(atom(), [term()]) -> no_return().

crash(Fun, Args) ->
    crash({Fun,Args,[]}).

-spec crash(atom(), atom(), arity() | [term()]) -> no_return().

crash(M, F, A) ->
    crash({M,F,A,[]}).

-spec crash(tuple()) -> no_return().

crash(Tuple) ->
    try erlang:error(undef)
    catch
	error:undef:Stacktrace ->
	    Stk = [Tuple|tl(Stacktrace)],
	    erlang:raise(error, undef, Stk)
    end.

%% If the code_server has not been started yet dynamic code loading
%% is handled by init.
ensure_loaded(Module) ->
    Self = self(),
    case whereis(code_server) of
	%% Perhaps double fault should be detected in code:ensure_loaded/1 
	%% instead, since this error handler cannot know whether the 
	%% code server can resolve the problem or not.
	%% An {error, Reason} return from there would crash the code server and 
	%% bring down the node.
	Self ->
	    Error = "The code server called the unloaded module `" ++
		atom_to_list(Module) ++ "'",
	    halt(Error);
	Pid when is_pid(Pid) ->
	    code:ensure_loaded(Module);
	_ ->
	    init:ensure_loaded(Module)
    end.

-doc false.
-spec stub_function(atom(), atom(), [_]) -> no_return().

stub_function(Mod, Func, Args) ->
    exit({undef,[{Mod,Func,Args,[]}]}).

call_undefined_function_handler(Module, Func, Args) ->
    Handler = '$handle_undefined_function',
    case erlang:function_exported(Module, Handler, 2) of
	false ->
	    crash(Module, Func, Args);
	true ->
	    Module:Handler(Func, Args)
    end.
