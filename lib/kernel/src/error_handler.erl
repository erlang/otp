%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%% FIXME: remove no_native directive after HiPE has been changed to make
%% remote calls link to the target's Export* like BEAM does.
%% For a detailed explanation see the commit titled
%% "error_handler: add no_native compiler directive"
-compile(no_native).

%% Callbacks called from the run-time system.
-export([undefined_function/3,undefined_lambda/3,breakpoint/3]).

%% Exported utility functions.
-export([raise_undef_exception/3]).
-export([stub_function/3]).

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

-spec breakpoint(Module :: atom(), Function :: atom(), Args :: [_]) ->
	any().

breakpoint(Module, Func, Args) ->
    (int()):eval(Module, Func, Args).

-spec raise_undef_exception(Module, Function, Args) -> no_return() when
      Module :: atom(),
      Function :: atom(),
      Args :: list().

raise_undef_exception(Module, Func, Args) ->
    crash({Module,Func,Args,[]}).

%% Used to make the call to the 'int' module a "weak" one, to avoid
%% building strong components in xref or dialyzer.

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
