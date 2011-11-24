%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(error_handler).
%% FIXME: remove no_native directive after HiPE has been changed to make
%% remote calls link to the target's Export* like BEAM does.
%% For a detailed explanation see the commit titled
%% "error_handler: add no_native compiler directive"
-compile(no_native).

%% A simple error handler.

-export([undefined_function/3, undefined_lambda/3, stub_function/3,
	 breakpoint/3]).

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
		    case check_inheritance(Module, Args) of
			{value, Base, Args1} ->
			    apply(Base, Func, Args1);
			none ->
			    crash(Module, Func, Args)
		    end
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

%% Used to make the call to the 'int' module a "weak" one, to avoid
%% building strong components in xref or dialyzer.

int() -> int.

%%
%% Crash providing a beautiful stack backtrace.
%%
-spec crash(atom(), [term()]) -> no_return().

crash(Fun, Args) ->
    crash({Fun,Args,[]}).

-spec crash(atom(), atom(), arity()) -> no_return().

crash(M, F, A) ->
    crash({M,F,A,[]}).

-spec crash(tuple()) -> no_return().

crash(Tuple) ->
    try erlang:error(undef)
    catch
	error:undef ->
	    Stk = [Tuple|tl(erlang:get_stacktrace())],
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

check_inheritance(Module, Args) ->
    Attrs = erlang:get_module_info(Module, attributes),
    case lists:keyfind(extends, 1, Attrs) of
	{extends, [Base]} when is_atom(Base), Base =/= Module ->
	    %% This is just a heuristic for detecting abstract modules
	    %% with inheritance so they can be handled; it would be
	    %% much better to do it in the emulator runtime
	    case lists:keyfind(abstract, 1, Attrs) of
		{abstract, [true]} ->
		    case lists:reverse(Args) of
			[M|Rs] when tuple_size(M) > 1,
			element(1,M) =:= Module,
			tuple_size(element(2,M)) > 0,
			is_atom(element(1,element(2,M))) ->
			    {value, Base, lists:reverse(Rs, [element(2,M)])};
			_ ->
			    {value, Base, Args}
		    end;
		_ ->
		    {value, Base, Args}
	    end;
	_ ->
	    none
    end.
