%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(dbg_debugged).

%% External exports
-export([eval/3]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% eval(Mod, Func, Args) -> Value
%% Main entry point from external (non-interpreted) code.
%% Called via the error handler.
%%--------------------------------------------------------------------
eval(Mod, Func, Args) ->
    SaveStacktrace = erlang:get_stacktrace(),
    Meta = dbg_ieval:eval(Mod, Func, Args),
    Mref = erlang:monitor(process, Meta),
    msg_loop(Meta, Mref, SaveStacktrace).

%%====================================================================
%% Internal functions
%%====================================================================

msg_loop(Meta, Mref, SaveStacktrace) ->
    receive

	%% Evaluated function has returned a value
	{sys, Meta, {ready, Val}} ->
	    erlang:demonitor(Mref, [flush]),

	    %% Restore original stacktrace and return the value
	    try erlang:raise(throw, stack, SaveStacktrace)
	    catch
		throw:stack ->
		    case Val of
			{dbg_apply,M,F,A} ->
			    apply(M, F, A);
			_ ->
			    Val
		    end
	    end;

	%% Evaluated function raised an (uncaught) exception
	{sys, Meta, {exception,{Class,Reason,Stacktrace}}} ->
	    erlang:demonitor(Mref, [flush]),

	    %% ...raise the same exception
	    erlang:error(erlang:raise(Class, Reason, Stacktrace), 
			 [Class,Reason,Stacktrace]);

	%% Meta is evaluating a receive, must be done within context
	%% of real (=this) process
	{sys, Meta, {'receive',Msg}} ->
	    receive Msg ->
		Meta ! {self(), rec_acked},
		ok
	    end,
	    msg_loop(Meta, Mref, SaveStacktrace);

	%% Meta needs something evaluated within context of real process
	{sys, Meta, {command,Command}} ->
	    Reply = handle_command(Command),
	    Meta ! {sys, self(), Reply},
	    msg_loop(Meta, Mref, SaveStacktrace);

	%% Meta has terminated
	%% Must be due to int:stop() (or -heaven forbid- a debugger bug)
	{'DOWN', Mref, _, _, Reason} ->

	    %% Restore original stacktrace and return a dummy value
	    try erlang:raise(throw, stack, SaveStacktrace)
	    catch
		throw:stack ->
		    {interpreter_terminated, Reason}
	    end
    end.

handle_command(Command) ->
    try
	reply(Command)
    catch Class:Reason ->
	    Stacktrace = stacktrace_f(erlang:get_stacktrace()),
	    {exception,{Class,Reason,Stacktrace}}
    end.

reply({apply,M,F,As}) ->
    {value, erlang:apply(M,F,As)};
reply({eval,Expr,Bs}) ->
    %% Bindings is an orddict (sort them)
    erl_eval:expr(Expr, lists:sort(Bs)). % {value, Value, Bs2}

%% Fix stacktrace - keep all above call to this module.
%%
stacktrace_f([]) -> [];
stacktrace_f([{?MODULE,_,_,_}|_]) -> [];
stacktrace_f([F|S]) -> [F|stacktrace_f(S)].
