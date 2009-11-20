%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
	    demonitor(Mref),

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
	    demonitor(Mref),

	    %% ...raise the same exception
	    erlang:error(erlang:raise(Class, Reason, Stacktrace), 
			 [Class,Reason,Stacktrace]);

	%% Meta is evaluating a receive, must be done within context
	%% of real (=this) process
	{sys, Meta, {'receive',Msg}} ->
	    receive Msg -> Meta ! {self(), rec_acked} end,
	    msg_loop(Meta, Mref, SaveStacktrace);

	%% Meta needs something evaluated within context of real process
	{sys, Meta, {command, Command, Stacktrace}} ->
	    Reply = handle_command(Command, Stacktrace),
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

handle_command(Command, Stacktrace) ->
    try reply(Command)
    catch Class:Reason ->
	    Stacktrace2 = stacktrace_f(erlang:get_stacktrace()),
	    {exception, {Class,Reason,Stacktrace2++Stacktrace}}
    end.

reply({apply,M,F,As}) ->
    {value, erlang:apply(M,F,As)};
reply({eval,Expr,Bs}) ->
    erl_eval:expr(Expr, Bs). % {value, Value, Bs2}

%% Demonitor and delete message from inbox
%%
demonitor(Mref) ->
    erlang:demonitor(Mref),
    receive {'DOWN',Mref,_,_,_} -> ok
    after 0 -> ok
    end.

%% Fix stacktrace - keep all above call to this module.
%%
stacktrace_f([]) -> [];
stacktrace_f([{?MODULE,_,_}|_]) -> [];
stacktrace_f([F|S]) -> [F|stacktrace_f(S)].
