%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
-module(dbg_istk).
-export([init/0,delayed_to_external/0,from_external/1,
	 push/3,pop/0,pop/1,stack_level/0,
	 delayed_stacktrace/0,delayed_stacktrace/2,
	 bindings/1,stack_frame/2,backtrace/2,
	 in_use_p/2]).

-include("dbg_ieval.hrl").

-define(STACK, ?MODULE).

-record(e,
	{level,					%Level
	 mfa,					%{Mod,Func,Args|Arity}|{Fun,Args}
	 line,					%Line called from
	 bindings,
	 lc					%Last call (true|false)
	 }).

init() ->
    init([]).

delayed_to_external() ->
    Stack = get(?STACK),
    fun() -> {stack,term_to_binary(Stack)} end.

from_external({stack,Stk}) ->
    put(?STACK, binary_to_term(Stk)).

init(Stack) ->
    put(?STACK, Stack).

%% We keep track of a call stack that is used for
%%  1) saving stack frames that can be inspected from an Attached
%%     Process GUI (using dbg_icmd:get(Meta, stack_frame, {Dir, SP})
%%  2) generate an approximation of regular stacktrace -- sent to
%%     Debugged when it should raise an exception or evaluate a
%%     function (since it might possible raise an exception)
%%
%% How to push depends on the "Stack Trace" option (value saved in
%% process dictionary item 'trace_stack').
%%   all - everything is pushed
%%   no_tail - tail recursive push
%%   false - nothing is pushed
%% Whenever a function returns, the corresponding call frame is popped.

push(Bs, #ieval{level=Le,module=Mod,function=Name,
		arguments=As,line=Li}=Ieval, Lc) ->
    Entry = #e{level=Le,mfa={Mod,Name,As},line=Li,bindings=Bs,lc=Lc},
    case get(trace_stack) of
	false ->
	    Ieval#ieval{level=Le+1};
	no_tail when Lc ->
	    Ieval;
	_ -> % all | no_tail when Lc =:= false
	    put(?STACK, [Entry|get(?STACK)]),
	    Ieval#ieval{level=Le+1}
    end.

pop() ->
    case get(trace_stack) of
	false -> ignore;
	_ -> % all | no_tail
	    case get(?STACK) of
		[_Entry|Entries] ->
		    put(?STACK, Entries);
		[] ->
		    ignore
	    end
    end.

pop(Le) ->
    case get(trace_stack) of
	false -> ignore;
	_ -> % all | no_tail
	    put(?STACK, pop(Le, get(?STACK)))
    end.

pop(Level, [#e{level=Le}|Stack]) when Level =< Le ->
    pop(Level, Stack);
pop(_Level, Stack) ->
    Stack.

%% stack_level() -> Le
%% stack_level(Stack) -> Le
%% Top call level
stack_level() ->
    stack_level(get(?STACK)).

stack_level([]) -> 1;
stack_level([#e{level=Le}|_]) -> Le.

%% delayed_stacktrace() -> CreateStacktraceFun
%% delayed_stacktrace(ArgFlag, #ieval{}) -> CreateStacktraceFun
%%   ArgFlag = no_args | include_args
%%   CreateStacktraceFun = fun(NumberOfEntries)
%%
%% Return a fun that can convert the internal stack format to
%% an imitation of the regular stacktrace.

delayed_stacktrace() ->
    Stack0 = get(?STACK),
    fun(NumEntries) ->
	    Stack = stacktrace(NumEntries, Stack0, []),
	    [finalize(ArityOnly) || {ArityOnly,_} <- Stack]
    end.

delayed_stacktrace(include_args, Ieval) ->
    #ieval{module=Mod,function=Name,arguments=As,line=Li} = Ieval,
    Stack0 = [#e{mfa={Mod,Name,As},line=Li}|get(?STACK)],
    fun(NumEntries) ->
	    case stacktrace(NumEntries, Stack0, []) of
		[] ->
		    [];
		[{_,WithArgs}|Stack] ->
		    [finalize(WithArgs) |
		     [finalize(ArityOnly) || {ArityOnly,_} <- Stack]]
	    end
    end;
delayed_stacktrace(no_args, Ieval) ->
    #ieval{module=Mod,function=Name,arguments=As,line=Li} = Ieval,
    Stack0 = [#e{mfa={Mod,Name,As},line=Li}|get(?STACK)],
    fun(NumEntries) ->
	    Stack = stacktrace(NumEntries, Stack0, []),
	    [finalize(ArityOnly) || {ArityOnly,_} <- Stack]
    end.

stacktrace(N, [#e{lc=true}|T], Acc) ->
    stacktrace(N, T, Acc);
stacktrace(N, [E|T], []) ->
    stacktrace(N-1, T, [normalize(E)]);
stacktrace(N, [E|T], [{P,_}|_]=Acc) when N > 0 ->
    case normalize(E) of
	{P,_} ->
	    stacktrace(N, T, Acc);
	New ->
	    stacktrace(N-1, T, [New|Acc])
    end;
stacktrace(_, _, Acc) ->
    lists:reverse(Acc).

normalize(#e{mfa={M,Fun,As},line=Li}) when is_function(Fun) ->
    Loc = {M,Li},
    {{Fun,length(As),Loc},{Fun,As,Loc}};
normalize(#e{mfa={M,F,As},line=Li}) ->
    Loc = {M,Li},
    {{M,F,length(As),Loc},{M,F,As,Loc}}.

finalize({M,F,A,Loc}) -> {M,F,A,line(Loc)};
finalize({Fun,A,Loc}) -> {Fun,A,line(Loc)}.

line({Mod,Line}) when Line > 0 ->
    [{file,atom_to_list(Mod)++".erl"},{line,Line}];
line(_) -> [].

%% bindings(SP) -> Bs
%%   SP = Le  % stack pointer
%% Return the bindings for the specified call level
bindings(SP) ->
    bindings(SP, get(?STACK)).

bindings(SP, [#e{level=SP,bindings=Bs}|_]) ->
    Bs;
bindings(SP, [_Entry|Entries]) ->
    bindings(SP, Entries);
bindings(_SP, []) ->
    erl_eval:new_bindings().

%% stack_frame(Dir, SP) -> {Le, Where, Bs} | top | bottom
%%   Dir = up | down
%%   Where = {Cm, Li}
%%     Cm = Module | undefined  % module
%%     Li = int()  | -1         % line number
%%     Bs = bindings()
%% Return stack frame info one step up/down from given stack pointer
%%  up = to lower call levels
%%  down = to higher call levels
stack_frame(up, SP) ->
    stack_frame(SP, up, get(?STACK));
stack_frame(down, SP) ->
    stack_frame(SP, down, lists:reverse(get(?STACK))).

stack_frame(SP, up, [#e{level=Le,mfa={Cm,_,_},line=Li,bindings=Bs}|_])
  when Le < SP ->
    {Le,{Cm,Li},Bs};
stack_frame(SP, down, [#e{level=Le,mfa={Cm,_,_},line=Li,bindings=Bs}|_])
  when Le > SP ->
    {Le,{Cm,Li},Bs};
stack_frame(SP, Dir, [#e{level=SP}|Stack]) ->
    case Stack of
	[#e{level=Le,mfa={Cm,_,_},line=Li,bindings=Bs}|_] ->
	    {Le,{Cm,Li},Bs};
	[] when Dir =:= up ->
	    top;
	[] when Dir =:= down ->
	    bottom
    end;
stack_frame(SP, Dir, [_Entry|Stack]) ->
    stack_frame(SP, Dir, Stack).

%% backtrace(HowMany) -> Backtrace
%%   HowMany = all | int()
%%   Backtrace = {Le, MFA}
%% Return all/the last N called functions, in reversed call order
backtrace(HowMany, Ieval) ->
    #ieval{level=Level,module=Mod,function=Name,arguments=As} = Ieval,
    Stack0 = [#e{level=Level,mfa={Mod,Name,As}}|get(?STACK)],
    Stack = case HowMany of
		all -> Stack0;
		N -> lists:sublist(Stack0, N)
	    end,
    [{Le,MFA} || #e{level=Le,mfa=MFA} <- Stack].

%%--------------------------------------------------------------------
%% in_use_p(Mod, Cm) -> boolean()
%%   Mod = Cm = atom()
%% Returns true if Mod is found on the stack, otherwise false.
%%--------------------------------------------------------------------
in_use_p(Mod, Mod) -> true;
in_use_p(Mod, _Cm) ->
    case get(trace_stack) of
	false -> true;
	_ -> %  all | no_tail
	    lists:any(fun(#e{mfa={M,_,_}}) when M =:= Mod -> true;
			 (_) -> false
		      end, get(?STACK))
    end.
