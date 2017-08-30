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

-module(tftp_test_lib).

-compile(export_all).

-include("tftp_test_lib.hrl").

%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    io:format("\n ", []),
    ?IGNORE(application:stop(inets)),   
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    ?IGNORE(application:stop(inets)),   
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Infrastructure for test suite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(Actual, Mod, Line) ->
    (catch global:send(tftp_global_logger, {failed, Mod, Line})),
    log("<ERROR> Bad result: ~p\n", [Actual], Mod, Line),
    Label = lists:concat([Mod, "(", Line, ") unexpected result"]),
    et:report_event(60, Mod, Mod, Label,
			[{line, Mod, Line}, {error, Actual}]),
    case global:whereis_name(tftp_test_case_sup) of
	undefined -> 
	    ignore;
	Pid -> 
	    Fail = #'REASON'{mod = Mod, line = Line, desc = Actual},
	    Pid ! {fail, self(), Fail}
    end,
    Actual.

log(Format, Args, Mod, Line) ->
    case global:whereis_name(tftp_global_logger) of
	undefined ->
	    io:format(user, "~p(~p): " ++ Format, 
		      [Mod, Line] ++ Args);
	Pid ->
	    io:format(Pid, "~p(~p): " ++ Format, 
		      [Mod, Line] ++ Args)
    end.

default_config() ->
    [].

t() -> 
    t([{?MODULE, all}]).

t(Cases) ->
    t(Cases, default_config()).

t(Cases, Config) ->
    process_flag(trap_exit, true),
    Res = lists:flatten(do_test(Cases, Config)),
    io:format("Res: ~p\n", [Res]),
    display_result(Res),
    Res.

do_test({Mod, Fun}, Config) when is_atom(Mod), is_atom(Fun) ->
    case catch apply(Mod, Fun, [suite]) of
	[] ->
	    io:format("Eval:   ~p:", [{Mod, Fun}]),
	    Res = eval(Mod, Fun, Config),
	    {R, _, _} = Res,
	    io:format(" ~p\n", [R]),
	    Res;

	Cases when is_list(Cases) ->
	    io:format("Expand: ~p ...\n", [{Mod, Fun}]),
	    Map = fun(Case) when is_atom(Case)-> {Mod, Case};
		     (Case) -> Case
		  end,
	    do_test(lists:map(Map, Cases), Config);

        {req, _, {conf, Init, Cases, Finish}} ->
	    case (catch apply(Mod, Init, [Config])) of
		Conf when is_list(Conf) ->
		    io:format("Expand: ~p ...\n", [{Mod, Fun}]),
		    Map = fun(Case) when is_atom(Case)-> {Mod, Case};
			     (Case) -> Case
			  end,
		    Res = do_test(lists:map(Map, Cases), Conf),
		    (catch apply(Mod, Finish, [Conf])),
		    Res;
		    
		{'EXIT', {skipped, Reason}} ->
		    io:format(" => skipping: ~p\n", [Reason]),
		    [{skipped, {Mod, Fun}, Reason}];
		    
		Error ->
		    io:format(" => failed: ~p\n", [Error]),
		    [{failed, {Mod, Fun}, Error}]
	    end;
		    
        {'EXIT', {undef, _}} ->
	    io:format("Undefined:   ~p\n", [{Mod, Fun}]),
	    [{nyi, {Mod, Fun}, ok}];
		    
        Error ->
	    io:format("Ignoring:   ~p: ~p\n", [{Mod, Fun}, Error]),
	    [{failed, {Mod, Fun}, Error}]
    end;
do_test(Mod, Config) when is_atom(Mod) ->
    Res = do_test({Mod, all}, Config),
    Res;
do_test(Cases, Config) when is_list(Cases) ->
    [do_test(Case, Config) || Case <- Cases];
do_test(Bad, _Config) ->
    [{badarg, Bad, ok}].

eval(Mod, Fun, Config) ->
    TestCase = {?MODULE, Mod, Fun},
    Label = lists:concat(["TEST CASE: ", Fun]),
    et:report_event(40, ?MODULE, Mod, Label ++ " started",
			[TestCase, Config]),
    global:register_name(tftp_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    Config2 = Mod:init_per_testcase(Fun, Config),
    Pid = spawn_link(?MODULE, do_eval, [self(), Mod, Fun, Config2]),
    R = wait_for_evaluator(Pid, Mod, Fun, Config2, []),
    Mod:end_per_testcase(Fun, Config2),
    global:unregister_name(tftp_test_case_sup),
    process_flag(trap_exit, Flag),
    R.

wait_for_evaluator(Pid, Mod, Fun, Config, Errors) ->
    TestCase = {?MODULE, Mod, Fun},
    Label = lists:concat(["TEST CASE: ", Fun]),
    receive
	{done, Pid, ok} when Errors == [] ->
	    et:report_event(40, Mod, ?MODULE, Label ++ " ok",
				[TestCase, Config]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, {ok, _}} when Errors == [] ->
	    et:report_event(40, Mod, ?MODULE, Label ++ " ok",
				[TestCase, Config]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, Fail} ->
	    et:report_event(20, Mod, ?MODULE, Label ++ " failed",
				[TestCase, Config, {return, Fail}, Errors]),
	    {failed, {Mod,Fun}, Fail};
	{'EXIT', Pid, {skipped, Reason}} -> 
	    et:report_event(20, Mod, ?MODULE, Label ++ " skipped",
				[TestCase, Config, {skipped, Reason}]),
	    {skipped, {Mod, Fun}, Errors};
	{'EXIT', Pid, Reason} -> 
	    et:report_event(20, Mod, ?MODULE, Label ++ " crashed",
				[TestCase, Config, {'EXIT', Reason}]),
	    {crashed, {Mod, Fun}, [{'EXIT', Reason} | Errors]};
	{fail, Pid, Reason} ->
	    wait_for_evaluator(Pid, Mod, Fun, Config, Errors ++ [Reason])
    end.

do_eval(ReplyTo, Mod, Fun, Config) ->
    case (catch apply(Mod, Fun, [Config])) of
	{'EXIT', {skipped, Reason}} ->
	    ReplyTo ! {'EXIT', self(), {skipped, Reason}};
	Other ->
	    ReplyTo ! {done, self(), Other}
    end,
    unlink(ReplyTo),
    exit(shutdown).

display_result([]) ->    
    io:format("OK\n", []);
display_result(Res) when is_list(Res) ->
    Ok      = [MF || {ok, MF, _}  <- Res],
    Nyi     = [MF || {nyi, MF, _} <- Res],
    Skipped = [{MF, Reason} || {skipped, MF, Reason} <- Res],
    Failed  = [{MF, Reason} || {failed, MF, Reason} <- Res],
    Crashed = [{MF, Reason} || {crashed, MF, Reason} <- Res],
    display_summary(Ok, Nyi, Skipped, Failed, Crashed),
    display_skipped(Skipped),
    display_failed(Failed),
    display_crashed(Crashed).

display_summary(Ok, Nyi, Skipped, Failed, Crashed) ->
    io:format("\nTest case summary:\n", []),
    display_summary(Ok,      "successful"),
    display_summary(Nyi,     "not yet implemented"),
    display_summary(Skipped, "skipped"),
    display_summary(Failed,  "failed"),
    display_summary(Crashed, "crashed"),
    io:format("\n", []).
   
display_summary(Res, Info) ->
    io:format("  ~w test cases ~s\n", [length(Res), Info]).
    
display_skipped([]) ->
    ok;
display_skipped(Skipped) ->
    io:format("Skipped test cases:\n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p\n", [MF, Reason]) end,
    lists:foreach(F, Skipped),
    io:format("\n", []).
    

display_failed([]) ->
    ok;
display_failed(Failed) ->
    io:format("Failed test cases:\n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p\n", [MF, Reason]) end,
    lists:foreach(F, Failed),
    io:format("\n", []).

display_crashed([]) ->
    ok;
display_crashed(Crashed) ->
    io:format("Crashed test cases:\n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p\n", [MF, Reason]) end,
    lists:foreach(F, Crashed),
    io:format("\n", []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generic callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(generic_state, {state, prepare, open, read, write, abort}).

prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    State   = lookup_option(state,   mandatory, Initial),
    Prepare = lookup_option(prepare, mandatory, Initial),
    Open    = lookup_option(open,    mandatory, Initial),
    Read    = lookup_option(read,    mandatory, Initial),
    Write   = lookup_option(write,   mandatory, Initial),
    Abort   = lookup_option(abort,   mandatory, Initial),
    case Prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, State) of
	{ok, AcceptedOptions, NewState} ->
	    {ok,
	     AcceptedOptions,
	     #generic_state{state   = NewState,
			    prepare = Prepare,
			    open    = Open,
			    read    = Read,
			    write   = Write,
			    abort   = Abort}};
	Other ->
	    Other
    end.

open(Peer, Access, LocalFilename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    case prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, Initial) of
 	{ok, SuggestedOptions2, GenericState} ->
	    open(Peer, Access, LocalFilename, Mode, SuggestedOptions2, GenericState);
	Other ->
	    Other
    end;
open(Peer, Access, LocalFilename, Mode, SuggestedOptions, #generic_state{state = State, open = Open} = GenericState) ->
    case Open(Peer, Access, LocalFilename, Mode, SuggestedOptions, State) of
	{ok, SuggestedOptions2, NewState} ->
	    {ok, SuggestedOptions2, GenericState#generic_state{state = NewState}};
	Other ->
	    Other
    end.

read(#generic_state{state = State, read = Read} = GenericState) ->
    case Read(State) of
	{more, DataBlock, NewState} ->
	    {more, DataBlock, GenericState#generic_state{state = NewState}};
	Other ->
	    Other
    end.

write(DataBlock, #generic_state{state = State, write = Write} = GenericState) ->
    case Write(DataBlock, State) of
	{more, NewState} ->
	    {more, GenericState#generic_state{state = NewState}};
	Other ->
	    Other
    end.

abort(Code, Text, #generic_state{state = State, abort = Abort}) ->
    Abort(Code, Text, State).

lookup_option(Key, Default, Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.

