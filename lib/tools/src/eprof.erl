%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% Purpose: Profile a system in order to figure out where the 
%% time goes.
%%

-module(eprof).
-behaviour(gen_server).

-export([start/0,
	 stop/0,
	 dump/0,
	 start_profiling/1, start_profiling/2, start_profiling/3,
	 profile/1, profile/2, profile/3, profile/4, profile/5,
	 stop_profiling/0,
	 analyze/0, analyze/1, analyze/2,
	 log/1]).

%% Internal exports 
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-record(bpd, {
	n   = 0,                 % number of total calls
	us  = 0,                 % sum of uS for all calls
	p   = gb_trees:empty(),  % tree of {Pid, {Mfa, {Count, Us}}}
	mfa = []                 % list of {Mfa, {Count, Us}}
    }).

-define(default_options, [{set_on_spawn, true}]).
-define(default_pattern, {'_','_','_'}).

-record(state, {
	profiling  = false,
	pattern    = ?default_pattern,
	rootset    = [],
	trace_opts = [],
	fd         = undefined,
	start_ts   = undefined,
	reply      = undefined,
	bpd        = #bpd{}
    }).



%% -------------------------------------------------------------------- %%
%%
%% API
%%
%% -------------------------------------------------------------------- %%

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop, infinity).

analyze() ->
    analyze(procs).

analyze(Type) when is_atom(Type) ->
    analyze(Type, []);
analyze(Opts) when is_list(Opts) ->
    analyze(procs, Opts).
analyze(Type, Opts) when is_list(Opts) ->
    gen_server:call(?MODULE, {analyze, Type, Opts}, infinity).

%% odd duck, should only been start_profiling/1
profile(Rootset) when is_list(Rootset) ->
    start_profiling(Rootset);

profile(Fun) when is_function(Fun) ->
    profile([], Fun).

profile(Fun, Opts) when is_function(Fun), is_list(Opts) ->
    profile([], erlang, apply, [Fun, []], ?default_pattern, Opts);

profile(Rootset, Fun) when is_list(Rootset), is_function(Fun) ->
    profile(Rootset, Fun, ?default_pattern).

profile(Rootset, Fun, Pattern) when is_list(Rootset), is_function(Fun) ->
    profile(Rootset, Fun, Pattern, ?default_options).

profile(Rootset, Fun, Pattern, Options) when is_list(Rootset), is_function(Fun), is_list(Options) ->
    profile(Rootset, erlang, apply, [Fun,[]], Pattern, Options);

profile(Rootset, M, F, A) when is_list(Rootset), is_atom(M), is_atom(F), is_list(A) ->
    profile(Rootset, M, F, A, ?default_pattern).

profile(Rootset, M, F, A, Pattern) when is_list(Rootset), is_atom(M), is_atom(F), is_list(A) ->
    profile(Rootset, M, F, A, Pattern, ?default_options).

%% Returns when M:F/A has terminated
profile(Rootset, M, F, A, Pattern, Options) ->
    ok = start_internal(),
    gen_server:call(?MODULE, {profile_start, Rootset, Pattern, {M,F,A}, Options}, infinity).

dump() -> 
    gen_server:call(?MODULE, dump, infinity).

log(File) ->
    gen_server:call(?MODULE, {logfile, File}, infinity).

%% Does not block
start_profiling(Rootset) ->
    start_profiling(Rootset, ?default_pattern).
start_profiling(Rootset, Pattern) ->
    start_profiling(Rootset, Pattern, ?default_options).
start_profiling(Rootset, Pattern, Options) ->
    ok = start_internal(),
    gen_server:call(?MODULE, {profile_start, Rootset, Pattern, undefined, Options}, infinity).

stop_profiling() ->
    gen_server:call(?MODULE, profile_stop, infinity).


%% -------------------------------------------------------------------- %%
%%
%% init
%%
%% -------------------------------------------------------------------- %%

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%% -------------------------------------------------------------------- %%
%%
%% handle_call
%%
%% -------------------------------------------------------------------- %%

%% analyze

handle_call({analyze, _, _}, _, #state{ bpd = #bpd{ p = {0,nil}, us = 0, n = 0} = Bpd } = S) when is_record(Bpd, bpd) ->
    {reply, nothing_to_analyze, S};

handle_call({analyze, procs, Opts}, _, #state{ bpd = #bpd{ p = Ps, us = Tus} = Bpd, fd = Fd} = S) when is_record(Bpd, bpd) ->
    lists:foreach(fun
	    ({Pid, Mfas}) ->
		{Pn, Pus} =  sum_bp_total_n_us(Mfas),
		format(Fd, "~n****** Process ~w    -- ~s % of profiled time *** ~n", [Pid, s("~.2f", [100.0*divide(Pus,Tus)])]),
		print_bp_mfa(Mfas, {Pn,Pus}, Fd, Opts),
		ok
	end, gb_trees:to_list(Ps)),
    {reply, ok, S};

handle_call({analyze, total, Opts}, _, #state{ bpd = #bpd{ mfa = Mfas, n = Tn, us = Tus} = Bpd, fd = Fd} = S) when is_record(Bpd, bpd) ->
    print_bp_mfa(Mfas, {Tn, Tus}, Fd, Opts),
    {reply, ok, S};

handle_call({analyze, Type, _Opts}, _, S) ->
    {reply, {error, {undefined, Type}}, S};

%% profile

handle_call({profile_start, _Rootset, _Pattern, _MFA, _Opts}, _From, #state{ profiling = true } = S) ->
    {reply, {error, already_profiling}, S};

handle_call({profile_start, Rootset, Pattern, {M,F,A}, Opts}, From, #state{fd = Fd } = S) ->

    ok = set_pattern_trace(false, S#state.pattern),
    _  = set_process_trace(false, S#state.rootset, S#state.trace_opts),

    Topts = get_trace_options(Opts),
    Pid   = setup_profiling(M,F,A),

    case set_process_trace(true, [Pid|Rootset], Topts) of
	true ->
	    ok = set_pattern_trace(true, Pattern),
	    T0 = erlang:timestamp(),
	    ok = execute_profiling(Pid),
	    {noreply, #state{
		    profiling  = true,
		    rootset    = [Pid|Rootset],
		    start_ts   = T0,
		    reply      = From,
		    fd         = Fd,
		    trace_opts = Topts,
		    pattern    = Pattern
		}};
	false ->
	    exit(Pid, eprof_kill),
	    {reply, error, #state{ fd = Fd}}
    end;

handle_call({profile_start, Rootset, Pattern, undefined, Opts}, From, #state{ fd = Fd } = S) ->

    ok    = set_pattern_trace(false, S#state.pattern),
    true  = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    Topts = get_trace_options(Opts),

    case set_process_trace(true, Rootset, Topts) of
	true ->
	    T0 = erlang:timestamp(),
	    ok = set_pattern_trace(true, Pattern),
	    {reply, profiling, #state{
		    profiling  = true,
		    rootset    = Rootset,
		    start_ts   = T0,
		    reply      = From,
		    fd         = Fd,
		    trace_opts = Topts,
		    pattern    = Pattern
		}};
	false ->
	    {reply, error, #state{ fd = Fd }}
    end;

handle_call(profile_stop, _From, #state{ profiling = false } = S) ->
    {reply, profiling_already_stopped, S};

handle_call(profile_stop, _From, #state{ profiling = true } = S) ->

    ok  = set_pattern_trace(pause, S#state.pattern),
    Bpd = collect_bpd(),
    _   = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    ok  = set_pattern_trace(false, S#state.pattern),

    {reply, profiling_stopped, S#state{
	profiling  = false,
	rootset    = [],
	trace_opts = [],
	pattern    = ?default_pattern,
	bpd        = Bpd
    }};

%% logfile
handle_call({logfile, File}, _From, #state{ fd = OldFd } = S) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    case OldFd of
		undefined -> ok;
		OldFd -> ok = file:close(OldFd)
	    end,
	    {reply, ok, S#state{fd = Fd}};
	Error ->
	    {reply, Error, S}
    end;

handle_call(dump, _From, #state{ bpd = Bpd } = S) when is_record(Bpd, bpd) ->
    {reply, gb_trees:to_list(Bpd#bpd.p), S};

handle_call(stop, _FromTag, S) ->
    {stop, normal, stopped, S}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

handle_cast(_Msg, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_info
%%
%% -------------------------------------------------------------------- %%

handle_info({'EXIT', _, normal}, S) ->
    {noreply, S};
handle_info({'EXIT', _, eprof_kill}, S) ->
    {noreply, S};
handle_info({'EXIT', _, Reason}, #state{ reply = FromTag } = S) ->

    _  = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    ok = set_pattern_trace(false, S#state.pattern),

    gen_server:reply(FromTag, {error, Reason}),
    {noreply, S#state{
	profiling  = false,
	rootset    = [],
	trace_opts = [],
	pattern    = ?default_pattern
    }};

% check if Pid is spawned process?
handle_info({_Pid, {answer, Result}}, #state{ reply = {From,_} = FromTag} = S) ->

    ok   = set_pattern_trace(pause, S#state.pattern),
    Bpd  = collect_bpd(),
    _    = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    ok   = set_pattern_trace(false, S#state.pattern),

    catch unlink(From),
    gen_server:reply(FromTag, {ok, Result}),
    {noreply, S#state{
	profiling  = false,
	rootset    = [],
	trace_opts = [],
	pattern    = ?default_pattern,
	bpd        = Bpd
    }}.

%% -------------------------------------------------------------------- %%
%%
%% termination
%%
%% -------------------------------------------------------------------- %%

terminate(_Reason, #state{ fd = undefined }) ->
    ok = set_pattern_trace(false, ?default_pattern),
    ok;
terminate(_Reason, #state{ fd = Fd }) ->
    ok = file:close(Fd),
    ok = set_pattern_trace(false, ?default_pattern),
    ok.

%% -------------------------------------------------------------------- %%
%%
%% code_change
%%
%% -------------------------------------------------------------------- %%

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% -------------------------------------------------------------------- %%
%%
%% AUX Functions
%%
%% -------------------------------------------------------------------- %%

setup_profiling(M,F,A) ->
    spawn_link(fun() -> spin_profile(M,F,A) end).

spin_profile(M, F, A) ->
    receive
	{Pid, execute} ->
	    Pid ! {self(), {answer, erlang:apply(M,F,A)}}
    end.

execute_profiling(Pid) ->
    Pid ! {self(), execute},
    ok.


get_trace_options([]) ->
    [call];
get_trace_options([{set_on_spawn, true}|Opts]) ->
    [set_on_spawn | get_trace_options(Opts)];
get_trace_options([set_on_spawn|Opts]) ->
    [set_on_spawn | get_trace_options(Opts)];
get_trace_options([_|Opts]) ->
    get_trace_options(Opts).


set_pattern_trace(Flag, Pattern) ->
    erlang:system_flag(multi_scheduling, block),
    erlang:trace_pattern(on_load, Flag, [call_time]),
    erlang:trace_pattern(Pattern, Flag, [call_time]),
    erlang:system_flag(multi_scheduling, unblock),
    ok.

set_process_trace(_, [], _) -> true;
set_process_trace(Flag, [Pid|Pids], Options) when is_pid(Pid) ->
    try
	erlang:trace(Pid, Flag, Options),
	set_process_trace(Flag, Pids, Options)
    catch
	_:_ ->
	    false
    end;
set_process_trace(Flag, [Name|Pids], Options) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    set_process_trace(Flag, Pids, Options);
	Pid ->
	    set_process_trace(Flag, [Pid|Pids], Options)
    end.

collect_bpd() ->
    collect_bpd([M || M <- [element(1, Mi) || Mi <- code:all_loaded()], M =/= ?MODULE]).

collect_bpd(Ms) when is_list(Ms) ->
    collect_bpdf(collect_mfas(Ms) ++ erlang:system_info(snifs)).

collect_mfas(Ms) ->
    lists:foldl(fun
	    (M, Mfas) ->
		Mfas ++ [{M, F, A} || {F, A} <- M:module_info(functions)]
	end, [], Ms).

collect_bpdf(Mfas) ->
    collect_bpdf(Mfas, #bpd{}).
collect_bpdf([], Bpd) ->
    Bpd;
collect_bpdf([Mfa|Mfas], #bpd{n = N, us = Us, p = Tree, mfa = Code } = Bpd) ->
    case erlang:trace_info(Mfa, call_time) of
	{call_time, []} ->
	    collect_bpdf(Mfas, Bpd);
	{call_time, Data} when is_list(Data) ->
	    {CTn, CTus, CTree} = collect_bpdfp(Mfa, Tree, Data),
	    collect_bpdf(Mfas, Bpd#bpd{
		    n   = CTn  + N,
		    us  = CTus + Us,
		    p   = CTree,
		    mfa = [{Mfa, {CTn, CTus}}|Code]
		});
	{call_time, false} ->
	    collect_bpdf(Mfas, Bpd);
	{call_time, _Other} ->
	    collect_bpdf(Mfas, Bpd)
    end.

collect_bpdfp(Mfa, Tree, Data) ->
     lists:foldl(fun
	({Pid, Ni, Si, Usi}, {PTno, PTuso, To}) ->
	    Time = Si * 1000000 + Usi,
	    Ti1  = case gb_trees:lookup(Pid, To) of
		none ->
		    gb_trees:enter(Pid, [{Mfa, {Ni, Time}}], To);
		{value, Pmfas} ->
		    gb_trees:enter(Pid, [{Mfa, {Ni, Time}}|Pmfas], To)
	    end,
	    {PTno + Ni, PTuso + Time, Ti1}
    end, {0,0, Tree}, Data).

%% manipulators
sort_mfa(Bpfs, mfa) when is_list(Bpfs) ->
    lists:sort(fun
	    ({A,_}, {B,_}) when A < B -> true;
	    (_, _) -> false
	end, Bpfs);
sort_mfa(Bpfs, time) when is_list(Bpfs) ->
    lists:sort(fun
	    ({_,{_,A}}, {_,{_,B}}) when A < B -> true;
	    (_, _) -> false
	end, Bpfs);
sort_mfa(Bpfs, calls) when is_list(Bpfs) ->
    lists:sort(fun
	    ({_,{A,_}}, {_,{B,_}}) when A < B -> true;
	    (_, _) -> false
	end, Bpfs);
sort_mfa(Bpfs, _) when is_list(Bpfs) -> sort_mfa(Bpfs, time).

filter_mfa(Bpfs, Ts) when is_list(Ts) ->
    filter_mfa(Bpfs, [], proplists:get_value(calls, Ts, 0), proplists:get_value(time, Ts, 0));
filter_mfa(Bpfs, _) -> Bpfs.
filter_mfa([], Out, _, _) -> lists:reverse(Out);
filter_mfa([{_, {C, T}}=Bpf|Bpfs], Out, Ct, Tt) when C >= Ct, T >= Tt -> filter_mfa(Bpfs, [Bpf|Out], Ct, Tt);
filter_mfa([_|Bpfs], Out, Ct, Tt) -> filter_mfa(Bpfs, Out, Ct, Tt).

sum_bp_total_n_us(Mfas) ->
    lists:foldl(fun ({_, {Ci,Usi}}, {Co, Uso}) -> {Co + Ci, Uso + Usi} end, {0,0}, Mfas).

%% strings and format

string_bp_mfa(Mfas, Tus) -> string_bp_mfa(Mfas, Tus, {0,0,0,0,0}, []).
string_bp_mfa([], _, Ws, Strings) -> {Ws, lists:reverse(Strings)};
string_bp_mfa([{Mfa, {Count, Time}}|Mfas], Tus, {MfaW, CountW, PercW, TimeW, TpCW}, Strings) ->
	Smfa   = s(Mfa),
	Scount = s(Count),
	Stime  = s(Time),
	Sperc  = s("~.2f", [100*divide(Time,Tus)]),
	Stpc   = s("~.2f", [divide(Time,Count)]),

	string_bp_mfa(Mfas, Tus, {
		erlang:max(MfaW,  length(Smfa)),
		erlang:max(CountW,length(Scount)),
		erlang:max(PercW, length(Sperc)),
		erlang:max(TimeW, length(Stime)),
		erlang:max(TpCW,  length(Stpc))
	    }, [[Smfa, Scount, Sperc, Stime, Stpc] | Strings]).

print_bp_mfa(Mfas, {Tn, Tus}, Fd, Opts) ->
    Fmfas = filter_mfa(sort_mfa(Mfas, proplists:get_value(sort, Opts)), proplists:get_value(filter, Opts)),
    {{MfaW, CountW, PercW, TimeW, TpCW}, Strs} = string_bp_mfa(Fmfas, Tus),
    TnStr    = s(Tn),
    TusStr   = s(Tus),
    TuspcStr = s("~.2f", [divide(Tus,Tn)]),
    Ws = {erlang:max(length("FUNCTION"), MfaW),
          lists:max([length("CALLS"), CountW, length(TnStr)]),
          erlang:max(length("      %"), PercW),
          lists:max([length("TIME"), TimeW, length(TusStr)]),
          lists:max([length("uS / CALLS"), TpCW, length(TuspcStr)])},
    format(Fd, Ws, ["FUNCTION", "CALLS", "      %", "TIME", "uS / CALLS"]),
    format(Fd, Ws, ["--------", "-----", "-------", "----", "----------"]),
    lists:foreach(fun (String) -> format(Fd, Ws, String) end, Strs),
    format(Fd, Ws, [lists:duplicate(N,$-)||N <- tuple_to_list(Ws)]),
    format(Fd, Ws, ["Total:", TnStr, "100.00%", TusStr, TuspcStr]),
    ok.

s({M,F,A}) -> s("~w:~w/~w",[M,F,A]);
s(Term) -> s("~p", [Term]).
s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).


format(Fd, {MfaW, CountW, PercW, TimeW, TpCW}, Strings) ->
    format(Fd, s("~~.~ps  ~~~ps  ~~~ps  ~~~ps  [~~~ps]~~n", [MfaW, CountW, PercW, TimeW, TpCW]), Strings);
format(undefined, Format, Strings) ->
    io:format(Format, Strings),
    ok;
format(Fd, Format, Strings) ->
    io:format(Fd, Format, Strings),
    io:format(Format, Strings),
    ok.

divide(_,0) -> 0.0;
divide(T,N) -> T/N.

start_internal() ->
    case start() of
        {ok, _} -> ok;
        {error, {already_started,_}} -> ok;
        Error -> Error
    end.
