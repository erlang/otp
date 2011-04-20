%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @author  Bartłomiej Puzoń <bartlomiej.puzon@erlang-solutions.com>
%% @doc <p>This module together with {@module example_config_gen} is an example
%% of a configuration back-end layer for higher-level tools which want to make 
%% use of ttb. The {@module example_server} acts as a middle man between any 
%% user-interface and the wrapper, and offers a possibility to store and load
%% configurations to and from files. </p>
%% <p>For a full example of how these two modules can be used, please refer to 
%% the {@module example_cli} module.
%% </p>
%%% Version: 1.0

%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met: 
%%% * Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution. 
%%% * Neither the name of the Erlang Training & Consulting nor the names of its
%%% contributors may be used to endorse or promote products
%%% derived from this software without specific prior written permission.
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(example_server).
-behaviour(gen_server).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').

%% Api towards onviso (all take own ref as argument)
-export([run_trace/1, run_trace/2,
         stop_trace/0,
         merge_trace/2, merge_trace/3]).

%% Api for managing trace cases
-export([add_trace_case/1,
         get_trace_cases/0,
         get_trace_case/1,
         delete_trace_case/1,
         save_to_file/1,
         read_from_file/1]).

%% Gen server API
-export([start_link/0,
         stop/0,
         init/1,
         handle_call/3,
         handle_cast/2, 
         handle_info/2, 
         code_change/3,
         terminate/2]).

-include("example_server.hrl").

-define(SERVER, ?MODULE).

-record(state, {trace_cases,
                running = undefined}).

-type merge_fail() :: {error,
		       no_such_trace}.

%%------------------------------------------------------------------------------
%% user control api
%%------------------------------------------------------------------------------

%% @doc Run one of the stored trace cases.
%% @end
-spec run_trace(integer()) -> ok.    
run_trace(Ref) ->
    run_trace(Ref, "").

%% @doc Run one of the stored trace cases, providing a comment.
%% @end
-spec run_trace(integer(), string()) -> ok.    
run_trace(Ref, Comment) ->
    case get_trace_case(Ref) of
        #trace_case{} = Case ->
            gen_server:call(?SERVER, {run_trace, Case, Comment});
        false ->
            {error, no_trace}
    end.

%% @doc Stop a trace
%% @end
-spec stop_trace() -> ok.
stop_trace() ->       
    gen_server:call(?SERVER, stop_trace).

%% @doc Merge the most recent trace run of the given trace case.
%% @end
-spec merge_trace(integer(), integer()) -> ok | merge_fail().
merge_trace(Ref, MergeNum) ->
    merge_trace(Ref, 1, MergeNum).

%% @doc Equivalent to merge_trace(Ref, RunNum, MergeNum, standard_io)
%% @end
-spec merge_trace(integer(), integer(), integer()) -> ok | merge_fail().
merge_trace(Ref, RunNum, MergeNum) ->
    merge_trace(Ref, RunNum, MergeNum, standard_io).

%% @doc Merge a stopped or running trace case.
%% If it is still running, then when 
%% this function is called, the trace will be stopped automatically. 
%% Out is the file to store merge output to (filename or standard_io).
%% @end
-spec merge_trace(integer(), integer(), integer(), string() | atom()) -> ok | merge_fail().
merge_trace(Ref, RunNum, MergeNum, Out) ->
    stop_trace(),
    case get_trace_case(Ref) of
	#trace_case{} = Case ->
	    Merges = Case#trace_case.merge_confs,
	    Runs = Case#trace_case.all_traces,
            #merge_conf{function = Function} = lists:nth(MergeNum, Merges),
	    %%Check if the selected run reference and the selected merge reference
	    %%are valid and run the merge
            gen_server:call(?SERVER, {merge_trace, 
                                      element(1, lists:nth(RunNum, Runs)),
                                      Function,
                                      Out});
	_ ->
	    {error, no_such_trace}
    end.
    
%%------------------------------------------------------------------------------
%% user api
%%------------------------------------------------------------------------------
-spec add_trace_case(#trace_case{}) -> ok.
add_trace_case(TraceCase) ->
    gen_server:call(?SERVER, {add_trace_case, TraceCase}).


%% ---- Getters
-spec get_trace_cases() -> list(#trace_case{}).
get_trace_cases() ->
    gen_server:call(?SERVER, get_trace_cases).

-spec get_trace_case(integer()) -> #trace_case{}.
get_trace_case(Reference) ->    
    gen_server:call(?SERVER, {get_trace_case, Reference}).

%% ---- Delete
-spec delete_trace_case(integer()) -> ok.    
delete_trace_case(Reference) ->
    gen_server:call(?SERVER, {delete_trace_case, Reference}).


%% ---- File management
%% @doc Save current configuration to a file. The file will be stored in a 
%% binary format. 
%% @end
-spec save_to_file(string()) -> {ok, integer()}.   
save_to_file(FileName) ->
    gen_server:call(?SERVER, {save_conf_to_file, FileName}).

%% @doc Read a configuration file as previously saved by 
%% {@link save_to_file/1. 'save_to_file/1'}.
%% @end
-spec read_from_file(string()) -> {ok, list()}.   
read_from_file(FileName) ->
    gen_server:call(?SERVER, {read_conf_from_file, FileName}).


%%------------------------------------------------------------------------------
%% gen_server exports
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

-spec init(term()) -> {ok, #state{}}.
init(_Args) ->
    State = #state{trace_cases = []},
    {ok, State}.

%% ---- Gen server calls
-spec handle_call(term(), pid(), #state{}) -> {reply, term(), #state{}}.
%% control handlers
handle_call({run_trace, Case, Comment}, _From, State) ->
    Patterns = [ {M, F, A, MS} || #pattern{module = M,
                                           function = F,
                                           arity = A,
                                           matchspec = MS} <- Case#trace_case.patterns ],
    FlagRecord = Case#trace_case.trace_flag,
    Flags = {FlagRecord#flags.scope, FlagRecord#flags.flags},
    {Reply, S2} = case ttb:start_trace(Case#trace_case.nodes,
                                        Patterns,
                                        Flags,
                                        Case#trace_case.trace_opts) of
                      {ok, _} ->
                          {ok, State#state{running={Case#trace_case.case_id, Comment}}};
                      {_, Error} ->
                          {{error, Error}, State}
            end,
    
    {reply, Reply, S2};

handle_call(stop_trace, _From, #state{running = undefined} = State) ->
    {reply, ok, State};

handle_call(stop_trace, _From, State) ->
    {Id, Comment} = State#state.running,
    case ttb:stop(return) of
        {stopped, Dir} ->
            Case = lists:keyfind(Id, 2, State#state.trace_cases),
            AllTraces = Case#trace_case.all_traces,
            AllTraces2 = [{Dir, Comment} | AllTraces],
            spawn(?MODULE, add_trace_case, [Case#trace_case{all_traces = AllTraces2}]),
            {reply, {stopped, Dir}, State#state{running = undefined}};
        Error ->
            {reply, {error, Error}, State#state{running = undefined}}
    end;

handle_call({merge_trace, Dir, Merge, Out}, _From, State) ->
    ttb:format(Dir, [{out, Out}, {handler, Merge}]),
    Reply = ok,
    {reply, Reply, State};    

%% file handlers
handle_call({read_conf_from_file, FileName}, _From, #state{running=undefined} = State) ->
    {Reply, NewState} = case file:read_file(FileName) of
                            {ok, BinaryData} ->
                                case sanity_check(binary_to_term(BinaryData)) of
                                    {Reason, []} ->
                                        {Reason, State}; % original state if error
                                    {ok, Data} ->
					%%Remove trace runs without data
					TraceCases = Data#state.trace_cases,
					Filtered = lists:map(fun validate_runs/1,
							     TraceCases),
                                        {ok, Data#state{trace_cases = Filtered}}
                                end;                        
                            Reason ->
                                {Reason, State}
                        end,    
    {reply, build_readable_reply({Reply, NewState}), NewState};
handle_call({read_conf_from_file, _}, _From, State) ->
    {reply, {error, trace_running}, State};

handle_call({save_conf_to_file, FileName}, _From, #state{running=undefined} = State) ->
    Reply = case file:open(FileName, [write]) of
		{ok, FD} ->
		    SerialisedData = term_to_binary(State),
		    
		    file:write(FD, SerialisedData),
		    file:close(FD),
		    
		    {ok, size(SerialisedData)};            
		Reason ->
		    Reason
	    end,
    {reply, Reply, State};
handle_call({save_conf_to_file, _}, _From, State) ->
    {reply, {error, trace_running}, State};

%% get handlers
handle_call(get_trace_cases, _From, State) ->
    {reply, State#state.trace_cases, State};

handle_call({get_trace_case, Ref}, _From, State) ->
    {reply, lists:keyfind(Ref, 2, State#state.trace_cases), State};

%% set handlers
handle_call({add_trace_case, TraceCase}, _From, State) ->
    NewCase = case TraceCase#trace_case.case_id of
		  undefined ->
		      NextId = length(State#state.trace_cases) + 1,
		      TraceCase#trace_case{case_id = NextId};
		  _ ->
		      TraceCase
	      end,

    NewCases = lists:keystore(TraceCase#trace_case.case_id, 2,
			      State#state.trace_cases, NewCase),
    NewState = State#state{trace_cases=NewCases},

    {reply, NewCase, NewState};

%% delete handlers
handle_call({delete_trace_case, Ref}, _From, State) ->
    NewList = lists:keydelete(Ref, 2, State#state.trace_cases),
    NewState = State#state{trace_cases = NewList},
    {reply, ok, NewState};

%% normal stop
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% ---- Gen server casts
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% ---- Gen server info
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% ---- Gen server other
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% helper functions
%%------------------------------------------------------------------------------
-spec validate_runs(#trace_case{}) -> #trace_case{}.
validate_runs(TraceCase) ->
    Runs = TraceCase#trace_case.all_traces,
    Runs2 = lists:filter(fun({RunRef, _}) ->
                                 filelib:is_dir(RunRef)
                         end, Runs),
    TraceCase#trace_case{all_traces = Runs2}.

sanity_check(#state{trace_cases = TraceCases} = State) ->
    case sanity_trace_cases(TraceCases) of
        ok ->
            {ok, State};
        _ ->
            {{error, file_corrupted}, []}
    end;
sanity_check(_Data) ->
    {{error, file_corrupted}, []}.

%%
sanity_trace_cases([]) ->
    ok;
sanity_trace_cases([#trace_case{}|Rest]) ->
    sanity_trace_cases(Rest).

%%
build_readable_reply({ok, #state{trace_cases = TraceCases}}) ->
    {ok, [{trace_cases, length(TraceCases)}]};
build_readable_reply({Reason, _}) ->
    Reason.
