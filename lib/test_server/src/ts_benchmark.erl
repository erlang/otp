%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2012. All Rights Reserved.
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
-module(ts_benchmark).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("kernel/include/file.hrl").
-include("ts.hrl").

-export([benchmarks/0,
	 run/3]).

%% gen_event callbacks
-export([init/1, handle_event/2]).

benchmarks() ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:specialized_specs(Cwd,"bench").

run(Specs, Opts, Vars) ->
    {ok, Cwd} = file:get_cwd(),
    {{YY,MM,DD},{HH,Mi,SS}} = calendar:local_time(),
    BName = lists:concat([YY,"_",MM,"_",DD,"T",HH,"_",Mi,"_",SS]),
    BDir = filename:join([Cwd,BName]),
    file:make_dir(BDir),
    [ts_run:run(atom_to_list(Spec),
		[{spec, [atom_to_list(Spec)++"_bench.spec"]}],
		[{event_handler, {ts_benchmark, [Spec,BDir]}}|Opts],Vars) 
     || Spec <- Specs],
    file:delete(filename:join(Cwd,"latest_benchmark")),
    {ok,D} = file:open(filename:join(Cwd,"latest_benchmark"),[write]),
    io:format(D,BDir,[]),
    file:close(D).
    

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

-record(state, { spec, suite, tc, stats_dir}).

init([Spec,Dir]) ->
    {ok, #state{ spec = Spec, stats_dir = Dir }}.

handle_event(#event{name = tc_start, data = {Suite,Tc}}, State) ->
    {ok,State#state{ suite = Suite, tc = Tc}};
handle_event(#event{name = benchmark_data, data = Data}, State) ->
    Spec = proplists:get_value(application, Data, State#state.spec),
    Suite = proplists:get_value(suite, Data, State#state.suite),
    Tc = proplists:get_value(name, Data, State#state.tc),
    Value = proplists:get_value(value, Data),
    {ok, D} = file:open(filename:join(
			  [State#state.stats_dir,
			   lists:concat([e(Spec),"-",e(Suite),"-",
					 e(Tc),".ebench"])]),
			[append]),
    io:format(D, "~p~n",[Value]),
    file:close(D),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.


e(Atom) when is_atom(Atom) ->
    Atom;
e(Str) when is_list(Str) ->
    lists:map(fun($/) ->
		      $\\;
		 (C) ->
		      C
	      end,Str).
