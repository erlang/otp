%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% megaco_profile: Utility module used for megaco profiling
%%----------------------------------------------------------------------

-module(megaco_profile).

-export([profile/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Execute Fun and profile it with fprof.
profile(Slogan, Fun) when is_function(Fun) ->
    Pids = [self()],
    profile(Slogan, Fun, Pids).

profile(Slogan, Fun, Pids) ->
    TraceFile = lists:concat(["profile_", Slogan, "-fprof.trace"]),
    DestFile  = lists:concat(["profile_", Slogan, ".fprof"]),
    TreeFile  = lists:concat(["profile_", Slogan, ".calltree"]),
    erlang:garbage_collect(),
    {ok, _Pid} = fprof:start(),
    TraceOpts = [start, 
		 {cpu_time, false}, 
		 {procs,    Pids}, 
		 {file,     TraceFile}
		],
    ok  = fprof:trace(TraceOpts), 
    Res = (catch Fun()),
    ok  = fprof:trace(stop),
    ok  = fprof:profile([{file, TraceFile}]),
    ok  = fprof:analyse([{dest, DestFile}]),
    ok  = fprof:stop(),
    ok  = file:delete(TraceFile),
    reformat_total(DestFile, TreeFile),
    Res.

reformat_total(FromFile, ToFile) ->
    {ok, ConsultedFromFile} = file:consult(FromFile),
    [_AnalysisOpts, [Totals] | Terms] = ConsultedFromFile,
    {totals, _, TotalAcc, _} = Totals,
    {ok, Fd} = file:open(ToFile, [write, raw]),
    Indent = "",
    log(Fd, Indent, TotalAcc, Totals),
    Processes = split_processes(Terms, [], []),
    Reformat = fun(P) -> reformat_process(Fd, "  " ++ Indent, TotalAcc, P) end,
    lists:foreach(Reformat, Processes),
    file:close(Fd).


split_processes([H | T], ProcAcc, TotalAcc) ->
    if
	is_tuple(H) ->
	    split_processes(T, [H | ProcAcc], TotalAcc);
	is_list(H), ProcAcc =:= [] ->
	    split_processes(T, [H], TotalAcc);
	is_list(H) ->
	    split_processes(T, [H], [lists:reverse(ProcAcc) | TotalAcc])
    end;
split_processes([], [], TotalAcc) ->
    lists:reverse(TotalAcc);
split_processes([], ProcAcc, TotalAcc) ->
    lists:reverse([lists:reverse(ProcAcc) | TotalAcc]).

reformat_process(Fd, Indent, TotalAcc, Terms) ->
    case Terms of
	[[{ProcLabel, _, _, _}] | All]  -> ok;
	[[{ProcLabel,_,_,_} | _] | All] -> ok
    end,
    [{_, {TopKey, TopCnt, TopAcc, TopOwn}, _} | _] = All,
    Process = {ProcLabel, TopCnt, TopAcc, TopOwn},
    log(Fd, Indent, TotalAcc, Process),
    reformat_calls(Fd, "  " ++ Indent, TotalAcc, TopKey, All, []).

reformat_calls(Fd, Indent, TotalAcc, Key, Terms, Stack) ->
    {_CalledBy, Current, Calls} = find(Key, Terms),
    log(Fd, Indent, TotalAcc, Current),
    case lists:member(Key, Stack) of
	true ->
	    ok;
	false ->
	    case Key of
		{io_lib, _, _} ->
		    ok;
		{disk_log, _, _} ->
		    ok;
		{lists, flatten, _} ->
		    ok;
		{lists, keysort, _} ->
		    ok;
		_ ->
		    Fun = fun({NextKey, _, _, _}) ->
				  reformat_calls(Fd,
						 "  " ++ Indent,
						 TotalAcc, 
						 NextKey, 
						 Terms, 
						 [Key | Stack])
			  end,
		    lists:foreach(Fun, Calls)
	    end
    end.

find(Key, [{_, {Key, _, _, _}, _} = H | _]) ->
    H;
find(Key, [{_, {_, _, _, _}, _} | T]) ->
    find(Key, T).

log(Fd, Indent, Total, {Label, Cnt, Acc, Own}) ->
    Percent = case Acc of
		  undefined -> 100;
		  _         -> trunc((lists:max([Acc, Own]) * 100) / Total)
	      end,
    Label2 = io_lib:format("~p", [Label]),
    IoList = io_lib:format("~s~p%    ~s \t~p \t~p \t~p\n",
			   [Indent, Percent, Label2, Cnt, trunc(Acc), trunc(Own)]),
    file:write(Fd, IoList).

