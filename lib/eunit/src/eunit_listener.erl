%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2009 Richard Carlsson
%% @private
%% @see eunit
%% @doc Generic listener process for eunit.

-module(eunit_listener).

-define(NODEBUG, true).
-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/1, start/2]).

-callback init(_) -> _.
-callback handle_begin(_, _, _) -> _.
-callback handle_end(_, _, _) -> _.
-callback handle_cancel(_, _, _) -> _.
-callback terminate(_, _) -> _.


-record(state, {callback,    % callback module
		pass = 0,
		fail = 0,
		skip = 0,
		cancel = 0,
		state        % substate
	       }).

start(Callback) ->
    start(Callback, []).

start(Callback, Options) ->
    St = #state{callback = Callback},
    spawn_opt(init_fun(St, Options),
	      proplists:get_all_values(spawn, Options)).

-spec init_fun(_, _) -> fun(() -> no_return()).

init_fun(St0, Options) ->
    fun () ->
            St1 = call(init, [Options], St0),
            St2 = expect([], undefined, St1),
            Data = [{pass, St2#state.pass},
                    {fail, St2#state.fail},
                    {skip, St2#state.skip},
                    {cancel, St2#state.cancel}],
            call(terminate, [{ok, Data}, St2#state.state], St2),
            exit(normal)
    end.

expect(Id, ParentId, St) ->
    case wait_for(Id, 'begin', ParentId) of
	{done, Data} ->
	    {done, Data, St};
	{ok, Msg} ->
	    case Msg of
		{group, Data} ->
		    group(Id, Data, St);
		{test, Data} ->
		    St1 = handle_begin(test, Id, Data, St),
		    case wait_for(Id, 'end', ParentId) of
			{cancel, Reason} ->
			    handle_cancel(test, Id, Data, Reason, St1);
			{ok, Result} ->
			    handle_end(test, Id, Data, Result, St1)
		    end
	    end
    end.

%% collect group items in order until group is done
group(Id, Data, St) ->
    St1 = handle_begin(group, Id, Data, St),
    group_loop(0, Id, Data, St1).

group_loop(N, Id, Data, St) ->
    N1 = N + 1,
    case expect(Id ++ [N1], Id, St) of
	{done, {cancel, Reason}, St1} ->
	    handle_cancel(group, Id, Data, Reason, St1);
	{done, Result, St1} ->
	    handle_end(group, Id, Data, Result, St1);
	St1 ->
	    group_loop(N1, Id, Data, St1)
    end.

%% waiting for [..., M, N] begin
%% get:
%%      [..., M, N] begin test  -> expect [..., M, N] end    (test begin)
%%      [..., M, N] begin group -> expect [..., M, N, 1] end (group begin)
%%      [..., M] end -> expect [..., M+1] begin        (parent end)
%%      cancel([..., M])                               (parent cancel)
%%
%% waiting for [..., M, N] end
%% get:
%%      [..., M, N] end -> expect [..., M, N+1] begin    (seen end)
%%      cancel([..., M, N])                              (cancelled)

wait_for(Id, Type, ParentId) ->
    ?debugFmt("waiting for ~w ~w", [Id, Type]),
    receive
	{status, Id, {progress, Type, Data}} ->
	    ?debugFmt("got status ~w ~w", [Id, Data]),
	    {ok, Data};
	{status, ParentId, {progress, 'end', Data}} when Type =:= 'begin' ->
	    ?debugFmt("got parent end ~w ~w", [ParentId, Data]),
	    {done, Data};
	{status, Id, {cancel, Reason}} when Type =:= 'end' ->
	    ?debugFmt("got cancel ~w ~w", [Id, Reason]),
	    {cancel, Reason};
	{status, ParentId, {cancel, _Reason}} ->
	    ?debugFmt("got parent cancel ~w ~w", [ParentId, _Reason]),
	    {done, {cancel, _Reason}}
    end.

call(F, As, St) when is_atom(F) ->
    try apply(St#state.callback, F, As) of
	Substate -> St#state{state = Substate}
    catch
	Class:Term:Trace ->
	    if F =/= terminate ->
		    call(terminate, [{error, {Class, Term, Trace}},
				     St#state.state], St);
	       true -> ok
	    end,
	    erlang:raise(Class, Term, Trace)
    end.

handle_begin(group, Id, Data0, St) ->
    Data = [{id, Id} | Data0],
    ?debugFmt("handle_begin group ~w ~w", [Id, Data0]),
    call(handle_begin, [group, Data, St#state.state], St);
handle_begin(test, Id, Data0, St) ->
    Data = [{id, Id} | Data0],
    ?debugFmt("handle_begin test ~w ~w", [Id, Data0]),
    call(handle_begin, [test, Data, St#state.state], St).

handle_end(group, Id, Data0, {Count, Data1}, St) ->
    Data = [{id, Id}, {size, Count} | Data0 ++ Data1],
    ?debugFmt("handle_end group ~w ~w", [Id, {Count, Data1}]),
    call(handle_end, [group, Data, St#state.state], St);
handle_end(test, Id, Data0, {Status, Data1}, St) ->
    Data = [{id, Id}, {status, Status} | Data0 ++ Data1],
    ?debugFmt("handle_end test ~w ~w", [Id, {Status, Data1}]),
    St1 = case Status of
	      ok -> St#state{pass = St#state.pass + 1};
	      {skipped,_} -> St#state{skip = St#state.skip + 1};
	      {error,_} -> St#state{fail = St#state.fail + 1}
	  end,
    call(handle_end, [test, Data, St#state.state], St1).

handle_cancel(group, Id, Data0, Reason, St) ->
    Data = [{id, Id}, {reason, Reason} | Data0],
    ?debugFmt("handle_cancel group ~w ~w", [Id, Reason]),
    call(handle_cancel, [group, Data, St#state.state],
	 St#state{cancel = St#state.cancel + 1});
handle_cancel(test, Id, Data0, Reason, St) ->
    Data = [{id, Id}, {reason, Reason} | Data0],
    ?debugFmt("handle_cancel test ~w ~w", [Id, Reason]),
    call(handle_cancel, [test, Data, St#state.state],
	 St#state{cancel = St#state.cancel + 1}).
