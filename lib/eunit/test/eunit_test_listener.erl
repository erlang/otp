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
%% @author The OTP team at Ericsson
%% @copyright Ericsson AB 2022
%% @private
%% @see eunit
%% @doc Module for receiving EUnit events for verification purposes

-module(eunit_test_listener).
-behaviour(eunit_listener).

-export([start/0, start/1]).
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
	 terminate/2]).

-record(state, {test_process = undefined}).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    [TestProcess] = Options,
    receive
	{start, _Reference} ->
            #state{test_process = TestProcess}
    end.

terminate({ok, Data}, #state{test_process = TestProcess}) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    TestProcess ! {test_report,
                   #{pass => Pass, fail => Fail, skip => Skip,
                     cancel => Cancel}},
    if Fail =:= 0, Skip =:= 0, Cancel =:= 0 ->
	    sync_end(ok);
       true ->
	    sync_end(error)
    end;
terminate({error, _Reason}, #state{}) ->
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

handle_begin(group, _Data, St) ->
    St;
handle_begin(test, _Data, St) ->
    St.

handle_end(group, _Data, St) ->
    St;
handle_end(test, _Data, St) ->
    St.

handle_cancel(group, _Data, St) ->
    St;
handle_cancel(test, _Data, St) ->
    St.
