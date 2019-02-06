%% =====================================================================
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
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end 
%% =====================================================================

%% @doc EDoc verbosity/error reporting.

-module(edoc_report).

%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([error/1,
	 error/2,
	 error/3,
	 report/2,
	 report/3,
	 report/4,
	 warning/1,
	 warning/2,
	 warning/3,
	 warning/4]).

-include("edoc.hrl").


error(What) ->
    error([], What).

error(Where, What) ->
    error(0, Where, What).

error(Line, Where, S) when is_list(S) ->
    report(Line, Where, S, []);
error(Line, Where, {S, D}) when is_list(S) ->
    report(Line, Where, S, D);
error(Line, Where, {format_error, M, D}) ->
    report(Line, Where, M:format_error(D), []).

warning(S) ->
    warning(S, []).

warning(S, Vs) ->
    warning([], S, Vs).

warning(Where, S, Vs) ->
    warning(0, Where, S, Vs).

warning(L, Where, S, Vs) ->
    report(L, Where, "warning: " ++ S, Vs).

report(S, Vs) ->
    report([], S, Vs).

report(Where, S, Vs) ->
    report(0, Where, S, Vs).

report(L, Where, S, Vs) ->
    io:put_chars(where(Where)),
    if is_integer(L), L > 0 ->
	    io:fwrite("at line ~w: ", [L]);
       true ->
	    ok
    end,
    io:fwrite(S, Vs),
    io:nl().

where({File, module}) ->
    io_lib:fwrite("~ts, in module header: ", [File]);
where({File, footer}) ->
    io_lib:fwrite("~ts, in module footer: ", [File]);
where({File, header}) ->
    io_lib:fwrite("~ts, in header file: ", [File]);
where({File, {F, A}}) ->
    io_lib:fwrite("~ts, function ~ts/~w: ", [File, F, A]);
where([]) ->
    io_lib:fwrite("~s: ", [?APPLICATION]);
where(File) when is_list(File) ->
    File ++ ": ".
