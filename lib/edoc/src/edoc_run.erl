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
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Interface for calling EDoc from Erlang startup options.
%%
%% The following is an example of typical usage in a Makefile:
%% ```docs:
%%            erl -noshell -run edoc_run application "'$(APP_NAME)'" \
%%              '"."' '[{def,{vsn,"$(VSN)"}}]'
%% '''
%% (note the single-quotes to avoid shell expansion, and the
%% double-quotes enclosing the strings).
%%
%% <strong>New feature in version 0.6.9</strong>: It is no longer
%% necessary to write `-s init stop' last on the command line in order
%% to make the execution terminate. The termination (signalling success
%% or failure to the operating system) is now built into these
%% functions.

-module(edoc_run).

-export([file/1, application/1, files/1, toc/1]).

-compile({no_auto_import,[error/1]}).

-import(edoc_report, [report/2, error/1]).

-type args() :: [string()].


%% @spec application([string()]) -> none()
%%
%% @doc Calls {@link edoc:application/3} with the corresponding
%% arguments. The strings in the list are parsed as Erlang constant
%% terms. The list can be either `[App]', `[App, Options]' or `[App,
%% Dir, Options]'. In the first case {@link edoc:application/1} is
%% called instead; in the second case, {@link edoc:application/2} is
%% called.
%%
%% The function call never returns; instead, the emulator is
%% automatically terminated when the call has completed, signalling
%% success or failure to the operating system.

-spec application(args()) -> no_return().
application(Args) ->
    F = fun () ->
		case parse_args(Args) of
		    [App] -> edoc:application(App);
		    [App, Opts] -> edoc:application(App, Opts);
		    [App, Dir, Opts] -> edoc:application(App, Dir, Opts);
		    _ ->
			invalid_args("edoc_run:application/1", Args)
		end
	end,
    run(F).

%% @spec files([string()]) -> none()
%%
%% @doc Calls {@link edoc:files/2} with the corresponding arguments. The
%% strings in the list are parsed as Erlang constant terms. The list can
%% be either `[Files]' or `[Files, Options]'. In the first case, {@link
%% edoc:files/1} is called instead.
%%
%% The function call never returns; instead, the emulator is
%% automatically terminated when the call has completed, signalling
%% success or failure to the operating system.

-spec files(args()) -> no_return().
files(Args) ->
    F = fun () ->
		case parse_args(Args) of
		    [Files] -> edoc:files(Files);
		    [Files, Opts] -> edoc:files(Files, Opts);
		    _ ->
			invalid_args("edoc_run:files/1", Args)
		end
	end,
    run(F).

%% @hidden   Not official yet
-spec toc(args()) -> no_return().
toc(Args) ->
    F = fun () ->
 		case parse_args(Args) of
 		    [Dir, Paths] -> edoc:toc(Dir,Paths);
 		    [Dir, Paths, Opts] -> edoc:toc(Dir,Paths,Opts);
 		    _ ->
 			invalid_args("edoc_run:toc/1", Args)
 		end
 	end,
    run(F).


%% @spec file([string()]) -> none()
%%
%% @deprecated This is part of the old interface to EDoc and is mainly
%% kept for backwards compatibility. The preferred way of generating
%% documentation is through one of the functions {@link application/1}
%% and {@link files/1}.
%%
%% @doc Calls {@link edoc:file/2} with the corresponding arguments. The
%% strings in the list are parsed as Erlang constant terms. The list can
%% be either `[File]' or `[File, Options]'. In the first case, an empty
%% list of options is passed to {@link edoc:file/2}.
%%
%% The following is an example of typical usage in a Makefile:
%% ```$(DOCDIR)/%.html:%.erl
%%            erl -noshell -run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]' \
%%              -s init stop'''
%%
%% The function call never returns; instead, the emulator is
%% automatically terminated when the call has completed, signalling
%% success or failure to the operating system.

-spec file(args()) -> no_return().
file(Args) ->
    F = fun () ->
		case parse_args(Args) of
		    [File] -> edoc:file(File, []);
		    [File, Opts] -> edoc:file(File, Opts);
		    _ ->
			invalid_args("edoc_run:file/1", Args)
		end
	end,
    run(F).

-spec invalid_args(string(), args()) -> no_return().
invalid_args(Where, Args) ->
    report("invalid arguments to ~ts: ~tw.", [Where, Args]),
    shutdown_error().

run(F) ->
    wait_init(),
    case catch {ok, F()} of
	{ok, _} ->
	    shutdown_ok();
	{'EXIT', E} ->
	    report("edoc terminated abnormally: ~tP.", [E, 10]),
	    shutdown_error();
	Thrown ->
	    report("internal error: throw without catch in edoc: ~tP.",
		   [Thrown, 15]),
	    shutdown_error()
    end.

wait_init() ->
    case erlang:whereis(code_server) of
	undefined ->
	    erlang:yield(),
	    wait_init();
	_ ->
	    ok
    end.

%% When and if a function init:stop/1 becomes generally available, we
%% can use that instead of delay-and-pray when there is an error.

-spec shutdown_ok() -> no_return().
shutdown_ok() ->
    %% shut down emulator nicely, signalling "normal termination"
    init:stop().

-spec shutdown_error() -> no_return().
shutdown_error() ->
    %% delay 1 second to allow I/O to finish
    receive after 1000 -> ok end,
    %% stop emulator the hard way with a nonzero exit value
    halt(1).

parse_args([A | As]) when is_atom(A) ->
    [parse_arg(atom_to_list(A)) | parse_args(As)];
parse_args([A | As]) ->
    [parse_arg(A) | parse_args(As)];
parse_args([]) ->
    [].

parse_arg(A) ->
    case catch {ok, edoc_lib:parse_expr(A, 1)} of
	{ok, Expr} ->
	    case catch erl_parse:normalise(Expr) of
		{'EXIT', _} ->
		    report("bad argument: '~ts':", [A]),
		    exit(error);
		Term ->
		    Term
	    end;
	{error, _, D} ->
	    report("error parsing argument '~ts'", [A]),
	    error(D),
	    exit(error)
    end.
