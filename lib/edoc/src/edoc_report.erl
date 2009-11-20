%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%% 
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@it.uu.se>
%% @see edoc
%% @end 
%% =====================================================================

%% @doc EDoc verbosity/error reporting.

-module(edoc_report).

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
    io_lib:fwrite("~s, in module header: ", [File]);
where({File, footer}) ->
    io_lib:fwrite("~s, in module footer: ", [File]);
where({File, header}) ->
    io_lib:fwrite("~s, in header file: ", [File]);
where({File, {F, A}}) ->
    io_lib:fwrite("~s, function ~s/~w: ", [File, F, A]);
where([]) ->
    io_lib:fwrite("~s: ", [?APPLICATION]);
where(File) when is_list(File) ->
    File ++ ": ".
