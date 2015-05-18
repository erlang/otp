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
%% @private
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @see edoc_parse_ref
%% @end
%% =====================================================================

%% @doc Representation and handling of EDoc object references. See
%% {@link edoc_parse_ref} for more details.

-module(edoc_refs).

-export([app/1, app/2, module/1, module/2, module/3,
	 function/2, function/3, function/4, type/1, type/2, type/3,
	 to_string/1, to_label/1, get_uri/2, is_top/2]).

-import(edoc_lib, [join_uri/2, escape_uri/1]).

-include("edoc.hrl").

-define(INDEX_FILE, "index.html").


%% Creating references:

app(App) ->
    {app, App}.

app(App, Ref) ->
    {app, App, Ref}.

module(M) ->
    {module, M}.

module(M, Ref) ->
    {module, M, Ref}.

module(App, M, Ref) ->
    app(App, module(M, Ref)).

function(F, A) ->
    {function, F, A}.

function(M, F, A) ->
    module(M, function(F, A)).

function(App, M, F, A) ->
    module(App, M, function(F, A)).

type(T) ->
    {type, T}.

type(M, T) ->
    module(M, type(T)).

type(App, M, T) ->
    module(App, M, type(T)).


%% Creating a print string for a reference

to_string({app, A}) ->
    "//" ++ atom_to_list(A);
to_string({app, A, Ref}) ->
    "//" ++ atom_to_list(A) ++ "/" ++ to_string(Ref);
to_string({module, M}) ->
    atom_to_list(M) ;
to_string({module, M, Ref}) ->
    atom_to_list(M) ++ ":" ++ to_string(Ref);
to_string({function, F, A}) ->
    atom_to_list(F) ++ "/" ++ integer_to_list(A);
to_string({type, T}) ->
    atom_to_list(T) ++ "()".


%% Creating URIs and anchors.

to_label({function, F, A}) ->
    escape_uri(atom_to_list(F)) ++ "-" ++ integer_to_list(A);
to_label({type, T}) ->
    "type-" ++ escape_uri(atom_to_list(T)).

get_uri({app, App}, Env) ->
    join_uri(app_ref(App, Env), ?INDEX_FILE);
get_uri({app, App, Ref}, Env) ->
    app_ref(App, Ref, Env);
get_uri({module, M, Ref}, Env) ->
    module_ref(M, Env) ++ "#" ++ to_label(Ref);
get_uri({module, M}, Env) ->
    module_ref(M, Env);
get_uri(Ref, _Env) ->
    "#" ++ to_label(Ref).

abs_uri({module, M}, Env) ->
    module_absref(M, Env);
abs_uri({module, M, Ref}, Env) ->
    module_absref(M, Env) ++ "#" ++ to_label(Ref).

module_ref(M, Env) ->
    case (Env#env.modules)(M) of
	"" ->
	    File = atom_to_list(M) ++ Env#env.file_suffix,
	    escape_uri(File);
	Base ->
	    join_uri(Base, module_absref(M, Env))
    end.

module_absref(M, Env) ->
    escape_uri(atom_to_list(M)) ++ escape_uri(Env#env.file_suffix).

app_ref(A, Env) ->
    case (Env#env.apps)(A) of
	"" ->
	    join_uri(Env#env.app_default,
		     join_uri(escape_uri(atom_to_list(A)), ?EDOC_DIR));
	Base ->
	    Base
    end.

app_ref(A, Ref, Env) ->
    join_uri(app_ref(A, Env), abs_uri(Ref, Env)).

is_top({app, _App}, _Env) ->
    true;
is_top(_Ref, _Env) ->
    false.

