%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Module alternative to diameterc for dictionary compilation.
%%
%% Eg. 1> diameter_make:file("mydict.dia").
%%
%%     $ erl -noshell \
%%           -boot start_clean \
%%           -s diameter_make file mydict.dia \
%%           -s init stop
%%

-module(diameter_make).

-export([file/1,
         file/2,
         dict/1,
         dict/2]).

-type opt() :: {outdir|include|name|prefix|inherits, string()}
             | verbose
             | debug.

%% dict/1-2

-spec file(string(), [opt()])
   -> ok
    | {error, string()}.

file(File, Opts) ->
    case dict(File, Opts) of
        {ok, Dict} ->
            make(File,
                 Opts,
                 Dict,
                 [spec || _ <- [1], lists:member(debug, Opts)] ++ [erl, hrl]);
        {error, _} = E ->
            E
    end.

file(File) ->
    file(File, []).

%% dict/2

-spec dict(string(), [opt()])
   -> {ok, orddict:orddict()}
    | {error, string()}.

dict(Path, Opts) ->
    case diameter_dict_util:parse({path, Path}, Opts) of
        {ok, _} = Ok ->
            Ok;
        {error = E, Reason} ->
            {E, diameter_dict_util:format_error(Reason)}
    end.

dict(File) ->
    dict(File, []).

%% ===========================================================================

make(_, _, _, []) ->
    ok;
make(File, Opts, Dict, [Mode | Rest]) ->
    try diameter_codegen:from_dict(File, Dict, Opts, Mode) of
        ok ->
            make(File, Opts, Dict, Rest)
    catch
        error: Reason ->
            {error, {Reason, Mode, erlang:get_stacktrace()}}
    end.
