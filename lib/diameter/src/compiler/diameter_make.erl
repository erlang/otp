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
%% Eg. 1> diameter_make:codec("mydict.dia").
%%
%%     $ erl -noinput \
%%           -boot start_clean \
%%           -eval 'ok = diameter_make:codec("mydict.dia")' \
%%           -s init stop
%%

-module(diameter_make).

-export([codec/1,
         codec/2,
         dict/1,
         dict/2,
         format/1,
         reformat/1]).

-export_type([opt/0]).

-type opt() :: {include|outdir|name|prefix|inherits, string()}
             | verbose
             | debug.

%% ===========================================================================

%% codec/1-2
%%
%% Parse a dictionary file and generate a codec module.

-spec codec(Path, [opt()])
   -> ok
    | {error, Reason}
 when Path :: string(),
      Reason :: string().

codec(File, Opts) ->
    case dict(File, Opts) of
        {ok, Dict} ->
            make(File,
                 Opts,
                 Dict,
                 [spec || _ <- [1], lists:member(debug, Opts)] ++ [erl, hrl]);
        {error, _} = E ->
            E
    end.

codec(File) ->
    codec(File, []).

%% dict/2
%%
%% Parse a dictionary file and return the orddict that a codec module
%% returns from dict/0.

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

%% format/1
%%
%% Turn an orddict returned by dict/1-2 back into a dictionary file
%% in the form of an iolist().

-spec format(orddict:orddict())
   -> iolist().

format(Dict) ->
    diameter_dict_util:format(Dict).

%% reformat/1
%%
%% Parse a dictionary file and return its formatted equivalent.

-spec reformat(File)
   -> {ok, iolist()}
    | {error, Reason}
 when File :: string(),
      Reason :: string().

reformat(File) ->
    case dict(File) of
        {ok, Dict} ->
            {ok, format(Dict)};
        {error, _} = No ->
            No
    end.

%% ===========================================================================

make(_, _, _, []) ->
    ok;
make(File, Opts, Dict, [Mode | Rest]) ->
    try
        ok = diameter_codegen:from_dict(File, Dict, Opts, Mode),
        make(File, Opts, Dict, Rest)
    catch
        error: Reason ->
            erlang:error({Reason, Mode, erlang:get_stacktrace()})
    end.
