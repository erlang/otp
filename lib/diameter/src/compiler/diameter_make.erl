%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
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

%% Options passed to codec/2.
-type opt() :: {include|outdir|name|prefix|inherits, string()}
             | return
             | verbose
             | debug.

%% Literal dictionary or path. A NL of CR identifies the former.
-type dict() :: iolist()
              | binary().

%% Name of a literal dictionary if otherwise unspecified.
-define(DEFAULT_DICT_NAME, "dictionary.dia").

%% ===========================================================================

%% codec/1-2
%%
%% Parse a dictionary file and generate a codec module. Input
%% dictionary can be either a path or the dictionary itself: the
%% occurrence of \n or \r in the argument is used to distinguish the
%% two.

-spec codec(File, [opt()])
   -> ok
    | {ok, Ret}
    | {error, Reason}
 when File :: dict(),
      Ret  :: list(),  %% [Erl, Hrl | Debug], Debug = [] | [ParseD, Forms]
      Reason :: string().

codec(File, Opts) ->
    case to_dict(File, Opts) of
        {ok, {Dict, Dictish}} ->
            make(file(Dictish), Opts, Dict);
        {error, _} = E ->
            E
    end.

codec(File) ->
    codec(File, []).

file({path, Path}) ->
    Path;
file(_) ->
    ?DEFAULT_DICT_NAME.

%% dict/2
%%
%% Parse a dictionary file and return the orddict that a codec module
%% returns from dict/0.

-spec dict(File, [opt()])
   -> {ok, orddict:orddict()}
    | {error, string()}
  when File :: dict().

dict(File, Opts) ->
    case to_dict(File, Opts) of
        {ok, {Dict, _}} ->
            {ok, Dict};
        {error, _} = E ->
            E
    end.

dict(File) ->
    dict(File, []).

%% to_dict/2

to_dict(File, Opts) ->
    Dictish = maybe_path(File),
    case diameter_dict_util:parse(Dictish, Opts) of
        {ok, Dict} ->
            {ok, {Dict, Dictish}};
        {error = E, Reason} ->
            {E, diameter_dict_util:format_error(Reason)}
    end.

maybe_path(File) ->
    Bin = iolist_to_binary([File]),
    case is_path(Bin) of
        true  -> {path, File};
        false -> Bin
    end.

%% Interpret anything containing \n or \r as a literal dictionary,
%% otherwise a path. (Which might be the wrong guess in the worst case.)
is_path(Bin) ->
    try
        [throw(C) || <<C>> <= Bin, $\n == C orelse $\r == C],
        true
    catch
        throw:_ -> false
    end.

%% format/1
%%
%% Turn an orddict returned by dict/1-2 back into a dictionary.

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
 when File :: dict(),
      Reason :: string().

reformat(File) ->
    case dict(File) of
        {ok, Dict} ->
            {ok, format(Dict)};
        {error, _} = No ->
            No
    end.

%% ===========================================================================

make(File, Opts, Dict) ->
    ok(lists:foldl(fun(M,A) -> [make(File, Opts, Dict, M) | A] end,
                   [],
                   lists:append([[dict, forms] || lists:member(debug, Opts)])
                   ++ [erl, hrl])).
%% The order in which results are generated (dict/forms/erl/hrl) is
%% intentional, in order of more processing (except for hrl, which
%% isn't needed by diameter itself), since an error raises an
%% exception. The order of return results is the reverse.

ok([ok,_|_]) ->
    ok;
ok([_,_|_] = L) ->
    {ok, L}.

make(File, Opts, Dict, Mode) ->
    try
        diameter_codegen:from_dict(File, Dict, Opts, Mode)
    catch
        error: Reason ->
            erlang:error({Reason, Mode, erlang:get_stacktrace()})
    end.
