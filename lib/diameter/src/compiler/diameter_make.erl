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

-export([codec/2,
         codec/1,
         format/1,
         flatten/1]).

-export_type([opt/0]).

-include("diameter_vsn.hrl").

%% Options passed to codec/2.
-type opt() :: {include|outdir|name|prefix|inherits, string()}
             | return
             | verbose
             | parse  %% internal parsed form
             | forms  %% abstract format from which erl is generated
             | beam   %% compiled directly from preprocessed forms
             | erl
             | hrl.

%% Internal parsed format with a version tag.
-type parsed() :: maybe_improper_list(integer(), orddict:orddict()).

%% Literal dictionary or path. A NL of CR identifies the former.
-type dict() :: iolist()
              | binary()
              | parsed().  %% as returned by codec/2

%% Name of a literal dictionary if otherwise unspecified.
-define(DEFAULT_DICT_FILE, "dictionary.dia").

%% ===========================================================================

%% codec/1-2
%%
%% Parse a dictionary file and generate a codec module. Input
%% dictionary can be either a path or the dictionary itself: the
%% occurrence of \n or \r in the argument is used to distinguish the
%% two.

-spec codec(File, [opt()])
   -> ok
    | {ok, list()}   %% with option 'return', one element for each output
    | {error, Reason}
 when File :: dict()
            | {path, file:name_all()},
      Reason :: string().

codec(File, Opts) ->
    {Dict, Path} = identify(File),
    case parse(Dict, Opts) of
        {ok, ParseD} ->
            make(Path, default(Opts), ParseD);
        {error = E, Reason} ->
            {E, diameter_dict_util:format_error(Reason)}
    end.

codec(File) ->
    codec(File, []).

%% format/1
%%
%% Turn an orddict returned by dict/1-2 back into a dictionary.

-spec format(parsed())
   -> iolist().

format([?VERSION | Dict]) ->
    diameter_dict_util:format(Dict).

%% flatten/1
%%
%% Reconstitute a dictionary without @inherits.

-spec flatten(parsed())
   -> parsed().

flatten([?VERSION = V | Dict]) ->
    [V | lists:foldl(fun flatten/2, Dict, [[avp_types, import_avps],
                                           [grouped, import_groups],
                                           [enum, import_enums]])].

flatten([_,_] = Keys, Dict) ->
    [Values, Imports] = [orddict:fetch(K, Dict) || K <- Keys],
    Vs = lists:append([Values | [V || {_,V} <- Imports]]),
    lists:foldl(fun store/2,
                Dict,
                lists:zip([inherits | Keys], [[], Vs, []])).

store({Key, Value}, Dict) ->
    orddict:store(Key, Value, Dict).

%% ===========================================================================

parse({dict, ParseD}, _) ->
    {ok, ParseD};
parse(File, Opts) ->
    diameter_dict_util:parse(File, Opts).

default(Opts) ->
    def(modes(Opts), Opts).

def([], Opts) ->
    [erl, hrl | Opts];
def(_, Opts) ->
    Opts.

modes(Opts) ->
    lists:filter(fun is_mode/1, Opts).

is_mode(T) ->
    lists:member(T, [erl, hrl, parse, forms, beam]).

identify([Vsn | [T|_] = ParseD])
  when is_tuple(T) ->
    ?VERSION == Vsn orelse erlang:error({version, {Vsn, ?VERSION}}),
    {{dict, ParseD}, ?DEFAULT_DICT_FILE};
identify({path, File} = T) ->
    {T, File};
identify(File) ->
    Bin = iolist_to_binary([File]),
    case is_path(Bin) of
        true  -> {{path, File}, File};
        false -> {Bin, ?DEFAULT_DICT_FILE}
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

make(File, Opts, Dict) ->
    ok(lists:foldl(fun(M,A) -> [make(File, Opts, Dict, M) | A] end,
                   [],
                   modes(Opts))).

ok([ok|_]) ->
    ok;
ok([_|_] = L) ->
    {ok, lists:reverse(L)}.

make(File, Opts, Dict, Mode) ->
    try
        diameter_codegen:from_dict(File, Dict, Opts, Mode)
    catch
        error: Reason ->
            erlang:error({Reason, Mode, erlang:get_stacktrace()})
    end.
