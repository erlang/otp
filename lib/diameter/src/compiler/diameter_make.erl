%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
         flatten/1,
         format_error/1]).

-export_type([opt/0]).

-include("diameter_vsn.hrl").

%% Options passed to codec/2.
-type opt() :: {include|outdir|name|prefix|inherits, string()}
             | return
             | verbose
             | parse  %% internal parsed form
             | forms  %% abstract format for compile:forms/1,2
             | erl
             | hrl.

%% Internal parsed format with a version tag.
-type parsed() :: list().

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
        {error, _} = E ->
            E
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
    [V | lists:foldl(fun flatten/2,
                     Dict,
                     [avp_vendor_id,
                      custom_types,
                      codecs,
                      [avp_types, import_avps],
                      [grouped, import_groups],
                      [enum, import_enums]])].

%% format_error/1

format_error(T) ->
    diameter_dict_util:format_error(T).

%% ===========================================================================

%% flatten/2

flatten([_,_] = Keys, Dict) ->
    [Values, Imports] = [orddict:fetch(K, Dict) || K <- Keys],
    Vs = lists:append([Values | [V || {_Mod, V} <- Imports]]),
    lists:foldl(fun({K,V},D) -> orddict:store(K,V,D) end,
                Dict,
                lists:zip([inherits | Keys], [[], Vs, []]));

%% Inherited avp's setting the 'V' flag get their value either from
%% @avp_vendor_id in the inheriting dictionary or from @vendor in the
%% *inherited* (not inheriting) dictionary: add the latter to
%% @avp_vendor_id as required.
flatten(avp_vendor_id = Key, Dict) ->
    Def = orddict:find(vendor, Dict),
    ModD = imports(Dict),
    Vids = orddict:fetch(Key, Dict),
    Avps = lists:append([As || {_,As} <- Vids]),
    orddict:store(Key,
                  dict:fold(fun(M, As, A) -> vid(M, As -- Avps, Def, A) end,
                            Vids,
                            ModD),
                  Dict);

%% Import @codecs and @custom_types from inherited dictionaries as
%% required.
flatten(Key, Dict) ->
    ImportAvps = orddict:fetch(import_avps, Dict),
    ImportItems = [{M, As}
                   || {Mod, Avps} <- ImportAvps,
                      [_|D] <- [Mod:dict()],
                      {M,As0} <- orddict:fetch(Key, D),
                      F <- [fun(A) -> lists:keymember(A, 1, Avps) end],
                      [_|_] = As <- [lists:filter(F, As0)]],
    orddict:store(Key,
                  lists:foldl(fun merge/2,
                              orddict:fetch(Key, Dict),
                              ImportItems),
                  Dict).

%% merge/2

merge({Mod, _Avps} = T, Acc) ->
    merge(lists:keyfind(Mod, 1, Acc), T, Acc).

merge({Mod, Avps}, {Mod, As}, Acc) ->
    lists:keyreplace(Mod, 1, Acc, {Mod, Avps ++ As});
merge(false, T, Acc) ->
    [T | Acc].

%% imports/1
%%
%% Return a module() -> [AVP] dict of inherited AVP's setting the V flag.

imports(Dict) ->
    lists:foldl(fun imports/2,
                dict:new(),
                orddict:fetch(import_avps, Dict)).

imports({Mod, Avps}, Dict) ->
    dict:store(Mod,
               [A || {A,_,_,Fs} <- Avps, lists:member($V, Fs)],
               Dict).

%% vid/4

vid(_, [], _, Acc) ->
    Acc;
vid(Mod, Avps, Def, Acc) ->
    v(Mod:vendor_id(), Avps, Def, Acc).

v(Vid, _, {ok, {Vid, _}}, Acc) -> %% same id as inheriting dictionary's
    Acc;
v(Vid, Avps, _, Acc) ->
    case lists:keyfind(Vid, 1, Acc) of
        {Vid, As} ->
            lists:keyreplace(Vid, 1, Acc, {Vid, As ++ Avps});
        false ->
            [{Vid, Avps} | Acc]
    end.

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
    lists:member(T, [erl, hrl, parse, forms]).

identify([Vsn | [T|_] = ParseD])
  when is_tuple(T) ->
    ?VERSION == Vsn orelse erlang:error({version, {Vsn, ?VERSION}}),
    {{dict, ParseD}, ?DEFAULT_DICT_FILE};
identify({path, File} = T) ->
    {T, File};
identify(File) ->
    case is_path([File]) of
        true  -> {{path, File}, File};
        false -> {File, ?DEFAULT_DICT_FILE}
    end.

%% Interpret anything containing \n or \r as a literal dictionary.

is_path([<<C,B/binary>> | T]) ->
    is_path([C, B | T]);

is_path([[C|L] | T]) ->
    is_path([C, L | T]);

is_path([C|_])
  when $\n == C;
       $\r == C ->
    false;

is_path([_|T]) ->
    is_path(T);

is_path([]) ->
    true.

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
        error: Reason: Stack ->
            erlang:error({Reason, Mode, Stack})
    end.
