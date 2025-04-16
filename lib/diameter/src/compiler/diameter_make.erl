%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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
-moduledoc """
Diameter dictionary compilation.

The function `codec/2` is used to compile a diameter
[dictionary file](diameter_dict.md) into Erlang source. The resulting source
implements the interface diameter requires to encode and decode the dictionary's
messages and AVPs.

The utility [diameterc(1)](diameterc_cmd.md) provides an alternate compilation
interface.

## BUGS

Unrecognized options are silently ignored.

## SEE ALSO

[diameterc(1)](diameterc_cmd.md), [diameter_dict(4)](diameter_dict.md)
""".
-moduledoc(#{since => "OTP R14B03"}).

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

-doc """
Compile a single dictionary file.

The input `File` can be either a path or a literal dictionary, the occurrence
of newline (ascii NL) or carriage return (ascii CR) identifying the latter.
`Opt` determines the format of the results and whether they are written to
file or returned, and can have the following types.

- **`parse | forms | erl | hrl`** - Specifies an output format. Whether the
  output is returned or written to file depends on whether or not option
  `return` is specified. When written to file, the resulting file(s) will have
  extensions `.D`, `.F`, `.erl`, and `.hrl` respectively, basenames defaulting
  to `dictionary` if the input dictionary is literal and does not specify
  [`@name`](diameter_dict.md#name). When returned, results are in the order of
  the corresponding format options. Format options default to `erl` and `hrl`
  (in this order) if unspecified.

  The `parse` format is an internal representation that can be passed to
  `flatten/1` and `format/1`, while the `forms` format can be passed to
  `compile:forms/2`. The `erl` and `hrl` formats are returned as iolists.

- **`{include, string()}`** - Prepend the specified directory to the code path.
  Use to point at beam files compiled from inherited dictionaries,
  [`@inherits`](diameter_dict.md#inherits) in a dictionary file creating a beam
  dependency, not an erl/hrl dependency.

  Multiple `include` options can be specified.

- **`{outdir, string()}`** - Write generated source to the specified directory.
  Defaults to the current working directory. Has no effect if option `return` is
  specified.

- **`return`** - Return results in a `{ok, [Out]}` tuple instead of writing to
  file and returning `ok`.

- **`{name|prefix, string()}`** - Transform the input dictionary before
  compilation, setting [`@name`](diameter_dict.md#name) or
  [`@prefix`](diameter_dict.md#prefix) to the specified string.

- **`{inherits, string()}`** - Transform the input dictionary before
  compilation, appending [`@inherits`](diameter_dict.md#inherits) of the
  specified string.

  Two forms have special meaning:

  ```text
  {inherits, "-"}
  {inherits, "Prev/Mod"}
  ```

  The first has the effect of clearing any previous inherits, the second of
  replacing a previous inherits of `Prev` to one of `Mod`. This allows the
  semantics of the input dictionary to be changed without modifying the file
  itself.

  Multiple `inherits` options can be specified.

Note that a dictionary's [`@name`](diameter_dict.md#name), together with the
`outdir` option, determine the output paths when the `return` option is not
specified. The [`@name`](diameter_dict.md#name) of a literal input dictionary
defaults to `dictionary`.

A returned error reason can be converted into a readable string using
`format_error/1`.
""".
-doc(#{since => <<"OTP R15B">>}).
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

-doc false.
codec(File) ->
    codec(File, []).

%% format/1
%%
%% Turn an orddict returned by dict/1-2 back into a dictionary.

-doc """
format(Parsed)

Turns a parsed dictionary, as returned by `codec/2`, back into the dictionary
format.
""".
-doc(#{since => <<"OTP R16B03">>}).
-spec format(parsed())
   -> iolist().

format([?VERSION | Dict]) ->
    diameter_dict_util:format(Dict).

%% flatten/1
%%
%% Reconstitute a dictionary without @inherits.

-doc """
flatten(Parsed)

Reconstitute a parsed dictionary, as returned by `codec/2`, without using
[`@inherits`](diameter_dict.md#inherits). That is, construct an equivalent
dictionary in which all AVP's are definined in the dictionary itself. The return
value is also a parsed dictionary.
""".
-doc(#{since => <<"OTP R16B03">>}).
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

-doc """
Turn an error reason returned by `codec/2` into a readable string.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec format_error(Reason) -> FormattedReason when
      Reason          :: term(),
      FormattedReason :: string().

format_error(Reason) ->
    diameter_dict_util:format_error(Reason).


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
