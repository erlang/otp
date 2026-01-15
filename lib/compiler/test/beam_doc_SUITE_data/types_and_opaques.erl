-module(types_and_opaques).

-export([foo/0, private_encoding_func/2,map_fun/0,ignore_type_from_hidden_fun/0]).

-export_type([name/1,unnamed/0, mmaybe/1, callback_mode_result/0]).

-export([uses_public/0]).
-export_type([public/0]).

-include("types_and_opaques.hrl").

-doc "
name(_)

Represents the name of a person.
".
-type name(_Ignored) :: string().

-doc #{since => "1.0"}.
-doc #{equiv => non_neg_integer/0}.
-type natural_number() :: non_neg_integer().

-doc "
Tests generics
".
-doc #{equiv => madeup()}.
-type param(X) :: {X, integer(), Y :: string()}.

-doc #{equiv => non_neg_integer()}.
-doc "
unnamed()

Represents the name of a person that cannot be named.
".
-opaque unnamed() :: name(integer()).



-export_type([natural_number/0, param/1]).



-doc #{ authors => "Someone else" }.
-doc "
mmaybe(X) ::= nothing | X.

Represents a maybe type.
".
-opaque mmaybe(X) :: nothing | X.

-opaque non_exported() :: atom().

-type not_exported_either() :: atom().

-doc hidden.
-doc #{ authors => "Someone else" }.
-type hidden_false() :: atom().

-doc false.
-type hidden() :: hidden_false().



-export_type([hidden_false/0, hidden/0]).



-type one() :: 1.
-type two() :: one().
-type three() :: two().
-type four() :: three().

-spec foo() -> three().
foo() -> 1.


-type encoding_func() :: fun((non_neg_integer()) -> boolean()).

-spec private_encoding_func(Data, Options) -> AbsTerm when
      Data :: term(),
      Options :: Location | [Option],
      Option :: {encoding, Encoding}
              | {line, Line}
              | {location, Location},
      Encoding :: 'latin1' | 'unicode' | 'utf8' | 'none' | encoding_func(),
      Line :: erl_anno:line(),
      Location :: erl_anno:location(),
      AbsTerm :: term().
private_encoding_func(_, _) ->
    ok.


-type callback_mode_result() ::
	callback_mode() | [callback_mode() | state_enter()].
-type callback_mode() :: 'state_functions' | 'handle_event_function'.
-type state_enter() :: 'state_enter'.


-type mymap() :: #{ foo => my_private_type(),
                    bar := my_other_private_type()}.

-type my_private_type() :: integer().
-type my_other_private_type() :: non_neg_integer().

-spec map_fun() -> mymap().
map_fun() ->
    ok.


-doc false.
-spec ignore_type_from_hidden_fun() -> four().
ignore_type_from_hidden_fun() ->
    ok.

%% Type below should be a warning, since it is refered
%% by a public function or type and the inner type is hidden.
-doc false.
-type hidden_type() :: integer().
%% Test suppression of hidden type warning.
-doc false.
-type hidden_nowarn_type() :: integer().
-compile({nowarn_hidden_doc, [hidden_nowarn_type/0]}).

-type intermediate() :: hidden_type() | hidden_included_type() | hidden_nowarn_type().
-type public() :: intermediate().

-spec uses_public() -> public().
uses_public() ->
    qux().

-doc "
Un-exported function with doc attribute
".
qux() ->
    ok.
