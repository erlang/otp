%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2025. All Rights Reserved.
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

-module(xref).
-moduledoc """
A Cross Reference Tool for analyzing dependencies between functions, modules,
applications, and releases.

Calls between functions are either _local calls_{: #local_call } such as `f()`,
or _external calls_{: #external_call } such as `mod:f()`.

_Module data_{:#module_data }, which are extracted from BEAM files,
include local functions, exported functions, local calls, and external
calls. By default, calls to built-in functions (BIF) are ignored, but
if the option `builtins`, accepted by some of this module's functions,
is set to `true`, calls to BIFs are included as well. It is the
analyzing OTP version that decides what functions are BIFs.
Functional objects are assumed to be called where they are created
(and nowhere else).

_Unresolved calls_{: #unresolved_call } are calls to `apply` or
`spawn` with variable module, variable function, or variable
arguments. Examples are `M:F(a)`, [`apply(M, f, [a])`](`apply/3`), and
[`spawn(m, f(), Args)`](`spawn/3`). Unresolved calls are represented
by calls where variable modules have been replaced with the atom
`'$M_EXPR'`, variable functions have been replaced with the atom
`'$F_EXPR'`, and variable number of arguments have been replaced with
the number `-1`. The above mentioned examples are represented by calls
to `'$M_EXPR':'$F_EXPR'/1`, `'$M_EXPR':f/1`, and `m:'$F_EXPR'/-1`. The
unresolved calls are a subset of the external calls.

> #### Warning {: .warning }
>
> Unresolved calls make module data incomplete, which implies that the results
> of analyses may be invalid.

_Applications_ are collections of modules. The BEAM files for the
modules are located in the `ebin` subdirectory of the application
directory. The name of the application directory determines the name
and version of the application.

_Releases_ are collections of applications located in the `lib` subdirectory of
the release directory. There is more to read about applications and releases in
the Design Principles book.

_Xref servers_{: #xref_server } are identified by names, supplied when
creating new servers. Each Xref server holds a set of releases, a set
of applications, and a set of modules with module data. Xref servers
are independent of each other, and all analyses are evaluated in the
context of one single Xref server (exceptions are the functions
[`m/1`](`m/1`) and [`d/1`](`d/1`) which do not use servers at
all).

The _mode_{: #mode } of an Xref server determines what module data are
extracted from BEAM files as modules are added to the server. BEAM
files compiled with the option `debug_info` contain [](){: #debug_info
} "debug information", which is an abstract representation of the
code.

- In `functions` mode, which is the default mode, function calls
  and line numbers are extracted from debug information.

- In `modules` mode, debug information is ignored if present, but
  dependencies between modules are extracted from other parts of the
  BEAM files. The `modules` mode is significantly less time and space
  consuming than the `functions` mode, but the analyses that can be
  done are limited.

An _analyzed module_{: #analyzed_module } is a module that has been added to an
Xref server together with its module data. A _library module_{: #library_module
} is a module located in some directory mentioned in the _library path_{:
#library_path }. A library module is said to be used if some of its exported
functions are used by some analyzed module. An _unknown module_{:
#unknown_module } is a module that is neither an analyzed module nor a library
module, but whose exported functions are used by some analyzed module.

An _unknown function_{: #unknown_function } is a used function that is
neither local or exported by any analyzed module nor exported by any
library module. An _undefined function_{: #undefined_function } is an
externally used function that is not exported by any analyzed module
or library module. With this notion, a local function can be an
undefined function, namely if it is externally used from some
module. All unknown functions are also undefined functions; there is a
[figure](xref_chapter.md#venn2) in the User's Guide that illustrates
this relationship.

The module attribute tag `deprecated` can be used to inform
Xref about _deprecated functions_{: #deprecated_function } and optionally when
functions are planned to be removed. A few examples show the idea:

- `-deprecated({f,1}).` - The exported function `f/1` is deprecated.
  Nothing is said whether `f/1` will be removed or not.

- `-deprecated({f,1,"Use g/1 instead"}).` - As above but with a descriptive
  string. The string is currently unused by `xref` but other tools can make use
  of it.

- `-deprecated({f,'_'}).` - All exported functions `f/0`, `f/1`, and so on
  are deprecated.

- `-deprecated(module).` - All exported functions in the module are
  deprecated. Equivalent to `-deprecated({'_','_'}).`.

- `-deprecated([{g,1,next_version}]).` - The function `g/1` is deprecated
  and will be removed in next version.

- `-deprecated([{g,2,next_major_release}]).` - The function `g/2` is
  deprecated and will be removed in next major release.

- `-deprecated([{g,3,eventually}]).` - The function `g/3` is deprecated
  and will eventually be removed.

- `-deprecated({'_','_',eventually}).` - All exported functions in the
  module are deprecated and will eventually be removed.

Before any analysis can take place, module data must be _set up_. For instance,
the cross reference and the unknown functions are computed when all module data
are known. The functions that need complete data
([`analyze/2,3`](`analyze/3`), [`q/2,3`](`q/3`), [`variables/1,2`](`variables/2`)
take care of setting up data automatically. Module data need to be set up
(again) after calls to any of the `add`, `replace`, `remove`,
[`set_library_path/2,3`](`set_library_path/3`), or
[`update/1,2`](`update/2`) functions.

The result of setting up module data is the _Call Graph_{: #call_graph }. A
(directed) graph consists of a set of vertices and a set of (directed) edges.
The edges represent _calls_{: #call } (From, To) between functions, modules,
applications, or releases. From is said to call To, and To is said to be used by
From. The vertices of the Call Graph are the functions of all module data: local
and exported functions of analyzed modules; used BIFs; used exported functions
of library modules; and unknown functions. The functions `module_info/0,1` added
by the compiler are included among the exported functions, but only when called
from some module. The edges are the function calls of all module data. A
consequence of the edges being a set is that there is only one edge if a
function is locally or externally used several times on one and the same line of
code.

The Call Graph is [](){: #representation } represented by Erlang terms (the sets
are lists), which is suitable for many analyses. But for analyses that look at
chains of calls, a list representation is much too slow. Instead the
representation offered by the `digraph` module is used. The translation of the
list representation of the Call Graph - or a subgraph thereof - to the `digraph`
representation does not come for free, so the language used for expressing
queries to be described below has a special operator for this task and a
possibility to save the `digraph` representation for subsequent analyses.

In addition to the Call Graph there is a graph called the _Inter Call Graph_{:
#inter_call_graph }. This is a graph of calls (From, To) such that there is a
chain of calls from From to To in the Call Graph, and every From and To is an
exported function or an unused local function. The vertices are the same as for
the Call Graph.

Calls between modules, applications and releases are also directed graphs. The
_types_{: #type } of the vertices and edges of these graphs are (ranging from
the most special to the most general): `Fun` for functions; `Mod` for modules;
`App` for applications; and `Rel` for releases. The following paragraphs will
describe the different constructs of the language used for selecting and
analyzing parts of the graphs, beginning with the _constants_{: #constants }:

- Expression ::= Constants
- Constants ::= Consts | Consts `:` Type | RegExpr
- Consts ::= Constant | `[`Constant`,` ...`]` | `{`Constant`,` ...`}`
- Constant ::= Call | Const
- Call ::= FunSpec `->` FunSpec | `{`MFA`,` MFA`}` | AtomConst `->` AtomConst |
  `{`AtomConst`,` AtomConst`}`
- Const ::= AtomConst | FunSpec | MFA
- AtomConst ::= Application | Module | Release
- FunSpec ::= Module `:` Function `/` Arity
- MFA ::= `{`Module`,` Function`,` Arity`}`
- RegExpr ::= RegString `:` Type | RegFunc | RegFunc `:` Type
- RegFunc ::= RegModule `:` RegFunction `/` RegArity
- RegModule ::= RegAtom
- RegFunction ::= RegAtom
- RegArity ::= RegString | Number | `_` | `-1`
- RegAtom ::= RegString | Atom | `_`
- RegString ::= - a regular expression, as described in the `re` module,
  enclosed in double quotes -
- Type ::= `Fun` | `Mod` | `App` | `Rel`
- Function ::= Atom
- Application ::= Atom
- Module ::= Atom
- Release ::= Atom
- Arity ::= Number | `-1`
- Atom ::= - same as Erlang atoms -
- Number ::= - same as non-negative Erlang integers -

Examples of constants are: `kernel`, `kernel->stdlib`, `[kernel, sasl]`,
`[pg -> mnesia, {tv, mnesia}] : Mod`. It is an error if an instance of `Const`
does not match any vertex of any graph. If there are more than one vertex
matching an untyped instance of `AtomConst`, then the one of the most general
type is chosen. A list of constants is interpreted as a set of constants, all of
the same type. A tuple of constants constitute a chain of calls (which may, but
does not have to, correspond to an actual chain of calls of some graph).
Assigning a type to a list or tuple of `Constant` is equivalent to assigning the
type to each `Constant`.

_Regular expressions_{: #regexp } are used as a means to select some of the
vertices of a graph. A `RegExpr` consisting of a `RegString` and a type - an
example is `"xref_.*" : Mod` \- is interpreted as those modules (or applications
or releases, depending on the type) that match the expression. Similarly, a
`RegFunc` is interpreted as those vertices of the Call Graph that match the
expression. An example is `"xref_.*":"add_.*"/"(2|3)"`, which matches all `add`
functions of arity two or three of any of the xref modules. Another example, one
that matches all functions of arity 10 or more: `_:_/"[1-9].+"`. Here `_` is an
abbreviation for `".*"`, that is, the regular expression that matches anything.

The syntax of _variables_{: #variable } is simple:

- Expression ::= Variable
- Variable ::= - same as Erlang variables -

There are two kinds of variables:

* **Predefined variables** {: #predefined_variable } - hold module data, and
  cannot be assigned to but only used in queries.

* **User variables** {: #user_variable } - can be assigned to, and are
  typically used for temporary results while evaluating a query, and
  for keeping results of queries for use in subsequent queries.

The predefined variables are (variables marked with (\*) are available
in `functions` mode only):

- **`E`** - Call Graph Edges (\*).

- **`V`** - Call Graph Vertices (\*).

- **`M`** - Modules. All modules: analyzed modules, used library modules, and
  unknown modules.

- **`A`** - Applications.

- **`R`** - Releases.

- **`ME`** - Module Edges. All module calls.

- **`AE`** - Application Edges. All application calls.

- **`RE`** - Release Edges. All release calls.

- **`L`** - Local Functions (\*). All local functions of analyzed modules.

- **`X`** - Exported Functions. All exported functions of analyzed modules and
  all used exported functions of library modules.

- **`F`** - Functions (\*).

- **`B`** - Used BIFs. `B` is empty if `builtins` is `false` for all analyzed
  modules.

- **`U`** - Unknown Functions.

- **`UU`** - Unused Functions (\*). All local and exported functions of analyzed
  modules that have not been used.

- **`XU`** - Externally Used Functions. Functions of all modules - including
  local functions - that have been used in some external call.

- **`LU`** - Locally Used Functions (\*). Functions of all modules that have
  been used in some local call.

- **`OL`** - Functions with an attribute tag `on_load` (\*).

- **`LC`** - Local Calls (\*).

- **`XC`** - External Calls (\*).

- **`AM`** - Analyzed Modules.

- **`UM`** - Unknown Modules.

- **`LM`** - Used Library Modules.

- **`UC`** - Unresolved Calls. Empty in `modules` mode.

- **`EE`** - Inter Call Graph Edges (\*).

- **`DF`** - Deprecated Functions. All deprecated exported functions and all
  used deprecated BIFs.

- **`DF_1`** - Deprecated Functions. All deprecated functions to be removed in
  next version.

- **`DF_2`** - Deprecated Functions. All deprecated functions to be removed in
  next version or next major release.

- **`DF_3`** - Deprecated Functions. All deprecated functions to be removed in
  next version, next major release, or later.

These are a few [](){: #simple_facts } facts about the predefined variables (the
set operators `+` (union) and `-` (difference) as well as the cast operator
`(`Type`)` are described below):

- `F` is equal to `L + X`.
- `V` is equal to `X + L + B + U`, where `X`, `L`, `B` and `U` are pairwise
  disjoint (that is, have no elements in common).
- `UU` is equal to `V - (XU + LU)`, where `LU` and `XU` may have elements in
  common. Put in another way:
- `V` is equal to `UU + XU + LU`.
- `OL` is a subset of `F`.
- `E` is equal to `LC + XC`. Note that `LC` and `XC` may have elements in
  common, namely if some function is locally and externally used from one and
  the same function.
- `U` is a subset of `XU`.
- `B` is a subset of `XU`.
- `LU` is equal to `range LC`.
- `XU` is equal to `range XC`.
- `LU` is a subset of `F`.
- `UU` is a subset of `F`.
- `range UC` is a subset of `U`.
- `M` is equal to `AM + LM + UM`, where `AM`, `LM` and `UM` are pairwise
  disjoint.
- `ME` is equal to `(Mod) E`.
- `AE` is equal to `(App) E`.
- `RE` is equal to `(Rel) E`.
- `(Mod) V` is a subset of `M`. Equality holds if all analyzed modules have some
  local, exported, or unknown function.
- `(App) M` is a subset of `A`. Equality holds if all applications have some
  module.
- `(Rel) A` is a subset of `R`. Equality holds if all releases have some
  application.
- `DF_1` is a subset of `DF_2`.
- `DF_2` is a subset of `DF_3`.
- `DF_3` is a subset of `DF`.
- `DF` is a subset of `X + B`.

An important notion is that of _conversion_{: #conversion } of expressions. The
syntax of a cast expression is:

- Expression ::= `(` Type `)` Expression

The interpretation of the cast operator depends on the named type `Type`, the
type of `Expression`, and the structure of the elements of the interpretation of
`Expression`. If the named type is equal to the expression type, no conversion
is done. Otherwise, the conversion is done one step at a time; `(Fun) (App) RE`,
for instance, is equivalent to `(Fun) (Mod) (App) RE`. Now assume that the
interpretation of `Expression` is a set of constants (functions, modules,
applications or releases). If the named type is more general than the expression
type, say `Mod` and `Fun` respectively, then the interpretation of the cast
expression is the set of modules that have at least one of their functions
mentioned in the interpretation of the expression. If the named type is more
special than the expression type, say `Fun` and `Mod`, then the interpretation
is the set of all the functions of the modules (in `modules` mode, the
conversion is partial since the local functions are not known). The conversions
to and from applications and releases work analogously. For instance,
`(App) "xref_.*" : Mod` returns all applications containing at least one module
such that `xref_` is a prefix of the module name.

Now assume that the interpretation of `Expression` is a set of calls. If the
named type is more general than the expression type, say `Mod` and `Fun`
respectively, then the interpretation of the cast expression is the set of calls
(M1, M2) such that the interpretation of the expression contains a call from
some function of M1 to some function of M2. If the named type is more special
than the expression type, say `Fun` and `Mod`, then the interpretation is the
set of all function calls (F1, F2) such that the interpretation of the
expression contains a call (M1, M2) and F1 is a function of M1 and F2 is a
function of M2 (in `modules` mode, there are no functions calls, so a cast to
`Fun` always yields an empty set). Again, the conversions to and from
applications and releases work analogously.

The interpretation of constants and variables are sets, and those sets can be
used as the basis for forming new sets by the application of _set operators_{:
#set_operator }. The syntax:

- Expression ::= Expression BinarySetOp Expression
- BinarySetOp ::= `+` | `*` | `-`

`+`, `*` and `-` are interpreted as union, intersection and difference
respectively: the union of two sets contains the elements of both sets; the
intersection of two sets contains the elements common to both sets; and the
difference of two sets contains the elements of the first set that are not
members of the second set. The elements of the two sets must be of the same
structure; for instance, a function call cannot be combined with a function. But
if a cast operator can make the elements compatible, then the more general
elements are converted to the less general element type. For instance, `M + F`
is equivalent to `(Fun) M + F`, and `E - AE` is equivalent to `E - (Fun) AE`.
One more example: `X * xref : Mod` is interpreted as the set of functions
exported by the module `xref`; `xref : Mod` is converted to the more special
type of `X` (`Fun`, that is) yielding all functions of `xref`, and the
intersection with `X` (all functions exported by analyzed modules and library
modules) is interpreted as those functions that are exported by some module
_and_ functions of `xref`.

There are also unary set operators:

- Expression ::= UnarySetOp Expression
- UnarySetOp ::= `domain` | `range` | `strict`

Recall that a call is a pair (From, To). `domain` applied to a set of calls is
interpreted as the set of all vertices From, and `range` as the set of all
vertices To. The interpretation of the `strict` operator is the operand with all
calls of the form (A, A) removed.

The interpretation of the _restriction operators_{: #restriction } is a subset
of the first operand, a set of calls. The second operand, a set of vertices, is
converted to the type of the first operand. The syntax of the restriction
operators:

- Expression ::= Expression RestrOp Expression
- RestrOp ::= `|`
- RestrOp ::= `||`
- RestrOp ::= `|||`

The interpretation in some detail for the three operators:

- **`|`** - The subset of calls from any of the vertices.

- **`||`** - The subset of calls to any of the vertices.

- **`|||`** - The subset of calls to and from any of the vertices. For all sets
  of calls `CS` and all sets of vertices `VS`, `CS ||| VS ` is equivalent to
  `CS | VS * CS || VS`.

[](){: #graph_analyses } Two functions (modules, applications, releases) belong
to the same strongly connected component if they call each other (in)directly.
The interpretation of the `components` operator is the set of strongly connected
components of a set of calls. The `condensation` of a set of calls is a new set
of calls between the strongly connected components such that there is an edge
between two components if there is some constant of the first component that
calls some constant of the second component.

The interpretation of the `of` operator is a chain of calls of the second
operand (a set of calls) that passes throw all of the vertices of the first
operand (a tuple of constants), in the given order. The second operand is
converted to the type of the first operand. For instance, the `of` operator can
be used for finding out whether a function calls another function indirectly,
and the chain of calls demonstrates how. The syntax of the graph analyzing
operators:

- Expression ::= Expression BinaryGraphOp Expression
- Expression ::= UnaryGraphOp Expression
- UnaryGraphOp ::= `components` | `condensation`
- BinaryGraphOp ::= `of`

As was mentioned before, the graph analyses operate on the `digraph`
representation of graphs. By default, the `digraph` representation is created
when needed (and deleted when no longer used), but it can also be created
explicitly by use of the `closure` operator:

- Expression ::= ClosureOp Expression
- ClosureOp ::= `closure`

The interpretation of the `closure` operator is the transitive closure of the
operand.

The restriction operators are defined for closures as well;
`closure E | xref : Mod` is interpreted as the direct or indirect function calls
from the `xref` module, while the interpretation of `E | xref : Mod` is the set
of direct calls from `xref`. If some graph is to be used in several graph
analyses, it saves time to assign the `digraph` representation of the graph to a
user variable, and then make sure that every graph analysis operates on that
variable instead of the list representation of the graph.

The lines where functions are defined (more precisely: where the first clause
begins) and the lines where functions are used are available in `functions`
mode. The line numbers refer to the files where the functions are defined. This
holds also for files included with the `-include` and `-include_lib` directives,
which may result in functions defined apparently in the same line. The _line
operators_ are used for assigning line numbers to functions and for assigning
sets of line numbers to function calls. The syntax is similar to the one of the
cast operator:

- Expression ::= `(` LineOp`)` Expression
- Expression ::= `(` XLineOp`)` Expression
- LineOp ::= `Lin` | `ELin` | `LLin` | `XLin`
- XLineOp ::= `XXL`

The interpretation of the `Lin` operator applied to a set of functions assigns
to each function the line number where the function is defined. Unknown
functions and functions of library modules are assigned the number 0.

The interpretation of some LineOp operator applied to a set of function calls
assigns to each call the set of line numbers where the first function calls the
second function. Not all calls are assigned line numbers by all operators:

- the `Lin` operator is defined for Call Graph Edges;
- the `LLin` operator is defined for Local Calls.
- the `XLin` operator is defined for External Calls.
- the `ELin` operator is defined for Inter Call Graph Edges.

The `Lin` (`LLin`, `XLin`) operator assigns the lines where calls (local calls,
external calls) are made. The `ELin` operator assigns to each call (From, To),
for which it is defined, every line L such that there is a chain of calls from
From to To beginning with a call on line L.

The `XXL` operator is defined for the interpretation of any of the LineOp
operators applied to a set of function calls. The result is that of replacing
the function call with a line numbered function call, that is, each of the two
functions of the call is replaced by a pair of the function and the line where
the function is defined. The effect of the `XXL` operator can be undone by the
LineOp operators. For instance, `(Lin) (XXL) (Lin) E` is equivalent to
`(Lin) E`.

The `+`, `-`, `*`, and `#` operators are defined for line number expressions,
provided the operands are compatible. The LineOp operators are also defined for
modules, applications, and releases; the operand is implicitly converted to
functions. Similarly, the cast operator is defined for the interpretation of the
LineOp operators.

The interpretation of the _counting operator_{: #count } is the number of
elements of a set. The operator is undefined for closures. The `+`, `-` and `*`
operators are interpreted as the obvious arithmetical operators when applied to
numbers. The syntax of the counting operator:

- Expression ::= CountOp Expression
- CountOp ::= `#`

All binary operators are left associative; for instance, `A | B  || C` is
equivalent to `(A | B) || C`. The following is a list of all operators, in
increasing order of _precedence_{: #precedence }:

- `+`, `-`
- `*`
- `#`
- `|`, `||`, `|||`
- `of`
- `(`Type`)`
- `closure`, `components`, `condensation`, `domain`, `range`, `strict`

Parentheses are used for grouping, either to make an expression more readable or
to override the default precedence of operators:

- Expression ::= `(` Expression `)`

A _query_{: #query } is a non-empty sequence of statements. A statement is
either an assignment of a user variable or an expression. The value of an
assignment is the value of the right hand side expression. It makes no sense to
put a plain expression anywhere else but last in queries. The syntax of queries
is summarized by these productions:

- Query ::= Statement`,` ...
- Statement ::= Assignment | Expression
- Assignment ::= Variable `:=` Expression | Variable `=` Expression

A variable cannot be assigned a new value unless first removed. Variables
assigned to by the `=` operator are removed at the end of the query, while
variables assigned to by the `:=` operator can only be removed by calls to
`forget`. There are no user variables when module data need to be set up again;
if any of the functions that make it necessary to set up module data again is
called, all user variables are forgotten.

## See Also

`m:beam_lib`, `m:digraph`, `m:digraph_utils`, `m:re`,
[User's Guide for Xref](xref_chapter.md)
""".

-behaviour(gen_server).

%% External exports
-export([start/1, start/2, stop/1]).

-export([m/1, d/1,
	 add_release/2, add_release/3,
	 add_application/2, add_application/3, 
	 add_module/2, add_module/3,
	 add_directory/2, add_directory/3,
	 replace_module/3, replace_module/4,
	 replace_application/3, replace_application/4,
	 remove_module/2, remove_application/2, remove_release/2,
	 get_library_path/1, set_library_path/2, set_library_path/3,
	 q/2, q/3, info/1, info/2, info/3, 
	 update/1, update/2, 
	 forget/1, forget/2, variables/1, variables/2,
	 analyze/2, analyze/3, analyse/2, analyse/3,
	 get_default/1, get_default/2, 
	 set_default/2, set_default/3]).

-export([format_error/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-import(lists, [keydelete/3, keysearch/3]).

-import(sofs, [to_external/1, is_sofs_set/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-type application() :: atom().
-type call() :: {atom(), atom()} | funcall().
-type constant() :: xmfa() | module() | application() | release().
-type directory() :: atom() | file:filename().
-type file() :: file:filename().
-type file_error() :: atom().
-type funcall() :: {xmfa(), xmfa()}.
-type function_name() :: atom().
-type library() :: atom().
-type library_path() :: path() | 'code_path'.
-type mode() :: 'functions' | 'modules'.
-type path() :: [file()].
-type release() :: atom().
-type string_position() :: pos_integer().
-type variable() :: atom().
-type xarity() :: arity() | -1.
-type xmfa() :: {module(), function_name(), xarity()}.
-type xref() :: atom() | pid().

-doc """
The given BEAM file (with or without the `.beam` extension) or the file found by
calling `code:which(Module)` is checked for calls to
[deprecated functions](`m:xref#deprecated_function`), calls to
[undefined functions](`m:xref#undefined_function`), and for unused local
functions.

The code path is used as [library path](`m:xref#library_path`).

If the BEAM file contains [debug information](`m:xref#debug_info`), a list
of tuples is returned. The first element of each tuple is one of:

- `deprecated`, the second element is a sorted list of calls to deprecated
  functions;
- `undefined`, the second element is a sorted list of calls to undefined
  functions;
- `unused`, the second element is a sorted list of unused local functions.

If the BEAM file does not contain debug information, a list of tuples is
returned. The first element of each tuple is one of:

- `deprecated`, the second element is a sorted list of externally used
  deprecated functions;
- `undefined`, the second element is a sorted list of undefined functions.
""".
-spec m(FileOrModule) -> [DebugInfoResult] |
                   [NoDebugInfoResult] |
                   {'error', module(), Reason} when
      FileOrModule :: file:filename() | module(),
      DebugInfoResult :: {'deprecated', [funcall()]}
                       | {'undefined', [funcall()]}
                       | {'unused', [mfa()]},
      NoDebugInfoResult :: {'deprecated', [xmfa()]}
                         | {'undefined', [xmfa()]},
      Reason :: {'cover_compiled', Module :: module()}
              | {'file_error', file(), file_error()}
              | {'interpreted', Module :: module()}
              | {'invalid_filename', term()}
              | {'no_such_module', Module :: module()}
              | beam_lib:chnk_rsn().

%% No user variables have been assigned digraphs, so there is no
%% need to call xref_base:delete/1.
m(Module) when is_atom(Module) ->
    case xref_utils:find_beam(Module) of
	{ok, File} ->
	    Fun = fun(S) -> 
                          xref_base:add_module(S, File, {builtins,true})
                  end,
	    case catch do_functions_analysis(Fun) of
		{error, _, {no_debug_info, _}} ->
		    catch do_modules_analysis(Fun);
		Result ->
		    Result
	    end;
	Error -> Error
    end;
m(File) ->
    case xref_utils:split_filename(File, ".beam") of
	false ->
	    {error, xref_base, {invalid_filename, File}};
	{Dir, BaseName} ->
	    BeamFile = filename:join(Dir, BaseName),
	    Fun = fun(S) -> 
                          xref_base:add_module(S, BeamFile, {builtins, true})
                  end,
	    case catch do_functions_analysis(Fun) of
		{error, _, {no_debug_info, _}} ->
		    catch do_modules_analysis(Fun);
		Result ->
		    Result
	    end
    end.

-doc """
The modules found in the given directory are checked for calls to
[deprecated functions](`m:xref#deprecated_function`), calls to
[undefined functions](`m:xref#undefined_function`), and for unused local
functions.

The code path is used as [library path](`m:xref#library_path`).

If some of the found BEAM files contain
[debug information](`m:xref#debug_info`), then those modules are checked and a
list of tuples is returned. The first element of each tuple is one of:

- `deprecated`, the second element is a sorted list of calls to deprecated
  functions;
- `undefined`, the second element is a sorted list of calls to undefined
  functions;
- `unused`, the second element is a sorted list of unused local functions.

If no BEAM file contains debug information, then a list of tuples is returned.
The first element of each tuple is one of:

- `deprecated`, the second element is a sorted list of externally used
  deprecated functions;
- `undefined`, the second element is a sorted list of undefined functions.
""".
-spec d(Directory) -> [DebugInfoResult] |
                      [NoDebugInfoResult] |
                      {'error', module(), Reason} when
      Directory :: directory(),
      DebugInfoResult :: {'deprecated', [funcall()]}
                       | {'undefined', [funcall()]}
                       | {'unused', [mfa()]},
      NoDebugInfoResult :: {'deprecated', [xmfa()]}
                         | {'undefined', [xmfa()]},
      Reason :: {'file_error', file(), file_error()}
              | {'invalid_filename', term()}
              | {'unrecognized_file', file()}
              | beam_lib:chnk_rsn().

d(Directory) ->
    Fun = fun(S) ->
                  xref_base:add_directory(S, Directory, {builtins, true})
          end,
    Fun1 = fun(S) ->
		   case Fun(S) of
		       {ok, [], _S} -> 
			   no_modules;
		       Reply -> 
			   Reply
		   end
	   end,
    case catch do_functions_analysis(Fun1) of
	no_modules ->
	    catch do_modules_analysis(Fun);
	Result -> 
	    Result
    end.

-doc """
Creates an [Xref server](`m:xref#xref_server`).

The process can optionally be given a name. The default
[mode](`m:xref#mode`) is `functions`. Options that are
not recognized by Xref are passed on to `gen_server:start/4`.
""".
-spec start(NameOrOptions) ->
                   {'ok', pid()} |
                   {'error', {'already_started', pid()}} when
      NameOrOptions :: Name | Options,
      Name :: atom(),
      Options :: Option | [Option],
      Option :: {'xref_mode', mode()} | term().

start(Name) when is_atom(Name) ->
    start(Name, []);
start(Opts0) when is_list(Opts0) ->
    {Args, Opts} = split_args(Opts0),
    gen_server:start(xref, Args, Opts).

-doc """
Creates an [Xref server](`m:xref#xref_server`) with a given name.

The default [mode](`m:xref#mode`) is `functions`. Options that are
not recognized by Xref are passed on to `gen_server:start/4`.
""".
-spec start(Name, Options) ->
                   {'ok', pid()} |
                   {'error', {'already_started', pid()}} when
      Name :: atom(),
      Options :: Option | [Option],
      Option :: {'xref_mode', mode()} | term().

start(Name, Opts0) when is_list(Opts0) ->
    {Args, Opts} = split_args(Opts0),
    gen_server:start({local, Name}, xref, Args, Opts);
start(Name, Opt) ->
    start(Name, [Opt]).

split_args(Opts) ->
    case keysearch(xref_mode, 1, Opts) of
	{value, Mode} ->
	    {[Mode], keydelete(xref_mode, 1, Opts)};
	false ->
	    {[], Opts}
    end.

-doc "Stops an [Xref server](`m:xref#xref_server`).".
-spec stop(XrefServer) -> 'stopped' when
      XrefServer :: xref().

stop(Name) ->
    try gen_server:call(Name, stop, infinity)
    after catch unregister(Name) % ensure the name is gone
    end.

-doc #{equiv => add_release(Name, Directory, [])}.
-spec add_release(XrefServer, Directory) ->
                         {'ok', release()} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | {'release_clash', {release(), directory(), directory()}}
              | add_dir_rsn().

add_release(Name, Dir) ->
    gen_server:call(Name, {add_release, Dir}, infinity).

-doc """
Adds a release, the applications of the release, the modules of the
applications, and [module data](`m:xref#module_data`) of the modules to an
[Xref server](`m:xref#xref_server`).

The applications will be members of the release, and the modules will
be members of the applications. The default is to use the base name of
the directory as release name, but this can be overridden by the
`name` option. Returns the name of the release.

If the given directory has a subdirectory named `lib`, the directories in that
directory are assumed to be application directories, otherwise all
subdirectories of the given directory are assumed to be application directories.
If there are several versions of some application, the one with the highest
version is chosen.

If the [mode](`m:xref#mode`) of the Xref server is `functions`, BEAM files that
contain no [debug information](`m:xref#debug_info`) are ignored.
""".
-spec add_release(XrefServer, Directory, Options) ->
                         {'ok', release()} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'name', release()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | {'release_clash', {release(), directory(), directory()}}
              | add_dir_rsn().

add_release(Name, Dir, Options) ->
    gen_server:call(Name, {add_release, Dir, Options}, infinity).

-doc #{equiv => add_application(XrefServer, Directory, [])}.
-spec add_application(XrefServer, Directory) ->
                             {'ok', application()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | add_dir_rsn().

add_application(Name, Dir) ->
    gen_server:call(Name, {add_application, Dir}, infinity).

-doc """
Adds an application, the modules of the application, and
[module data](`m:xref#module_data`) of the modules to an
[Xref server](`m:xref#xref_server`).

The modules will be members of the application. The default is to use
the base name of the directory with the version removed as application
name, but this can be overridden by the `name` option. Returns the
name of the application.

If the given directory has a subdirectory named `ebin`, modules (BEAM files) are
searched for in that directory, otherwise modules are searched for in the given
directory.

If the [mode](`m:xref#mode`) of the Xref server is `functions`, BEAM files that
contain no [debug information](`m:xref#debug_info`) are ignored.
""".
-spec add_application(XrefServer, Directory, Options) ->
                             {'ok', application()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'name', application()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | add_dir_rsn().

add_application(Name, Dir, Options) ->
    gen_server:call(Name, {add_application, Dir, Options}, infinity).

-type add_mod_rsn() ::
        {'file_error', file(), file_error()}
      | {'invalid_filename', term()}
      | {'invalid_options', term()}
      | {'module_clash', {module(), file(), file()}}
      | {'no_debug_info', file()}
      | beam_lib:chnk_rsn().

-doc #{equiv => add_module(Name, File, [])}.
-spec add_module(XrefServer, File) -> 
                             {'ok', module()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      File :: file:filename(),
      Reason :: add_mod_rsn().

add_module(Name, File) ->
    gen_server:call(Name, {add_module, File}, infinity).

-doc """
Adds a module and its [module data](`m:xref#module_data`) to an
[Xref server](`m:xref#xref_server`).

The module will not be member of any application. Returns the name of the module.

If the [mode](`m:xref#mode`) of the Xref server is `functions`, and the BEAM
file contains no [debug information](`m:xref#debug_info`), the error message
`no_debug_info` is returned.
""".
-spec add_module(XrefServer, File, Options) -> 
                             {'ok', module()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      File :: file:filename(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: add_mod_rsn().

add_module(Name, File, Options) ->
    gen_server:call(Name, {add_module, File, Options}, infinity).

-type add_dir_rsn() ::
        {'file_error', file(), file_error()}
      | {'invalid_filename', term()}
      | {'invalid_options', term()}
      | {'unrecognized_file', file()}
      | beam_lib:chnk_rsn().

-doc #{equiv => add_directory(Name, Directory, [])}.
-spec add_directory(XrefServer, Directory) ->
                           {'ok', Modules} |
                           {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Modules :: [module()],
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | add_dir_rsn().

add_directory(Name, Dir) ->
    gen_server:call(Name, {add_directory, Dir}, infinity).

-doc """
Adds the modules found in the given directory and the
[modules' data](`m:xref#module_data`) to an [Xref server](`m:xref#xref_server`).

The default is not to examine subdirectories, but if the option `recurse` has
the value `true`, modules are searched for in subdirectories on all levels as
well as in the given directory. Returns a sorted list of the names of the added
modules.

The modules added will not be members of any applications.

If the [mode](`m:xref#mode`) of the Xref server is `functions`, BEAM files that
contain no [debug information](`m:xref#debug_info`) are ignored.
""".
-spec add_directory(XrefServer, Directory, Options) ->
                           {'ok', Modules} |
                           {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'recurse', boolean()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'recurse'
              | 'verbose'
              | 'warnings',
      Modules :: [module()],
      Reason :: add_dir_rsn().

add_directory(Name, Dir, Options) ->
    gen_server:call(Name, {add_directory, Dir, Options}, infinity).

-doc #{equiv => replace_module(XrefServer, Module, File, [])}.
-spec replace_module(XrefServer, Module, File) ->
                                 {'ok', Module} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Module :: module(),
      File :: file(),
      Reason :: {'module_mismatch', Module, ReadModule :: module()}
              | {'no_such_module', Module}
              | add_mod_rsn().

replace_module(Name, Module, File) ->
    gen_server:call(Name, {replace_module, Module, File}, infinity).

-doc """
Replaces [module data](`m:xref#module_data`) of an
[analyzed module](`m:xref#analyzed_module`) with data read from a BEAM file.

Application membership of the module is retained, and so is the value of the
`builtins` option of the module. An error is returned if the name of the read
module differs from the given module.

The `update` function is an alternative for updating module data of recompiled
modules.
""".
-spec replace_module(XrefServer, Module, File, Options) ->
                                 {'ok', Module} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Module :: module(),
      File :: file(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'verbose'
              | 'warnings',
      Reason :: {'module_mismatch', Module, ReadModule :: module()}
              | {'no_such_module', Module}
              | add_mod_rsn().

replace_module(Name, Module, File, Options) ->
    gen_server:call(Name, {replace_module, Module, File, Options}, infinity).

-doc #{equiv => replace_application(XrefServer, Application, Directory, [])}.
-spec replace_application(XrefServer, Application, Directory) ->
                                 {'ok', Application} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Application :: application(),
      Directory :: directory(),
      Reason :: {'no_such_application', Application}
              | add_dir_rsn().

replace_application(Name, App, Dir) ->
    gen_server:call(Name, {replace_application, App, Dir}, infinity).

-doc """
Replaces the modules of an application with other modules read from an
application directory.

Release membership of the application is retained. Note that the name
of the application is kept; the name of the given directory is not
used.
""".
-spec replace_application(XrefServer, Application, Directory, Options) ->
                                 {'ok', Application} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Application :: application(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | {'no_such_application', Application}
              | add_dir_rsn().

replace_application(Name, App, Dir, Options) ->
    gen_server:call(Name, {replace_application, App, Dir, Options}, infinity).

-doc """
Removes [analyzed modules](`m:xref#analyzed_module`) and
[module data](`m:xref#module_data`) from an [Xref server](`m:xref#xref_server`).
""".
-spec remove_module(XrefServer, Modules) ->
                                'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Modules :: module() | [module()],
      Reason :: {'no_such_module', module()}.

remove_module(Name, Mod) ->
    gen_server:call(Name, {remove_module, Mod}, infinity).

-doc """
Removes applications and their modules and [module data](`m:xref#module_data`)
from an [Xref server](`m:xref#xref_server`).
""".
-spec remove_application(XrefServer, Applications) ->
                                'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Applications :: application() | [application()],
      Reason :: {'no_such_application', application()}.

remove_application(Name, App) ->
    gen_server:call(Name, {remove_application, App}, infinity).

-doc """
Removes releases and their applications, modules, and
[module data](`m:xref#module_data`) from an [Xref server](`m:xref#xref_server`).
""".
-spec remove_release(XrefServer, Releases) ->
                                'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Releases :: release() | [release()],
      Reason :: {'no_such_release', release()}.

remove_release(Name, Rel) ->
    gen_server:call(Name, {remove_release, Rel}, infinity).

-doc "Returns the [library path](`m:xref#library_path`).".
-spec get_library_path(XrefServer) -> {'ok', LibraryPath} when
      XrefServer :: xref(),
      LibraryPath :: library_path().

get_library_path(Name) ->
    gen_server:call(Name, get_library_path, infinity).

-doc #{equiv => set_library_path(XrefServer, LibraryPath, [])}.
-spec set_library_path(XrefServer, LibraryPath) ->
                              'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      LibraryPath :: library_path(),
      Reason :: {'file_error', file(), file_error()}
              | {'invalid_options', term()}
              | {'invalid_path', term()}.

set_library_path(Name, Path) ->
    gen_server:call(Name, {set_library_path, Path}, infinity).

-doc """
Sets the [library path](`m:xref#library_path`).

If the given path is a list of directories, the set of [library
modules](`m:xref#library_module`) is determined by choosing the first
module encountered while traversing the directories in the given
order, for those modules that occur in more than one directory. By
default, the library path is an empty list.

The library path `code_path`{: #code_path } is used by the functions
[`m/1`](`m/1`) and [`d/1`](`d/1`), but can also be set explicitly. However,
note that the code path will be traversed once for each used
[library module](`m:xref#library_module`) while setting up module data. On the
other hand, if there are only a few modules that are used but not analyzed,
using `code_path` may be faster than setting the library path to
`code:get_path/0`.

If the library path is set to `code_path`, the set of library modules is not
determined, and the `info` functions will return empty lists of library modules.
""".
-spec set_library_path(XrefServer, LibraryPath, Options) ->
                              'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      LibraryPath :: library_path(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Reason :: {'invalid_options', term()}
              | {'invalid_path', term()}.

set_library_path(Name, Path, Options) ->
    gen_server:call(Name, {set_library_path, Path, Options}, infinity).

-type info() :: {'application', Application :: [application()]}
              | {'builtins', boolean()}
              | {'directory', directory()}
              | {'library_path', library_path()}
              | {'mode', mode()}
              | {'no_analyzed_modules', non_neg_integer()}
              | {'no_applications', non_neg_integer()}
              | {'no_calls', {NoResolved :: non_neg_integer(),
                              NoUnresolved :: non_neg_integer()}}
              | {'no_function_calls',
                 {NoLocal :: non_neg_integer(),
                  NoResolvedExternal :: non_neg_integer(),
                  NoUnresolved :: non_neg_integer()}}
              | {'no_functions', {NoLocal :: non_neg_integer(),
                                  NoExternal :: non_neg_integer()}}
              | {'no_inter_function_calls', non_neg_integer()}
              | {'no_releases', non_neg_integer()}
              | {'release', Release :: [release()]}
              | {'version', Version :: [non_neg_integer()]}.

-doc """
The `info/1` function returns information as a list of pairs `{Tag, term()` in
some order about the state and the [module data](`m:xref#module_data`) of an
[Xref server](`m:xref#xref_server`).

[`info/1`](`info/1`) returns information with the following tags (tags marked
with (\*) are only available in `functions` mode):

- `library_path`, the [library path](`m:xref#library_path`);
- `mode`, the [mode](`m:xref#mode`);
- `no_releases`, number of releases;
- `no_applications`, total number of applications (of all releases);
- `no_analyzed_modules`, total number of
  [analyzed modules](`m:xref#analyzed_module`);
- `no_calls` (\*), total number of calls (in all modules), regarding instances
  of one function call in different lines as separate calls;
- `no_function_calls` (\*), total number of [local calls](`m:xref#local_call`),
  resolved [external calls](`m:xref#external_call`) and
  [unresolved calls](`m:xref#unresolved_call`);
- `no_functions` (\*), total number of local and exported functions;
- `no_inter_function_calls` (\*), total number of calls of the
  [Inter Call Graph](`m:xref#inter_call_graph`).
""".
-spec info(XrefServer) -> [Info] when
      XrefServer :: xref(),
      Info :: info().

info(Name) ->
    gen_server:call(Name, info, infinity).

-doc """
Returns information about all items belonging to category `Category`.
See `info/3` for details.
""".
-spec info(XrefServer, Category) ->
                  [{Item, [Info]}] |
                  {'error', module(), {'no_such_info', Category}} when
      XrefServer :: xref(),
      Category :: 'modules' | 'applications' | 'releases' | 'libraries',
      Item :: module() | application() | release() | library(),
      Info :: info().

info(Name, What) ->
    gen_server:call(Name, {info, What}, infinity).

-doc """
The `info` functions return information as a list of pairs `{Tag, term()}` in
some order about the state and the [module data](`m:xref#module_data`) of an
[Xref server](`m:xref#xref_server`).

[`info/2`](`info/2`) and [`info/3`](`info/3`) return information about all or
some of the analyzed modules, applications, releases, or library modules of an
Xref server. The following information is returned for every analyzed module:

- `application`, an empty list if the module does not belong to any application,
  otherwise a list of the application name;
- `builtins`, whether calls to BIFs are included in the module's data;
- `directory`, the directory where the module's BEAM file is located;
- `no_calls` (\*), number of calls, regarding instances of one function call in
  different lines as separate calls;
- `no_function_calls` (\*), number of local calls, resolved external calls and
  unresolved calls;
- `no_functions` (\*), number of local and exported functions;
- `no_inter_function_calls` (\*), number of calls of the Inter Call Graph;

The following information is returned for every application:

- `directory`, the directory where the modules' BEAM files are located;
- `no_analyzed_modules`, number of analyzed modules;
- `no_calls` (\*), number of calls of the application's modules, regarding
  instances of one function call in different lines as separate calls;
- `no_function_calls` (\*), number of local calls, resolved external calls and
  unresolved calls of the application's modules;
- `no_functions` (\*), number of local and exported functions of the
  application's modules;
- `no_inter_function_calls` (\*), number of calls of the Inter Call Graph of the
  application's modules;
- `release`, an empty list if the application does not belong to any release,
  otherwise a list of the release name;
- `version`, the application's version as a list of numbers. For instance, the
  directory "kernel-2.6" results in the application name `kernel` and the
  application version \[2,6]; "kernel" yields the name `kernel` and the version
  [].

The following information is returned for every release:

- `directory`, the release directory;
- `no_analyzed_modules`, number of analyzed modules;
- `no_applications`, number of applications;
- `no_calls` (\*), number of calls of the release's modules, regarding instances
  of one function call in different lines as separate calls;
- `no_function_calls` (\*), number of local calls, resolved external calls and
  unresolved calls of the release's modules;
- `no_functions` (\*), number of local and exported functions of the release's
  modules;
- `no_inter_function_calls` (\*), number of calls of the Inter Call Graph of the
  release's modules.

The following information is returned for every library module:

- `directory`, the directory where the
  [library module's](`m:xref#library_module`) BEAM file is located.

For every number of calls, functions, and so on returned by the `no_` tags, there is a
query returning the same number. Listed below are examples of such queries. Some
of the queries return the sum of a two or more of the `no_` tags numbers. `mod`
(`app`, `rel`) refers to any module (application, release).

- `no_analyzed_modules`

  - `"# AM"` (info/1)
  - `"# (Mod) app:App"` (application)
  - `"# (Mod) rel:Rel"` (release)

- `no_applications`

  - `"# A"` (info/1)

- `no_calls`. The sum of the number of resolved and unresolved calls:

  - `"# (XLin) E + # (LLin) E"` (info/1)
  - `"T = E | mod:Mod, # (LLin) T + # (XLin) T"` (module)
  - `"T = E | app:App, # (LLin) T + # (XLin) T"` (application)
  - `"T = E | rel:Rel, # (LLin) T + # (XLin) T"` (release)

- `no_functions`. Functions in library modules and the functions
  `module_info/0,1` are not counted by `info`. Assuming that
  `"Extra := _:module_info/\"(0|1)\" + LM"` has been evaluated, the sum of the
  number of local and exported functions are:

  - `"# (F - Extra)"` (info/1)
  - `"# (F * mod:Mod - Extra)"` (module)
  - `"# (F * app:App - Extra)"` (application)
  - `"# (F * rel:Rel - Extra)"` (release)

- `no_function_calls`. The sum of the number of local calls, resolved external
  calls and unresolved calls:

  - `"# LC + # XC"` (info/1)
  - `"# LC | mod:Mod + # XC | mod:Mod"` (module)
  - `"# LC | app:App + # XC | app:App"` (application)
  - `"# LC | rel:Rel + # XC | mod:Rel"` (release)

- `no_inter_function_calls`

  - `"# EE"` (info/1)
  - `"# EE | mod:Mod"` (module)
  - `"# EE | app:App"` (application)
  - `"# EE | rel:Rel"` (release)

- `no_releases`

  - `"# R"` (info/1)
""".
-spec info(XrefServer, Category, Items) -> 
                  [{Item, [Info]}] |
                  {'error', module(), Reason} when
      XrefServer :: xref(),
      Category :: 'modules' | 'applications' | 'releases' | 'libraries',
      Items :: Item | [Item],
      Item :: module() | application() | release() | library(),
      Info :: info(),
      Reason :: {'no_such_application', Item}
              | {'no_such_info', Category}
              | {'no_such_library', Item}
              | {'no_such_module', Item}
              | {'no_such_release', Item}.

info(Name, What, Qual) ->
    gen_server:call(Name, {info, What, Qual}, infinity).

-doc #{equiv => update(XrefServer, [])}.
-spec update(XrefServer) ->
                    {'ok', Modules} |
                    {'error', module(), Reason} when
      XrefServer :: xref(),
      Modules :: [module()],
      Reason :: {'module_mismatch', module(), ReadModule :: module()}
              | add_mod_rsn(). % except 'invalid_options'

update(Name) ->
    gen_server:call(Name, update, infinity).

-doc """
Replaces the [module data](`m:xref#module_data`) of all
[analyzed modules](`m:xref#analyzed_module`) the BEAM files of which have been
modified since last read by an `add` function or `update`.

Application membership of the modules is retained, and so is the value
of the `builtins`option. Returns a sorted list of the names of
the replaced modules.
""".
-spec update(XrefServer, Options) ->
                    {'ok', Modules} |
                    {'error', module(), Reason} when
      XrefServer :: xref(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'verbose'
              | 'warnings',
      Modules :: [module()],
      Reason :: {'module_mismatch', module(), ReadModule :: module()}
              | add_mod_rsn().

update(Name, Options) ->
    gen_server:call(Name, {update, Options}, infinity).

-doc """
Removes all [user variables](`m:xref#user_variable`) of an
[Xref server](`m:xref#xref_server`).
""".
-spec forget(XrefServer) -> 'ok' when
      XrefServer :: xref().

forget(Name) ->
    gen_server:call(Name, forget, infinity).

-doc """
Removes the [user variables](`m:xref#user_variable`) given by `Variables` from
an [Xref server](`m:xref#xref_server`).
""".
-spec forget(XrefServer, Variables) -> 'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Variables :: variable() | [variable()],
      Reason :: {'not_user_variable', term()}.

forget(Name, Variable) ->
    gen_server:call(Name, {forget, Variable}, infinity).

-doc #{equiv => variables(XrefServer, [])}.
-spec variables(XrefServer) -> {'ok', [VariableInfo]} when
      XrefServer :: xref(),
      VariableInfo :: {'predefined', [variable()]}
                    | {'user', [variable()]}.

variables(Name) ->
    gen_server:call(Name, variables, infinity).

-doc """
Returns a sorted lists of the names of the variables of an
[Xref server](`m:xref#xref_server`).

The default is to return only the
[user variables](`m:xref#user_variable`).
""".
-spec variables(XrefServer, Options) -> {'ok', [VariableInfo]} when
      XrefServer :: xref(),
      Options :: Option | [Option],
      Option :: 'predefined'
              | 'user'
              | {'verbose', boolean()}
              | 'verbose',
      VariableInfo :: {'predefined', [variable()]}
                    | {'user', [variable()]}.

variables(Name, Options) ->
    gen_server:call(Name, {variables, Options}, infinity).

-doc false.
-spec analyse(XrefServer, Analysis) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyse(Name, What) ->
    gen_server:call(Name, {analyze, What}, infinity).

-doc false.
-spec analyse(XrefServer, Analysis, Options) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyse(Name, What, Options) ->
    gen_server:call(Name, {analyze, What, Options}, infinity).

-type app_spec() :: application() | [application()].
-type depr_flag() :: 'next_version'
                   | 'next_major_release'
                   | 'eventually'.
-type func_spec() :: xmfa() | [xmfa()].
-type mod_spec() :: module() | [module()].
-type rel_spec() :: release() | [release()].
-type analysis() :: 'undefined_function_calls'
                  | 'undefined_functions'
                  | 'locals_not_used'
                  | 'exports_not_used'
                  | 'deprecated_function_calls'
                  | {'deprecated_function_calls', DeprFlag :: depr_flag()}
                  | 'deprecated_functions'
                  | {'deprecated_functions', DeprFlag :: depr_flag()}
                  | {'call', FuncSpec :: func_spec()}
                  | {'use', FuncSpec :: func_spec()}
                  | {'module_call', ModSpec :: mod_spec()}
                  | {'module_use', ModSpec :: mod_spec()}
                  | {'application_call', AppSpec :: app_spec()}
                  | {'application_use', AppSpec :: app_spec()}
                  | {'release_call', RelSpec :: rel_spec()}
                  | {'release_use', RelSpec :: rel_spec()}.

-type analyze_rsn() :: {'invalid_options', term()}
                     | {'parse_error', string_position(), term()}
                     | {'unavailable_analysis', term()}
                     | {'unknown_analysis', term()}
                     | {'unknown_constant', string()}
                     | {'unknown_variable', variable()}.

-doc #{equiv => analyze(XrefServer, Analysis, [])}.
-spec analyze(XrefServer, Analysis) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Answer :: [term()],
      Reason :: analyze_rsn().

analyze(Name, What) ->
    gen_server:call(Name, {analyze, What}, infinity).

-doc """
[](){: #analyze } Evaluates a predefined analysis.

Returns a sorted list without duplicates of `t:call/0` or
`t:constant/0`, depending on the chosen analysis.  The predefined
analyses, which operate on all [analyzed
modules](`m:xref#analyzed_module`), are (analyses marked with (\*) are
available only in [mode `functions`](`m:xref#mode`)):

- **`undefined_function_calls`(\*)** - Returns a list of calls to
  [undefined functions](`m:xref#undefined_function`).

- **`undefined_functions`** - Returns a list of
  [undefined functions](`m:xref#undefined_function`).

- **`locals_not_used`(\*)** - Returns a list of local functions that have not
  been locally used.

- **`exports_not_used`** - Returns a list of exported functions that have not
  been externally used. Note that in `modules` mode, `M:behaviour_info/1` is
  never reported as unused.

- **`deprecated_function_calls`(\*)** - Returns a list of external calls to
  [deprecated functions](`m:xref#deprecated_function`).

- **`{deprecated_function_calls, DeprFlag}`(\*)** - Returns a list of external
  calls to deprecated functions. If `DeprFlag` is equal to `next_version`, calls
  to functions to be removed in next version are returned. If `DeprFlag` is
  equal to `next_major_release`, calls to functions to be removed in next major
  release are returned as well as calls to functions to be removed in next
  version. Finally, if `DeprFlag` is equal to `eventually`, all calls to
  functions to be removed are returned, including calls to functions to be
  removed in next version or next major release.

- **`deprecated_functions`** - Returns a list of externally used deprecated
  functions.

- **`{deprecated_functions, DeprFlag}`** - Returns a list of externally used
  deprecated functions. If `DeprFlag` is equal to `next_version`, functions to
  be removed in next version are returned. If `DeprFlag` is equal to
  `next_major_release`, functions to be removed in next major release are
  returned as well as functions to be removed in next version. Finally, if
  `DeprFlag` is equal to `eventually`, all functions to be removed are returned,
  including functions to be removed in next version or next major release.

- **`{call, FuncSpec}`(\*)** - Returns a list of functions called by some of the
  given functions.

- **`{use, FuncSpec}`(\*)** - Returns a list of functions that use some of the
  given functions.

- **`{module_call, ModSpec}`** - Returns a list of modules called by some of the
  given modules.

- **`{module_use, ModSpec}`** - Returns a list of modules that use some of the
  given modules.

- **`{application_call, AppSpec}`** - Returns a list of applications called by
  some of the given applications.

- **`{application_use, AppSpec}`** - Returns a list of applications that use
  some of the given applications.

- **`{release_call, RelSpec}`** - Returns a list of releases called by some of
  the given releases.

- **`{release_use, RelSpec}`** - Returns a list of releases that use some of the
  given releases.
""".
-spec analyze(XrefServer, Analysis, Options) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyze(Name, What, Options) ->
    gen_server:call(Name, {analyze, What, Options}, infinity).

-type component() :: [constant()].
-type define_at() :: {xmfa(), LineNumber :: non_neg_integer()}.
-type answer() :: 'false'
                | [constant()]
                | [(Call :: call()) |
                   (ComponentCall :: {component(), component()})]
                | [Component :: component()]
                | non_neg_integer()
                | [DefineAt :: define_at()]
                | [CallAt :: {funcall(), LineNumbers :: [non_neg_integer()]}]
                | [AllLines :: {{define_at(), define_at()},
                                LineNumbers :: [non_neg_integer()]}].
-type q_rsn() :: {'invalid_options', term()}
               | {'parse_error', string_position(), term()}
               | {'type_error', string()}
               | {'type_mismatch', string(), string()}
               | {'unknown_analysis', term()}
               | {'unknown_constant', string()}
               | {'unknown_variable', variable()}
               | {'variable_reassigned', string()}.

-doc #{equiv => q(XrefServer, Query, [])}.
-spec q(XrefServer, Query) ->
               {'ok', Answer} |
               {'error', module(), Reason} when
      XrefServer :: xref(),
      Query :: string() | atom(),
      Answer :: answer(),
      Reason :: q_rsn().

q(Name, Q) ->
    gen_server:call(Name, {qry, Q}, infinity).

-doc """
Evaluates a [query](`m:xref#query`) in the context of an
[Xref server](`m:xref#xref_server`), and returns the value of the last
statement.

The syntax of the value depends on the expression:

- A set of calls is represented by a sorted list without duplicates of
  `t:call/0`.
- A set of constants is represented by a sorted list without duplicates of
  `t:constant/0`.
- A set of strongly connected components is a sorted list without duplicates of
  `Component`.
- A set of calls between strongly connected components is a sorted list without
  duplicates of `ComponentCall`.
- A chain of calls is represented by a list of `t:constant/0`. The list contains
  the From vertex of every call and the To vertex of the last call.
- The `of` operator returns `false` if no chain of calls between the given
  constants can be found.
- The value of the `closure` operator (the `digraph` representation) is
  represented by the atom `'closure()'`.
- A set of line numbered functions is represented by a sorted list without
  duplicates of `DefineAt`.
- A set of line numbered function calls is represented by a sorted list without
  duplicates of `CallAt`.
- A set of line numbered functions and function calls is represented by a sorted
  list without duplicates of `AllLines`.

For both `CallAt` and `AllLines` it holds that for no list element is
`LineNumbers` an empty list; such elements have been removed. The constants of
`component` and the integers of `LineNumbers` are sorted and without duplicates.
""".
-spec q(XrefServer, Query, Options) ->
               {'ok', Answer} |
               {'error', module(), Reason} when
      XrefServer :: xref(),
      Query :: string() | atom(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Answer :: answer(),
      Reason :: q_rsn().

q(Name, Q, Options) ->
    gen_server:call(Name, {qry, Q, Options}, infinity).

-doc """
Returns a list of all options and their default values.
""".
-spec get_default(XrefServer) -> [{Option, Value}] when
      XrefServer :: xref(),
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean().

get_default(Name) ->
    gen_server:call(Name, get_default, infinity).

-doc "Returns the default value for option `Option`.".
-spec get_default(XrefServer, Option) ->
                         {'ok', Value} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean(),
      Reason :: {'invalid_options', term()}. % 'invalid_option'

get_default(Name, Option) ->
    gen_server:call(Name, {get_default, Option}, infinity).

-doc """
Sets default values for multiple options given by `OptionValues`.

See `set_default/3` for the name of options and their allowed values.
""".
-spec set_default(XrefServer, OptionValues) ->
                         'ok' |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      OptionValues :: OptionValue | [OptionValue],
      OptionValue :: {Option, Value},
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean(),
      Reason :: {'invalid_options', term()}.

set_default(Name, OptionValues) ->
    gen_server:call(Name, {set_default, OptionValues}, infinity).

-doc """
Sets the default value of one or more options.

The options that can be set this way are:

- `builtins`, with initial default value `false`;
- `recurse`, with initial default value `false`;
- `verbose`, with initial default value `false`;
- `warnings`, with initial default value `true`.

The initial default values are set when creating an
[Xref server](`m:xref#xref_server`).
""".
-spec set_default(XrefServer, Option, Value) ->
                         {'ok', OldValue} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean(),
      OldValue :: boolean(),
      Reason :: {'invalid_options', term()}.

set_default(Name, Option, Value) ->
    gen_server:call(Name, {set_default, Option, Value}, infinity).

-doc """
Given the error returned by any function of this module, the function
`format_error` returns a descriptive string of the error in English.

For file errors, the function `file:format_error/1` is called.
""".
-spec format_error(Error) -> io_lib:chars() when
      Error :: {'error', module(), Reason :: term()}.

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error(E) ->
    io_lib:format("~tp~n", [E]).

%%%----------------------------------------------------------------------
%%%Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
-doc false.
init(Args) ->
    case xref_base:new(Args) of
	{ok, S} ->
	    {ok, S};
	{error, _Module, Reason} ->
	    {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
-doc false.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({add_release, Dir}, _From, State) ->
    case xref_base:add_release(State, Dir) of
	{ok, ReleaseName, NewState} ->
	    {reply, {ok, ReleaseName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_release, Dir, Options}, _From, State) ->
    case xref_base:add_release(State, Dir, Options) of
	{ok, ReleaseName, NewState} ->
	    {reply, {ok, ReleaseName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_application, Dir}, _From, State) ->
    case xref_base:add_application(State, Dir) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_application, Dir, Options}, _From, State) ->
    case xref_base:add_application(State, Dir, Options) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_module, File}, _From, State) ->
    case xref_base:add_module(State, File) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error,  State}
    end;
handle_call({add_module, File, Options}, _From, State) ->
    case xref_base:add_module(State, File, Options) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error,  State}
    end;
handle_call({replace_application, Appl, Dir}, _From, State) ->
    case xref_base:replace_application(State, Appl, Dir) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_application, Appl, Dir, Opts}, _From, State) ->
    case xref_base:replace_application(State, Appl, Dir, Opts) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_module, Mod}, _From, State) ->
    case xref_base:remove_module(State, Mod) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_application, Appl}, _From, State) ->
    case xref_base:remove_application(State, Appl) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_release, Rel}, _From, State) ->
    case xref_base:remove_release(State, Rel) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_directory, Dir}, _From, State) ->
    case xref_base:add_directory(State, Dir) of
	{ok, Modules, NewState} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_directory, Dir, Options}, _From, State) ->
    case xref_base:add_directory(State, Dir, Options) of
	{ok, Modules, NewState} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(get_library_path, _From, State) ->
    Path = xref_base:get_library_path(State),
    {reply, Path, State};
handle_call({set_library_path, Path}, _From, State) ->
    case xref_base:set_library_path(State, Path) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({set_library_path, Path, Options}, _From, State) ->
    case xref_base:set_library_path(State, Path, Options) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_module, Module, File}, _From, State) ->
    case xref_base:replace_module(State, Module, File) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_module, Module, File, Options}, _From, State) ->
    case xref_base:replace_module(State, Module, File, Options) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(info, _From, State) ->
    {reply, xref_base:info(State), State};
handle_call({info, What}, _From, State) ->
    {reply, xref_base:info(State, What), State};
handle_call({info, What, Qual}, _From, State) ->
    {reply, xref_base:info(State, What, Qual), State};
handle_call(update, _From, State) ->
    case xref_base:update(State) of
	{ok, NewState, Modules} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({update, Options}, _From, State) ->
    case xref_base:update(State, Options) of
	{ok, NewState, Modules} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(forget, _From, State) ->
    {ok, NewState} = xref_base:forget(State),
    {reply, ok, NewState};
handle_call({forget, Variable}, _From, State) ->
    case xref_base:forget(State, Variable) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(variables, _From, State) ->
    %% The reason the ok-Error pattern is broken for variables, q and
    %% analyze is that the state may have been updated even if an
    %% error occurs.
    {Reply, NewState} = xref_base:variables(State),
    {reply, Reply, NewState};
handle_call({variables, Options}, _From, State) ->
    {Reply, NewState} = xref_base:variables(State, Options),
    {reply, Reply, NewState};
handle_call({analyze, What}, _From, State) ->
    {Reply, NewState} = xref_base:analyze(State, What),
    {reply, unsetify(Reply), NewState};
handle_call({analyze, What, Options}, _From, State) ->
    {Reply, NewState} = xref_base:analyze(State, What, Options),
    {reply, unsetify(Reply), NewState};
handle_call({qry, Q}, _From, State) ->
    {Reply, NewState} = xref_base:q(State, Q),
    {reply, unsetify(Reply), NewState};
handle_call({qry, Q, Options}, _From, State) ->
    {Reply, NewState} = xref_base:q(State, Q, Options),
    {reply, unsetify(Reply), NewState};
handle_call(get_default, _From, State) ->
    Reply = xref_base:get_default(State),
    {reply, Reply, State};
handle_call({get_default, Option}, _From, State) ->
    Reply = xref_base:get_default(State, Option),
    {reply, Reply, State};
handle_call({set_default, OptionValues}, _From, State) ->
    case xref_base:set_default(State, OptionValues) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({set_default, Option, Value}, _From, State) ->
    case xref_base:set_default(State, Option, Value) of
	{ok, OldValue, NewState} ->
	    {reply, {ok, OldValue}, NewState};
	Error ->
	    {reply, Error, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2 
%% Returns: {noreply, State} | 
%% {noreply, State, Timeout} | 
%% {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-doc false.
handle_cast(_Msg, State) -> {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
-doc false.
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-doc false.
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_functions_analysis(FFun) ->
    {ok, State} = xref_base:new(),
    {ok, State1} = xref_base:set_library_path(State, code_path),
    {ok, State2} = xref_base:set_default(State1, 
					 [{verbose,false},{warnings,false}]),
    State3 = case FFun(State2) of
		 {ok, _, S} -> S;
		 Error2 -> throw(Error2)
	     end,
    {Undef, State4} = do_analysis(State3, undefined_function_calls),
    {Unused, State5} = do_analysis(State4, locals_not_used),
    {Deprecated, _} = do_analysis(State5, deprecated_function_calls),
    [{deprecated,to_external(Deprecated)},
     {undefined,to_external(Undef)}, 
     {unused,to_external(Unused)}].

do_modules_analysis(FFun) ->
    {ok, State} = xref_base:new({xref_mode, modules}),
    {ok, State1} = xref_base:set_library_path(State, code_path),
    {ok, State2} = xref_base:set_default(State1, 
					 [{verbose,false},{warnings,false}]),
    State3 = case FFun(State2) of
		 {ok, _, S} -> S;
		 Error2 -> throw(Error2)
	     end,
    {Undef, State4} = do_analysis(State3, undefined_functions),
    {Deprecated, _} = do_analysis(State4, deprecated_functions),
    [{deprecated,to_external(Deprecated)},
     {undefined,to_external(Undef)}].

do_analysis(State, Analysis) ->
    case xref_base:analyze(State, Analysis) of
	{{ok, Reply}, NewState} ->
	    {Reply, NewState};
	{Error, _} ->
	    throw(Error)
    end.

unsetify(Reply={ok, X}) ->
    case is_sofs_set(X) of
	true -> {ok, to_external(X)};
	false -> Reply
    end;
unsetify(Reply) ->
    Reply.
