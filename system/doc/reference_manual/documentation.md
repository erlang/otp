# Documentation

Documentation in Erlang is done through the `-moduledoc` and `-doc` [attributes][]. For example:

    -module(math).
    -moduledoc """
    A module for basic arithmetic.
    """.
    
    -export([add/2]).
    
    -doc "Adds two numbers together."
    add(One, Two) -> One + Two.

The `-moduledoc` attribute has to be located before the first `-doc` attribute or
function declaration. It documents the overall purpose of the module.

The `-doc` attribute always precedes the [function][] or [attribute][attributes] it documents.
The attributes that can be documented are [user-defined types][] (`-type` and `-opaque`) and
[behaviour module attributes][] (`-callback`).

By default the format used for documentation attributes is [Markdown][wikipedia]
but that can be changed by setting [module documentation metadata](#moduledoc-metadata).

A good starting point to writing Markdown is [Basic writing and formatting syntax][github].

For details on what is allowed to be part of the `-moduledoc` and `-doc` attributes, see
[Documentation Attributes][doc_attrs].

`-doc` attributes have been available since Erlang/OTP 27.

[attributes]: system/reference_manual:modules#module-attributes
[function]: system/reference_manual:functions
[user-defined types]: system/reference_manual:typespec#type-declarations-of-user-defined-types
[behaviour module attributes]: system/reference_manual:modules#behaviour-module-attribute
[Earmark]: https://github.com/robertdober/earmark_parser
[wikipedia]: https://en.wikipedia.org/wiki/Markdown
[github]: https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax
[doc_attrs]: system/reference_manual:modules#documentation-attributes

## Documentation metadata

It is possible to add metadata to the documentation entry. You do this by adding
a `-moduledoc` or `-doc` attribute with a map as argument. For example:

    -module(math).
    -moduledoc """
    A module for basic arithmetic.
    """.
    -moduledoc #{ since => "1.0" }.
    
    -export([add/2]).
    
    -doc "Adds two number together."
    -doc(#{ since => "1.0" }).
    add(One, Two) -> One + Two.

The metadata is used by documentation tools to provide extra information to
the user. There can be multiple metadata documentation entries, in which case
the maps will be merged with the latest taking precedence if there are
duplicate keys. Example:

    -doc "Adds two number together."
    -doc #{ since => "1.0", author => "Joe" }.
    -doc #{ since => "2.0" }.
    add(One, Two) -> One + Two.

This will result in a metadata entry of `#{ since => "2.0", author => "Joe" }`.

The keys and values in the metadata map can be any type, but it is recommended
that only [atoms][] are used for keys and [strings][] for the values.

[atoms]: data_types#atom
[strings]: data_types#string

## External documentation files

The `-moduledoc` and `-doc` can also be placed in external files. To do so use
`-doc {file, "path/to/doc.md"}` to point to the documentation. The path used is
relative to the file where the `-doc` attribute is located. For example:

    %% doc/add.md
    Adds two numbers together

and

    %% src/math.erl
    -doc({file, "../doc/add.md"}).
    add(One, Two) -> One + Two.

## Documenting a module

The module description should include details on how to use the API
and examples of the different functions working together. Here is a
good place to use images and other diagrams to better show the usage
of the module. Instead of writing a long text in the `moduledoc`
attribute, it could be better to break it out into an external page.

The `moduledoc` attribute should start with a short paragraph
describing the module and then go into greater details. For example:

    -module(math).
    -moduledoc """
       A module for basic arithmetic.

       This module can be used to add and subtract values. For example:

       ```
       1> math:subtract(math:add(2, 3), 1).
       4
       ```
       """.

### Moduledoc metadata

There are three reserved metadata keys for `-moduledoc`:

- `since` - Shows in which version of the application the module was added.
- `deprecated` - Shows a text in the documentation explaining that it is deprecated
  and what to use instead.
- `format` - The format to use for all documentation in this module.
  The default is `text/markdown`.
  It should be written using the [mime type][] of the format.

Example:

    -moduledoc {file, "../doc/math.asciidoc"}.
    -moduledoc #{ since => "0.1", format => "text/asciidoc" }.
    -moduledoc #{ deprecated => "Use the stdlib math module instead." }.

[mime type]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types

## Documenting functions, user-defined types, and callbacks

Functions, types, and callbacks can be documented using the `-doc` attribute.
Each entry should start with a short paragraph describing the purpose of entity,
and then go into greater detail in needed.

It is not recommended to include images or diagrams in this documentation as
it is used by IDEs and [c:h/1][] to show the documentation to the user.

For example:

    -doc """
    A number that can be used by the math module.
    
    We use a special number here so that we know
    that this number comes from this module.
    """.
    -opaque number() :: {math, erlang:number()}.
    
    -doc """
    Adds two number together.
    
    ### Example:
    
    ```
    1> math:add(math:number(1), math:number(2)).
    {number, 3}
    ```
    """.
    -spec add(number(), number()) -> number().
    add({number, One}, {number, Two}) -> {number, One + Two}.

[c:h/1]: seemfa/stdlib:c#h/1

### Doc metadata

There are four reserved metadata keys for `-doc`:

- `since => unicode:chardata()` - Shows which version of the application the module
  was added.
- `deprecated => unicode:chardata()` - Shows a text in the documentation explaining
  that it is deprecated and what to use instead. The compiler will automatically
  insert this key if there is a `-deprecated` attribute marking a function as deprecated.
- `equiv => unicode:chardata()` - Notes that this function is equivalent to another function
  in this module. The equivalence can be described using either `Func/Arity` or `Func(Args)`.
  For example:

        -doc #{ equiv => add/3 }.
        add(One, Two) -> add(One, Two, []).
        add(One, Two, Options) -> ...
    
    or
    
        -doc #{ equiv => add(One, Two, []) }.
        -spec add(One :: number(), Two :: number()) -> number().
        add(One, Two) -> add(One, Two, []).
        add(One, Two, Options) -> ...
    
    The entry into the [EEP-48][] doc chunk metadata is the value converted to a string.

- `exported => boolean()` - A [boolean/0][] signifying if the entry is `exported`
  or not. This value is automatically set by the compiler
  and should not be set by the user.

[boolean/0]: seetype/erts:erlang#boolean

### Doc slogans

The doc slogan is a short text shown to describe the function and its arguments.
By default it is determined by looking at the names of the arguments in the `-spec` or
function. For example:

    add(One, Two) -> One + Two.
    
    -spec sub(One :: integer(), Two :: integer()) -> integer().
    sub(X, Y) -> X - Y.

will have a slogan of `add(One, Two)` and `sub(One, Two)`.

For types or callbacks, the slogan is derived from the type or callback specification.
For example:

    -type number(Value) :: {number, Value}.
    %% slogan will be `number(Value)`
    
    -opaque number() :: {number, number()}.
    %% slogan will be `number()`
    
    -callback increment(In :: number()) -> Out.
    %% slogan will be `increment(In)`
    
    -callback increment(In) -> Out when
       In :: number().
    %% slogan will be `increment(In)`

If it is not possible to "easily" figure out a nice slogan from the code, the
MFA syntax is used instead. For example: `add/2`, `number/1`, `increment/1`

It is possible to supply a custom slogan by placing it as the first line of
the `-doc` attribute. The provided slogan must be in the form of a function
declaration up until the `->`. For example:

    -doc """
    add(One, Two)
    
    Adds two numbers.
    """.
    add(A, B) -> A + B.

Will create the slogan `add(One, Two)`. The slogan will be removed from the
documentation string, so in the example above only the text `"Adds two numbers"`
will be part of the documentation. This works for functions, types, and callbacks.

## Links in Markdown

When writing documentation in Markdown, links are automatically found in any
inline code segment that looks like an MFA. For example:

    -doc "See `sub/2` for more details".

will create a link to the `sub/2` function in the current module if it exists.
One can also use `` `sub/2` `` as the link target. For example:

    -doc "See [subtract](`sub/2`) for more details".
    -doc "See [`sub/2`] for more details".
    -doc """
    See [subtract] for more details

    [subtract]: `sub/2`
    """.
    -doc """
    See [subtract][1] for more details
    
    [1]: `sub/2`
    """.

The above examples result in the same link being created.

The link can also other entities:

- `remote functions` - Use `module:function/arity` syntax.
  
  Example:
  
      -doc "See `math:sub/2` for more details".
  
- `modules` - Write the module with a `m` prefix. Use anchors to
  jump to a specific place in the module.
  
  Example:
  
      -doc "See `m:math` for more details".
      -doc "See `m:math#anchor` for more details".

- `types` - Use the same syntax as for local/remote function but add a `t` prefix.
  
  Example:
  
      -doc "See `t:number/0` for more details".
      -doc "See `t:math:number/0` for more details".

- `callbacks` - Use the same syntax as for local/remote function but add a `c` prefix.
  
  Example:
  
      -doc "See `c:increment/0` for more details".
      -doc "See `c:math:increment/0` for more details".

- `extra pages` - For extra pages in the current application use a normal link,
  for example "`[release notes](notes.md)`".
  For extra pages in another application use the `e` prefix and state which
  application the page belongs to. One can also use anchors to jump to a specific
  place in the page.
  
  Example:
    
      -doc "See `e:stdlib:unicode_usage` for more details".
      -doc "See `e:stdlib:unicode_usage#notes-about-raw-filenames` for more details".

## What is visible versus hidden?

An Erlang [application][] normally consists of various public and private modules. That is,
modules that should be used by other applications and modules that should not. By default
all modules in an application are visible, but by setting `-moduledoc false.`
specific modules can be hidden from being listed as part of the available API.

An Erlang [module][] consists of public and private functions and type attributes.
By default, all exported functions, exported types and callbacks are considered
visible and part of the modules public API. In addition, any non-exported
type that is referred to by any other visible type attribute is also visible,
but not considered to be part of the public API. For example:

    -export([example/0]).
    
    -type private() :: one.
    -spec example() -> private().
    example() -> one.

in the above code, the function `example/0` is exported and it referenced the
un-exported type `private/0`. Therefore both `example/0` and `private/0` will
be marked as visible. The `private/0` type will have the metadata field `exported`
set to `false` to show that it is not part of the public API.

If you want to make a visible entity hidden you need to set the `-doc` attribute to
`false`. Let's revisit out previous example:

    -export([example/0]).
    
    -type private() :: one.
    -spec example() -> private().
    -doc false.
    example() -> one.

The function `example/0` is exported but explicitly marked as hidden; therefore
both `example/0` and `private/0` will be hidden.

Any documentation added to an automatically hidden entity
(non-exported function or type) is ignored and will generate a
warning. Such functions can be documented using comments.

[application]: seeerl/kernel:application
[module]: modules

## Compiling and getting documentation

The Erlang compiler has support for compiling the documentation into [EEP-48][]
documentation chunks by passing the [beam_docs][] flag to [compile:file/1][], or
`+beam_docs` to [erlc][].

The documentation can then be retrieved using [code:get_doc/1][], or viewed using the
shell built-in command [h()][c:h/1]. For example:

    1> h(math).
    
          math
    
      A module for basic arithmetic.
    
    2> h(math, add).
    
          add(One, Two)
    
      Adds two numbers together.

[EEP-48]: kernel:eep48_chapter
[compile:file/1]: seemfa/compiler:compile#file/1
[beam_docs]: seeerl/compiler:compile#beam_docs
[erlc]: seecom/erts:erlc
[code:get_doc/1]: seemfa/kernel:code#get_doc/1

## Using ExDoc to generate HTML/ePub documentation

[ExDoc][] has built-in support to generate documentation from Markdown. The simplest
way to use it is by using the [rebar3_ex_doc][] plugin. To setup a rebar3 project to
use [ExDoc][] to generate documentation add the following to your `rebar3.config`.

    %% Enable the plugin
    {plugins, [rebar3_ex_doc]}.
    
    %% Configure the compiler to emit documentation
    {profiles, [{docs, [{erl_opts, [beam_docs]}]}]}.
    
    {ex_doc, [
     {extras, ["README.md"]},
     {main, "README.md"},
     {source_url, "https://github.com/namespace/your_app"}
    ]}.

When configured you can run `rebar3 ex_doc` and the documentation will be generated to
`doc/index.html`. For more details and options see the [rebar3_ex_doc][] documentation.

You can also download the [release escript bundle][ex_doc_escript] from github and
run it from the command line. The documentation for using the escript is
found by running `ex_doc --help`.

If you are writing documentation that will be using [ExDoc][] to generate HTML/ePub
it is highly recommended to read its documentation.

[ExDoc]: https://hexdocs.pm/ex_doc/
[rebar3_ex_doc]: https://hexdocs.pm/rebar3_ex_doc
[ex_doc_escript]: https://github.com/elixir/ex_doc/releases/latest
[Earmark]: https://hexdocs.pm/earmark_parser
