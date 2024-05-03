# EEP-48: Implementation in Erlang/OTP

## EEP-48: Documentation storage and format

[EEP-48](`e:kernel:eep48_chapter.md`) defines a common documentation storage
format for module documentation in the Erlang/OTP ecosystem. Erl_Docgen can
generate documentation in this format from XML files following the DTD's
described in the other User's Guides in this application.

Some special considerations have to be taken when writing documentation that
should also be available through EEP-48 style storage.

## Erlang Documentation Format

When generating documentation for EEP-48 edoc uses the format mime type
<<"application/erlang+html">>. The documentation content is an Erlang term that
represents an HTML like structure.

```text
-type chunk_elements() :: [chunk_element()].
-type chunk_element() :: {chunk_element_type(),chunk_element_attrs(),
                          chunk_elements()} | unicode:unicode_binary().
-type chunk_element_attrs() :: [chunk_element_attr()].
-type chunk_element_attr() :: {atom(),unicode:unicode_binary()}.
-type chunk_element_type() :: chunk_element_inline_type() | chunk_element_block_type().
-type chunk_element_inline_type() :: a | code | strong | b | em | i.
-type chunk_element_block_type() :: p | 'div' | br | pre | ul |
                                    ol | li | dl | dt | dd |
                                    h1 | h2 | h3 | h4 | h5 | h6.
```

The different element types follow their HTML meaning when rendered. The
following are some general rules for how the chunk elements are allowed to be
generated.

- Inline and `pre` elements are not allowed to contain block elements.
- `p` elements are not allowed to be nested.

The attributes on some elements have a special meaning.

- **`{'div',[{class,unicode:unicode_binary()}],_}`** - The class name will be
  used to provide styling to the content in the div. The types of classes used
  by Erlang/OTP are: `warning`, `note`, `do`, `dont` and `quote`.

- **`{ul,[{class,<<"types">>}],_}`** - This is a list containing type
  documentation.

- **`{li,[{name,TypeName :: unicode:unicode_binary()}],_}`** - A list item with
  a type specification located in the metadata of this modules EEP-48
  documentation. The implementation should look for the AST representation of
  the type under the `types` key. This attribute is only valid under a `ul` with
  class <<"types">>.

- **`{li,[{class,<<"type">>}],_}`** - A list item with the type described in the
  Erlang Documentation Format. This attribute is only valid under a `ul` with
  class <<"types">>.

- **`{li,[{class,<<"description">>}],_}`** - A list item with the description of
  the type previous in the list. This attribute is only valid under a `ul` with
  class <<"types">>.

The `shell_docs:validate/1` function can be used to do a validation of the
Erlang Documentation Format.

## Erlang Documentation extra Metadata

Erlang/OTP uses some extra metadata fields to embed more information into the
EEP-48 docs.

- Fields on module level:

  - **`otp_doc_vsn := {non_neg_integer(),non_neg_integer(),non_neg_integer()}`** -
    Describes the version of the Erlang Documentation Format used within this
    module

  - **`types := #{ TypeName :: unicode:unicode_binary() => TypeAST }`** - A map
    containing the AST of the types that are part of this module. This map is
    used to by functions and callbacks to render the types inline into their
    documentation.

- Fields on functions and types:

  - **`signature := SpecAST`** - The spec AST associated with this function. It
    is used to render a more descriptive slogan for the documentation entry.

  - **`equiv := {Type,Name,Arity}`** - The current function/type shares
    documentation with another function/type. This means that if this and the
    target function/type are to be shown at the same time only the prototype of
    this function/type should will be displayed and the documentation will use a
    common body of text.

## See Also

`m:shell_docs`, `code:get_doc/1`
