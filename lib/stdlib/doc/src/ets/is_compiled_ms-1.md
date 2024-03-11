Checks if a term represent a valid compiled
[match specification](`m:ets#match_spec`). A compiled match specification is
only valid on the Erlang node where it was compiled by calling
`match_spec_compile/1`.

> #### Note {: .info }
>
> Before STDLIB 3.4 (OTP 20.0) compiled match specifications did not have an
> external representation. If passed through
> [`binary_to_term(term_to_binary(CMS))`](`binary_to_term/1`) or sent to another
> node and back, the result was always an empty binary `<<>>`.
>
> After STDLIB 3.4 (OTP 20.0) compiled match specifications have an external
> representation as a node specific reference to the original compiled match
> specification. If passed through
> [`binary_to_term(term_to_binary(CMS))`](`binary_to_term/1`) or sent to another
> node and back, the result _may or may not_ be a valid compiled match
> specification depending on if the original compiled match specification was
> still alive.
