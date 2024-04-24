### Archives

The following features for archives are deprecated:

* Using archives for packaging a single application or parts of a
  single application into an archive file that is included in the code
  path.

* All functionality to handle archives in module
  [`erl_prim_loader`](https://www.erlang.org/doc/man/erl_prim_loader).

* The `-code_path_choice` flag for `erl`.

Using a single archive file for holding BEAM files and other data
files in an Escript is **not** deprecated. However, to access files in
the archive the `escript:extract/2` function has to be used.
