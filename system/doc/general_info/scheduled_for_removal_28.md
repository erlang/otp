### Archives

The following features of archives will be removed:

* Using archives for packaging a single application or parts of a single application
  into an archive file that is included in the code path.

* All functionality to handle archives in module `m:erl_prim_loader`.

* The `-code_path_choice` flag for `erl`.

The functionality to use a single archive file in Escripts is **not**
deprecated and will continue to work.  However, to access files in the
archive, the `escript:extract/2` function has to be used.
