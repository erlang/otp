-module(doc_with_file_error).

-export([main/1, main2/1]).
-export_type([foo/1]).

-moduledoc {file, "doesnotexist"}.

-doc {file, "doesnotexist"}.
-type private_type_exported() :: integer().

-doc {file, "doesnotexist"}.
-spec main(Var) -> foo(Var).
main(X) ->
    X.

-doc(({file, "folder/doesnotexist"})).
-spec main2( I :: integer()) -> bar(I :: integer()).
main2(X) when is_atom(X) ->
    X;
main2(X) ->
    X.

-type foo(X) :: { X, private_type_exported()}.
-type bar(X) :: foo({X, private_type_exported()}).
