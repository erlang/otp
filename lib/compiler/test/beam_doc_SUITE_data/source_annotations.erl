-module(source_annotations).

-export([main/1,
         bar/0,
         no_slogan/1,
         spec_slogan/1,
         spec_slogan/2,
         no_doc_slogan/1,
         spec_no_doc_slogan/1,
         spec_multiclause_slogan_ignored/1,
         connect/2
        ]).

-spec main(X :: integer()) -> ok.
main(_X) ->
    ok.

bar() ->
    ok.
