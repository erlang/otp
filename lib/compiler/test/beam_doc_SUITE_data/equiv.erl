-module(equiv).

-export([main/1, main/2]).

-doc #{ equiv => main(A, 1) }.
main(A) ->
    main(A, 1).

main(A, B) ->
    {A, B}.
