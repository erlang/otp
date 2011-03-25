-module(zoltan_kis4).

-export([f/0, gen/0]).

-opaque id() :: string().

-spec f() -> boolean().

%%Equality test issue  
f() -> "Dummy" == gen().

-spec gen() -> id().

gen() -> "Dummy".
