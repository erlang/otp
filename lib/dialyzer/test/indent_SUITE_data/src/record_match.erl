-module(record_match).

-export([select/0]).

-record(b_literal, {val}).
-record(b_remote, {mod,name,arity}).
-record(b_local, {name,arity}).

-type b_remote()   :: #b_remote{}.
-type b_local()    :: #b_local{}.

-type argument()   :: b_remote() | b_local().

-record(b_set, {args=[] :: [argument()]}).

select() ->
    #b_set{args=[#b_remote{},#b_literal{}]}.
