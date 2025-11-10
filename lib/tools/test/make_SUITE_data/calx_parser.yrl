%% From: https://github.com/knutin/calx
Nonterminals
expr
arg_list
.


Terminals
operator
digit
'('
')'
.

Rootsymbol expr.

expr -> '(' operator arg_list ')' : {expr, value_of('$2'), '$3'}.

arg_list -> expr : ['$1'].
arg_list -> digit : [{digit, ?l2i(value_of('$1'))}].
arg_list -> digit arg_list : [{digit, ?l2i(value_of('$1'))}] ++ '$2'.


Erlang code.

-define(l2i(L), list_to_integer(L)).
value_of({_,_,V}) -> V.
