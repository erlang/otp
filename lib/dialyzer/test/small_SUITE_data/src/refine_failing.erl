%% This testcase shows why it's a bad idea to block refinement (by forwarding
%% any() to all arguments) when a failing call is encountered. The initial
%% success typing for update_one allows anything to be an element of the list in
%% the second argument. This will be refined during dataflow by the result from
%% add_counters to just a list of tuples. This will cause the call in the second
%% clause of update_one to fail correctly and identify the discrepancy. It could
%% be a better idea to refuse to add the failing calls but this may lead to a
%% ton of unused functions,
%%
%% by Stavros Aronis<aronisstav@gmail.com>

-module(refine_failing).

-export([foo/2]).

foo(A, B) -> update_all(add_counters(A, []), B).

add_counters(   [], Acc) -> Acc;
add_counters([H|T], Acc) -> add_counters(T, [{H, 0}|Acc]).

update_all(Ds,     []) -> Ds;
update_all(Ds, [F|Fs]) -> update_all(update_one(F, Ds, []), Fs).

update_one(_F,           [], Acc) -> Acc;
update_one( F, [{F, Cr},Ds], Acc) -> update_one(F, Ds, [{F,Cr+1}|Acc]);
update_one( F, [      D|Ds], Acc) -> update_one(F, Ds, [       D|Acc]).
