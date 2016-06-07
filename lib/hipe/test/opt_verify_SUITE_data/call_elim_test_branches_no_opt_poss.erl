-module(call_elim_test_branches_no_opt_poss).

-export([test/1]).

test(A) ->
    if A > 0 ->
	    false = has_a_field(#{b=>true}),
	    true  = has_a_field(#{b=>1, a=>"2"}),
	    false = has_a_field(#{b=>5, c=>4}),
	    false = has_tuple_field(#{{ab, 2}=><<"qq">>, 1      =>0}),
	    false = has_tuple_field(#{up     =>down,     {ab, 2}=>[]}),
	    false = has_tuple_field(#{{ab, 2}=>42});
       A =< 0 ->
	    true = has_a_field(#{a=>q,     'A'  =>nej}),
	    true = has_a_field(#{a=>"hej", false=>true}),
	    true = has_a_field(#{a=>3}),
	    true = has_tuple_field(#{{ab, 1}=>q,     'A'  =>nej}),
	    true = has_tuple_field(#{{ab, 1}=>"hej", false=>true}),
	    true = has_tuple_field(#{{ab, 1}=>3})
    end,
    true = has_nil_field(#{[]         =>3, b=>"seven"}),
    true = has_nil_field(#{"seventeen"=>17}),
    ok.

has_tuple_field(#{{ab, 1}:=_}) -> true;
has_tuple_field(#{}) -> false.

has_a_field(#{a:=_}) -> true;
has_a_field(#{}) -> false.

has_nil_field(#{[]:=_}) -> true;
has_nil_field(#{}) -> false.
