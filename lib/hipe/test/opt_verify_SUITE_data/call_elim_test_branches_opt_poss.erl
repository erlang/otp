-module(call_elim_test_branches_opt_poss).

-export([test/1]).

test(A) ->
    if A > 0 ->
	    true = has_a_field(#{a=>true}),
	    true = has_a_field(#{b=>1, a=>"2"}),
	    true = has_a_field(#{a=>5, c=>4}),
	    true = has_tuple_field(#{{ab, 1}=><<"qq">>, 1      =>0}),
	    true = has_tuple_field(#{up     =>down,     {ab, 1}=>[]}),
	    true = has_tuple_field(#{{ab, 1}=>42});
       A =< 0 ->
	    true = has_a_field(#{a=>q,     'A'  =>nej}),
	    true = has_a_field(#{a=>"hej", false=>true}),
	    true = has_a_field(#{a=>3}),
	    true = has_tuple_field(#{{ab, 1}=>q,     'A'  =>nej}),
	    true = has_tuple_field(#{{ab, 1}=>"hej", false=>true}),
	    true = has_tuple_field(#{{ab, 1}=>3})
    end,
    true = has_nil_field(#{[]         =>3,  b =>"seven"}),
    true = has_nil_field(#{"seventeen"=>17, []=>nil}),
    ok.

has_tuple_field(#{{ab, 1}:=_}) -> true;
has_tuple_field(#{}) -> false.

has_a_field(#{a:=_}) -> true;
has_a_field(#{}) -> false.

has_nil_field(#{[]:=_}) -> true;
has_nil_field(#{}) -> false.
