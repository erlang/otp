-module(invalid_spec2).

-export([foo/0]).

foo() ->
    case
	invalid_spec1:get_plan_dirty(mnesia:dirty_all_keys(cmClassInfo))
    of
	{[],[]} -> foo;
	{ _, _} -> bar
    end.
