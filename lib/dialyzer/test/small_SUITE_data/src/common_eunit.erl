%%=====================================================================
%% Program with an erroneous type declaration that caused dialyzer to
%% go into an infinite loop. There are some comments that explain the
%% symptoms and the culprit: the return of test_fun() is erroneous and
%% its type should read
%%	fun((config()) -> test_rep() | [test_rep()])
%% instead. But this should not throw dialyzer into an infinite loop.
%% This concerned dialyzer in R14B02 (and probably prior).
%%=====================================================================
-module(common_eunit).

-export([expand_cases/2]).

-type test_name() :: atom() | {'group', atom()}.

-type test_rep()  :: {{atom(), atom(), arity()}, fun()}
                   | {'setup', fun(), fun()}
                   | {'setup', fun(), fun(), fun()}
                   | {atom(), test_rep()}
                   | {atom(), term(), test_rep()}.

-type config()    :: [proplists:property()].

-type control()   :: tuple() | atom().

%% The combination of the following type and the (erroneous) spec for
%% expand_cases/2 is the reason for the infinite loop in dialyzer.
-type test_fun()  :: fun((config()) -> test_rep()).

%% If one comments out this spec the infinite loop disappears.
-spec expand_cases(atom(), test_name() | [test_name()]) -> test_fun().
expand_cases(Module, Cases) ->
    if is_list(Cases) ->
            TestFuns = [expand_case(Module, Case) || Case <- Cases],
            fun(Config) -> [F(Config) || F <- TestFuns] end;
       is_atom(Cases); is_tuple(Cases) ->
           expand_cases(Module, [Cases])
    end.

-spec expand_case(atom(), test_name()) -> test_fun().
expand_case(Module, CaseName) when is_atom(CaseName) ->
    TestFun = fun(Config) ->
		      {{Module, CaseName, 1},
		       fun() -> apply(Module, CaseName, [Config]) end}
	      end,
    setup_wrapper(Module, TestFun, {init_per_testcase, [CaseName]},
		  {end_per_testcase, [CaseName]});
expand_case(Module, {group, GroupName}) ->
    {Control, Cases} = group_specification(Module, GroupName),
    TestFun = control_wrapper(Control, expand_cases(Module, Cases)),
    setup_wrapper(Module, TestFun, {init_per_group, [GroupName]},
		  {end_per_group, [GroupName]}).

-spec control_wrapper([control()], test_fun()) -> test_fun().
control_wrapper([Control|T], TestFun0) ->
    TestFun1 = control_wrapper(T, TestFun0),
    fun(Config) ->
	    case Control of
		parallel ->
		    {inparallel, TestFun1(Config)};
		sequence ->
		    {inorder, TestFun1(Config)};
		{timetrap, Time} ->
		    Seconds = case Time of
				  {hours, Hs}   -> Hs * 60 * 60;
				  {minutes, Ms} -> Ms * 60;
				  {seconds, Ss} -> Ss;
				  MSs           -> MSs / 1000
			      end,
		    {timeout, Seconds, TestFun1(Config)};
		C when is_atom(C) ->
		    {C, TestFun1(Config)};
		{C, Arg} ->
		    {C, Arg, TestFun1(Config)}
	    end
    end;
control_wrapper([], TestFun) ->
    TestFun.

-spec setup_wrapper(atom(), test_fun(), Callback, Callback) -> test_fun()
        when Callback :: {atom(), list()}.
setup_wrapper(Module, TestFun, {Setup, SA}, {Cleanup, CA}) ->
    case erlang:function_exported(Module, Setup, length(SA) + 1) of
        true ->
            case erlang:function_exported(Module, Cleanup, length(CA) + 1) of
                true ->
                    fun(Config0) ->
			    {setup,
			     fun() ->
				     apply(Module, Setup, SA ++ [Config0])
			     end,
			     fun(Config1) ->
				     apply(Module, Cleanup, CA ++ [Config1])
			     end,
			     TestFun}
                    end;
                false ->
                    fun(Config) ->
			    {setup,
			     fun() ->
				     apply(Module, Setup, SA ++ [Config])
			     end,
			     TestFun}
                    end
            end;
        false ->
            TestFun
    end.

-spec group_specification(atom(), atom()) -> {[control()], [test_name()]}.
group_specification(Module, GroupName) ->
    case lists:keyfind(GroupName, 1, Module:groups()) of
        {_, Control, Cases} when is_list(Control), is_list(Cases) ->
            {Control, Cases};
        {_, Cases} when is_list(Cases) ->
            {[], Cases};
        false ->
            exit({missing_group, GroupName});
        _ ->
            exit({bad_group_spec, GroupName})
    end.
