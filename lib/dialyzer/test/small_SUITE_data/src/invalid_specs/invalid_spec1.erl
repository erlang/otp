-module(invalid_spec1).

-export([get_plan_dirty/1]).

-spec get_plan_dirty([string()]) -> {{atom(), any()}, [atom()]}.

get_plan_dirty(ClassL) ->
    get_plan_dirty(ClassL, [], []).

get_plan_dirty([], Res, FoundClassList) ->
    {Res,FoundClassList};
get_plan_dirty([Class|ClassL], Res, FoundClassList) ->
    ClassPlan = list_to_atom(Class ++ "_plan"),
    case catch mnesia:dirty_all_keys(ClassPlan) of
        {'EXIT',_} ->
            get_plan_dirty(ClassL, Res, FoundClassList);
        [] ->
            get_plan_dirty(ClassL, Res, FoundClassList);
        KeyL ->
            ClassAtom = list_to_atom(Class),
            Res2 =
                lists:foldl(fun(Key, Acc) ->
                                   [{ClassAtom,Key}|Acc]
                            end,
                            Res,
                            KeyL),
            get_plan_dirty(ClassL, Res2, [ClassAtom|FoundClassList])
    end.
