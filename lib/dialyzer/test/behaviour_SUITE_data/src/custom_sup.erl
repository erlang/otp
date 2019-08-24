%%%
%%% Dialyzer was giving a warning with this input because of a bug in the
%%% substitution of remote types in specs. Remote types in the first element of
%%% a tuple would not update the tuple's tag set and we could end up with a
%%% non-normalized representation.
%%%
%%% Reported by Damian Dobroczyński on 29/02/2012

-module(custom_sup).

-behavior(supervisor).

-export([init/1]).

-spec init(atom()) ->
	{ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()},
	      [supervisor:child_spec()]}} | ignore.

init(StorageName) ->
  Strategy = {one_for_all, 100, 1},
  %% get application-wide storage parameters
  case application:get_env(storage) of
    undefined ->
      ignore;
    {ok, Storage} ->
      BackendId   = proplists:get_value(backend, Storage),
      BackendArgs = proplists:get_value(args, Storage),
      if
        (BackendId =:= undefined) orelse (BackendArgs =:= undefined) ->
          ignore;
        true ->
          {ok, {Strategy,
                 [{id1, {a_module, start_link, []},
		   permanent, 5000, worker, [a_module]},
                  {id2, {another_module, start_link, []},
		   permanent, 5000, worker, [another_module]}]}}
      end
  end.
