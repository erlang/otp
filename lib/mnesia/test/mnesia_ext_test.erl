-module(mnesia_ext_test).
-author('ulf@wiger.net').

-include("mnesia_test_lib.hrl").

-export([all/0,
	 groups/0,
	 init_per_group/2,
	 end_per_group/2]).

-export([create_schema/1,
	 create_table/1]).

-define(BACKEND_TYPES, mnesia_ext_filesystem:types()).
-define(TYPE1, fs_copies).

all() ->
    [{group, schema_grp}].

groups() ->
    [{schema_grp, [],
      [create_schema, create_table]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_schema(doc) ->
    ["Create a mnesia schema with a backend plugin"];
create_schema(suite) -> [];
create_schema(Config) when is_list(Config) ->
    ok = cleanup([node()]).

create_table(doc) ->
    ["Create a mnesia table using a custom backend type"];
create_table(suite) -> [];
create_table(Config) when is_list(Config) ->
    ok = do_create_schema(Config),
    ok = do_create_tab(t, Config),
    ok = mnesia:dirty_write({t,1,a}),
    [{t,1,a}] = mnesia:dirty_read({t,1}),
    cleanup([node()]).


do_create_schema(_Config) ->
    ok = mnesia:create_schema([node()], [{backend_types, ?BACKEND_TYPES}]),
    ok = mnesia:start().

do_create_tab(t, _Config) ->
    {atomic, ok} = mnesia:create_table(t, [{?TYPE1, [node()]}]),
    ok.

cleanup(Nodes) ->
    %% currently only cleaning up on single node
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema(Nodes).
