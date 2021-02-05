-module(eep48_redundant).

-spec f_redundant_spec() -> atom().
%% @doc Function with a redundant spec.
%% @spec f_redundant_spec() -> tag()
f_redundant_spec() -> ok.

-spec f_only_attr() -> atom().
%% @doc Function with only a spec attribute.
f_only_attr() -> ok.

%% @spec f_only_tag() -> atom()
%% @doc Function with only a spec tag.
f_only_tag() -> ok.
