-module(eep48_redundant).

-export_type([t_only_attr/0,
	      t_only_tag/0,
	      t_redundant/0]).

-type t_only_attr() :: any().
%% Type `t_only_attr' defined with an attribute.

%% @type t_only_tag() = any().
%% Type `t_only_tag' defined with a tag.

-type t_redundant() :: any().
%% Type `t_redundant' defined with an attribute, redundant with a tag.

%% @type t_redundant() = any().
%% Type `t_redundant' defined with a tag, redundant with an attribute.

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

-spec eep48_redundant:f_prefixed_spec() -> any().
f_prefixed_spec() -> ok.
