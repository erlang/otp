%% @doc Test visibility of various kinds of items in the generated docs.
-module(eep48_visibility).

-export([public_function/0, hidden_function/0, private_function/0]).

-export_type([public_type/0]).

%% Note: There is no such thing as a hidden or private type.

-type public_type() :: {}.
%% This type should be visible in the generated docs.
-type non_exported_type() :: ok.
%% This type should not be visible in the generated docs.

%% @doc This function should be visible in the generated docs.
-spec public_function() -> ok.
public_function() -> ok.

%% @doc This function should not be visible in the generated docs.
%% @hidden
-spec hidden_function() -> ok.
hidden_function() -> ok.

%% @doc This function should not be visible in the generated docs.
%% @private
-spec private_function() -> non_exported_type().
private_function() -> non_exported_function().

%% @doc This type should not be visible in the generated docs.
-spec non_exported_function() -> non_exported_type().
non_exported_function() -> ok.