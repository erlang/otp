%%----------------------------------------------------------------------------
%% Module that tests consistency of spec declarations in the presence of
%% opaque types.  Contains both valid and invalid contracts with opaque types.
%%----------------------------------------------------------------------------

-module(int_adt).

-export([new_i/0, add_i/2, div_i/2, add_f/2, div_f/2]).

-export_type([int/0]).

-opaque int() :: integer().

%% the user has declared the return to be an opaque type, but the success
%% typing inference is too strong and finds a subtype as a return: this is OK
-spec new_i() -> int().
new_i() -> 42.

%% the success typing is more general than the contract: this is OK
-spec add_i(int(), int()) -> int().
add_i(X, Y) -> X + Y.

%% the success typing coincides with the contract: this is OK, of course
-spec div_i(int(), int()) -> int().
div_i(X, Y) -> X div Y.

%% the success typing has an incompatible domain element: this is invalid
-spec add_f(int(), int()) -> int().
add_f(X, Y) when is_float(Y) -> X + trunc(Y).

%% the success typing has an incompatible range: this is invalid
-spec div_f(int(), int()) -> int().
div_f(X, Y) -> X / Y.
