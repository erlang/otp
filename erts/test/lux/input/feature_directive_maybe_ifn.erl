%% This module knows about the both features ifn and maybe
%% These atoms are thus quoted

-module(feature_directive_maybe_ifn).

-export([foo/0,
	 bar/0,
	 foo_feature/0,
	 ifn_0/0,
	 ifn_1/0,
	 ifn_2/0,
	 maybe_0/0,
	 maybe_1/0,
	 use_ifn/1
	]).

-define(XLINE, 42).

use_ifn(X) ->
    ifn X > 0 ->
	    io:format("!(~p > 0)\n", [X])
    end.

-if(?FEATURE_AVAILABLE('ifn_expr')).
foo_feature() ->
    true.
-else.
foo_feature() ->
     false.
-endif.

-if(?FEATURE_ENABLED('ifn_expr')).
ifn_0() ->
    true.
-else.
ifn_0() -> false.
-endif.


-define(FOO(X), ((X) == 'ifn' orelse (X) == 'maybe')).
-define(BAR(X), false).
%% NOTE: We need to quote the feature name due to it being the same as
%% the new reserved word and it might have been enabled earlier, i.e.,
%% from the command line or in argumemts to compile:file/..
-compile({feature, 'ifn_expr', enable}).

%% use_ifn(X) ->
%%     ifn X > 0 ->
%% 	    io:format("!(~p > 0\n", [X])
%%     end.

-if(?FEATURE_ENABLED('ifn_expr')).
ifn_1() ->
    true.
-else.
ifn_1() -> false.
-endif.

foo() ->
    %% Note: maybe not active here
    ['ifn', maybe, else, 'if'].

-if(?FEATURE_ENABLED('maybe_expr')).
maybe_0() ->
    true.
-else.
maybe_0() -> false.
-endif.

-compile({feature, 'maybe_expr', enable}).

-if(?FEATURE_ENABLED('maybe_expr')).
maybe_1() ->
    true.
-else.
maybe_1() -> false.
-endif.

-if(?FEATURE_ENABLED('ifn_expr')).
ifn_2() ->
    true.
-else.
ifn_2() -> false.
-endif.

bar() ->
    ['else', 'maybe'].
