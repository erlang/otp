-module(redefine_builtin_type).
-export([lookup/2, verify_mfa/1, verify_pid/1]).

-type map() :: {atom(), erlang:map()}.

-spec lookup(atom(), map()) -> {'ok', term()} | 'error'.

lookup(Key, {Key, Map}) when is_atom(Key), is_map(Map) ->
    {ok, Map};
lookup(Key1, {Key2, Map}) when is_atom(Key1), is_atom(Key2), is_map(Map) ->
    error.

%% Type `mfa()` depends on `erlang::module()`. Make sure that `mfa()`
%% does not attempt to use our local definition of `module()`.

-type module() :: pid().

-spec verify_mfa(mfa()) -> 'ok'.
verify_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    ok.

-spec verify_pid(module()) -> 'ok'.
verify_pid(Pid) when is_pid(Pid) ->
    ok.
