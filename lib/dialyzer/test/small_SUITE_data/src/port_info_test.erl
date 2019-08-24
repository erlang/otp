%%
%% Tests hardcoded dependent type info
%% and the quality of the warnings that Dialyzer spits out
%%
-module(port_info_test).
-export([t1/1, t2/1, t3/1, t4/1, t5/2, t6/1, buggy/1]).

%% The following errors are correctly caught, but the messages are a bit weird
t1(X) when is_port(X) ->
  {connected, 42} = erlang:port_info(X, connected);
t1(_) -> ok.

t2(X) when is_port(X) ->
  {registered_name, "42"} = erlang:port_info(X, registered_name);
t2(_) -> ok.

%% Here only one od the two errors is reported...
t3(X) when is_atom(X) ->
  {output, 42} = erlang:port_info(X, connected);
t3(_) -> ok.

t4(X) when is_atom(X) ->
  {Atom, _} = erlang:port_info(X, connected),
  Atom = links;
t4(_) -> ok.

t5(X, Atom) when is_port(X) ->
  {gazonk, _} = erlang:port_info(X, Atom);
t5(_, _) -> ok.

t6(X) when is_port(X) ->
  {os_pid, "42"} = erlang:port_info(X, os_pid);
t6(_) -> ok.

%% The type system is not strong enough to catch the following errors
buggy(X) when is_atom(X) ->
  {links, X} = erlang:port_info(foo, X).
