%%%-------------------------------------------------------------------
%%% File    : test_comprehensions.erl
%%% Author  : Per Gustafsson <pergu@jobberl>
%%% Description : Test module to see that pretty printing etc.  
%%%               works on extended comprehensions
%%% Created : 15 Oct 2007 by Per Gustafsson <pergu@jobberl>
%%%-------------------------------------------------------------------
-module(test_comprehensions).

-compile(binary_comprehension).

-export([test/0]).

test() ->
  {bbc(),llc(),blc(),lbc(),bblc(),lblc()}.

binary() ->
  <<1,2,3>>.

list() ->
  [1,2,3].

bbc() ->
  << <<X>> || <<X>> <= binary(), X > 1 >>.

llc() ->
  [X || X <- list(), X > 1].

blc() ->
  << <<X>> || X <- list(), X > 1 >>.

lbc() ->	       
  [X || <<X>> <= binary(), X > 1].

bblc() ->
  << <<(X+Y)>> || <<X>> <= binary(), Y <- list(), X > 1 >>.
	 
lblc() ->
  [(X+Y) || <<X>> <= binary(), Y <- list(), X > 1].
