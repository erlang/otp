%%---------------------------------------------------------------------
%% This module does not test gb_sets. Instead it tests that we can
%% create records whose fields are declared with an opaque type and
%% retrieve these fields without problems. Unitialized record fields
%% used to cause trouble for the analysis due to the implicit
%% 'undefined' value that record fields contain. The problem was the
%% strange interaction of ?opaque() and ?union() in the definition of
%% erl_types:t_inf/3. This was fixed 18/1/2009.
%% --------------------------------------------------------------------

-module(gb_sets_rec).

-export([new/0, get_g/1]).

-record(rec, {g :: gb_sets:set()}).

-spec new() -> #rec{}.
new() ->
  #rec{g = gb_sets:empty()}.

-spec get_g(#rec{}) -> gb_sets:set().
get_g(R) ->
  R#rec.g.
