%%---------------------------------------------------------------------------
%% A test case with:
%%   - a genuine matching error					-- 1st branch
%%   - a violation of the opacity of timer:tref()		-- 2nd branch
%%   - a subtle violation of the opacity of timer:tref()	-- 3rd branch
%% The test is supposed to check that these cases are treated properly.
%%---------------------------------------------------------------------------

-module(timer_use).
-export([wrong/0]).

-spec wrong() -> error.

wrong() ->
  case timer:kill_after(42, self()) of
    gazonk -> weird;
    {ok, 42} -> weirder;
    {Tag, gazonk} when Tag =/= error -> weirdest;
    {error, _} -> error
  end.
