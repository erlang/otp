%%---------------------------------------------------------------------------
%% From: Nicolas Tranger <ikke@nicolast.be>
%% Date: 10/11/2110
%%
%% After adding spec statements to my module, Dialyzer failed on execution
%% though. I've been trying to create a minimal reproducible case and
%% reduced the code to something similar of about 80 LOC.  As noted in the
%% comments, commenting out some lines makes Dialyzer parse and analyze
%% the file correctly.  The code executes correctly and as expected.
%%
%% I'm not sure what causes the issue. parse_result is polymorphic in its
%% return type, but statically typed based on the type of the 3th argument
%% (well, that's how I see things from a Haskell background).
%%---------------------------------------------------------------------------
%% This was a bug in erl_types:t_subtract/2 which was not handling the case
%% of free variables in prameterized types. Fixed 15/10/2010.
%%---------------------------------------------------------------------------
-module(param_types_crash).

-export([test/0]).

-type socket_error() :: 'connection_closed' | 'timeout'.
-type app_error() :: 'no_magic' | 'unknown_failure'.

-type resulthandler_result(T) :: {'ok', T} | socket_error() | app_error().
-type resulthandler(T) :: fun((binary()) -> resulthandler_result(T)).

test() ->
  Data = <<0:32/little-unsigned, 1:8/little, 1:8/little-unsigned>>,
  case parse_result(Data, get_option(fun get_bool/1)) of
	%% Removing the next 2 lines makes
	%% dialyzer param_types_crash.erl -Wunmatched_returns -Wunderspecs
	%% succeed. With these lines, it fails.
	{ok, none} -> none;
	{ok, {some, Value}} -> Value;
	Reply -> {error, Reply}
  end.

-spec parse_result(binary(), resulthandler(T)) -> resulthandler_result(T).
parse_result(<<ResultCode:32/little-unsigned, Rest/binary>>, ResultHandler) ->
  case ResultCode of
	0 -> ResultHandler(Rest);
	1 -> no_magic;
	2 -> unknown_failure
  end.

-spec get_bool(binary()) -> {'ok', boolean()} | socket_error().
get_bool(Data) ->
  case get_data(Data, 1, size(Data)) of
    {<<Value:8/little-unsigned>>, <<>>} -> {ok, (Value =:= 1)};
    Other -> Other
  end.

-spec get_option(resulthandler(T)) -> resulthandler('none' | {'some', T}).
get_option(Fun) ->
  fun(Data) ->
      case get_data(Data, 1, size(Data)) of
	{<<HasValue:8/little>>, Rest} ->
	  case HasValue of
	    0 -> {ok, none};
	    1 -> {ok, Value} = Fun(Rest),
		 {ok, {some, Value}}
	   end;
	Other -> Other
      end
    end.

-spec get_data(binary(), non_neg_integer(), non_neg_integer()) ->
	{binary(), binary()} | socket_error().
get_data(Data, Length, Received) when Length > Received ->
    case Data of
      <<>> -> connection_closed;
      _ -> timeout
    end;
get_data(Data, Length, _) ->
    <<Bin:Length/binary, Rest/binary>> = Data,
    {Bin, Rest}.
