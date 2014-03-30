-module(small).

-export([go/0,go/2]).


-small_attribute({value,3}).

go() -> go(3, 3.0).
go(A,B) ->
    V1 = A + B,
    V2 = A * B,
    V3 = V1 / V2,
    V4 = V3 / 0.3,
    V5 = V1 + V2 + V3 + V4,
    try
	R = call(<<"wazzup">>, A),
	{A,B,V5,R,t(),recv()}
    catch
	C:E ->
	    {error, C, E}
    end.

-spec call(binary(), term()) -> binary().

call(<<"wa", B/binary>>,V) when is_integer(V) -> B;
call(B,_) -> B.

t() ->
    <<23:32, V:14, _:2, B/binary>> = id(<<"wazzup world">>),
    {V,B}.

recv() ->
    F = fun() ->
	    receive
		1 -> ok;
		2 -> ok;
		3 -> ok;
		a -> ok;
		_ -> none
	    after 0 -> tmo
	    end
    end,
    tmo = F(),
    ok.


id(I) -> I.

