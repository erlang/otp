-module(badmap_17).
-export([update/1]).

%% Compile this source file with OTP 17.

update(Map) ->
    try
	update_1(Map),
	error(update_did_not_fail)
    catch
	error:{badmap,Map} ->
	    ok
    end,
    try
	update_2(Map),
	error(update_did_not_fail)
    catch
	error:{badmap,Map} ->
	    ok
    end.

update_1(M) ->
    M#{a=>42}.

update_2(M) ->
    M#{a:=42}.
