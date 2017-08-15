-module(badmap_17).
-export([update/1]).

%% Compile this source file with OTP 17.0.

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
    end,
    try
	update_3(Map),
	error(update_did_not_fail)
    catch
	error:{badmap,Map} ->
	    ok
    end,
    ok = update_4(Map),
    ok = update_5(Map),
    ok.

update_1(M) ->
    M#{a=>42}.

update_2(M) ->
    M#{a:=42}.

update_3(M) ->
    id(M),
    M#{a=>42}.

update_4(M) when M#{a=>b} =:= M ->
    did_not_fail;
update_4(_) ->
    ok.

update_5(M) ->
    id(M),
    case id(true) of
        true when M#{a=>b} =:= M ->
            did_not_fail;
        true ->
            ok
    end.

id(I) ->
    I.

