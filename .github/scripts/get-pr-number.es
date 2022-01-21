#!/usr/bin/env escript
%%! -pa jsx/_build/default/lib/jsx/ebin/
-mode(compile).

main([Repo, HeadSha]) ->
    io:format("Looking for: ~ts",[HeadSha]),
    AllOpenPrs = ghapi("gh api --paginate -X GET /repos/"++Repo++"/pulls -f state=open"),
    case lists:search(
           fun(#{ <<"number">> := NR, <<"head">> := #{ <<"sha">> := Sha }}) ->
                   io:format("~p: Checking ~ts~n",[NR, Sha]),
                   string:equal(HeadSha, Sha)
           end, AllOpenPrs) of
        {value, #{ <<"number">> := Number } } ->
            io:format("::set-output result=~p~n", [Number]);
        false ->
            io:format("::set-output result=~ts~n", [""])
    end.

ghapi(CMD) ->
    Data = cmd(CMD),
    try jsx:decode(Data, [{return_maps,true}])
    catch E:R:ST ->
            io:format("Failed to decode: ~ts",[Data]),
            erlang:raise(E,R,ST)
    end.

cmd(CMD) ->
    ListCmd = unicode:characters_to_list(CMD),
    io:format("cmd: ~ts~n",[ListCmd]),
    unicode:characters_to_binary(os:cmd(ListCmd)).
