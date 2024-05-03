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
            append_to_github_output("result=~p~n", [Number]);
        false ->
            append_to_github_output("result=~ts~n", [""])
    end.

append_to_github_output(Fmt, Args) ->
    case os:getenv("GITHUB_OUTPUT") of
        false ->
            io:format(standard_error, "GITHUB_OUTPUT env var missing?~n", []);
        GitHubOutputFile ->
            {ok, F} = file:open(GitHubOutputFile, [write, append]),
            ok = io:fwrite(F, Fmt, Args),
            ok = file:close(F)
    end.

ghapi(CMD) ->
    decode(cmd(CMD)).

decode(Data) ->
    try jsx:decode(Data,[{return_maps, true}, return_tail]) of
        {with_tail, Json, <<>>} ->
            Json;
        {with_tail, Json, Tail} ->
            lists:concat([Json | decodeTail(Tail)])
    catch E:R:ST ->
            io:format("Failed to decode: ~ts",[Data]),
            erlang:raise(E,R,ST)
    end.

decodeTail(Data) ->
    try jsx:decode(Data,[{return_maps, true}, return_tail]) of
        {with_tail, Json, <<>>} ->
            [Json];
        {with_tail, Json, Tail} ->
            [Json | decodeTail(Tail)]
    catch E:R:ST ->
            io:format(standard_error, "Failed to decode: ~ts",[Data]),
            erlang:raise(E,R,ST)
    end.

cmd(CMD) ->
    ListCmd = unicode:characters_to_list(CMD),
    io:format("cmd: ~ts~n",[ListCmd]),
    unicode:characters_to_binary(os:cmd(ListCmd)).
