#!/usr/bin/env escript
%%! -pa jsx/_build/default/lib/jsx/ebin/
%%
%% This scripts downloads the docs + test results from an otp repo
%% into the Target folder. It tries its best to not create too large
%% files so that gh will still be happy with us when this is published to
%% gh pages
-mode(compile).

main([Repo, Target]) ->
    AllOpenPrs = ghapi("gh api --paginate -X GET /repos/"++Repo++"/pulls -f state=open"),
    %% Download all updates, there really should not be any to download as they
    %% are updated when a PR is updated, but we do it anyways just to be safe.
    handle_prs(Repo, Target,AllOpenPrs),

    %% Delete any PRs that have been closed
    {ok, AllPrs} = file:list_dir(Target),
    lists:foreach(
      fun(PRNo) ->
              case lists:search(
                     fun(#{ <<"number">> := No }) ->
                        No =:= list_to_integer(PRNo)
                     end, AllOpenPrs) of
                  {value, _} ->
                      ok;
                  false ->
                      cmd("rm -rf " ++ filename:join(Target,PRNo))
              end
      end, AllPrs);
main([Repo, Target, PRNo]) ->
    handle_prs(Repo, Target, [ghapi("gh api /repos/"++Repo++"/pulls/"++PRNo)]).

handle_prs(Repo, Target, AllPRs) ->

    [handle_pr(Repo, Target, PR) || PR <- AllPRs],

    %% Remove all links and files > 50MB
    cmd(["find ",Target," -type l -exec rm -f {} \\;"]),
    cmd(["find ",Target," -type f -size +50M -exec rm -f {} \\;"]),

    ok.

%% In order to get the latest gh actions run for a PR, we have to first list
%% all workflow runs for that branch, and then look for a matching sha with the
%% current top of the PR. Github does not have any API to find it any other way.
%% See https://github.community/t/retrieve-workflow-id-for-a-given-pr/199745/4
%%   for a discussion about this.
%%
handle_pr(Repo, Target,
          #{ <<"number">> := Number,
             <<"head">> := #{ <<"ref">> := Ref, <<"sha">> := Sha } }) ->
    PRDir = filename:join(Target,integer_to_list(Number)),
    Runs = ghapi(["gh api --paginate -X GET /repos/"++Repo++"/actions/runs -f event=pull_request -f 'branch=",Ref,"'"]),
    case lists:search(
           fun(#{ <<"head_sha">> := HeadSha, <<"status">> := Status }) ->
                   string:equal(HeadSha, Sha) andalso string:equal(Status, <<"completed">>)
           end, maps:get(<<"workflow_runs">>, Runs)) of
        {value, Run} ->
            Ident = integer_to_list(maps:get(<<"id">>,Run)),
            io:format("Checking for ~ts~n", [filename:join(PRDir, Ident)]),
            case file:read_file_info(filename:join(PRDir, Ident)) of
                {error, enoent} ->
                    cmd("rm -rf "++PRDir),
                    ok = file:make_dir(PRDir),
                    ok = file:write_file(filename:join(PRDir,Ident), integer_to_list(Number)),

                    #{ <<"artifacts">> := Artifacts } =
                        ghapi(["gh api --paginate -X GET ",maps:get(<<"artifacts_url">>, Run)]),

                    lists:foreach(
                      fun(#{ <<"name">> := <<"test_results">>, <<"archive_download_url">> := Url }) ->
                              cmd(["gh api ", unicode:characters_to_list(Url), " > /tmp/test_results.zip"]),
                              cmd("unzip -d /tmp/test_results /tmp/test_results.zip"),
                              cmd(["tar xvzf /tmp/test_results/test_results.tar.gz "
                                   "-C ",PRDir," make_test_dir/ct_logs --strip-components=1"]),
                              cmd("rm -rf /tmp/test_results*");
                         (#{ <<"name">> := <<"otp_doc_html">>, <<"archive_download_url">> := Url }) ->
                              cmd(["gh api ", unicode:characters_to_list(Url), " > /tmp/otp_doc_html.zip"]),
                              cmd("unzip -d /tmp/otp_doc_html /tmp/otp_doc_html.zip"),
                              cmd(["tar xvzf /tmp/otp_doc_html/otp_doc_html.tar.gz -C ",PRDir]),
                              cmd(["find ",PRDir," -name '*.pdf' -exec rm -f {} \\;"]),
                              cmd("rm -rf /tmp/otp_doc_html*");
                         (_) ->
                              ok
                      end, Artifacts),
                    CTLogsIndex = filename:join([PRDir,"ct_logs","index.html"]),
                    case file:read_file_info(CTLogsIndex) of
                        {ok, _} -> ok;
                        _ ->
                             ok = filelib:ensure_dir(CTLogsIndex),
                             ok = file:write_file(CTLogsIndex, ["No test logs found for ", Sha])
                    end,
                    DocIndex = filename:join([PRDir,"doc","index.html"]),
                    case file:read_file_info(DocIndex) of
                        {ok, _} -> ok;
                        _ -> ok = filelib:ensure_dir(DocIndex),
                             ok = file:write_file(DocIndex, ["No documentation found for ", Sha])
                    end;
                {ok,_} ->
                    ok
            end;
        false ->
            ok
    end.

ghapi(CMD) ->
    Data = cmd(CMD),
    try jsx:decode(Data,[{return_maps, true}])
    catch E:R:ST ->
            io:format("Failed to decode: ~ts",[Data]),
            erlang:raise(E,R,ST)
    end.

cmd(CMD) ->
    ListCmd = unicode:characters_to_list(CMD),
    io:format("cmd: ~ts~n",[ListCmd]),
    unicode:characters_to_binary(os:cmd(ListCmd)).
