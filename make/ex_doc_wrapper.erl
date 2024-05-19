-module(ex_doc_wrapper).

-export([main/1]).

main(Args) ->
    case os:find_executable("ex_doc") of
        false ->  
            case prompt() of
                {ok, download} ->
                    case download_otp() of
                        error ->
                            io:format("Unable to read ERL_TOP from environment?~n");
                        ok ->
                            main(Args)
                    end;
                {error, try_again} ->
                    main(Args);
                {error, do_not_download} ->
                    io:format("Skipping download...~n"),
                    halt(1)
            end;

        ExDocExe ->
            % Passing in --option "hello world" leads to ["--option", "hello world"]
            % Re-wrap the latter in quotes
            Quoted = lists:map(fun quote_arguments/1, Args),
            Cmd = lists:append(["exec ", ExDocExe, " "],  lists:join(" ", Quoted)),
            io:format("Running ~s~n", [Cmd]),
            os:cmd(Cmd)
    end.

download_otp() ->
    case os:getenv("ERL_TOP") of
        false -> 
            error;
        ErlTop ->
            Cmd = [ErlTop, "/otp_build download_ex_doc"],
            io:format("Running ~s~n", [Cmd]),
            Out = os:cmd(Cmd),
            io:format("~s", [Out]),
            ok
    end.

prompt() ->
    case io:get_line("Do you want to download latest ex_doc from github? (y/n)? ") of
        {error, _Description} -> 
            io:format("Unable to get line?~n"),
            {error, try_again};
        eof ->
            io:format("Got end of file?~n"),
            {error, try_again};
        Input ->
            case string:casefold(string:trim(Input)) of
                "y" -> {ok, download};
                "n" -> {error, do_not_download};
                _ -> {error, try_again}
            end
    end.

quote_arguments(String) ->
    case string:find(String, " ") of
        nomatch -> String;
        _ -> string:pad(String, string:length(String) + 2, both, $")
    end.
