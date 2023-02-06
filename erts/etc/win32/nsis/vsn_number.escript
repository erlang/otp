

main([OtpVsn, WinPathFile]) ->
    try
        {ok, Bin} = file:read_file(WinPathFile),
        <<"OTP-", _/binary>> = Bin,
        case [list_to_integer(Str) || Str <- string:lexemes(OtpVsn, ".")] of
            [_,_,_,_|_] -> io:format("~s~n", [OtpVsn]);
            [_,_,_] -> io:format("~s.0~n",[OtpVsn]);
            [_,_] -> io:format("~s.0.0~n",[OtpVsn]);
            [_] -> io:format("~s.0.0.0~n",[OtpVsn])
        end
    catch _:_R:_ST -> %% release candidate or development branch set fake version as 0.0.0.0
            %% io:format("Err: ~p ~p~n ~p~n",[_R,_ST, WinPathFile]),
            io:format("0.0.0.0~n")
    end.
