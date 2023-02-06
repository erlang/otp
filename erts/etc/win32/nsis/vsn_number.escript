

main([OtpVsn]) ->
    try
        ErlTop = os:getenv("ERL_TOP"),
        {ok, File} = file:read_file(filename:join([ErlTop, "make", "otp_version_tickets"])),
        <<"OTP-", _/binary>> = File,
        case [list_to_integer(Str) || Str <- string:lexemes(OtpVsn, ".")] of
            [_,_,_,_|_] -> io:format("~s~n", [OtpVsn]);
            [_,_,_] -> io:format("~s.0~n",[OtpVsn]);
            [_,_] -> io:format("~s.0.0~n",[OtpVsn]);
            [_] -> io:format("~s.0.0.0~n",[OtpVsn])
        end
    catch _:_ -> %% release candidate or development branch set fake version as 0.0.0.0
            io:format("0.0.0.0")
    end.
