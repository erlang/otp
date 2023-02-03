

main([OtpVsn]) ->
    case string:lexemes(OtpVsn, ".") of
        [_,_,_,_|_] -> io:format("~s~n", [OtpVsn]);
        [_,_,_] -> io:format("~s.0~n",[OtpVsn]);
        [_,_] -> io:format("~s.0.0~n",[OtpVsn]);
        [_] -> io:format("~s.0.0.0~n",[OtpVsn])
    end.
