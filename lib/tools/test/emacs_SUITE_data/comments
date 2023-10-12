%% -*- Mode: erlang; indent-tabs-mode: nil -*-
%% Copyright Ericsson AB 2017. All Rights Reserved.

%%% 3 comment chars: always left indented
%%% 2 comment chars: Context indented
%%% 1 comment char: Right indented

%%% left
%% context dependent
                                                % right

func() ->
%%% left
    %% context dependent
                                                % right indented
    case get(foo) of
        undefined ->
            %% Testing indentation
            ok;
        %% Catch all
        Other ->
            Other
    end,
    ok.

