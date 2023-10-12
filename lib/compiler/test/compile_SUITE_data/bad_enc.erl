%% coding: utf-8
%% This file claims to be in UTF-8 but is actually in Latin-1
%% causing the parser to give up
-module(bad_enc).
bar() ->
	    %% these lines are indented with TAB+SPC to check error column
	    {ok, "xyzåäö"}.
