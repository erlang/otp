%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(testMegaco).

-export([compile/3,main/2]).

-include_lib("test_server/include/test_server.hrl").

compile(_Config,ber,[optimize]) ->
    {ok,no_module,no_module};
compile(_Config,per,[optimize]) ->
    {ok,no_module,no_module};
compile(Config,Erule,Options) ->
    asn1_test_lib:compile("MEDIA-GATEWAY-CONTROL.asn", Config, [Erule|Options]),
    asn1_test_lib:compile("OLD-MEDIA-GATEWAY-CONTROL.asn", Config, [Erule|Options]),
    {ok,'OLD-MEDIA-GATEWAY-CONTROL','MEDIA-GATEWAY-CONTROL'}.


main(no_module,_) -> ok;
main('OLD-MEDIA-GATEWAY-CONTROL',Config) ->
    CaseDir = ?config(case_dir, Config),
    {ok,Msg} = asn1ct:value('OLD-MEDIA-GATEWAY-CONTROL','MegacoMessage',
                            [{i, CaseDir}]),
    ?line {ok,Bytes} = asn1_wrapper:encode('OLD-MEDIA-GATEWAY-CONTROL',
					   'MegacoMessage',Msg),
    ?line {ok,Msg} = asn1_wrapper:decode('OLD-MEDIA-GATEWAY-CONTROL',
					 'MegacoMessage',
					 Bytes),
    ok;
main(Mod='MEDIA-GATEWAY-CONTROL',Config) ->
    ?line DataDir = ?config(data_dir,Config),
    io:format("DataDir:~p~n",[DataDir]),
    ?line {ok,FilenameList} = file:list_dir(filename:join([DataDir,
							   megacomessages])),
    %% remove any junk files that may be in the megacomessage directory
    Pred = fun(X) ->
		   case lists:reverse(X) of
		       [$l,$a,$v,$.|_R] ->true;
		       _ -> false
		   end
	   end,
    MegacoMsgFilenameList = lists:filter(Pred,FilenameList),

    Fun = fun(F) ->
                  M = read_msg(filename:join([DataDir,megacomessages,F])),
		  {ok,B} = asn1_wrapper:encode(Mod,element(1,M),M),
		  {ok,M} = asn1_wrapper:decode(Mod,element(1,M),B)
	  end,
    ?line lists:foreach(Fun,MegacoMsgFilenameList),
    ok.

read_msg(File) ->
    case file:read_file(File) of
        {ok,Bin} ->
            binary_to_term(Bin);
        _ -> 
	    io:format("couldn't read file ~p~n",[File])
    end.
