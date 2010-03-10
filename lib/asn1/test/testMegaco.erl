%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

-export([compile/3,main/2,msg11/0]).

-include_lib("test_server/include/test_server.hrl").
-define(MID, {ip4Address, #'IP4Address'{address = [124, 124, 124, 222],
                                            portNumber = 55555}}).
-define(A4444, ["11111111"]).

-record('MegacoMessage',
        {
          authHeader = asn1_NOVALUE,
          mess
         }).

-record('Message',
        {
          version, 
          mId, 
          messageBody
         }). % with extension mark

-record('IP4Address',
        {
          address,
          portNumber = asn1_NOVALUE
         }).

-record('TransactionRequest',
        {
          transactionId, 
          actions = []
         }). % with extension mark

-record('ActionRequest',
        {
          contextId, 
          contextRequest = asn1_NOVALUE, 
          contextAttrAuditReq = asn1_NOVALUE, 
          commandRequests = []
         }).

-record('CommandRequest',
        {
          command, 
          optional = asn1_NOVALUE, 
          wildcardReturn = asn1_NOVALUE
         }). % with extension mark

-record('NotifyRequest',
        {
          terminationID, 
          observedEventsDescriptor, 
          errorDescriptor = asn1_NOVALUE
         }). % with extension mark

-record('ObservedEventsDescriptor',
        {
          requestId, 
          observedEventLst = []
         }).

-record('ObservedEvent',
        {
          eventName, 
          streamID = asn1_NOVALUE, 
          eventParList = [], 
          timeNotation = asn1_NOVALUE
         }). % with extension mark

-record('EventParameter',
        {
          eventParameterName, 
          value
         }).

-record('TimeNotation',
        {
          date, 
          time
         }).

-record(megaco_term_id, {contains_wildcards = ["f"], id}).


compile(_Config,ber,[optimize]) ->
    {ok,no_module,no_module};
compile(_Config,per,[optimize]) ->
    {ok,no_module,no_module};
compile(Config,Erule,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(DataDir ++ 
			      "MEDIA-GATEWAY-CONTROL.asn",
			      [Erule,{outdir,OutDir}]++Options),
    
    ?line ok = asn1ct:compile(DataDir ++ 
			      "OLD-MEDIA-GATEWAY-CONTROL.asn",
			      [Erule,{outdir,OutDir}]++Options),
    {ok,'OLD-MEDIA-GATEWAY-CONTROL','MEDIA-GATEWAY-CONTROL'}.


main(no_module,_) -> ok;
main('OLD-MEDIA-GATEWAY-CONTROL',_) ->
%    Msg = msg11(),
    {ok,Msg} = asn1ct:value('OLD-MEDIA-GATEWAY-CONTROL','MegacoMessage'),
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


request(Mid, TransId, ContextId, CmdReq) when is_list(CmdReq) ->
    Actions = [#'ActionRequest'{contextId = ContextId,
                                commandRequests = CmdReq}],
    Req = {transactions,
           [{transactionRequest,
             #'TransactionRequest'{transactionId = TransId,
                                   actions = Actions}}]},
    #'MegacoMessage'{mess = #'Message'{version = 1,
                                       mId = Mid,
                                       messageBody = Req}}.

msg11() ->
    TimeStamp = #'TimeNotation'{date = "19990729",
                                time = "22010001"},
    Parm = #'EventParameter'{eventParameterName = "ds",
                             value = "916135551212"},

    Event = #'ObservedEvent'{eventName = "ddce",
                             timeNotation = TimeStamp,
                             eventParList = [Parm]},
    Desc = #'ObservedEventsDescriptor'{requestId = 2223,
                                       observedEventLst = [Event]},
    NotifyReq = #'NotifyRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                                 observedEventsDescriptor = Desc},
    CmdReq = #'CommandRequest'{command = {notifyReq, NotifyReq}},
    request(?MID, 10002, 0, [CmdReq]).
