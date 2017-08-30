%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%% 
%%----------------------------------------------------------------------
%% File        : CosFileTransfer_FileIterator_impl.erl
%% Description : 
%%
%% Created     : 12 Sept 2000
%%----------------------------------------------------------------------
-module('CosFileTransfer_FileIterator_impl').




%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("cosFileTransferApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

%% Interface functions
-export([next_one/2,
	 next_n/3,
	 destroy/2]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([FileList]) ->
    {ok, FileList}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------%
%% function : handle_info/2
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    case Info of
        {'EXIT', _Pid, Reason} ->
            {stop, Reason, State};
        _Other ->
            {noreply, State}
    end.

%%======================================================================
%% CosFileTransfer::FileIterator
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : next_one
%% Arguments  : 
%% Returns    : {boolean(), FileWrapper}
%% Description: 
%%----------------------------------------------------------------------
next_one(_OE_This, []) ->
    {reply, {false, 
	     #'CosFileTransfer_FileWrapper'{the_file = corba:create_nil_objref(),
					    file_type = nfile}}, []};
next_one(_OE_This, [FileWrapper]) ->
    {reply, {true, FileWrapper}, []};
next_one(_OE_This, [FileWrapper|Rest]) ->
    {reply, {true, FileWrapper}, Rest}.

%%---------------------------------------------------------------------%
%% Function   : next_n
%% Arguments  : HowMany - ulong()
%% Returns    : {boolean(), FileWrapperList}
%% Description: 
%%----------------------------------------------------------------------
next_n(_OE_This, [], _) ->
    {reply, {false, []}, []};
next_n(_OE_This, FileWrapperList, HowMany) when HowMany > length(FileWrapperList) ->
    {reply, {true, FileWrapperList}, []};
next_n(_OE_This, FileWrapperList, HowMany) ->
    {reply, {true, lists:sublist(FileWrapperList, HowMany)}, 
     lists:nthtail(HowMany, FileWrapperList)}.

%%---------------------------------------------------------------------%
%% Function   : destroy
%% Arguments  : -
%% Returns    : -
%% Description: 
%%----------------------------------------------------------------------
destroy(_OE_This, State) ->
    {stop, normal, ok, State}.


%%======================================================================
%% Internal functions
%%======================================================================

    

%%======================================================================
%% END OF MODULE
%%======================================================================
