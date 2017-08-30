%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File    : orber_iiop_tracer_stealth.erl
%% Purpose : Use for debugging only.
%%--------------------------------------------------------------------

-module(orber_iiop_tracer_stealth).


%% Interceptor functions.
-export([new_out_connection/5,
	 new_in_connection/5,
	 closed_in_connection/1,
	 closed_out_connection/1,
	 in_request_encoded/6,
	 in_reply_encoded/6,
	 out_reply_encoded/6,
	 out_request_encoded/6,
	 in_request/6,
	 in_reply/6,
	 out_reply/6,
	 out_request/6]).


%%--------------- INTERCEPTOR FUNCTIONS ----------------------
%%------------------------------------------------------------
%% function : new_in_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
new_in_connection(_Arg, PHost, PPort, SHost, SPort) ->
    error_logger:info_msg("=============== new_in_connection ========~n"
			  "Node      : ~p~n"
			  "From      : ~s:~p~n"
			  "To        : ~s:~p~n"
			  "==========================================~n", 
			  [node(), PHost, PPort, SHost, SPort]),
    {PHost, PPort, SHost, SPort}.
 
%%------------------------------------------------------------
%% function : new_out_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
new_out_connection(_Arg, PHost, PPort, SHost, SPort) ->
    error_logger:info_msg("=============== new_out_connection =======~n"
			  "Node      : ~p~n"
			  "From      : ~s:~p~n"
			  "To        : ~s:~p~n"
			  "==========================================~n", 
			  [node(), SHost, SPort, PHost, PPort]),
    {PHost, PPort, SHost, SPort}.
 
%%------------------------------------------------------------
%% function : closed_in_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
closed_in_connection(Arg) ->
    error_logger:info_msg("=============== closed_in_connection =====~n"
			  "Node      : ~p~n"
			  "Connection: ~p~n"
			  "==========================================~n", 
			  [node(), Arg]),
    Arg.
 
%%------------------------------------------------------------
%% function : closed_out_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
closed_out_connection(Arg) ->
    error_logger:info_msg("=============== closed_out_connection ====~n"
			  "Node      : ~p~n"
			  "Connection: ~p~n"
			  "==========================================~n", 
			  [node(), Arg]),
    Arg.
 
%%------------------------------------------------------------
%% function : in_request_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_request_encoded(_Ref, _ObjKey, _Ctx, _Op, Bin, Args) ->
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : in_reply_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_reply_encoded(_Ref, _ObjKey, _Ctx, _Op, Bin, Args) ->
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : out_reply_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_reply_encoded(_Ref, _ObjKey, _Ctx, _Op, Bin, Args) ->
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : out_request_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_request_encoded(_Ref, _ObjKey, _Ctx, _Op, Bin, Args) ->
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : in_request
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_request(Ref, _ObjKey, _Ctx, Op, Params, Args) ->
    error_logger:info_msg("=============== in_request ===============~n"
			  "Connection: ~p~n"
			  "Operation : ~p~n"
			  "==========================================~n", 
			  [Ref, Op]),
    {Params, Args}.
 
%%------------------------------------------------------------
%% function : in_reply
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_reply(Ref, _ObjKey, _Ctx, Op, Reply, Args) ->
    error_logger:info_msg("=============== in_reply =================~n"
			  "Connection: ~p~n"
			  "Operation : ~p~n"
			  "==========================================~n", 
			  [Ref, Op]),
    {Reply, Args}.
 
%%------------------------------------------------------------
%% function : out_reply
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_reply(Ref, _ObjKey, _Ctx, Op, Reply, Args) ->
    error_logger:info_msg("=============== out_reply ================~n"
			  "Connection: ~p~n"
			  "Operation : ~p~n"
			  "==========================================~n", 
			  [Ref, Op]),
    {Reply, Args}.
 
%%------------------------------------------------------------
%% function : out_request
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_request(Ref, _ObjKey, _Ctx, Op, Params, Args) ->
    error_logger:info_msg("=============== out_request ==============~n"
			  "Connection: ~p~n"
			  "Operation : ~p~n"
			  "==========================================~n", 
			  [Ref, Op]),
    {Params, Args}.

%%======================================================================
%% END OF MODULE
%%======================================================================

