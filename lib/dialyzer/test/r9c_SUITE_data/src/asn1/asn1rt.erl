%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1rt.erl,v 1.1 2008/12/17 09:53:30 mikpe Exp $
%%
-module(asn1rt).

%% Runtime functions for ASN.1 (i.e encode, decode)

-export([encode/2,encode/3,decode/3,load_driver/0,unload_driver/0,info/1]).

encode(Module,{Type,Term}) ->
    encode(Module,Type,Term).

encode(Module,Type,Term) ->
    case catch apply(Module,encode,[Type,Term]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Type}}};
	Result ->
	    Result
    end.

decode(Module,Type,Bytes) ->
    case catch apply(Module,decode,[Type,Bytes]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Type}}};
	Result ->
	    Result
    end.

load_driver() ->
    asn1rt_driver_handler:load_driver(),
    receive
	driver_ready ->
	    ok;
	Err={error,_Reason} ->
	    Err;
	Error ->
	    {error,Error}
    end.

unload_driver() ->
    case catch asn1rt_driver_handler:unload_driver() of
	ok ->
	    ok;
	Error ->
	    {error,Error}
    end.


info(Module) ->
    case catch apply(Module,info,[]) of
	{'EXIT',{undef,_Reason}} ->
	    {error,{asn1,{undef,Module,info}}};
	Result ->
	    {ok,Result}
    end.
