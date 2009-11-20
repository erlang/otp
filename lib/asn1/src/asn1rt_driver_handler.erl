%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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

-module(asn1rt_driver_handler).

-include("asn1_records.hrl").

-export([load_driver/0,unload_driver/0,client_port/0]).

%% Internal exports
-export([init/2]).

%% Macros
-define(port_names,
	{ asn1_drv01, asn1_drv02, asn1_drv03, asn1_drv04,
	  asn1_drv05, asn1_drv06, asn1_drv07, asn1_drv08,
	  asn1_drv09, asn1_drv10, asn1_drv11, asn1_drv12,
	  asn1_drv13, asn1_drv14, asn1_drv15, asn1_drv16 }).

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------
load_driver() ->
    load_driver(noreason).

load_driver(Reason) ->
    Ref = make_ref(),
    case whereis(asn1_driver_owner) of % to prevent unnecessary spawn
	Pid when is_pid(Pid) ->
	    asn1_driver_owner ! {self(),Ref,are_you_ready},
	    receive
		{Ref,driver_ready} ->
		    ok
	    after 10000 ->
		    {error,{timeout,waiting_for_drivers}}
	    end;
	_ ->
	    {_,Mref} = spawn_monitor(asn1rt_driver_handler, init, [self(),Ref]),
	    receive
		{'DOWN', Mref, _, _, NewReason} ->
		    case NewReason of
			Reason -> {error,Reason};
			_ -> load_driver(NewReason)
		    end;
		{Ref,driver_ready} ->
		    erlang:demonitor(Mref),
		    ok;
		{Ref,Error = {error,_Reason}} ->
		    erlang:demonitor(Mref),
		    Error
	    after 10000 -> %% 10 seconds
		    {error,{timeout,waiting_for_drivers}}
	    end
    end.

init(FromPid,FromRef) ->
    register(asn1_driver_owner,self()),
    Dir = filename:join([code:priv_dir(asn1),"lib"]),
    case catch erl_ddll:load_driver(Dir,asn1_erl_drv) of
	ok ->
	    Result = open_named_ports(),
	    catch (FromPid ! {FromRef,Result}),
	    loop(Result);
	{error,Err} -> % if erl_ddll:load_driver fails
	    ForErr = erl_ddll:format_error(Err),
	    OSDir = filename:join(Dir,erlang:system_info(system_architecture)),
	    case catch erl_ddll:load_driver(OSDir,asn1_erl_drv) of
		ok ->
		    Result = open_named_ports(),
		    catch (FromPid ! {FromRef,Result}),
		    loop(Result);
		{error,Err2} ->
%		    catch (FromPid ! {FromRef,Error})
		    ForErr2 = erl_ddll:format_error(Err2),
		    catch (FromPid ! {FromRef,{error,{{Dir,ForErr},{OSDir,ForErr2}}}})
	    end
    end.

    
open_named_ports() ->
    open_named_ports(size(?port_names)).

open_named_ports(0) ->
    driver_ready;
open_named_ports(N) ->
    case catch open_port({spawn,"asn1_erl_drv"},[]) of
	{'EXIT',Reason} ->
	    {error,{port_error,Reason}};
	Port ->
	    register(element(N,?port_names),Port),
	    open_named_ports(N-1)
    end.

loop(Result) ->
    receive
	{_FromPid,_FromRef,unload} ->
	    close_ports(size(?port_names)),
	    erl_ddll:unload_driver(asn1_erl_drv),
	    ok;
	{FromPid,FromRef,are_you_ready} ->
	    catch (FromPid ! {FromRef,driver_ready}),
	    loop(Result);
	_ ->
	    loop(Result)
    end.

unload_driver() ->
    case whereis(asn1_driver_owner) of
	Pid when is_pid(Pid) ->
	    Pid ! {self(),make_ref(),unload},
	    ok;
	_ -> 
	    ok
    end.

close_ports(0) ->
    ok;
close_ports(N) ->   
    element(N,?port_names) ! {self(), close}, %% almost same as port_close(Name)
    close_ports(N-1).

client_port() ->
    element(erlang:system_info(scheduler_id) rem size(?port_names) + 1,
	    ?port_names).
