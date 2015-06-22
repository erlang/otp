%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1rt_driver_handler.erl,v 1.1 2008/12/17 09:53:30 mikpe Exp $

-module(asn1rt_driver_handler).

-export([init/1,load_driver/0,unload_driver/0]).


load_driver() ->
    spawn(asn1rt_driver_handler, init, [self()]).

init(From) ->
    Port=
	case load_driver("asn1_erl_drv") of
	    ok ->
		open_named_port(From);
	    already_done ->
		From ! driver_ready;
	    Error -> % if erl_ddll:load_driver fails
		erl_ddll:unload_driver("asn1_erl_drv"),
		From ! Error
	end,
    register_and_loop(Port).

load_driver(DriverName) ->
    case is_driver_loaded(DriverName) of
	false ->
	    Dir = filename:join([code:priv_dir(asn1),"lib"]),
	    erl_ddll:load_driver(Dir,DriverName);
	true ->
	    ok
    end.


is_driver_loaded(_Name) ->
    case whereis(asn1_driver_owner) of
	undefined ->
	    false;
	_ ->
	    true
    end.

open_named_port(From) ->
    case is_port_open(drv_complete) of
	false ->
	    case catch open_port({spawn,"asn1_erl_drv"},[]) of
		{'EXIT',Reason} ->
		    From ! {port_error,Reason};
		Port ->
		    register(drv_complete,Port),
		    From ! driver_ready,
		    Port
	    end;
	_ ->
	    From ! driver_ready,
	    ok
    end.

is_port_open(Name) ->
    case whereis(Name) of
	Port when port(Port) ->
	    true;
	_ -> false
    end.

register_and_loop(Port) when port(Port) ->
    register(asn1_driver_owner,self()),
    loop();
register_and_loop(_) ->
    ok.

loop() ->
    receive
	unload ->
	    case whereis(drv_complete) of
		Port when port(Port) ->
		    port_close(Port);
		_ -> ok
	    end,
	    erl_ddll:unload_driver("asn1_erl_drv"),
	    ok;
	_ ->
	    loop()
    end.

unload_driver() ->
    case whereis(asn1_driver_owner) of
	Pid when pid(Pid) ->
	    Pid ! unload,
	    ok;
	_ ->
	    ok
    end.
