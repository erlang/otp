%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wx.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 22 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

%% @doc A port of <a href="http://www.wxwidgets.org/">wxWidgets</a>.
%% 
%% This is the base api of <a href="http://www.wxwidgets.org/">wxWidgets</a>.
%% This module contains functions for 
%% starting and stopping the wx-server, as well as  other utility functions.
%%
%% wxWidgets is object oriented, and not functional. Thus, in wxErlang a
%% module represents a class, and the object created by this class
%% has an own type, wxCLASS().  This module represents the base
%% class, and all other wxMODULE's are sub-classes of this class.
%% 
%% Objects of a class are created with wxCLASS:new(...) and destroyed with 
%% wxCLASS:destroy(). Member functions are called with wxCLASS:member(Object, ...)
%% instead of as in C++ Object.member(...).
%% 
%% Sub class modules inherit (non static) functions from their parents.
%% The inherited functions are not documented in the sub-classes.
%% 
%% This erlang port of wxWidgets tries to be a one-to-one mapping with
%% the original wxWidgets library. Some things are different though,
%% as the optional arguments use property lists and can be in any
%% order. The main difference is the event handling which is different
%% from the original library. See {@link wxEvtHandler}.
%%
%% The following classes are implemented directly as erlang types: <br />
%% wxPoint={x,y},wxSize={w,h},wxRect={x,y,w,h},wxColour={r,g,b [,a]},
%% wxString={@link //stdlib/unicode:charlist()},
%% wxGBPosition={r,c},wxGBSpan={rs,cs},wxGridCellCoords={r,c}.
%%
%% wxWidgets uses a process specific environment, which is created by
%% {@link wx:new/0}.  To be able to use the environment from other
%% processes, call {@link get_env/0} to retrieve the environment and
%% {@link set_env/1} to assign the environment in the other process.
%%
%% Global (classless) functions are located in the wx_misc module.

%% @type wxObject().      Opaque object 
%% @type wx_env().  Wx process environment
%% @type wx_mem().  Wx memory area
%% @type colour().  A 3 or 4 tuple: {R,G,B,A} or as argument {R,G,B} is also accepted
%%                  where each colour channel is a an integer between 0-255. 
%% @type datetime(). {{Year,Month,Day}, {Hour,Minute,Second}} in local timezone.
%% @type mouseState().   See #wxMouseState{} defined in wx.hrl


-module(wx).

-export([parent_class/1, new/0, new/1, destroy/0,
	 get_env/0,set_env/1, debug/1,
	 batch/1,foreach/2,map/2,foldl/3,foldr/3,
	 getObjectType/1, typeCast/2, 
	 null/0, is_null/1]).

-export([create_memory/1, get_memory_bin/1,
	 retain_memory/1, release_memory/1]).


-export([demo/0]).

-include("wxe.hrl").

%% @hidden
parent_class(_) -> true. %% Let the null pointers be sent down.

%% @spec () -> wxObject()
%% @doc Starts a wx server.
new() ->
    new([]).

%% @spec ([Option]) -> wxObject()
%% @doc Starts a wx server. 
%% Option may be {debug, Level}, see debug/1.
new(Options) when is_list(Options) ->
    #wx_env{} = wxe_server:start(),
    Debug = proplists:get_value(debug, Options, 0),
    debug(Debug),
    null().

%% @spec () -> ok
%% @doc Stops a wx server.
destroy() ->
    wxe_server:stop(),
    erase(?WXE_IDENTIFIER),
    ok.

%% @spec () -> wx_env()
%% @doc Gets this process's current wx environment.
%% Can be sent to other processes to allow them use this process wx environment.
%% @see set_env/1
get_env() ->
    case get(?WXE_IDENTIFIER) of
	undefined -> erlang:error({wxe,unknown_port});
	Env = #wx_env{} -> Env
    end.

%% @spec (wx_env()) -> ok
%% @doc Sets the process wx environment, allows this process to use
%% another process wx environment.
set_env(#wx_env{sv=Pid} = Env) ->
    put(?WXE_IDENTIFIER, Env),
    %%    wxe_util:cast(?REGISTER_PID, <<>>),
    wxe_server:register_me(Pid),
    ok.

%% @spec () -> wxObject()
%% @doc Returns the null object
null() ->
    #wx_ref{ref=0, type=wx}.

%% @spec (wxObject()) -> boolean()
%% @doc Returns true if object is null, false otherwise
is_null(#wx_ref{ref=NULL}) -> NULL =:= 0.

%% @spec (wxObject()) -> atom()
%% @doc Returns the object type
getObjectType(#wx_ref{type=Type}) ->
    Type.

%% @spec (wxObject(), atom()) -> wxObject()
%% @doc Casts the object to class NewType.
%%  It is needed when using functions like wxWindow:findWindow/2, which 
%%  returns a generic wxObject type.
typeCast(Old=#wx_ref{}, NewType) when is_atom(NewType) ->
    Old#wx_ref{type=NewType}.

%% @spec (function()) -> term() 
%% @doc Batches all <c>wx</c> commands
%% used in the fun.  Improves performance of the command processing by
%% grabbing the wxWidgets thread so that no event processing will be
%% done before the complete batch of commands is invoked.
%% 
%% @see map/2
%% @see foreach/2
%% @see foldl/3
%% @see foldr/3
batch(Fun) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try Fun()
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), list()) -> ok
%% @doc Behaves like {@link //stdlib/lists:foreach/2} but batches wx commands. See {@link batch/1}. 
foreach(Fun, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:foreach(Fun, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), list()) -> list()
%% @doc Behaves like {@link //stdlib/lists:map/2} but batches wx commands. See {@link batch/1}. 
map(Fun, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:map(Fun, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), term(), list()) -> term()
%% @doc Behaves like {@link //stdlib/lists:foldl/3} but batches wx commands. See {@link batch/1}. 
foldl(Fun, Acc, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:foldl(Fun, Acc, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), term(), list()) -> term()
%% @doc Behaves like {@link //stdlib/lists:foldr/3} but batches wx commands. See {@link batch/1}. 
foldr(Fun, Acc, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:foldr(Fun, Acc, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

-define(MIN_BIN_SIZE, 64).  %% Current emulator min off heap size

%% @spec (integer()) -> wx_memory()
%% @doc Creates a memory area (of Size in bytes) which can be used by an external library (i.e. opengl).
%% It is up to the client to keep a reference to this object so it does
%% not get garbage collected by erlang while still in use by the external
%% library.
%%
%% This is far from erlang's intentional usage and can crash the erlang emulator.
%% Use it carefully.
create_memory(Size) when Size > ?MIN_BIN_SIZE ->
    #wx_mem{bin = <<0:(Size*8)>>, size = Size};
create_memory(Size) ->
    #wx_mem{bin = <<0:((?MIN_BIN_SIZE+1)*8)>>, size = Size}.

%% @spec (wx_memory()) -> binary()
%% @doc Returns the memory area as a binary.
get_memory_bin(#wx_mem{bin=Bin, size=Size}) when Size > ?MIN_BIN_SIZE ->
    Bin;
get_memory_bin(#wx_mem{bin=Bin, size=Size}) ->
    <<WithCorrectSize:Size/binary, _/binary>> = Bin,
    WithCorrectSize.

%% @spec (wx_memory()) -> ok
%% @doc Saves the memory from deletion until release_memory/1 is called.
%% If release_memory/1 is not called the memory will not be garbage collected.
retain_memory(#wx_mem{bin=Bin}) ->
    wxe_util:send_bin(Bin),
    ok = wxe_util:cast(?WXE_BIN_INCR, <<>>);
retain_memory(Bin) when is_binary(Bin) ->
    case byte_size(Bin) > ?MIN_BIN_SIZE of
	true  -> ok;
	false -> erlang:error(small_bin)
    end,
    wxe_util:send_bin(Bin),
    ok = wxe_util:cast(?WXE_BIN_INCR, <<>>).

release_memory(#wx_mem{bin=Bin}) ->
    wxe_util:send_bin(Bin),
    ok = wxe_util:cast(?WXE_BIN_DECR, <<>>);
release_memory(Bin) when is_binary(Bin) ->
    wxe_util:send_bin(Bin),
    ok = wxe_util:cast(?WXE_BIN_DECR, <<>>).
    



%% @spec (Level::term()) -> ok
%%   Level = none | verbose | trace | driver | [Level]
%% @doc Sets debug level. If debug level is verbose or trace
%% each call is printed on console. If Level is driver each allocated 
%% object and deletion is printed on the console.
debug(none) -> debug(0);
debug(verbose) -> debug(1);
debug(trace) -> debug(2);
debug(driver) -> debug(16);
debug([]) -> debug(0);

debug(List) when is_list(List) -> 
    {Drv,Erl} = 
	lists:foldl(fun(verbose, {Drv,_Erl}) -> 
			    {Drv,1};
		       (trace, {Drv,_Erl}) ->
			    {Drv,2};
		       (driver, {_Drv,Erl}) ->
			    {16, Erl}
		    end, {0,0}, List),
    debug(Drv + Erl);
debug(Level) when is_integer(Level) ->
    case get(?WXE_IDENTIFIER) of
	undefined -> erlang:error({wxe,unknown_port});
	#wx_env{debug=Old} when Old =:= Level -> ok;
	Env = #wx_env{sv=Server, port=Port, debug=Old} -> 
	    if 
		Old > 16, Level > 16 -> ok;
		Old < 16, Level < 16 -> ok;
		true ->
		    erlang:port_call(Port,?WXE_DEBUG_DRIVER, [Level bsr 4])
	    end,		    
	    put(?WXE_IDENTIFIER, Env#wx_env{debug=Level}),
	    wxe_server:set_debug(Server,Level),
	    ok
    end.

%% @spec () -> ok
%% @doc Starts a wxErlang demo if examples directory exists and is compiled
demo() ->
    Priv = code:priv_dir(wx),
    Demo = filename:join([filename:dirname(Priv),examples,demo]),
    Mod  = list_to_atom("demo"), %% Fool xref tests
    case file:set_cwd(Demo) of
	ok ->	             
	    apply(Mod, start, []);
	_  ->
	    {error, no_demo_dir}
    end.
    
