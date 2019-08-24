%%%----------------------------------------------------------------------
%%% File    : uds_server.erl
%%% Purpose : Holder for the uds_drv ddll driver.
%%% Created : 15 Mar 2000 
%%%----------------------------------------------------------------------

-module(uds_server).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DRIVER_NAME,"uds_drv").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    case load_driver() of
	ok ->
	    {ok, []};
	{error, already_loaded} ->
	    {ok, []};
	Error ->
	    exit(Error)
    end.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    erl_ddll:unload_driver(?DRIVER_NAME),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Actually load the driver.
%%
load_driver() ->
    Dir = find_priv_lib(),
    erl_ddll:load_driver(Dir,?DRIVER_NAME).

%%
%% As this server may be started by the distribution, it is not safe to assume 
%% a working code server, neither a working file server.
%% I try to utilize the most primitive interfaces available to determine
%% the directory of the port_program.
%%
find_priv_lib() ->
    PrivDir = case (catch code:priv_dir(uds_dist)) of
		  {'EXIT', _} ->
		      %% Code server probably not startet yet
		      {ok, P} = erl_prim_loader:get_path(),
		      ModuleFile = atom_to_list(?MODULE) ++ extension(),
		      Pd = (catch lists:foldl
			    (fun(X,Acc) ->
				     M = filename:join([X, ModuleFile]),
				     %% The file server probably not started
				     %% either, has to use raw interface.
				     case file:raw_read_file_info(M) of 
					 {ok,_} -> 
					     %% Found our own module in the
					     %% path, lets bail out with
					     %% the priv_dir of this directory
					     Y = filename:split(X),
					     throw(filename:join
						   (lists:sublist
						    (Y,length(Y) - 1) 
						    ++ ["priv"])); 
					 _ -> 
					     Acc 
				     end 
			     end,
			     false,P)),
		      case Pd of
			  false ->
			      exit(uds_dist_priv_lib_indeterminate);
			  _ ->
			      Pd
		      end;
		  Dir ->
		      Dir
	      end,
    filename:join([PrivDir, "lib"]).

extension() ->
    %% erlang:info(machine) returns machine name as text in all uppercase
    "." ++ lists:map(fun(X) ->
			     X + $a - $A
		     end,
		     erlang:info(machine)).

