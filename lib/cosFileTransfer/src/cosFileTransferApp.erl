%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : cosFileTransferApp.erl
%% Purpose : 
%% Created : 25 Aug 2000
%%----------------------------------------------------------------------
-module(cosFileTransferApp).


%%--------------- INCLUDES -----------------------------------
-include("cosFileTransferApp.hrl").

%%--------------- EXPORTS-------------------------------------
%% cosFileTransferApp API external
-export([start/0, stop/0, install/0, uninstall/0, create_VFS/4, create_VFS/5, 
	 get_buffert_size/0]).

%% cosFileTransferApp API internal
-export([create_link/3, get_option/3, type_check/2, configure/2]).
 
%% Application callbacks
-export([start/2, init/1, stop/1]).

%% INTERNAL EXPORTS!! DO NOT USE THESE!!
-export([create_dir/2, create_dir/3, create_file/2, create_file/3, split_paths/1,
	 create_name/1]).

-export([ssl_server_certfile/0, ssl_client_certfile/0, ssl_port/0,
	 ssl_server_verify/0, 
	 ssl_client_verify/0,
	 ssl_server_depth/0, ssl_client_depth/0, 
	 ssl_server_cacertfile/0,
	 ssl_client_cacertfile/0]).


%%--------------- DEFINES ------------------------------------
-define(SUPERVISOR_NAME, oe_cosFileTransferSup).
-define(SUP_FLAG,        {simple_one_for_one,50,10}).
-define(SUP_DIRECTORY_SPEC(Name, Args), 
        ['CosFileTransfer_Directory',Args, 
         [{sup_child, true}, {regname, {global, Name}}]]).
-define(SUP_CHILD, 
        {"oe_FileTransferChild",
         {cosFileTransfer,create_link, []},
	 transient,100000,worker,
         []}).

%%------------------------------------------------------------
%% function : install
%% Arguments: -
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------
install() -> 
    oe_CosFileTransfer:oe_register().

%%------------------------------------------------------------
%% function : uninstall
%% Arguments: - 
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosFileTransfer from the IFR DB
%%------------------------------------------------------------
uninstall() -> 
    oe_CosFileTransfer:oe_unregister().

	
%%------------------------------------------------------------
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosFileTransfer application.
%%------------------------------------------------------------
start() ->
    application:start(cosFileTransfer).
stop() ->
    application:stop(cosFileTransfer).
 
%%------------------------------------------------------------
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosFileTransferApp, app_init).
 
 
%%------------------------------------------------------------
%% function : stop
%% Arguments: Arg - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
stop(_) ->
    ok.
 
%%-----------------------------------------------------------%
%% function : init
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
%% Starting using create_factory/X
init(own_init) ->
    {ok,{?SUP_FLAG, [?SUP_CHILD]}};
%% When starting as an application.
init(app_init) ->
    {ok,{?SUP_FLAG, [?SUP_CHILD]}}.
 
%%------------------------------------------------------------
%% function : create_VFS
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
create_VFS(Type, Content, Host, Port) ->
    create_VFS(Type, Content, Host, Port, []).

create_VFS('FTP', Content, Host, Port, Options) 
  when is_list(Host) andalso is_integer(Port) andalso is_list(Options) ->
    'CosFileTransfer_VirtualFileSystem':oe_create(['FTP', Content, Host, Port,
						   Options],
						  [{pseudo, true}]);
create_VFS({'NATIVE', Mod}, Content, Host, Port, Options)
  when is_list(Host) andalso is_integer(Port) andalso is_list(Options) ->
    'CosFileTransfer_VirtualFileSystem':oe_create([{'NATIVE', Mod}, Content, 
						   Host, Port, Options],
						  [{pseudo, true}]);
create_VFS(_, _, _, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------%
%% function : create_link
%% Arguments: Module - which Module to call
%%            Env/ArgList - ordinary oe_create arguments.
%% Returns  : 
%% Exception: 
%% Effect   : Necessary since we want the supervisor to be a 
%%            'simple_one_for_one'. Otherwise, using for example,
%%            'one_for_one', we have to call supervisor:delete_child
%%            to remove the childs startspecification from the 
%%            supervisors internal state.
%%------------------------------------------------------------
create_link(Module, Env, ArgList) ->
    Module:oe_create_link(Env, ArgList).

%%-----------------------------------------------------------%
%% function : get_option
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
get_option(Key, OptionList, DefaultList) ->
    case lists:keysearch(Key, 1, OptionList) of
        {value,{Key,Value}} ->
            Value;
        _ ->
            case lists:keysearch(Key, 1, DefaultList) of
                {value,{Key,Value}} ->
                    Value;
                _->
                    {error, "Invalid option"}
            end
    end.

%%-----------------------------------------------------------%
%% function : type_check
%% Arguments: Obj  - objectrefernce to test.
%%            Mod  - Module which contains typeID/0.
%% Returns  : 'ok' or raises exception.
%% Effect   : 
%%------------------------------------------------------------
type_check(Obj, Mod) ->
    case catch corba_object:is_a(Obj,Mod:typeID()) of
        true ->
            ok;
        _ ->
	    corba:raise(#'BAD_PARAM'{minor=700, completion_status=?COMPLETED_NO})
    end.


%%-----------------------------------------------------------%
%% function : create_name/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
create_name(Type) ->
    {MSec, Sec, USec} = erlang:now(),
    lists:concat(['oe_',node(),'_',Type,'_',MSec, '_', Sec, '_', USec]).


%%-----------------------------------------------------------%
%% function : get_buffert_size/0
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Lookup the configuration variable 'buffert_size'
%%------------------------------------------------------------
get_buffert_size() ->
    case application:get_env(cosFileTransfer, buffert_size) of
	{ok, Size}  when is_integer(Size) ->
	    Size;
	_ ->
	    ?DEFAULT_BUFSIZE
    end.

%%-----------------------------------------------------------%
%% function : configure/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
configure(buffert_size, Value) when is_integer(Value) ->
    do_configure(buffert_size, Value);
configure(ssl_port, Value) when is_integer(Value) ->
    do_safe_configure(ssl_port, Value);
configure(ssl_server_certfile, Value) when is_list(Value) ->
    do_safe_configure(ssl_server_certfile, Value);
configure(ssl_server_certfile, Value) when is_atom(Value) ->
    do_safe_configure(ssl_server_certfile, atom_to_list(Value));
configure(ssl_client_certfile, Value) when is_list(Value) ->
    do_safe_configure(ssl_client_certfile, Value);
configure(ssl_client_certfile, Value) when is_atom(Value) ->
    do_safe_configure(ssl_client_certfile, atom_to_list(Value));
configure(ssl_server_verify, Value) when is_integer(Value) ->
    do_safe_configure(ssl_server_verify, Value);
configure(ssl_client_verify, Value) when is_integer(Value) ->
    do_safe_configure(ssl_client_verify, Value);
configure(ssl_server_depth, Value) when is_integer(Value) ->
    do_safe_configure(ssl_server_depth, Value);
configure(ssl_client_depth, Value) when is_integer(Value) ->
    do_safe_configure(ssl_client_depth, Value);
configure(ssl_server_cacertfile, Value) when is_list(Value) ->
    do_safe_configure(ssl_server_cacertfile, Value);
configure(ssl_server_cacertfile, Value) when is_atom(Value) ->
    do_safe_configure(ssl_server_cacertfile, atom_to_list(Value));
configure(ssl_client_cacertfile, Value) when is_list(Value) ->
    do_safe_configure(ssl_client_cacertfile, Value);
configure(ssl_client_cacertfile, Value) when is_atom(Value) ->
    do_safe_configure(ssl_client_cacertfile, atom_to_list(Value));
configure(_, _) ->
    exit({error, "Bad configure parameter(s)"}).

%% This function may be used as long as it is safe to change a value at any time.
do_configure(Key, Value) ->
    case is_loaded() of
	false ->
	    application:load(cosFileTransfer),
	    application_controller:set_env(cosFileTransfer, Key, Value);
	true ->
	    application_controller:set_env(cosFileTransfer, Key, Value)
    end.


%% This function MUST(!!) be used when we cannot change a value if cosFileTransfer
%% is running.
do_safe_configure(Key, Value) ->
    case is_loaded() of
	false ->
	    application:load(cosFileTransfer),
	    application_controller:set_env(cosFileTransfer, Key, Value);
	true ->
	    case is_running() of
		false ->
		    application_controller:set_env(cosFileTransfer, Key, Value);
		true ->
		    exit("cosFileTransfer already running, the given key may not be updated!")
	    end
    end.

%%-----------------------------------------------------------%
%% function : SSL parameter access functions
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
ssl_port() ->
    case application:get_env(cosFileTransfer, ssl_port) of
	{ok, Port} when is_integer(Port) ->
	    Port;
	_ ->
	    -1
    end.

ssl_server_certfile() ->
    case application:get_env(cosFileTransfer, ssl_server_certfile) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	{ok, V2}  when is_atom(V2) ->
	    atom_to_list(V2);
	_What ->
	    {ok, Cwd} = file:get_cwd(),
	    filename:join(Cwd,"ssl_server_cert.pem")
    end.


ssl_client_certfile() ->
    case application:get_env(cosFileTransfer, ssl_client_certfile) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	{ok, V2}  when is_atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    {ok, Cwd} = file:get_cwd(),
	    filename:join(Cwd,"ssl_client_cert.pem")
    end.

ssl_server_verify() ->
    Verify = case application:get_env(cosFileTransfer, ssl_server_verify) of
		 {ok, V} when is_integer(V) ->
		     V;
		 _ ->
		     0
	     end,
    if
	Verify =< 2, Verify >= 0 ->
	    Verify;
	true ->
	   0
    end.
    
ssl_client_verify() ->
    Verify = case application:get_env(cosFileTransfer, ssl_client_verify) of
		 {ok, V1} when is_integer(V1) ->
		     V1;
		 _ ->
		     0
	     end,
    if
	Verify =< 2, Verify >= 0 ->
	    Verify;
	true ->
	    0
    end.

ssl_server_depth() ->
    case application:get_env(cosFileTransfer, ssl_server_depth) of
	{ok, V1} when is_integer(V1) ->
	    V1;
	_ ->
	    1
    end.
    
ssl_client_depth() ->
    case application:get_env(cosFileTransfer, ssl_client_depth) of
	{ok, V1} when is_integer(V1) ->
	    V1;
	_ ->
	    1
    end.


ssl_server_cacertfile() ->
    case application:get_env(cosFileTransfer, ssl_server_cacertfile) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	{ok, V2}  when is_atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    []
    end.
    
ssl_client_cacertfile() ->
    case application:get_env(cosFileTransfer, ssl_client_cacertfile) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	{ok, V2}  when is_atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    []
    end.


%%============================================================
%% Internal functions
%%============================================================
%%-----------------------------------------------------------%
%% function : is_loaded/0
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Check if the application is loaded
%%------------------------------------------------------------
is_loaded() ->
    is_loaded(application:loaded_applications()).

is_running() ->
    is_loaded(application:which_applications()).

is_loaded([]) ->
    false;
is_loaded([{cosFileTransfer, _, _} |_As]) ->
     true;
is_loaded([_ |As]) ->
    is_loaded(As).




%%-----------------------------------------------------------%
%% function : create_dir/3/4
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
create_dir(Session, FileNameList) ->
    create_dir(Session, FileNameList, corba:create_nil_objref()).
create_dir(Session, FileNameList, Parent) ->
    'CosFileTransfer_Directory':oe_create([lists:last(FileNameList), FileNameList, 
					   Parent, Session], 
					  [{pseudo, true}]).

%%-----------------------------------------------------------%
%% function : create_file/2/3
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
create_file(Session, FileNameList) ->
    create_file(Session, FileNameList, corba:create_nil_objref()).
create_file(Session, FileNameList, Parent) ->
    'CosFileTransfer_File':oe_create([lists:last(FileNameList), FileNameList, 
				      Parent, Session], [{pseudo, true}]).

%%-----------------------------------------------------------%
%% function : split_paths
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
split_paths(Listing) ->
    split_paths(string:tokens(Listing, ?SEPARATOR), []).
split_paths([], Acc) ->
    Acc;
split_paths([H|T], Acc) ->
     split_paths(T, [filename:split(H)|Acc]).


%%--------------- END OF MODULE ------------------------------


