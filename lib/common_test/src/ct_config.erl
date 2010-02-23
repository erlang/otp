%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : ct_config.erl
%% Description : CT module for reading and manipulating of configuration
%%		 data
%%
%% Created : 15 February 2010
%%----------------------------------------------------------------------
-module(ct_config).

% start of the config server
-export([start/0, start/1, start/2, stop/0]).

% manipulating with config files
-export([read_config_files/1,
	get_config_file_list/1]).

% require
-export([require/1, require/2]).

% get config data
-export([get_config/1, get_config/2, get_config/3,
	get_all_config/0]).

% set config data
-export([set_config/1, set_config/2, set_config/3,
	set_default_config/2, set_default_config/3]).

% delete config data
-export([delete_config/1, delete_default_config/1]).

% update and reload config
-export([%reload_config/1,
	update_config/2]).

% ?
-export([release_allocated/0]).

-export([encrypt_config_file/2, encrypt_config_file/3,
	 decrypt_config_file/2, decrypt_config_file/3,
	 get_crypt_key_from_file/0, get_crypt_key_from_file/1]).

% references
-export([get_ref_from_name/1, get_name_from_ref/1, get_key_from_name/1]).

-include("ct_util.hrl").
-include("ct_event.hrl").

-define(cryptfile, ".ct_config.crypt").

% TODO: add handler field here
-record(ct_conf,{key,value,ref,name='_UNDEF',default=false}).

%%%-----------------------------------------------------------------
%%% @spec start(Mode) -> Pid | exit(Error)
%%%       Mode = normal | interactive
%%%       Pid = pid()
%%%
%%% @doc Start start the ct_config_server process
%%% (tool-internal use only).
%%%
%%% <p>This function is called from ct_run.erl. It starts and initiates
%%% the <code>ct_config_server</code></p>
%%%
%%% <p>Returns the process identity of the
%%% <code>ct_config_server</code>.</p>
%%%
%%% @see ct
start() ->
    start(normal,".").

start(LogDir) when is_list(LogDir) ->
    start(normal,LogDir);
start(Mode) ->
    start(Mode,".").

start(Mode,LogDir) ->
    case whereis(ct_config_server) of
	undefined ->
	    Me = self(),
	    Pid = spawn_link(fun() -> do_start(Me) end),
	    receive
		{Pid,started} -> Pid;
		{Pid,Error} -> exit(Error)
	    end;
	Pid ->
	    case ct_util:get_mode() of
		interactive when Mode==interactive ->
		    Pid;
		interactive ->
		    {error,interactive_mode};
		_OtherMode ->
		    Pid
	    end
    end.

do_start(Parent) ->
    process_flag(trap_exit,true),
    register(ct_config_server,self()),
    ct_util:create_table(?attr_table,bag,#ct_conf.key),
    {ok,StartDir} = file:get_cwd(),
    Opts = case ct_util:read_opts() of
	       {ok,Opts1} ->
		   Opts1;
	       Error ->
		   Parent ! {self(),Error},
		   exit(Error)
	   end,
    case read_config_files(Opts) of
	ok ->
	    Parent ! {self(),started},
	    loop(StartDir);
	ReadError ->
	    Parent ! {self(),ReadError},
	    exit(ReadError)
    end.

%%%-----------------------------------------------------------------
%%% @spec stop() -> ok
%%%
%%% @doc Stop the ct_config_server and close all existing connections
%%% (tool-internal use only).
%%%
%%% @see ct
stop() ->
    case whereis(ct_config_server) of
	undefined -> ok;
	_ -> call({stop})
    end.

call(Msg) ->
    MRef = erlang:monitor(process, whereis(ct_config_server)),
    Ref = make_ref(),
    ct_config_server ! {Msg,{self(),Ref}},
    receive
	{Ref, Result} ->
	    erlang:demonitor(MRef),
	    Result;
	{'DOWN',MRef,process,_,Reason} ->
	    {error,{ct_util_server_down,Reason}}
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result}.


loop(StartDir) ->
    receive
	{{require,Name,Tag,SubTags},From} ->
	    Result = do_require(Name,Tag,SubTags),
	    return(From,Result),
	    loop(StartDir);
	{{set_default_config,{Config,Scope}},From} ->
	    ct_config:set_config(Config,{true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{set_default_config,{Name,Config,Scope}},From} ->
	    ct_config:set_config(Name,Config,{true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{delete_default_config,Scope},From} ->
	    ct_config:delete_config({true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{update_config,{Name,NewConfig}},From} ->
	    update_conf(Name,NewConfig),
	    return(From,ok),
	    loop(StartDir);
	{{stop},From} ->
	    ets:delete(?attr_table),
	    file:set_cwd(StartDir),
	    return(From,ok)
    end.

read_config_files(Opts) ->
    AddCallback = fun(CallBack, Files)->
	lists:map(fun(X)-> {CallBack, X} end, Files)
    end,
    ConfigFiles = case lists:keyfind(config, 1, Opts) of
	{config, ConfigLists}->
	    lists:foldr(fun({Callback,Files}, Acc)->
				AddCallback(Callback,Files) ++ Acc
			end,
			[],
			ConfigLists);
	false->
	    []
    end,
    read_config_files_int(ConfigFiles).

read_config_files_int([{Callback, File}|Files])->
    case Callback:read_config_file(File) of
	{ok, Config}->
	    set_config(Config),
	    read_config_files_int(Files);
	{error, ErrorName, ErrorDetail}->
	    {user_error, {ErrorName, File, ErrorDetail}}
    end;
read_config_files_int([])->
    ok.

set_config(Config) ->
    set_config('_UNDEF',Config,false).

set_config(Config,Default) ->
    set_config('_UNDEF',Config,Default).

set_config(Name,Config,Default) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,value=Val,ref=ct_util:ct_make_ref(),
			 name=Name,default=Default}) ||
	{Key,Val} <- Config].

get_config(KeyOrName,Default,Opts) when is_atom(KeyOrName) ->
    case lookup_config(KeyOrName) of
	[] ->
	    Default;
	[{_Ref,Val}|_] = Vals ->
	    case {lists:member(all,Opts),lists:member(element,Opts)} of
		{true,true} ->
		    [{KeyOrName,V} || {_R,V} <- lists:sort(Vals)];
		{true,false} ->
		    [V || {_R,V} <- lists:sort(Vals)];
		{false,true} ->
		    {KeyOrName,Val};
		{false,false} ->
		    Val
	    end
    end;

get_config({KeyOrName,SubKey},Default,Opts) ->
    case lookup_config(KeyOrName) of
	[] ->
	    Default;
	Vals ->
	    Vals1 = case [Val || {_Ref,Val} <- lists:sort(Vals)] of
			Result=[L|_] when is_list(L) ->
			    case L of
				[{_,_}|_] ->
				    Result;
				_ ->
				    []
			    end;
			_ ->
			    []
		    end,
	    case get_subconfig([SubKey],Vals1,[],Opts) of
		{ok,[{_,SubVal}|_]=SubVals} ->
		    case {lists:member(all,Opts),lists:member(element,Opts)} of
			{true,true} ->
			    [{{KeyOrName,SubKey},Val} || {_,Val} <- SubVals];
			{true,false} ->
			    [Val || {_SubKey,Val} <- SubVals];
			{false,true} ->
			    {{KeyOrName,SubKey},SubVal};
			{false,false} ->
			    SubVal
		    end;
		_ ->
		    Default
	    end
    end.


%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
update_conf(Name, NewConfig) ->
    Old = ets:select(?attr_table,[{#ct_conf{name=Name,_='_'},[],['$_']}]),
    lists:foreach(fun(OldElem) ->
			  NewElem = OldElem#ct_conf{value=NewConfig},
			  ets:delete_object(?attr_table, OldElem),
			  ets:insert(?attr_table, NewElem)
		  end, Old),
    ok.

%%%-----------------------------------------------------------------
%%% @spec release_allocated() -> ok
%%%
%%% @doc Release all allocated resources, but don't take down any
%%% connections.
release_allocated() ->
    Allocated = ets:select(?attr_table,[{#ct_conf{name='$1',_='_'},
					 [{'=/=','$1','_UNDEF'}],
					 ['$_']}]),
    release_allocated(Allocated).
release_allocated([H|T]) ->
    ets:delete_object(?attr_table,H),
    ets:insert(?attr_table,H#ct_conf{name='_UNDEF'}),
    release_allocated(T);
release_allocated([]) ->
    ok.

allocate(Name,Key,SubKeys) ->
    case ets:match_object(?attr_table,#ct_conf{key=Key,name='_UNDEF',_='_'}) of
	[] ->
	    {error,{not_available,Key}};
	Available ->
	    case allocate_subconfig(Name,SubKeys,Available,false) of
		ok ->
		    ok;
		Error ->
		    Error
	    end
    end.

allocate_subconfig(Name,SubKeys,[C=#ct_conf{value=Value}|Rest],Found) ->
    case do_get_config(SubKeys,Value,[]) of
	{ok,_SubMapped} ->
	    ets:insert(?attr_table,C#ct_conf{name=Name}),
	    allocate_subconfig(Name,SubKeys,Rest,true);
	_Error ->
	    allocate_subconfig(Name,SubKeys,Rest,Found)
    end;
allocate_subconfig(_Name,_SubKeys,[],true) ->
    ok;
allocate_subconfig(_Name,SubKeys,[],false) ->
    {error,{not_available,SubKeys}}.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_config/1
get_config(KeyOrName) ->
    ct_config:get_config(KeyOrName,undefined,[]).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_config/2
get_config(KeyOrName,Default) ->
    ct_config:get_config(KeyOrName,Default,[]).

get_subconfig(SubKeys,Values) ->
    get_subconfig(SubKeys,Values,[],[]).

get_subconfig(SubKeys,[Value|Rest],Mapped,Opts) ->
    case do_get_config(SubKeys,Value,[]) of
	{ok,SubMapped} ->
	    case lists:member(all,Opts) of
		true ->
		    get_subconfig(SubKeys,Rest,Mapped++SubMapped,Opts);
		false ->
		    {ok,SubMapped}
	    end;
	_Error ->
	    get_subconfig(SubKeys,Rest,Mapped,Opts)
    end;
get_subconfig(SubKeys,[],[],_) ->
    {error,{not_available,SubKeys}};
get_subconfig(_SubKeys,[],Mapped,_) ->
    {ok,Mapped}.

do_get_config([Key|Required],Available,Mapped) ->
    case lists:keysearch(Key,1,Available) of
	{value,{Key,Value}} ->
	    NewAvailable = lists:keydelete(Key,1,Available),
	    NewMapped = [{Key,Value}|Mapped],
	    do_get_config(Required,NewAvailable,NewMapped);
	false ->
	    {error,{not_available,Key}}
    end;
do_get_config([],_Available,Mapped) ->
    {ok,lists:reverse(Mapped)}.

get_all_config() ->
    ets:select(?attr_table,[{#ct_conf{name='$1',key='$2',value='$3',
				      default='$4',_='_'},
			     [],
			     [{{'$1','$2','$3','$4'}}]}]).

lookup_config(KeyOrName) ->
    case lookup_name(KeyOrName) of
	[] ->
	    lookup_key(KeyOrName);
	Values ->
	    Values
    end.

lookup_name(Name) ->
    ets:select(?attr_table,[{#ct_conf{ref='$1',value='$2',name=Name,_='_'},
			     [],
			     [{{'$1','$2'}}]}]).
lookup_key(Key) ->
    ets:select(?attr_table,[{#ct_conf{key=Key,ref='$1',value='$2',name='_UNDEF',_='_'},
			     [],
			     [{{'$1','$2'}}]}]).

delete_config(Default) ->
    ets:match_delete(?attr_table,#ct_conf{default=Default,_='_'}),
    ok.

process_user_configs(Opts, Acc)->
    case lists:keytake(userconfig, 1, Opts) of
	false->
	    Acc;
	{value, {userconfig, {Callback, Files=[File|_]}}, NewOpts} when
	    is_list(File)->
		process_user_configs(NewOpts, [{Callback, Files} | Acc]);
	{value, {userconfig, {Callback, File=[C|_]}}, NewOpts} when
	    is_integer(C)->
		process_user_configs(NewOpts, [{Callback, [File]} | Acc]);
	{value, {userconfig, {_Callback, []}}, NewOpts}->
	    process_user_configs(NewOpts, Acc)
    end.

process_default_configs(Opts)->
    case lists:keysearch(config, 1, Opts) of
	{value,{_,Files=[File|_]}} when is_list(File) ->
	    Files;
	{value,{_,File=[C|_]}} when is_integer(C) ->
	    [File];
	{value,{_,[]}} ->
	    [];
	false ->
	    []
    end.

get_config_file_list(Opts)->
    DefaultConfigs = process_default_configs(Opts),
    CfgFiles =
	if
	    DefaultConfigs == []->
		[];
	    true->
		[{ct_config_plain, DefaultConfigs}]
	end ++
	process_user_configs(Opts, []),
    CfgFiles.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:require/1
require(Key) when is_atom(Key) ->
    require({Key,[]});
require({Key,SubKeys}) when is_atom(Key) ->
    allocate('_UNDEF',Key,to_list(SubKeys));
require(Key) ->
    {error,{invalid,Key}}.


%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:require/2
require(Name,Key) when is_atom(Key) ->
    require(Name,{Key,[]});
require(Name,{Key,SubKeys}) when is_atom(Name), is_atom(Key) ->
    call({require,Name,Key,to_list(SubKeys)});
require(Name,Keys) ->
    {error,{invalid,{Name,Keys}}}.

to_list(X) when is_list(X) -> X;
to_list(X) -> [X].

do_require(Name,Key,SubKeys) when is_list(SubKeys) ->
    case get_key_from_name(Name) of
	{error,_} ->
	    allocate(Name,Key,SubKeys);
	{ok,Key} ->
	    %% already allocated - check that it has all required subkeys
	    Vals = [Val || {_Ref,Val} <- lookup_name(Name)],
	    case get_subconfig(SubKeys,Vals) of
		{ok,_SubMapped} ->
		    ok;
		Error ->
		    Error
	    end;
	{ok,OtherKey} ->
	    {error,{name_in_use,Name,OtherKey}}
    end.

%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
encrypt_config_file(SrcFileName, EncryptFileName) ->
    case get_crypt_key_from_file() of
	{error,_} = E ->
	    E;
	Key ->
	    encrypt_config_file(SrcFileName, EncryptFileName, {key,Key})
    end.


get_ref_from_name(Name) ->
    case ets:select(?attr_table,[{#ct_conf{name=Name,ref='$1',_='_'},
				  [],
				  ['$1']}]) of
	[Ref] ->
	    {ok,Ref};
	_ ->
	    {error,{no_such_name,Name}}
    end.

get_name_from_ref(Ref) ->
    case ets:select(?attr_table,[{#ct_conf{name='$1',ref=Ref,_='_'},
				  [],
				  ['$1']}]) of
	[Name] ->
	    {ok,Name};
	_ ->
	    {error,{no_such_ref,Ref}}
    end.

get_key_from_name(Name) ->
    case ets:select(?attr_table,[{#ct_conf{name=Name,key='$1',_='_'},
				  [],
				  ['$1']}]) of
	[Key|_] ->
	    {ok,Key};
	_ ->
	    {error,{no_such_name,Name}}
    end.

%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
encrypt_config_file(SrcFileName, EncryptFileName, {file,KeyFile}) ->
    case get_crypt_key_from_file(KeyFile) of
	{error,_} = E ->
	    E;
	Key ->
	    encrypt_config_file(SrcFileName, EncryptFileName, {key,Key})
    end;

encrypt_config_file(SrcFileName, EncryptFileName, {key,Key}) ->
    crypto:start(),
    {K1,K2,K3,IVec} = make_crypto_key(Key),
    case file:read_file(SrcFileName) of
	{ok,Bin0} ->
	    Bin1 = term_to_binary({SrcFileName,Bin0}),
	    Bin2 = case byte_size(Bin1) rem 8 of
		       0 -> Bin1;
		       N -> list_to_binary([Bin1,random_bytes(8-N)])
		   end,
	    EncBin = crypto:des3_cbc_encrypt(K1, K2, K3, IVec, Bin2),
	    case file:write_file(EncryptFileName, EncBin) of
		ok ->
		    io:format("~s --(encrypt)--> ~s~n",
			      [SrcFileName,EncryptFileName]),
		    ok;
		{error,Reason} ->
		    {error,{Reason,EncryptFileName}}
	    end;
	{error,Reason} ->
	    {error,{Reason,SrcFileName}}
    end.

%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
decrypt_config_file(EncryptFileName, TargetFileName) ->
    case get_crypt_key_from_file() of
	{error,_} = E ->
	    E;
	Key ->
	    decrypt_config_file(EncryptFileName, TargetFileName, {key,Key})
    end.


set_default_config(NewConfig, Scope) ->
    call({set_default_config, {NewConfig, Scope}}).

set_default_config(Name, NewConfig, Scope) ->
    call({set_default_config, {Name, NewConfig, Scope}}).

delete_default_config(Scope) ->
    call({delete_default_config, Scope}).

update_config(Name, Config) ->
    call({update_config, {Name, Config}}).

%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
decrypt_config_file(EncryptFileName, TargetFileName, {file,KeyFile}) ->
    case get_crypt_key_from_file(KeyFile) of
	{error,_} = E ->
	    E;
	Key ->
	    decrypt_config_file(EncryptFileName, TargetFileName, {key,Key})
    end;

decrypt_config_file(EncryptFileName, TargetFileName, {key,Key}) ->
    crypto:start(),
    {K1,K2,K3,IVec} = make_crypto_key(Key),
    case file:read_file(EncryptFileName) of
	{ok,Bin} ->
	    DecBin = crypto:des3_cbc_decrypt(K1, K2, K3, IVec, Bin),
	    case catch binary_to_term(DecBin) of
		{'EXIT',_} ->
		    {error,bad_file};
		{_SrcFile,SrcBin} ->
		    case TargetFileName of
			undefined ->
			    {ok,SrcBin};
			_ ->
			    case file:write_file(TargetFileName, SrcBin) of
				ok ->
				    io:format("~s --(decrypt)--> ~s~n",
					      [EncryptFileName,TargetFileName]),
				    ok;
				{error,Reason} ->
				    {error,{Reason,TargetFileName}}
			    end
		    end
	    end;
	{error,Reason} ->
	    {error,{Reason,EncryptFileName}}
    end.


get_crypt_key_from_file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    case catch string:tokens(binary_to_list(Bin), [$\n,$\r]) of
		[Key] ->
		    Key;
		_ ->
		    {error,{bad_crypt_file,File}}
	    end;
	{error,Reason} ->
	    {error,{Reason,File}}
    end.

get_crypt_key_from_file() ->
    CwdFile = filename:join(".",?cryptfile),
    {Result,FullName} =
	case file:read_file(CwdFile) of
	    {ok,Bin} ->
		{Bin,CwdFile};
	    _ ->
		case init:get_argument(home) of
		    {ok,[[Home]]} ->
			HomeFile = filename:join(Home,?cryptfile),
			case file:read_file(HomeFile) of
			    {ok,Bin} ->
				{Bin,HomeFile};
			    _ ->
				{{error,no_crypt_file},noent}
			end;
		    _ ->
			{{error,no_crypt_file},noent}
		end
	end,
    case FullName of
	noent ->
	    Result;
	_ ->
	    case catch string:tokens(binary_to_list(Result), [$\n,$\r]) of
		[Key] ->
		    io:format("~nCrypt key file: ~s~n", [FullName]),
		    Key;
		_ ->
		    {error,{bad_crypt_file,FullName}}
	    end
    end.

make_crypto_key(String) ->
    <<K1:8/binary,K2:8/binary>> = First = erlang:md5(String),
    <<K3:8/binary,IVec:8/binary>> = erlang:md5([First|lists:reverse(String)]),
    {K1,K2,K3,IVec}.

random_bytes(N) ->
    {A,B,C} = now(),
    random:seed(A, B, C),
    random_bytes_1(N, []).

random_bytes_1(0, Acc) -> Acc;
random_bytes_1(N, Acc) -> random_bytes_1(N-1, [random:uniform(255)|Acc]).
