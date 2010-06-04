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

-export([start/1, stop/0]).

-export([read_config_files/1,
	get_config_file_list/1]).

-export([require/1, require/2]).

-export([get_config/1, get_config/2, get_config/3,
	get_all_config/0]).

-export([set_default_config/2, set_default_config/3]).

-export([delete_default_config/1]).

-export([reload_config/1, update_config/2]).

-export([release_allocated/0]).

-export([encrypt_config_file/2, encrypt_config_file/3,
	 decrypt_config_file/2, decrypt_config_file/3,
	 get_crypt_key_from_file/0, get_crypt_key_from_file/1]).

-export([get_ref_from_name/1, get_name_from_ref/1, get_key_from_name/1]).

-export([check_config_files/1, prepare_config_list/1]).

-export([add_config/2, remove_config/2]).

-include("ct_util.hrl").

-define(cryptfile, ".ct_config.crypt").

-record(ct_conf,{key,value,handler,config,ref,name='_UNDEF',default=false}).

start(Mode) ->
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
	    erlang:demonitor(MRef, [flush]),
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
	    set_config(Config,{true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{set_default_config,{Name,Config,Scope}},From} ->
	    set_config(Name,Config,{true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{delete_default_config,Scope},From} ->
	    delete_config({true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{update_config,{Name,NewConfig}},From} ->
	    update_conf(Name,NewConfig),
	    return(From,ok),
	    loop(StartDir);
	{{reload_config, KeyOrName},From}->
	    NewValue = reload_conf(KeyOrName),
	    return(From, NewValue),
	    loop(StartDir);
	{{stop},From} ->
	    ets:delete(?attr_table),
	    file:set_cwd(StartDir),
	    return(From,ok)
    end.

set_default_config(NewConfig, Scope) ->
    call({set_default_config, {NewConfig, Scope}}).

set_default_config(Name, NewConfig, Scope) ->
    call({set_default_config, {Name, NewConfig, Scope}}).

delete_default_config(Scope) ->
    call({delete_default_config, Scope}).

update_config(Name, Config) ->
    call({update_config, {Name, Config}}).

reload_config(KeyOrName) ->
    call({reload_config, KeyOrName}).

process_default_configs(Opts) ->
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

process_user_configs(Opts, Acc) ->
    case lists:keytake(userconfig, 1, Opts) of
	false ->
	    lists:reverse(Acc);
	{value, {userconfig, Config=[{_,_}|_]}, NewOpts} ->
	    Acc1 = lists:map(fun({_Callback, []}=Cfg) ->
				     Cfg;
				({Callback, Files=[File|_]}) when is_list(File) ->
				     {Callback, Files};
				({Callback, File=[C|_]}) when is_integer(C) ->
				     {Callback, [File]}
			     end, Config),
	    process_user_configs(NewOpts, lists:reverse(Acc1)++Acc);
	{value, {userconfig, {Callback, []}}, NewOpts} ->
	    process_user_configs(NewOpts, [{Callback, []} | Acc]);
	{value, {userconfig, {Callback, Files=[File|_]}}, NewOpts} when is_list(File) ->
		process_user_configs(NewOpts, [{Callback, Files} | Acc]);
	{value, {userconfig, {Callback, File=[C|_]}}, NewOpts} when is_integer(C) ->
		process_user_configs(NewOpts, [{Callback, [File]} | Acc])
    end.

get_config_file_list(Opts) ->
    DefaultConfigs = process_default_configs(Opts),
    CfgFiles =
	if
	    DefaultConfigs == []->
		[];
	    true->
		[{?ct_config_txt, DefaultConfigs}]
	end ++
	process_user_configs(Opts, []),
    CfgFiles.

read_config_files(Opts) ->
    AddCallback = fun(CallBack, []) ->
			[{CallBack, []}];
		     (CallBack, [F|_]=Files) when is_integer(F) ->
			[{CallBack, Files}];
		     (CallBack, [F|_]=Files) when is_list(F) ->
			lists:map(fun(X) -> {CallBack, X} end, Files)
		  end,
    ConfigFiles = case lists:keyfind(config, 1, Opts) of
	{config, ConfigLists}->
	    lists:foldr(fun({Callback,Files}, Acc) ->
				AddCallback(Callback,Files) ++ Acc
			end,
			[],
			ConfigLists);
	false->
	    []
    end,
    read_config_files_int(ConfigFiles, fun store_config/3).

read_config_files_int([{Callback, File}|Files], FunToSave) ->
    case Callback:read_config(File) of
	{ok, Config} ->
	    FunToSave(Config, Callback, File),
	    read_config_files_int(Files, FunToSave);
	{error, ErrorName, ErrorDetail}->
	    {user_error, {ErrorName, File, ErrorDetail}}
    end;
read_config_files_int([], _FunToSave) ->
    ok.

store_config(Config, Callback, File) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,
			 value=Val,
			 handler=Callback,
			 config=File,
			 ref=ct_util:ct_make_ref(),
			 default=false}) ||
	{Key,Val} <- Config].

keyfindall(Key, Pos, List) ->
    [E || E <- List, element(Pos, E) =:= Key].

rewrite_config(Config, Callback, File) ->
    OldRows = ets:match_object(?attr_table,
				#ct_conf{handler=Callback,
					 config=File,_='_'}),
    ets:match_delete(?attr_table,
		     #ct_conf{handler=Callback,
			      config=File,_='_'}),
    Updater = fun({Key, Value}) ->
	case keyfindall(Key, #ct_conf.key, OldRows) of
	    []->
		ets:insert(?attr_table,
			   #ct_conf{key=Key,
				    value=Value,
				    handler=Callback,
				    config=File,
				    ref=ct_util:ct_make_ref()});
	    RowsToUpdate ->
		Inserter = fun(Row) ->
				   ets:insert(?attr_table,
					      Row#ct_conf{value=Value,
							  ref=ct_util:ct_make_ref()})
			   end,
		lists:foreach(Inserter, RowsToUpdate)
	end
	      end,
    [Updater({Key, Value})||{Key, Value}<-Config].

set_config(Config,Default) ->
    set_config('_UNDEF',Config,Default).

set_config(Name,Config,Default) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,value=Val,ref=ct_util:ct_make_ref(),
			 name=Name,default=Default}) ||
	{Key,Val} <- Config].

get_config(KeyOrName) ->
    get_config(KeyOrName,undefined,[]).

get_config(KeyOrName,Default) ->
    get_config(KeyOrName,Default,[]).

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

lookup_handler_for_config({Key, _Subkey}) ->
    lookup_handler_for_config(Key);
lookup_handler_for_config(KeyOrName) ->
    case lookup_handler_for_name(KeyOrName) of
	[] ->
	    lookup_handler_for_key(KeyOrName);
	Values ->
	    Values
    end.

lookup_handler_for_name(Name) ->
    ets:select(?attr_table,[{#ct_conf{handler='$1',config='$2',name=Name,_='_'},
			     [],
			     [{{'$1','$2'}}]}]).

lookup_handler_for_key(Key) ->
    ets:select(?attr_table,[{#ct_conf{handler='$1',config='$2',key=Key,_='_'},
			     [],
			     [{{'$1','$2'}}]}]).


update_conf(Name, NewConfig) ->
    Old = ets:select(?attr_table,[{#ct_conf{name=Name,_='_'},[],['$_']}]),
    lists:foreach(fun(OldElem) ->
			  NewElem = OldElem#ct_conf{value=NewConfig},
			  ets:delete_object(?attr_table, OldElem),
			  ets:insert(?attr_table, NewElem)
		  end, Old),
    ok.

reload_conf(KeyOrName) ->
    case lookup_handler_for_config(KeyOrName) of
	[]->
	    undefined;
	HandlerList->
	    HandlerList2 = lists:usort(HandlerList),
	    read_config_files_int(HandlerList2, fun rewrite_config/3),
	    get_config(KeyOrName)
    end.

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

delete_config(Default) ->
    ets:match_delete(?attr_table,#ct_conf{default=Default,_='_'}),
    ok.

require(Key) when is_atom(Key) ->
    require({Key,[]});
require({Key,SubKeys}) when is_atom(Key) ->
    allocate('_UNDEF',Key,to_list(SubKeys));
require(Key) ->
    {error,{invalid,Key}}.

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

decrypt_config_file(EncryptFileName, TargetFileName) ->
    case get_crypt_key_from_file() of
	{error,_} = E ->
	    E;
	Key ->
	    decrypt_config_file(EncryptFileName, TargetFileName, {key,Key})
    end.

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

check_callback_load(Callback) ->
    case code:is_loaded(Callback) of
	{file, _Filename}->
	    check_exports(Callback);
	false->
	    case code:load_file(Callback) of
		{module, Callback}->
		    check_exports(Callback);
		{error, Error}->
		    {error, Error}
	    end
    end.

check_exports(Callback) ->
    Fs = Callback:module_info(exports),
    case {lists:member({check_parameter,1},Fs),
	  lists:member({read_config,1},Fs)} of
	{true, true} ->
	    {ok, Callback};
	_ ->
	    {error, missing_callback_functions}
    end.

check_config_files(Configs) ->
    ConfigChecker = fun
	({Callback, [F|_R]=Files}) ->
	    case check_callback_load(Callback) of
		{ok, Callback} ->
			if is_integer(F) ->
				Callback:check_parameter(Files);
			   is_list(F) ->
				lists:map(fun(File) ->
						  Callback:check_parameter(File)
					  end,
					  Files)
			end;
		{error, Why}->
		    {error, {callback, {Callback,Why}}}
	    end;
	({Callback, []}) ->
	    case check_callback_load(Callback) of
		{ok, Callback}->
		     Callback:check_parameter([]);
		{error, Why}->
		     {error, {callback, {Callback,Why}}}
	    end
    end,
    lists:keysearch(error, 1, lists:flatten(lists:map(ConfigChecker, Configs))).

prepare_user_configs([ConfigString|UserConfigs], Acc, new) ->
    prepare_user_configs(UserConfigs,
			 [{list_to_atom(ConfigString), []}|Acc],
			 cur);
prepare_user_configs(["and"|UserConfigs], Acc, _) ->
    prepare_user_configs(UserConfigs, Acc, new);
prepare_user_configs([ConfigString|UserConfigs], [{LastMod, LastList}|Acc], cur) ->
    prepare_user_configs(UserConfigs,
			 [{LastMod, [ConfigString|LastList]}|Acc],
			 cur);
prepare_user_configs([], Acc, _) ->
    Acc.

prepare_config_list(Args) ->
    ConfigFiles = case lists:keysearch(ct_config, 1, Args) of
		      {value,{ct_config,Files}}->
			  [{?ct_config_txt,[filename:absname(F) || F <- Files]}];
		      false->
			  []
		  end,
    UserConfigs = case lists:keysearch(userconfig, 1, Args) of
		      {value,{userconfig,UserConfigFiles}}->
			  prepare_user_configs(UserConfigFiles, [], new);
		      false->
			  []
		  end,
    ConfigFiles ++ UserConfigs.

% TODO: add logging of the loaded configuration file to the CT FW log!!!
add_config(Callback, []) ->
    read_config_files_int([{Callback, []}], fun store_config/3);
add_config(Callback, [File|_Files]=Config) when is_list(File) ->
    lists:foreach(fun(CfgStr) ->
	read_config_files_int([{Callback, CfgStr}], fun store_config/3) end,
	Config);
add_config(Callback, [C|_]=Config) when is_integer(C) ->
    read_config_files_int([{Callback, Config}], fun store_config/3),
    ok.

remove_config(Callback, Config) ->
    ets:match_delete(?attr_table,
		     #ct_conf{handler=Callback,
			      config=Config,_='_'}),
    ok.
