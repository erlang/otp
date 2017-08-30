%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-export([get_key_from_name/1]).

-export([check_config_files/1, add_default_callback/1, prepare_config_list/1]).

-export([add_config/2, remove_config/2]).

-include("ct_util.hrl").

-define(cryptfile, ".ct_config.crypt").

-record(ct_conf,{key,value,handler,config,name='_UNDEF',default=false}).

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
    To ! {Ref, Result},
    ok.

loop(StartDir) ->
    receive
	{{require,Name,Key},From} ->
	    Result = do_require(Name,Key),
	    return(From,Result),
	    loop(StartDir);
	{{set_default_config,{Config,Scope}},From} ->
	    _ = set_config(Config,{true,Scope}),
	    return(From,ok),
	    loop(StartDir);
	{{set_default_config,{Name,Config,Scope}},From} ->
	    _ = set_config(Name,Config,{true,Scope}),
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
	    ok = file:set_cwd(StartDir),
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
    lists:flatmap(fun({config,[_|_] = FileOrFiles}) ->
			  case {io_lib:printable_list(FileOrFiles),
				io_lib:printable_list(hd(FileOrFiles))} of
			      {false,true} ->
				  FileOrFiles;
			      {true,false} ->
				  [FileOrFiles];
			      _ ->
				  []
			  end;
		     (_) ->
			  []
		  end,Opts).

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
	    DefaultConfigs == [] ->
		[];
	    true ->
		[{?ct_config_txt, DefaultConfigs}]
	end ++
	process_user_configs(Opts, []),
    CfgFiles.

add_default_callback(Opts) ->
    case lists:keytake(config, 1, Opts) of
	{value, {config, [File | _] = Files}, NoConfigOpts}
	  when is_integer(File) =/= true ->
	    [{config, lists:flatmap(fun add_def_cb/1, Files)} | NoConfigOpts];
	{value, {config, File}, NoConfigOpts} ->
	    [{config, add_def_cb(File)} | NoConfigOpts];
	false ->
	    Opts
    end.

add_def_cb([]) ->
    [];
add_def_cb(Config) when is_tuple(Config) ->
    [Config];
add_def_cb([H|_T] = Config ) when is_integer(H) ->
    [{?ct_config_txt, [Config]}].

read_config_files(Opts) ->
    AddCallback = fun(CallBack, []) ->
			[{CallBack, []}];
		     (CallBack, [F|_]=Files) when is_integer(F) ->
			[{CallBack, Files}];
		     (CallBack, [F|_]=Files) when is_list(F) ->
			lists:map(fun(X) -> {CallBack, X} end, Files)
		  end,

    ConfigFiles = case lists:keyfind(config, 1, Opts) of
		      {config,ConfigLists} ->
                          lists:foldr(fun({Callback,Files}, Acc) ->
					      AddCallback(Callback,Files)
						  ++ Acc
				      end,[],ConfigLists);
		      false ->
			  []
		  end,
    read_config_files_int(ConfigFiles, fun store_config/3).

read_config_files_int([{Callback, File}|Files], FunToSave) ->
    case Callback:read_config(File) of
	{ok, Config} ->
	    _ = FunToSave(Config, Callback, File),
	    read_config_files_int(Files, FunToSave);
	{error, {ErrorName, ErrorDetail}} ->
	    {user_error, {ErrorName, File, ErrorDetail}};
	{error, ErrorName, ErrorDetail} ->
	    {user_error, {ErrorName, File, ErrorDetail}}
    end;
read_config_files_int([], _FunToSave) ->
    ok.


read_config_files(ConfigFiles, FunToSave) ->
    case read_config_files_int(ConfigFiles, FunToSave) of
        {user_error, Error} ->
            {error, Error};
        ok ->
            ok
    end.

store_config(Config, Callback, File) when is_tuple(Config) ->
    store_config([Config], Callback, File);

store_config(Config, Callback, File) when is_list(Config) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,
			 value=Val,
			 handler=Callback,
			 config=File,
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
	    [] ->
		ets:insert(?attr_table,
			   #ct_conf{key=Key,
				    value=Value,
				    handler=Callback,
				    config=File});
	    RowsToUpdate ->
		Inserter = fun(Row) ->
				   ets:insert(?attr_table,
					      Row#ct_conf{value=Value})
			   end,
		lists:foreach(Inserter, RowsToUpdate)
	end
	      end,
    [Updater({Key, Value})||{Key, Value}<-Config].

set_config(Config,Default) ->
    set_config('_UNDEF',Config,Default).

set_config(Name,Config,Default) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,value=Val,
			 name=Name,default=Default}) ||
	{Key,Val} <- Config].

get_config(KeyOrName) ->
    get_config(KeyOrName,undefined,[]).

get_config(KeyOrName,Default) ->
    get_config(KeyOrName,Default,[]).

get_config(KeyOrName,Default,Opts) when is_atom(KeyOrName) ->
    case get_config({KeyOrName}, Default, Opts) of
	%% If only an atom is given, then we need to unwrap the
	%% key if it is returned
	{{KeyOrName}, Val} ->
	    {KeyOrName, Val};
	[{{KeyOrName}, _Val}|_] = Res ->
	    [{K, Val} || {{K},Val} <- Res, K == KeyOrName];
	Else ->
	    Else
    end;

%% This useage of get_config is only used by internal ct functions
%% and may change at any time
get_config({DeepKey,SubKey}, Default, Opts) when is_tuple(DeepKey) ->
    get_config(erlang:append_element(DeepKey, SubKey), Default, Opts);
get_config(KeyOrName,Default,Opts) when is_tuple(KeyOrName) ->
    case lookup_config(element(1,KeyOrName)) of
	[] ->
	    format_value([Default],KeyOrName,Opts);
	Vals ->
	    NewVals =
		lists:map(
		  fun({Val}) ->
			  get_config(tl(tuple_to_list(KeyOrName)),
				     Val,Default,Opts)
		  end,Vals),
	    format_value(NewVals,KeyOrName,Opts)
    end.

get_config([],Vals,_Default,_Opts) ->
    Vals;
get_config([[]],Vals,Default,Opts) ->
    get_config([],Vals,Default,Opts);
%% This case is used by {require,{unix,[port,host]}} functionality
get_config([SubKeys], Vals, Default, _Opts) when is_list(SubKeys) ->
    case do_get_config(SubKeys, Vals, []) of
	{ok, SubVals} ->
	    [SubVal || {_,SubVal} <- SubVals];

	_ ->
	    Default
    end;
get_config([Key|Rest], Vals, Default, Opts) ->
    case do_get_config([Key], Vals, []) of
	{ok, [{Key,NewVals}]} ->
	    get_config(Rest, NewVals, Default, Opts);
	_ ->
	    Default
    end.

do_get_config([Key|_], Available, _Mapped) when not is_list(Available) ->
    {error,{not_available,Key}};
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
			     [],[{{'$1','$2','$3','$4'}}]}]).

lookup_config(KeyOrName) ->
    case lookup_name(KeyOrName) of
	[] ->
	    lookup_key(KeyOrName);
	Values ->
	    Values
    end.

lookup_name(Name) ->
    ets:select(?attr_table,[{#ct_conf{value='$1',name=Name,_='_'},
			     [],[{{'$1'}}]}]).
lookup_key(Key) ->
    ets:select(?attr_table,[{#ct_conf{key=Key,value='$1',name='_UNDEF',_='_'},
			     [],[{{'$1'}}]}]).

format_value([SubVal|_] = SubVals, KeyOrName, Opts) ->
    case {lists:member(all,Opts),lists:member(element,Opts)} of
	{true,true} ->
	    [{KeyOrName,Val} || Val <- SubVals];
	{true,false} ->
	    [Val || Val <- SubVals];
	{false,true} ->
	    {KeyOrName,SubVal};
	{false,false} ->
	    SubVal
    end.

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
	[] ->
	    undefined;
	HandlerList ->
	    HandlerList2 = lists:usort(HandlerList),
	    case read_config_files(HandlerList2, fun rewrite_config/3) of
		ok ->
		    get_config(KeyOrName);
		Error ->
		    Error
	    end
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

allocate(Name,Key) ->
    Ref = make_ref(),
    case get_config(Key,Ref,[all,element]) of
	[{_,Ref}] ->
	    {error,{not_available,Key}};
	Configs ->
	    associate(Name,Key,Configs),
	    ok
    end.


associate('_UNDEF',_Key,_Configs) ->
    ok;
associate(Name,{Key,SubKeys},Configs) when is_atom(Key), is_list(SubKeys) ->
    associate_int(Name,Configs,"true");
associate(Name,_Key,Configs) ->
    associate_int(Name,Configs,os:getenv("COMMON_TEST_ALIAS_TOP")).

associate_int(Name,Configs,"true") ->
    lists:foreach(fun({K,_Config}) ->
		      Cs = ets:match_object(
			     ?attr_table,
			     #ct_conf{key=element(1,K),
				      name='_UNDEF',_='_'}),
		      [ets:insert(?attr_table,C#ct_conf{name=Name})
		       || C <- Cs]
		  end,Configs);
associate_int(Name,Configs,_) ->
    lists:foreach(fun({K,Config}) ->
		      Key = if is_tuple(K) -> element(1,K);
			       is_atom(K) -> K
			    end,

		      Cs = ets:match_object(
			     ?attr_table,
			     #ct_conf{key=Key,
				      name='_UNDEF',_='_'}),
		      [ets:insert(?attr_table,C#ct_conf{name=Name,
							value=Config})
		       || C <- Cs]
		  end,Configs).



delete_config(Default) ->
    ets:match_delete(?attr_table,#ct_conf{default=Default,_='_'}),
    ok.

require(Key) when is_atom(Key); is_tuple(Key) ->
    allocate('_UNDEF',Key);
require(Key) ->
    {error,{invalid,Key}}.

require(Name,Key) when is_atom(Name),is_atom(Key) orelse is_tuple(Key) ->
    call({require,Name,Key});
require(Name,Keys) ->
    {error,{invalid,{Name,Keys}}}.

do_require(Name,Key) ->
    case get_key_from_name(Name) of
	{error,_} ->
	    allocate(Name,Key);
	{ok,NameKey} when NameKey == Key; 
			  is_tuple(Key) andalso element(1,Key) == NameKey ->
	    %% already allocated - check that it has all required subkeys
	    R = make_ref(),
	    case get_config(Key,R,[]) of
		R ->
		    {error,{not_available,Key}};
		{error,_} = Error ->
		    Error;
		_Error ->
		    ok
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
    _ = crypto:start(),
    {Key,IVec} = make_crypto_key(Key),
    case file:read_file(SrcFileName) of
	{ok,Bin0} ->
	    Bin1 = term_to_binary({SrcFileName,Bin0}),
	    Bin2 = case byte_size(Bin1) rem 8 of
		       0 -> Bin1;
		       N -> list_to_binary([Bin1,random_bytes(8-N)])
		   end,
	    EncBin = crypto:block_encrypt(des3_cbc, Key, IVec, Bin2),
	    case file:write_file(EncryptFileName, EncBin) of
		ok ->
		    io:format("~ts --(encrypt)--> ~ts~n",
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
    _ = crypto:start(),
    {Key,IVec} = make_crypto_key(Key),
    case file:read_file(EncryptFileName) of
	{ok,Bin} ->
	    DecBin = crypto:block_decrypt(des3_cbc, Key, IVec, Bin),
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
				    io:format("~ts --(decrypt)--> ~ts~n",
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
		    io:format("~nCrypt key file: ~ts~n", [FullName]),
		    Key;
		_ ->
		    {error,{bad_crypt_file,FullName}}
	    end
    end.

make_crypto_key(String) ->
    <<K1:8/binary,K2:8/binary>> = First = erlang:md5(String),
    <<K3:8/binary,IVec:8/binary>> = erlang:md5([First|lists:reverse(String)]),
    {[K1,K2,K3],IVec}.

random_bytes(N) ->
    random_bytes_1(N, []).

random_bytes_1(0, Acc) -> Acc;
random_bytes_1(N, Acc) -> random_bytes_1(N-1, [rand:uniform(255)|Acc]).

check_callback_load(Callback) ->
    case code:is_loaded(Callback) of
	{file, _Filename} ->
	    check_exports(Callback);
	false ->
	    case code:load_file(Callback) of
		{module, Callback} ->
		    check_exports(Callback);
		{error, Error} ->
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
		{error, Why} ->
		    {error, {callback, {Callback,Why}}}
	    end;
	({Callback, []}) ->
	    case check_callback_load(Callback) of
		{ok, Callback} ->
		     Callback:check_parameter([]);
		{error, Why} ->
		     {error, {callback, {Callback,Why}}}
	    end
    end,
    lists:keysearch(error, 1, lists:flatten(lists:map(ConfigChecker, Configs))).

prepare_user_configs([CallbackMod|UserConfigs], Acc, new) ->
    prepare_user_configs(UserConfigs,
			 [{list_to_atom(CallbackMod),[]}|Acc],
			 cur);
prepare_user_configs(["and"|UserConfigs], Acc, _) ->
    prepare_user_configs(UserConfigs, Acc, new);
prepare_user_configs([ConfigString|UserConfigs], [{LastMod,LastList}|Acc], cur) ->
    prepare_user_configs(UserConfigs,
			 [{LastMod, [ConfigString|LastList]}|Acc],
			 cur);
prepare_user_configs([], Acc, _) ->
    Acc.

prepare_config_list(Args) ->
    ConfigFiles = case lists:keysearch(ct_config, 1, Args) of
		      {value,{ct_config,Files}} ->
			  [{?ct_config_txt,[filename:absname(F) || F <- Files]}];
		      false ->
			  []
		  end,
    UserConfigs = case lists:keysearch(userconfig, 1, Args) of
		      {value,{userconfig,UserConfigFiles}} ->
			  prepare_user_configs(UserConfigFiles, [], new);
		      false ->
			  []
		  end,
    ConfigFiles ++ UserConfigs.

% TODO: add logging of the loaded configuration file to the CT FW log!!!
add_config(Callback, []) ->
    read_config_files([{Callback, []}], fun store_config/3);
add_config(Callback, [File|_Files]=Config) when is_list(File) ->
    lists:foreach(fun(CfgStr) ->
	read_config_files([{Callback, CfgStr}], fun store_config/3) end,
	Config);
add_config(Callback, [C|_]=Config) when is_integer(C) ->
    read_config_files([{Callback, Config}], fun store_config/3).

remove_config(Callback, Config) ->
    ets:match_delete(?attr_table,
		     #ct_conf{handler=Callback,
			      config=Config,_='_'}),
    ok.
