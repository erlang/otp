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
%%     $Id: mod_security.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
%%
-module(mod_security).

%% Security Audit Functionality

%% User API exports
-export([list_blocked_users/1, list_blocked_users/2, list_blocked_users/3,
	 block_user/4, block_user/5,
	 unblock_user/2, unblock_user/3, unblock_user/4,
	 list_auth_users/1, list_auth_users/2, list_auth_users/3]).

%% module API exports
-export([do/1, load/2, store/2, remove/1]).

-include("httpd.hrl").

-define(VMODULE,"SEC").
-include("httpd_verbosity.hrl").


%% do/1
do(Info) ->
    ?vdebug("~n   do with ~n   Info: ~p",[Info]),
    %% Check and see if any user has been authorized.
    case httpd_util:key1search(Info#mod.data,remote_user,not_defined_user) of
	not_defined_user ->
	    %% No user has been authorized.
	    case httpd_util:key1search(Info#mod.data, status) of
		%% A status code has been generated!
		{401, PhraseArgs, Reason} ->
		    case httpd_util:key1search(Info#mod.parsed_header,
					       "authorization") of
			undefined ->
			    %% Not an authorization attempt (server just replied to
			    %% challenge for authentication)
			    {proceed, Info#mod.data};
			[$B,$a,$s,$i,$c,$ |EncodedString] ->
			    %% Someone tried to authenticate, and obviously failed!
			    ?vlog("~n   Authentication failed: ~s",
				  [EncodedString]),
			    report_failed(Info, EncodedString,"Failed authentication"),
			    take_failed_action(Info, EncodedString),
			    {proceed, Info#mod.data}
		    end;
		_ ->
		    {proceed, Info#mod.data}
	    end;
	User ->
	    %% A user has been authenticated, now is he blocked ?
	    ?vtrace("user '~p' authentication",[User]),
	    Path = mod_alias:path(Info#mod.data,
				  Info#mod.config_db,
				  Info#mod.request_uri),
	    {Dir, SDirData} = secretp(Path, Info#mod.config_db),
	    Addr = httpd_util:lookup(Info#mod.config_db, bind_address),
	    Port = httpd_util:lookup(Info#mod.config_db, port),
	    DF   = httpd_util:key1search(SDirData, data_file),
	    case mod_security_server:check_blocked_user(Info, User,
							SDirData,
							Addr, Port) of
		true ->
		    ?vtrace("user blocked",[]),
		    report_failed(Info,httpd_util:decode_base64(User) ,"User Blocked"),
		    {proceed, [{status, {403, Info#mod.request_uri, ""}}|Info#mod.data]};
		false ->
		    ?vtrace("user not blocked",[]),
		    EncodedUser=httpd_util:decode_base64(User),
		    report_failed(Info, EncodedUser,"Authentication Succedded"),
		    mod_security_server:store_successful_auth(Addr, Port,
							      User, SDirData),
		    {proceed, Info#mod.data}
	    end
    end.



report_failed(Info, EncodedString,Event) ->
    Request = Info#mod.request_line,
    Decoded = httpd_util:decode_base64(EncodedString),
    {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
    String = RemoteHost++" : " ++ Event ++ " : "++Request++" : "++Decoded,
    mod_disk_log:security_log(Info,String),
    mod_log:security_log(Info, String).

take_failed_action(Info, EncodedString) ->
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db, Info#mod.request_uri),
    {Dir, SDirData} = secretp(Path, Info#mod.config_db),
    Addr = httpd_util:lookup(Info#mod.config_db, bind_address),
    Port = httpd_util:lookup(Info#mod.config_db, port),
    DecodedString = httpd_util:decode_base64(EncodedString),
    mod_security_server:store_failed_auth(Info, Addr, Port,
					  DecodedString, SDirData).

secretp(Path, ConfigDB) ->
    Directories = ets:match(ConfigDB,{directory,'$1','_'}),
    case secret_path(Path, Directories) of
	{yes, Directory} ->
	    SDirs0 = httpd_util:multi_lookup(ConfigDB, security_directory),
	    SDir = lists:filter(fun(X) ->
					lists:member({path, Directory}, X)
				end, SDirs0),
	    {Directory, lists:flatten(SDir)};
	no ->
	    error_report({internal_error_secretp, ?MODULE}),
	    {[], []}
    end.

secret_path(Path,Directories) ->
    secret_path(Path, httpd_util:uniq(lists:sort(Directories)), to_be_found).

secret_path(Path, [], to_be_found) ->
    no;
secret_path(Path, [], Directory) ->
    {yes, Directory};
secret_path(Path, [[NewDirectory]|Rest], Directory) ->
    case regexp:match(Path, NewDirectory) of
	{match, _, _} when Directory == to_be_found ->
	    secret_path(Path, Rest, NewDirectory);
	{match, _, Length} when Length > length(Directory)->
	    secret_path(Path, Rest, NewDirectory);
	{match, _, Length} ->
	    secret_path(Path, Rest, Directory);
	nomatch ->
	    secret_path(Path, Rest, Directory)
    end.


load([$<,$D,$i,$r,$e,$c,$t,$o,$r,$y,$ |Directory],[]) ->
    Dir = httpd_conf:custom_clean(Directory,"",">"),
    {ok, [{security_directory, Dir, [{path, Dir}]}]};
load(eof,[{security_directory,Directory, DirData}|_]) ->
    {error, ?NICE("Premature end-of-file in "++Directory)};
load([$S,$e,$c,$u,$r,$i,$t,$y,$D,$a,$t,$a,$F,$i,$l,$e,$ |FileName],
     [{security_directory, Dir, DirData}]) ->
    File = httpd_conf:clean(FileName),
    {ok, [{security_directory, Dir, [{data_file, File}|DirData]}]};
load([$S,$e,$c,$u,$r,$i,$t,$y,$C,$a,$l,$l,$b,$a,$c,$k,$M,$o,$d,$u,$l,$e,$ |ModuleName],
     [{security_directory, Dir, DirData}]) ->
    Mod = list_to_atom(httpd_conf:clean(ModuleName)),
    {ok, [{security_directory, Dir, [{callback_module, Mod}|DirData]}]};
load([$S,$e,$c,$u,$r,$i,$t,$y,$M,$a,$x,$R,$e,$t,$r,$i,$e,$s,$ |Retries],
     [{security_directory, Dir, DirData}]) ->
    MaxRetries = httpd_conf:clean(Retries),
    load_return_int_tag("SecurityMaxRetries", max_retries,
			httpd_conf:clean(Retries), Dir, DirData);
load([$S,$e,$c,$u,$r,$i,$t,$y,$B,$l,$o,$c,$k,$T,$i,$m,$e,$ |Time],
     [{security_directory, Dir, DirData}]) ->
    load_return_int_tag("SecurityBlockTime", block_time,
			httpd_conf:clean(Time), Dir, DirData);
load([$S,$e,$c,$u,$r,$i,$t,$y,$F,$a,$i,$l,$E,$x,$p,$i,$r,$e,$T,$i,$m,$e,$ |Time],
     [{security_directory, Dir, DirData}]) ->
    load_return_int_tag("SecurityFailExpireTime", fail_expire_time,
			httpd_conf:clean(Time), Dir, DirData);
load([$S,$e,$c,$u,$r,$i,$t,$y,$A,$u,$t,$h,$T,$i,$m,$e,$o,$u,$t,$ |Time0],
     [{security_directory, Dir, DirData}]) ->
    Time = httpd_conf:clean(Time0),
    load_return_int_tag("SecurityAuthTimeout", auth_timeout,
			httpd_conf:clean(Time), Dir, DirData);
load([$A,$u,$t,$h,$N,$a,$m,$e,$ |Name0],
     [{security_directory, Dir, DirData}]) ->
    Name = httpd_conf:clean(Name0),
    {ok, [{security_directory, Dir, [{auth_name, Name}|DirData]}]};
load("</Directory>",[{security_directory,Directory, DirData}]) ->
    {ok, [], {security_directory, Directory, DirData}}.

load_return_int_tag(Name, Atom, Time, Dir, DirData) ->
    case Time of
	"infinity" ->
	    {ok, [{security_directory, Dir, [{Atom, 99999999999999999999999999999}|DirData]}]};
	Int ->
	    case catch list_to_integer(Time) of
		{'EXIT', _} ->
		    {error, Time++" is an invalid "++Name};
		Val ->
		    {ok, [{security_directory, Dir, [{Atom, Val}|DirData]}]}
	    end
    end.

store({security_directory, Dir0, DirData}, ConfigList) ->
    ?CDEBUG("store(security_directory) -> ~n"
	    "      Dir0:       ~p~n"
	    "      DirData:    ~p",
	    [Dir0, DirData]),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Port = httpd_util:key1search(ConfigList, port),
    mod_security_server:start(Addr, Port),
    SR = httpd_util:key1search(ConfigList, server_root),
    Dir =
	case filename:pathtype(Dir0) of
	    relative ->
		filename:join(SR, Dir0);
	    _ ->
		Dir0
	end,
    case httpd_util:key1search(DirData, data_file, no_data_file) of
	no_data_file ->
	    {error, no_security_data_file};
	DataFile0 ->
	    DataFile =
		case filename:pathtype(DataFile0) of
		    relative ->
			filename:join(SR, DataFile0);
		    _ ->
			DataFile0
		end,
	    case mod_security_server:new_table(Addr, Port, DataFile) of
		{ok, TwoTables} ->
		    NewDirData0 = lists:keyreplace(data_file, 1, DirData,
						   {data_file, TwoTables}),
		    NewDirData1 = case Addr of
				      undefined ->
					  [{port,Port}|NewDirData0];
				      _ ->
					  [{port,Port},{bind_address,Addr}|
					   NewDirData0]
				  end,
		    {ok, {security_directory,NewDirData1}};
		{error, Err} ->
		    {error, {{open_data_file, DataFile}, Err}}
	    end
    end.


remove(ConfigDB) ->
    Addr = case ets:lookup(ConfigDB, bind_address) of
	       [] ->
		   undefined;
	       [{bind_address, Address}] ->
		   Address
	   end,
    [{port, Port}] = ets:lookup(ConfigDB, port),
    mod_security_server:delete_tables(Addr, Port),
    mod_security_server:stop(Addr, Port).


%%
%% User API
%%

%% list_blocked_users

list_blocked_users(Port) ->
    list_blocked_users(undefined, Port).

list_blocked_users(Port, Dir) when integer(Port) ->
    list_blocked_users(undefined,Port,Dir);
list_blocked_users(Addr, Port) when integer(Port) ->
    mod_security_server:list_blocked_users(Addr, Port).

list_blocked_users(Addr, Port, Dir) ->
    mod_security_server:list_blocked_users(Addr, Port, Dir).


%% block_user

block_user(User, Port, Dir, Time) ->
    block_user(User, undefined, Port, Dir, Time).
block_user(User, Addr, Port, Dir, Time) ->
    mod_security_server:block_user(User, Addr, Port, Dir, Time).


%% unblock_user

unblock_user(User, Port) ->
    unblock_user(User, undefined, Port).

unblock_user(User, Port, Dir) when integer(Port) ->
    unblock_user(User, undefined, Port, Dir);
unblock_user(User, Addr, Port) when integer(Port) ->
    mod_security_server:unblock_user(User, Addr, Port).

unblock_user(User, Addr, Port, Dir) ->
    mod_security_server:unblock_user(User, Addr, Port, Dir).


%% list_auth_users

list_auth_users(Port) ->
    list_auth_users(undefined,Port).

list_auth_users(Port, Dir) when integer(Port) ->
    list_auth_users(undefined, Port, Dir);
list_auth_users(Addr, Port) when integer(Port) ->
    mod_security_server:list_auth_users(Addr, Port).

list_auth_users(Addr, Port, Dir) ->
    mod_security_server:list_auth_users(Addr, Port, Dir).


error_report(M) ->
    error_logger:error_report(M).
