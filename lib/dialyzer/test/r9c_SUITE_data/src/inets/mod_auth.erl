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
%%     $Id: mod_auth.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
-module(mod_auth).


%% The functions that the webbserver call on startup stop
%% and when the server traverse the modules.
-export([do/1, load/2, store/2, remove/1]).

%% User entries to the gen-server.
-export([add_user/2, add_user/5, add_user/6,
	 add_group_member/3, add_group_member/4, add_group_member/5,
	 list_users/1, list_users/2, list_users/3,
	 delete_user/2, delete_user/3, delete_user/4,
	 delete_group_member/3, delete_group_member/4, delete_group_member/5,
	 list_groups/1, list_groups/2, list_groups/3,
	 delete_group/2, delete_group/3, delete_group/4,
	 get_user/2, get_user/3, get_user/4,
	 list_group_members/2, list_group_members/3, list_group_members/4,
	 update_password/6, update_password/5]).

-include("httpd.hrl").
-include("mod_auth.hrl").

-define(VMODULE,"AUTH").
-include("httpd_verbosity.hrl").

-define(NOPASSWORD,"NoPassword").


%% do
do(Info) ->
    ?vtrace("do", []),
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	    {proceed, Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data,response) of
		%% No response has been generated!
		undefined ->
		    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
					  Info#mod.request_uri),
		    %% Is it a secret area?
		    case secretp(Path,Info#mod.config_db) of
			{yes, Directory, DirectoryData} ->
			    %% Authenticate (allow)
			    case allow((Info#mod.init_data)#init_data.peername,
				       Info#mod.socket_type,Info#mod.socket,
				       DirectoryData) of
				allowed ->
				    case deny((Info#mod.init_data)#init_data.peername,
					      Info#mod.socket_type, Info#mod.socket,
					      DirectoryData) of
					not_denied ->
					    case httpd_util:key1search(DirectoryData,
								       auth_type) of
						undefined ->
						    {proceed, Info#mod.data};
						none ->
						    {proceed, Info#mod.data};
						AuthType ->
						    do_auth(Info,
							    Directory,
							    DirectoryData,
							    AuthType)
					    end;
					{denied, Reason} ->
					    {proceed,
					     [{status,{403,Info#mod.request_uri,Reason}}|
					      Info#mod.data]}
				    end;
				{not_allowed, Reason} ->
				    {proceed,[{status,{403,Info#mod.request_uri,Reason}}|
					      Info#mod.data]}
			    end;
			no ->
			    {proceed, Info#mod.data}
		    end;
		%% A response has been generated or sent!
		Response ->
		    {proceed, Info#mod.data}
	    end
    end.


do_auth(Info, Directory, DirectoryData, AuthType) ->
    %% Authenticate (require)
    case require(Info, Directory, DirectoryData) of
	authorized ->
	    {proceed,Info#mod.data};
	{authorized, User} ->
	    {proceed, [{remote_user,User}|Info#mod.data]};
	{authorization_failed, Reason} ->
	    ?vtrace("do_auth -> authorization_failed: ~p",[Reason]),
	    {proceed, [{status,{401,none,Reason}}|Info#mod.data]};
	{authorization_required, Realm} ->
	    ?vtrace("do_auth -> authorization_required: ~p",[Realm]),
	    ReasonPhrase = httpd_util:reason_phrase(401),
	    Message = httpd_util:message(401,none,Info#mod.config_db),
	    {proceed,
	     [{response,
	       {401,
		["WWW-Authenticate: Basic realm=\"",Realm,
		 "\"\r\n\r\n","<HTML>\n<HEAD>\n<TITLE>",
		 ReasonPhrase,"</TITLE>\n",
		 "</HEAD>\n<BODY>\n<H1>",ReasonPhrase,
		 "</H1>\n",Message,"\n</BODY>\n</HTML>\n"]}}|
	      Info#mod.data]};
	{status, {StatusCode,PhraseArgs,Reason}} ->
	    {proceed, [{status,{StatusCode,PhraseArgs,Reason}}|
		       Info#mod.data]}
    end.


%% require

require(Info, Directory, DirectoryData) ->
    ParsedHeader = Info#mod.parsed_header,
    ValidUsers   = httpd_util:key1search(DirectoryData, require_user),
    ValidGroups  = httpd_util:key1search(DirectoryData, require_group),

    %% Any user or group restrictions?
    case ValidGroups of
	undefined when ValidUsers == undefined ->
	    authorized;
	_ ->
	    case httpd_util:key1search(ParsedHeader, "authorization") of
		%% Authorization required!
		undefined ->
		    case httpd_util:key1search(DirectoryData, auth_name) of
			undefined ->
			    {status,{500,none,?NICE("AuthName directive not specified")}};
			Realm ->
			    {authorization_required, Realm}
		    end;
		%% Check credentials!
		[$B,$a,$s,$i,$c,$ | EncodedString] ->
		    DecodedString = httpd_util:decode_base64(EncodedString),
		    case a_valid_user(Info, DecodedString,
				      ValidUsers, ValidGroups,
				      Directory, DirectoryData) of
			{yes, User} ->
			    {authorized, User};
			{no, Reason} ->
			    {authorization_failed, Reason};
			{status, {StatusCode,PhraseArgs,Reason}} ->
			    {status,{StatusCode,PhraseArgs,Reason}}
		    end;
		%% Bad credentials!
		BadCredentials ->
		    {status,{401,none,?NICE("Bad credentials "++BadCredentials)}}
	    end
    end.

a_valid_user(Info,DecodedString,ValidUsers,ValidGroups,Dir,DirData) ->
    case httpd_util:split(DecodedString,":",2) of
	{ok,[SupposedUser, Password]} ->
	    case user_accepted(SupposedUser, ValidUsers) of
		true ->
		    check_password(SupposedUser, Password, Dir, DirData);
		false ->
		    case group_accepted(Info,SupposedUser,ValidGroups,Dir,DirData) of
			true ->
			    check_password(SupposedUser,Password,Dir,DirData);
			false ->
			    {no,?NICE("No such user exists")}
		    end
	    end;
	{ok,BadCredentials} ->
	    {status,{401,none,?NICE("Bad credentials "++BadCredentials)}}
    end.

user_accepted(SupposedUser, undefined) ->
    false;
user_accepted(SupposedUser, ValidUsers) ->
    lists:member(SupposedUser, ValidUsers).


group_accepted(Info, User, undefined, Dir, DirData) ->
    false;
group_accepted(Info, User, [], Dir, DirData) ->
    false;
group_accepted(Info, User, [Group|Rest], Dir, DirData) ->
    Ret = int_list_group_members(Group, Dir, DirData),
    case Ret of
	{ok, UserList} ->
	    case lists:member(User, UserList) of
		true ->
		    true;
		false ->
		    group_accepted(Info, User, Rest, Dir, DirData)
	    end;
	Other ->
	    false
    end.

check_password(User, Password, Dir, DirData) ->
    case int_get_user(DirData, User) of
	{ok, UStruct} ->
	    case UStruct#httpd_user.password of
		Password ->
		    %% FIXME
		    {yes, UStruct#httpd_user.username};
		Other ->
		    {no, "No such user"}   % Don't say 'Bad Password' !!!
	    end;
	_ ->
	    {no, "No such user"}
    end.


%% Middle API. Theese functions call the appropriate authentication module.
int_get_user(DirData, User) ->
    AuthMod = auth_mod_name(DirData),
    apply(AuthMod, get_user, [DirData, User]).

int_list_group_members(Group, Dir, DirData) ->
    AuthMod = auth_mod_name(DirData),
    apply(AuthMod, list_group_members, [DirData, Group]).

auth_mod_name(DirData) ->
    case httpd_util:key1search(DirData, auth_type, plain) of
	plain ->    mod_auth_plain;
	mnesia ->   mod_auth_mnesia;
	dets ->	    mod_auth_dets
    end.


%%
%% Is it a secret area?
%%

%% secretp

secretp(Path,ConfigDB) ->
    Directories = ets:match(ConfigDB,{directory,'$1','_'}),
    case secret_path(Path, Directories) of
	{yes,Directory} ->
	    {yes,Directory,
	     lists:flatten(ets:match(ConfigDB,{directory,Directory,'$1'}))};
	no ->
	    no
    end.

secret_path(Path,Directories) ->
    secret_path(Path, httpd_util:uniq(lists:sort(Directories)),to_be_found).

secret_path(Path,[],to_be_found) ->
    no;
secret_path(Path,[],Directory) ->
    {yes,Directory};
secret_path(Path,[[NewDirectory]|Rest],Directory) ->
    case regexp:match(Path,NewDirectory) of
	{match,_,_} when Directory == to_be_found ->
	    secret_path(Path,Rest,NewDirectory);
	{match,_,Length} when Length > length(Directory)->
	    secret_path(Path,Rest,NewDirectory);
	{match,_,Length} ->
	    secret_path(Path,Rest,Directory);
	nomatch ->
	    secret_path(Path,Rest,Directory)
    end.

%%
%% Authenticate
%%

%% allow

allow({_,RemoteAddr},SocketType,Socket,DirectoryData) ->
    Hosts = httpd_util:key1search(DirectoryData, allow_from, all),
    case validate_addr(RemoteAddr,Hosts) of
	true ->
	    allowed;
	false ->
	    {not_allowed, ?NICE("Connection from your host is not allowed")}
    end.

validate_addr(RemoteAddr,all) ->            % When called from 'allow'
    true;
validate_addr(RemoteAddr,none) ->           % When called from 'deny'
    false;
validate_addr(RemoteAddr,[]) ->
    false;
validate_addr(RemoteAddr,[HostRegExp|Rest]) ->
    ?DEBUG("validate_addr -> RemoteAddr: ~p HostRegExp: ~p",
	   [RemoteAddr, HostRegExp]),
    case regexp:match(RemoteAddr, HostRegExp) of
	{match,_,_} ->
	    true;
	nomatch ->
	    validate_addr(RemoteAddr,Rest)
    end.

%% deny

deny({_,RemoteAddr},SocketType,Socket,DirectoryData) ->
    ?DEBUG("deny -> RemoteAddr: ~p",[RemoteAddr]),
    Hosts = httpd_util:key1search(DirectoryData, deny_from, none),
    ?DEBUG("deny -> Hosts: ~p",[Hosts]),
    case validate_addr(RemoteAddr,Hosts) of
	true ->
	    {denied, ?NICE("Connection from your host is not allowed")};
	false ->
	    not_denied
    end.

%%
%% Configuration
%%

%% load/2
%%

%% mod_auth recognizes the following Configuration Directives:
%% <Directory /path/to/directory>
%%  AuthDBType
%%  AuthName
%%  AuthUserFile
%%  AuthGroupFile
%%  AuthAccessPassword
%%  require
%%  allow
%% </Directory>

%% When a <Directory> directive is found, a new context is set to
%% [{directory, Directory, DirData}|OtherContext]
%% DirData in this case is a key-value list of data belonging to the
%% directory in question.
%%
%% When the </Directory> statement is found, the Context created earlier
%% will be returned as a ConfigList and the context will return to the
%% state it was previously.

load([$<,$D,$i,$r,$e,$c,$t,$o,$r,$y,$ |Directory],[]) ->
    Dir = httpd_conf:custom_clean(Directory,"",">"),
    {ok,[{directory, Dir, [{path, Dir}]}]};
load(eof,[{directory,Directory, DirData}|_]) ->
    {error, ?NICE("Premature end-of-file in "++Directory)};

load([$A,$u,$t,$h,$N,$a,$m,$e,$ |AuthName], [{directory,Directory, DirData}|Rest]) ->
    {ok, [{directory,Directory,
	   [ {auth_name, httpd_conf:clean(AuthName)}|DirData]} | Rest ]};

load([$A,$u,$t,$h,$U,$s,$e,$r,$F,$i,$l,$e,$ |AuthUserFile0],
     [{directory, Directory, DirData}|Rest]) ->
    AuthUserFile = httpd_conf:clean(AuthUserFile0),
    {ok,[{directory,Directory,
	  [ {auth_user_file, AuthUserFile}|DirData]} | Rest ]};

load([$A,$u,$t,$h,$G,$r,$o,$u,$p,$F,$i,$l,$e,$ |AuthGroupFile0],
	 [{directory,Directory, DirData}|Rest]) ->
    AuthGroupFile = httpd_conf:clean(AuthGroupFile0),
    {ok,[{directory,Directory,
	  [ {auth_group_file, AuthGroupFile}|DirData]} | Rest]};

%AuthAccessPassword
load([$A,$u,$t,$h,$A,$c,$c,$e,$s,$s,$P,$a,$s,$s,$w,$o,$r,$d,$ |AuthAccessPassword0],
	 [{directory,Directory, DirData}|Rest]) ->
    AuthAccessPassword = httpd_conf:clean(AuthAccessPassword0),
    {ok,[{directory,Directory,
	  [{auth_access_password, AuthAccessPassword}|DirData]} | Rest]};




load([$A,$u,$t,$h,$D,$B,$T,$y,$p,$e,$ |Type],
	 [{directory, Dir, DirData}|Rest]) ->
    case httpd_conf:clean(Type) of
	"plain" ->
	    {ok, [{directory, Dir, [{auth_type, plain}|DirData]} | Rest ]};
	"mnesia" ->
	    {ok, [{directory, Dir, [{auth_type, mnesia}|DirData]} | Rest ]};
	"dets" ->
	    {ok, [{directory, Dir, [{auth_type, dets}|DirData]} | Rest ]};
	_ ->
	    {error, ?NICE(httpd_conf:clean(Type)++" is an invalid AuthDBType")}
    end;

load([$r,$e,$q,$u,$i,$r,$e,$ |Require],[{directory,Directory, DirData}|Rest]) ->
    case regexp:split(Require," ") of
	{ok,["user"|Users]} ->
	    {ok,[{directory,Directory,
		  [{require_user,Users}|DirData]} | Rest]};
	{ok,["group"|Groups]} ->
	    {ok,[{directory,Directory,
		  [{require_group,Groups}|DirData]} | Rest]};
	{ok,_} ->
	    {error,?NICE(httpd_conf:clean(Require)++" is an invalid require")}
    end;

load([$a,$l,$l,$o,$w,$ |Allow],[{directory,Directory, DirData}|Rest]) ->
    case regexp:split(Allow," ") of
	{ok,["from","all"]} ->
	    {ok,[{directory,Directory,
		  [{allow_from,all}|DirData]} | Rest]};
	{ok,["from"|Hosts]} ->
	    {ok,[{directory,Directory,
		  [{allow_from,Hosts}|DirData]} | Rest]};
	{ok,_} ->
	    {error,?NICE(httpd_conf:clean(Allow)++" is an invalid allow")}
    end;

load([$d,$e,$n,$y,$ |Deny],[{directory,Directory, DirData}|Rest]) ->
    case regexp:split(Deny," ") of
	{ok, ["from", "all"]} ->
	    {ok,[{directory, Directory,
		  [{deny_from, all}|DirData]} | Rest]};
	{ok, ["from"|Hosts]} ->
	    {ok,[{directory, Directory,
		  [{deny_from, Hosts}|DirData]} | Rest]};
	{ok, _} ->
	    {error,?NICE(httpd_conf:clean(Deny)++" is an invalid deny")}
    end;

load("</Directory>",[{directory,Directory, DirData}|Rest]) ->
    {ok, Rest, {directory, Directory, DirData}};

load([$A,$u,$t,$h,$M,$n,$e,$s,$i,$a,$D,$B,$ |AuthMnesiaDB],
      [{directory, Dir, DirData}|Rest]) ->
    case httpd_conf:clean(AuthMnesiaDB) of
	"On" ->
	    {ok,[{directory,Dir,[{auth_type,mnesia}|DirData]}|Rest]};
	"Off" ->
	    {ok,[{directory,Dir,[{auth_type,plain}|DirData]}|Rest]};
	_ ->
	    {error, ?NICE(httpd_conf:clean(AuthMnesiaDB)++" is an invalid AuthMnesiaDB")}
    end.

%% store

store({directory,Directory0, DirData0}, ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port),
    DirData = case httpd_util:key1search(ConfigList, bind_address) of
		  undefined ->
		      [{port, Port}|DirData0];
		  Addr ->
		      [{port, Port},{bind_address,Addr}|DirData0]
	      end,
    Directory =
	case filename:pathtype(Directory0) of
	    relative ->
		SR = httpd_util:key1search(ConfigList, server_root),
		filename:join(SR, Directory0);
	    _ ->
		Directory0
	end,
    AuthMod =
	case httpd_util:key1search(DirData0, auth_type) of
	    mnesia -> mod_auth_mnesia;
	    dets ->   mod_auth_dets;
	    plain ->  mod_auth_plain;
	    _ ->      no_module_at_all
	end,
    case AuthMod of
	no_module_at_all ->
	    {ok, {directory, Directory, DirData}};
	_ ->
	    %% Control that there are a password or add a standard password:
	    %% "NoPassword"
	    %% In this way a user must select to use a noPassword
	    Pwd = case httpd_util:key1search(DirData,auth_access_password)of
		      undefined->
			  ?NOPASSWORD;
		      PassW->
			  PassW
		  end,
	    DirDataLast = lists:keydelete(auth_access_password,1,DirData),
	    case catch AuthMod:store_directory_data(Directory, DirDataLast) of
		ok ->
		    add_auth_password(Directory,Pwd,ConfigList),
		    {ok, {directory, Directory, DirDataLast}};
		{ok, NewDirData} ->
		    add_auth_password(Directory,Pwd,ConfigList),
		    {ok, {directory, Directory, NewDirData}};
		{error, Reason} ->
		    {error, Reason};
		Other ->
		    ?ERROR("unexpected result: ~p",[Other]),
		    {error, Other}
	    end
    end.


add_auth_password(Dir, Pwd0, ConfigList) ->
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Port = httpd_util:key1search(ConfigList, port),
    mod_auth_server:start(Addr, Port),
    mod_auth_server:add_password(Addr, Port, Dir, Pwd0).

%% remove


remove(ConfigDB) ->
    lists:foreach(fun({directory, Dir, DirData}) ->
			  AuthMod = auth_mod_name(DirData),
			  (catch apply(AuthMod, remove, [DirData]))
		  end,
		  ets:match_object(ConfigDB,{directory,'_','_'})),
    Addr = case lookup(ConfigDB, bind_address) of
	       [] ->
		   undefined;
	       [{bind_address, Address}] ->
		   Address
	   end,
    [{port, Port}] = lookup(ConfigDB, port),
    mod_auth_server:stop(Addr, Port),
    ok.




%% --------------------------------------------------------------------

%% update_password

update_password(Port, Dir, Old, New, New)->
    update_password(undefined, Port, Dir, Old, New, New).

update_password(Addr, Port, Dir, Old, New, New) when list(New) ->
    mod_auth_server:update_password(Addr, Port, Dir, Old, New);

update_password(_Addr, _Port, _Dir, _Old, New, New) ->
    {error, badtype};
update_password(_Addr, _Port, _Dir, _Old, New, New1) ->
    {error, notqeual}.


%% add_user

add_user(UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd}->
	    case get_options(Opt, userData) of
		{error, Reason}->
		    {error, Reason};
		{UserData, Password}->
		    User = [#httpd_user{username  = UserName,
					password  = Password,
					user_data = UserData}],
		    mod_auth_server:add_user(Addr, Port, Dir, User, AuthPwd);
		{error, Reason} ->
		    {error, Reason}
	    end
    end.


add_user(UserName, Password, UserData, Port, Dir) ->
    add_user(UserName, Password, UserData, undefined, Port, Dir).
add_user(UserName, Password, UserData, Addr, Port, Dir) ->
    User = [#httpd_user{username  = UserName,
			password  = Password,
			user_data = UserData}],
    mod_auth_server:add_user(Addr, Port, Dir, User, ?NOPASSWORD).


%% get_user

get_user(UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:get_user(Addr, Port, Dir, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

get_user(UserName, Port, Dir) ->
    get_user(UserName, undefined, Port, Dir).
get_user(UserName, Addr, Port, Dir) ->
    mod_auth_server:get_user(Addr, Port, Dir, UserName, ?NOPASSWORD).


%% add_group_member

add_group_member(GroupName, UserName, Opt)->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd}->
	    mod_auth_server:add_group_member(Addr, Port, Dir,
					     GroupName, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

add_group_member(GroupName, UserName, Port, Dir) ->
    add_group_member(GroupName, UserName, undefined, Port, Dir).

add_group_member(GroupName, UserName, Addr, Port, Dir) ->
    mod_auth_server:add_group_member(Addr, Port, Dir,
				     GroupName, UserName, ?NOPASSWORD).


%% delete_group_member

delete_group_member(GroupName, UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:delete_group_member(Addr, Port, Dir,
						GroupName, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

delete_group_member(GroupName, UserName, Port, Dir) ->
    delete_group_member(GroupName, UserName, undefined, Port, Dir).
delete_group_member(GroupName, UserName, Addr, Port, Dir) ->
    mod_auth_server:delete_group_member(Addr, Port, Dir,
					GroupName, UserName, ?NOPASSWORD).


%% list_users

list_users(Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:list_users(Addr, Port, Dir, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

list_users(Port, Dir) ->
    list_users(undefined, Port, Dir).
list_users(Addr, Port, Dir) ->
    mod_auth_server:list_users(Addr, Port, Dir, ?NOPASSWORD).


%% delete_user

delete_user(UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:delete_user(Addr, Port, Dir, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

delete_user(UserName, Port, Dir) ->
    delete_user(UserName, undefined, Port, Dir).
delete_user(UserName, Addr, Port, Dir) ->
    mod_auth_server:delete_user(Addr, Port, Dir, UserName, ?NOPASSWORD).


%% delete_group

delete_group(GroupName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd}->
	    mod_auth_server:delete_group(Addr, Port, Dir, GroupName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

delete_group(GroupName, Port, Dir) ->
    delete_group(GroupName, undefined, Port, Dir).
delete_group(GroupName, Addr, Port, Dir) ->
    mod_auth_server:delete_group(Addr, Port, Dir, GroupName, ?NOPASSWORD).


%% list_groups

list_groups(Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd}->
	    mod_auth_server:list_groups(Addr, Port, Dir, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

list_groups(Port, Dir) ->
    list_groups(undefined, Port, Dir).
list_groups(Addr, Port, Dir) ->
    mod_auth_server:list_groups(Addr, Port, Dir, ?NOPASSWORD).


%% list_group_members

list_group_members(GroupName,Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:list_group_members(Addr, Port, Dir, GroupName,
					       AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

list_group_members(GroupName, Port, Dir) ->
    list_group_members(GroupName, undefined, Port, Dir).
list_group_members(GroupName, Addr, Port, Dir) ->
    mod_auth_server:list_group_members(Addr, Port, Dir, GroupName, ?NOPASSWORD).



%% Opt = [{port, Port},
%%        {addr, Addr},
%%        {dir,  Dir},
%%        {authPassword, AuthPassword} | FunctionSpecificData]
get_options(Opt, mandatory)->
    case httpd_util:key1search(Opt, port, undefined) of
	Port when integer(Port) ->
	    case httpd_util:key1search(Opt, dir, undefined) of
		Dir when list(Dir) ->
		    Addr = httpd_util:key1search(Opt,
						 addr,
						 undefined),
		    AuthPwd = httpd_util:key1search(Opt,
						    authPassword,
						    ?NOPASSWORD),
		    {Addr, Port, Dir, AuthPwd};
		_->
		    {error, bad_dir}
	    end;
	_ ->
	    {error, bad_dir}
    end;

%% FunctionSpecificData = {userData, UserData} | {password, Password}
get_options(Opt, userData)->
    case httpd_util:key1search(Opt, userData, undefined) of
	undefined ->
	    {error, no_userdata};
	UserData ->
	    case httpd_util:key1search(Opt, password, undefined) of
		undefined->
		    {error, no_password};
		Pwd ->
		    {UserData, Pwd}
	    end
    end.


lookup(Db, Key) ->
    ets:lookup(Db, Key).
