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
%%     $Id: httpd_conf.erl,v 1.1 2008/12/17 09:53:33 mikpe Exp $
%%
-module(httpd_conf).
-export([load/1, load_mime_types/1,
	 load/2, store/1, store/2,
	 remove_all/1, remove/1,
	 is_directory/1, is_file/1,
	 make_integer/1, clean/1, custom_clean/3, check_enum/2]).


-define(VMODULE,"CONF").
-include("httpd_verbosity.hrl").

%% The configuration data is handled in three (3) phases:
%% 1. Parse the config file and put all directives into a key-vale
%%    tuple list (load/1).
%% 2. Traverse the key-value tuple list store it into an ETS table.
%%    Directives depending on other directives are taken care of here
%%    (store/1).
%% 3. Traverse the ETS table and do a complete clean-up (remove/1).

-include("httpd.hrl").

%%
%% Phase 1: Load
%%

%% load

load(ConfigFile) ->
    ?CDEBUG("load -> ConfigFile: ~p",[ConfigFile]),
    case read_config_file(ConfigFile) of
	{ok, Config} ->
	    case bootstrap(Config) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Modules} ->
		    load_config(Config, lists:append(Modules, [?MODULE]))
	    end;
	{error, Reason} ->
	    {error, ?NICE("Error while reading config file: "++Reason)}
    end.


bootstrap([]) ->
    {error, ?NICE("Modules must be specified in the config file")};
bootstrap([Line|Config]) ->
    case Line of
	[$M,$o,$d,$u,$l,$e,$s,$ |Modules] ->
	    {ok, ModuleList} = regexp:split(Modules," "),
	    TheMods = [list_to_atom(X) || X <- ModuleList],
	    case verify_modules(TheMods) of
		ok ->
		    {ok, TheMods};
		{error, Reason} ->
		    ?ERROR("bootstrap -> : validation failed: ~p",[Reason]),
		    {error, Reason}
	    end;
	_ ->
	    bootstrap(Config)
    end.


%%
%% verify_modules/1 -> ok | {error, Reason}
%%
%% Verifies that all specified modules are available.
%%
verify_modules([]) ->
    ok;
verify_modules([Mod|Rest]) ->
    case code:which(Mod) of
	non_existing ->
	    {error, ?NICE(atom_to_list(Mod)++" does not exist")};
	Path ->
	    verify_modules(Rest)
    end.

%%
%% read_config_file/1 -> {ok, [line(), line()..]} | {error, Reason}
%%
%% Reads the entire configuration file and returns list of strings or
%% and error.
%%


read_config_file(FileName) ->
    case file:open(FileName, [read]) of
	{ok, Stream} ->
	    read_config_file(Stream, []);
	{error, Reason} ->
	    {error, ?NICE("Cannot open "++FileName)}
    end.

read_config_file(Stream, SoFar) ->
    case io:get_line(Stream, []) of
	eof ->
	    {ok, lists:reverse(SoFar)};
	{error, Reason} ->
	    {error, Reason};
	[$#|Rest] ->
	    %% Ignore commented lines for efficiency later ..
	    read_config_file(Stream, SoFar);
	Line ->
	    {ok, NewLine, _}=regexp:sub(clean(Line),"[\t\r\f ]"," "),
	    case NewLine of
		[] ->
		    %% Also ignore empty lines ..
		    read_config_file(Stream, SoFar);
		Other ->
		    read_config_file(Stream, [NewLine|SoFar])
	    end
    end.

is_exported(Module, ToFind) ->
    Exports = Module:module_info(exports),
    lists:member(ToFind, Exports).

%%
%% load/4 -> {ok, ConfigList} | {error, Reason}
%%
%% This loads the config file into each module specified by Modules
%% Each module has its own context that is passed to and (optionally)
%% returned by the modules load function. The module can also return
%% a ConfigEntry, which will be added to the global configuration
%% list.
%% All configuration directives are guaranteed to be passed to all
%% modules. Each module only implements the function clauses of
%% the load function for the configuration directives it supports,
%% it's ok if an apply returns {'EXIT', {function_clause, ..}}.
%%
load_config(Config, Modules) ->
    %% Create default contexts for all modules
    Contexts = lists:duplicate(length(Modules), []),
    load_config(Config, Modules, Contexts, []).


load_config([], _Modules, _Contexts, ConfigList) ->
    case a_must(ConfigList, [server_name,port,server_root,document_root]) of
	ok ->
	    {ok, ConfigList};
	{missing, Directive} ->
	    {error, ?NICE(atom_to_list(Directive)++
			  " must be specified in the config file")}
    end;

load_config([Line|Config], Modules, Contexts, ConfigList) ->
    ?CDEBUG("load_config -> Line: ~p",[Line]),
    case load_traverse(Line, Contexts, Modules, [], ConfigList, no) of
	{ok, NewContexts, NewConfigList} ->
	    load_config(Config, Modules, NewContexts, NewConfigList);
	{error, Reason} ->
	    ?ERROR("load_config -> traverse failed: ~p",[Reason]),
	    {error, Reason}
    end.


load_traverse(Line, [], [], NewContexts, ConfigList, no) ->
    ?CDEBUG("load_traverse/no -> ~n"
	    "     Line:        ~p~n"
	    "     NewContexts: ~p~n"
	    "     ConfigList:  ~p",
	    [Line,NewContexts,ConfigList]),
    {error, ?NICE("Configuration directive not recognized: "++Line)};
load_traverse(Line, [], [], NewContexts, ConfigList, yes) ->
    ?CDEBUG("load_traverse/yes -> ~n"
	    "     Line:        ~p~n"
	    "     NewContexts: ~p~n"
	    "     ConfigList:  ~p",
	    [Line,NewContexts,ConfigList]),
    {ok, lists:reverse(NewContexts), ConfigList};
load_traverse(Line, [Context|Contexts], [Module|Modules], NewContexts, ConfigList, State) ->
    ?CDEBUG("load_traverse/~p -> ~n"
	    "     Line:        ~p~n"
	    "     Module:      ~p~n"
	    "     Context:     ~p~n"
	    "     Contexts:    ~p~n"
	    "     NewContexts: ~p",
	    [State,Line,Module,Context,Contexts,NewContexts]),
    case is_exported(Module, {load, 2}) of
	true ->
	    ?CDEBUG("load_traverse -> ~p:load/2 exported",[Module]),
	    case catch apply(Module, load, [Line, Context]) of
		{'EXIT', {function_clause, _}} ->
		    ?CDEBUG("load_traverse -> exit: function_clause"
			    "~n   Module: ~p"
			    "~n   Line:   ~s",[Module,Line]),
		    load_traverse(Line, Contexts, Modules, [Context|NewContexts], ConfigList, State);
		{'EXIT', Reason} ->
		    ?CDEBUG("load_traverse -> exit: ~p",[Reason]),
		    error_logger:error_report({'EXIT', Reason}),
		    load_traverse(Line, Contexts, Modules, [Context|NewContexts], ConfigList, State);
		{ok, NewContext} ->
		    ?CDEBUG("load_traverse -> ~n"
			    "     NewContext: ~p",[NewContext]),
		    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts], ConfigList,yes);
		{ok, NewContext, ConfigEntry} when tuple(ConfigEntry) ->
		    ?CDEBUG("load_traverse (tuple) -> ~n"
			    "     NewContext:  ~p~n"
			    "     ConfigEntry: ~p",[NewContext,ConfigEntry]),
		    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
				  [ConfigEntry|ConfigList], yes);
		{ok, NewContext, ConfigEntry} when list(ConfigEntry) ->
		    ?CDEBUG("load_traverse (list) -> ~n"
			    "     NewContext:  ~p~n"
			    "     ConfigEntry: ~p",[NewContext,ConfigEntry]),
		    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
				  lists:append(ConfigEntry, ConfigList), yes);
		{error, Reason} ->
		    ?CDEBUG("load_traverse -> error: ~p",[Reason]),
		    {error, Reason}
	    end;
	false ->
	    ?CDEBUG("load_traverse -> ~p:load/2 not exported",[Module]),
	    load_traverse(Line, Contexts, Modules, [Context|NewContexts],
			  ConfigList,yes)
    end.


load(eof, []) ->
    eof;

load([$M,$a,$x,$H,$e,$a,$d,$e,$r,$S,$i,$z,$e,$ |MaxHeaderSize], []) ->
    ?DEBUG("load -> MaxHeaderSize: ~p",[MaxHeaderSize]),
    case make_integer(MaxHeaderSize) of
        {ok, Integer} ->
            {ok, [], {max_header_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxHeaderSize)++
                          " is an invalid number of MaxHeaderSize")}
    end;
load([$M,$a,$x,$H,$e,$a,$d,$e,$r,$A,$c,$t,$i,$o,$n,$ |Action], []) ->
    ?DEBUG("load -> MaxHeaderAction: ~p",[Action]),
    {ok, [], {max_header_action,list_to_atom(clean(Action))}};
load([$M,$a,$x,$B,$o,$d,$y,$S,$i,$z,$e,$ |MaxBodySize], []) ->
    ?DEBUG("load -> MaxBodySize: ~p",[MaxBodySize]),
    case make_integer(MaxBodySize) of
        {ok, Integer} ->
            {ok, [], {max_body_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxBodySize)++
                          " is an invalid number of MaxBodySize")}
    end;
load([$M,$a,$x,$B,$o,$d,$y,$A,$c,$t,$i,$o,$n,$ |Action], []) ->
    ?DEBUG("load -> MaxBodyAction: ~p",[Action]),
    {ok, [], {max_body_action,list_to_atom(clean(Action))}};
load([$S,$e,$r,$v,$e,$r,$N,$a,$m,$e,$ |ServerName], []) ->
    ?DEBUG("load -> ServerName: ~p",[ServerName]),
    {ok,[],{server_name,clean(ServerName)}};
load([$S,$o,$c,$k,$e,$t,$T,$y,$p,$e,$ |SocketType], []) ->
    ?DEBUG("load -> SocketType: ~p",[SocketType]),
    case check_enum(clean(SocketType),["ssl","ip_comm"]) of
	{ok, ValidSocketType} ->
	    {ok, [], {com_type,ValidSocketType}};
	{error,_} ->
	    {error, ?NICE(clean(SocketType) ++ " is an invalid SocketType")}
    end;
load([$P,$o,$r,$t,$ |Port], []) ->
    ?DEBUG("load -> Port: ~p",[Port]),
    case make_integer(Port) of
	{ok, Integer} ->
	    {ok, [], {port,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(Port)++" is an invalid Port")}
    end;
load([$B,$i,$n,$d,$A,$d,$d,$r,$e,$s,$s,$ |Address], []) ->
    ?DEBUG("load -> Address:  ~p",[Address]),
    case clean(Address) of
	"*" ->
	    {ok, [], {bind_address,any}};
	CAddress ->
	    ?CDEBUG("load -> CAddress:  ~p",[CAddress]),
	    case inet:getaddr(CAddress,inet) of
		{ok, IPAddr} ->
		    ?CDEBUG("load -> IPAddr:  ~p",[IPAddr]),
		    {ok, [], {bind_address,IPAddr}};
		{error, _} ->
		    {error, ?NICE(CAddress++" is an invalid address")}
	    end
    end;
load([$K,$e,$e,$p,$A,$l,$i,$v,$e,$ |OnorOff], []) ->
    case list_to_atom(clean(OnorOff)) of
	off ->
	    {ok, [], {persistent_conn, false}};
	_ ->
	    {ok, [], {persistent_conn, true}}
    end;
load([$M,$a,$x,$K,$e,$e,$p,$A,$l,$i,$v,$e,$R,$e,$q,$u,$e,$s,$t,$ |MaxRequests], []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests)++" is an invalid MaxKeepAliveRequest")}
    end;
load([$K,$e,$e,$p,$A,$l,$i,$v,$e,$T,$i,$m,$e,$o,$u,$t,$ |Timeout], []) ->
    case make_integer(Timeout) of
	{ok, Integer} ->
	    {ok, [], {keep_alive_timeout, Integer*1000}};
	{error, _} ->
	    {error, ?NICE(clean(Timeout)++" is an invalid KeepAliveTimeout")}
    end;
load([$M,$o,$d,$u,$l,$e,$s,$ |Modules], []) ->
    {ok, ModuleList} = regexp:split(Modules," "),
    {ok, [], {modules,[list_to_atom(X) || X <- ModuleList]}};
load([$S,$e,$r,$v,$e,$r,$A,$d,$m,$i,$n,$ |ServerAdmin], []) ->
    {ok, [], {server_admin,clean(ServerAdmin)}};
load([$S,$e,$r,$v,$e,$r,$R,$o,$o,$t,$ |ServerRoot], []) ->
    case is_directory(clean(ServerRoot)) of
	{ok, Directory} ->
	    MimeTypesFile =
		filename:join([clean(ServerRoot),"conf", "mime.types"]),
	    case load_mime_types(MimeTypesFile) of
		{ok, MimeTypesList} ->
		    {ok, [], [{server_root,string:strip(Directory,right,$/)},
			      {mime_types,MimeTypesList}]};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, _} ->
	    {error, ?NICE(clean(ServerRoot)++" is an invalid ServerRoot")}
    end;
load([$M,$a,$x,$C,$l,$i,$e,$n,$t,$s,$ |MaxClients], []) ->
    ?DEBUG("load -> MaxClients: ~p",[MaxClients]),
    case make_integer(MaxClients) of
	{ok, Integer} ->
	    {ok, [], {max_clients,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxClients)++" is an invalid number of MaxClients")}
    end;
load([$D,$o,$c,$u,$m,$e,$n,$t,$R,$o,$o,$t,$ |DocumentRoot],[]) ->
    case is_directory(clean(DocumentRoot)) of
	{ok, Directory} ->
	    {ok, [], {document_root,string:strip(Directory,right,$/)}};
	{error, _} ->
	    {error, ?NICE(clean(DocumentRoot)++"is an invalid DocumentRoot")}
    end;
load([$D,$e,$f,$a,$u,$l,$t,$T,$y,$p,$e,$ |DefaultType], []) ->
    {ok, [], {default_type,clean(DefaultType)}};
load([$S,$S,$L,$C,$e,$r,$t,$i,$f,$i,$c,$a,$t,$e,$F,$i,$l,$e,$ | SSLCertificateFile], []) ->
    ?DEBUG("load -> SSLCertificateFile: ~p",[SSLCertificateFile]),
    case is_file(clean(SSLCertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_file,File}};
    {error, _} ->
	    {error, ?NICE(clean(SSLCertificateFile)++
			  " is an invalid SSLCertificateFile")}
    end;
load([$S,$S,$L,$C,$e,$r,$t,$i,$f,$i,$c,$a,$t,$e,$K,$e,$y,$F,$i,$l,$e,$ |
      SSLCertificateKeyFile], []) ->
    ?DEBUG("load -> SSLCertificateKeyFile: ~p",[SSLCertificateKeyFile]),
    case is_file(clean(SSLCertificateKeyFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_key_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCertificateKeyFile)++
			  " is an invalid SSLCertificateKeyFile")}
    end;
load([$S,$S,$L,$V,$e,$r,$i,$f,$y,$C,$l,$i,$e,$n,$t,$ |SSLVerifyClient], []) ->
    ?DEBUG("load -> SSLVerifyClient: ~p",[SSLVerifyClient]),
    case make_integer(clean(SSLVerifyClient)) of
	{ok, Integer} when Integer >=0,Integer =< 2 ->
	    {ok, [], {ssl_verify_client,Integer}};
	{ok, Integer} ->
	    {error,?NICE(clean(SSLVerifyClient)++" is an invalid SSLVerifyClient")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyClient)++" is an invalid SSLVerifyClient")}
    end;
load([$S,$S,$L,$V,$e,$r,$i,$f,$y,$D,$e,$p,$t,$h,$ |
      SSLVerifyDepth], []) ->
    ?DEBUG("load -> SSLVerifyDepth: ~p",[SSLVerifyDepth]),
    case make_integer(clean(SSLVerifyDepth)) of
	{ok, Integer} when Integer > 0 ->
	    {ok, [], {ssl_verify_client_depth,Integer}};
	{ok, Integer} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")}
    end;
load([$S,$S,$L,$C,$i,$p,$h,$e,$r,$s,$ | SSLCiphers], []) ->
    ?DEBUG("load -> SSLCiphers: ~p",[SSLCiphers]),
    {ok, [], {ssl_ciphers, clean(SSLCiphers)}};
load([$S,$S,$L,$C,$A,$C,$e,$r,$t,$i,$f,$i,$c,$a,$t,$e,$F,$i,$l,$e,$ |
      SSLCACertificateFile], []) ->
    case is_file(clean(SSLCACertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_ca_certificate_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCACertificateFile)++
			  " is an invalid SSLCACertificateFile")}
    end;
load([$S,$S,$L,$P,$a,$s,$s,$w,$o,$r,$d,$C,$a,$l,$l,$b,$a,$c,$k,$M,$o,$d,$u,$l,$e,$ | SSLPasswordCallbackModule], []) ->
    ?DEBUG("load -> SSLPasswordCallbackModule: ~p",
	   [SSLPasswordCallbackModule]),
    {ok, [], {ssl_password_callback_module,
	      list_to_atom(clean(SSLPasswordCallbackModule))}};
load([$S,$S,$L,$P,$a,$s,$s,$w,$o,$r,$d,$C,$a,$l,$l,$b,$a,$c,$k,$F,$u,$n,$c,$t,$i,$o,$n,$ | SSLPasswordCallbackFunction], []) ->
    ?DEBUG("load -> SSLPasswordCallbackFunction: ~p",
	   [SSLPasswordCallbackFunction]),
    {ok, [], {ssl_password_callback_function,
	      list_to_atom(clean(SSLPasswordCallbackFunction))}}.


%%
%% load_mime_types/1 -> {ok, MimeTypes} | {error, Reason}
%%
load_mime_types(MimeTypesFile) ->
    case file:open(MimeTypesFile, [read]) of
	{ok, Stream} ->
	    parse_mime_types(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open " ++ MimeTypesFile)}
    end.

parse_mime_types(Stream,MimeTypesList) ->
    Line=
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		clean(String)
	end,
    parse_mime_types(Stream, MimeTypesList, Line).

parse_mime_types(Stream, MimeTypesList, eof) ->
    file:close(Stream),
    {ok, MimeTypesList};
parse_mime_types(Stream, MimeTypesList, "") ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, [$#|_]) ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, Line) ->
    case regexp:split(Line, " ") of
	{ok, [NewMimeType|Suffixes]} ->
	    parse_mime_types(Stream,lists:append(suffixes(NewMimeType,Suffixes),
						 MimeTypesList));
	{ok, _} ->
	    {error, ?NICE(Line)}
    end.

suffixes(MimeType,[]) ->
    [];
suffixes(MimeType,[Suffix|Rest]) ->
    [{Suffix,MimeType}|suffixes(MimeType,Rest)].

%%
%% Phase 2: Store
%%

%% store

store(ConfigList) ->
    Modules = httpd_util:key1search(ConfigList, modules, []),
    Port = httpd_util:key1search(ConfigList, port),
    Addr = httpd_util:key1search(ConfigList,bind_address),
    Name = httpd_util:make_name("httpd_conf",Addr,Port),
    ?CDEBUG("store -> Name = ~p",[Name]),
    ConfigDB = ets:new(Name, [named_table, bag, protected]),
    ?CDEBUG("store -> ConfigDB = ~p",[ConfigDB]),
    store(ConfigDB, ConfigList, lists:append(Modules,[?MODULE]),ConfigList).

store(ConfigDB, ConfigList, Modules,[]) ->
    ?vtrace("store -> done",[]),
    ?CDEBUG("store -> done",[]),
    {ok, ConfigDB};
store(ConfigDB, ConfigList, Modules, [ConfigListEntry|Rest]) ->
    ?vtrace("store -> entry with"
	    "~n   ConfigListEntry: ~p",[ConfigListEntry]),
    ?CDEBUG("store -> "
	"~n   ConfigListEntry: ~p",[ConfigListEntry]),
    case store_traverse(ConfigListEntry,ConfigList,Modules) of
	{ok, ConfigDBEntry} when tuple(ConfigDBEntry) ->
	    ?vtrace("store -> ConfigDBEntry(tuple): "
		    "~n   ~p",[ConfigDBEntry]),
	    ?CDEBUG("store -> ConfigDBEntry(tuple): "
		    "~n   ~p",[ConfigDBEntry]),
	    ets:insert(ConfigDB,ConfigDBEntry),
	    store(ConfigDB,ConfigList,Modules,Rest);
	{ok, ConfigDBEntry} when list(ConfigDBEntry) ->
	    ?vtrace("store -> ConfigDBEntry(list): "
		    "~n   ~p",[ConfigDBEntry]),
	    ?CDEBUG("store -> ConfigDBEntry(list): "
		"~n   ~p",[ConfigDBEntry]),
	    lists:foreach(fun(Entry) ->
				  ets:insert(ConfigDB,Entry)
			  end,ConfigDBEntry),
	    store(ConfigDB,ConfigList,Modules,Rest);
	{error, Reason} ->
	    ?vlog("store -> error: ~p",[Reason]),
	    ?ERROR("store -> error: ~p",[Reason]),
	    {error,Reason}
    end.

store_traverse(ConfigListEntry,ConfigList,[]) ->
    {error,?NICE("Unable to store configuration...")};
store_traverse(ConfigListEntry, ConfigList, [Module|Rest]) ->
    case is_exported(Module, {store, 2}) of
	true ->
	    ?CDEBUG("store_traverse -> call ~p:store/2",[Module]),
	    case catch apply(Module,store,[ConfigListEntry, ConfigList]) of
		{'EXIT',{function_clause,_}} ->
		    ?CDEBUG("store_traverse -> exit: function_clause",[]),
		    store_traverse(ConfigListEntry,ConfigList,Rest);
		{'EXIT',Reason} ->
		    ?ERROR("store_traverse -> exit: ~p",[Reason]),
		    error_logger:error_report({'EXIT',Reason}),
		    store_traverse(ConfigListEntry,ConfigList,Rest);
		Result ->
		    ?CDEBUG("store_traverse -> ~n"
			    "      Result: ~p",[Result]),
		    Result
	    end;
	false ->
	    store_traverse(ConfigListEntry,ConfigList,Rest)
    end.

store({mime_types,MimeTypesList},ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Name = httpd_util:make_name("httpd_mime",Addr,Port),
    ?CDEBUG("store(mime_types) -> Name: ~p",[Name]),
    {ok, MimeTypesDB} = store_mime_types(Name,MimeTypesList),
    ?CDEBUG("store(mime_types) -> ~n"
	    "     MimeTypesDB:      ~p~n"
	    "     MimeTypesDB info: ~p",
	    [MimeTypesDB,ets:info(MimeTypesDB)]),
    {ok, {mime_types,MimeTypesDB}};
store(ConfigListEntry,ConfigList) ->
    ?CDEBUG("store/2 -> ~n"
	    "        ConfigListEntry: ~p~n"
	    "        ConfigList:      ~p",
	    [ConfigListEntry,ConfigList]),
    {ok, ConfigListEntry}.


%% store_mime_types
store_mime_types(Name,MimeTypesList) ->
    ?CDEBUG("store_mime_types -> Name: ~p",[Name]),
    MimeTypesDB = ets:new(Name, [set, protected]),
    ?CDEBUG("store_mime_types -> MimeTypesDB: ~p",[MimeTypesDB]),
    store_mime_types1(MimeTypesDB, MimeTypesList).

store_mime_types1(MimeTypesDB,[]) ->
    {ok, MimeTypesDB};
store_mime_types1(MimeTypesDB,[Type|Rest]) ->
    ?CDEBUG("store_mime_types1 -> Type: ~p",[Type]),
    ets:insert(MimeTypesDB, Type),
    store_mime_types1(MimeTypesDB, Rest).


%%
%% Phase 3: Remove
%%

remove_all(ConfigDB) ->
    Modules = httpd_util:lookup(ConfigDB,modules,[]),
    remove_traverse(ConfigDB, lists:append(Modules,[?MODULE])).

remove_traverse(ConfigDB,[]) ->
    ?vtrace("remove_traverse -> done", []),
    ok;
remove_traverse(ConfigDB,[Module|Rest]) ->
    ?vtrace("remove_traverse -> call ~p:remove", [Module]),
    case (catch apply(Module,remove,[ConfigDB])) of
	{'EXIT',{undef,_}} ->
	    ?vtrace("remove_traverse -> undef", []),
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',{function_clause,_}} ->
	    ?vtrace("remove_traverse -> function_clause", []),
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',Reason} ->
	    ?vtrace("remove_traverse -> exit: ~p", [Reason]),
	    error_logger:error_report({'EXIT',Reason}),
	    remove_traverse(ConfigDB,Rest);
	{error,Reason} ->
	    ?vtrace("remove_traverse -> error: ~p", [Reason]),
	    error_logger:error_report(Reason),
	    remove_traverse(ConfigDB,Rest);
	_ ->
	    remove_traverse(ConfigDB,Rest)
    end.

remove(ConfigDB) ->
    ets:delete(ConfigDB),
    ok.


%%
%% Utility functions
%%

%% is_directory

is_directory(Directory) ->
    case file:read_file_info(Directory) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_directory(Type,Access,FileInfo,Directory);
	{error,Reason} ->
	    {error,Reason}
    end.

is_directory(directory,read,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(directory,read_write,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(_Type,_Access,FileInfo,_Directory) ->
    {error,FileInfo}.


%% is_file

is_file(File) ->
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_file(Type,Access,FileInfo,File);
	{error,Reason} ->
	    {error,Reason}
    end.

is_file(regular,read,_FileInfo,File) ->
    {ok,File};
is_file(regular,read_write,_FileInfo,File) ->
    {ok,File};
is_file(_Type,_Access,FileInfo,_File) ->
    {error,FileInfo}.

%% make_integer

make_integer(String) ->
    case regexp:match(clean(String),"[0-9]+") of
	{match, _, _} ->
	    {ok, list_to_integer(clean(String))};
	nomatch ->
	    {error, nomatch}
    end.


%% clean

clean(String) ->
    {ok,CleanedString,_} = regexp:gsub(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$",""),
    CleanedString.

%% custom_clean

custom_clean(String,MoreBefore,MoreAfter) ->
    {ok,CleanedString,_}=regexp:gsub(String,"^[ \t\n\r\f"++MoreBefore++
				     "]*|[ \t\n\r\f"++MoreAfter++"]*\$",""),
    CleanedString.

%% check_enum

check_enum(Enum,[]) ->
    {error, not_valid};
check_enum(Enum,[Enum|Rest]) ->
    {ok, list_to_atom(Enum)};
check_enum(Enum, [NotValid|Rest]) ->
    check_enum(Enum, Rest).

%% a_must

a_must(ConfigList,[]) ->
    ok;
a_must(ConfigList,[Directive|Rest]) ->
    case httpd_util:key1search(ConfigList,Directive) of
	undefined ->
	    {missing,Directive};
	_ ->
	    a_must(ConfigList,Rest)
    end.
