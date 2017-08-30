%%-----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%
%%
%%----------------------------------------------------------------------
%% File    : fileTransfer_SUITE.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(fileTransfer_SUITE).

%%--------------- INCLUDES -----------------------------------
-include_lib("cosFileTransfer/src/cosFileTransferApp.hrl").

-include_lib("common_test/include/ct.hrl").

%%--------------- DEFINES ------------------------------------
-define(default_timeout, test_server:minutes(20)).
-define(match(ExpectedRes, Expr),
        fun() ->
               AcTuAlReS = (catch (Expr)),
               case AcTuAlReS of
                   ExpectedRes ->
                       io:format("------ CORRECT RESULT ------~n~p~n",
                                 [AcTuAlReS]),
                       AcTuAlReS;
                   _ ->
                       io:format("###### ERROR ERROR ######~n~p~n",
                                 [AcTuAlReS]),
                       exit(AcTuAlReS)
               end
       end()).
 
-define(matchnopr(ExpectedRes, Expr),
        fun() ->
               AcTuAlReS = (catch (Expr)),
               case AcTuAlReS of
                   ExpectedRes ->
                       io:format("------ CORRECT RESULT (~p) ------~n", [?LINE]),
                       AcTuAlReS;
                   _ ->
                       io:format("###### ERROR ERROR ######~n~p~n",
                                 [AcTuAlReS]),
                       exit(AcTuAlReS)
               end
       end()).
 



 
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0,suite/0,groups/0,
	 init_per_group/2,end_per_group/2, 
	 cases/0, 
	 init_per_suite/1, 
	 end_per_suite/1, 
	 fileIterator_api/1,
	 fts_ftp_file_api/1, 
	 fts_ftp_file_ssl_api/1, 
	 fts_ftp_dir_api/1, 
	 fts_native_file_api/1, 
	 fts_native_file_ssl_api/1, 
	 fts_native_dir_api/1, 
	 init_per_testcase/2, 
	 end_per_testcase/2,
	 install_data/2,
	 uninstall_data/1,
	 slave_sup/0,
	 app_test/1]).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].



init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [fts_ftp_dir_api, fts_ftp_file_api,
     fts_ftp_file_ssl_api, fts_native_dir_api,
     fts_native_file_api, fts_native_file_ssl_api,
     fileIterator_api, app_test].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    case crypto_works() of
	false ->
	    {skip,"Could not start crypto!"};
	true ->
	    orber:jump_start(),
	    cosProperty:install(),
	    cosProperty:start(),
	    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
	    %% Client
	    cosFileTransferApp:configure(ssl_client_certfile,
					 filename:join([Dir, "client", "cert.pem"])),
	    cosFileTransferApp:configure(ssl_client_cacertfile,
					 filename:join([Dir, "client", "cacerts.pem"])),
	    cosFileTransferApp:configure(ssl_client_verify, 1),
	    cosFileTransferApp:configure(ssl_client_depth, 0),
	    %% Server
	    cosFileTransferApp:configure(ssl_server_certfile,
					 filename:join([Dir, "server", "cert.pem"])),
	    cosFileTransferApp:configure(ssl_server_cacertfile,
					 filename:join([Dir, "server", "cacerts.pem"])),
	    cosFileTransferApp:configure(ssl_server_verify, 1),
	    cosFileTransferApp:configure(ssl_server_depth, 0),
	    crypto:start(),
	    ssl:start(),
	    cosFileTransferApp:install(),
	    cosFileTransferApp:start(),
	    if
		is_list(Config) ->
		    Config;
		true ->
		    exit("Config not a list")
	    end
    end.

crypto_works() ->
    try crypto:start() of
	{error,{already_started,crypto}} -> true;
	ok -> true
    catch
	error:_ ->
	    false
    end.

end_per_suite(Config) ->
    ssl:stop(),
    crypto:stop(),
    cosFileTransferApp:stop(),
    cosProperty:stop(),
    cosProperty:uninstall(),
    cosFileTransferApp:uninstall(),
    orber:jump_stop(),
    Config.

%%-----------------------------------------------------------------
%%  Local definitions
%%-----------------------------------------------------------------
-define(FTP_USER, "anonymous").
-define(FTP_PASS, "fileTransfer_SUITE@localhost").
-define(TEST_DIR,["/", "incoming"]).

-define(FTP_PORT, 21).
-define(FTP_ACC,  "anonymous").
 
-define(BAD_HOST, "badhostname").
-define(BAD_USER, "baduser").
-define(BAD_DIR,  "baddirectory").

-define(TEST_FILE_DATA,   "If this file exists after a completed test an error occurred.").   
-define(TEST_FILE_DATA2,  "1234567890123").

%%-----------------------------------------------------------------
%%  aoo-file test
%%-----------------------------------------------------------------
app_test(_Config) ->
    ok=?t:app_test(cosFileTransfer),
    ok.

%%-----------------------------------------------------------------
%%  FileIterator API tests 
%%-----------------------------------------------------------------
fileIterator_api(Config) ->
    case ftp_host(Config) of
	{skipped, SkippedReason} ->
	    {skipped, SkippedReason};
	Host ->
	    
	    {ok, Node} = create_node("fileIterator_api", 4008, normal),
	    ?match(ok, remote_apply(Node, ?MODULE, install_data, 
					  [tcp, {{'NATIVE', 
						  'cosFileTransferNATIVE_file'}, Host, 
						 "fileIterator_api"}])),
	    
            %% Create a Virtual File System.
%%    VFS = ?match({_,_,_,_,_,_},
%%		 cosFileTransferApp:create_VFS({'NATIVE', 
%%						'cosFileTransferNATIVE_file'}, 
%%					       [], Host, ?FTP_PORT)),
	    VFS = ?matchnopr({'IOP_IOR',"IDL:omg.org/CosFileTransfer/VirtualFileSystem:1.0",_}, 
				   corba:string_to_object("corbaname::1.2@localhost:4008/NameService#fileIterator_api")),

            %% Start two File Transfer Sessions (Source and Target).
	    {FS, Dir} = ?matchnopr({{_,_,_},{_,_,_}},
			'CosFileTransfer_VirtualFileSystem':login(VFS, 
								  ?FTP_USER, 
								  ?FTP_PASS, 
								  ?FTP_ACC)),

            %% Do some basic test on one of the Directories attributes.
	    ?match([_H|_], 'CosFileTransfer_Directory':'_get_name'(Dir)),
	    ?match([_H|_], 'CosFileTransfer_Directory':'_get_complete_file_name'(Dir)),
	    ?match({'IOP_IOR',[],[]}, 'CosFileTransfer_Directory':'_get_parent'(Dir)),
	    ?matchnopr(FS, 'CosFileTransfer_Directory':'_get_associated_session'(Dir)),
	    {ok,[],FileIter} = ?match({ok,[],_}, 'CosFileTransfer_Directory':list(Dir, 0)),
            %% Usually the working directory for the test is not empty so no need for
            %% creating files of our own?!
	    #any{value=Children} = ?match({any, _, _},
					  'CosPropertyService_PropertySet':
					  get_property_value(Dir, "num_children")),

	    if
		Children > 5 ->
		    ?matchnopr({true, _}, 'CosFileTransfer_FileIterator':next_one(FileIter)),
		    ?matchnopr({true, _}, 'CosFileTransfer_FileIterator':next_n(FileIter, 3)),
		    ?matchnopr({true, _}, 'CosFileTransfer_FileIterator':next_n(FileIter, 
										      Children)),
		    ?matchnopr({false, _}, 'CosFileTransfer_FileIterator':next_one(FileIter)),
		    ?match({false, []}, 'CosFileTransfer_FileIterator':next_n(FileIter, 1)),
		    ok;
		true ->
	    ok
	    end,
	    ?match(ok, 'CosFileTransfer_FileIterator':destroy(FileIter)),
	    ?match(false, corba_object:non_existent(FS)),
	    ?match(ok, 'CosFileTransfer_FileTransferSession':logout(FS)),
            %% To make sure Orber can remove it from mnesia.
	    timer:sleep(1000),
	    ?match(true, corba_object:non_existent(FS)),
	    ?match(ok, remote_apply(Node, ?MODULE, uninstall_data, ["fileIterator_api"])),
	    stop_orber_remote(Node, normal),
	    ok
    end.
    

%%-----------------------------------------------------------------
%%  FileTransferSession API tests 
%%-----------------------------------------------------------------
fts_ftp_file_api(Config) ->
    {ok, Node} = create_node("ftp_file_api", 4004, normal),
    file_helper(Config, 'FTP', ?TEST_DIR, Node, 4004, "ftp_file_api", tcp).

fts_ftp_file_ssl_api(Config) ->
    {ok, Node} = create_node("ftp_file_api_ssl", {4005, 1}, ssl),
    file_helper(Config, 'FTP', ?TEST_DIR, Node, 4005, "ftp_file_api_ssl", ssl).

fts_native_file_api(Config) ->
    {ok, Node} = create_node("native_file_api", 4006, normal),
    {ok, Pwd} = file:get_cwd(),
    file_helper(Config,{'NATIVE', 'cosFileTransferNATIVE_file'},filename:split(Pwd),
		Node, 4006, "native_file_api", tcp).
				 
fts_native_file_ssl_api(Config) ->
    {ok, Node} = create_node("native_file_ssl_api", {4007, 1}, ssl),
    {ok, Pwd} = file:get_cwd(),
    file_helper(Config,{'NATIVE', 'cosFileTransferNATIVE_file'},filename:split(Pwd),
	Node, 4007, "native_file_ssl_api", ssl).
				 

file_helper(Config, WhichType, TEST_DIR, Node, Port, Name, Type) ->
    case ftp_host(Config) of
	{skipped, SkippedReason} ->
	    {skipped, SkippedReason};
	Host ->
	    TEST_SOURCE  = TEST_DIR ++ [create_name(remove_me_source)],
	    TEST_SOURCE2 = TEST_DIR ++ [create_name(remove_me_source)],
	    TEST_TARGET  = TEST_DIR ++ [create_name(remove_me_target)],
	    
	    io:format("<<<<<< CosFileTransfer Testing Configuration >>>>>>~n",[]),
	    io:format("Source: ~p~nTarget: ~p~n", [TEST_SOURCE, TEST_TARGET]),
	    
	    ?match(ok, remote_apply(Node, ?MODULE, install_data, 
					  [Type, {WhichType, Host, Name}])),

	    VFST = ?match({'IOP_IOR',"IDL:omg.org/CosFileTransfer/VirtualFileSystem:1.0",_}, 
				corba:string_to_object("corbaname::1.2@localhost:"++integer_to_list(Port)++"/NameService#"++Name)),


            %% Create a Virtual File System.
	    VFS = ?match({_,_,_,_,_,_},
			       cosFileTransferApp:create_VFS(WhichType, [], Host, ?FTP_PORT, 
							     [{protocol, Type}])),
            %% Start two File Transfer Sessions (Source and Target).
	    {FST, _DirT} = ?match({{_,_,_},{_,_,_}},
					'CosFileTransfer_VirtualFileSystem':login(VFST, 
										  ?FTP_USER, 
										  ?FTP_PASS, 
										  ?FTP_ACC)),
	    {FSS, DirS} = ?match({{_,_,_,_,_,_},{_,_,_,_,_,_}},
				       'CosFileTransfer_VirtualFileSystem':login(VFS, 
										 ?FTP_USER, 
										 ?FTP_PASS, 
										 ?FTP_ACC)),

            %% Do some basic test on one of the Directories attributes.
	    ?match([_H|_], 'CosFileTransfer_Directory':'_get_name'(DirS)),
	    ?match([_H|_], 'CosFileTransfer_Directory':'_get_complete_file_name'(DirS)),
	    ?match({'IOP_IOR',[],[]}, 'CosFileTransfer_Directory':'_get_parent'(DirS)),
	    ?match(FSS, 'CosFileTransfer_Directory':'_get_associated_session'(DirS)),

            %% Get a FileList before we create any new Files
	    #'CosFileTransfer_FileWrapper'{the_file = Dir} = 
		?match({'CosFileTransfer_FileWrapper', _, ndirectory},
		       'CosFileTransfer_FileTransferSession':get_file(FSS, TEST_DIR)),
	    {ok,FileList, Iter1} = ?match({ok,_,_}, 'CosFileTransfer_Directory':list(Dir, 10)),
	    loop_files(FileList),

	    case Iter1 of
		{'IOP_IOR',[],[]} ->
		    ok;
		_->
		    ?match(ok, 'CosFileTransfer_FileIterator':destroy(Iter1))
	    end,
	    
	    #any{value=Count1} = ?match({any, _, _}, 'CosPropertyService_PropertySet':
					get_property_value(Dir, "num_children")),
	    
            %% Now we want to transfer a file from source to target. First, we'll create
            %% a a file to work with.
	    create_file_on_source_node(WhichType, Config, Host, 
					     filename:join(TEST_SOURCE), TEST_DIR, 
					     ?TEST_FILE_DATA),
	    create_file_on_source_node(WhichType, Config, Host, 
					     filename:join(TEST_SOURCE2), TEST_DIR, 
					     ?TEST_FILE_DATA2),

	    #'CosFileTransfer_FileWrapper'{the_file = FileS} = 
		?matchnopr({'CosFileTransfer_FileWrapper', _, nfile},
			   'CosFileTransfer_FileTransferSession':get_file(FSS, TEST_SOURCE)),
	    #'CosFileTransfer_FileWrapper'{the_file = FileS2} = 
		?matchnopr({'CosFileTransfer_FileWrapper', _, nfile},
			   'CosFileTransfer_FileTransferSession':get_file(FSS, TEST_SOURCE2)),
	    
	    #any{value=Count2} = ?match({any, _, _}, 'CosPropertyService_PropertySet':
					get_property_value(Dir, "num_children")),
	    timer:sleep(2000),
	    ?match(true, (Count1+2 == Count2)),

            %% Create a target File
	    FileT = ?matchnopr({_,_,_}, 
				     'CosFileTransfer_FileTransferSession':create_file(FST, TEST_TARGET)),
            %% Try to delete the non-existing file.
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_FileTransferSession':delete(FST, FileT)),

	    ?match(ok, 'CosFileTransfer_FileTransferSession':transfer(FSS, FileS, FileT)),
    
            %% Remove this test when ftp supports append.
	    case WhichType of
		{'NATIVE', 'cosFileTransferNATIVE_file'} ->
		    ?match(ok, 'CosFileTransfer_FileTransferSession':append(FSS, FileS, FileT)),
		    ?match(ok, 'CosFileTransfer_FileTransferSession':insert(FSS, FileS2, FileT, 7));
		_->
		    ok
	    end,
	    
            %% Delete source and target files
	    ?match(ok, 'CosFileTransfer_FileTransferSession':delete(FSS, FileS)),
	    ?match(ok, 'CosFileTransfer_FileTransferSession':delete(FSS, FileS2)),
	    ?match(ok, 'CosFileTransfer_FileTransferSession':delete(FST, FileT)),

            %% Should be back where we started.
	    timer:sleep(2000),
	    #any{value=Count3} = ?match({any, _, _}, 'CosPropertyService_PropertySet':
					get_property_value(Dir, "num_children")),
	    ?match(true, (Count1 == Count3)),


	    ?match(false, corba_object:non_existent(FSS)),
	    ?match(false, corba_object:non_existent(FST)),
	    ?match(ok, 'CosFileTransfer_FileTransferSession':logout(FSS)),
	    ?match(ok, 'CosFileTransfer_FileTransferSession':logout(FST)),
            %% To make sure Orber can remove it from mnesia.
	    timer:sleep(2000),
	    ?match(true, corba_object:non_existent(FSS)),
	    ?match(true, corba_object:non_existent(FST)),
	    ?match(ok, remote_apply(Node, ?MODULE, uninstall_data, [Name])),
	    stop_orber_remote(Node, normal),
	    ok
    end.

%%-----------------------------------------------------------------
%%  FileTransferSession API tests 
%%-----------------------------------------------------------------
fts_ftp_dir_api(Config) ->
    {ok, Node} = create_node("ftp_dir_api", 4009, normal),
    dir_helper(Config, 'FTP', ?TEST_DIR, Node, 4009, "ftp_dir_api").

fts_native_dir_api(Config) ->
    {ok, Node} = create_node("native_dir_api", 4010, normal),
    {ok, Pwd} = file:get_cwd(),
    dir_helper(Config, {'NATIVE', 'cosFileTransferNATIVE_file'}, 
	       filename:split(Pwd), Node, 4010, "native_dir_api").

dir_helper(Config, WhichType, TEST_DIR, Node, Port, Name) ->
    case ftp_host(Config) of
	{skipped, SkippedReason} ->
	    {skipped, SkippedReason};
	Host ->
	    TEST_DIR_LEVEL1 = TEST_DIR ++ [create_name(remove_me_dir1)],
	    TEST_DIR_LEVEL2 = TEST_DIR_LEVEL1 ++ [create_name(remove_me_dir2)],
	    
	    io:format("<<<<<< CosFileTransfer Testing Configuration >>>>>>~n",[]),
	    io:format("Top Dir: ~p~nLevel2 Dir: ~p~n", [TEST_DIR_LEVEL1, TEST_DIR_LEVEL2]),
	    
	    ?match(ok, remote_apply(Node, ?MODULE, install_data, 
					  [tcp, {WhichType, Host, Name}])),
	    
	    VFS = ?matchnopr({'IOP_IOR',"IDL:omg.org/CosFileTransfer/VirtualFileSystem:1.0",_}, 
				   corba:string_to_object("corbaname::1.2@localhost:"++integer_to_list(Port)++"/NameService#"++Name)),
	    
            %% Start two File Transfer Sessions (Source and Target).
	    {FS, DirS} = ?matchnopr({{'IOP_IOR',_,_}, _},
					  'CosFileTransfer_VirtualFileSystem':login(VFS, 
										    ?FTP_USER, 
										    ?FTP_PASS, 
										    ?FTP_ACC)),

            %% Do some basic test on one of the Directories attributes.
	    ?match([_H|_], 'CosFileTransfer_Directory':'_get_name'(DirS)),
	    ?match([_H|_], 'CosFileTransfer_Directory':'_get_complete_file_name'(DirS)),
	    ?match({'IOP_IOR',[],[]}, 'CosFileTransfer_Directory':'_get_parent'(DirS)),
	    ?matchnopr(FS, 'CosFileTransfer_Directory':'_get_associated_session'(DirS)),

            %% Create a Root Directory. Currently we only need to create one but
            %% later on, when supporting other protocols than FTP it's not enough.
	    Dir1 =  'CosFileTransfer_FileTransferSession':create_directory(FS, 
										 TEST_DIR_LEVEL1),
	    io:format("<<<<<< CosFileTransfer Testing Properties >>>>>>~n",[]),
	    ?match({ok, [tk_long, tk_boolean]}, 
			 'CosFileTransfer_Directory':get_allowed_property_types(Dir1)),
	    ?match({ok, [_,_]}, 
			 'CosFileTransfer_Directory':get_allowed_properties(Dir1)),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':define_property_with_mode(Dir1, 
									       "num_children",
									       #any{typecode=tk_long, value=0},
									       fixed_readonly)),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':define_property_with_mode(Dir1, 
									       "wrong",
									       #any{typecode=tk_long, value=0},
									       fixed_readonly)),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':define_property_with_mode(Dir1, 
									       "num_children",
									       #any{typecode=tk_short, value=0},
									       fixed_readonly)),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':define_property_with_mode(Dir1, 
									       "num_children",
									       #any{typecode=tk_long, value=0},
									       fixed_normal)),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':define_properties_with_modes(Dir1, 
										  [#'CosPropertyService_PropertyDef'
										   {property_name  = "num_children", 
										    property_value = #any{typecode=tk_long, value=0}, 
										    property_mode  = fixed_readonly}])),
	    ?match(fixed_readonly, 
			 'CosFileTransfer_Directory':get_property_mode(Dir1, "num_children")),
	    ?match({true,
			  [#'CosPropertyService_PropertyMode'{property_name = "num_children", 
							      property_mode = fixed_readonly}]}, 
			 'CosFileTransfer_Directory':get_property_modes(Dir1, ["num_children"])),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':set_property_mode(Dir1, "num_children", fixed_readonly)),
	    
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':
			 set_property_modes(Dir1, 
					    [#'CosPropertyService_PropertyMode'
					     {property_name = "num_children", 
					      property_mode = fixed_readonly}])),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':
			 set_property_modes(Dir1, 
					    [#'CosPropertyService_PropertyMode'
					     {property_name = "wrong", 
					      property_mode = fixed_readonly}])),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':
			 set_property_modes(Dir1, 
					    [#'CosPropertyService_PropertyMode'
					     {property_name = "num_children", 
					      property_mode = fixed_normal}])),
	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':define_property(Dir1, 
								     "num_children",
								     #any{typecode=tk_long, value=0})),
	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':define_property(Dir1, 
								     "wrong",
								     #any{typecode=tk_long, value=0})),
	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':define_property(Dir1, 
								     "num_children",
								     #any{typecode=tk_short, value=0})),
	    
	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':define_property(Dir1, 
								     "num_children",
								     #any{typecode=tk_long, value=0})),

	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':
			 define_properties(Dir1, 
					   [#'CosPropertyService_Property'
					    {property_name = "num_children", 
					     property_value = #any{typecode=tk_long, 
								   value=0}}])),
	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':
			 define_properties(Dir1, 
					   [#'CosPropertyService_Property'
					    {property_name = "wrong", 
					     property_value = #any{typecode=tk_long, 
								   value=0}}])),
	    ?match({'EXCEPTION', _},
			 'CosFileTransfer_Directory':
			 define_properties(Dir1, 
					   [#'CosPropertyService_Property'
					    {property_name = "num_children", 
					     property_value = #any{typecode=tk_short, 
								   value=0}}])),
	    ?match(2, 'CosFileTransfer_Directory':get_number_of_properties(Dir1)),

	    ?match({ok, ["num_children", "is_directory"], {'IOP_IOR',[],[]}}, 
			 'CosFileTransfer_Directory':get_all_property_names(Dir1, 2)),
	    ?match({ok, ["is_directory"], _}, 
			 'CosFileTransfer_Directory':get_all_property_names(Dir1, 1)),
	    
	    ?match(#any{}, 
			 'CosFileTransfer_Directory':get_property_value(Dir1, "num_children")),
	    ?match(#any{}, 
			 'CosFileTransfer_Directory':get_property_value(Dir1, "is_directory")),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':get_property_value(Dir1, "wrong")),
	    
	    ?match({true, 
			  [#'CosPropertyService_Property'{property_name = "num_children"}]}, 
			 'CosFileTransfer_Directory':get_properties(Dir1, ["num_children"])),
	    ?match({false, 
			  [#'CosPropertyService_Property'{property_name = "wrong"}]},
			 'CosFileTransfer_Directory':get_properties(Dir1, ["wrong"])),
	    
	    ?match({ok, [_],_}, 
			 'CosFileTransfer_Directory':get_all_properties(Dir1, 1)),
	    ?match({ok, [_,_], {'IOP_IOR',[],[]}}, 
			 'CosFileTransfer_Directory':get_all_properties(Dir1, 2)),
	    
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':delete_property(Dir1, "num_children")),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':delete_property(Dir1, "wrong")),
	    
	    
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':delete_properties(Dir1, ["num_children"])),
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_Directory':delete_properties(Dir1, ["wrong"])),
	    ?match(false, 'CosFileTransfer_Directory':delete_all_properties(Dir1)),
	    ?match(true, 
			 'CosFileTransfer_Directory':is_property_defined(Dir1, "num_children")),
	    ?match(false, 
			 'CosFileTransfer_Directory':is_property_defined(Dir1, "wrong")),

            %% The Top Dir should be empty and ...
	    ?match({ok,[],_}, 'CosFileTransfer_Directory':list(Dir1, 1000)),
	    ?match( #any{value=0},
			  'CosPropertyService_PropertySet':get_property_value(Dir1, "num_children")),
            %% Create a sub-directory.
	    Dir2 = 'CosFileTransfer_FileTransferSession':create_directory(FS, 
										TEST_DIR_LEVEL2),
	    ?match( #any{value=1},
			  'CosPropertyService_PropertySet':get_property_value(Dir1, "num_children")),
	    
	    ?match({ok, [_,_], {'IOP_IOR',[],[]}}, 
			 'CosFileTransfer_Directory':get_all_properties(Dir1, 2)),
	    {_,_,Iterator1} = ?match({ok, [_], _}, 
					   'CosFileTransfer_Directory':get_all_properties(Dir1, 1)),
	    ?match({false, [_]}, 
			 'CosPropertyService_PropertiesIterator':next_n(Iterator1,4)),
	    
	    {_,_,Iterator0} = ?match({ok, [], _}, 
					   'CosFileTransfer_Directory':get_all_properties(Dir1, 0)),
	    
	    ?match({false, [_, {'CosPropertyService_Property',
				      "num_children",{any,tk_long,1}}]},
			 'CosPropertyService_PropertiesIterator':next_n(Iterator0,4)),
	    
	    ?match({true, 
			  [#'CosPropertyService_Property'{property_name = "num_children"}]}, 
			 'CosFileTransfer_Directory':get_properties(Dir1, ["num_children"])),
	    
            %% The Top Directory is not emtpy any more and ...
	    {ok,[#'CosFileTransfer_FileWrapper'{the_file = DirRef}],_} = 
		?matchnopr({ok,[{'CosFileTransfer_FileWrapper', _, ndirectory}],_}, 
			   'CosFileTransfer_Directory':list(Dir1, 1000)),
            %% ... its name eq. to 'TEST_DIR_LEVEL2'
	    ?match(TEST_DIR_LEVEL2, 
			 'CosFileTransfer_Directory':'_get_complete_file_name'(DirRef)),
	    
	    #'CosFileTransfer_FileWrapper'{the_file = Dir3} = 
		?matchnopr({'CosFileTransfer_FileWrapper', _, ndirectory},
			   'CosFileTransfer_FileTransferSession':get_file(FS, TEST_DIR_LEVEL1)),
	    
            %% Must get the same result for the 'get_file' operation.
	    {ok,[#'CosFileTransfer_FileWrapper'{the_file = DirRef2}],_} = 
		?matchnopr({ok,[{'CosFileTransfer_FileWrapper', _, ndirectory}],_}, 
			   'CosFileTransfer_Directory':list(Dir3,1000)),
	    ?match(TEST_DIR_LEVEL2, 
			 'CosFileTransfer_Directory':'_get_complete_file_name'(DirRef2)),
	    
            %% Since the top directory isn't empty deleting it must fail.
	    ?match({'EXCEPTION', _}, 
			 'CosFileTransfer_FileTransferSession':delete(FS, Dir1)),
	    
            %% Delete the sub-directory and ...
	    ?match(ok, 'CosFileTransfer_FileTransferSession':delete(FS, Dir2)),
            %% ... see if the top directory realyy is empty.
	    ?match({ok,[],_}, 'CosFileTransfer_Directory':list(Dir1, 1000)),

	    ?match(ok, 'CosFileTransfer_FileTransferSession':delete(FS, Dir1)),
            %% Test if the top directory been removed as intended.
	    ?match({'EXCEPTION', {'CosFileTransfer_FileNotFoundException', _, _}},
			 'CosFileTransfer_FileTransferSession':get_file(FS, TEST_DIR_LEVEL1)),

	    ?match(false, corba_object:non_existent(FS)),
	    ?match(ok, 'CosFileTransfer_FileTransferSession':logout(FS)),
            %% To make sure Orber can remove it from mnesia.
	    timer:sleep(1000),
	    ?match(true, corba_object:non_existent(FS)),
	    ?match(ok, remote_apply(Node, ?MODULE, uninstall_data, [Name])),
	    stop_orber_remote(Node, normal),
	    ok
    end.


%%-----------------------------------------------------------------
%%  Internal functions
%%-----------------------------------------------------------------
ftp_host(Config) ->
    case ?config(ftp_remote_host, Config) of
	undefined ->
	    {skipped, "The configuration parameter 'ftp_remote_host' not defined."};
	Host ->
	    Host
    end.

loop_files([]) ->
    io:format("@@@ DONE @@@~n", []);
loop_files([#'CosFileTransfer_FileWrapper'{the_file = H}|T]) ->
    FullName = 'CosFileTransfer_File':'_get_complete_file_name'(H),
    Name = 'CosFileTransfer_File':'_get_name'(H),
    io:format("FULL NAME: ~p   SHORT NAME: ~p~n", [FullName, Name]),
    loop_files(T).


create_file_on_source_node('FTP', _Config, Host, FileName, Path, Data) ->
    io:format("<<<<<< CosFileTransfer Testing File >>>>>>~n",[]),
    io:format("Host: ~p~nPath: ~p~nFile: ~p~n", [Host, Path, FileName]),
    {ok, Pid} = ?match({ok, _}, inets:start(ftpc, [{host, Host}], stand_alone)),
    ?match(ok, ftp:user(Pid, ?FTP_USER, ?FTP_PASS)),
    ?match(ok, ftp:cd(Pid, Path)),
    ?match(ok, ftp:send_bin(Pid, list_to_binary(Data), FileName)),
    ?match(ok, inets:stop(ftpc, Pid));
create_file_on_source_node({'NATIVE', _}, _Config, Host, FileName, Path, Data) ->
    io:format("<<<<<< CosFileTransfer Testing File >>>>>>~n",[]),
    io:format("Host: ~p~nPath: ~p~nFile: ~p~n", [Host, Path, FileName]),
    ?match(ok, file:write_file(FileName, list_to_binary(Data))).
    
create_name(Type) ->
    Time = erlang:system_time(),
    Unique = erlang:unique_integer([positive]),
    lists:concat([Type, '_', Time, '_', Unique]).




%%------------------------------------------------------------
%% function : create_node/4
%% Arguments: Name - the name of the new node (atom())
%%            Port - which iiop_port (integer())
%%            Domain - which domain.
%%            Type - if /4 used the types defines the extra arguments
%%                   to be used.
%% Returns  : {ok, Node} | {error, _}
%% Effect   : Starts a new slave-node with given (optinally)
%%            extra arguments. If fails it retries 'Retries' times.
%%------------------------------------------------------------
create_node(Name, Port, normal) ->
    Args = basic_args(Name),
    create_node(Name, Port, 10, normal, Args, []);
create_node(Name, {Port, _Depth}, ssl) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    Args = basic_args(Name),
    {ok, Node} = create_node(list_to_atom(Name), Port, 10, ssl, Args, []),
    %% Client
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_client_certfile,
					  filename:join([Dir, "client", "cert.pem"])]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_client_cacertfile,
					  filename:join([Dir, "client", "cacerts.pem"])]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_client_keyfile,
					  filename:join([Dir, "client", "key.pem"])]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_client_verify, 1]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_client_depth, 0]),
    
    %% Server
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_server_certfile,
					  filename:join([Dir, "server", "cert.pem"])]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_server_cacertfile,
					  filename:join([Dir, "server", "cacerts.pem"])]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_server_keyfile,
					  filename:join([Dir, "server", "key.pem"])]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_server_verify, 1]),
    rpc:call(Node, application, set_env, [cosFileTransfer, ssl_server_depth, 0]),
    {ok, Node}.

%create_node(Name, {Port, Depth}, ssl) ->
%    TestLibs = filename:join(filename:dirname(code:which(?MODULE)), "ssl_data"),
%    Args = basic_args(Name),
%    SArgs = basic_ssl_args(TestLibs, Args),
%    LArgs = level_based_ssl(Depth, TestLibs, SArgs),
%    create_node(list_to_atom(Name), Port, 10, ssl, LArgs, [{sslpath, TestLibs}]).

create_node(Name, Port, Retries, Type, Args, Options) ->
    [_, Host] = ?match([_,_],string:tokens(atom_to_list(node()), [$@])),
    case starter(Host, Name, Args) of
	{ok, NewNode} ->
            ?match(pong, net_adm:ping(NewNode)),
            {ok, Cwd} = file:get_cwd(),
            Path = code:get_path(),
            ?match(ok, rpc:call(NewNode, file, set_cwd, [Cwd])),
            true = rpc:call(NewNode, code, set_path, [Path]),
	    ?match(ok, start_orber_remote(NewNode, Type, Options, Port)),
            spawn_link(NewNode, ?MODULE, slave_sup, []),
            rpc:multicall([node() | nodes()], global, sync, []),
            {ok, NewNode};
        {error, Reason} when Retries == 0->
            {error, Reason};
        {error, Reason} ->          
            io:format("Could not start slavenode ~p ~p retrying~n", 
                      [{Host, Name, Args}, Reason]),
            timer:sleep(500),
            create_node(Name, Port, Retries - 1, Type, Args, Options)
    end.

starter(Host, Name, Args) ->
    slave:start(Host, Name, Args).

slave_sup() ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', _, _} -> 
	    ignore
    end.


%%------------------------------------------------------------
%% function : destroy_node
%% Arguments: Node - which node to destroy.
%%            Type - normal | ssl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
-ifdef(false).
destroy_node(Node, Type) ->
    stopper(Node, Type).

stopper(Node, Type) ->
    catch stop_orber_remote(Node, Type),
    slave:stop(Node).
-endif.
  
%%------------------------------------------------------------
%% function : remote_apply
%% Arguments: N - Node, M - Module,
%%            F - Function, A - Arguments (list)
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
remote_apply(N, M,F,A) ->
    case rpc:call(N, M, F, A) of
	{badrpc, Reason} ->
	    exit(Reason);
	Other ->
	    Other
    end.

%%------------------------------------------------------------
%% function : stop_orber_remote
%% Arguments: Node - which node to stop orber on.
%%            Type - normal | ssl | light | .......
%% Returns  : ok
%% Effect   : Stops orber on given node and, if specified,
%%            other applications or programs.
%%------------------------------------------------------------
stop_orber_remote(Node, ssl) ->
    rpc:call(Node, ssl, stop, []),
    rpc:call(Node, crypto, stop, []),
    orb_rpc_blast(Node, ssl);
stop_orber_remote(Node, Type) ->
    orb_rpc_blast(Node, Type).

orb_rpc_blast(Node, _) ->
    rpc:call(Node, cosFileTransferApp, stop, []),
    rpc:call(Node, cosProperty, stop, []),
    rpc:call(Node, cosFileTransferApp, uninstall, []),
    rpc:call(Node, cosProperty, uninstall, []),
    rpc:call(Node, orber, jump_stop, []).

%%------------------------------------------------------------
%% function : start_orber_remote
%% Arguments: Node - which node to start orber on.
%%            Type - normal | ssl | light | .......
%% Returns  : ok
%% Effect   : Starts orber on given node and, if specified,
%%            other applications or programs.
%%------------------------------------------------------------
start_orber_remote(Node, ssl, _Options, Port) ->
    rpc:call(Node, ssl, start, []),
    rpc:call(Node, crypto, start, []),
    rpc:call(Node, ssl, seed, ["testing"]),
    orb_rpc_setup(Node, ssl, Port);
start_orber_remote(Node, Type, _, Port) ->
    orb_rpc_setup(Node, Type, Port).

orb_rpc_setup(Node, _, Port) ->
    rpc:call(Node, orber, jump_start, [Port]),
    rpc:call(Node, cosProperty, install, []),
    rpc:call(Node, cosProperty, start, []),
    rpc:call(Node, cosFileTransferApp, install, []).

%%--------------- MISC FUNCTIONS -----------------------------
basic_args(_Name) ->
    TestLibs = filename:dirname(code:which(?MODULE)),
    " -orber orber_debug_level 10" ++
	" -pa " ++
        TestLibs ++ 
	" -pa " ++
        filename:join(TestLibs, "all_SUITE_data") ++ 
        " -pa " ++ 
        filename:dirname(code:which(cosFileTransferApp)).

-ifdef(false).
basic_ssl_args(TestLibs, Args) ->
%    Args ++
%	" -cosFileTransfer ssl_client_certfile \\\"" ++
%	filename:join(TestLibs, "ssl_client_cert.pem") ++
%	"\\\" -cosFileTransfer ssl_server_certfile \\\""++
%	filename:join(TestLibs, "ssl_server_cert.pem")++"\\\"".

    io:format("<<<<<< SSL LIBS ~p >>>>>>~n",[TestLibs]),
    NewArgs = Args ++
	" -cosFileTransfer ssl_client_certfile \\\"" ++
	filename:join(TestLibs, "ssl_client_cert.pem") ++
	"\\\" -cosFileTransfer ssl_server_certfile \\\""++
	filename:join(TestLibs, "ssl_server_cert.pem")++"\\\"",
    io:format("<<<<<< SSL LIBS ARGS ~p >>>>>>~n",[NewArgs]),
    NewArgs.

level_based_ssl(1, _TestLibs, Args) ->
    Args;
level_based_ssl(2, _TestLibs, Args) ->
    Args.% ++
%	" -cosFileTransfer ssl_server_depth 2 " ++
%	" -cosFileTransfer ssl_client_depth 2 " ++
%	" -cosFileTransfer ssl_server_verify " ++
%	" -cosFileTransfer ssl_client_verify " ++
%	" -cosFileTransfer ssl_server_cacertfile " ++
%	" -cosFileTransfer ssl_client_cacertfile " ++

-endif.

install_data(Protocol, {WhichType, Host, Name}) ->
    io:format("<<<<<< Starting ~p/~p VFS at ~p/~p>>>>>>~n",
	      [Protocol, WhichType, Host, Name]),
    %% Create a Virtual File System.
    VFS = ?match({_,_,_,_,_,_},
		       cosFileTransferApp:create_VFS(WhichType, [], Host, ?FTP_PORT, 
						     [{protocol, Protocol}])),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), Name),
    N = lname:insert_component(lname:create(), 1, NC1),
    'CosNaming_NamingContext':rebind(NS, N, VFS).

uninstall_data(Name) ->
    VFS = ?match({_,_,_,_,_,_}, 
		       corba:string_to_object("corbaname:rir:/NameService#"++Name)),
    ?match(ok, corba:dispose(VFS)),
    ok.
    


%%------------------- EOF MODULE-----------------------------------
