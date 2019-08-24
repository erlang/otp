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
%%     $Id: mod_htaccess.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $

-module(mod_htaccess).

-export([do/1, load/2]).
-export([debug/0]).

-include("httpd.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public methods that interface the eswapi                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% Public method called by the webbserver to insert the data about
% Names on accessfiles
%----------------------------------------------------------------------
load([$A,$c,$c,$e,$s,$s,$F,$i,$l,$e,$N,$a,$m,$e|FileNames],Context)->
    CleanFileNames=httpd_conf:clean(FileNames),
    %%io:format("\n The filenames is:" ++ FileNames ++ "\n"),
    {ok,[],{access_files,string:tokens(CleanFileNames," ")}}.


%----------------------------------------------------------------------
% Public method that the webbserver calls to control the page
%----------------------------------------------------------------------
do(Info)->
    case httpd_util:key1search(Info#mod.data,status) of
	{Status_code,PhraseArgs,Reason}->
	    {proceed,Info#mod.data};
	undefined ->
	    control_path(Info)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% The functions that start the control if there is a accessfile    %%
%% and if so controls if the dir is allowed or not                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
%Info = record mod as specified in httpd.hrl
%returns either {proceed,Info#mod.data}
%{proceed,[{status,403....}|Info#mod.data]}
%{proceed,[{status,401....}|Info#mod.data]}
%{proceed,[{status,500....}|Info#mod.data]}
%----------------------------------------------------------------------
control_path(Info) ->
    Path = mod_alias:path(Info#mod.data,
			  Info#mod.config_db,
			  Info#mod.request_uri),
    case isErlScriptOrNotAccessibleFile(Path,Info) of
	true->
	    {proceed,Info#mod.data};
	false->
	    case getHtAccessData(Path,Info)of
		{ok,public}->
		    %%There was no restrictions on the page continue
		    {proceed,Info#mod.data};
		{error,Reason} ->
		    %Something got wrong continue or quit??????????????????/
                   {proceed,Info#mod.data};
		{accessData,AccessData}->
		    controlAllowedMethod(Info,AccessData)
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% These methods controls that the method the client used in the    %%
%% request is one of the limited                                    %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
%Control that if the accessmethod used is in the list of modes to challenge
%
%Info is the mod record as specified in httpd.hrl
%AccessData is an ets table whit the data in the .htaccessfiles
%----------------------------------------------------------------------
controlAllowedMethod(Info,AccessData)->
    case allowedRequestMethod(Info,AccessData) of
	allow->
	    %%The request didnt use one of the limited methods
	    ets:delete(AccessData),
	    {proceed,Info#mod.data};
	challenge->
	    authenticateUser(Info,AccessData)
    end.

%----------------------------------------------------------------------
%Check the specified access method in the .htaccessfile
%----------------------------------------------------------------------
allowedRequestMethod(Info,AccessData)->
    case ets:lookup(AccessData,limit) of
	[{limit,all}]->
	    challenge;
	[{limit,Methods}]->
	    isLimitedRequestMethod(Info,Methods)
    end.


%----------------------------------------------------------------------
%Check the specified accessmethods in the .htaccesfile against the users
%accessmethod
%
%Info is the record from the do call
%Methods is a list of the methods specified in the .htaccessfile
%----------------------------------------------------------------------
isLimitedRequestMethod(Info,Methods)->
    case lists:member(Info#mod.method,Methods) of
	true->
	    challenge;
	false ->
	    allow
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% These methods controls that the user comes from an allowwed net  %%
%% and if so wheather its a valid user or a challenge shall be      %%
%% generated                                                        %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
%The first thing to control is that the user is from a network
%that has access to the page
%----------------------------------------------------------------------
authenticateUser(Info,AccessData)->
    case controlNet(Info,AccessData) of
	allow->
	    %the network is ok control that it is an allowed user
	    authenticateUser2(Info,AccessData);
	deny->
	    %The user isnt allowed to access the pages from that network
	    ets:delete(AccessData),
	    {proceed,[{status,{403,Info#mod.request_uri,
	    "Restricted area not allowed from your network"}}|Info#mod.data]}
    end.


%----------------------------------------------------------------------
%The network the user comes from is allowed to view the resources
%control whether the user needsto supply a password or not
%----------------------------------------------------------------------
authenticateUser2(Info,AccessData)->
    case ets:lookup(AccessData,require) of
	[{require,AllowedUsers}]->
	    case ets:lookup(AccessData,auth_name) of
		[{auth_name,Realm}]->
		    authenticateUser2(Info,AccessData,Realm,AllowedUsers);
		_NoAuthName->
		    ets:delete(AccessData),
		    {break,[{status,{500,none,
				     ?NICE("mod_htaccess:AuthName directive not specified")}}]}
	    end;
	[] ->
	    %%No special user is required the network is ok so let
	    %%the user in
	    ets:delete(AccessData),
	    {proceed,Info#mod.data}
    end.


%----------------------------------------------------------------------
%The user must send a userId and a password to get the resource
%Control if its already in the http-request
%if the file with users is bad send an 500 response
%----------------------------------------------------------------------
authenticateUser2(Info,AccessData,Realm,AllowedUsers)->
    case authenticateUser(Info,AccessData,AllowedUsers) of
	allow ->
	    ets:delete(AccessData),
	    {user,Name,Pwd}=getAuthenticatingDataFromHeader(Info),
	    {proceed, [{remote_user_name,Name}|Info#mod.data]};
	challenge->
	    ets:delete(AccessData),
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
	deny->
	    ets:delete(AccessData),
	    {break,[{status,{500,none,
			     ?NICE("mod_htaccess:Bad path to user or group file")}}]}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Methods that validate the netwqork the user comes from           %%
%% according to the allowed networks                                %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------------------------------------------------------------
%Controls the users networkaddress agains the specifed networks to
%allow or deny
%
%returns either allow or deny
%----------------------------------------------------------------------
controlNet(Info,AccessData)->
    UserNetwork=getUserNetworkAddress(Info),
    case getAllowDenyOrder(AccessData) of
	{_deny,[],_allow,[]}->
	    allow;
	{deny,[],allow,AllowedNetworks}->
	    controlIfAllowed(AllowedNetworks,UserNetwork,allow,deny);
	{allow,AllowedNetworks,deny,[]}->
	    controlIfAllowed(AllowedNetworks,UserNetwork,allow,deny);

	{deny,DeniedNetworks,allow,[]}->
	    controlIfAllowed(DeniedNetworks,UserNetwork,allow,deny);
	{allow,[],deny,DeniedNetworks}->
	    controlIfAllowed(DeniedNetworks,UserNetwork,allow,deny);

	{deny,DeniedNetworks,allow,AllowedNetworks}->
	    controlDenyAllow(DeniedNetworks,AllowedNetworks,UserNetwork);
	{allow,AllowedNetworks,deny,DeniedNetworks}->
	    controlAllowDeny(AllowedNetworks,DeniedNetworks,UserNetwork)
    end.


%----------------------------------------------------------------------
%Returns the users IP-Number
%----------------------------------------------------------------------
getUserNetworkAddress(Info)->
    {_Socket,Address}=(Info#mod.init_data)#init_data.peername,
    Address.


%----------------------------------------------------------------------
%Control the users Ip-number against the ip-numbers in the .htaccessfile
%----------------------------------------------------------------------
controlIfAllowed(AllowedNetworks,UserNetwork,IfAllowed,IfDenied)->
    case AllowedNetworks of
	[{allow,all}]->
	   IfAllowed;
	[{deny,all}]->
	    IfDenied;
        [{deny,Networks}]->
	    memberNetwork(Networks,UserNetwork,IfDenied,IfAllowed);
	[{allow,Networks}]->
	    memberNetwork(Networks,UserNetwork,IfAllowed,IfDenied);
	_Error->
	    IfDenied
    end.


%--------------------------------------------------------------------%
%The Denycontrol isn't necessary to preform since the allow control  %
%override the deny control                                           %
%--------------------------------------------------------------------%
controlDenyAllow(DeniedNetworks,AllowedNetworks,UserNetwork)->
    case AllowedNetworks of
	[{allow,all}]->
	    allow;
	[{allow,Networks}]->
	  case memberNetwork(Networks,UserNetwork) of
	      true->
		  allow;
	      false->
		  deny
	  end
    end.


%----------------------------------------------------------------------%
%Control that the user is in the allowed list if so control that the   %
%network is in the denied list
%----------------------------------------------------------------------%
controlAllowDeny(AllowedNetworks,DeniedNetworks,UserNetwork)->
    case controlIfAllowed(AllowedNetworks,UserNetwork,allow,deny) of
	allow->
	    controlIfAllowed(DeniedNetworks,UserNetwork,deny,allow);
	deny ->
	    deny
    end.

%----------------------------------------------------------------------
%Controls if the users Ipnumber is in the list of either denied or
%allowed networks
%----------------------------------------------------------------------
memberNetwork(Networks,UserNetwork,IfTrue,IfFalse)->
    case memberNetwork(Networks,UserNetwork) of
	true->
	    IfTrue;
	false->
	    IfFalse
    end.


%----------------------------------------------------------------------
%regexp match the users ip-address against the networks in the list of
%ipadresses or subnet addresses.
memberNetwork(Networks,UserNetwork)->
    case lists:filter(fun(Net)->
			      case regexp:match(UserNetwork,
						formatRegexp(Net)) of
				  {match,1,_}->
				      true;
				  _NotSubNet ->
				      false
			      end
		      end,Networks) of
	[]->
	    false;
	MemberNetWork ->
	    true
    end.


%----------------------------------------------------------------------
%Creates a regexp from an ip-number i.e "127.0.0-> "^127[.]0[.]0.*"
%"127.0.0.-> "^127[.]0[.]0[.].*"
%----------------------------------------------------------------------
formatRegexp(Net)->
    [SubNet1|SubNets]=string:tokens(Net,"."),
    NetRegexp=lists:foldl(fun(SubNet,Newnet)->
				  Newnet ++ "[.]" ++SubNet
			  end,"^"++SubNet1,SubNets),
    case string:len(Net)-string:rchr(Net,$.) of
	0->
	    NetRegexp++"[.].*";
	_->
	    NetRegexp++".*"
    end.


%----------------------------------------------------------------------
%If the user has specified if the allow or deny check shall be preformed
%first get that order if no order is specified take
%allow - deny since its harder that deny - allow
%----------------------------------------------------------------------
getAllowDenyOrder(AccessData)->
    case ets:lookup(AccessData,order) of
	[{order,{deny,allow}}]->
	    {deny,ets:lookup(AccessData,deny),
	     allow,ets:lookup(AccessData,allow)};
	_DefaultOrder->
	    {allow,ets:lookup(AccessData,allow),
	     deny,ets:lookup(AccessData,deny)}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% The methods that validates the user                              %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
%Control if there is anyu autheticating data in threquest header
%if so it controls it against the users in the list Allowed Users
%----------------------------------------------------------------------
authenticateUser(Info,AccessData,AllowedUsers)->
    case getAuthenticatingDataFromHeader(Info) of
	{user,User,PassWord}->
	    authenticateUser(Info,AccessData,AllowedUsers,
			     {user,User,PassWord});
	{error,nouser}->
	    challenge;
	{error,BadData}->
	    challenge
    end.


%----------------------------------------------------------------------
%Returns the Autheticating data in the http-request
%----------------------------------------------------------------------
getAuthenticatingDataFromHeader(Info)->
    PrsedHeader=Info#mod.parsed_header,
    case httpd_util:key1search(PrsedHeader,"authorization" ) of
	undefined->
	    {error,nouser};
	[$B,$a,$s,$i,$c,$\ |EncodedString]->
	    UnCodedString=httpd_util:decode_base64(EncodedString),
	    case httpd_util:split(UnCodedString,":",2) of
		{ok,[User,PassWord]}->
		    {user,User,PassWord};
		{error,Error}->
		    {error,Error}
	    end;
	BadCredentials ->
	    {error,BadCredentials}
    end.


%----------------------------------------------------------------------
%Returns a list of all members of the allowed groups
%----------------------------------------------------------------------
getGroupMembers(Groups,AllowedGroups)->
    Allowed=lists:foldl(fun({group,Name,Members},AllowedMembers)->
				case lists:member(Name,AllowedGroups) of
				    true->
					AllowedMembers++Members;
				    false ->
					AllowedMembers
				end
	       end,[],Groups),
    {ok,Allowed}.

authenticateUser(Info,AccessData,{{users,[]},{groups,Groups}},User)->
    authenticateUser(Info,AccessData,{groups,Groups},User);
authenticateUser(Info,AccessData,{{users,Users},{groups,[]}},User)->
    authenticateUser(Info,AccessData,{users,Users},User);

authenticateUser(Info,AccessData,{{users,Users},{groups,Groups}},User)->
    AllowUser=authenticateUser(Info,AccessData,{users,Users},User),
    AllowGroup=authenticateUser(Info,AccessData,{groups,Groups},User),
    case {AllowGroup,AllowUser} of
	{_,allow}->
	    allow;
	{allow,_}->
	    allow;
	{challenge,_}->
	    challenge;
	{_,challenge}->
	    challenge;
	{_deny,_deny}->
	    deny
    end;


%----------------------------------------------------------------------
%Controls that the user is a member in one of the allowed group
%----------------------------------------------------------------------
authenticateUser(Info,AccessData,{groups,AllowedGroups},{user,User,PassWord})->
    case getUsers(AccessData,group_file) of
	{group_data,Groups}->
	    case  getGroupMembers(Groups,AllowedGroups) of
	       {ok,Members}->
		    authenticateUser(Info,AccessData,{users,Members},
				     {user,User,PassWord});
		{error,BadData}->
		    deny
	    end;
	{error,BadData}->
	    deny
    end;


%----------------------------------------------------------------------
%Control that the user is one of the allowed users and that the passwd is ok
%----------------------------------------------------------------------
authenticateUser(Info,AccessData,{users,AllowedUsers},{user,User,PassWord})->
    case lists:member(User,AllowedUsers) of
       true->
	    %Get the usernames and passwords from the file
	    case getUsers(AccessData,user_file) of
		{error,BadData}->
		    deny;
		{user_data,Users}->
		    %Users is a list of the users in
		    %the userfile [{user,User,Passwd}]
		    checkPassWord(Users,{user,User,PassWord})
	    end;
	false ->
	    challenge
    end.


%----------------------------------------------------------------------
%Control that the user User={user,"UserName","PassWd"} is
%member of the list of Users
%----------------------------------------------------------------------
checkPassWord(Users,User)->
    case lists:member(User,Users) of
	true->
	    allow;
	false->
	    challenge
    end.


%----------------------------------------------------------------------
%Get the users in the specified file
%UserOrGroup is an atom that specify if its a group file or a user file
%i.e. group_file or user_file
%----------------------------------------------------------------------
getUsers({file,FileName},UserOrGroup)->
    case file:open(FileName,[read]) of
        {ok,AccessFileHandle} ->
	    getUsers({stream,AccessFileHandle},[],UserOrGroup);
        {error,Reason} ->
	    {error,{Reason,FileName}}
    end;


%----------------------------------------------------------------------
%The method that starts the lokkong for user files
%----------------------------------------------------------------------

getUsers(AccessData,UserOrGroup)->
    case ets:lookup(AccessData,UserOrGroup) of
	[{UserOrGroup,File}]->
	    getUsers({file,File},UserOrGroup);
	_ ->
	    {error,noUsers}
    end.


%----------------------------------------------------------------------
%Reads data from the filehandle File to the list FileData and when its
%reach the end it returns the list in a tuple {user_file|group_file,FileData}
%----------------------------------------------------------------------
getUsers({stream,File},FileData,UserOrGroup)->
    case io:get_line(File,[]) of
        eof when UserOrGroup==user_file->
	    {user_data,FileData};
	eof when UserOrGroup ==group_file->
	   {group_data,FileData};
        Line ->
	    getUsers({stream,File},
		     formatUser(Line,FileData,UserOrGroup),UserOrGroup)
    end.


%----------------------------------------------------------------------
%If the line is a comment remove it
%----------------------------------------------------------------------
formatUser([$#|UserDataComment],FileData,_UserOrgroup)->
    FileData;


%----------------------------------------------------------------------
%The user name in the file is Username:Passwd\n
%Remove the newline sign and split the user name in
%UserName and Password
%----------------------------------------------------------------------
formatUser(UserData,FileData,UserOrGroup)->
    case string:tokens(UserData," \r\n")of
	[User|Whitespace] when UserOrGroup==user_file->
	    case string:tokens(User,":") of
		[Name,PassWord]->
		    [{user,Name,PassWord}|FileData];
		_Error->
		    FileData
	    end;
	GroupData when UserOrGroup==group_file ->
	    parseGroupData(GroupData,FileData);
	_Error ->
	    FileData
    end.


%----------------------------------------------------------------------
%if everything is right GroupData is on the form
% ["groupName:", "Member1", "Member2", "Member2"
%----------------------------------------------------------------------
parseGroupData([GroupName|GroupData],FileData)->
    [{group,formatGroupName(GroupName),GroupData}|FileData].


%----------------------------------------------------------------------
%the line in the file is GroupName: Member1 Member2 .....MemberN
%Remove the : from the group name
%----------------------------------------------------------------------
formatGroupName(GroupName)->
    string:strip(GroupName,right,$:).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%  Functions that parses the accessfiles                           %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
%Control that the asset is a real file and not a request for an virtual
%asset
%----------------------------------------------------------------------
isErlScriptOrNotAccessibleFile(Path,Info)->
    case file:read_file_info(Path) of
	{ok,_fileInfo}->
	    false;
	{error,_Reason} ->
	    true
    end.


%----------------------------------------------------------------------
%Path=PathToTheRequestedFile=String
%Innfo=record#mod
%----------------------------------------------------------------------
getHtAccessData(Path,Info)->
    HtAccessFileNames=getHtAccessFileNames(Info),
    case getData(Path,Info,HtAccessFileNames) of
	{ok,public}->
	    {ok,public};
	{accessData,AccessData}->
	    {accessData,AccessData};
	{error,Reason} ->
	    {error,Reason}
    end.


%----------------------------------------------------------------------
%returns the names of the accessfiles
%----------------------------------------------------------------------
getHtAccessFileNames(Info)->
    case httpd_util:lookup(Info#mod.config_db,access_files) of
	undefined->
	    [".htaccess"];
	Files->
	    Files
    end.
%----------------------------------------------------------------------
%HtAccessFileNames=["accessfileName1",..."AccessFileName2"]
%----------------------------------------------------------------------
getData(Path,Info,HtAccessFileNames)->
    case regexp:split(Path,"/") of
	{error,Error}->
	    {error,Error};
	{ok,SplittedPath}->
	    getData2(HtAccessFileNames,SplittedPath,Info)
	end.


%----------------------------------------------------------------------
%Add to together the data in the Splittedpath up to the path
%that is the alias or the document root
%Since we do not need to control after any accessfiles before here
%----------------------------------------------------------------------
getData2(HtAccessFileNames,SplittedPath,Info)->
    case getRootPath(SplittedPath,Info) of
	{error,Path}->
	    {error,Path};
	{ok,StartPath,RestOfSplittedPath} ->
	    getData2(HtAccessFileNames,StartPath,RestOfSplittedPath,Info)
    end.


%----------------------------------------------------------------------
%HtAccessFilenames is a list the names the accesssfiles can have
%Path is the shortest match against all alias and documentroot
%rest of splitted path is a list of the parts of the path
%Info is the mod recod from the server
%----------------------------------------------------------------------
getData2(HtAccessFileNames,StartPath,RestOfSplittedPath,Info)->
    case getHtAccessFiles(HtAccessFileNames,StartPath,RestOfSplittedPath) of
	[]->
	    %No accessfile qiut its a public directory
	    {ok,public};
	Files ->
	    loadAccessFilesData(Files)
    end.


%----------------------------------------------------------------------
%Loads the data in the accessFiles specifiied by
% AccessFiles=["/hoem/public/html/accefile",
%               "/home/public/html/priv/accessfile"]
%----------------------------------------------------------------------
loadAccessFilesData(AccessFiles)->
    loadAccessFilesData(AccessFiles,ets:new(accessData,[])).


%----------------------------------------------------------------------
%Returns the found data
%----------------------------------------------------------------------
contextToValues(AccessData)->
    case ets:lookup(AccessData,context) of
	[{context,Values}]->
	    ets:delete(AccessData,context),
	    insertContext(AccessData,Values),
	    {accessData,AccessData};
	_Error->
	    {error,errorInAccessFile}
    end.


insertContext(AccessData,[])->
    ok;

insertContext(AccessData,[{allow,From}|Values])->
    insertDenyAllowContext(AccessData,{allow,From}),
    insertContext(AccessData,Values);

insertContext(AccessData,[{deny,From}|Values])->
    insertDenyAllowContext(AccessData,{deny,From}),
    insertContext(AccessData,Values);

insertContext(AccessData,[{require,{GrpOrUsr,Members}}|Values])->
    case ets:lookup(AccessData,require) of
	[]when GrpOrUsr==users->
	    ets:insert(AccessData,{require,{{users,Members},{groups,[]}}});

	[{require,{{users,Users},{groups,Groups}}}]when GrpOrUsr==users ->
	    ets:insert(AccessData,{require,{{users,Users++Members},
					   {groups,Groups}}});
	[]when GrpOrUsr==groups->
	    ets:insert(AccessData,{require,{{users,[]},{groups,Members}}});

	[{require,{{users,Users},{groups,Groups}}}]when GrpOrUsr==groups ->
	    ets:insert(AccessData,{require,{{users,Users},
					   {groups,Groups++Members}}})
    end,
    insertContext(AccessData,Values);



%%limit and order directive need no transforming they areis just to insert
insertContext(AccessData,[Elem|Values])->
    ets:insert(AccessData,Elem),
    insertContext(AccessData,Values).


insertDenyAllowContext(AccessData,{AllowDeny,From})->
    case From of
	all->
	    ets:insert(AccessData,{AllowDeny,all});
	AllowedSubnets->
	    case ets:lookup(AccessData,AllowDeny) of
		[]->
		    ets:insert(AccessData,{AllowDeny,From});
		[{AllowDeny,all}]->
		    ok;
		[{AllowDeny,Networks}]->
		    ets:insert(AccessData,{allow,Networks++From})
	    end
    end.

loadAccessFilesData([],AccessData)->
    %preform context to limits
    contextToValues(AccessData),
    {accessData,AccessData};

%----------------------------------------------------------------------
%Takes each file in the list and load the data to the ets table
%AccessData
%----------------------------------------------------------------------
loadAccessFilesData([FileName|FileNames],AccessData)->
    case loadAccessFileData({file,FileName},AccessData) of
	overRide->
	    loadAccessFilesData(FileNames,AccessData);
	noOverRide ->
	    {accessData,AccessData};
	error->
	    ets:delete(AccessData),
	    {error,errorInAccessFile}
    end.

%----------------------------------------------------------------------
%opens the filehandle to the specified file
%----------------------------------------------------------------------
loadAccessFileData({file,FileName},AccessData)->
    case file:open(FileName,[read]) of
        {ok,AccessFileHandle}->
	    loadAccessFileData({stream,AccessFileHandle},AccessData,[]);
        {error,Reason} ->
	    overRide
    end.

%----------------------------------------------------------------------
%%look att each line in the file and add them to the database
%%When end of file is reached control i overrride is allowed
%% if so return
%----------------------------------------------------------------------
loadAccessFileData({stream,File},AccessData,FileData)->
    case io:get_line(File,[]) of
        eof->
	    insertData(AccessData,FileData),
	    case ets:match_object(AccessData,{'_',error}) of
		[]->
		    %Case we got no error control that we can override a
		    %at least some of the values
		    case ets:match_object(AccessData,
					  {allow_over_ride,none}) of
			[]->
			    overRide;
			_NoOverride->
			    noOverRide
		    end;
		Errors->
		    error
	    end;
	Line ->
	    loadAccessFileData({stream,File},AccessData,
			       insertLine(string:strip(Line,left),FileData))
    end.

%----------------------------------------------------------------------
%AccessData is a ets table where the previous found data is inserted
%FileData is a list of the directives in the last parsed file
%before insertion a control is done that the directive is allowed to
%override
%----------------------------------------------------------------------
insertData(AccessData,{{context,Values},FileData})->
    insertData(AccessData,[{context,Values}|FileData]);

insertData(AccessData,FileData)->
    case ets:lookup(AccessData,allow_over_ride) of
	[{allow_over_ride,all}]->
	    lists:foreach(fun(Elem)->
				  ets:insert(AccessData,Elem)
			  end,FileData);
	[]->
	    lists:foreach(fun(Elem)->
				  ets:insert(AccessData,Elem)
			  end,FileData);
	[{allow_over_ride,Directives}]when list(Directives)->
	    lists:foreach(fun({Key,Value})->
				  case lists:member(Key,Directives) of
				      true->
					  ok;
				      false ->
					  ets:insert(AccessData,{Key,Value})
				  end
			  end,FileData);
	[{allow_over_ride,_}]->
	    %Will never appear if the user
	    %aint doing very strang econfig files
	    ok
    end.
%----------------------------------------------------------------------
%Take a line in the accessfile and transform it into a tuple that
%later can be inserted in to the ets:table
%----------------------------------------------------------------------
%%%Here is the alternatives that resides inside the limit context

insertLine([$o,$r,$d,$e,$r|Order],{{context,Values},FileData})->
    {{context,[{order,getOrder(Order)}|Values]},FileData};
%%Let the user place a tab in the beginning
insertLine([$\t,$o,$r,$d,$e,$r|Order],{{context,Values},FileData})->
    {{context,[{order,getOrder(Order)}|Values]},FileData};

insertLine([$a,$l,$l,$o,$w|Allow],{{context,Values},FileData})->
    {{context,[{allow,getAllowDenyData(Allow)}|Values]},FileData};
insertLine([$\t,$a,$l,$l,$o,$w|Allow],{{context,Values},FileData})->
    {{context,[{allow,getAllowDenyData(Allow)}|Values]},FileData};

insertLine([$d,$e,$n,$y|Deny],{{context,Values},FileData})->
    {{context,[{deny,getAllowDenyData(Deny)}|Values]},FileData};
insertLine([$\t,$d,$e,$n,$y|Deny],{{context,Values},FileData})->
    {{context,[{deny,getAllowDenyData(Deny)}|Values]},FileData};


insertLine([$r,$e,$q,$u,$i,$r,$e|Require],{{context,Values},FileData})->
    {{context,[{require,getRequireData(Require)}|Values]},FileData};
insertLine([$\t,$r,$e,$q,$u,$i,$r,$e|Require],{{context,Values},FileData})->
    {{context,[{require,getRequireData(Require)}|Values]},FileData};


insertLine([$<,$/,$L,$i,$m,$i,$t|EndLimit],{Context,FileData})->
    [Context|FileData];

insertLine([$<,$L,$i,$m,$i,$t|Limit],FileData)->
    {{context,[{limit,getLimits(Limit)}]}, FileData};



insertLine([$A,$u,$t,$h,$U,$s,$e,$r,$F,$i,$l,$e,$\ |AuthUserFile],FileData)->
    [{user_file,string:strip(AuthUserFile,right,$\n)}|FileData];

insertLine([$A,$u,$t,$h,$G,$r,$o,$u,$p,$F,$i,$l,$e,$\ |AuthGroupFile],
	   FileData)->
    [{group_file,string:strip(AuthGroupFile,right,$\n)}|FileData];

insertLine([$A,$l,$l,$o,$w,$O,$v,$e,$r,$R,$i,$d,$e|AllowOverRide],FileData)->
    [{allow_over_ride,getAllowOverRideData(AllowOverRide)}
     |FileData];

insertLine([$A,$u,$t,$h,$N,$a,$m,$e,$\ |AuthName],FileData)->
    [{auth_name,string:strip(AuthName,right,$\n)}|FileData];

insertLine([$A,$u,$t,$h,$T,$y,$p,$e|AuthType],FileData)->
    [{auth_type,getAuthorizationType(AuthType)}|FileData];

insertLine(_BadDirectiveOrComment,FileData)->
    FileData.

%----------------------------------------------------------------------
%transform the Data specified about override to a form that is ieasier
%handled later
%Override data="all"|"md5"|"Directive1 .... DirectioveN"
%----------------------------------------------------------------------

getAllowOverRideData(OverRideData)->
   case string:tokens(OverRideData," \r\n") of
       [[$a,$l,$l]|_]->
	   all;
        [[$n,$o,$n,$e]|_]->
	   none;
       Directives ->
	   getOverRideDirectives(Directives)
   end.

getOverRideDirectives(Directives)->
    lists:map(fun(Directive)->
		      transformDirective(Directive)
	      end,Directives).
transformDirective([$A,$u,$t,$h,$U,$s,$e,$r,$F,$i,$l,$e|_])->
    user_file;
transformDirective([$A,$u,$t,$h,$G,$r,$o,$u,$p,$F,$i,$l,$e|_]) ->
    group_file;
transformDirective([$A,$u,$t,$h,$N,$a,$m,$e|_])->
    auth_name;
transformDirective([$A,$u,$t,$h,$T,$y,$p,$e|_])->
    auth_type;
transformDirective(_UnAllowedOverRideDirective) ->
    unallowed.
%----------------------------------------------------------------------
%Replace the string that specify which method to use for authentication
%and replace it with the atom for easier mathing
%----------------------------------------------------------------------
getAuthorizationType(AuthType)->
    [Arg|Crap]=string:tokens(AuthType,"\n\r\ "),
    case Arg of
	[$B,$a,$s,$i,$c]->
	    basic;
	[$M,$D,$5] ->
	    md5;
	_What ->
	    error
    end.
%----------------------------------------------------------------------
%Returns a list of the specified methods to limit or the atom all
%----------------------------------------------------------------------
getLimits(Limits)->
    case regexp:split(Limits,">")of
	{ok,[_NoEndOnLimit]}->
	    error;
	{ok,[Methods|Crap]}->
	    case regexp:split(Methods," ")of
		{ok,[]}->
		    all;
		{ok,SplittedMethods}->
		    SplittedMethods;
		{error,Error}->
		    error
	    end;
	{error,_Error}->
	    error
    end.


%----------------------------------------------------------------------
% Transform the order to prefrom deny allow control to a tuple of atoms
%----------------------------------------------------------------------
getOrder(Order)->
    [First|Rest]=lists:map(fun(Part)->
		      list_to_atom(Part)
	      end,string:tokens(Order," \n\r")),
    case First of
	deny->
	    {deny,allow};
	allow->
	    {allow,deny};
	_Error->
	    error
    end.

%----------------------------------------------------------------------
% The string AllowDeny is "from all" or "from Subnet1 Subnet2...SubnetN"
%----------------------------------------------------------------------
getAllowDenyData(AllowDeny)->
    case string:tokens(AllowDeny," \n\r") of
	[_From|AllowDenyData] when length(AllowDenyData)>=1->
	    case lists:nth(1,AllowDenyData) of
		[$a,$l,$l]->
		    all;
		Hosts->
		    AllowDenyData
	    end;
	Error->
	    errror
    end.
%----------------------------------------------------------------------
% Fix the string that describes who is allowed to se the page
%----------------------------------------------------------------------
getRequireData(Require)->
    [UserOrGroup|UserData]=string:tokens(Require," \n\r"),
    case UserOrGroup of
	[$u,$s,$e,$r]->
	    {users,UserData};
	[$g,$r,$o,$u,$p] ->
	    {groups,UserData};
	_Whatever ->
	    error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Methods that collects the searchways to the accessfiles          %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% Get the whole path to the different accessfiles
%----------------------------------------------------------------------
getHtAccessFiles(HtAccessFileNames,Path,RestOfSplittedPath)->
    getHtAccessFiles(HtAccessFileNames,Path,RestOfSplittedPath,[]).

getHtAccessFiles(HtAccessFileNames,Path,[[]],HtAccessFiles)->
    HtAccessFiles ++ accessFilesOfPath(HtAccessFileNames,Path++"/");

getHtAccessFiles(HtAccessFileNames,Path,[],HtAccessFiles)->
    HtAccessFiles;
getHtAccessFiles(HtAccessFileNames,Path,[NextDir|RestOfSplittedPath],
		 AccessFiles)->
    getHtAccessFiles(HtAccessFileNames,Path++"/"++NextDir,RestOfSplittedPath,
		     AccessFiles ++
		     accessFilesOfPath(HtAccessFileNames,Path++"/")).


%----------------------------------------------------------------------
%Control if therer are any accessfies in the path
%----------------------------------------------------------------------
accessFilesOfPath(HtAccessFileNames,Path)->
    lists:foldl(fun(HtAccessFileName,Files)->
			case file:read_file_info(Path++HtAccessFileName) of
			    {ok,FileInfo}->
				[Path++HtAccessFileName|Files];
			    {error,_Error} ->
				Files
			end
		end,[],HtAccessFileNames).


%----------------------------------------------------------------------
%Sake the splitted path and joins it up to the documentroot or the alias
%that match first
%----------------------------------------------------------------------

getRootPath(SplittedPath,Info)->
    DocRoot=httpd_util:lookup(Info#mod.config_db,document_root,"/"),
    PresumtiveRootPath=
	[DocRoot|lists:map(fun({Alias,RealPath})->
				   RealPath
			   end,
		 httpd_util:multi_lookup(Info#mod.config_db,alias))],
    getRootPath(PresumtiveRootPath,SplittedPath,Info).


getRootPath(PresumtiveRootPath,[[],Splittedpath],Info)->
    getRootPath(PresumtiveRootPath,["/",Splittedpath],Info);


getRootPath(PresumtiveRootPath,[Part,NextPart|SplittedPath],Info)->
    case lists:member(Part,PresumtiveRootPath)of
	true->
	    {ok,Part,[NextPart|SplittedPath]};
	false ->
	    getRootPath(PresumtiveRootPath,
			[Part++"/"++NextPart|SplittedPath],Info)
    end;

getRootPath(PresumtiveRootPath,[Part],Info)->
    case lists:member(Part,PresumtiveRootPath)of
	true->
	    {ok,Part,[]};
	false ->
	    {error,Part}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Debug methods                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
% Simulate the webserver by calling do/1 with apropiate parameters
%----------------------------------------------------------------------
debug()->
    Conf=getConfigData(),
    Uri=getUri(),
    {_Proceed,Data}=getDataFromAlias(Conf,Uri),
    Init_data=#init_data{peername={socket,"127.0.0.1"}},
    ParsedHeader=headerparts(),
    do(#mod{init_data=Init_data,
	    data=Data,
	    config_db=Conf,
	    request_uri=Uri,
	    parsed_header=ParsedHeader,
	   method="GET"}).

%----------------------------------------------------------------------
%Add authenticate data to the fake http-request header
%----------------------------------------------------------------------
headerparts()->
    [{"authorization","Basic " ++ httpd_util:encode_base64("lotta:potta")}].

getDataFromAlias(Conf,Uri)->
    mod_alias:do(#mod{config_db=Conf,request_uri=Uri}).

getUri()->
    "/appmon/test/test.html".

getConfigData()->
    Tab=ets:new(test_inets,[bag,public]),
    ets:insert(Tab,{server_name,"localhost"}),
    ets:insert(Tab,{bind_addresss,{127,0,0,1}}),
    ets:insert(Tab,{erl_script_alias,{"/webcover/erl",["webcover"]}}),
    ets:insert(Tab,{erl_script_alias,{"/erl",["webappmon"]}}),
    ets:insert(Tab,{com_type,ip_comm}),
    ets:insert(Tab,{modules,[mod_alias,mod_auth,mod_header]}),
    ets:insert(Tab,{default_type,"text/plain"}),
    ets:insert(Tab,{server_root,
		    "/home/gandalf/marting/exjobb/webtool-1.0/priv/root"}),
    ets:insert(Tab,{port,8888}),
    ets:insert(Tab,{document_root,
		    "/home/gandalf/marting/exjobb/webtool-1.0/priv/root"}),
    ets:insert(Tab,
	       {alias,
		{"/appmon"
		 ,"/home/gandalf/marting/exjobb/webappmon-1.0/priv"}}),
    ets:insert(Tab,{alias,
		    {"/webcover"
		     ,"/home/gandalf/marting/exjobb/webcover-1.0/priv"}}),
    ets:insert(Tab,{access_file,[".htaccess","kalle","pelle"]}),
    Tab.
