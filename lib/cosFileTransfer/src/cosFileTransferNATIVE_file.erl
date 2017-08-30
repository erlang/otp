%%----------------------------------------------------------------------
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
%% File    : cosFileTransferNATIVE_file.erl
%% Description : 
%%
%% Created :  9 Nov 2000
%%----------------------------------------------------------------------
-module('cosFileTransferNATIVE_file').


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("cosFileTransferApp.hrl").
-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([open/1,
	 open/2,
	 open/3,
	 user/3,
	 pwd/1,
	 cd/2,
	 mkdir/2,
	 rmdir/2,
	 nlist/1,
	 nlist/2,
	 delete/2,
	 recv/2,
	 recv/3,
	 send/2,
	 send/3,
	 close/1,
	 insert/4]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% function : open
%% Arguments: 
%% Returns  : {ok, Ref} | {error, ehost} for future use
%% Effect   : 
%%----------------------------------------------------------------------
open(_Host) ->
    {ok, 'NATIVE'}.
open(_Host, _Port) ->
    {ok, 'NATIVE'}.
open(_Host, _Port, _Flags) ->
    {ok, 'NATIVE'}.

%%----------------------------------------------------------------------
%% function : user
%% Arguments: Ref - what's returned by open/1/2
%%            User = Password = string()
%% Returns  : ok | {error, euser | econn} for future use
%% Effect   : 
%%----------------------------------------------------------------------
user(_Ref, _User, _Password) ->
    ok.

%%----------------------------------------------------------------------
%% function : pwd
%% Arguments: Ref - what's returned by open/1/2
%% Returns  : {ok, string()} | {error, elogin | econn}
%% Effect   : 
%%----------------------------------------------------------------------
pwd(_Ref) ->
    case file:get_cwd() of
	{ok, Cwd} ->
	    {ok, Cwd};
	_ ->
	    {error, econn}
    end.

%%----------------------------------------------------------------------
%% function : cd
%% Arguments: Ref - what's returned by open/1/2
%%            Dir - string()
%% Returns  : ok | {error, epath | elogin | econn}
%% Effect   : 
%%----------------------------------------------------------------------
cd(_Ref, Dir) ->
    case file:set_cwd(Dir) of
	ok ->
	    ok;
	{error, _} ->
	    {error, epath}
    end.

%%----------------------------------------------------------------------
%% function : mkdir
%% Arguments: Ref - what's returned by open/1/2
%%            Dir - string() 
%% Returns  : ok | {error, epath}
%% Effect   : 
%%----------------------------------------------------------------------
mkdir(_Ref, Dir) ->
    case file:make_dir(Dir) of
	ok ->
	    ok;
	{error, _Reason} ->
	    {error, epath}
    end.
	    
%%----------------------------------------------------------------------
%% function : rmdir
%% Arguments: Ref - what's returned by open/1/2
%%            Dir - string()
%% Returns  : ok | {error, epath}
%% Effect   : 
%%----------------------------------------------------------------------
rmdir(_Ref, Dir) ->
    case file:del_dir(Dir) of
	ok ->
	    ok;
	 {error, _Reason} ->
	    {error, epath}
    end.
	    
%%----------------------------------------------------------------------
%% function : nlist
%% Arguments: Ref - what's returned by open/1/2
%%            Dir - string()
%% Returns  : {ok, Listing} | {error, epath | elogin | econn}
%% Effect   : 
%%----------------------------------------------------------------------
nlist(_Ref) ->
    case file:get_cwd() of
	{ok, Cwd} ->
	    %% Here we can assume that it's a Directory is tested.
	    convert_to_nlist(file:list_dir(Cwd));
	_ ->
	    {error, epath}
    end.
nlist(_Ref, Dir) ->
    case file:list_dir(Dir) of
	{error, _} ->
	    %% Might be a File
	    case file:read_file_info(Dir) of
		{ok, _} ->
		    convert_to_nlist_helper([Dir], []);
		_ ->
		    {error, epath}  
	    end;
	{ok, Content} ->
	    convert_to_nlist_helper(Content, [])
    end.

convert_to_nlist({error, _}) ->
    {error, epath};
convert_to_nlist({ok, Content}) ->
    convert_to_nlist_helper(Content, []).

convert_to_nlist_helper([], Acc) ->
    {ok, lists:concat(Acc)};
convert_to_nlist_helper([H|T], Acc) ->
    convert_to_nlist_helper(T, [H, "\r\n"|Acc]).

%%----------------------------------------------------------------------
%% function : delete
%% Arguments: Ref - what's returned by open/1/2
%%            File - string()
%% Returns  : ok | {error, epath}
%% Effect   : 
%%----------------------------------------------------------------------
delete(_Ref, File) ->
    case file:delete(File) of
	ok ->
	    ok;
	 {error, _Reason} ->
	    {error, epath}
    end.
  
%%----------------------------------------------------------------------
%% function : recv
%% Arguments: Ref - what's returned by open/1/2
%% Returns  : ok | {error, epath | elogin | econn}
%% Effect   : 
%%----------------------------------------------------------------------
recv(_Ref, _Remote) ->
    ok.
recv(_Ref, Remote, Local) ->
    copy_file(Remote, Local).

%%----------------------------------------------------------------------
%% function : send
%% Arguments: Ref - what's returned by open/1/2
%% Returns  : ok | {error, epath | elogin | econn | etnospc | epnospc | efnamena}
%% Effect   : 
%%----------------------------------------------------------------------
send(_Ref, _Local) -> 
    ok.
send(_Ref, Local, Remote) -> 
    copy_file(Local, Remote).

%%----------------------------------------------------------------------
%% function : close
%% Arguments: Ref - what's returned by open/1/2
%% Returns  : ok
%% Effect   : Currently none.
%%----------------------------------------------------------------------
close(_) ->
    ok.


%%----------------------------------------------------------------------
%% function : insert
%% Arguments: Ref    - what's returned by open/1/2
%%            Local  - absolute file name
%%            Remote - absolute file name
%%            Offset - long()
%% Returns  : ok
%% Effect   : 
%%----------------------------------------------------------------------
insert(_Ref, Source, Target, Offset) ->
    case file:open(Source, [raw, binary, read]) of
	{ok, SourceDev} ->
	    case file:open(Target, [raw, binary, read, write]) of
		{ok, TargetDev} ->
		    {ok, #file_info{size=SSize}} = 
			file:read_file_info(Source),
		    {ok, #file_info{size=TSize}} = 
			file:read_file_info(Target),
		    insert_file_helper(SourceDev, TargetDev, SSize, TSize, Offset);
		Reason ->
		    file:close(SourceDev),
		    convert_error(Reason)
	    end;
	Reason ->
	    convert_error(Reason)
    end.
    

insert_file_helper(SourceDev, TargetDev, SSize, TSize, Offset) ->
    BuffSize = cosFileTransferApp:get_buffert_size(),
    move_data(TargetDev, TSize, SSize+TSize, TSize-Offset, BuffSize),
    insert_data(SourceDev, TargetDev, 0, Offset, BuffSize),
    file:close(SourceDev),
    file:close(TargetDev).
move_data(_F, _RLocation, _WLocation, Counter, _BuffSize) when Counter == 0 ->
    ok;
move_data(F, RLocation, WLocation, Counter, BuffSize) when Counter =< BuffSize ->
    case file:pread(F, RLocation-Counter, Counter) of
	{ok, Bin} ->
	    file:pwrite(F, WLocation-Counter, Bin);
	eof ->
	    ok
    end;
move_data(F, RLocation, WLocation, Counter, BuffSize) ->
    NewRLC = RLocation-BuffSize,
    NewWLC = WLocation-BuffSize,
    case file:pread(F, NewRLC, BuffSize) of
	{ok, Bin} ->
	    file:pwrite(F, NewWLC, Bin),
	    move_data(F, NewRLC, NewWLC, Counter-BuffSize, BuffSize);
	eof ->
	    ok
    end.

insert_data(FSRC, FTGT, RLocation, WLocation, BuffSize) ->
    case file:pread(FSRC, RLocation, BuffSize) of
	{ok, Bin} ->
	    file:pwrite(FTGT, WLocation, Bin),
	    insert_data(FSRC, FTGT, RLocation+BuffSize, WLocation+BuffSize, BuffSize);
	eof ->
	    ok
    end.


%%======================================================================
%% Internal functions
%%======================================================================
copy_file(Source, Target) ->
    case file:open(Source, [raw, binary, read]) of
	{ok, SourceDev} ->
	    case file:open(Target, [raw, binary, append]) of
		{ok, TargetDev} ->
		    BuffSize = cosFileTransferApp:get_buffert_size(),
		    copy_file_helper(SourceDev, TargetDev, BuffSize);
		Reason ->
		    file:close(SourceDev),
		    convert_error(Reason)
	    end;
	Reason ->
	    convert_error(Reason)
    end.

copy_file_helper(SourceDev, TargetDev, BuffSize) ->
    case file:read(SourceDev, BuffSize) of
	eof ->
	    file:close(SourceDev),
	    file:close(TargetDev);
	{ok, Bin} ->
	    case  file:write(TargetDev, Bin) of
		ok ->
		    copy_file_helper(SourceDev, TargetDev, BuffSize);
		Reason ->
		    file:close(SourceDev),
		    file:close(TargetDev),
		    convert_error(Reason)
	    end;
	{error, Reason} ->
	    file:close(SourceDev),
	    file:close(TargetDev),
	    convert_error(Reason)
    end.

convert_error({error, eacces}) ->
    {error, elogin};
convert_error({error, enoent}) ->
    {error, epath};
convert_error({error, enotdir}) ->
    {error, epath};
convert_error({error, eidir}) ->
    {error, epath};
convert_error({error, enospc}) ->
    {error, etnospc};
convert_error(_) ->
    {error, epath}.

%%======================================================================
%% END OF MODULE
%%======================================================================
