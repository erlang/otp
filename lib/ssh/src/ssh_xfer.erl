%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

%%% Description: SFTP functions

-module(ssh_xfer).

-export([open/6, opendir/3, readdir/3, close/3, read/5, write/5,
	 rename/5, remove/3, mkdir/4, rmdir/3, realpath/3, extended/4,
	 stat/4, fstat/4, lstat/4, setstat/4,
	 readlink/3, fsetstat/4, symlink/4,
	 protocol_version_request/2,
	 xf_reply/2,
	 xf_send_reply/3, xf_send_names/3, xf_send_name/4,
	 xf_send_status/3, xf_send_status/4, xf_send_status/5,
	 xf_send_handle/3, xf_send_attr/3, xf_send_data/3,
	 encode_erlang_status/1,
	 decode_open_flags/2, encode_open_flags/1,
	 decode_ace_mask/1, decode_ext/1,
	 decode_ATTR/2, encode_ATTR/2]).

-include("ssh.hrl").
-include("ssh_xfer.hrl").

-import(lists, [foldl/3, reverse/1]).

-define(is_set(F, Bits),
	((F) band (Bits)) == (F)).

protocol_version_request(XF, Version) ->
    xf_request(XF, ?SSH_FXP_INIT, <<?UINT32(Version)>>).

open(XF, ReqID, FileName, Access, Flags, Attrs) -> 
    Vsn = XF#ssh_xfer.vsn,
    MBits = if Vsn >= 5 -> 
		    M = encode_ace_mask(Access),
		    ?uint32(M);
	       true ->
		    (<<>>)
	    end,
    F = encode_open_flags(Flags),
    xf_request(XF,?SSH_FXP_OPEN, 
	       [?uint32(ReqID),
		?string_utf8(FileName),
		MBits,
		?uint32(F),
		encode_ATTR(Vsn,Attrs)]).    
    
opendir(XF, ReqID, DirName) ->
    xf_request(XF, ?SSH_FXP_OPENDIR, 
	       [?uint32(ReqID),
		?string_utf8(DirName)]).


close(XF, ReqID, Handle) ->
    xf_request(XF, ?SSH_FXP_CLOSE,
	       [?uint32(ReqID),
		?binary(Handle)]).

read(XF, ReqID, Handle, Offset, Length) ->
    xf_request(XF, ?SSH_FXP_READ,
	       [?uint32(ReqID),
		?binary(Handle),
		?uint64(Offset),
		?uint32(Length)]).

readdir(XF, ReqID, Handle) ->
    xf_request(XF, ?SSH_FXP_READDIR,
	       [?uint32(ReqID),
		?binary(Handle)]).    

write(XF,ReqID, Handle, Offset, Data) ->
    Data1 = if 
		is_binary(Data) ->
		    Data;
		is_list(Data) -> 
		    unicode:characters_to_binary(Data)
	    end,
    xf_request(XF,?SSH_FXP_WRITE,
	       [?uint32(ReqID),
		?binary(Handle),
		?uint64(Offset),
		?binary(Data1)]).

%% Remove a file
remove(XF, ReqID, File) ->
    xf_request(XF, ?SSH_FXP_REMOVE, 
	       [?uint32(ReqID),
		?string_utf8(File)]).

%% Rename a file/directory
rename(XF, ReqID, OldPath, NewPath, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    FlagBits
	= if Vsn >= 5 ->
		  F0 = encode_rename_flags(Flags),
		  ?uint32(F0);
	     true ->
		  (<<>>)
	  end,
    xf_request(XF, ?SSH_FXP_RENAME, 
	       [?uint32(ReqID),
		?string_utf8(OldPath),
		?string_utf8(NewPath),
		FlagBits]).



%% Create directory
mkdir(XF, ReqID, Path, Attrs) ->
    xf_request(XF, ?SSH_FXP_MKDIR, 
	       [?uint32(ReqID),
		?string_utf8(Path),
		encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).

%% Remove a directory
rmdir(XF, ReqID, Dir) ->
    xf_request(XF, ?SSH_FXP_RMDIR,
	       [?uint32(ReqID),
		?string_utf8(Dir)]).

%% Stat file
stat(XF, ReqID, Path, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    xf_request(XF, ?SSH_FXP_STAT, 
	       [?uint32(ReqID),
		?string_utf8(Path),
		AttrFlags]).


%% Stat file - follow symbolic links
lstat(XF, ReqID, Path, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    xf_request(XF, ?SSH_FXP_LSTAT, 
	     [?uint32(ReqID),
	      ?string_utf8(Path),
	      AttrFlags]).

%% Stat open file
fstat(XF, ReqID, Handle, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    xf_request(XF, ?SSH_FXP_FSTAT, 
	       [?uint32(ReqID),
		?binary(Handle),
		AttrFlags]).

%% Modify file attributes
setstat(XF, ReqID, Path, Attrs) ->
    xf_request(XF, ?SSH_FXP_SETSTAT, 
	       [?uint32(ReqID),
		?string_utf8(Path),
		encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).


%% Modify file attributes
fsetstat(XF, ReqID, Handle, Attrs) ->
    xf_request(XF, ?SSH_FXP_FSETSTAT, 
	       [?uint32(ReqID),
		?binary(Handle),
		encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).
    
%% Read a symbolic link
readlink(XF, ReqID, Path) ->
    xf_request(XF, ?SSH_FXP_READLINK, 
	       [?uint32(ReqID),
		?string_utf8(Path)]).


%% Create a symbolic link    
symlink(XF, ReqID, LinkPath, TargetPath) ->
    LinkPath1 = unicode:characters_to_binary(LinkPath),
    TargetPath1 = unicode:characters_to_binary(TargetPath),
    xf_request(XF, ?SSH_FXP_SYMLINK, 
	       [?uint32(ReqID),
		?binary(LinkPath1),
		?binary(TargetPath1)]).

%% Convert a path into a 'canonical' form
realpath(XF, ReqID, Path) ->
    xf_request(XF, ?SSH_FXP_REALPATH,     
	       [?uint32(ReqID),
		?string_utf8(Path)]).

extended(XF, ReqID, Request, Data) ->
    xf_request(XF, ?SSH_FXP_EXTENDED,
	       [?uint32(ReqID),
		?string(Request),
		?binary(Data)]).


%% Send xfer request to connection manager
xf_request(XF, Op, Arg) ->
    CM = XF#ssh_xfer.cm,
    Channel = XF#ssh_xfer.channel,
    Data = if 
	       is_binary(Arg) -> 
		   Arg;
	       is_list(Arg) ->
		   list_to_binary(Arg)
	   end,
    Size = 1+size(Data),
    ssh_connection:send(CM, Channel, [<<?UINT32(Size), Op, Data/binary>>]).

xf_send_reply(#ssh_xfer{cm = CM, channel = Channel}, Op, Arg) ->    
    Data = if 
	       is_binary(Arg) ->
		   Arg;
	       is_list(Arg) ->
		   list_to_binary(Arg)
	   end,
    Size = 1 + size(Data),
    ssh_connection:send(CM, Channel, [<<?UINT32(Size), Op, Data/binary>>]).

xf_send_name(XF, ReqId, Name, Attr) ->
    xf_send_names(XF, ReqId, [{Name, Attr}]).
					    

xf_send_handle(#ssh_xfer{cm = CM, channel = Channel},
	       ReqId, Handle) ->
    HLen = length(Handle),
    Size = 1 + 4 + 4+HLen,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_HANDLE, ?UINT32(ReqId), ?UINT32(HLen)>>,
	      Handle],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_names(#ssh_xfer{cm = CM, channel = Channel, vsn = Vsn},
	      ReqId, NamesAndAttrs) ->
    Count = length(NamesAndAttrs),
    {Data, Len} = encode_names(Vsn, NamesAndAttrs),
    Size = 1 + 4 + 4 + Len,
    ToSend = [<<?UINT32(Size), 
		?SSH_FXP_NAME, 
		?UINT32(ReqId),
		?UINT32(Count)>>,
	      Data],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_status(XF, ReqId, ErrorCode) ->
    xf_send_status(XF, ReqId, ErrorCode, "").

xf_send_status(XF, ReqId, ErrorCode, ErrorMsg) ->
    xf_send_status(XF, ReqId, ErrorCode, ErrorMsg, <<>>).

xf_send_status(#ssh_xfer{cm = CM, channel = Channel},
	       ReqId, ErrorCode, ErrorMsg, Data) ->
    LangTag = "en",
    ELen = length(ErrorMsg),
    TLen = 2, %% length(LangTag),
    Size = 1 + 4 + 4 + 4+ELen + 4+TLen + size(Data),
    ToSend = [<<?UINT32(Size), ?SSH_FXP_STATUS, ?UINT32(ReqId),
	       ?UINT32(ErrorCode)>>,
	      <<?UINT32(ELen)>>, ErrorMsg,
	      <<?UINT32(TLen)>>, LangTag,
	      Data],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_attr(#ssh_xfer{cm = CM, channel = Channel, vsn = Vsn}, ReqId, Attr) ->
    EncAttr = encode_ATTR(Vsn, Attr),
    ALen = size(EncAttr),
    Size = 1 + 4 + ALen,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_ATTRS, ?UINT32(ReqId)>>, EncAttr],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_data(#ssh_xfer{cm = CM, channel = Channel}, ReqId, Data) ->
    DLen = size(Data),
    Size = 1 + 4 + 4+DLen,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_DATA, ?UINT32(ReqId), ?UINT32(DLen)>>,
	      Data],
    ssh_connection:send(CM, Channel, ToSend).    

xf_reply(_XF, << ?SSH_FXP_STATUS, ?UINT32(ReqID), ?UINT32(Status), 
	      ?UINT32(ELen), Err:ELen/binary,
	      ?UINT32(LLen), Lang:LLen/binary,
	      Reply/binary >> ) ->
    Stat = decode_status(Status),
    {status, ReqID, {Stat,binary_to_list(Err),binary_to_list(Lang),
		     Reply}};
xf_reply(_XF, << ?SSH_FXP_STATUS, ?UINT32(ReqID), ?UINT32(Status)>> ) ->
    Stat = decode_status(Status),
    {status, ReqID, {Stat,"","",<<>>}};
xf_reply(_XF, <<?SSH_FXP_HANDLE, ?UINT32(ReqID),
	      ?UINT32(HLen), Handle:HLen/binary>>) ->
    {handle, ReqID, Handle};
xf_reply(_XF, <<?SSH_FXP_DATA, ?UINT32(ReqID),
	      ?UINT32(DLen), Data:DLen/binary>>) ->
    {data, ReqID, Data};
xf_reply(XF, <<?SSH_FXP_NAME, ?UINT32(ReqID),
	      ?UINT32(Count), AData/binary>>) ->
    {name, ReqID, decode_names(XF#ssh_xfer.vsn, Count, AData)};
xf_reply(XF, <<?SSH_FXP_ATTRS, ?UINT32(ReqID),
	      AData/binary>>) ->
    {A, _} = decode_ATTR(XF#ssh_xfer.vsn, AData),
    {attrs, ReqID, A};
xf_reply(_XF, <<?SSH_FXP_EXTENDED_REPLY, ?UINT32(ReqID),
	      RData>>) ->
    {extended_reply, ReqID, RData}.



decode_status(Status) ->
    case Status of
	?SSH_FX_OK -> ok;
	?SSH_FX_EOF -> eof;
	?SSH_FX_NO_SUCH_FILE -> no_such_file;
	?SSH_FX_PERMISSION_DENIED -> permission_denied;
	?SSH_FX_FAILURE -> failure;
	?SSH_FX_BAD_MESSAGE -> bad_message;
	?SSH_FX_NO_CONNECTION -> no_connection;
	?SSH_FX_CONNECTION_LOST -> connection_lost;
	?SSH_FX_OP_UNSUPPORTED -> op_unsupported;
	?SSH_FX_INVALID_HANDLE -> invalid_handle;
	?SSH_FX_NO_SUCH_PATH -> no_such_path;
	?SSH_FX_FILE_ALREADY_EXISTS -> file_already_exists;
	?SSH_FX_WRITE_PROTECT -> write_protect;
	?SSH_FX_NO_MEDIA -> no_media;
	?SSH_FX_NO_SPACE_ON_FILESYSTEM -> no_space_on_filesystem;
	?SSH_FX_QUOTA_EXCEEDED -> quota_exceeded;
	?SSH_FX_UNKNOWN_PRINCIPLE -> unknown_principle;
	?SSH_FX_LOCK_CONFlICT -> lock_conflict;
	?SSH_FX_NOT_A_DIRECTORY -> not_a_directory;
	?SSH_FX_FILE_IS_A_DIRECTORY -> file_is_a_directory;
	?SSH_FX_CANNOT_DELETE -> cannot_delete;
	_ -> {error,Status}
    end.

encode_erlang_status(Status) ->
    case Status of
	ok -> ?SSH_FX_OK;
	eof -> ?SSH_FX_EOF;
	enoent -> ?SSH_FX_NO_SUCH_FILE;
	eacces -> ?SSH_FX_PERMISSION_DENIED;
	eisdir -> ?SSH_FX_FILE_IS_A_DIRECTORY;
	eperm -> ?SSH_FX_CANNOT_DELETE;
	eexist -> ?SSH_FX_FILE_ALREADY_EXISTS;
	_ -> ?SSH_FX_FAILURE
    end.

decode_ext(<<?UINT32(NameLen), Name:NameLen/binary,
	    ?UINT32(DataLen), Data:DataLen/binary,
	    Tail/binary>>) ->
    [{binary_to_list(Name), binary_to_list(Data)}
     | decode_ext(Tail)];
decode_ext(<<>>) ->
    [].

%%
%% Encode rename flags
%%
encode_rename_flags(Flags) ->
    encode_bits(
      fun(overwrite) -> ?SSH_FXP_RENAME_OVERWRITE;
	 (atomic) -> ?SSH_FXP_RENAME_ATOMIC;
	 (native) -> ?SSH_FXP_RENAME_NATIVE
      end, Flags).

%% decode_rename_flags(F) ->
%%     decode_bits(F,
%% 		[{?SSH_FXP_RENAME_OVERWRITE, overwrite},
%% 		 {?SSH_FXP_RENAME_ATOMIC, atomic},
%% 		 {?SSH_FXP_RENAME_NATIVE, native}]).
    

encode_open_flags(Flags) ->
    encode_bits(
      fun (read) -> ?SSH_FXF_READ;
	  (write) -> ?SSH_FXF_WRITE;
	  (append) -> ?SSH_FXF_APPEND;
	  (creat) -> ?SSH_FXF_CREAT;
	  (trunc)  -> ?SSH_FXF_TRUNC;
	  (excl)   -> ?SSH_FXF_EXCL;
	  (create_new) -> ?SSH_FXF_CREATE_NEW;
	  (create_truncate) -> ?SSH_FXF_CREATE_TRUNCATE;
	  (open_existing) -> ?SSH_FXF_OPEN_EXISTING;
	  (open_or_create) -> ?SSH_FXF_OPEN_OR_CREATE;
	  (truncate_existing) -> ?SSH_FXF_TRUNCATE_EXISTING;
	  (append_data) -> ?SSH_FXF_ACCESS_APPEND_DATA;
	  (append_data_atomic) -> ?SSH_FXF_ACCESS_APPEND_DATA_ATOMIC;
	  (text_mode) -> ?SSH_FXF_ACCESS_TEXT_MODE;
	  (read_lock) -> ?SSH_FXF_ACCESS_READ_LOCK;
	  (write_lock) -> ?SSH_FXF_ACCESS_WRITE_LOCK;
	  (delete_lock) -> ?SSH_FXF_ACCESS_DELETE_LOCK
      end, Flags).

encode_ace_mask(Access) ->
    encode_bits(
      fun(read_data) -> ?ACE4_READ_DATA;
	 (list_directory) -> ?ACE4_LIST_DIRECTORY;
	 (write_data) -> ?ACE4_WRITE_DATA;
	 (add_file) -> ?ACE4_ADD_FILE;
	 (append_data) -> ?ACE4_APPEND_DATA;
	 (add_subdirectory) -> ?ACE4_ADD_SUBDIRECTORY;
	 (read_named_attrs) -> ?ACE4_READ_NAMED_ATTRS;
	 (write_named_attrs) -> ?ACE4_WRITE_NAMED_ATTRS;
	 (execute) -> ?ACE4_EXECUTE;
	 (delete_child) -> ?ACE4_DELETE_CHILD;
	 (read_attributes) -> ?ACE4_READ_ATTRIBUTES;
	 (write_attributes) -> ?ACE4_WRITE_ATTRIBUTES;
	 (delete) -> ?ACE4_DELETE;
	 (read_acl) -> ?ACE4_READ_ACL;
	 (write_acl) -> ?ACE4_WRITE_ACL;
	 (write_owner) -> ?ACE4_WRITE_OWNER;
	 (synchronize) -> ?ACE4_SYNCHRONIZE
      end, Access).

decode_ace_mask(F) ->
    decode_bits(F,
		[
		 {?ACE4_READ_DATA, read_data},
		 {?ACE4_LIST_DIRECTORY, list_directory},
		 {?ACE4_WRITE_DATA, write_data},
		 {?ACE4_ADD_FILE, add_file},
		 {?ACE4_APPEND_DATA, append_data},
		 {?ACE4_ADD_SUBDIRECTORY, add_subdirectory},
		 {?ACE4_READ_NAMED_ATTRS, read_named_attrs},
		 {?ACE4_WRITE_NAMED_ATTRS, write_named_attrs},
		 {?ACE4_EXECUTE, execute},
		 {?ACE4_DELETE_CHILD, delete_child},
		 {?ACE4_READ_ATTRIBUTES, read_attributes},
		 {?ACE4_WRITE_ATTRIBUTES, write_attributes},
		 {?ACE4_DELETE, delete},
		 {?ACE4_READ_ACL, read_acl},
		 {?ACE4_WRITE_ACL, write_acl},
		 {?ACE4_WRITE_OWNER, write_owner},
		 {?ACE4_SYNCHRONIZE, synchronize}
		]).

decode_open_flags(Vsn, F) when Vsn =< 3 ->
    decode_bits(F,
		[
		 {?SSH_FXF_READ, read},
		 {?SSH_FXF_WRITE, write},
		 {?SSH_FXF_APPEND, append},
		 {?SSH_FXF_CREAT, creat},
		 {?SSH_FXF_TRUNC, trunc},
		 {?SSH_FXF_EXCL, excl}
		 ]);
decode_open_flags(Vsn, F) when Vsn >= 4 ->
    R = decode_bits(F,
		    [
		     {?SSH_FXF_ACCESS_APPEND_DATA, append_data},
		     {?SSH_FXF_ACCESS_APPEND_DATA_ATOMIC, append_data_atomic},
		     {?SSH_FXF_ACCESS_TEXT_MODE, text_mode},
		     {?SSH_FXF_ACCESS_READ_LOCK, read_lock},
		     {?SSH_FXF_ACCESS_WRITE_LOCK, write_lock},
		     {?SSH_FXF_ACCESS_DELETE_LOCK, delete_lock}
		    ]),
    AD = case F band ?SSH_FXF_ACCESS_DISPOSITION of
	     ?SSH_FXF_CREATE_NEW -> create_new;
	     ?SSH_FXF_CREATE_TRUNCATE -> create_truncate;
	     ?SSH_FXF_OPEN_EXISTING -> open_existing;
	     ?SSH_FXF_OPEN_OR_CREATE -> open_or_create;
	     ?SSH_FXF_TRUNCATE_EXISTING -> truncate_existing
	 end,
    [AD | R].    

encode_ace_type(Type) ->
    case Type of
	access_allowed -> ?ACE4_ACCESS_ALLOWED_ACE_TYPE;
	access_denied  -> ?ACE4_ACCESS_DENIED_ACE_TYPE;
	system_audit   -> ?ACE4_SYSTEM_AUDIT_ACE_TYPE;
	system_alarm   -> ?ACE4_SYSTEM_ALARM_ACE_TYPE
    end.

decode_ace_type(F) ->
    case F of
	?ACE4_ACCESS_ALLOWED_ACE_TYPE -> access_allowed;
	?ACE4_ACCESS_DENIED_ACE_TYPE -> access_denied;
	?ACE4_SYSTEM_AUDIT_ACE_TYPE -> system_audit;
	?ACE4_SYSTEM_ALARM_ACE_TYPE -> system_alarm
    end.

encode_ace_flag(Flag) ->
    encode_bits(
      fun(file_inherit) -> ?ACE4_FILE_INHERIT_ACE;
	 (directory_inherit) -> ?ACE4_DIRECTORY_INHERIT_ACE;
	 (no_propagte_inherit) -> ?ACE4_NO_PROPAGATE_INHERIT_ACE;
	 (inherit_only) -> ?ACE4_INHERIT_ONLY_ACE;
	 (successful_access) -> ?ACE4_SUCCESSFUL_ACCESS_ACE_FLAG;
	 (failed_access) -> ?ACE4_FAILED_ACCESS_ACE_FLAG;
	 (identifier_group) -> ?ACE4_IDENTIFIER_GROUP
      end, Flag).

decode_ace_flag(F) ->
    decode_bits(F,
		[
		 {?ACE4_FILE_INHERIT_ACE, file_inherit},
		 {?ACE4_DIRECTORY_INHERIT_ACE, directory_inherit},
		 {?ACE4_NO_PROPAGATE_INHERIT_ACE, no_propagte_inherit},
		 {?ACE4_INHERIT_ONLY_ACE, inherit_only},
		 {?ACE4_SUCCESSFUL_ACCESS_ACE_FLAG, successful_access},
		 {?ACE4_FAILED_ACCESS_ACE_FLAG, failed_access},
		 {?ACE4_IDENTIFIER_GROUP, identifier_group}
		]).

encode_attr_flags(Vsn, all) ->
    encode_attr_flags(Vsn,
		      [size, uidgid, permissions,
		       acmodtime, accesstime, createtime,
		       modifytime, acl, ownergroup, subsecond_times,
		       bits, extended]);
encode_attr_flags(Vsn, Flags) ->
    encode_bits(
      fun(size) -> ?SSH_FILEXFER_ATTR_SIZE;
	 (uidgid) when Vsn =<3 -> ?SSH_FILEXFER_ATTR_UIDGID;
	 (permissions) -> ?SSH_FILEXFER_ATTR_PERMISSIONS;
	 (acmodtime) when Vsn =< 3 -> ?SSH_FILEXFER_ATTR_ACMODTIME;
	 (accesstime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_ACCESSTIME;
	 (createtime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_CREATETIME;
	 (modifytime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_MODIFYTIME;
	 (acl) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_ACL;
	 (ownergroup) when  Vsn >= 5 -> ?SSH_FILEXFER_ATTR_OWNERGROUP;
	 (subsecond_times) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_SUBSECOND_TIMES;
	 (bits) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_BITS;
	 (extended) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_EXTENDED;
	 (_) -> 0
      end, Flags).

encode_file_type(Type) ->
    case Type of
	regular -> ?SSH_FILEXFER_TYPE_REGULAR;
	directory -> ?SSH_FILEXFER_TYPE_DIRECTORY;
	symlink -> ?SSH_FILEXFER_TYPE_SYMLINK;
	special -> ?SSH_FILEXFER_TYPE_SPECIAL;
	unknown -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	other -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	socket -> ?SSH_FILEXFER_TYPE_SOCKET;
	char_device -> ?SSH_FILEXFER_TYPE_CHAR_DEVICE;
	block_device -> ?SSH_FILEXFER_TYPE_BLOCK_DEVICE;
	fifo -> ?SSH_FILEXFER_TYPE_FIFO;
	undefined -> ?SSH_FILEXFER_TYPE_UNKNOWN
    end.

decode_file_type(Type) ->
    case Type of
	?SSH_FILEXFER_TYPE_REGULAR -> regular;
	?SSH_FILEXFER_TYPE_DIRECTORY -> directory;
	?SSH_FILEXFER_TYPE_SYMLINK -> symlink;
	?SSH_FILEXFER_TYPE_SPECIAL -> special;
	?SSH_FILEXFER_TYPE_UNKNOWN -> other; % unknown
	?SSH_FILEXFER_TYPE_SOCKET -> socket;
	?SSH_FILEXFER_TYPE_CHAR_DEVICE -> char_device;
	?SSH_FILEXFER_TYPE_BLOCK_DEVICE -> block_device;
	?SSH_FILEXFER_TYPE_FIFO -> fifo
    end.

encode_attrib_bits(Bits) ->
    encode_bits(
      fun(readonly) -> ?SSH_FILEXFER_ATTR_FLAGS_READONLY;
	 (system) -> ?SSH_FILEXFER_ATTR_FLAGS_SYSTEM;
	 (hidden) -> ?SSH_FILEXFER_ATTR_FLAGS_HIDDEN;
	 (case_insensitive) -> ?SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE;
	 (arcive) -> ?SSH_FILEXFER_ATTR_FLAGS_ARCHIVE;
	 (encrypted) -> ?SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED;
	 (compressed) -> ?SSH_FILEXFER_ATTR_FLAGS_COMPRESSED;
	 (sparse) -> ?SSH_FILEXFER_ATTR_FLAGS_SPARSE;
	 (append_only) -> ?SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY;
	 (immutable) -> ?SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE;
	 (sync) -> ?SSH_FILEXFER_ATTR_FLAGS_SYNC
      end, Bits).

decode_attrib_bits(F) ->
    decode_bits(F,
		[{?SSH_FILEXFER_ATTR_FLAGS_READONLY, readonly},
		 {?SSH_FILEXFER_ATTR_FLAGS_SYSTEM, system},
		 {?SSH_FILEXFER_ATTR_FLAGS_HIDDEN, hidden},
		 {?SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE, case_insensitive},
		 {?SSH_FILEXFER_ATTR_FLAGS_ARCHIVE, arcive},
		 {?SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED, encrypted},
		 {?SSH_FILEXFER_ATTR_FLAGS_COMPRESSED, compressed},
		 {?SSH_FILEXFER_ATTR_FLAGS_SPARSE, sparse},
		 {?SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY, append_only},
		 {?SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE, immutable},
		 {?SSH_FILEXFER_ATTR_FLAGS_SYNC, sync}]).


%% 
%% Encode file attributes
%% 
encode_ATTR(Vsn, A) ->
    {Flags,As} =
	encode_As(Vsn, 
		  [{size, A#ssh_xfer_attr.size},
		   {ownergroup, A#ssh_xfer_attr.owner},
		   {ownergroup, A#ssh_xfer_attr.group},
		   {permissions, A#ssh_xfer_attr.permissions},
		   {acmodtime, A#ssh_xfer_attr.atime},
		   {acmodtime, A#ssh_xfer_attr.mtime},
		   {accesstime,  A#ssh_xfer_attr.atime},
		   {subsecond_times, A#ssh_xfer_attr.atime_nseconds},
		   {createtime,  A#ssh_xfer_attr.createtime},
		   {subsecond_times, A#ssh_xfer_attr.createtime_nseconds},
		   {modifytime,  A#ssh_xfer_attr.mtime},
		   {subsecond_times, A#ssh_xfer_attr.mtime_nseconds},
		   {acl, A#ssh_xfer_attr.acl},
		   {bits, A#ssh_xfer_attr.attrib_bits},
		   {extended, A#ssh_xfer_attr.extensions}],
		  0, []),
    Type = encode_file_type(A#ssh_xfer_attr.type),
    Result = list_to_binary([?uint32(Flags),
			     if Vsn >= 5 ->
				     ?byte(Type);
				true ->
				     (<<>>)
			     end, As]),
    Result.


encode_As(Vsn, [{_AName, undefined}|As], Flags, Acc) ->
    encode_As(Vsn, As, Flags, Acc);
encode_As(Vsn, [{AName, X}|As], Flags, Acc) ->
    case AName of
	size ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_SIZE,
		      [?uint64(X) | Acc]);
	ownergroup when Vsn=<4 ->
	     encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_UIDGID,
		       [?uint32(X) | Acc]);
	ownergroup when Vsn>=5 ->
	    X1 = list_to_binary(integer_to_list(X)), % TODO: check owner and group
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_OWNERGROUP,
		      [?binary(X1) | Acc]);
	permissions ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_PERMISSIONS,
		      [?uint32(X) | Acc]);
	acmodtime when Vsn=<3 ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_ACMODTIME,
		      [?uint32(X) | Acc]);
	accesstime when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_ACCESSTIME,
		      [?uint64(X) | Acc]);
	createtime when Vsn>=5->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_CREATETIME,
		      [?uint64(X) | Acc]);
	modifytime when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_MODIFYTIME,
		      [?uint64(X) | Acc]);
	subsecond_times when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,
		      [?uint64(X) | Acc]);
	acl when Vsn >=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_ACL,
		      [encode_acl(X) | Acc]);
	bits when Vsn>=5 ->
	    F = encode_attrib_bits(X),
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_BITS,
		      [?uint32(F) | Acc]);
	extended ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_EXTENDED,
		      [encode_extensions(X) | Acc]);
	_ ->
	    encode_As(Vsn, As, Flags, Acc)
    end;
encode_As(_Vsn, [], Flags, Acc) ->
    {Flags, reverse(Acc)}.


decode_ATTR(Vsn, <<?UINT32(Flags), Tail/binary>>) ->
    {Type,Tail2} =
	if Vsn =< 3 ->
		{?SSH_FILEXFER_TYPE_UNKNOWN, Tail};
	   true ->
		<<?BYTE(T), TL/binary>> = Tail,
		{T, TL}
	end,
    decode_As(Vsn, 
	      [{size, #ssh_xfer_attr.size},
	       {ownergroup, #ssh_xfer_attr.owner},
	       {ownergroup, #ssh_xfer_attr.group},
	       {permissions, #ssh_xfer_attr.permissions},
	       {acmodtime, #ssh_xfer_attr.atime},
	       {acmodtime, #ssh_xfer_attr.mtime},
	       {accesstime,  #ssh_xfer_attr.atime},
	       {subsecond_times, #ssh_xfer_attr.atime_nseconds},
	       {createtime,  #ssh_xfer_attr.createtime},
	       {subsecond_times, #ssh_xfer_attr.createtime_nseconds},
	       {modifytime,  #ssh_xfer_attr.mtime},
	       {subsecond_times, #ssh_xfer_attr.mtime_nseconds},
	       {acl, #ssh_xfer_attr.acl},
	       {bits, #ssh_xfer_attr.attrib_bits},
	       {extended, #ssh_xfer_attr.extensions}],
	      #ssh_xfer_attr { type = decode_file_type(Type) },
	      Flags,
	      Tail2).

decode_As(Vsn, [{AName, AField}|As], R, Flags, Tail) ->
    case AName of
	size when ?is_set(?SSH_FILEXFER_ATTR_SIZE, Flags) ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	ownergroup when ?is_set(?SSH_FILEXFER_ATTR_UIDGID, Flags),Vsn=<3 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	ownergroup when ?is_set(?SSH_FILEXFER_ATTR_OWNERGROUP, Flags),Vsn>=5 ->
	    <<?UINT32(Len), Bin:Len/binary, Tail2/binary>> = Tail,
	    X = binary_to_list(Bin),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);

	permissions when ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,Flags),Vsn>=5->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);

	permissions when ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,Flags),Vsn=<3->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    R1 = setelement(AField, R, X),
	    Type = case X band ?S_IFMT of
		       ?S_IFDIR -> directory;
		       ?S_IFCHR -> char_device;
		       ?S_IFBLK -> block_device;
		       ?S_IFIFO -> fifi;
		       ?S_IFREG -> regular;
		       ?S_IFSOCK -> socket;
		       ?S_IFLNK -> symlink;
		       _ -> unknown
		   end,
	    decode_As(Vsn, As, R1#ssh_xfer_attr { type=Type}, Flags, Tail2);

	acmodtime when ?is_set(?SSH_FILEXFER_ATTR_ACMODTIME,Flags),Vsn=<3 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	accesstime when ?is_set(?SSH_FILEXFER_ATTR_ACCESSTIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	modifytime when ?is_set(?SSH_FILEXFER_ATTR_MODIFYTIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	createtime when ?is_set(?SSH_FILEXFER_ATTR_CREATETIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	subsecond_times when ?is_set(?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,Flags),Vsn>=5 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	acl when ?is_set(?SSH_FILEXFER_ATTR_ACL, Flags), Vsn>=5 ->
	    {X,Tail2} = decode_acl(Tail),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	bits when ?is_set(?SSH_FILEXFER_ATTR_BITS, Flags), Vsn >=5 ->
	    <<?UINT32(Y), Tail2/binary>> = Tail,
	    X = decode_attrib_bits(Y),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	extended when ?is_set(?SSH_FILEXFER_ATTR_EXTENDED, Flags) ->
	    {X,Tail2} = decode_extended(Tail),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	_ ->
	    decode_As(Vsn, As, R, Flags, Tail)
    end;
decode_As(_Vsn, [], R, _, Tail) ->
    {R, Tail}.


	

decode_names(_Vsn, 0, _Data) ->
    [];
decode_names(Vsn, I, <<?UINT32(Len), FileName:Len/binary, 
		      ?UINT32(LLen), _LongName:LLen/binary,
		      Tail/binary>>) when Vsn =< 3 ->
    Name = unicode:characters_to_list(FileName),
    {A, Tail2} = decode_ATTR(Vsn, Tail),
    [{Name, A} | decode_names(Vsn, I-1, Tail2)];
decode_names(Vsn, I, <<?UINT32(Len), FileName:Len/binary, 
		      Tail/binary>>) when Vsn >= 4 ->
    Name = unicode:characters_to_list(FileName),
    {A, Tail2} = decode_ATTR(Vsn, Tail),
    [{Name, A} | decode_names(Vsn, I-1, Tail2)].

encode_names(Vsn, NamesAndAttrs) ->
    lists:mapfoldl(fun(N, L) -> encode_name(Vsn, N, L) end, 0, NamesAndAttrs).

encode_name(Vsn, {NameUC,Attr}, Len) when Vsn =< 3 ->
    Name = binary_to_list(unicode:characters_to_binary(NameUC)),
    NLen = length(Name),
    EncAttr = encode_ATTR(Vsn, Attr),
    ALen = size(EncAttr),
    NewLen = Len + NLen*2 + 4 + 4 + ALen,
    {[<<?UINT32(NLen)>>, Name, <<?UINT32(NLen)>>, Name, EncAttr], NewLen};
encode_name(Vsn, {NameUC,Attr}, Len) when Vsn >= 4 ->
    Name = binary_to_list(unicode:characters_to_binary(NameUC)),
    NLen = length(Name),
    EncAttr = encode_ATTR(Vsn, Attr),
    ALen = size(EncAttr),
    {[<<?UINT32(NLen)>>, Name, EncAttr],
     Len + 4 + NLen + ALen}.

encode_acl(ACLList) ->
    Count = length(ACLList),
    [?uint32(Count) | encode_acl_items(ACLList)].

encode_acl_items([ACE|As]) ->
    Type = encode_ace_type(ACE#ssh_xfer_ace.type),
    Flag = encode_ace_flag(ACE#ssh_xfer_ace.flag), 
    Mask = encode_ace_mask(ACE#ssh_xfer_ace.mask), 
    Who = ACE#ssh_xfer_ace.who,
    [?uint32(Type), ?uint32(Flag), ?uint32(Mask), 
     ?string_utf8(Who) | encode_acl_items(As)];
encode_acl_items([]) ->
    [].


decode_acl(<<?UINT32(Count), Tail/binary>>) ->
    decode_acl_items(Count, Tail, []).

decode_acl_items(0, Tail, Acc) -> 
    {reverse(Acc), Tail};
decode_acl_items(I, <<?UINT32(Type), 
	       ?UINT32(Flag),
	       ?UINT32(Mask),
	       ?UINT32(WLen), BWho:WLen/binary,
	       Tail/binary>>, Acc) ->
    decode_acl_items(I-1, Tail,
		     [#ssh_xfer_ace { type = decode_ace_type(Type),
				      flag = decode_ace_flag(Flag),
				      mask = decode_ace_mask(Mask),
				      who = unicode:characters_to_list(BWho)} | Acc]).

encode_extensions(Exts) ->
    Count = length(Exts),
    [?uint32(Count) | encode_ext(Exts)].

encode_ext([{Type, Data} | Exts]) ->
    [?string(Type), ?string(Data) | encode_ext(Exts)];
encode_ext([]) ->
    [].


decode_extended(<<?UINT32(Count), Tail/binary>>) ->     
    decode_ext(Count, Tail, []).

decode_ext(0, Tail, Acc) ->
    {reverse(Acc), Tail};
decode_ext(I, <<?UINT32(TLen), Type:TLen/binary,
	       ?UINT32(DLen), Data:DLen/binary,
	       Tail/binary>>,  Acc) ->
    decode_ext(I-1, Tail, [{binary_to_list(Type), Data}|Acc]).



%% Encode bit encoded flags
encode_bits(Fun, BitNames) ->
    encode_bits(Fun, 0, BitNames).

encode_bits(Fun, F, [Bit|BitNames]) ->
    encode_bits(Fun, Fun(Bit) bor F, BitNames);
encode_bits(_Fun, F, []) ->
    F.

%% Decode bit encoded flags
decode_bits(F, [{Bit,BitName}|Bits]) ->
    if F band Bit == Bit ->
	    [BitName | decode_bits(F, Bits)];
       true ->
	    decode_bits(F, Bits)
    end;
decode_bits(_F, []) ->
    [].
