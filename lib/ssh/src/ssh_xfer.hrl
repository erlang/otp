%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%% Description: SFTP defines
-define(SSH_SFTP_PROTOCOL_VERSION, 6).		
%%%----------------------------------------------------------------------
%%% #   SSH_FXP_xxx
%%% Description: Request and initialization packet types for file transfer
%%%              protocol.
%%%----------------------------------------------------------------------
-define(SSH_FXP_INIT,		1).
-define(SSH_FXP_VERSION,	2).
-define(SSH_FXP_OPEN,		3).
-define(SSH_FXP_CLOSE,		4).
-define(SSH_FXP_READ,		5).
-define(SSH_FXP_WRITE,		6).
-define(SSH_FXP_LSTAT,		7).
-define(SSH_FXP_FSTAT,		8).
-define(SSH_FXP_SETSTAT,	9).
-define(SSH_FXP_FSETSTAT,	10).
-define(SSH_FXP_OPENDIR,	11).
-define(SSH_FXP_READDIR,	12).
-define(SSH_FXP_REMOVE,		13).
-define(SSH_FXP_MKDIR,		14).
-define(SSH_FXP_RMDIR,		15).
-define(SSH_FXP_REALPATH,	16).
-define(SSH_FXP_STAT,		17).
-define(SSH_FXP_RENAME,		18).
-define(SSH_FXP_READLINK,	19).
-define(SSH_FXP_SYMLINK,	20).
-define(SSH_FXP_STATUS,		101).
-define(SSH_FXP_HANDLE,		102).
-define(SSH_FXP_DATA,		103).
-define(SSH_FXP_NAME,		104).
-define(SSH_FXP_ATTRS,		105).
-define(SSH_FXP_EXTENDED,	200).
-define(SSH_FXP_EXTENDED_REPLY,	201).

%%%----------------------------------------------------------------------
%%% #   SSH_FX_xxx
%%% Description: Response packet types for file transfer protocol.
%%%----------------------------------------------------------------------
-define(SSH_FX_OK,			0).
-define(SSH_FX_EOF,			1).
-define(SSH_FX_NO_SUCH_FILE,		2).
-define(SSH_FX_PERMISSION_DENIED,	3).
-define(SSH_FX_FAILURE,			4).
-define(SSH_FX_BAD_MESSAGE,		5).
-define(SSH_FX_NO_CONNECTION,		6).
-define(SSH_FX_CONNECTION_LOST,		7).
-define(SSH_FX_OP_UNSUPPORTED,		8).
-define(SSH_FX_INVALID_HANDLE,          9).
-define(SSH_FX_NO_SUCH_PATH,            10).
-define(SSH_FX_FILE_ALREADY_EXISTS,     11).
-define(SSH_FX_WRITE_PROTECT,           12).
-define(SSH_FX_NO_MEDIA,                13).
-define(SSH_FX_NO_SPACE_ON_FILESYSTEM,  14).
-define(SSH_FX_QUOTA_EXCEEDED,          15).
-define(SSH_FX_UNKNOWN_PRINCIPLE,       16).
-define(SSH_FX_LOCK_CONFlICT,           17).
-define(SSH_FX_DIR_NOT_EMPTY,           18).
-define(SSH_FX_NOT_A_DIRECTORY,         19).
-define(SSH_FX_INVALID_FILENAME,        20).
-define(SSH_FX_LINK_LOOP,               21).
-define(SSH_FX_CANNOT_DELETE,           22).
-define(SSH_FX_INVALID_PARAMETER,       23).
-define(SSH_FX_FILE_IS_A_DIRECTORY,     24). 
-define(SSH_FX_BYTE_RANGE_LOCK_CONFLICT,25).
-define(SSH_FX_BYTE_RANGE_LOCK_REFUSED, 26).
-define(SSH_FX_DELETE_PENDING,          27).
-define(SSH_FX_FILE_CORRUPT,            28).
-define(SSH_FX_OWNER_INVALID,           29).
-define(SSH_FX_GROUP_INVALID,           30).
-define(SSH_FX_NO_MATCHING_BYTE_RANGE_LOCK,31).

%%%----------------------------------------------------------------------
%%% #   SSH_FILEXFER_xxx
%%% Description: Bits for file attributes bit mask
%%%----------------------------------------------------------------------
-define(SSH_FILEXFER_ATTR_SIZE,            16#00000001). %% vsn 3,5
-define(SSH_FILEXFER_ATTR_UIDGID,          16#00000002). %% vsn 3
-define(SSH_FILEXFER_ATTR_PERMISSIONS,     16#00000004). %% vsn 3,5
-define(SSH_FILEXFER_ATTR_ACCESSTIME,      16#00000008). %% vsn 5
-define(SSH_FILEXFER_ATTR_ACMODTIME,       16#00000008). %% vsn 3
-define(SSH_FILEXFER_ATTR_CREATETIME,      16#00000010). %% vsn 5
-define(SSH_FILEXFER_ATTR_MODIFYTIME,      16#00000020) .%% vsn 5
-define(SSH_FILEXFER_ATTR_ACL,             16#00000040). %% vsn 5
-define(SSH_FILEXFER_ATTR_OWNERGROUP,      16#00000080). %% vsn 5
-define(SSH_FILEXFER_ATTR_SUBSECOND_TIMES, 16#00000100). %% vsn 5
-define(SSH_FILEXFER_ATTR_BITS,            16#00000200). %% vsn 5
-define(SSH_FILEXFER_ATTR_EXTENDED,        16#80000000). %% vsn 3,5

%% File types
-define(SSH_FILEXFER_TYPE_REGULAR,         1).
-define(SSH_FILEXFER_TYPE_DIRECTORY,       2).
-define(SSH_FILEXFER_TYPE_SYMLINK,         3).
-define(SSH_FILEXFER_TYPE_SPECIAL,         4).
-define(SSH_FILEXFER_TYPE_UNKNOWN,         5).
-define(SSH_FILEXFER_TYPE_SOCKET,          6).
-define(SSH_FILEXFER_TYPE_CHAR_DEVICE,     7).
-define(SSH_FILEXFER_TYPE_BLOCK_DEVICE,    8).
-define(SSH_FILEXFER_TYPE_FIFO,            9).

%% Permissions
-define(S_IRUSR,  8#0000400).
-define(S_IWUSR,  8#0000200).
-define(S_IXUSR,  8#0000100).
-define(S_IRGRP,  8#0000040).
-define(S_IWGRP,  8#0000020).
-define(S_IXGRP,  8#0000010).
-define(S_IROTH,  8#0000004).
-define(S_IWOTH,  8#0000002).
-define(S_IXOTH,  8#0000001).
-define(S_ISUID,  8#0004000).
-define(S_ISGID,  8#0002000).
-define(S_ISVTX,  8#0001000).
%% type bits (version 3 only?)
-define(S_IFMT,   8#0170000).  %% file type mask 
-define(S_IFDIR,  8#0040000).
-define(S_IFCHR,  8#0020000).
-define(S_IFBLK,  8#0060000).
-define(S_IFIFO,  8#0010000).
-define(S_IFREG,  8#0100000).
-define(S_IFLNK,  8#0120000).
-define(S_IFSOCK, 8#0140000).

%% ACE-Type
-define(ACE4_ACCESS_ALLOWED_ACE_TYPE,    16#00000000).
-define(ACE4_ACCESS_DENIED_ACE_TYPE,     16#00000001).
-define(ACE4_SYSTEM_AUDIT_ACE_TYPE,      16#00000002).
-define(ACE4_SYSTEM_ALARM_ACE_TYPE,      16#00000003).

%% ACE-Flag
-define(ACE4_FILE_INHERIT_ACE,           16#00000001).
-define(ACE4_DIRECTORY_INHERIT_ACE,      16#00000002).
-define(ACE4_NO_PROPAGATE_INHERIT_ACE,   16#00000004).
-define(ACE4_INHERIT_ONLY_ACE,           16#00000008).
-define(ACE4_SUCCESSFUL_ACCESS_ACE_FLAG, 16#00000010).
-define(ACE4_FAILED_ACCESS_ACE_FLAG,     16#00000020).
-define(ACE4_IDENTIFIER_GROUP,           16#00000040).

%% ACE-Mask
-define(ACE4_READ_DATA,         16#00000001).
-define(ACE4_LIST_DIRECTORY,    16#00000001).
-define(ACE4_WRITE_DATA,        16#00000002).
-define(ACE4_ADD_FILE,          16#00000002).
-define(ACE4_APPEND_DATA,       16#00000004).
-define(ACE4_ADD_SUBDIRECTORY,  16#00000004).
-define(ACE4_READ_NAMED_ATTRS,  16#00000008).
-define(ACE4_WRITE_NAMED_ATTRS, 16#00000010).
-define(ACE4_EXECUTE,           16#00000020).
-define(ACE4_DELETE_CHILD,      16#00000040).
-define(ACE4_READ_ATTRIBUTES,   16#00000080).
-define(ACE4_WRITE_ATTRIBUTES,  16#00000100).
-define(ACE4_DELETE,            16#00010000).
-define(ACE4_READ_ACL,          16#00020000).
-define(ACE4_WRITE_ACL,         16#00040000).
-define(ACE4_WRITE_OWNER,       16#00080000).
-define(ACE4_SYNCHRONIZE,       16#00100000).

%% Attrib-bits
-define(SSH_FILEXFER_ATTR_FLAGS_READONLY,         16#00000001).
-define(SSH_FILEXFER_ATTR_FLAGS_SYSTEM,           16#00000002).
-define(SSH_FILEXFER_ATTR_FLAGS_HIDDEN,           16#00000004).
-define(SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE, 16#00000008).
-define(SSH_FILEXFER_ATTR_FLAGS_ARCHIVE,          16#00000010).
-define(SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED,        16#00000020).
-define(SSH_FILEXFER_ATTR_FLAGS_COMPRESSED,       16#00000040).
-define(SSH_FILEXFER_ATTR_FLAGS_SPARSE,           16#00000080).
-define(SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY,      16#00000100).
-define(SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE,        16#00000200).
-define(SSH_FILEXFER_ATTR_FLAGS_SYNC,             16#00000400).

%% Open flags (version 3)
-define(SSH_FXF_READ,			16#00000001).
-define(SSH_FXF_WRITE,  		16#00000002).
-define(SSH_FXF_APPEND,		        16#00000004).
-define(SSH_FXF_CREAT,			16#00000008).
-define(SSH_FXF_TRUNC,			16#00000010).
-define(SSH_FXF_EXCL,			16#00000020).

%% Open flags (version 5)
-define(SSH_FXF_ACCESS_DISPOSITION,   16#00000007).
-define(SSH_FXF_CREATE_NEW,           16#00000000).
-define(SSH_FXF_CREATE_TRUNCATE,      16#00000001).
-define(SSH_FXF_OPEN_EXISTING,        16#00000002).
-define(SSH_FXF_OPEN_OR_CREATE,       16#00000003).
-define(SSH_FXF_TRUNCATE_EXISTING,    16#00000004).
-define(SSH_FXF_ACCESS_APPEND_DATA,   16#00000008).
-define(SSH_FXF_ACCESS_APPEND_DATA_ATOMIC, 16#00000010).
-define(SSH_FXF_ACCESS_TEXT_MODE,     16#00000020).
-define(SSH_FXF_ACCESS_READ_LOCK,     16#00000040).
-define(SSH_FXF_ACCESS_WRITE_LOCK,    16#00000080).
-define(SSH_FXF_ACCESS_DELETE_LOCK,   16#00000100).

%% Rename flags
-define(SSH_FXP_RENAME_OVERWRITE, 16#00000001).
-define(SSH_FXP_RENAME_ATOMIC,    16#00000002).
-define(SSH_FXP_RENAME_NATIVE,    16#00000004).


-define(SSH_FILEXFER_LARGEFILESIZE, (1 bsl 63)).

-record(ssh_xfer_attr,
	{
	  type,    %% regular, dirctory, symlink, ...
	  size,
	  owner,
	  group,
	  permissions,
	  atime,
	  atime_nseconds,
	  createtime,
	  createtime_nseconds,
	  mtime,
	  mtime_nseconds,
	  acl,
	  attrib_bits,
	  extensions    %% list of [{type,data}]
	 }).

-record(ssh_xfer_ace,
	{
	  type,
	  flag,
	  mask,
	  who
	 }).

%% connection endpoint and server/client info
-record(ssh_xfer,
	{
	  vsn,      %% server version
	  ext,      %% server extensions
	  cm,       %% connection mgr
	  channel   %% SFTP channel
	 }).

	

	  


