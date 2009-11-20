%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%

-ifndef(FILE_HRL_).
-define(FILE_HRL_, 1).
%%--------------------------------------------------------------------------

%%-type namelist() :: [char() | atom() | namelist()].
-type namelist() :: [_].       %% XXX: GROSS OVERAPPROXIMATION -- FIX ME
-type name() :: string() | atom() | namelist().
-type posix() :: atom().

-type date() :: {pos_integer(), pos_integer(), pos_integer()}.
-type time() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type date_time() :: {date(), time()}.

%%--------------------------------------------------------------------------

-record(file_info,
	{size   :: non_neg_integer(),	% Size of file in bytes.
	 type   :: 'device' | 'directory' | 'other' | 'regular' | 'symlink',
	 access :: 'read' | 'write' | 'read_write' | 'none',
	 atime  :: date_time(),	% The local time the file was last read:
				% {{Year, Mon, Day}, {Hour, Min, Sec}}.
	 mtime  :: date_time(),	% The local time the file was last written.
	 ctime  :: date_time(),	% The interpretation of this time field
	                        % is dependent on operating system.
				% On Unix it is the last time the file or
				% or the inode was changed.  On Windows,
				% it is the creation time.
	 mode   :: integer(),		% File permissions.  On Windows,
	 				% the owner permissions will be
					% duplicated for group and user.
	 links  :: non_neg_integer(),	% Number of links to the file (1 if the
					% filesystem doesn't support links).
	 major_device :: integer(),	% Identifies the file system (Unix),
	 				% or the drive number (A: = 0, B: = 1)
					% (Windows).
	 %% The following are Unix specific.
	 %% They are set to zero on other operating systems.
	 minor_device :: integer(),	% Only valid for devices.
	 inode  :: integer(),  		% Inode number for file.
	 uid    :: integer(),  		% User id for owner.
	 gid    :: integer()}).	        % Group id for owner.


-record(file_descriptor,
	{module :: module(),      % Module that handles this kind of file
	 data   :: term()}).     % Module dependent data

-type fd() :: pid() | #file_descriptor{}.

%%--------------------------------------------------------------------------
-endif.
