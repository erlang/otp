%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2015. All Rights Reserved.
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

-ifndef(FILE_HRL_).
-define(FILE_HRL_, 1).
%%--------------------------------------------------------------------------

-record(file_info,
        {size   :: non_neg_integer() | 'undefined',  % Size of file in bytes.
         type   :: 'device' | 'directory' | 'other' | 'regular' | 'symlink'
                 | 'undefined',
         access :: 'read' | 'write' | 'read_write' | 'none' | 'undefined',
         atime  :: file:date_time() | non_neg_integer() | 'undefined',
                                     % The local time the file was last read:
                                     % {{Year, Mon, Day}, {Hour, Min, Sec}}.
                                     % atime, ctime, mtime may also be unix epochs()
         mtime  :: file:date_time() | non_neg_integer() | 'undefined',
                                     % The local time the file was last written.
         ctime  :: file:date_time() | non_neg_integer() | 'undefined',
                                     % The interpretation of this time field
                                     % is dependent on operating system.
                                     % On Unix it is the last time the file
                                     % or the inode was changed.  On Windows,
                                     % it is the creation time.
         mode   :: non_neg_integer() | 'undefined',
                                     % File permissions.  On Windows,
                                     % the owner permissions will be
                                     % duplicated for group and user.
         links  :: non_neg_integer() | 'undefined',
                                     % Number of links to the file (1 if the
                                     % filesystem doesn't support links).
         major_device :: non_neg_integer() | 'undefined',
                                     % Identifies the file system (Unix),
                                     % or the drive number (A: = 0, B: = 1)
                                     % (Windows).
         %% The following are Unix specific.
         %% They are set to zero on other operating systems.
         minor_device :: non_neg_integer() | 'undefined',
                                             % Only valid for devices.
         inode   :: non_neg_integer() | 'undefined',  % Inode number for file.
         uid     :: non_neg_integer() | 'undefined',   % User id for owner.
         gid     :: non_neg_integer() | 'undefined'}). % Group id for owner.


-record(file_descriptor,
	{module :: module(),     % Module that handles this kind of file
	 data   :: term()}).     % Module dependent data

%%--------------------------------------------------------------------------
-endif.
