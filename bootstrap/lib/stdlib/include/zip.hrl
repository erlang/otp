%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

-record(zip_file, {
	  name      :: string(),	  % file name
	  info	    :: file:file_info(),
	  comment   :: string(),	  % zip file comment
	  offset    :: non_neg_integer(), % offset of file's local header in archive
	  comp_size :: non_neg_integer()  % compressed size
	 }).

-record(zip_comment, {
	  comment   :: string()		  % zip archive comment
	 }).

