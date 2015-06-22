%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

