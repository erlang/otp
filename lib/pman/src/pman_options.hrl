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

%%
%% An options-record contains the return data from the option dialog.
%% 

-record(trace_options, {send=true,		    
		 treceive=true,
		 functions=true,
		 events=true,
		 to_file=false,
		 file="",
		 inherit_on_1st_spawn=false,
		 inherit_on_all_spawn=true,
		 inherit_on_1st_link=false,
		 inherit_on_all_link=true}).

