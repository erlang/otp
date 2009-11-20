%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%%% For character conversion

-record(in_opts, {expand_entities=false,
		  encode_filter = fun(X) -> X end}).
-record(out_opts, {escape_chars=false,
		   remove_nl=false,
		   delete_trailing_whitespace=false,
		   delete_trailing_nl=false,
		   compress_white_space=false,
		   escape_filter = fun(X) -> X end}).


-define(pcdata_IN, #in_opts{expand_entities=true}).
-define(rcdata_IN, #in_opts{expand_entities=true}).
-define(cdata_IN,  #in_opts{}).

