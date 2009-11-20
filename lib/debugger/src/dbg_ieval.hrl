%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
-record(ieval, {level = 1,        % Current call level
		line = -1,        % Current source code line (of module)
		module,           % MFA which called the currently
		function,         %  interpreted function
		arguments,        %
		last_call = false % True if current expression is
	       }).                % the VERY last to be evaluated
                                  % (ie at all, not only in a clause)
