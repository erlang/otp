%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

%% Purpose: record definitions shared between ssl_prim.erl and ssl_broker.erl

-record(st, {brokertype = nil,	% connector | listener | acceptor
	     server = nil, 	% pid of ssl_server
	     client = nil, 	% client pid
	     collector = nil, 	% client pid, or collector during change of 
	     			% controlling process
	     fd = nil, 		% fd of "external" socket in port program
	     active = true, 	% true | false | once
	     opts = [], 	% options
	     thissock = nil,    % this sslsocket
	     proxysock = nil, 	% local proxy socket within Erlang
	     proxyport = nil,   % local port for proxy within Erlang
	     status = nil,	% open | closing | closed 
	     encrypted = false, %
	     debug = false	%
	    }).
