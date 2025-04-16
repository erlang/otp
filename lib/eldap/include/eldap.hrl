%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: MIT
%%
%% Copyright (c) 2010, Torbjörn Törnkvist
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% %CopyrightEnd%
%% 

-ifndef( _ELDAP_HRL ).
-define( _ELDAP_HRL , 1 ).

%%%
%%% Search input parameters
%%%
-record(eldap_search, {
	  base = [],             % Baseobject
	  filter = [],           % Search conditions
	  size_limit = 0,        % Setting default size limit to 0 makes it unlimited
	  scope=wholeSubtree,    % Search scope
	  deref=derefAlways,     % Dereference
	  attributes = [],       % Attributes to be returned
	  types_only = false,    % Return types+values or types
	  timeout = 0            % Timelimit for search
	 }).

%%%
%%% Returned search result
%%%
-record(eldap_search_result, {
	  entries = [],          % List of #eldap_entry{} records
	  referrals = [],        % List of referrals
	  controls = []          % List of controls
	  }).

%%%
%%% LDAP entry
%%%
-record(eldap_entry, {
	  object_name = "",      % The DN for the entry
	  attributes = []        % List of {Attribute, Value} pairs
	 }).

-endif.
