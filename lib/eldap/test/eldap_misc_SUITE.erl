%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2014. All Rights Reserved.
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

-module(eldap_misc_SUITE).

-compile(export_all). %% Use this only in test suites...

-include_lib("common_test/include/ct.hrl").
-include_lib("eldap/include/eldap.hrl").
-include_lib("eldap/ebin/ELDAPv3.hrl").

all() ->
    [
     encode,
     decode
    ].


encode(_Config) ->
    {ok,Bin} = 'ELDAPv3':encode('AddRequest', #'AddRequest'{entry="hejHopp"  ,attributes=[]} ),
    Expected = <<104,11,4,7,104,101,106,72,111,112,112,48,0>>,
    Expected = Bin.

decode(_Config) ->
    {ok,Res} = 'ELDAPv3':decode('AddRequest', <<104,11,4,7,104,101,106,72,111,112,112,48,0>>),
    ct:log("Res = ~p", [Res]),
    Expected = #'AddRequest'{entry = "hejHopp",attributes = []},
    case Res of
	Expected -> ok;
	#'AddRequest'{entry= <<"hejHopp">>, attributes=[]} -> 
	    {fail, "decoded to (correct) binary!!"};
	_ ->
	    {fail, "Bad decode"}
    end.
	    
