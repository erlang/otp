%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: eunit_tests.erl 238 2007-11-15 10:23:54Z mremond $ 
%%
%% @author Richard Carlsson <richard@it.uu.se>
%% @copyright 2007 Richard Carlsson
%% @private
%% @see eunit
%% @doc External tests for eunit.erl

-module(eunit_tests).

-include("eunit.hrl").

-ifdef(TEST).
%% Cause all the other modules to be tested as well as this one.
full_test_() ->
    %%{application, eunit}.    % this currently causes a loop
    %% We use the below until loop detection is implemented
    [eunit_autoexport,
     eunit_striptests,
     eunit_server,
     eunit_proc,
     eunit_serial,
     eunit_test,
     eunit_lib,
     eunit_data,
     eunit_tty].
-endif.
