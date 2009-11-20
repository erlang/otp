  ``The contents of this file are subject to the Erlang Public License,
  Version 1.1, (the "License"); you may not use this file except in
  compliance with the License. You should have received a copy of the
  Erlang Public License along with this software. If not, it can be
  retrieved via the world wide web at http://www.erlang.org/.
  
  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
  the License for the specific language governing rights and limitations
  under the License.
  
  The Initial Developer of the Original Code is Ericsson Utvecklings AB.
  Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
  AB. All Rights Reserved.''
  
      $Id$


This example shows how pre and post condition can be used for a Corba server object.


The example consists of three files;

ex.idl        - the interface specification
m_i_impl.erl  - the server implementation
tracer.erl    - a module which contains a pre and a post condition


The IDL file can for example be compiled with the following options:

ic:gen(ex, [{precond, {tracer, pre}},{{postcond, "m::i::f"}, {tracer, post}}]).

The result is that the function m::i::f gets both a pre and post condition call while 
the function m::i::g just get a pre condition call.


A pre/post condition function should always return the atom ok and if something is wrong
it should raise an exception ( ex: corba:raise(#userexception{}) ).




Compile all erlang files and test the application.

First start an erlang node, then type the following commands in the erlang shell.

1> mnesia:create_schema([]).
2> orber:install([]).
3> orber:start().
3>
3> X = m_i:oe_create().
4> catch m_i:f(X, 17).
Precond called in process <0.139.0>: m_i:f() [[],17]
f working ....
Postcond called in process <0.139.0>: m_i:f() [[],17] {reply,{17,17},[]}
17
5> 
5> catch m_i:f(X, q).
6> {'EXCEPTION',{m_NotAnInteger,"IDL:m/NotAnInteger:1.0"}}
7>
7>m_i:g(X, 17).
Precond called in process <0.139.0>: m_i:g() [[],17]
ok
g working ....
8> 
8>corba_boa:dispose(X).
9> orber:stop().
10>





