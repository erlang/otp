/**
 * <copyright>
 * <year>2000-2007</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
/**
 * This module gives an example how it is possible to setup a connection
 * Orbix3.0.1 and Orber3.0.2
 */


#define USE_IIOP
 
#include <iostream.h>
#include <stdio.h>
#include <fstream.h>
#include <stdlib.h>
 
#include "NamingService.hh"
#include "InitialReference.hh"
#include "InitialReferences.hh"
#include "stack.hh"
 
 
int main(int argc, char** argv) {
 
  CORBA::Object_ptr nsRef, initRef, objRef;
  InitialReference init;
  Orber::InitialReferences_var initp;
  CosNaming::NamingContext_var Ns; 
  CosNaming::NameComponent nc;
  CosNaming::Name_var name;
  StackModule::StackFactory_var stackFac;
  StackModule::Stack_var stack;
 
  if (argc < 3) {
    cout << "usage: " << argv[0] << " <hostname>" << " <srvport>" << endl;
    exit (-1);
  }
 
  string srvHost = argv[1];
  long srvPort = atoi(argv[2]);
  
  cout << "Using host: " << srvHost << " Port: " << srvPort << endl;
 
  try
    {
      // Create Initial reference (objectkey "INIT").
      const string s  = init.stringified_ior(srvHost, srvPort);
      initRef  = CORBA::Orbix.string_to_object(s);
      initp    = Orber::InitialReferences::_narrow(initRef);
      nsRef    = initp->get("NameService");
      Ns       = CosNaming::NamingContext::_narrow(nsRef);
 
      // Create a name component.
      name         = new CosNaming::Name(1);
      name->length(1);
      name[0].id   = CORBA::string_dup("StackFactory");
      name[0].kind = CORBA::string_dup("");
 
      // Look up the Object in the NamingService.
      objRef   = Ns->resolve(name);
      stackFac = StackModule::StackFactory::_narrow(objRef);
      stack    = stackFac->create_stack();
 
      // push & pop
      stack->push(8);
      stack->push(7);
      stack->push(6);
      cout << "Stack: " << stack->pop()
           << " " << stack->pop()
           << " " << stack->pop() << endl;
 
    } catch(...) {
      cerr << "call failed" << endl;
      cerr << "Unexpected exception " << endl;
      exit(1);
    }
    cout << "Completed successfully" << endl;
    return 0;
}
