/**
 *<copyright>
 * <year>1997-2007</year>
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
/*
 * InitialReference is a class for creating an external IOR for the object
 * reference INIT.
 */
#ifndef _INITIALREFERENCE_HH
#define _INITIALREFERENCE_HH
#include <stdio.h>
#include <string.h>

#if HAVE_SSTREAM
#include <sstream>
typedef std::stringstream STRINGSTREAM;
typedef std::stringbuf STRINGBUF;
#else
#include <strstream.h>
typedef strstream STRINGSTREAM;
typedef strstreambuf STRINGBUF;
#endif

class InitialReference {
private:
  char* iorString;
  char* host;
  int port;

  void enc_ushort(int s, char *byteArray);
  void enc_ulong(long l, char *byteArray);
  void createIOR(STRINGSTREAM& byte, long length);
  long align(STRINGBUF* sbuf, long currentLength, int alignment);

public:
  InitialReference();
  ~InitialReference();

  char* stringified_ior(char* host, int port);
  
};

#endif
