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
/**
 * InitialReference is a class which generates the INIT reference
 * which can be used by the InitialReferences interface.
 *
 * creation date: 1997-11-04
 */
#include "InitialReference.hh"

InitialReference::InitialReference()
{
  host = 0;
  iorString = 0;
};

InitialReference::~InitialReference()
{
  if(host){
    delete host;
    delete iorString;
  }
};

  /**
   * Returns the stringified objectreference to the initial reference server
   */
char* InitialReference::stringified_ior(char* newhost, int newport)
{
  STRINGSTREAM iorByteString;
  STRINGSTREAM profileData;

  STRINGBUF *s;
  STRINGBUF *profileDataBuf;
  STRINGBUF *iorByteStringBuf;
  long profileDataLength = 0;
  char *str;

  // byte_order followed by ' {"", [{0, ' 
  //      char iorBytesFirstPart[] = {0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0};
  char iorBytesFirstPart[48] = {0,0,0,0,0,0,0,32,73,68,76,58,79,114,98,101,114,47,73,110,105,116,105,97,108,82,101,102,101,114,101,110,99,101,115,58,49,46,48,0,0,0,0,1,0,0,0,0};
  // the objectkey "INIT
  char iorBytesLastPart[8] = {0,0,0,4,73,78,73,84};
  
  // Fix the ProfileData struct.
  char pdPrefix[4] = {0,1,0,0};
  char hsize[4];
  char profileDataLengthBytes[4];
  char portbytes[2];
  long hostLength = strlen(newhost);
  
  
  if(host) 
    if(strcmp(newhost, host) == 0 && newport == port)
      return iorString;
    else {
      delete host;
      delete iorString;
    }
  host = new char[hostLength+1];
  memcpy(host, newhost, hostLength+1);
  port = newport;
  
  enc_ulong(hostLength + 1, hsize);
  enc_ushort(port, portbytes);

  profileDataBuf = profileData.rdbuf();

  profileDataBuf->sputn(pdPrefix, 4);
  profileDataBuf->sputn(hsize, 4);
  profileDataBuf->sputn(host, hostLength);
  profileDataBuf->sputc(0);
  profileDataLength = 4 + 4 + hostLength + 1;
  
  profileDataLength = align(profileDataBuf, profileDataLength, 2);
  
  profileDataBuf->sputn(portbytes, 2);
  profileDataLength += 2;
  
  profileDataLength = align(profileDataBuf, profileDataLength, 4);
  
  profileDataBuf->sputn(iorBytesLastPart, 8);
  profileDataLength += 8;
  //cout << "pd length: " << profileDataLength << "\n";

  enc_ulong(profileDataLength, profileDataLengthBytes);

  // Fix the whole IOR

  iorByteStringBuf = iorByteString.rdbuf();

  iorByteStringBuf->sputn(iorBytesFirstPart, 48);
  iorByteStringBuf->sputn(profileDataLengthBytes, 4);
#ifdef HAVE_SSTREAM
  iorByteStringBuf->sputn(profileData.str().data(), profileDataLength);
#else
  str = profileData.str();
  iorByteStringBuf->sputn(str, profileDataLength);
  delete str;
#endif

  createIOR(iorByteString, 48 + 4 + profileDataLength);

  return iorString;
}


void InitialReference::enc_ushort(int s, char *byteArray)
{
  byteArray[0] = (char) ((s >> 8) & 0xFF);
  byteArray[1] = (char) ((s >> 0) & 0xFF);
  
  return;
}

void InitialReference::enc_ulong(long l, char *byteArray)
{
  byteArray[0] = (char) ((l >> 24) & 0xFF);
  byteArray[1] = (char) ((l >> 16) & 0xFF);
  byteArray[2] = (char) ((l >> 8) & 0xFF);
  byteArray[3] = (char) ((l >> 0) & 0xFF);
  
  return;
}

void InitialReference::createIOR(strstream& byte, long length)
{
  STRINGBUF *stringbuf;
  STRINGSTREAM string;

  int i;
#ifdef HAVE_SSTREAM
  const char *c;
  const char *bytestr = byte.str().c_str();
#else
  char *c;
  char *bytestr = byte.str();
#endif
  int b, n1, n2, c1, c2;

  stringbuf = string.rdbuf();
  stringbuf->sputn("IOR:",4);
  
  for(i = 0, c = bytestr; i < length; c++, i++)
    {
      b = *c;
      if(b<0) b+= 256;
      n1 = b / 16;
      n2 = b % 16;
      c1 = (n1 < 10) ? ('0' + n1) : ('a' + (n1 - 10));
      c2 = (n2 < 10) ? ('0' + n2) : ('a' + (n2 - 10));
      
      stringbuf->sputc(c1);
      stringbuf->sputc(c2);
      
    }

  stringbuf->sputc(0);

  delete bytestr;

#ifdef HAVE_SSTREAM
  iorString = (char *)string.str().c_str();
#else
  iorString = string.str();
#endif
      
  return;
}

long InitialReference::align(STRINGBUF* sbuf, long currentLength, 
			    int alignment)
{
  long length = currentLength;
  
  int remainder = alignment - (currentLength % alignment);
  if (remainder == alignment) return length;
  
  for (int i = 0; i < remainder; i++)
    {
      sbuf->sputc(0);
      length++;
    }
  return length;
}

  
