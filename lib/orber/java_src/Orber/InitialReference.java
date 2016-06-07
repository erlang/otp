/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * %CopyrightEnd%
 *
 */
/**
 * InitialReference is a class which generates the INIT reference
 * which can be used by the InitialReferences interface.
 */
package Orber;

public class InitialReference 
{

  /**
   * Constructor.
   */
  public InitialReference(){;}

  /**
   * Returns the stringified objectreference to the initial reference server
   */
  public String stringified_ior(String host, int port)
    {
      String iorByteString;
      String profileData;
      String iorString;

      // byte_order followed by ' {"", [{0, ' 
      //      char iorBytesFirstPart[] = {0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0};
      char iorBytesFirstPart[] = {0,0,0,0,0,0,0,32,73,68,76,58,79,114,98,101,114,47,73,110,105,116,105,97,108,82,101,102,101,114,101,110,99,101,115,58,49,46,48,0,0,0,0,1,0,0,0,0};
      // the objectkey "INIT
      char iorBytesLastPart[] = {0,0,0,4,73,78,73,84};

      // Fix the ProfileData struct.
      char pdPrefix[] = {0,1,0,0};
      char nullbyte[] = {0};
      profileData = new String(pdPrefix) + enc_ulong(host.length() + 1) + host + new String(nullbyte);
      profileData = align(profileData, 2);
      profileData +=  enc_ushort(port);
      profileData = align(profileData, 4);
      profileData += new String(iorBytesLastPart);
      // Fix the whole IOR
      iorByteString = new String(iorBytesFirstPart) + enc_ulong(profileData.length()) +
	profileData;

      //      System.out.print("Start[" + profileData.length() + "]");
      //      System.out.print("[");
      //      for(int x = 0; x < iorByteString.length(); x++)
      //	{
      //	  System.out.print((int) iorByteString.charAt(x) + ",");
      //	}
      //      System.out.println("]");

      iorString = createIOR(iorByteString);
      //      System.out.println(iorString);
      return iorString;
    }


  private String enc_ushort(int s)
    {
      char byteArray[] = {(char) ((s >>> 8) & 0xFF),
			  (char) ((s >>> 0) & 0xFF)};

      return new String(byteArray);
    }

  private String enc_ulong(int l)
    {
      char byteArray[] = {(char) ((l >>> 24) & 0xFF),
			  (char) ((l >>> 16) & 0xFF),
			  (char) ((l >>> 8) & 0xFF),
			  (char) ((l >>> 0) & 0xFF)};

      return new String(byteArray);

    }

  private String createIOR(String bytes)
    {
      int i;
      StringBuffer sb = new StringBuffer("IOR:");

      for(i = 0; i < bytes.length(); i++)
	{
	  int b = bytes.charAt(i);
	  if(b<0) b+= 256;
	  int n1 = b / 16;
	  int n2 = b % 16;
	  int c1 = (n1 < 10) ? ('0' + n1) : ('a' + (n1 - 10));
	  int c2 = (n2 < 10) ? ('0' + n2) : ('a' + (n2 - 10));
	  sb.append((char)c1);
	  sb.append((char)c2);	  
	}

      return sb.toString();
    }

  private String align(String buffer, int alignment)
    {
      String s = buffer;
      char nullbyte[] = {0};

      int remainder = alignment - (buffer.length() % alignment);
      if (remainder == alignment) return s;

      for (int i = 0; i < remainder; i++)
	{
	  s += new String(nullbyte);
	}
      return s;
    }

  
}
