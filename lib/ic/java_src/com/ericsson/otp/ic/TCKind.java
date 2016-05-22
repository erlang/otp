/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
 * The TCKind class for Java IDL
 *
 */
package com.ericsson.otp.ic;

/**
  The TCKind class is the implementation of the OMG-IDL enumerant type TCKind.
  **/

final public class TCKind {

   // instance variables
   public static final int _tk_null = 0,
                           _tk_void = 1,
                           _tk_short = 2,
                           _tk_long = 3,
                           _tk_ushort = 4,
                           _tk_ulong = 5,
                           _tk_float = 6,
                           _tk_double = 7,
                           _tk_boolean = 8,
                           _tk_char = 9,
                           _tk_octet = 10,
                           _tk_any = 11,
                           _tk_TypeCode = 12,
                           _tk_Principal = 13,
                           _tk_objref = 14,
                           _tk_struct = 15,
                           _tk_union = 16,
                           _tk_enum = 17,
                           _tk_string = 18,
                           _tk_sequence = 19,
                           _tk_array = 20,
                           _tk_alias = 21,
                           _tk_except = 22,
                           _tk_longlong = 23,
                           _tk_ulonglong = 24,
                           _tk_longdouble = 25,
                           _tk_wchar = 26,
                           _tk_wstring = 27,
                           _tk_fixed = 28,
                           _tk_atom = 20000, /* Used for union label default value only */
                           _tk_pid = 20001,  /* Used for special pid struct */
                           _tk_port = 20002, /* Used for special port struct */
                           _tk_ref = 20003,  /* Used for special ref struct */
                           _tk_term = 20004; /* Used for special term struct */

   public static final TCKind tk_null = new TCKind(_tk_null);
   public static final TCKind tk_void = new TCKind(_tk_void);
   public static final TCKind tk_short = new TCKind(_tk_short);
   public static final TCKind tk_long = new TCKind(_tk_long);
   public static final TCKind tk_ushort = new TCKind(_tk_ushort);
   public static final TCKind tk_ulong = new TCKind(_tk_ulong);
   public static final TCKind tk_float = new TCKind(_tk_float);
   public static final TCKind tk_double = new TCKind(_tk_double);
   public static final TCKind tk_boolean = new TCKind(_tk_boolean);
   public static final TCKind tk_char = new TCKind(_tk_char);
   public static final TCKind tk_octet = new TCKind(_tk_octet);
   public static final TCKind tk_any = new TCKind(_tk_any);
   public static final TCKind tk_TypeCode = new TCKind(_tk_TypeCode);
   public static final TCKind tk_Principal = new TCKind(_tk_Principal);
   public static final TCKind tk_objref = new TCKind(_tk_objref);
   public static final TCKind tk_struct = new TCKind(_tk_struct);
   public static final TCKind tk_union = new TCKind(_tk_union);
   public static final TCKind tk_enum = new TCKind(_tk_enum);
   public static final TCKind tk_string = new TCKind(_tk_string);
   public static final TCKind tk_sequence = new TCKind(_tk_sequence);
   public static final TCKind tk_array = new TCKind(_tk_array);
   public static final TCKind tk_alias = new TCKind(_tk_alias);
   public static final TCKind tk_except = new TCKind(_tk_except);
   public static final TCKind tk_longlong = new TCKind(_tk_longlong);
   public static final TCKind tk_ulonglong = new TCKind(_tk_ulonglong);
   public static final TCKind tk_longdouble = new TCKind(_tk_longdouble);
   public static final TCKind tk_wchar = new TCKind(_tk_wchar);
   public static final TCKind tk_wstring = new TCKind(_tk_wstring);
   public static final TCKind tk_fixed = new TCKind(_tk_fixed);
   protected static final TCKind tk_atom = new TCKind(_tk_atom);
   protected static final TCKind tk_pid = new TCKind(_tk_pid);
   protected static final TCKind tk_port = new TCKind(_tk_port);
   protected static final TCKind tk_ref = new TCKind(_tk_ref);
   protected static final TCKind tk_term = new TCKind(_tk_term);
   private int _value;

   // constructors
   private TCKind(int __value) {
      _value = __value;
   }

   // methods

  /**
    Accessor method for the value of TCKind.
    @return int, the value of TCKind object
    **/
   public int value() {
      return _value;
   }

  /**
    Translator method for TCKind.
    Traslates the input integer value to a TCKind enumerant object.
    @return TCKind, a TCKind object
    **/
   public static final TCKind from_int(int __value)  throws java.lang.Exception {
      switch (__value) {
         case _tk_null:
            return tk_null;
         case _tk_void:
            return tk_void;
         case _tk_short:
            return tk_short;
         case _tk_long:
            return tk_long;
         case _tk_ushort:
            return tk_ushort;
         case _tk_ulong:
            return tk_ulong;
         case _tk_float:
            return tk_float;
         case _tk_double:
            return tk_double;
         case _tk_boolean:
            return tk_boolean;
         case _tk_char:
            return tk_char;
         case _tk_octet:
            return tk_octet;
         case _tk_any:
            return tk_any;
         case _tk_TypeCode:
            return tk_TypeCode;
         case _tk_Principal:
            return tk_Principal;
         case _tk_objref:
            return tk_objref;
         case _tk_struct:
            return tk_struct;
         case _tk_union:
            return tk_union;
         case _tk_enum:
            return tk_enum;
         case _tk_string:
            return tk_string;
         case _tk_sequence:
            return tk_sequence;
         case _tk_array:
            return tk_array;
         case _tk_alias:
            return tk_alias;
         case _tk_except:
            return tk_except;
         case _tk_longlong:
            return tk_longlong;
         case _tk_ulonglong:
            return tk_ulonglong;
         case _tk_longdouble:
            return tk_longdouble;
         case _tk_wchar:
            return tk_wchar;
         case _tk_wstring:
            return tk_wstring;
         case _tk_fixed:
            return tk_fixed;
	 case _tk_atom:
	   return tk_atom;
         case _tk_pid:
	   return tk_pid;
	 case _tk_port:
           return tk_port;
	 case _tk_ref:
           return tk_ref;
	 case _tk_term:
           return tk_term;
         default:
            throw new java.lang.Exception("");
      }
   }

}
