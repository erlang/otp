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
package com.ericsson.otp.ic;


/**

The Any class is the java mapping of the any OMG-IDL type. 


**/


public class Any { 

  // Typecode value holder
  protected TypeCode tcV;

  // Primitive value holder
  protected java.lang.String stringV;
  protected byte byteV;
  protected boolean booleanV;
  protected char charV;
  protected short shortV;
  protected int intV;
  protected long longV;
  protected float floatV;
  protected double doubleV;

  // Streams used for user defined types
  protected com.ericsson.otp.erlang.OtpInputStream is;
  protected com.ericsson.otp.erlang.OtpOutputStream os;


  // Constructor
  public Any() {
    tcV = null;
  }

  // Equal function

  /**
    Any comparison method
    @return true if the input Any is equal to the object, false otherwize 
  **/
  public boolean equal(com.ericsson.otp.ic.Any _any) {
    
    int _is1Len,_is2Len;
    byte _compressed[];
    com.ericsson.otp.erlang.OtpInputStream _is1,_is2;
    TypeCode _tc = _any.type();
    
    if (!tcV.equal(_tc))
      return false;

    try {
    
      TCKind _tck = _tc.kind();
      
      switch (_tck.value()) {
	
      case TCKind._tk_short:
	return (_any.extract_short() == shortV);

      case TCKind._tk_ushort:
	return (_any.extract_ushort() == shortV);
	
      case TCKind._tk_long:	
	return (_any.extract_long() == intV);
	
      case TCKind._tk_longlong:	
	return (_any.extract_longlong() == longV);

      case TCKind._tk_ulong:
	return (_any.extract_ulong() == intV);

      case TCKind._tk_ulonglong:
	return (_any.extract_ulonglong() == longV);

      case TCKind._tk_float:
	return equal(_any.extract_float(),floatV);
	
      case TCKind._tk_double:
	return equal(_any.extract_double(),doubleV);
	
      case TCKind._tk_boolean:
	return (_any.extract_boolean() == booleanV);
	
      case TCKind._tk_char:
	return (_any.extract_char() == charV);
	
      case TCKind._tk_wchar:
	return (_any.extract_wchar() == charV);
	
      case TCKind._tk_octet:
	return (_any.extract_octet() == byteV);
	
      case TCKind._tk_string:
	return (_any.extract_string().compareTo(stringV) == 0);

      case TCKind._tk_wstring:
	return (_any.extract_wstring().compareTo(stringV) == 0);
	
      case TCKind._tk_sequence:

	_is1 = new com.ericsson.otp.erlang.OtpInputStream(os.toByteArray());

	_is2 = _any.extract_Streamable();

	if (_is1.peek() != _is2.peek()) {
	  
	  // _is1's sequence is compressed to string
	  if(_is1.peek() == com.ericsson.otp.erlang.OtpExternal.stringTag) {

	    _compressed = (_is1.read_string()).getBytes();
	    _is1Len = _compressed.length;
	    
	    _is2.read_list_head();
	    
	    for(int i = 0; i < _is1Len; i++) {
	      if ((long)(_compressed[i] & 0xff) != _is2.read_long())
		return false;
	    }
	    
	    _is2.read_nil();
	  }
	  else { // _is2's sequence is compressed to string

	    _compressed = (_is2.read_string()).getBytes();
	    _is2Len = _compressed.length;

	    _is1.read_list_head();

	    for(int i = 0; i < _is2Len; i++) 
	      if ((long)(_compressed[i] & 0xff) != _is1.read_long())
		return false;

	    _is1.read_nil();
	  }
	}
	else { // None of them is compressed
	  
	  _is2Len = _is2.available();

	  if (_is1.available() !=  _is2Len)
	    return false;
	    
	  for(int i = 0; i < _is2Len; i++) {
	    if (_is1.read() != _is2.read())
	      return false;
	  }
	}

	return true;

      case TCKind._tk_struct:      
      case TCKind._tk_union:
      case TCKind._tk_array:
      case TCKind._tk_enum:
	
	_is1 = new com.ericsson.otp.erlang.OtpInputStream(os.toByteArray());

	_is2 = _any.extract_Streamable();

	_is2Len = _is2.available();

	if (_is1.available() !=  _is2Len)
	  return false;

	for(int i = 0; i < _is2Len; i++) {
	  if (_is1.read() != _is2.read())
	    return false;
	}

	return true;
	
	// Not used in real
      case TCKind._tk_any:
      case TCKind._tk_void:
      case TCKind._tk_atom:
      case TCKind._tk_null:
      case TCKind._tk_TypeCode:
      case TCKind._tk_Principal:
      case TCKind._tk_objref:
      case TCKind._tk_alias:
      case TCKind._tk_except:
      case TCKind._tk_longdouble:
      case TCKind._tk_fixed:
	return true;
	
    default :
      return false;
      
      }
    } catch (Exception e) {
      //e.printStackTrace();
      return false;
    }
    
  }

  
  /* Equal function for floats ( relative diff ) */
  boolean equal(float x, float y) {
    
    if (x != 0)
      return (java.lang.Math.abs((x-y)/x) < 1.0E-15);

    if (y != 0)
      return (java.lang.Math.abs((y-x)/y) < 1.0E-15);

    return (x==y);
  }

  /* Equal function for doubles ( relative diff ) */
  boolean equal(double x, double y) {
        
    if (x != 0)
      return (java.lang.Math.abs((x-y)/x) < 1.0E-15);

    if (y != 0)
      return (java.lang.Math.abs((y-x)/y) < 1.0E-15);

    return (x==y);
  }



  /**
    TypeCode accessor method
    @return the Any's TypeCode 
    **/
  public TypeCode type() {
    return tcV;
  }
  
  
  /**
    TypeCode insertion method  
    **/
  public void type(TypeCode _tc) {
    tcV = _tc;
  }


  /* Value accessors */

  /**
    Reads a value from the stream, according to the inserted TypeCode 
    **/
  public void read_value(com.ericsson.otp.erlang.OtpInputStream _is,
			 TypeCode _tc) 
    throws java.lang.Exception {

      tcV = _tc;
       
      switch(tcV.kind().value()) {
	
      case TCKind._tk_short :
	shortV = _is.read_short();
	break;
      case TCKind._tk_ushort : 
	shortV = _is.read_ushort();
	break;
      case TCKind._tk_long : 
	intV = _is.read_int();
	break;
      case TCKind._tk_ulong : 
	intV = _is.read_uint();
	break;
      case TCKind._tk_longlong : 
	longV = _is.read_long();
	break;
      case TCKind._tk_ulonglong : 
	longV = _is.read_ulong();
	break;
      case TCKind._tk_float : 
	floatV = _is.read_float();
	break;
      case TCKind._tk_double : 
	doubleV = _is.read_double();
	break;
      case TCKind._tk_boolean : 
	booleanV = _is.read_boolean();
	break;
      case TCKind._tk_char : 
      case TCKind._tk_wchar : 
	charV = _is.read_char();
	break;
      case TCKind._tk_octet : 
	byteV = _is.read_byte();
	break;
      case TCKind._tk_string : 
      case TCKind._tk_wstring : 
	stringV = _is.read_string();
	break;
      case TCKind._tk_atom : 
	stringV = _is.read_atom();
	break;
      case TCKind._tk_void :
	_is.read_atom();
	break;

	 /*
	  * Not supported types
	  */
      case TCKind._tk_any :
      case TCKind._tk_null : 
      case TCKind._tk_TypeCode : 
      case TCKind._tk_Principal : 
      case TCKind._tk_objref : 
      case TCKind._tk_alias : 
      case TCKind._tk_except :  
      case TCKind._tk_longdouble :  
      case TCKind._tk_fixed :
	throw new java.lang.Exception("Unsupported type");
	
      default: // User defined type

	if (os == null)
	  os = new com.ericsson.otp.erlang.OtpOutputStream();
	else
	  os.reset();

	try {
	  read_user_defined(_is, _tc);
	  is = new com.ericsson.otp.erlang.OtpInputStream(os.toByteArray());
	} catch (Exception e) {
	  throw new java.lang.Exception("BAD VALUE");
	}
      }

  }

  void read_user_defined(com.ericsson.otp.erlang.OtpInputStream _is, TypeCode _tc) 
    throws java.lang.Exception {
      
      TypeCode memberTC = null;
      int len = -1;
      int __tag;
      
      switch(_tc.kind().value()) {

      case TCKind._tk_short :
	os.write_short(_is.read_short());
	break;
      case TCKind._tk_ushort : 
	os.write_ushort(_is.read_ushort());
	break;
      case TCKind._tk_long :
	os.write_int(_is.read_int());
	break;
      case TCKind._tk_longlong :
	os.write_long(_is.read_long());
	break;
      case TCKind._tk_ulong :
	os.write_uint(_is.read_uint());
	break;
      case TCKind._tk_ulonglong :
	os.write_ulong(_is.read_ulong());
	break;
      case TCKind._tk_float :
	os.write_float(_is.read_float());
	break;
      case TCKind._tk_double :
	os.write_double(_is.read_double());
	break;
      case TCKind._tk_boolean : 
	os.write_boolean(_is.read_boolean());
	break;
      case TCKind._tk_char : 
      case TCKind._tk_wchar : 
	os.write_char(_is.read_char());
	break;
      case TCKind._tk_octet :
	os.write_byte(_is.read_byte());
	break;
      case TCKind._tk_string :
      case TCKind._tk_wstring :
	os.write_string(_is.read_string());
	break;
      
      case TCKind._tk_struct:	
	len = _is.read_tuple_head();
	os.write_tuple_head(len);
	os.write_atom(_is.read_atom());
	// Member list
	len -=1;
	for(int i=0; i<len; i++) 
	  read_user_defined(_is,_tc.member_type(i));
	break;

      case TCKind._tk_union:
	os.write_tuple_head(_is.read_tuple_head());
	os.write_atom(_is.read_atom());

	int __mlen = _tc.member_count();
	__tag = _is.peek();
	boolean __found = false;

	switch (__tag) {
	case (com.ericsson.otp.erlang.OtpExternal.atomTag):
	case (com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag):
	case (com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag):
	  java.lang.String __elabel = _is.read_atom(); // Enumerant or Boolean
	  os.write_atom(__elabel);

	  for (int i=0; i<__mlen; i++) {
	    java.lang.String __mlabel;
	    if (_tc.member_label(i).type().kind().value() == TCKind._tk_string)
	      __mlabel = _tc.member_label(i).extract_string();
	    else   // Default 
	      __mlabel = _tc.member_label(i).extract_atom();
	    
	    if (__elabel.compareTo(__mlabel)==0) {
	      read_user_defined(_is,_tc.member_type(i));
	      i = __mlen;
	      __found = true;
	    }
	  }
	  break;

	default: // Integer type
	  long __ilabel = _is.read_long();
	  os.write_long(__ilabel);

	  for (int i=0; i<__mlen; i++) {
	    boolean __itype = true;
	    long __mlabel = 0;

	    switch (_tc.member_label(i).type().kind().value()) {

	    case TCKind._tk_short :
	      __mlabel = _tc.member_label(i).extract_short();
	      break;
	    case TCKind._tk_ushort :
	      __mlabel = _tc.member_label(i).extract_ushort();
	      break;
	    case TCKind._tk_long :
	      __mlabel = _tc.member_label(i).extract_long();
	      break;
	    case TCKind._tk_longlong :
	      __mlabel = _tc.member_label(i).extract_longlong();
	      break;
	    case TCKind._tk_ulong :
	      __mlabel = _tc.member_label(i).extract_ulong();
	      break;
	    case TCKind._tk_ulonglong :
	      __mlabel = _tc.member_label(i).extract_ulonglong();
	      break;
	    case TCKind._tk_char :
	      __mlabel = _tc.member_label(i).extract_char();
	      break;
	    case TCKind._tk_wchar :
	      __mlabel = _tc.member_label(i).extract_wchar();
	      break;
	      
	    default :  // Default label
	      __itype = false;
	      
	    }
	    
	    if (__itype) {
	      if (__ilabel == __mlabel) {
		read_user_defined(_is,_tc.member_type(i));
		i = __mlen;
		__found = true;
	      }
	    }
	  } 
	}

	// Use the default label instead
	if (!__found)
	  read_user_defined(_is,_tc.member_type(_tc.default_index()));
	
	break;
	
      case TCKind._tk_sequence:
	__tag = _is.peek();

	switch(__tag) {
	case com.ericsson.otp.erlang.OtpExternal.stringTag:
	  os.write_string(_is.read_string());
	  break;
	default:
	  len = _is.read_list_head();
	  os.write_list_head(len);

	  for (int i=0; i<len; i++)
	    read_user_defined(_is,_tc.content_type());

	  _is.read_nil();
	  os.write_nil();
	}
	break;

      case TCKind._tk_array:
	len = _is.read_tuple_head();
	os.write_tuple_head(len);
	for (int i=0; i<len; i++)
	  read_user_defined(_is,_tc.content_type());
	break;

      case TCKind._tk_enum:
	os.write_atom(_is.read_atom());
	break;

      case TCKind._tk_void : 
	os.write_atom(_is.read_atom());
	break;

      case TCKind._tk_any : 
	AnyHelper.marshal(os,AnyHelper.unmarshal(_is));
	break;

	/*
	 * Not supported types
	 */
      default :
	throw new java.lang.Exception("");
	
      }
			   	       
  }


  /**
    Writes the Any's value to the ouput stream
    **/
  public void write_value(com.ericsson.otp.erlang.OtpOutputStream _os) 
    throws java.lang.Exception {

      switch(tcV.kind().value()) {
  
      case TCKind._tk_short :
	_os.write_short(shortV);
	break;
      case TCKind._tk_ushort : 
	_os.write_ushort(shortV);
	break;
      case TCKind._tk_long :
	_os.write_int(intV);
	break;
      case TCKind._tk_ulong :
	_os.write_uint(intV);
	break;
      case TCKind._tk_longlong :
	_os.write_long(longV);
	break;
      case TCKind._tk_ulonglong :
	_os.write_ulong(longV);
	break;
      case TCKind._tk_float :
	_os.write_float(floatV);
	break;
      case TCKind._tk_double :
	_os.write_double(doubleV);
	break;
      case TCKind._tk_boolean : 
	_os.write_boolean(booleanV);
	break;
      case TCKind._tk_char : 
      case TCKind._tk_wchar : 
	_os.write_char(charV);
	break;
      case TCKind._tk_octet :
	_os.write_byte(byteV);
	break;
      case TCKind._tk_string :
      case TCKind._tk_wstring :
	_os.write_string(stringV);
	break;
      case TCKind._tk_atom :
	_os.write_atom(stringV);
	break;
      case TCKind._tk_void : 
	_os.write_atom("ok");
	break;
	
	 /*
	  * Not supported types
	  */
      case TCKind._tk_any : 
      case TCKind._tk_null : 
      case TCKind._tk_TypeCode : 
      case TCKind._tk_Principal : 
      case TCKind._tk_objref : 
      case TCKind._tk_alias : 
      case TCKind._tk_except : 
      case TCKind._tk_longdouble :  
      case TCKind._tk_fixed :
	throw new java.lang.Exception("BAD KIND");
	
      default:
	_os.write(os.toByteArray());
      }
  }
    
  
  /*
   * Insert and extract each primitive type
   */
  
  /* short */

  /**
    Short value extractor method
    @return short, the value of Any 
  **/
  public short extract_short() 
    throws java.lang.Exception {
      if (tcV.kind() == TCKind.tk_short)
	return shortV;

      throw new java.lang.Exception("");
  }
  
  /**
    Short value insertion method
  **/
  public void insert_short(short s) {
    shortV = s;
    tcV = new TypeCode(TCKind.tk_short);
  };
  
  
  /* long */
  /**
    Long value extractor method
    @return int, the value of Any 
  **/
  public int extract_long() 
    throws java.lang.Exception {
      if (tcV.kind() == TCKind.tk_long)
	return intV;

      throw new java.lang.Exception("");
  }
  
  /**
    Long value insertion method
  **/
  public void insert_long(int i){
      intV = i;
      tcV = new TypeCode(TCKind.tk_long);
  } 


    
  /* long long */
  /**
    Long Long value extractor method
    @return long, the value of Any 
  **/
  public long extract_longlong() 
    throws java.lang.Exception {
      if (tcV.kind() == TCKind.tk_longlong)
	return longV;

      throw new java.lang.Exception("");
  }
  
  /**
    Long Long value insertion method
  **/
  public void insert_longlong(long l){
      longV = l;
      tcV = new TypeCode(TCKind.tk_longlong);
  } 
  

  /* ushort */
  /**
    Unsigned Short value extractor method
    @return short, the value of Any 
  **/
  public short extract_ushort() 
    throws java.lang.Exception {
      if (tcV.kind() == TCKind.tk_ushort)
	return shortV;
      
      throw new java.lang.Exception("");
  }

  /**
    Unsigned Short value insertion method
    **/
  public void insert_ushort(short s){
      shortV = s;
      tcV = new TypeCode(TCKind.tk_ushort);
  } 
  

  /* ulong */
  
  /**
    Unsigned Long value extractor method
    @return int, the value of Any 
  **/
  public int extract_ulong() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_ulong)
	return intV;
      
      throw new java.lang.Exception("");
  } 
  
   /**
    Unsigned Long value insertion method
    **/
  public void insert_ulong(int i){
    intV = i;
    tcV = new TypeCode(TCKind.tk_ulong);
  } 



    
  /* unsigned long long */
  /**
    Unsigned Long Long value extractor method
    @return long, the value of Any 
    **/
  public long extract_ulonglong() 
    throws java.lang.Exception {
      if (tcV.kind() == TCKind.tk_ulonglong)
	return longV;

      throw new java.lang.Exception("");
  }
  
  /**
    Unsigned Long Long value insertion method
  **/
  public void insert_ulonglong(long l){
      longV = l;
      tcV = new TypeCode(TCKind.tk_ulonglong);
  } 


  /* float */
  /**
    Float value extractor method
    @return float, the value of Any 
  **/
  public float extract_float() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_float)
	return floatV;

      throw new java.lang.Exception("");
  } 
   
  /**
    Float value insertion method
    **/
  public void insert_float(float f){
      floatV = f;
      tcV = new TypeCode(TCKind.tk_float);
  } 
  
  
  /* double */
  /**
    Double value extractor method
    @return double, the value of Any 
    **/
  public double extract_double() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_double)
	return doubleV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Double value insertion method
    **/
  public void insert_double(double d){
    doubleV = d;
    tcV = new TypeCode(TCKind.tk_double);
  } 
  

  /* boolean */
  /**
    Boolean value extractor method
    @return boolean, the value of Any 
    **/
  public boolean extract_boolean() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_boolean)
	return booleanV;
      
      throw new java.lang.Exception("");
  }

  /**
    Boolean value insertion method
    **/
  public void insert_boolean(boolean b){
    booleanV = b;
    tcV = new TypeCode(TCKind.tk_boolean);
  } 
  


  /* char */
  /**
    Char value extractor method
    @return char, the value of Any 
    **/
  public char extract_char() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_char)
	return charV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Char value insertion method
    **/
  public void insert_char(char c) {
    charV = c;
    tcV = new TypeCode(TCKind.tk_char);
  } 


  /* wchar */
  /**
    Wchar value extractor method
    @return char, the value of Any 
    **/
  public char extract_wchar() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_wchar)
	return charV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Wchar value insertion method
    **/
  public void insert_wchar(char c) {
    charV = c;
    tcV = new TypeCode(TCKind.tk_wchar);
  } 
  


  /* octet */
  /**
    Octet value extractor method
    @return byte, the value of Any 
  **/
  public byte extract_octet() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_octet)
	return byteV;
      
      throw new java.lang.Exception("");
  } 
   
  /**
    Octet value insertion method
    **/
  public void insert_octet(byte b){
    byteV = b;
    tcV = new TypeCode(TCKind.tk_octet);
  }
  

  /* string */
  /**
    String value extractor method
    @return String, the value of Any 
  **/
  public java.lang.String extract_string() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_string)
	return stringV;

      throw new java.lang.Exception("");
  }
  
  /**
    String value insertion method
    **/
  public void insert_string(java.lang.String s) {
      stringV = s;
      tcV = new TypeCode(TCKind.tk_string);
  }



  /* wstring */
  /**
    Wstring value extractor method
    @return String, the value of Any 
  **/
  public java.lang.String extract_wstring() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_wstring)
	return stringV;

      throw new java.lang.Exception("");
  }
  
  /**
    Wstring value insertion method
    **/
  public void insert_wstring(java.lang.String s) {
      stringV = s;
      tcV = new TypeCode(TCKind.tk_wstring);
  }



  /* atom */
  /**
    Atom value extractor method
    @return atom, the value of Any 
  **/
  public java.lang.String extract_atom() 
    throws java.lang.Exception{
      if (tcV.kind() == TCKind.tk_atom)
	return stringV;

      throw new java.lang.Exception("");
  }
  
  /**
    Atom value insertion method
    **/
  public void insert_atom(java.lang.String s) {
      stringV = s;
      tcV = new TypeCode(TCKind.tk_atom);
  }
  

  /**
    Object Stream insertion method
  **/
  public void insert_Streamable(com.ericsson.otp.erlang.OtpOutputStream _os) {
    os = _os;
  }

  /**
    Object Stream extractor method
    @return OtpInputStream, the stream value of Any 
  **/
  public com.ericsson.otp.erlang.OtpInputStream extract_Streamable() {

    if (is == null) {
      if (os == null)
	return null;
      else {
	is = new com.ericsson.otp.erlang.OtpInputStream(os.toByteArray());
      }
    }
    
    is.reset();
    return is;
  }

}







































