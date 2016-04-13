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

The Term class is intended to represent the erlang term generic type. 
It extends the Any class and is basically used the same way as the Any class.
<p>The main difference between Term and Any is the use of guard methods 
instead for TypeCode to determine the data included in the Term.
This actual when cannot determine a Term's value class returned at compile time.

**/

final public class Term extends Any { 

  // Primitive value holder
  protected java.lang.String atomV;
  protected long longV;
  protected Pid PidV;
  protected Ref RefV;
  protected Port PortV;
  protected com.ericsson.otp.erlang.OtpErlangObject ObjV;
  protected int tag;

  /**
    Tag accessor method
    @return int, the tag of the Object that denotes the erlang external format tag
  **/
  public int tag() {
    return tag;
  }

  /* Guards */

  /**
    Guard method
    @return true if the Term is an OtpErlangAtom, false otherwize 
  **/
  public boolean isAtom() {

    if (ObjV == null) { 
      if (tag == com.ericsson.otp.erlang.OtpExternal.atomTag ||
	  tag == com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag ||
	  tag == com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag)

	return true;

      return false;
    }

    return (ObjV instanceof com.ericsson.otp.erlang.OtpErlangAtom) ;
  }

  /**
    Guard method
    @return true if the Term is not an OtpErlangList nor an OtpErlangTuple, false otherwize 
  **/
  public boolean isConstant() {
    if (isList())
      return false;

    if (isTuple())
      return false;

    return true;
  }

  /**
    Guard method
    @return true if the Term is an OtpErlangFloat, false otherwize
  **/
  public boolean isFloat() {
    if (tag == com.ericsson.otp.erlang.OtpExternal.floatTag)
      return true;
    
    return false;
  }

  /**
    Guard method
    @return true if the Term is an OtpErlangInt, false otherwize 
  **/
  public boolean isInteger() {
    switch(tag) {
    case com.ericsson.otp.erlang.OtpExternal.smallIntTag:
    case com.ericsson.otp.erlang.OtpExternal.intTag:
    case com.ericsson.otp.erlang.OtpExternal.smallBigTag:
      return true;
    default:
      return false;
    }
  }

  /**
    Guard method
    @return true if the Term is an OtpErlangList, false otherwize 
  **/
  public boolean isList() {

    if (ObjV == null) {
      switch(tag) {
      case com.ericsson.otp.erlang.OtpExternal.listTag:
      case com.ericsson.otp.erlang.OtpExternal.stringTag:
      case com.ericsson.otp.erlang.OtpExternal.nilTag:
	return true;
      default:
	return false;
      }
    }

    if (ObjV instanceof com.ericsson.otp.erlang.OtpErlangList) 
      return true;

    if (ObjV instanceof com.ericsson.otp.erlang.OtpErlangString) 
      return true;

    return false;
  }


  /**
    Guard method
    @return true if the Term is an OtpErlangString, false otherwize 
  **/
  public boolean isString() {

    if (ObjV == null) {
      switch(tag) {
      case com.ericsson.otp.erlang.OtpExternal.stringTag:
      case com.ericsson.otp.erlang.OtpExternal.nilTag:
	return true;
      default:
	try {
	  stringV = extract_string();
	  return true;
	} catch (Exception e) {
	  return false;
	}
      }
    }

    if (ObjV instanceof com.ericsson.otp.erlang.OtpErlangString) 
      return true;

    try {
      stringV = extract_string();
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  /**
    Guard method
    @return true if the Term is an OtpErlangInteger or an OtpErlangFloat, false otherwize 
  **/
  public boolean isNumber() {
    switch(tag) {
    case com.ericsson.otp.erlang.OtpExternal.smallIntTag:
    case com.ericsson.otp.erlang.OtpExternal.intTag:
    case com.ericsson.otp.erlang.OtpExternal.smallBigTag:
    case com.ericsson.otp.erlang.OtpExternal.floatTag:
	return true;
    default :
      return false;
    }
  }


  /**
    Guard method
    @return true if the Term is an OtpErlangPid or Pid, false otherwize 
  **/
  public boolean isPid() {
    
    if (ObjV == null) {
      if (tag == com.ericsson.otp.erlang.OtpExternal.pidTag)
	return true;
      
      return false;
    }

    return (ObjV instanceof com.ericsson.otp.erlang.OtpErlangPid) ;
  }


  /**
    Guard method
    @return true if the Term is an OtpErlangPort or Port, false otherwize 
  **/
  public boolean isPort() {
    if (ObjV == null) {
      if (tag == com.ericsson.otp.erlang.OtpExternal.portTag)
	return true;
      
      return false;
    }

    return (ObjV instanceof com.ericsson.otp.erlang.OtpErlangPort);
  }


  /**
    Guard method
    @return true if the Term is an OtpErlangRef, false otherwize 
  **/
  public boolean isReference() {    
    if (ObjV == null) {      
      switch(tag) {
      case com.ericsson.otp.erlang.OtpExternal.refTag:
      case com.ericsson.otp.erlang.OtpExternal.newRefTag:
	return true;
      default :
	return false;
      }
    }

    return (ObjV instanceof com.ericsson.otp.erlang.OtpErlangRef) ;
  }


  /**
    Guard method
    @return true if the Term is an OtpErlangTuple, false otherwize 
  **/
  public boolean isTuple() {
    if (ObjV == null) {      
      switch(tag) {
      case com.ericsson.otp.erlang.OtpExternal.smallTupleTag:
      case com.ericsson.otp.erlang.OtpExternal.largeTupleTag:
	return true;
      default :
	return false;
      }
    }
    
    return (ObjV instanceof com.ericsson.otp.erlang.OtpErlangTuple);
  }


  /**
    Guard method
    @return true if the Term is an OtpErlangBinary, false otherwize 
  **/
  public boolean isBinary() {
    if (ObjV == null) {
      if (tag == com.ericsson.otp.erlang.OtpExternal.binTag)
	return true;
      
      return false;
    }

    return (ObjV instanceof com.ericsson.otp.erlang.OtpErlangBinary);
  }

 


  // Equal function
  /**
    Term comparison method
    @return true if the input Term is equal to the object, false otherwize 
  **/
  public boolean equal(Term _any) {
    
    try {

      /* Pids */
      if ((PidV != null) && (_any.PidV != null))
	if (PidV.equal(_any.PidV))
	  return true;

      /* Refs */
      if ((RefV != null) && (_any.RefV != null))
	if (RefV.equal(_any.RefV))
	  return true;

      /* Ports */
      if ((PortV != null) && (_any.PortV != null))
	if (PortV.equals(_any.PortV))
	  return true;

      /* strings */
      if ((stringV != null) && (_any.stringV != null))
	if (stringV.equals(_any.stringV))
	  return true;

      /* atoms and booleans */
      if ((atomV != null) && (_any.atomV != null))
	if (atomV.equals(_any.atomV))
	  return true;

      /* booleans */
      if (atomV != null) 
	if (_any.booleanV == Boolean.valueOf(atomV).booleanValue())
	  return true;

      if (_any.atomV != null) 
	if (booleanV == Boolean.valueOf(_any.atomV).booleanValue())
	  return true;     

      /* integer types plus floating point types */
      double _ownNS = 
	longV+doubleV;
      
      double _othersNS = 
	_any.longV+_any.doubleV;

      if ((equal(_ownNS,_othersNS)) &&
	  (!equal(_ownNS,0)))			     
	return true;

      /* All together, 0 or false */
      if ((equal(_ownNS,_othersNS)) &&
	  booleanV == _any.booleanV)			     
	return true;
	  
  
      return false;

    } catch (Exception e) {
      e.printStackTrace();
      return false;
    } 
  }

  /**
    Writes the value of Term to a stream 
  **/
  public void write_value(com.ericsson.otp.erlang.OtpOutputStream _os) 
    throws java.lang.Exception {
      
      if ((tcV == null) && (ObjV != null))
	_os.write_any(ObjV); // Type not generated by IC
      
      else {
	
	switch(tcV.kind().value()) {
	  
	case TCKind._tk_octet :
	case TCKind._tk_char : 
	case TCKind._tk_wchar : 
	case TCKind._tk_short :
	case TCKind._tk_ushort : 
	case TCKind._tk_long :
	case TCKind._tk_longlong :
	case TCKind._tk_ulong :
	case TCKind._tk_ulonglong :
	  _os.write_long(longV);
	  break;

	case TCKind._tk_float :
	  _os.write_double(doubleV);
	  break;

	case TCKind._tk_double :
	  _os.write_double(doubleV);
	  break;

	case TCKind._tk_boolean : 
	  _os.write_boolean(booleanV);
	  break;

	case TCKind._tk_string :
	case TCKind._tk_wstring :
	  _os.write_string(stringV);
	  break;

	case TCKind._tk_atom :
	  _os.write_atom(stringV);
	  break;
	
	case TCKind._tk_struct:
	  if (isPid())
	    PidHelper.marshal(_os, PidV);
	  else {
	    if (isReference())
	      RefHelper.marshal(_os, RefV);
	    else {
	      if (isPort())
		PortHelper.marshal(_os, PortV);
	      else
		_os.write(os.toByteArray());
	    }
	  }
	  break;

	case TCKind._tk_union:
	case TCKind._tk_array:
	case TCKind._tk_sequence:
	case TCKind._tk_enum:
	  _os.write(os.toByteArray());
	  break;
	  
	case TCKind._tk_void : 
	  _os.write_atom("ok");
	  break;
	  
	  /*
	   * Not supported types
	   */
	default:
	  throw new java.lang.Exception("BAD KIND");
	}
      }
  }
  
  
  
  /*
   * Insert and extract each primitive type
   */
  

  /* short */

  /**
    Short value extractor method
    @return short, the value of Term 
  **/
  public short extract_short() 
    throws java.lang.Exception {

      if (tcV == null)
	return (short) longV;
      
      if (tcV.kind() == TCKind.tk_short)
	return (short) longV;
      
      throw new java.lang.Exception("");
  }
  
  /**
    Short value insertion method
  **/
  public void insert_short(short s) {
    longV = s;
    tag = com.ericsson.otp.erlang.OtpExternal.intTag;
    tcV = new TypeCode(TCKind.tk_short);
  };

  /**
    Short value insertion method
  **/
  public void insert_short(long l) {
    longV = l;
    tag = com.ericsson.otp.erlang.OtpExternal.intTag;
    tcV = new TypeCode(TCKind.tk_short);
  };
  
  
  /* long */

  /**
    Long value extractor method
    @return int, the value of Term 
  **/
  public int extract_long() 
    throws java.lang.Exception {

      if (tcV == null)
	return (int) longV;

      if (tcV.kind() == TCKind.tk_long)
	return (int) longV;

      throw new java.lang.Exception("");
  }
  
  /**
    Long value insertion method
  **/
  public void insert_long(int i){
      longV = i;
      tag = com.ericsson.otp.erlang.OtpExternal.intTag;
      tcV = new TypeCode(TCKind.tk_long);
  } 

  /**
    Long value insertion method
  **/
  public void insert_long(long l){
      longV = l;
      tag = com.ericsson.otp.erlang.OtpExternal.intTag;
      tcV = new TypeCode(TCKind.tk_long);
  } 


  /* longlong */

  /**
    Long Long value extractor method
    @return long, the value of Term 
  **/
  public long extract_longlong() 
    throws java.lang.Exception {

      if (tcV == null)
	return longV;

      if (tcV.kind() == TCKind.tk_longlong)
	return longV;

      throw new java.lang.Exception("");
  }
  

  /**
    Long Long value insertion method
  **/
  public void insert_longlong(long l){
      longV = l;
      tag = com.ericsson.otp.erlang.OtpExternal.intTag;
      tcV = new TypeCode(TCKind.tk_longlong);
  } 
  

  /* ushort */

  /**
    Unsigned Short value extractor method
    @return short, the value of Term 
  **/
  public short extract_ushort() 
    throws java.lang.Exception {

      if (tcV == null)
	return (short) longV;

      if (tcV.kind() == TCKind.tk_ushort)
	return (short) longV;
      
      throw new java.lang.Exception("");
  }
   
  /**
    Unsigned Short value insertion method
  **/
  public void insert_ushort(short s){
      longV = s;
      tag = com.ericsson.otp.erlang.OtpExternal.intTag;
      tcV = new TypeCode(TCKind.tk_ushort);
  } 

  /**
    Unsigned Short value insertion method
  **/
  public void insert_ushort(long l){
      longV = l;
      tag = com.ericsson.otp.erlang.OtpExternal.intTag;
      tcV = new TypeCode(TCKind.tk_ushort);
  } 
  

  /* ulong */

  /**
    Unsigned Long value extractor method
    @return int, the value of Term 
  **/
  public int extract_ulong() 
    throws java.lang.Exception{

      if (tcV == null)
	return (int) longV;

      if (tcV.kind() == TCKind.tk_ulong)
	return (int) longV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Unsigned Long value insertion method
    **/
  public void insert_ulong(int i){
    longV = i;
    tag = com.ericsson.otp.erlang.OtpExternal.intTag;
    tcV = new TypeCode(TCKind.tk_ulong);
  } 


  /**
    Unsigned Long value insertion method
  **/
  public void insert_ulong(long l){
    longV = l;
    tag = com.ericsson.otp.erlang.OtpExternal.intTag;
    tcV = new TypeCode(TCKind.tk_ulong);
  } 


  
  /* ulonglong */

  /**
    Unsigned Long Long value extractor method
    @return long, the value of Term 
  **/
  public long extract_ulonglong() 
    throws java.lang.Exception {

      if (tcV == null)
	return longV;

      if (tcV.kind() == TCKind.tk_ulonglong)
	return longV;

      throw new java.lang.Exception("");
  }
  

  /**
    Unsigned Long Long value insertion method
  **/
  public void insert_ulonglong(long l){
      longV = l;
      tag = com.ericsson.otp.erlang.OtpExternal.intTag;
      tcV = new TypeCode(TCKind.tk_ulonglong);
  } 



  /* float */
  /**
    Float value extractor method
    @return float, the value of Term 
  **/
  public float extract_float() 
    throws java.lang.Exception{

      if (tcV == null)
	return (float) doubleV;

      if (tcV.kind() == TCKind.tk_float)
	return (float) doubleV;

      throw new java.lang.Exception("");
  } 


  /**
    Float value insertion method
    **/
  public void insert_float(float f){
      doubleV = f;
      tag = com.ericsson.otp.erlang.OtpExternal.floatTag;
      tcV = new TypeCode(TCKind.tk_float);
  } 

  /**
    Float value insertion method
  **/
  public void insert_float(double f){
      doubleV = f;
      tag = com.ericsson.otp.erlang.OtpExternal.floatTag;
      tcV = new TypeCode(TCKind.tk_float);
  } 
  
  
  /* double */
  /**
    Double value extractor method
    @return double, the value of Term 
    **/
  public double extract_double() 
    throws java.lang.Exception{

      if (tcV == null)
	return doubleV;

      if (tcV.kind() == TCKind.tk_double)
	return doubleV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Double value insertion method
    **/
  public void insert_double(double d){
    doubleV = d;
    tag = com.ericsson.otp.erlang.OtpExternal.floatTag;
    tcV = new TypeCode(TCKind.tk_double);
  } 
  

  /* boolean */
  /**
    Boolean value extractor method
    @return boolean, the value of Term 
    **/
  public boolean extract_boolean() 
    throws java.lang.Exception{
      
      if ((tcV == null) && (atomV != null))
	return Boolean.valueOf(atomV).booleanValue();

      if (tcV.kind() == TCKind.tk_boolean)
	return booleanV;
      
      throw new java.lang.Exception("");
  }
   
  /**
    Boolean value insertion method
    **/
  public void insert_boolean(boolean b){
    booleanV = b;
    tag = com.ericsson.otp.erlang.OtpExternal.atomTag;
    tcV = new TypeCode(TCKind.tk_boolean);
  } 
  

  /* char */
  /**
    Char value extractor method
    @return char, the value of Term 
    **/
  public char extract_char() 
    throws java.lang.Exception{

      if (tcV == null) 
	return (char) longV;

      if (tcV.kind() == TCKind.tk_char)
	return (char) longV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Char value insertion method
    **/
  public void insert_char(char c) {
    longV = c;
    tag = com.ericsson.otp.erlang.OtpExternal.smallIntTag;
    tcV = new TypeCode(TCKind.tk_char);
  } 

  /**
    Char value insertion method
    **/
  public void insert_char(long l) {
    longV = l;
    tag = com.ericsson.otp.erlang.OtpExternal.smallIntTag;
    tcV = new TypeCode(TCKind.tk_char);
  } 
  


    /* wchar */
  /**
    Wchar value extractor method
    @return char, the value of Term 
    **/
  public char extract_wchar() 
    throws java.lang.Exception{

      if (tcV == null) 
	return (char) longV;

      if (tcV.kind() == TCKind.tk_wchar)
	return (char) longV;
      
      throw new java.lang.Exception("");
  } 
  
  /**
    Wchar value insertion method
    **/
  public void insert_wchar(char c) {
    longV = c;
    tag = com.ericsson.otp.erlang.OtpExternal.smallIntTag;
    tcV = new TypeCode(TCKind.tk_wchar);
  } 

  /**
    Wchar value insertion method
    **/
  public void insert_wchar(long l) {
    longV = l;
    tag = com.ericsson.otp.erlang.OtpExternal.smallIntTag;
    tcV = new TypeCode(TCKind.tk_wchar);
  } 


  /* octet */
  /**
    Octet value extractor method
    @return byte, the value of Term 
  **/
  public byte extract_octet() 
    throws java.lang.Exception{

      if (tcV == null) 
	return (byte) longV;

      if (tcV.kind() == TCKind.tk_octet)
	return (byte) longV;
      
      throw new java.lang.Exception("");
  } 
   
  /**
    Octet value insertion method
    **/
  public void insert_octet(byte b){
    longV = b;
    tag = com.ericsson.otp.erlang.OtpExternal.smallIntTag;
    tcV = new TypeCode(TCKind.tk_octet);
  }

  /**
    Octet value insertion method
    **/
  public void insert_octet(long l){
    longV = l;
    tag = com.ericsson.otp.erlang.OtpExternal.smallIntTag;
    tcV = new TypeCode(TCKind.tk_octet);
  }
  


  /* string */

  /**
    String value extractor method
    @return String, the value of Term 
  **/
  public java.lang.String extract_string() 
    throws java.lang.Exception{

      if (tcV == null) {
	if (stringV != null)
	  return stringV;
	else {
	  is = this.extract_Streamable();
	  stringV = is.read_string();
	  return stringV;
	}
      }
      else
	if (tcV.kind() == TCKind.tk_string)
	  return stringV;
      
      throw new java.lang.Exception("");
  }
  
  /**
    String value insertion method
    **/
  public void insert_string(java.lang.String s) {
      stringV = s;
      tag = com.ericsson.otp.erlang.OtpExternal.stringTag;
      tcV = new TypeCode(TCKind.tk_string);
  }


  
  /* wstring */
  /**
    Wstring value extractor method
    @return String, the value of Term 
  **/
  public java.lang.String extract_wstring() 
    throws java.lang.Exception{
      
      if (tcV == null) {
	if (stringV != null)
	  return stringV;
	else {
	  is = this.extract_Streamable();
	  stringV = is.read_string();
	  return stringV;
	}
      }
      else
	if (tcV.kind() == TCKind.tk_wstring)
	  return stringV;
      
      throw new java.lang.Exception("");
  }
  
  /**
    Wstring value insertion method
    **/
  public void insert_wstring(java.lang.String s) {
    stringV = s;
    tag = com.ericsson.otp.erlang.OtpExternal.stringTag;
    tcV = new TypeCode(TCKind.tk_wstring);
  }



  /* atom */
  /**
    Atom value extractor method
    @return atom, the value of Term 
  **/
  public java.lang.String extract_atom() 
    throws java.lang.Exception{

      if ((tcV == null) && (atomV != null))
	return atomV;

      if (tcV.kind() == TCKind.tk_atom)
	return stringV;

      throw new java.lang.Exception("");
  }
  

  /**
    Atom value insertion method
    **/
  public void insert_atom(java.lang.String s) {
      stringV = s;
      tag = com.ericsson.otp.erlang.OtpExternal.atomTag;
      tcV = new TypeCode(TCKind.tk_atom);
  }


  /* Pid */
  /**
    Pid value extractor method
    @return Pid, the value of Term 
  **/
  public Pid extract_Pid() 
    throws java.lang.Exception{
      
      if ((tcV == null) && (PidV != null))
	return PidV;
      
      if (tcV.equal(PidHelper.type()))
	return PidV;
      
      throw new java.lang.Exception("");
  }
  
  
  /**
    Pid value insertion method
    **/
  public void insert_Pid(Pid p) {
      PidV = p;
      tag = com.ericsson.otp.erlang.OtpExternal.pidTag;
      tcV = PidHelper.type();
  }



  /* Ref */
  /**
    Ref value extractor method
    @return Ref, the value of Term 
  **/
  public Ref extract_Ref() 
    throws java.lang.Exception{
      
      if ((tcV == null) && (RefV != null))
	return RefV;
      
      if (tcV.equal(RefHelper.type()))
	return RefV;
      
      throw new java.lang.Exception("");
  }
  
  /**
    Ref value insertion method
    **/
  public void insert_Ref(Ref r) {
      RefV = r;

      if (r.isNewRef())
	tag = com.ericsson.otp.erlang.OtpExternal.newRefTag;
      else
	tag = com.ericsson.otp.erlang.OtpExternal.refTag;

      tcV = RefHelper.type();
  }


  
  /* Port */
  /**
    Port value extractor method
    @return Port, the value of Term 
  **/
  public Port extract_Port() 
    throws java.lang.Exception{
      
      if ((tcV == null) && (PortV != null))
	return PortV;
      
      if (tcV.equal(PortHelper.type()))
	return PortV;
      
      throw new java.lang.Exception("");
  }
  
  /**
    Port value insertion method
    **/
  public void insert_Port(Port p) {
      PortV = p;
      tag = com.ericsson.otp.erlang.OtpExternal.portTag;
      tcV = PortHelper.type();
  }


  /**
    Object Stream extractor method
    @return OtpInputStream, the stream value of Term 
  **/
  public com.ericsson.otp.erlang.OtpInputStream extract_Streamable() {

    if (is == null) {
      if (os == null) {
	if (stringV == null)
	  return null;
	else {
	  // A sequence that become a string !
	  os = new com.ericsson.otp.erlang.OtpOutputStream();
	  os.write_string(stringV);
	  is = new com.ericsson.otp.erlang.OtpInputStream(os.toByteArray());
	}
      }
      else {
	is = new com.ericsson.otp.erlang.OtpInputStream(os.toByteArray());
      }
    }
    
    is.reset();
    return is;
  }

  /**
    Inserts Objects to Term
  **/
  public void insert_Object(com.ericsson.otp.erlang.OtpErlangObject o) {
    ObjV = o;
  }

  /**
    Extract Object value from Term
    @return OtpErlangObject, the Object value of Term 
  **/
  public com.ericsson.otp.erlang.OtpErlangObject extract_Object() {
    return ObjV;
  }


}







































