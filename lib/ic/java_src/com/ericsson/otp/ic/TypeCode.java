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
 * The TypeCode class for Java IDL
 *
 */
package com.ericsson.otp.ic;

/**
  The TypeCode class is the implementation of the OMG-IDL TypeCode type.
  **/

public class TypeCode {

  private TCKind _kind;
  private java.lang.String _id,_name;
  private int _length,_member_count,_default_index;
  private TypeCode _member_type,_discriminator_type,_content_type;
  private Any _member_label;
  private boolean extracted;
  private TypeCode _members[];
  private java.lang.String _member_names[];
  private Any _member_labels[];
  


  /*
   * Constructors
   */
  public TypeCode() {
    extracted = false;
    _members = null;
    _member_names = null;
    _member_labels = null;
    _kind = null;
    _id = null;
    _name = null;
    _length = -1;
    _member_count = -1;
    _default_index = -1;
    _member_type = null;
    _content_type = null;
    _discriminator_type = null;
    _member_label = null;
  }

  public TypeCode(TCKind __kind) {
    _kind = __kind;
  }


  /*
   * Operation "TypeCode::equal" 
   */

  /**
    Comparisson method for TypeCode.
    @return true if the input TypeCode value equals the value of the current object, false otherwize
    **/
  public boolean equal(TypeCode tc) {

    try {
      
      TCKind tck = tc.kind();
      
      switch (tck.value()) {
	
      case TCKind._tk_short:
      case TCKind._tk_long:
      case TCKind._tk_longlong:
      case TCKind._tk_ushort:
      case TCKind._tk_ulong:
      case TCKind._tk_ulonglong:
      case TCKind._tk_float:
      case TCKind._tk_double:
      case TCKind._tk_boolean:
      case TCKind._tk_char:
      case TCKind._tk_wchar:
      case TCKind._tk_octet:
      case TCKind._tk_string:
      case TCKind._tk_wstring:
      case TCKind._tk_any:
      case TCKind._tk_void:
      case TCKind._tk_atom:
	
	return (tck.value() == _kind.value());
	
      case TCKind._tk_struct:
	
	if((tc.id().compareTo(_id) == 0) &&
	   (tc.name().compareTo(_name) == 0) &&
	   (tc.member_count() == _member_count)){
	  
	  for (int i = 0; i < _member_count; i++)
	    if (!tc.member_type(i).equal(_members[i]))
	      return false;

	  return true;
	}
	else
	  return false;
      
      case TCKind._tk_union:
	
	if((tc.id().compareTo(_id) == 0) &&
	   (tc.name().compareTo(_name) == 0) &&
	   (tc.member_count() == _member_count) &&
	   (tc.discriminator_type().equal(_discriminator_type))){
	  
	  for (int i = 0; i < _member_count; i++)
	    if ((!tc.member_type(i).equal(_members[i])) &&
		(tc.member_name(i).compareTo(_member_names[i]) != 0))
	      return false;
	  
	  return true;
	}
	else
	  return false; 

      case TCKind._tk_sequence:
      case TCKind._tk_array:
	
	if((tck.value() == _kind.value()) &&
	   (tc.content_type().equal(_content_type)))
	  return true;
	else
	  return false;

      case TCKind._tk_enum:
	if((tck.value() == _kind.value()) &&
	   (tc.member_count() == _member_count)) {
	  
	  for (int i = 0; i < _member_count; i++)
	    if (tc.member_name(i).compareTo(_member_names[i]) != 0)
	      return false;
	  
	  return true;
	}
	else
	  return false;
	
	// Not used in real
      case TCKind._tk_null:
      case TCKind._tk_TypeCode:
      case TCKind._tk_Principal:
      case TCKind._tk_objref:
      case TCKind._tk_alias:
      case TCKind._tk_except:
      case TCKind._tk_longdouble:
      case TCKind._tk_fixed:

	return (tck.value() == _kind.value());
	
      default :
	return false;
	
      }
    } catch (Exception e) {
      return false;
    }
      
  }
  
  
  /*
   * Operation "TypeCode::kind" 
   */
  
  /**
    Accessor method for the TCKind value of TypeCode.
    @return TCKind, the TCKind value of the TypeCode object.
    **/
  public TCKind kind() {
    return _kind;
  }

  /**
    Insertion method for the TCKind value of TypeCode.
    Sets the TCKind value of the object.
    **/
  public void kind(TCKind __kind) {
    _kind = __kind;
  }

  /**
    Insertion method for the TCKind value of TypeCode.
    Sets the TCKind value of the object.
    **/
  public static TCKind kind(java.lang.String atom) 
    throws java.lang.Exception {
    
    if (atom.equals("tk_null"))
      return TCKind.tk_null;     
    else
      if (atom.equals("tk_void"))
	return TCKind.tk_void;     
    else
      if (atom.equals("tk_short"))
	return TCKind.tk_short;     
    else
      if (atom.equals("tk_long"))
	return TCKind.tk_long;     
    else
      if (atom.equals("tk_ushort"))
	return TCKind.tk_ushort;     
    else
      if (atom.equals("tk_ulong"))
	return TCKind.tk_ulong;     
    else 
      if (atom.equals("tk_float"))
	return TCKind.tk_float;     
    else
      if (atom.equals("tk_double"))
	return TCKind.tk_double;     
    else
      if (atom.equals("tk_boolean"))
	return TCKind.tk_boolean;     
    else
      if (atom.equals("tk_char"))
	return TCKind.tk_char;     
    else
      if (atom.equals("tk_octet"))
	return TCKind.tk_octet;     
    else
      if (atom.equals("tk_any"))
	return TCKind.tk_any;     
    else
      if (atom.equals("tk_TypeCode"))
	return TCKind.tk_TypeCode;     
    else
      if (atom.equals("tk_Principal"))
	return TCKind.tk_Principal;     
    else
      if (atom.equals("tk_objref"))
	return TCKind.tk_objref;     
    else
      if (atom.equals("tk_struct"))
	return TCKind.tk_struct;     
    else
      if (atom.equals("tk_union"))
	return TCKind.tk_union;     
    else
      if (atom.equals("tk_enum"))
	return TCKind.tk_enum;     
    else
      if (atom.equals("tk_string"))
	return TCKind.tk_string;     
    else
      if (atom.equals("tk_sequence"))
	return TCKind.tk_sequence;     
    else
      if (atom.equals("tk_array"))
	return TCKind.tk_array;     
    else
      if (atom.equals("tk_alias"))
	return TCKind.tk_alias;     
    else
      if (atom.equals("tk_except"))
	return TCKind.tk_except;     
    else
      if (atom.equals("tk_longlong"))
	return TCKind.tk_longlong;     
    else
      if (atom.equals("tk_ulonglong"))
	return TCKind.tk_ulonglong;   
    else
      if (atom.equals("tk_longdouble"))
	return TCKind.tk_longdouble;     
    else
      if (atom.equals("tk_wchar"))
	return TCKind.tk_wchar;
    else
      if (atom.equals("tk_wstring"))
	return TCKind.tk_wstring;     
    else
      if (atom.equals("tk_fixed"))
	return TCKind.tk_fixed;
    else
      if (atom.equals("tk_atom"))
	return TCKind.tk_atom;    
    else
      throw new java.lang.Exception("BAD KIND");
  
  }
  
  
  
  /*
   * Operation "TypeCode::id" 
   */

  /**
    Accessor method for the id value of TypeCode.
    @return String, the id value of TypeCode object
    **/
  public java.lang.String id()
    throws java.lang.Exception{
      
      if (_id == null) 
	throw new java.lang.Exception("BAD KIND");

      return _id;
  }


  /**
    Insertion method for the id value of TypeCode.
    Sets the id value of the object.
    **/
  public void id(java.lang.String __id) {
      
      _id = __id;
  }
  

  
  /*
   * Operation "TypeCode::name" 
   */

  /**
    Accessor method for the name value of TypeCode.
    @return String, the name value of TypeCode object
    **/
  public java.lang.String name()
    throws java.lang.Exception{
      
      if (_name == null) 
	throw new java.lang.Exception("BAD KIND");
      
      return _name;
  }
  
  /**
    Insertion method for the name value of TypeCode.
    Sets the name value of the object.
    **/
  public void name(java.lang.String __name) {
      _name = __name;
  }


  
  /*
   * Operation "TypeCode::member_count" 
   */
  
  /**
    Accessor method for the member number value of TypeCode.
    @return int, the number of members of TypeCode object
    **/
  public int member_count()
    throws java.lang.Exception{
      
      if (_member_count == -1) 
	throw new java.lang.Exception("BAD KIND");
      
      return _member_count;
  }

  /**
    Insertion method for the member number value of TypeCode.
    Sets the number of members value of the object.
    **/
  public void member_count(int __member_count) {

    switch(_kind.value()) {
    case TCKind._tk_struct:
      _members = new TypeCode[__member_count];
      _member_names = new java.lang.String[__member_count];
      _member_count = __member_count;
      break;
    case TCKind._tk_union:
      _members = new TypeCode[__member_count];
      _member_names = new java.lang.String[__member_count];
      _member_labels = new Any[__member_count];
      _member_count = __member_count;
      break;
    case TCKind._tk_enum:
      _member_names = new java.lang.String[__member_count];
      _member_count = __member_count;
      break;
    default :
      // Do nothing
    }
  }

  
  /*
   * Operation "TypeCode::member_name" 
   */
  
  /**
    Member name accessor method for TypeCode.
    @return String, the name value of the member of the TypeCode object
    on the selected index
    **/
  public java.lang.String member_name(int __index)
    throws java.lang.Exception{
      
      return _member_names[__index];
  }
  
  /**
    Insertion method for the indexed member name of TypeCode.
    Sets the name of a member value of the object at the selected index..
    **/
  public void member_name(int __index, java.lang.String __member_name) {
    _member_names[__index] = __member_name;
  }

  
  /*
   * Operation "TypeCode::member_type" 
   */
  
  /**
    Member type accessor method for TypeCode.
    @return TypeCOde, the type of the member of the TypeCode object
    on the selected index
    **/
  public TypeCode member_type(int __index)
    throws java.lang.Exception{
      
      return _members[__index];
  }

  /**
    Insertion method for the indexed member type of TypeCode.
    Sets the type of a member value of the object at the selected index..
    **/
  public void member_type(int __index, TypeCode __member_type) {
    _members[__index] = __member_type;
  }
  
  
  /*
   * Operation "TypeCode::member_label" 
   */

  /**
    Member label accessor method for TypeCode.
    @return Any, the label of the member of the TypeCode object
    on the selected index
    **/
  public Any member_label(int __index)
    throws java.lang.Exception{
      
      return _member_labels[__index];
  }
  
  /**
    Insertion method for the indexed member label of TypeCode.
    Sets the label of a member value of the object at the selected index.
    **/
  public void member_label(int __index, Any __member_label) {
    _member_labels[__index] = __member_label;
  }


  /*
   * Operation "TypeCode::discriminator_type" 
   */
  
  /**
    Discriminator type accessor method for TypeCode.
    @return TypeCode, the type of the discriminator of the TypeCode object
    **/
  public TypeCode discriminator_type()
    throws java.lang.Exception{
      
      if (_discriminator_type == null)
	throw new java.lang.Exception("BAD KIND");
      
      return _discriminator_type;
  }

  /**
    Insertion method for the type of the discriminator value of TypeCode.
    Sets the discriminator type value of the object.
    **/
  public void discriminator_type(TypeCode __discriminator_type) {
    _discriminator_type = __discriminator_type;
  }
  
  
  /*
   * Operation "TypeCode::default_index" 
   */

  /**
    Index accessor method for TypeCode.
    @return int, the default index value of the member of the TypeCode object
    **/
  public int default_index()
    throws java.lang.Exception{
      
      if (_default_index == -1)
	throw new java.lang.Exception("BAD KIND");
      
      return _default_index;
  }

  /**
    Insertion method for the default index value of TypeCode.
    Sets the default index value of the object.
    **/
  public void default_index(int __default_index) {
    _default_index = __default_index;
  }
  
  
  /*
   * Operation "TypeCode::length" 
   */

  /**
    Length accessor method for TypeCode.
    @return int, the length of the TypeCode object
    **/
  public int length()
    throws java.lang.Exception{
      
      if (_length == -1)
	throw new java.lang.Exception("BAD KIND");
      
      return _length;
  }
  
  /**
    Insertion method for the length value of TypeCode.
    Sets the length value of the object.
    **/
  public void length(int __length) {
    _length = __length;
  }

  
  /*
   * Operation "TypeCode::content_type" 
   */
  
  /**
    Content type accessor method for TypeCode.
    @return TypeCode, the content type of the TypeCode object
    **/
  public TypeCode content_type()
    throws java.lang.Exception {
      
      if (_content_type == null)
	throw new java.lang.Exception("BAD KIND");
      
      return _content_type;
  }

  /**
    Insertion method for the content type value of TypeCode.
    Sets the content type value of the object.
    **/
  public void content_type(TypeCode __content_type) {
    _content_type = __content_type;
  }


  /**
    Marshal operation for TypeCode. 
    **/
  public static void marshal(com.ericsson.otp.erlang.OtpOutputStream _os, TypeCode _tc) 
    throws java.lang.Exception {

      TypeCode memberTC = null;
      int len = -1;
			       
      switch(_tc.kind().value()) {

      case TCKind._tk_short :
	_os.write_atom("tk_short");
	break;
      case TCKind._tk_ushort : 
	_os.write_atom("tk_ushort");
	break;
      case TCKind._tk_long :
	_os.write_atom("tk_long");
	break;
      case TCKind._tk_longlong :
	_os.write_atom("tk_longlong");
	break;
      case TCKind._tk_ulong :
	_os.write_atom("tk_ulong");
	break;
      case TCKind._tk_ulonglong :
	_os.write_atom("tk_ulonglong");
	break;
      case TCKind._tk_float :
	_os.write_atom("tk_float");
	break;
      case TCKind._tk_double :
	_os.write_atom("tk_double");
	break;
      case TCKind._tk_boolean : 
	_os.write_atom("tk_boolean");
	break;
      case TCKind._tk_char : 
	_os.write_atom("tk_char");
	break;
      case TCKind._tk_wchar : 
	_os.write_atom("tk_wchar");
	break;
      case TCKind._tk_octet :
	_os.write_atom("tk_octet");
	break;
      case TCKind._tk_string :
	_os.write_tuple_head(2);
	_os.write_atom("tk_string");
	_os.write_ulong(_tc.length());
	break;
      case TCKind._tk_wstring :
	_os.write_tuple_head(2);
	_os.write_atom("tk_wstring");
	_os.write_ulong(_tc.length());
	break;
      case TCKind._tk_struct:	
	len = _tc.member_count();
	_os.write_tuple_head(4);
	_os.write_atom("tk_struct");
	_os.write_string(_tc.id());
	_os.write_string(_tc.name());	
	// Member list
	_os.write_list_head(len);
	for(int i=0; i<len; i++) {
	  _os.write_tuple_head(2);
	  _os.write_string(_tc.member_name(i));
	  marshal(_os,_tc.member_type(i));
	}
	_os.write_nil();
	break;
      case TCKind._tk_union:
	len = _tc.member_count();
	_os.write_tuple_head(6);
	_os.write_atom("tk_union");
	_os.write_string(_tc.id());
	_os.write_string(_tc.name());
	marshal(_os,_tc.discriminator_type());
	_os.write_int(_tc.default_index());
	// Member list
	_os.write_list_head(len);
	for(int i=0; i<len; i++) {
	  _os.write_tuple_head(3);
	  _tc.member_label(i).write_value(_os);
	  _os.write_string(_tc.member_name(i));
	  marshal(_os,_tc.member_type(i));
	}
	_os.write_nil();
	break;
      case TCKind._tk_sequence:
	_os.write_tuple_head(3);
	_os.write_atom("tk_sequence");
	marshal(_os,_tc.content_type());
	_os.write_int(_tc.length());
	break;
      case TCKind._tk_array:
	_os.write_tuple_head(3);
	_os.write_atom("tk_array");
	marshal(_os,_tc.content_type());
	_os.write_int(_tc.length());
	break;
      case TCKind._tk_enum:
	len = _tc.member_count();
	_os.write_tuple_head(4);
	_os.write_atom("tk_enum");
	_os.write_string(_tc.id());
	_os.write_string(_tc.name());
	_os.write_list_head(len);
	for(int i=0; i<len; i++)
	  _os.write_string(_tc.member_name(i));
	_os.write_nil();
	break;
      case TCKind._tk_any:
	_os.write_atom("tk_any");
	break;
      case TCKind._tk_void : 
	_os.write_atom("tk_void");
	break;
	/*
	 * Not supported types
	 */
      default :
	throw new java.lang.Exception("Unsupported type");
	
      }
			   	       
  }


  /**
    Unmarshal operation for TypeCode.
    @return TypeCode, the TypeCode read from the input stream.
   **/
  public static TypeCode unmarshal(com.ericsson.otp.erlang.OtpInputStream _is)
    throws java.lang.Exception {
      
      TypeCode _tc, __member;
      TCKind __kind;
      int __len;
      int __tag = _is.peek();
      
      switch(__tag) {
      case (com.ericsson.otp.erlang.OtpExternal.atomTag):
      case (com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag):
      case (com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag):
	__kind = TypeCode.kind(_is.read_atom());
	
	switch(__kind.value()) {
	case TCKind._tk_short :
	case TCKind._tk_ushort : 
	case TCKind._tk_long :
	case TCKind._tk_longlong :
	case TCKind._tk_ulong :
	case TCKind._tk_ulonglong :
	case TCKind._tk_float :
	case TCKind._tk_double :
	case TCKind._tk_boolean :
	case TCKind._tk_char : 
	case TCKind._tk_wchar : 
	case TCKind._tk_octet :
	case TCKind._tk_void : 
	case TCKind._tk_any : 
	  _tc = new TypeCode();
	  _tc.kind(__kind);
	  
	  return _tc;
	default :
	  throw new java.lang.Exception("Unsupported type");
	}
	
      case (com.ericsson.otp.erlang.OtpExternal.smallTupleTag):
      case (com.ericsson.otp.erlang.OtpExternal.largeTupleTag):
	
	__len = _is.read_tuple_head();
	__tag = _is.peek();
	
	switch(__tag) {

	case (com.ericsson.otp.erlang.OtpExternal.atomTag):
	case (com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag):
	case (com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag):
	  
	  __kind = TypeCode.kind(_is.read_atom());
	  _tc = new TypeCode();
	  _tc.kind(__kind);
	  
	  switch(__kind.value()) {
	  
	  case TCKind._tk_string :
	    _tc.length((int)_is.read_ulong());
	    return _tc;

	  case TCKind._tk_wstring :
	    _tc.length((int)_is.read_ulong());
	    return _tc;

	  case TCKind._tk_struct:
	    
	    _tc.id(_is.read_string());
	    _tc.name(_is.read_string());
	    __len = _is.read_list_head();
	    _tc.member_count(__len);
	    
	    for(int i=0; i<__len; i++) {
	      _is.read_tuple_head();
	      _tc.member_name(i,_is.read_string());
	      _tc.member_type(i,unmarshal(_is));
	    }
	    _is.read_nil();
	    
	    return _tc;
	    
	    
	  case TCKind._tk_union:
	    
	    _tc.id(_is.read_string());
	    _tc.name(_is.read_string());	    
	    _tc.discriminator_type(unmarshal(_is));
	    _tc.default_index(_is.read_int());
	    __len = _is.read_list_head();
	    _tc.member_count(__len);

	    for(int i=0; i<__len; i++) {
	      _is.read_tuple_head();
	      
	      __tag = _is.peek();
	      Any __label = new Any();
	      TypeCode __label_type = new TypeCode();

	      __label_type.kind(com.ericsson.otp.ic.TCKind.tk_long);
	      __label.type(__label_type);

	      switch(__tag) {
	      case (com.ericsson.otp.erlang.OtpExternal.stringTag):
		java.lang.String __enum = _is.read_string();
		__label.insert_string(__enum);
		break;
	      case (com.ericsson.otp.erlang.OtpExternal.atomTag):
	      case (com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag):
	      case (com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag):

		java.lang.String __default = _is.read_atom();
		__label.insert_atom(__default);
		break;
	      default:
		__label.insert_long(_is.read_int());
	      }

	      _tc.member_label(i,__label);
	      _tc.member_name(i,_is.read_string());
	      _tc.member_type(i,unmarshal(_is));
	    }
	    _is.read_nil();

	    return _tc;


	  case TCKind._tk_sequence:
	    _tc.content_type(unmarshal(_is));
	    _tc.length(_is.read_int());
	    return _tc;


	  case TCKind._tk_array:
	    _tc.content_type(unmarshal(_is));
	    _tc.length(_is.read_int());
	    return _tc;


	  case TCKind._tk_enum:
	    
	    _tc.id(_is.read_string());
	    _tc.name(_is.read_string());
	    __len = _is.read_list_head();
	    _tc.member_count(__len);
	    
	    for(int i=0; i<__len; i++)
	      _tc.member_name(i,_is.read_string());
	
	    _is.read_nil();
	    
	    return _tc;
	    
	  default:
	    throw new java.lang.Exception("Unsupported type");
	    
	  }
	  
	default:
	  throw new java.lang.Exception("Unsupported type");
	}
	
      }
      
      return null;
  }

}


