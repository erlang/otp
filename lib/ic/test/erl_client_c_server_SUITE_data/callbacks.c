/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
#include <stdio.h>
#include <stdlib.h>
#ifndef __WIN32__
#  include <unistd.h>
#endif
#include <string.h>
#include <ctype.h>
#include <ic.h>
#include <erl_interface.h>
#include <ei.h>
#include "m_i__s.h"



/* OK */

void my_void_test(CORBA_Object oe_obj, 
		  CORBA_Environment *oe_env)
{
    /* printf("void test !\n"); */
}
 
m_i_void_test__rs* m_i_void_test__cb(CORBA_Object oe_obj, 
				     CORBA_Environment *oe_env)
{
    return (m_i_void_test__rs*) (my_void_test);
}



/* OK */

void my_long_test(CORBA_Object oe_obj, 
		  long* a, 
		  long* b, 
		  long* c, 
		  CORBA_Environment *oe_env)
{
    /* printf("long test !\n"); */
}
 
 
m_i_long_test__rs* m_i_long_test__cb(CORBA_Object oe_obj, 
				     long* a, 
				     long* b, 
				     long* c, 
				     CORBA_Environment *oe_env)
{ 
    *a = *b;
    *c = *b;
    return (m_i_long_test__rs*) (my_long_test);
}

/* OK */

void my_longlong_test(CORBA_Object oe_obj, 
		      CORBA_long_long* a, 
		      CORBA_long_long* b, 
		      CORBA_long_long* c, 
		      CORBA_Environment *oe_env)
{
    /* printf("long test !\n"); */
}
 
m_i_longlong_test__rs* m_i_longlong_test__cb(CORBA_Object oe_obj, 
					     CORBA_long_long* a, 
					     CORBA_long_long* b, 
					     CORBA_long_long* c, 
					     CORBA_Environment *oe_env)
{ 
    *a = *b;
    *c = *b;
    return (m_i_longlong_test__rs*) (my_longlong_test);
}

/* OK */
void my_ulong_test(CORBA_Object oe_obj, 
		   unsigned long* a, 
		   unsigned long* b, 
		   unsigned long* c, 
		   CORBA_Environment *oe_env)
{
    /* printf("ulong test !\n"); */
}
 
m_i_ulong_test__rs* m_i_ulong_test__cb(CORBA_Object oe_obj, 
				       unsigned long* a, 
				       unsigned long* b, 
				       unsigned long* c, 
				       CORBA_Environment *oe_env)
{
    *a = *b;
    *c = *b;
    return (m_i_ulong_test__rs*) (my_ulong_test);
}

/* OK */
void my_ulonglong_test(CORBA_Object oe_obj,
		       CORBA_unsigned_long_long* a,
		       CORBA_unsigned_long_long* b,
		       CORBA_unsigned_long_long* c,
		       CORBA_Environment *oe_env)
{
    /* printf("ulong test !\n"); */
}
 
m_i_ulonglong_test__rs* m_i_ulonglong_test__cb(CORBA_Object oe_obj,
					       CORBA_unsigned_long_long* a,
					       CORBA_unsigned_long_long* b,
					       CORBA_unsigned_long_long* c,
					       CORBA_Environment *oe_env)
{
    *a = *b;
    *c = *b;
    return (m_i_ulonglong_test__rs*) (my_ulonglong_test);
}

m_i_ushort_test__rs* m_i_ushort_test__cb(CORBA_Object oe_obj,
					 unsigned short* a,
					 unsigned short* b,
					 unsigned short* c,
					 CORBA_Environment *oe_env)
{
    *a = *b;
    *c = *b;
    return (m_i_ushort_test__rs*) NULL;
}


/* OK */
void my_double_test(CORBA_Object oe_obj,
		    double* a,
		    double* b,
		    double* c,
		    CORBA_Environment *oe_env)
{
    /* printf("double test !\n"); */
}
 
m_i_double_test__rs* m_i_double_test__cb(CORBA_Object oe_obj,
					 double* a,
					 double* b,
					 double* c,
					 CORBA_Environment *oe_env)
{
    *a = *b;
    *c = *b;
    return (m_i_double_test__rs*) (my_double_test);
}

/* OK */
m_i_char_test__rs* m_i_char_test__cb(CORBA_Object oe_obj,
				     char* a,
				     char* b,
				     char* c,
				     CORBA_Environment *oe_env)
{
    m_i_char_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}


/* OK */
m_i_wchar_test__rs* m_i_wchar_test__cb(CORBA_Object oe_obj,
				       CORBA_wchar* a,
				       CORBA_wchar* b,
				       CORBA_wchar* c,
				       CORBA_Environment *oe_env)
{
    m_i_wchar_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}

/* OK */
m_i_octet_test__rs* m_i_octet_test__cb(CORBA_Object oe_obj,
				       char* a,
				       char* b,
				       char* c,
				       CORBA_Environment *oe_env)
{
    m_i_octet_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}

/* OK */
m_i_bool_test__rs* m_i_bool_test__cb(CORBA_Object oe_obj,
				     CORBA_boolean* a,
				     CORBA_boolean* b,
				     CORBA_boolean* c,
				     CORBA_Environment *oe_env)
{
    m_i_bool_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}

/* OK */
void my_struct_test(CORBA_Object oe_obj,
		    m_b* a,
		    m_b* b,
		    m_b* c,
		    CORBA_Environment *oe_env)
{
    /* printf("struct test !\n"); */
}
 
m_i_struct_test__rs* m_i_struct_test__cb(CORBA_Object oe_obj,
					 m_b* a,
					 m_b* b,
					 m_b* c,
					 CORBA_Environment *oe_env)
{
    *a = *b;
    *c = *b;
    return (m_i_struct_test__rs*) (my_struct_test); 
}

/* OK */
m_i_struct2_test__rs* m_i_struct2_test__cb(CORBA_Object oe_obj,
					   m_es* a,
					   m_es* b,
					   m_es* c,
					   CORBA_Environment *oe_env)
{
    m_i_struct2_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}

/* OK */
/* XXX Commented out
m_i_struct3_test__rs* m_i_struct3_test__cb(CORBA_Object oe_obj,
					   m_simple* a,
					   m_simple* b,
					   m_simple* c,
					   CORBA_Environment *oe_env)
{
  m_i_struct3_test__rs* rs = NULL; 
  *a = *b;
  *c = *b;
  return rs;
}
*/

/* OK */
m_i_seq1_test__rs* m_i_seq1_test__cb(CORBA_Object oe_obj,
				     m_bseq** a,
				     m_bseq* b,
				     m_bseq** c,
				     CORBA_Environment *oe_env)
{
    m_i_seq1_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}


/* OK */
m_i_seq2_test__rs* m_i_seq2_test__cb(CORBA_Object oe_obj,
				     m_aseq** a,
				     m_aseq* b,
				     m_aseq** c,
				     CORBA_Environment *oe_env)
{
    m_i_seq2_test__rs* rs = NULL; 
  
    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_seq3_test__rs* m_i_seq3_test__cb(CORBA_Object oe_obj,
				     m_lseq** a,
				     m_lseq* b,
				     m_lseq** c,
				     CORBA_Environment *oe_env)
{
    m_i_seq3_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_seq4_test__rs* m_i_seq4_test__cb(CORBA_Object oe_obj,
				     m_ssstr3** a,
				     m_ssstr3* b,
				     m_ssstr3** c,
				     CORBA_Environment *oe_env)
{
    m_i_seq4_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_seq5_test__rs* m_i_seq5_test__cb(CORBA_Object oe_obj,
				     m_ssarr3** a,
				     m_ssarr3* b,
				     m_ssarr3** c,
				     CORBA_Environment *oe_env)
{
    m_i_seq5_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_array1_test__rs* m_i_array1_test__cb(CORBA_Object oe_obj,
					 m_arr1 a,
					 m_arr1 b,
					 m_arr1 c,
					 CORBA_Environment *oe_env)
{
    int i;
    m_i_array1_test__rs* rs = NULL; 
  
    for (i = 0; i < 500; i++) {
	a[i] = b[i]; 
	c[i] = b[i];
    }
    return rs;
}

/* OK */
m_i_array2_test__rs* m_i_array2_test__cb(CORBA_Object oe_obj,
					 m_dd a,
					 m_dd b,
					 m_dd c,
					 CORBA_Environment *oe_env)
{
    int i,j;
    m_i_array2_test__rs* rs = NULL; 
  
    for (i = 0; i < 2; i++)
	for (j = 0; j < 3; j++) {
	    a[i][j] = b[i][j];
	    c[i][j] = b[i][j];
	}
    return rs;
}


/* OK */
m_i_enum_test__rs* m_i_enum_test__cb(CORBA_Object oe_obj,
				     m_fruit* a,
				     m_fruit* b,
				     m_fruit* c,
				     CORBA_Environment *oe_env)
{
    m_i_enum_test__rs* rs = NULL; 
  
    *a = *b;
    *c = *b;
    return rs;
}

/* OK */
m_i_string1_test__rs* m_i_string1_test__cb(CORBA_Object oe_obj,
					   char ** a,
					   char * b,
					   char ** c,
					   CORBA_Environment *oe_env)
{
    m_i_string1_test__rs* rs = NULL; 

    /*printf("\nString in ------> %s\n\n",b);*/
    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_string2_test__rs* m_i_string2_test__cb(CORBA_Object oe_obj,
					   m_sseq** a,
					   m_sseq* b,
					   m_sseq** c,
					   CORBA_Environment *oe_env)
{
    m_i_string2_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_string3_test__rs* m_i_string3_test__cb(CORBA_Object oe_obj,
					   char ** a,
					   char * b,
					   char ** c,
					   CORBA_Environment *oe_env)
{
    m_i_string3_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}

m_i_string4_test__rs* m_i_string4_test__cb(CORBA_Object oe_obj,
					   m_strRec** a,
					   m_strRec* b,
					   m_strRec** c,
					   CORBA_Environment *oe_env)
{
    *a = b;
    *c = b;
 
    return (m_i_string4_test__rs*) NULL; 
}

/* OK */
m_i_wstring1_test__rs* m_i_wstring1_test__cb(CORBA_Object oe_obj,
					     CORBA_wchar ** a,
					     CORBA_wchar * b,
					     CORBA_wchar ** c,
					     CORBA_Environment *oe_env)
{
    int tmp;
    m_i_wstring1_test__rs* rs = NULL; 

    /*printf("\nString in ------> %s\n\n",b);*/

    for(tmp = 0; tmp < 5; tmp++) 
	fprintf(stderr,"\np[%d] = %ld\n", tmp, b[tmp]);
    *a = b;
    *c = b;
    return rs;
}


/* OK */
m_i_pid_test__rs* m_i_pid_test__cb(CORBA_Object oe_obj,
				   erlang_pid* a,
				   erlang_pid* b,
				   erlang_pid* c,
				   CORBA_Environment *oe_env)
{
    m_i_pid_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}

/* OK */
m_i_port_test__rs* m_i_port_test__cb(CORBA_Object oe_obj,
				     erlang_port* a,
				     erlang_port* b,
				     erlang_port* c,
				     CORBA_Environment *oe_env)
{
    m_i_port_test__rs* rs = NULL; 

    strcpy((*a).node,(*b).node);
    (*a).id = (*b).id;
    (*a).creation = 0;

    strcpy((*c).node,(*b).node);
    (*c).id = (*b).id;
    (*c).creation = 0;
    return rs;
}

/* OK */
m_i_ref_test__rs* m_i_ref_test__cb(CORBA_Object oe_obj,
				   erlang_ref* a,
				   erlang_ref* b,
				   erlang_ref* c,
				   CORBA_Environment *oe_env)
{

    m_i_ref_test__rs* rs = NULL; 

    strcpy((*a).node,(*b).node);
    /*(*a).id = (*b).id;*/
    (*a).len = (*b).len;
    (*a).n[0] = (*b).n[0];
    (*a).n[1] = (*b).n[1];
    (*a).n[2] = (*b).n[2];
    (*a).creation = 0;

    strcpy((*c).node,(*b).node);
    /*(*c).id = (*b).id;*/
    (*c).len = (*b).len;
    (*c).n[0] = (*b).n[0];
    (*c).n[1] = (*b).n[1];
    (*c).n[2] = (*b).n[2];
    (*c).creation = 0;
    return rs;
}

/* OK */
m_i_term_test__rs* m_i_term_test__cb(CORBA_Object oe_obj,
				     ETERM** a,
				     ETERM** b,
				     ETERM** c,
				     CORBA_Environment *oe_env)
{
    m_i_term_test__rs* rs = NULL; 

    *a = *b;
    *c = *b;
    return rs;
}

m_i_typedef_test__rs* m_i_typedef_test__cb(CORBA_Object oe_obj,
					   long* a,
					   ETERM** b,
					   erlang_port* c,
					   ETERM** d ,
					   erlang_port* e,
					   CORBA_Environment *oe_env)
{
    m_i_typedef_test__rs* rs = NULL; 
  
    *d = *b;
    strcpy((*e).node,(*c).node);
    (*e).id = (*c).id;
    (*e).creation = 0;
    *a = 4711;
    return rs;
}

/* OK */
m_i_inline_sequence_test__rs* m_i_inline_sequence_test__cb(
    CORBA_Object oe_obj,
    m_s** a,
    m_s* b,
    m_s** c,
    CORBA_Environment *oe_env)
{
    m_i_inline_sequence_test__rs* rs = NULL; 

    *a = b;
    *c = b;
    return rs;
}

/* OK */
m_i_term_sequence_test__rs* m_i_term_sequence_test__cb(
    CORBA_Object oe_obj,
    m_etseq** a,
    m_etseq* b,
    m_etseq** c,
    CORBA_Environment *oe_env)
{
    m_i_term_sequence_test__rs* rs = NULL; 

    *a = b;
    *c = b; 
    return rs;
}


/* OK */
m_i_term_struct_test__rs* m_i_term_struct_test__cb(CORBA_Object oe_obj,
						   m_et* a,
						   m_et* b,
						   m_et* c,
						   CORBA_Environment *oe_env)
{
    m_i_term_struct_test__rs* rs = NULL; 
  
    *a = *b;
    *c = *b;
    return rs;
}

