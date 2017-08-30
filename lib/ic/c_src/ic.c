/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#include <ic.h>

static int oe_send(CORBA_Environment *env);

void CORBA_free(void *p)
{
    if (p != NULL) 
	free(p); 
}


CORBA_char *CORBA_string_alloc(CORBA_unsigned_long len) 
{
    return (CORBA_char *) malloc(len+1);
}


CORBA_wchar *CORBA_wstring_alloc(CORBA_unsigned_long len)
{
    return (CORBA_wchar *) malloc(len*(__OE_WCHAR_SIZE_OF__+1));
}


CORBA_Environment *CORBA_Environment_alloc(int inbufsz, int outbufsz)
{
    CORBA_Environment *env;

    env = malloc(sizeof(CORBA_Environment));

    if (env != NULL) {

	/* CORBA */
	env->_major = CORBA_NO_EXCEPTION;

	/* Set by user */
	env->_fd= -1;
	env->_inbufsz = inbufsz;
	env->_inbuf = malloc(inbufsz);
	env->_outbufsz = outbufsz;
	env->_outbuf = malloc(outbufsz);
	env->_memchunk = __OE_MEMCHUNK__;
	env->_regname[0] = '\0';
	env->_to_pid = NULL;
	env->_from_pid = NULL;

	/* Set by client or server */
	env->_iin = 0;
	env->_iout = 0;
	env->_operation[0] = '\0';
	env->_received = 0;
	/* env->_caller  */
	/* env->_unique */
	env->_exc_id = NULL;
	env->_exc_value = NULL;
	env->_ref_counter_1 = 0;
	env->_ref_counter_2 = 0;
	env->_ref_counter_3 = 0;
    }

    return env;
}

#if 0
/* NOT EXPORTED SO FAR */
void CORBA_Environment_free(CORBA_Environment *env)
{

    CORBA_free(env->_inbuf);
    CORBA_free(env->_outbuf);
    CORBA_exception_free(env);
    CORBA_free(env);
} 
#endif


CORBA_char *CORBA_exception_id(CORBA_Environment *env)
{

    return env->_exc_id;
}

void *CORBA_exception_value(CORBA_Environment *env)
{

    return env->_exc_value;
}

void CORBA_exception_free(CORBA_Environment *env)
{
 
    /* Setting major value */
    env->_major=CORBA_NO_EXCEPTION;

    /* Freeing storage */
    CORBA_free(env->_exc_id);
    CORBA_free(env->_exc_value);
    env->_exc_id = env->_exc_value = NULL;
}

void CORBA_exc_set(CORBA_Environment *env, 
		   CORBA_exception_type Major, 
		   CORBA_char *Id, 
		   CORBA_char *Value)
{
    int ilen,vlen;

    /* Create exception only if exception not already set */
    if (env->_major == CORBA_NO_EXCEPTION) {

	/* Counting lengths */
	ilen = strlen(Id)+1;
	vlen = strlen(Value)+1;
    
	/* Allocating storage */
	env->_exc_id = (CORBA_char *) malloc(ilen);
	env->_exc_value = (CORBA_char *) malloc(vlen);
    
	/* Initiating */
	env->_major = Major;
	strcpy(env->_exc_id,Id);
	strcpy(env->_exc_value,Value);
    }
}

#define ERLANG_REF_NUM_SIZE  18
#define ERLANG_REF_MASK      (~(~((unsigned int)0) << ERLANG_REF_NUM_SIZE))

/* Initiating message reference */
void ic_init_ref(CORBA_Environment *env, erlang_ref *ref)
{

    strcpy(ref->node, erl_thisnodename());

    ref->len = 3;

    ++env->_ref_counter_1;
    env->_ref_counter_1 &= ERLANG_REF_MASK;
    if (env->_ref_counter_1 == 0)
	if (++env->_ref_counter_2 == 0) 
	    ++env->_ref_counter_3;
    ref->n[0] = env->_ref_counter_1;
    ref->n[1] = env->_ref_counter_2;
    ref->n[2] = env->_ref_counter_3;
    
    ref->creation = erl_thiscreation();
}

/* Comparing message references */
int ic_compare_refs(erlang_ref *ref1, erlang_ref *ref2)
{
    int i;

    if(strcmp(ref1->node, ref2->node) != 0) 
	return -1;
 
    if (ref1->len != ref2->len) 
	return -1;

    for (i = 0; i < ref1->len; i++)
	if (ref1->n[i] != ref2->n[i])
	    return -1;
    
    return 0; 
}

/* Length counter for wide strings */
int ic_wstrlen(CORBA_wchar * p)
{
    int len = 0;

    while(1) {
	if (p[len] == 0)
	    return len;

	len+=1;
    }
}


/* Wide string compare function */
int ic_wstrcmp(CORBA_wchar * ws1, CORBA_wchar * ws2)
{
    int index = 0;

    while(1) {
	if (ws1[index] == ws2[index]) {

	    if (ws1[index] == 0)
		return 0;
      
	    index += 1;

	} else 
	    return -1;
    }
}

/* For backward compatibility -- replaced by prepare_request_decoding() */
int ___call_info___(CORBA_Object obj, CORBA_Environment *env)
{
    return oe_prepare_request_decoding(env);
}

/* #define DEBUG_MAP */

#if defined(DEBUG_MAP)

#define PRINT_MAPS(P, M, S) print_maps(P, M, S)
#define PRINT_MAP(T, M)     print_map(T, "", M)

static void print_map(char *title, char *prefix, oe_map_t *map)
{
    if (map == NULL) {
	fprintf(stdout, "%s => NULL\n", title);
	return;
    }

    fprintf(stdout, "%s%s\n", prefix, title);

    {
	int j, len = map->length; 

	fprintf(stdout, "%s  length:     %d\n", prefix, len);
	fprintf(stdout, "%s  operations: 0x%X%d\n", prefix, map->operations);
    
	for (j = 0 ; j < len ; j++) {
	    fprintf(stdout, "%s  operation[%d]:\n", prefix, j);
      
	    if (map->operations[j].interface != NULL) {
		fprintf(stdout, "%s    intf: %s\n", prefix, 
			map->operations[j].interface);
	    } else {
		fprintf(stdout, "%s    intf: NULL\n", prefix);
	    }
	    fprintf(stdout, "%s    name: %s\n", prefix, 
		    map->operations[j].name);
	    fprintf(stdout, "%s    func: 0x%X\n", prefix, 
		    map->operations[j].function);
	}
    }
    fflush(stdout);
}

static void print_maps(char* title, oe_map_t * maps, int size)
{
    int  i;
    char p[64];

    fprintf(stdout, "%s\n", title);

    for (i = 0 ; i < size ; i++) {
	sprintf(p, "map[%d]:", i);
	print_map(p, "  ", &maps[i]);
    }
    fprintf(stdout, "\n");
    fflush(stdout);
}

#else

#define PRINT_MAPS(P, M, S) 
#define PRINT_MAP(T, M)     

#endif /* if defined(DEBUG_MAP) */


/* Generic server switch */
int oe_exec_switch(CORBA_Object obj, CORBA_Environment *env, oe_map_t *map)
{
    /* Setting local variables */
    int res = 0;
    int index = 0;

    /* XXX map may be NULL !! */
    int length = map->length;
    char* op = env->_operation;
    
    PRINT_MAP("switching on map", map);

    /* Initiating exception indicator */
    env->_major = CORBA_NO_EXCEPTION;
    
    if ((res = oe_prepare_request_decoding(env) < 0))
	return res;
#if defined(DEBUG_MAP)
    fprintf(stdout, "looking for operation: %s\n", op); fflush(stdout);
#endif
    for (index = 0; index < length; index++) {
#if defined(DEBUG_MAP)
	fprintf(stdout, "map->operations[%d].name: %s\n",  
		index, map->operations[index].name);  
	fflush(stdout); 
#endif
	if(strcmp(map->operations[index].name, op) == 0) {
#if defined(DEBUG_MAP)
	    fprintf(stdout, "calling map->operations[%d].function: 0x%X\n",
		    index, map->operations[index].function);  
	    fflush(stdout); 
#endif
	    return map->operations[index].function(obj, env);
	}
    }
    /* Bad call */
    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, 
		  "Invalid operation");
    return -1;
}

/* For backward compatibility */
int ___switch___(CORBA_Object obj, CORBA_Environment *env, oe_map_t *map)
{
    return oe_exec_switch(obj, env, map);
}


oe_map_t* oe_merge_maps(oe_map_t *maps, int size)
{ 
    int i, j, length, len, maplen, malloc_size;
    void *memp;
    oe_map_t *merged;

    if ((maps == NULL) || (size <= 0))
	return NULL;

    PRINT_MAPS("merging maps", maps, size);
	
    length = 0;
    for (i = 0; i < size; i++)
	length += (maps[i].length);

    maplen = OE_ALIGN(sizeof(oe_map_t));
    malloc_size = maplen + OE_ALIGN(length*sizeof(oe_operation_t));
    if ((memp = malloc(malloc_size)) == NULL)
	return NULL;

    merged = memp;
    merged->length = length;
    merged->operations = (oe_operation_t *)((char*)memp + maplen);
    	
    for (i = 0, len = 0; i < size; i++) {
	for(j = 0 ; j < maps[i].length; j++)
	    merged->operations[len+j] = maps[i].operations[j];
	len += maps[i].length;
    }
    PRINT_MAP("merged map", merged);
    return merged;
}

/* For backward compatibility */
oe_map_t* ___merge___(oe_map_t *maps, int size)
{
    return oe_merge_maps(maps, size);
}

/* Client send message (Erlang distribution protocol) */
static int oe_send(CORBA_Environment *env)
{
    if (strlen(env->_regname) == 0) { 
	if (ei_send_encoded(env->_fd, env->_to_pid, env->_outbuf,
			    env->_iout) < 0) { 
	    /* XXX Cannot send to peer? */
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, 
			  "Cannot connect to server"); 
	    return -1; 
	}
    } else {
	if (ei_send_reg_encoded(env->_fd, env->_from_pid, 
				env->_regname, env->_outbuf, 
				env->_iout) < 0) {
	    /* XXX Cannot send to peer? */
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, 
			  "Cannot connect to server"); 
	    return -1; 
	}
    }
    return 0;
}

/* Send notification (gen_server client) */
int oe_send_notification(CORBA_Environment *env)
{
    return oe_send(env);
}

/* Send request and receive reply (gen_server client) */
int oe_send_request_and_receive_reply(CORBA_Environment *env)
{
    int msgType = 0;
    erlang_msg msg;

    if (oe_send(env) < 0)
	return -1;

    do { 
	if ((msgType = ei_receive_encoded(env->_fd,
					  &env->_inbuf, 
					  &env->_inbufsz, 
					  &msg, &env->_iin)) < 0) { 
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, MARSHAL,
			  "Cannot decode message"); 
	    return -1; 
	}
    } while (msgType != ERL_SEND && msgType != ERL_REG_SEND); 

    /* Extracting return message header */ 
    if (oe_prepare_reply_decoding(env) < 0) {
	CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, MARSHAL, "Bad message"); 
	return -1;
    }
    return 0;
}

/* Prepare notification encoding (gen_server client) */
int oe_prepare_notification_encoding(CORBA_Environment *env)
{
    env->_iout = 0;  
    oe_ei_encode_version(env);
    oe_ei_encode_tuple_header(env, 2);
    oe_ei_encode_atom(env, "$gen_cast");
    return 0;
}

/* Prepare request encoding (gen_server client) */
int oe_prepare_request_encoding(CORBA_Environment *env)
{
    int error = 0;

    env->_iout = 0;  
    oe_ei_encode_version(env);
    oe_ei_encode_tuple_header(env, 3);
    oe_ei_encode_atom(env, "$gen_call");
    oe_ei_encode_tuple_header(env, 2);
    if ((error = oe_ei_encode_pid(env, env->_from_pid)) < 0)
	return error; 
    if ((error = oe_ei_encode_ref(env, &env->_unique)) < 0)
	return error;
    return 0;
}

/* Prepare reply decoding (gen_server client) */
int oe_prepare_reply_decoding(CORBA_Environment *env)
{
    int error = 0;
    int version = 0;
    erlang_ref unique;

    env->_iin = 0;
    env->_received = 0;

    if ((error = ei_decode_version(env->_inbuf, 
				   &env->_iin, 
				   &version)) < 0)
        return error;
    if ((error = ei_decode_tuple_header(env->_inbuf, 
					&env->_iin, 
					&env->_received)) < 0)
        return error;
    if ((error = ei_decode_ref(env->_inbuf,
			       &env->_iin, 
			       &unique)) < 0)
        return error;
    return ic_compare_refs(&env->_unique, &unique);
}   


/* Prepare request decoding (gen_server server) */
int oe_prepare_request_decoding(CORBA_Environment *env)
{
    char gencall_atom[10];
    int error = 0;
    int version = 0;

    env->_iin = 0;
    env->_received = 0;
    memset(gencall_atom, 0, 10);
    ei_decode_version(env->_inbuf, &env->_iin, &version);
    ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
    ei_decode_atom(env->_inbuf, &env->_iin, gencall_atom);

    if (strcmp(gencall_atom, "$gen_cast") == 0) {
	if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
				    env->_operation)) < 0) {
	    ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
	    if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
					env->_operation)) < 0) { 
		CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, 
			      "Bad Message, cannot extract operation");
		return error;
	    }
	    env->_received -= 1;
	} else
	    env->_received -= 2;
	return 0;
    }
    if (strcmp(gencall_atom, "$gen_call") == 0) {
	ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
	if ((error = ei_decode_pid(env->_inbuf, &env->_iin, 
				   &env->_caller)) < 0) {
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, MARSHAL, 
			  "Bad Message, bad caller identity");
	    return error;
	}
	if ((error = ei_decode_ref(env->_inbuf, &env->_iin, 
				   &env->_unique)) < 0) {
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, MARSHAL, 
			  "Bad Message, bad message reference");
	    return error;
	}
	if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
				    env->_operation)) < 0) {
	    
	    ei_decode_tuple_header(env->_inbuf, &env->_iin, &env->_received);
	    
	    if ((error = ei_decode_atom(env->_inbuf, &env->_iin, 
					env->_operation)) < 0) { 
		CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, 
			      "Bad Message, cannot extract operation");
		return error;
	    }
	    env->_received -= 1;
	    return 0;	  
	}
	else {
	    env->_received -= 2;
	    return 0;
	}
    }
    
    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, MARSHAL, 
		  "Bad message, neither cast nor call");
    return -1;
}

/* Prepare reply encoding (gen_server server) */
int oe_prepare_reply_encoding(CORBA_Environment *env)
{
    env->_iout = 0;
    oe_ei_encode_version(env);
    oe_ei_encode_tuple_header(env, 2);
    oe_ei_encode_ref(env, &env->_unique);
    return 0;
}

/* ---- Function for making it more easy to implement a server */
/* Server receive (possibly) send reply (gen_server server) */

int oe_server_receive(CORBA_Environment *env, oe_map_t *map)
{
    int res = 0, loop = 1;
    erlang_msg msg;

    while (res >= 0 && loop > 0) {
        res = ei_receive_encoded(env->_fd, &env->_inbuf, &env->_inbufsz, 
				 &msg, &env->_iin); 
	switch(res) {
	case ERL_SEND:
	case ERL_REG_SEND:
	    oe_exec_switch(NULL, env, map);
	    switch(env->_major) {
	    case CORBA_NO_EXCEPTION:
		break;
	    case CORBA_SYSTEM_EXCEPTION:
		/* XXX stderr */
		fprintf(stderr, "Request failure, reason : %s\n", 
			(char *) CORBA_exception_value(env));
		CORBA_exception_free(env);
		break;
	    default: /* Should not happen */
		CORBA_exception_free(env);
		break;
	    }
	    /* send reply */
	    /* XXX We are required to set env->_iout = 0 if oneway?? */
	    if (env->_iout > 0) 
		ei_send_encoded(env->_fd, &env->_caller, env->_outbuf, 
				env->_iout);
	    loop = 0;
	    break;
	case ERL_TICK:
	    break;
	default: 
	    /* XXX */
	    if (res < 0) {
		fprintf(stderr, "Result negative: %d\n", res);
		loop = 0;
	    }
	    break;
	}
    }	

    return 0;
}

