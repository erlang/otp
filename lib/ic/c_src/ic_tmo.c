/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

static int oe_send_tmo(CORBA_Environment *env, unsigned int ms);

/* Client send message (Erlang distribution protocol) */
static int oe_send_tmo(CORBA_Environment *env, unsigned int ms)
{
    if (strlen(env->_regname) == 0) { 
	if (ei_send_encoded_tmo(env->_fd, env->_to_pid, env->_outbuf,
			    env->_iout, ms) < 0) { 
	    /* XXX Cannot send to peer? */
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, 
			  "Cannot connect to server"); 
	    return -1; 
	}
    } else {
	if (ei_send_reg_encoded_tmo(env->_fd, env->_from_pid, 
				env->_regname, env->_outbuf, 
				env->_iout, ms) < 0) {
	    /* XXX Cannot send to peer? */
	    CORBA_exc_set(env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, 
			  "Cannot connect to server"); 
	    return -1; 
	}
    }
    return 0;
}

/* Send notification (gen_server client) */
int oe_send_notification_tmo(CORBA_Environment *env, unsigned int send_ms)
{
    return oe_send_tmo(env, send_ms);
}

/* Send request and receive reply (gen_server client) */
int oe_send_request_and_receive_reply_tmo(CORBA_Environment *env, 
					  unsigned int send_ms,
					  unsigned int recv_ms)
{
    int msgType = 0;
    erlang_msg msg;

    if (oe_send_tmo(env, send_ms) < 0)
	return -1;

    do { 
	if ((msgType = ei_receive_encoded_tmo(env->_fd,
					      &env->_inbuf, 
					      &env->_inbufsz, 
					      &msg, &env->_iin,
					      recv_ms)) < 0) { 
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

/* Server receive (possibly) send reply (gen_server server) */

int oe_server_receive_tmo(CORBA_Environment *env, oe_map_t *map, 
			  unsigned int send_ms, 
			  unsigned int recv_ms)
{
    int res = 0, loop = 1;
    erlang_msg msg;

    while (res >= 0 && loop > 0) {
        res = ei_receive_encoded_tmo(env->_fd, &env->_inbuf, &env->_inbufsz, 
				 &msg, &env->_iin, recv_ms); 
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
		ei_send_encoded_tmo(env->_fd, &env->_caller, env->_outbuf, 
				env->_iout, send_ms);
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

