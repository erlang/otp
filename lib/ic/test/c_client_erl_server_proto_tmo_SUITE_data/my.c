/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2011. All Rights Reserved.
 * 
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
 * %CopyrightEnd%
 *
 */
#include "ic.h"
#include "m_i.h"

int my_prepare_notification_encoding(CORBA_Environment *env)
{
    return oe_prepare_notification_encoding(env);
}

int my_send_notification_tmo(CORBA_Environment *env, unsigned int send_ms)
{
    return oe_send_notification_tmo(env, send_ms);
}

int my_prepare_request_encoding(CORBA_Environment *env) 
{
    return oe_prepare_request_encoding(env);
}

int my_send_request_and_receive_reply_tmo(CORBA_Environment *env, 
				      unsigned int send_ms, 
				      unsigned int recv_ms)
{
    return oe_send_request_and_receive_reply_tmo(env, send_ms, recv_ms);
}

int my_prepare_reply_decoding(CORBA_Environment *env)
{
    return oe_prepare_reply_decoding(env);
}



