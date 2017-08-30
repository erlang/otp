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



