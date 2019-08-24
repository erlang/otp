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
 */
#ifndef _EICODE_H
#define _EICODE_H

/*
 * The following functions are used to encode from c native types directly into
 * Erlang external format. To use them, you need
 *
 * - a destination buffer
 * - an index counter
 * - some data
 * - an idea of how you want to represent the data as an Erlang term.
 *
 * You can encode exactly one (1) term into the buffer if you are
 * going to transmit it to Erlang. Do the following:
 *
 * 1. Set your index to 0
 * 2. Encode the version into the buffer: ei_encode_version(buf,&index);
 *    The function has now advanced index so the next item can be encoded.
 * 3. Encode your term:
 *
 * Encoding non-compound types (i.e. not lists or tuples) is
 * straightforward. Just do it!
 *
 * Encoding tuples is done by first encoding the tuple header (it
 * contains the arity) and then encoding the tuple elements in
 * sequence.
 *
 * Encoding lists is done by first encoding the list header (it
 * contains the arity) and then encoding the list elements in
 * sequence, and finally encoding an empty list. 
 *
 * After all this, the index counter will tell you how much buffer you
 * used. If you really need to know in advance how big the buffer
 * should be, go through the same steps but with a NULL buffer. No
 * attempt will be made to modify the buffer, but index will be
 * updated as though you really did encode something.
 */
 
/* encode the given object into buf[index] as 'type'. 0 is
 * returned and index is updated to the position for the next item. if
 * buf == NULL, no data is actually copied, but index is updated to
 * indicate the number of bytes that would have been necessary.
 */

/* FIXME where do we put these..... */

erlang_big *ei_alloc_big(int arity);
void ei_free_big(erlang_big *b);


#endif /* _EICODE_H */
