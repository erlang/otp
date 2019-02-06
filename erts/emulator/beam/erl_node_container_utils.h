/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2018. All Rights Reserved.
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

#ifndef ERL_NODE_CONTAINER_UTILS_H__
#define ERL_NODE_CONTAINER_UTILS_H__

#include "erl_ptab.h"

/*
 * Note regarding node containers:
 *
 * The term "node container" is used as a group name (internally in
 * the emulator) for the Erlang data types that contain a reference
 * to a node, i.e. pids, ports, and references.
 *
 * Node containers are divided into internal and external node containers.
 * An internal node container refer to the current incarnation of the
 * node which it reside on. An external node container refer to
 * either a remote node (i.e. a node with another node name than the 
 * node name of the node on which the node container resides on) or another
 * incarnation of the node which the node container resides on (i.e
 * another node with the same node name but another creation).
 *
 * External node containers are boxed data types. The data of an
 * external node container is stored on the heap together with a pointer
 * to an element in the node table (see erl_term.h and erl_node_tables.h).
 * The elements of the node table are garbage collected by reference
 * counting (much like refc binaries, and funs in the separate heap case).
 *
 * Internal node containers are stored as they previously were (in R8)
 * with the exception of changed internal layouts (see erl_term.h), i.e.
 * internal pid, and internal port are immediate data types and internal
 * reference is a boxed data type. An internal node container have an
 * implicit reference to the 'erts_this_node' element in the node table.
 *
 * OBSERVE! Pids doesn't use fixed size 'serial' and 'number' fields any
 * more. Previously the 15 bit 'number' field of a pid was used as index
 * into the process table, and the 3 bit 'serial' field was used as a
 * "wrap counter". The needed number of bits for index into the process
 * table is now calculated at startup and the rest (of the 18 bits used)
 * are used as 'serial'. In the "emulator interface" (external format,
 * list_to_pid, etc) the least significant 15 bits are presented as
 * 'number' and the most significant 3 bits are presented as 'serial',
 * though. The makro internal_pid_index() can be used for retrieving
 * index into the process table. Do *not* use the result from
 * pid_number() as an index into the process table. The pid_number() and
 * pid_serial() (and friends) fetch the old fixed size 'number' and
 * 'serial' fields.
 */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Node containers                                                         *
\*                                                                         */

#define node_container_node_name(x)	(is_external(x)			\
					 ? external_node_name((x))	\
					 : internal_node_name((x)))
#define node_container_creation(x) 	(is_external(x)			\
					 ? external_creation((x))	\
					 : internal_creation((x)))
#define node_container_dist_entry(x) 	(is_external(x)			\
					 ? external_dist_entry((x))	\
					 : internal_dist_entry((x)))
#define node_container_channel_no(x)	(is_external((x))		\
					 ? external_channel_no((x))	\
					 : internal_channel_no((x)))
#define is_node_container(x)		(is_external((x)) || is_internal((x)))
#define is_not_node_container(x)	(!is_node_container((x)))

#define is_internal(x) 			(is_internal_pid((x))		\
					 || is_internal_port((x))	\
					 || is_internal_ref((x)))
#define is_not_internal(x) 		(!is_internal((x)))
#define internal_node_name(x)		(erts_this_node->sysname)
#define external_node_name(x)		external_node((x))->sysname
#define internal_creation(x)		(erts_this_node->creation)
#define external_creation(x)		(external_node((x))->creation)
#define internal_dist_entry(x)		(erts_this_node->dist_entry)
#define external_dist_entry(x)		(external_node((x))->dist_entry)

/*
 * For this node (and previous incarnations of this node), 0 is used as
 * channel no. For other nodes, the atom index of the atom corresponding
 * to the node name is used as channel no.
 *
 * (We used to assert for correct node names, but we removed that assertion
 *  as it is possible to sneak in incorrect node names for instance using
 *  the external format.)
 */
#define dist_entry_channel_no(x)				\
  ((x) == erts_this_dist_entry					\
   ? ((Uint) 0)							\
   : (ASSERT(is_atom((x)->sysname)),			        \
      (Uint) atom_val((x)->sysname)))
#define internal_channel_no(x) ((Uint) ERST_INTERNAL_CHANNEL_NO)
#define external_channel_no(x) \
  (dist_entry_channel_no(external_dist_entry((x))))

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Pids                                                                    *
\*                                                                         */

extern ErtsPTab erts_proc;

#define make_internal_pid(D)		erts_ptab_make_id(&erts_proc, \
							  (D), \
							  _TAG_IMMED1_PID)

#define internal_pid_index(PID)		(ASSERT(is_internal_pid((PID))), \
					 erts_ptab_id2pix(&erts_proc, (PID)))

#define internal_pid_data(PID)		(ASSERT(is_internal_pid((PID))), \
					 erts_ptab_id2data(&erts_proc, (PID)))

#define internal_pid_number(x)		_GET_PID_NUM(internal_pid_data((x)))
#define internal_pid_serial(x)		_GET_PID_SER(internal_pid_data((x)))

#define internal_pid_node_name(x)	(internal_pid_node((x))->sysname)
#define external_pid_node_name(x)	(external_pid_node((x))->sysname)
#define internal_pid_creation(x)	(internal_pid_node((x))->creation)
#define external_pid_creation(x)	(external_pid_node((x))->creation)
#define internal_pid_dist_entry(x)	(internal_pid_node((x))->dist_entry)
#define external_pid_dist_entry(x)	(external_pid_node((x))->dist_entry)

#define internal_pid_channel_no(x)	(internal_channel_no((x)))
#define external_pid_channel_no(x)	(external_channel_no((x)))

#define pid_data_words(x)		(is_internal_pid((x))		\
					 ? internal_pid_data_words((x))	\
					 : external_pid_data_words((x)))
#define pid_number(x)			(is_internal_pid((x))		\
					 ? internal_pid_number((x))	\
					 : external_pid_number((x)))
#define pid_serial(x)			(is_internal_pid((x))		\
					 ? internal_pid_serial((x))	\
					 : external_pid_serial((x)))
#define pid_node(x)			(is_internal_pid((x))		\
					 ? internal_pid_node((x))	\
					 : external_pid_node((x)))
#define pid_node_name(x)		(is_internal_pid((x))		\
					 ? internal_pid_node_name((x))	\
					 : external_pid_node_name((x)))
#define pid_creation(x)			(is_internal_pid((x))		\
					 ? internal_pid_creation((x))	\
					 : external_pid_creation((x)))
#define pid_dist_entry(x)		(is_internal_pid((x))		\
					 ? internal_pid_dist_entry((x))	\
					 : external_pid_dist_entry((x)))
#define pid_channel_no(x)		(is_internal_pid((x))		\
					 ? internal_pid_channel_no((x))	\
					 : external_pid_channel_no((x)))
#define is_pid(x)			(is_internal_pid((x))		\
					 || is_external_pid((x)))
#define is_not_pid(x)			(!is_pid(x))

/*
 * Maximum number of processes. We want the number to fit in a SMALL on
 * 32-bit CPU.
 */

#define ERTS_MAX_PROCESSES		(ERTS_PTAB_MAX_SIZE-1)
#define ERTS_MAX_PID_DATA		((1 << _PID_DATA_SIZE) - 1)
#define ERTS_MAX_PID_NUMBER		((1 << _PID_NUM_SIZE) - 1)
#define ERTS_MAX_PID_SERIAL		((1 << _PID_SER_SIZE) - 1)

#define ERTS_PROC_BITS			(_PID_SER_SIZE + _PID_NUM_SIZE)

#define ERTS_INVALID_PID		ERTS_PTAB_INVALID_ID(_TAG_IMMED1_PID)

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Ports                                                                   *
\*                                                                         */

extern ErtsPTab erts_port;

#define make_internal_port(D)		erts_ptab_make_id(&erts_port, \
							  (D), \
							  _TAG_IMMED1_PORT)

#define internal_port_index(PRT)	(ASSERT(is_internal_port((PRT))), \
					 erts_ptab_id2pix(&erts_port, (PRT)))

#define internal_port_data(PRT)		(ASSERT(is_internal_port((PRT))), \
					 erts_ptab_id2data(&erts_port, (PRT)))

#define internal_port_number(x) _GET_PORT_NUM(internal_port_data((x)))

#define internal_port_node_name(x)	(internal_port_node((x))->sysname)
#define external_port_node_name(x)	(external_port_node((x))->sysname)
#define internal_port_creation(x)	(internal_port_node((x))->creation)
#define external_port_creation(x)	(external_port_node((x))->creation)
#define internal_port_dist_entry(x)	(internal_port_node((x))->dist_entry)
#define external_port_dist_entry(x)	(external_port_node((x))->dist_entry)

#define internal_port_channel_no(x)	(internal_channel_no((x)))
#define external_port_channel_no(x)	(external_channel_no((x)))

#define port_data_words(x)		(is_internal_port((x))		\
					 ? internal_port_data_words((x))\
					 : external_port_data_words((x)))
#define port_number(x)			(is_internal_port((x))		\
					 ? internal_port_number((x))	\
					 : external_port_number((x)))
#define port_node(x)			(is_internal_port((x))		\
					 ? internal_port_node((x))	\
					 : external_port_node((x)))
#define port_node_name(x)		(is_internal_port((x))		\
					 ? internal_port_node_name((x))	\
					 : external_port_node_name((x)))
#define port_creation(x)		(is_internal_port((x))		\
					 ? internal_port_creation((x))	\
					 : external_port_creation((x)))
#define port_dist_entry(x)		(is_internal_port((x))		\
					 ? internal_port_dist_entry((x))\
					 : external_port_dist_entry((x)))
#define port_channel_no(x)		(is_internal_port((x))		\
					 ? internal_port_channel_no((x))\
					 : external_port_channel_no((x)))

#define is_port(x)			(is_internal_port((x))		\
					 || is_external_port((x)))
#define is_not_port(x)			(!is_port(x))

/* Highest port-ID part in a term of type Port 
   Not necessarily the same as current maximum port table size
   which defines the maximum number of simultaneous Ports
   in the Erlang node. ERTS_MAX_PORTS is a hard upper limit.
*/
#define ERTS_MAX_PORTS			(ERTS_PTAB_MAX_SIZE-1)
#define ERTS_MAX_PORT_DATA		((1 << _PORT_DATA_SIZE) - 1)
#define ERTS_MAX_PORT_NUMBER		((1 << _PORT_NUM_SIZE) - 1)

#define ERTS_PORTS_BITS			(_PORT_NUM_SIZE)

#define ERTS_INVALID_PORT		ERTS_PTAB_INVALID_ID(_TAG_IMMED1_PORT)

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Refs                                                                    *
\*                                                                         */

#define internal_ref_no_numbers(x)	ERTS_REF_NUMBERS
#define internal_ref_numbers(x)		(is_internal_ordinary_ref((x)) \
					 ? internal_ordinary_ref_numbers((x)) \
					 : (ASSERT(is_internal_magic_ref((x))), \
					    internal_magic_ref_numbers((x))))
#if defined(ARCH_64)

#define external_ref_no_numbers(x)					\
  (external_ref_data((x))[0])
#define external_thing_ref_no_numbers(thing)			        \
  (external_thing_ref_data(thing)[0])
#define external_ref_numbers(x)						\
  (&external_ref_data((x))[1])
#define external_thing_ref_numbers(thing)				\
  (&external_thing_ref_data(thing)[1])


#else

#define external_ref_no_numbers(x)	(external_ref_data_words((x)))
#define external_thing_ref_no_numbers(t) (external_thing_ref_data_words((t)))
#define external_ref_numbers(x)		(external_ref_data((x)))
#define external_thing_ref_numbers(t)   (external_thing_ref_data((t)))

#endif

#define internal_ref_node_name(x)	(internal_ref_node((x))->sysname)
#define external_ref_node_name(x)	(external_ref_node((x))->sysname)
#define internal_ref_creation(x)	(internal_ref_node((x))->creation)
#define external_ref_creation(x)	(external_ref_node((x))->creation)
#define internal_ref_dist_entry(x)	(internal_ref_node((x))->dist_entry)
#define external_ref_dist_entry(x)	(external_ref_node((x))->dist_entry)


#define internal_ref_channel_no(x)	(internal_channel_no((x)))
#define external_ref_channel_no(x)	(external_channel_no((x)))

#define ref_no_numbers(x)		(is_internal_ref((x))		\
					 ? internal_ref_no_numbers((x))\
					 : external_ref_no_numbers((x)))
#define ref_numbers(x)			(is_internal_ref((x))		\
					 ? internal_ref_numbers((x))	\
					 : external_ref_numbers((x)))
#define ref_node(x)			(is_internal_ref((x))		\
					 ? internal_ref_node(x)		\
					 : external_ref_node((x)))
#define ref_node_name(x)		(is_internal_ref((x))		\
					 ? internal_ref_node_name((x))	\
					 : external_ref_node_name((x)))
#define ref_creation(x)			(is_internal_ref((x))		\
					 ? internal_ref_creation((x))	\
					 : external_ref_creation((x)))
#define ref_dist_entry(x)		(is_internal_ref((x))		\
					 ? internal_ref_dist_entry((x))	\
					 : external_ref_dist_entry((x)))
#define ref_channel_no(x)		(is_internal_ref((x))		\
					 ? internal_ref_channel_no((x))	\
					 : external_ref_channel_no((x)))
#define is_ref(x)			(is_internal_ref((x))		\
					 || is_external_ref((x)))
#define is_not_ref(x)			(!is_ref(x))

#endif
