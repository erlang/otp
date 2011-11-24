/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
 */

#ifndef ERL_NODE_CONTAINER_UTILS_H__
#define ERL_NODE_CONTAINER_UTILS_H__

#include "erl_term.h"

/*
 * Note regarding node containers:
 *
 * The term "node container" is used as a group name (internally in
 * the emulator) for the Erlang data types that contain a reference
 * to a node, i.e. pids, ports, and references.
 *
 * Observe! The layouts of the node container data types have been
 * changed in R9. 
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
 * Due to the R9 changes in layouts of node containers there are room to
 * store more data than previously. Today (R9) this extra space is unused,
 * but it is planned to be used in the future. For example only 18 bits
 * are used for data in a pid but there is room for 28 bits of data (on a
 * 32-bit machine). Some preparations have been made in the emulator for
 * usage of this extra space.
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

extern int erts_use_r9_pids_ports;

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
   : (ASSERT_EXPR(is_atom((x)->sysname)),			\
      (Uint) atom_val((x)->sysname)))
#define internal_channel_no(x) ((Uint) ERST_INTERNAL_CHANNEL_NO)
#define external_channel_no(x) \
  (dist_entry_channel_no(external_dist_entry((x))))

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Pids                                                                    *
\*                                                                         */

#define internal_pid_index(x)		(internal_pid_data((x))	\
					 & erts_process_tab_index_mask)

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

#define ERTS_MAX_R9_PROCESSES		(1 << ERTS_R9_PROC_BITS)

/*
 * Maximum number of processes. We want the number to fit in a SMALL on
 * 32-bit CPU.
 */

#define ERTS_MAX_PROCESSES ((1L << 27)-1)
#if (ERTS_MAX_PROCESSES > MAX_SMALL)
# error "The maximum number of processes must fit in a SMALL."
#endif

#define ERTS_MAX_PID_DATA		((1 << _PID_DATA_SIZE) - 1)
#define ERTS_MAX_PID_NUMBER		((1 << _PID_NUM_SIZE) - 1)
#define ERTS_MAX_PID_SERIAL		((1 << _PID_SER_SIZE) - 1)
#define ERTS_MAX_PID_R9_SERIAL		((1 << _PID_R9_SER_SIZE) - 1)

#define ERTS_R9_PROC_BITS		(_PID_R9_SER_SIZE + _PID_NUM_SIZE)
#define ERTS_PROC_BITS			(_PID_SER_SIZE + _PID_NUM_SIZE)

#define ERTS_INVALID_PID		make_internal_pid(ERTS_MAX_PID_DATA)

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Ports                                                                   *
\*                                                                         */

#define internal_port_index(x)		(internal_port_data((x))	\
					 & erts_port_tab_index_mask)

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
   Not necessarily the same as the variable erts_max_ports
   which defines the maximum number of simultaneous Ports
   in the Erlang node. ERTS_MAX_PORTS is a hard upper limit.
*/
#define ERTS_MAX_R9_PORTS		(1 << ERTS_R9_PORTS_BITS)
#define ERTS_MAX_PORTS			(1 << ERTS_PORTS_BITS)

#define ERTS_MAX_PORT_DATA		((1 << _PORT_DATA_SIZE) - 1)
#define ERTS_MAX_PORT_NUMBER		((1 << _PORT_NUM_SIZE) - 1)

#define ERTS_R9_PORTS_BITS		(_PORT_R9_NUM_SIZE)
#define ERTS_PORTS_BITS			(_PORT_NUM_SIZE)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Refs                                                                    *
\*                                                                         */

#if defined(ARCH_64) && !HALFWORD_HEAP

#define internal_ref_no_of_numbers(x)					\
  (internal_ref_data((x))[0])
#define internal_thing_ref_no_of_numbers(thing)			        \
  (internal_thing_ref_data(thing)[0])
#define internal_ref_numbers(x)						\
  (&internal_ref_data((x))[1])
#define internal_thing_ref_numbers(thing)				\
  (&internal_thing_ref_data(thing)[1])
#define external_ref_no_of_numbers(x)					\
  (external_ref_data((x))[0])
#define external_thing_ref_no_of_numbers(thing)			        \
  (external_thing_ref_data(thing)[0])
#define external_ref_numbers(x)						\
  (&external_ref_data((x))[1])
#define external_thing_ref_numbers(thing)				\
  (&external_thing_ref_data(thing)[1])


#else

#define internal_ref_no_of_numbers(x)	(internal_ref_data_words((x)))
#define internal_thing_ref_no_of_numbers(t) (internal_thing_ref_data_words(t))
#define internal_ref_numbers(x)		(internal_ref_data((x)))
#define internal_thing_ref_numbers(t)   (internal_thing_ref_data(t))
#define external_ref_no_of_numbers(x)	(external_ref_data_words((x)))
#define external_thing_ref_no_of_numbers(t) (external_thing_ref_data_words((t)))
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

#define ref_data_words(x)		(is_internal_ref((x))		\
					 ? internal_ref_data_words((x))	\
					 : external_ref_data_words((x)))
#define ref_data(x)			(is_internal_ref((x))		\
					 ? internal_ref_data((x))	\
					 : external_ref_data((x)))
#define ref_no_of_numbers(x)		(is_internal_ref((x))		\
					 ? internal_ref_no_of_numbers((x))\
					 : external_ref_no_of_numbers((x)))
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
#define is_ref_rel(x,Base)		(is_internal_ref_rel((x),Base)	\
					 || is_external_ref_rel((x),Base))
#define is_not_ref(x)			(!is_ref(x))

#endif


