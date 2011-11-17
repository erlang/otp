/*
 * %CopyrightBegin%
 *
 * Copyright Dustin Sallings, Michal Ptaszek, Scott Lystig Fritchie 2011.
 * All Rights Reserved.
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

/*
 * A note on probe naming: if "__" appears in a provider probe
 * definition, then two things happen during compilation:
 *
 *    1. The "__" will turn into a hypen, "-", for the probe name.
 *    2. The "__" will turn into a single underscore, "_", for the
 *       macro names and function definitions that the compiler and
 *       C developers will see.
 *
 * We'll try to use the following naming convention.  We're a bit
 * limited because, as a USDT probe, we can only specify the 4th part
 * of the probe name, e.g. erlang*:::mumble.  The 2nd part of the
 * probe name is always going to be "beam" or "beam.smp", and the 3rd
 * part of the probe name will always be the name of the function
 * that's calling the probe.
 *
 * So, all probes will be have names defined in this file using the
 * convention category__name or category__sub_category__name.  This
 * will translate to probe names of category-name or
 * category-sub_category-name.
 *
 * Each of "category", "sub_category", and "name" may have underscores
 * but may not have hyphens.
 */

provider erlang {
    /**
     * Fired when a message is sent from one local process to another.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * @param sender the PID (string form) of the sender
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__send(char *sender, char *receiver, uint32_t size,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a message is sent from a local process to a remote process.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * @param sender the PID (string form) of the sender
     * @param node_name the Erlang node name (string form) of the receiver
     * @param receiver the PID/name (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__send__remote(char *sender, char *node_name, char *receiver,
                                uint32_t size,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a message is queued to a local process.  This probe
     * will not fire if the sender's pid == receiver's pid.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * NOTE: In cases of messages in external format (i.e. from another
     *       Erlang node), we probably don't know the message size
     *       without performing substantial extra computation.  To
     *       avoid the extra CPU overhead, the message size may be
     *       reported as -1, which can appear to a D script as 4294967295.
     *
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param queue_len length of the queue of the receiving process
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__queued(char *receiver, uint32_t size, uint32_t queue_len,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a message is 'receive'd by a local process and removed
     * from its mailbox.
     *
     * NOTE: The 'size' parameter is in machine-dependent words and
     *       that the actual size of any binary terms in the message
     *       are not included.
     *
     * NOTE: In cases of messages in external format (i.e. from another
     *       Erlang node), we probably don't know the message size
     *       without performing substantial extra computation.  To
     *       avoid the extra CPU overhead, the message size may be
     *       reported as -1, which can appear to a D script as 4294967295.
     *
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered (words)
     * @param queue_len length of the queue of the receiving process
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe message__receive(char *receiver, uint32_t size, uint32_t queue_len,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when an Eterm structure is being copied.
     *
     * NOTE: Due to the placement of this probe, the process ID of
     *       owner of the Eterm is not available.
     *
     * @param size the size of the structure
     */
    probe copy__struct(uint32_t size);

    /**
     * Fired when an Eterm is being copied onto a process.
     *
     * @param proc the PID (string form) of the recipient process
     * @param size the size of the structure
     */
    probe copy__object(char *proc, uint32_t size);

    /* PID, Module, Function, Arity */

    /**
     * Fired whenever a user function is being called.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     * @param depth the stack depth
     */
    probe function__entry(char *p, char *mfa, int depth);

    /**
     * Fired whenever a user function returns.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     * @param depth the stack depth
     */
    probe function__return(char *p, char *mfa, int depth);

    /**
     * Fired whenever a Built In Function is called.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe bif__entry(char *p, char *mfa);

    /**
     * Fired whenever a Built In Function returns.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe bif__return(char *p, char *mfa);

    /**
     * Fired whenever a Native Function is called.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe nif__entry(char *p, char *mfa);

    /**
     * Fired whenever a Native Function returns.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe nif__return(char *p, char *mfa);

    /**
     * Fired when a major GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param need the number of words needed on the heap
     */
    probe gc_major__start(char *p, int need);

    /**
     * Fired when a minor GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param need the number of words needed on the heap
     */
    probe gc_minor__start(char *p, int need);

    /**
     * Fired when a major GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param reclaimed the amount of space reclaimed
     */
    probe gc_major__end(char *p, int reclaimed);

    /**
     * Fired when a minor GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param reclaimed the amount of space reclaimed
     */
    probe gc_minor__end(char *p, int reclaimed);

    /**
     * Fired when a process is spawned.
     *
     * @param p the PID (string form) of the new process.
     * @param mfa the m:f/a of the function
     */
    probe process__spawn(char *p, char *mfa);

    /**
     * Fired when a process is exiting.
     *
     * @param p the PID (string form) of the exiting process
     * @param reason the reason for the exit (may be truncated)
     */
    probe process__exit(char *p, char *reason);

    /**
     * Fired when exit signal is delivered to a local process.
     *
     * @param sender the PID (string form) of the exiting process
     * @param receiver the PID (string form) of the process receiving EXIT signal
     * @param reason the reason for the exit (may be truncated)
     */
    probe process__exit_signal(char *sender, char *receiver, char *reason);

    /**
     * Fired when exit signal is delivered to a remote process.
     *
     * @param sender the PID (string form) of the exiting process
     * @param node_name the Erlang node name (string form) of the receiver
     * @param receiver the PID (string form) of the process receiving EXIT signal
     * @param reason the reason for the exit (may be truncated)
     * @param token_label for the sender's sequential trace token
     * @param token_previous count for the sender's sequential trace token
     * @param token_current count for the sender's sequential trace token
     */
    probe process__exit_signal__remote(char *sender, char *node_name,
                                       char *receiver, char *reason,
                        int token_label, int token_previous, int token_current);

    /**
     * Fired when a process is scheduled.
     *
     * @param p the PID (string form) of the newly scheduled process
     * @param mfa the m:f/a of the function it should run next
     */
    probe process__scheduled(char *p, char *mfa);

    /**
     * Fired when a process is unscheduled.
     *
     * @param p the PID (string form) of the process that has been
     * unscheduled.
     */
    probe process__unscheduled(char *p);

    /**
     * Fired when a process goes into hibernation.
     *
     * @param p the PID (string form) of the process entering hibernation
     * @param mfa the m:f/a of the location to resume
     */
    probe process__hibernate(char *p, char *mfa);

    /**
     * Fired when a process is unblocked after a port has been unblocked.
     *
     * @param p the PID (string form) of the process that has been
     * unscheduled.
     * @param port the port that is no longer busy (i.e., is now unblocked)
     */
    probe process__port_unblocked(char *p, char *port);

    /**
     * Fired when process' heap is growing.
     *
     * @param p the PID (string form)
     * @param old_size the size of the old heap
     * @param new_size the size of the new heap
     */
    probe process__heap_grow(char *p, int old_size, int new_size);

    /**
     * Fired when process' heap is shrinking.
     *
     * @param p the PID (string form)
     * @param old_size the size of the old heap
     * @param new_size the size of the new heap
     */
    probe process__heap_shrink(char *p, int old_size, int new_size);

    /* network distribution */

    /**
     * Fired when network distribution event monitor events are triggered.
     *
     * @param node the name of the reporting node
     * @param what the type of event, e.g., nodeup, nodedown
     * @param monitored_node the name of the monitored node
     * @param type the type of node, e.g., visible, hidden
     * @param reason the reason term, e.g., normal, connection_closed, term()
     */
    probe dist__monitor(char *node, char *what, char *monitored_node,
                        char *type, char *reason);

    /**
     * Fired when network distribution port is busy (i.e. blocked),
     * usually due to the remote node not consuming distribution
     * data quickly enough.
     *
     * @param node the name of the reporting node
     * @param port the port ID of the busy port
     * @param remote_node the name of the remote node.
     * @param pid the PID (string form) of the local process that has
     *        become unschedulable until the port becomes unblocked.
     */
    probe dist__port_busy(char *node, char *port, char *remote_node,
                          char *pid);

    /**
     * Fired when network distribution's driver's "output" callback is called
     *
     * @param node the name of the reporting node
     * @param port the port ID of the busy port
     * @param remote_node the name of the remote node.
     * @param bytes the number of bytes written
     */
    probe dist__output(char *node, char *port, char *remote_node, int bytes);

    /**
     * Fired when network distribution's driver's "outputv" callback is called
     *
     * @param node the name of the reporting node
     * @param port the port ID of the busy port
     * @param remote_node the name of the remote node.
     * @param bytes the number of bytes written
     */
    probe dist__outputv(char *node, char *port, char *remote_node, int bytes);

    /**
     * Fired when network distribution port is no longer busy (i.e. blocked).
     *
     * NOTE: This probe may fire multiple times after the same single
     *       dist-port_busy probe firing.
     *
     * @param node the name of the reporting node
     * @param port the port ID of the busy port
     * @param remote_node the name of the remote node.
     */
    probe dist__port_not_busy(char *node, char *port, char *remote_node);

    /* ports */

    /**
     * Fired when new port is opened.
     *
     * @param process the PID (string form)
     * @param port_name the string used when the port was opened
     * @param port the Port (string form) of the new port
     */
    probe port__open(char *process, char *port_name, char *port);

    /**
     * Fired when port_command is issued.
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param command_type type of the issued command, one of: "close", "command" or "connect"
     */
    probe port__command(char *process, char *port, char *port_name, char *command_type);

    /**
     * Fired when port_control is issued.
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param command_no command number that has been issued to the port
     */
    probe port__control(char *process, char *port, char *port_name, int command_no);

    /**
     * Fired when port is closed via port_close/1 (reason = 'normal')
     * or is sent an exit signal.
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param reason Erlang term representing the exit signal, e.g. 'normal'
     */
    probe port__exit(char *process, char *port, char *port_name,
                     char *new_process);

    /**
     * Fired when port_connect is issued.
     *
     * @param process the PID (string form) of the current port owner
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param new_process the PID (string form) of the new port owner
     */
    probe port__connect(char *process, char *port, char *port_name,
                        char *new_process);

    /**
     * Fired when a port is busy (i.e. blocked)
     *
     * @param port the port ID of the busy port
     */
    probe port__busy(char *port);

    /**
     * Fired when a port is no longer busy (i.e. no longer blocked)
     *
     * @param port the port ID of the not busy port
     */
    probe port__not_busy(char *port);

    /* drivers */

    /**
     * Fired when drivers's "init" callback is called.
     *
     * @param name the name of the driver
     * @param major the major version number
     * @param minor the minor version number
     * @param flags the flags argument
     */
    probe driver__init(char *name, int major, int minor, int flags);

    /**
     * Fired when drivers's "start" callback is called.
     *
     * @param process the PID (string form) of the calling process
     * @param name the name of the driver
     * @param port the Port (string form) of the driver's port
     */
     probe driver__start(char *process, char *name, char *port);

    /**
     * Fired when drivers's "stop" callback is called.
     *
     * @param process the PID (string form) of the calling process
     * @param name the name of the driver
     * @param port the Port (string form) of the driver's port
     */
     probe driver__stop(char *process, char *name, char *port);

    /**
     * Fired when drivers's "finish" callback is called.
     *
     * @param name the name of the driver
     */
     probe driver__finish(char *name);

    /**
     * Fired when drivers's "flush" callback is called.
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__flush(char *process, char *port, char *port_name);

    /**
     * Fired when driver's "output" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param bytes the number of bytes written
     */
    probe driver__output(char *node, char *port, char *port_name, int bytes);

    /**
     * Fired when driver's "outputv" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param bytes the number of bytes written
     */
    probe driver__outputv(char *node, char *port, char *port_name, int bytes);

    /**
     * Fired when driver's "control" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param command the command #
     * @param bytes the number of bytes written
     */
    probe driver__control(char *node, char *port, char *port_name,
                          int command, int bytes);

    /**
     * Fired when driver's "call" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     * @param command the command #
     * @param bytes the number of bytes written
     */
    probe driver__call(char *node, char *port, char *port_name,
                       int command, int bytes);

    /**
     * Fired when driver's "event" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__event(char *node, char *port, char *port_name);

    /**
     * Fired when driver's "ready_input" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__ready_input(char *node, char *port, char *port_name);

    /**
     * Fired when driver's "read_output" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__ready_output(char *node, char *port, char *port_name);

    /**
     * Fired when driver's "timeout" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__timeout(char *node, char *port, char *port_name);

    /**
     * Fired when drivers's "ready_async" callback is called.
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__ready_async(char *process, char *port, char *port_name);

    /**
     * Fired when driver's "process_exit" callback is called
     *
     * @param process the PID (string form)
     * @param port the Port (string form)
     * @param port_name the string used when the port was opened
     */
    probe driver__process_exit(char *node, char *port, char *port_name);

    /**
     * Fired when driver's "stop_select" callback is called
     *
     * @param name the name of the driver
     */
    probe driver__stop_select(char *name);


    /* Async driver pool */

    /**
     * Show the post-add length of the async driver thread pool member's queue.
     *
     * NOTE: The port name is not available: additional lock(s) must
     *       be acquired in order to get the port name safely in an SMP
     *       environment.  The same is true for the aio__pool_get probe.
     *
     * @param port the Port (string form)
     * @param new queue length
     */
    probe aio_pool__add(char *, int);

    /**
     * Show the post-get length of the async driver thread pool member's queue.
     *
     * @param port the Port (string form)
     * @param new queue length
     */
    probe aio_pool__get(char *, int);

    /* Probes for efile_drv.c */

    /**
     * Entry into the efile_drv.c file I/O driver
     *
     * For a list of command numbers used by this driver, see the section
     * "Guide to probe arguments" in ../../../README.md.  That section
     * also contains explanation of the various integer and string
     * arguments that may be present when any particular probe fires.
     *
     * NOTE: Not all Linux platforms (using SystemTap) can support
     *       arguments beyond arg9.
     *
     *
     * TODO: Adding the port string, args[10], is a pain.  Making that
     *       port string available to all the other efile_drv.c probes
     *       will be more pain.  Is the pain worth it?  If yes, then
     *       add them everywhere else and grit our teeth.  If no, then
     *       rip it out.
     *
     * @param thread-id number of the scheduler Pthread                   arg0
     * @param tag number: {thread-id, tag} uniquely names a driver operation
     * @param user-tag string                                             arg2
     * @param command number                                              arg3
     * @param string argument 1                                           arg4
     * @param string argument 2                                           arg5
     * @param integer argument 1                                          arg6
     * @param integer argument 2                                          arg7
     * @param integer argument 3                                          arg8
     * @param integer argument 4                                          arg9
     * @param port the port ID of the busy port                       args[10]
     */
    probe efile_drv__entry(int, int, char *, int, char *, char *,
                           int64_t, int64_t, int64_t, int64_t, char *);

    /**
     * Entry into the driver's internal work function.  Computation here
     * is performed by a async worker pool Pthread.
     *
     * @param thread-id number
     * @param tag number
     * @param command number
     */
    probe efile_drv__int_entry(int, int, int);

    /**
     * Return from the driver's internal work function.
     *
     * @param thread-id number
     * @param tag number
     * @param command number
     */
    probe efile_drv__int_return(int, int, int);

    /**
     * Return from the efile_drv.c file I/O driver
     *
     * @param thread-id number                                            arg0
     * @param tag number                                                  arg1
     * @param user-tag string                                             arg2
     * @param command number                                              arg3
     * @param Success? 1 is success, 0 is failure                         arg4
     * @param If failure, the errno of the error.                         arg5
     */
    probe efile_drv__return(int, int, char *, int, int, int);

/*
 * NOTE:
 * For formatting int64_t arguments within a D script, see:
 *
 *   http://mail.opensolaris.org/pipermail/dtrace-discuss/2006-November/002830.html
 *   Summary:
 *       "1) you don't need the 'l' printf() modifiers with DTrace ever"
 */

/*
 * NOTE: For file_drv_return + SMP + R14B03 (and perhaps other
 *       releases), the sched-thread-id will be the same as the
 *       work-thread-id: erl_async.c's async_main() function
 *       will call the asynchronous invoke function and then
 *       immediately call the drivers ready_async function while
 *       inside the same I/O worker pool thread.
 *       For R14B03's source, see erl_async.c lines 302-317.
 */
};

#pragma D attributes Evolving/Evolving/Common provider erlang provider
#pragma D attributes Private/Private/Common provider erlang module
#pragma D attributes Private/Private/Common provider erlang function
#pragma D attributes Evolving/Evolving/Common provider erlang name
#pragma D attributes Evolving/Evolving/Common provider erlang args
