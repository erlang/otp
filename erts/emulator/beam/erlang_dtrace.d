/*
 * %CopyrightBegin%
 *
 * Copyright Dustin Sallings, Michal Ptaszek, Scott Lystig Fritchie 2011-2018.
 * All Rights Reserved.
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
     * @param token_label for the sender's sequential trace token. This will be
     *        INT_MIN if the label does not fit into a 32-bit integer.
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
     * @param token_label for the sender's sequential trace token. This will be
     *        INT_MIN if the label does not fit into a 32-bit integer.
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
     * @param token_label for the sender's sequential trace token. This will be
     *        INT_MIN if the label does not fit into a 32-bit integer.
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
     * @param token_label for the sender's sequential trace token. This will be
     *        INT_MIN if the label does not fit into a 32-bit integer.
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
     * Fired whenever a user function is being called locally.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     * @param depth the stack depth
     */
    probe local__function__entry(char *p, char *mfa, int depth);

    /**
     * Fired whenever a user function is called externally
     * (through an export entry).
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     * @param depth the stack depth
     */
    probe global__function__entry(char *p, char *mfa, int depth);

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
     * @param token_label for the sender's sequential trace token. This will be
     *        INT_MIN if the label does not fit into a 32-bit integer.
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


/*
 * The set of probes called by the erlang tracer nif backend. In order
 * to receive events on these you both have to enable tracing in erlang
 * using the trace bifs and also from dtrace/systemtap.
 */


    /**
     * A trace message of type `event` was triggered by process `p`.
     *
     *
     * @param p the PID (string form) of the process
     * @param event the event that was triggered (i.e. call or spawn)
     * @param state the state of the tracer nif as a string
     * @param arg1 first argument to the trace event
     * @param arg2 second argument to the trace event
     */
    probe trace(char *p, char *event, char *state, char *arg1, char *arg2);

    /**
     * A sequence trace message of type `label` was triggered.
     *
     * @param state the state of the tracer nif as a string
     * @param label the seq trace label
     * @param seq_info the seq trace info tuple as a string
     */
    probe trace_seq(char *state, char *label, char *seq_info);

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

/*
 * The set of probes for use by Erlang code ... moved to here from
 * lib/runtime_tools/c_src/dtrace_user.d until a more portable solution
 * is found. This move pollutes the Erlang VM with functions that are
 * only used by the NIF shared library code in
 * lib/runtime_tools/c_src/dyntrace.c. The reason this is necessary is
 * in order to work around an issue on several platforms, including
 * SystemTap 1.3 and Solaris. The Solaris issue is discussed in the
 * `dtrace-discuss` mailing list thread on 01 Dec 2011 17:58:15.
 */
    /**
     * If you use only a single probe, but you also embed that probe
     * in many different places in your code, if that probe fires 100K
     * or more times per second, then it *will* hurt when you have to
     * enable that probe.
     * 
     * However, if you have any different probes, then you can ensure
     * that any probe on a hot code path will use separate probe(s)
     * than everyone else ... and you can then enable many non-hot
     * probes in production without worry about creating too much
     * measurement overhead.
     *
     * In an ideal world, we would use the libusdt library to be able
     * to create arbitrary DTrace probes with more convenient and
     * meaningful names than "user_trace-n7".  But libusdt doesn't
     * (yet) support all of the platforms that DTrace does, and there
     * is no known (yet) equivalent for SystemTap.
     */
    probe user_trace__n0(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n1(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n2(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n3(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n4(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n5(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n6(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n7(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n8(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n9(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n10(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n11(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n12(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n13(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n14(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n15(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n16(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n17(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n18(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n19(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n20(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n21(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n22(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n23(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n24(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n25(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n26(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n27(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n28(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n29(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n30(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n31(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n32(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n33(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n34(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n35(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n36(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n37(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n38(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n39(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n40(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n41(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n42(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n43(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n44(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n45(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n46(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n47(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n48(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n49(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n50(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n51(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n52(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n53(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n54(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n55(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n56(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n57(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n58(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n59(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n60(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n61(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n62(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n63(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n64(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n65(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n66(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n67(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n68(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n69(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n70(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n71(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n72(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n73(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n74(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n75(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n76(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n77(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n78(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n79(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n80(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n81(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n82(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n83(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n84(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n85(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n86(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n87(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n88(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n89(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n90(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n91(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n92(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n93(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n94(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n95(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n96(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n97(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n98(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n99(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n100(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n101(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n102(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n103(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n104(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n105(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n106(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n107(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n108(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n109(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n110(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n111(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n112(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n113(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n114(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n115(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n116(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n117(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n118(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n119(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n120(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n121(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n122(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n123(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n124(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n125(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n126(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n127(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n128(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n129(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n130(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n131(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n132(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n133(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n134(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n135(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n136(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n137(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n138(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n139(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n140(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n141(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n142(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n143(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n144(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n145(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n146(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n147(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n148(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n149(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n150(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n151(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n152(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n153(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n154(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n155(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n156(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n157(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n158(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n159(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n160(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n161(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n162(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n163(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n164(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n165(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n166(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n167(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n168(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n169(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n170(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n171(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n172(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n173(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n174(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n175(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n176(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n177(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n178(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n179(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n180(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n181(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n182(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n183(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n184(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n185(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n186(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n187(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n188(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n189(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n190(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n191(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n192(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n193(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n194(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n195(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n196(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n197(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n198(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n199(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n200(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n201(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n202(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n203(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n204(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n205(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n206(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n207(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n208(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n209(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n210(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n211(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n212(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n213(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n214(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n215(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n216(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n217(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n218(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n219(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n220(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n221(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n222(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n223(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n224(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n225(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n226(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n227(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n228(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n229(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n230(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n231(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n232(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n233(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n234(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n235(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n236(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n237(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n238(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n239(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n240(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n241(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n242(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n243(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n244(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n245(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n246(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n247(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n248(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n249(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n250(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n251(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n252(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n253(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n254(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n255(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n256(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n257(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n258(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n259(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n260(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n261(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n262(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n263(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n264(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n265(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n266(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n267(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n268(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n269(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n270(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n271(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n272(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n273(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n274(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n275(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n276(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n277(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n278(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n279(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n280(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n281(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n282(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n283(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n284(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n285(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n286(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n287(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n288(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n289(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n290(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n291(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n292(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n293(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n294(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n295(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n296(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n297(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n298(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n299(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n300(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n301(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n302(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n303(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n304(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n305(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n306(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n307(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n308(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n309(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n310(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n311(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n312(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n313(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n314(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n315(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n316(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n317(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n318(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n319(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n320(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n321(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n322(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n323(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n324(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n325(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n326(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n327(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n328(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n329(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n330(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n331(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n332(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n333(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n334(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n335(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n336(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n337(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n338(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n339(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n340(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n341(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n342(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n343(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n344(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n345(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n346(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n347(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n348(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n349(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n350(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n351(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n352(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n353(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n354(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n355(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n356(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n357(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n358(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n359(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n360(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n361(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n362(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n363(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n364(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n365(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n366(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n367(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n368(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n369(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n370(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n371(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n372(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n373(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n374(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n375(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n376(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n377(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n378(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n379(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n380(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n381(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n382(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n383(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n384(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n385(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n386(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n387(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n388(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n389(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n390(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n391(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n392(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n393(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n394(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n395(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n396(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n397(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n398(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n399(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n400(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n401(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n402(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n403(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n404(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n405(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n406(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n407(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n408(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n409(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n410(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n411(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n412(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n413(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n414(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n415(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n416(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n417(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n418(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n419(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n420(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n421(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n422(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n423(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n424(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n425(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n426(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n427(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n428(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n429(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n430(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n431(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n432(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n433(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n434(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n435(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n436(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n437(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n438(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n439(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n440(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n441(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n442(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n443(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n444(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n445(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n446(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n447(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n448(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n449(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n450(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n451(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n452(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n453(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n454(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n455(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n456(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n457(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n458(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n459(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n460(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n461(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n462(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n463(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n464(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n465(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n466(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n467(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n468(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n469(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n470(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n471(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n472(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n473(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n474(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n475(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n476(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n477(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n478(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n479(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n480(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n481(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n482(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n483(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n484(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n485(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n486(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n487(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n488(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n489(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n490(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n491(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n492(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n493(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n494(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n495(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n496(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n497(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n498(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n499(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n500(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n501(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n502(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n503(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n504(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n505(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n506(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n507(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n508(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n509(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n510(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n511(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n512(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n513(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n514(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n515(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n516(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n517(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n518(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n519(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n520(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n521(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n522(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n523(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n524(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n525(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n526(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n527(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n528(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n529(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n530(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n531(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n532(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n533(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n534(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n535(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n536(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n537(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n538(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n539(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n540(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n541(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n542(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n543(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n544(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n545(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n546(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n547(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n548(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n549(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n550(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n551(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n552(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n553(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n554(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n555(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n556(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n557(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n558(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n559(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n560(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n561(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n562(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n563(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n564(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n565(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n566(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n567(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n568(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n569(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n570(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n571(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n572(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n573(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n574(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n575(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n576(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n577(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n578(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n579(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n580(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n581(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n582(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n583(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n584(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n585(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n586(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n587(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n588(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n589(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n590(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n591(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n592(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n593(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n594(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n595(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n596(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n597(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n598(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n599(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n600(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n601(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n602(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n603(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n604(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n605(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n606(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n607(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n608(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n609(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n610(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n611(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n612(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n613(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n614(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n615(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n616(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n617(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n618(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n619(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n620(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n621(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n622(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n623(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n624(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n625(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n626(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n627(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n628(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n629(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n630(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n631(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n632(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n633(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n634(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n635(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n636(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n637(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n638(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n639(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n640(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n641(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n642(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n643(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n644(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n645(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n646(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n647(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n648(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n649(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n650(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n651(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n652(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n653(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n654(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n655(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n656(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n657(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n658(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n659(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n660(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n661(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n662(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n663(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n664(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n665(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n666(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n667(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n668(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n669(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n670(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n671(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n672(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n673(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n674(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n675(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n676(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n677(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n678(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n679(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n680(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n681(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n682(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n683(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n684(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n685(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n686(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n687(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n688(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n689(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n690(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n691(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n692(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n693(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n694(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n695(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n696(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n697(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n698(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n699(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n700(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n701(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n702(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n703(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n704(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n705(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n706(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n707(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n708(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n709(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n710(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n711(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n712(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n713(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n714(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n715(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n716(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n717(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n718(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n719(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n720(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n721(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n722(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n723(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n724(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n725(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n726(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n727(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n728(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n729(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n730(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n731(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n732(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n733(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n734(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n735(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n736(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n737(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n738(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n739(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n740(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n741(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n742(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n743(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n744(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n745(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n746(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n747(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n748(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n749(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n750(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n751(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n752(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n753(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n754(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n755(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n756(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n757(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n758(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n759(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n760(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n761(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n762(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n763(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n764(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n765(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n766(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n767(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n768(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n769(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n770(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n771(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n772(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n773(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n774(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n775(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n776(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n777(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n778(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n779(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n780(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n781(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n782(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n783(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n784(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n785(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n786(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n787(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n788(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n789(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n790(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n791(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n792(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n793(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n794(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n795(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n796(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n797(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n798(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n799(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n800(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n801(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n802(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n803(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n804(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n805(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n806(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n807(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n808(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n809(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n810(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n811(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n812(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n813(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n814(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n815(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n816(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n817(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n818(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n819(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n820(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n821(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n822(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n823(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n824(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n825(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n826(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n827(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n828(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n829(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n830(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n831(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n832(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n833(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n834(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n835(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n836(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n837(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n838(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n839(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n840(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n841(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n842(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n843(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n844(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n845(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n846(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n847(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n848(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n849(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n850(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n851(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n852(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n853(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n854(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n855(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n856(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n857(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n858(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n859(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n860(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n861(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n862(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n863(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n864(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n865(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n866(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n867(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n868(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n869(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n870(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n871(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n872(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n873(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n874(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n875(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n876(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n877(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n878(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n879(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n880(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n881(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n882(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n883(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n884(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n885(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n886(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n887(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n888(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n889(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n890(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n891(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n892(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n893(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n894(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n895(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n896(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n897(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n898(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n899(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n900(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n901(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n902(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n903(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n904(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n905(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n906(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n907(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n908(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n909(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n910(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n911(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n912(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n913(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n914(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n915(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n916(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n917(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n918(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n919(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n920(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n921(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n922(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n923(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n924(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n925(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n926(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n927(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n928(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n929(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n930(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n931(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n932(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n933(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n934(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n935(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n936(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n937(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n938(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n939(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n940(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n941(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n942(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n943(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n944(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n945(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n946(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n947(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n948(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n949(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    probe user_trace__n950(char *proc, char *user_tag,
                            int i1, int i2, int i3, int i4,
                            char *s1, char *s2, char *s3, char *s4);
    /**
     * Send a single string to a probe.
     * This probe is deprecated.
     *
     * @param NUL-terminated string
     */
    probe user_trace__s1(char* message);

    /**
     * Multi-purpose probe: up to 4 NUL-terminated strings and 4
     * 64-bit integer arguments.
     * This probe is deprecated.
     *
     * @param proc, the PID (string form) of the sending process
     * @param user_tag, the user tag of the sender
     * @param i1, integer
     * @param i2, integer
     * @param i3, integer
     * @param i4, integer
     * @param s1, string/iolist. D's arg6 is NULL if not given by Erlang
     * @param s2, string/iolist. D's arg7 is NULL if not given by Erlang
     * @param s3, string/iolist. D's arg8 is NULL if not given by Erlang
     * @param s4, string/iolist. D's arg9 is NULL if not given by Erlang
     */
    probe user_trace__i4s4(char *proc, char *user_tag,
                           int i1, int i2, int i3, int i4,
                           char *s1, char *s2, char *s3, char *s4);

};

#pragma D attributes Evolving/Evolving/Common provider erlang provider
#pragma D attributes Private/Private/Common provider erlang module
#pragma D attributes Private/Private/Common provider erlang function
#pragma D attributes Evolving/Evolving/Common provider erlang name
#pragma D attributes Evolving/Evolving/Common provider erlang args
