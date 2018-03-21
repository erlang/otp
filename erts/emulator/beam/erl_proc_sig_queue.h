/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2018. All Rights Reserved.
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
 * Description:	Process signal queue implementation.
 *
 *              Currently the following signals are handled:
 *              - Messages
 *              - Exit
 *              - Monitor
 *              - Demonitor
 *              - Monitor down
 *              - Persistent monitor message
 *              - Link
 *              - Unlink
 *              - Group leader
 *              - Is process alive
 *              - Trace change
 *
 *              The signal queue consists of three parts:
 *              - Outer queue (sig_inq field in process struct)
 *              - Middle queue (sig_qs field in process struct)
 *              - Inner queue (sig_qs field in process struct)
 *
 *              Incoming signals are placed in the outer queue
 *              by other processes, ports, or by the runtime system
 *              itself. This queue is protected by the msgq process
 *              lock and may be accessed by any other entity. While
 *              a signal is located in the outer queue, it is still
 *              in transit between sender and receiver.
 *
 *              The middle and the inner queues are private to the
 *              receiving process and can only be accessed while
 *              holding the main process lock. The signal changes
 *              from being in transit to being received while in
 *              the middle queue. Non-message signals are handled
 *              immediately upon reception while message signals
 *              are moved into the inner queue.
 *
 *              In the outer and middle queues both message signals
 *              and non-message signals are mixed. Signals in these
 *              queues are referenced using two single linked lists.
 *              One single linked list that go through all signals
 *              in the queue and another single linked list that
 *              goes through only non-message signals. The list
 *              through the non-message signals is used for fast
 *              access to these signals in the middle queue, since
 *              these should be handled immediately upon reception.
 *
 *              The inner queue consists only of one single linked
 *              list through the message signals. A receive
 *              expression can only operate on messages once they
 *              have entered the inner queue.
 *
 * Author: 	Rickard Green
 */

#ifndef ERTS_PROC_SIG_QUEUE_H_TYPE__
#define ERTS_PROC_SIG_QUEUE_H_TYPE__

#if 0
#  define ERTS_PROC_SIG_HARD_DEBUG
#endif

struct erl_mesg;

typedef struct {
    struct erl_mesg *next;
    union {
        struct erl_mesg **next;
        void *attachment;
    } specific;
    Eterm tag;
} ErtsSignalCommon;

#define ERTS_SIG_HANDLE_REDS_MAX_PREFERED (CONTEXT_REDS/40)

#ifdef ERTS_PROC_SIG_HARD_DEBUG
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(P)       \
    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__((P), "")
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(P) \
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((P), "")
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__(P, What)   \
    erts_proc_sig_hdbg_check_in_queue((P), (What), __FILE__, __LINE__)
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(P, What)   \
    erts_proc_sig_hdbg_check_priv_queue((P), (What), __FILE__, __LINE__)
struct process;
void erts_proc_sig_hdbg_check_priv_queue(struct process *c_p, char *what,
                                         char *file, int line);
void erts_proc_sig_hdbg_check_in_queue(struct process *c_p, char *what,
                                       char *file, int line);
#else
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(P)
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(P)
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__(P, What)
#define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(P, What)
#endif

#endif

#if !defined(ERTS_PROC_SIG_QUEUE_H__) && !defined(ERTS_PROC_SIG_QUEUE_TYPE_ONLY)
#define ERTS_PROC_SIG_QUEUE_H__

struct dist_entry_;

/*
 * Send operations of currently supported process signals follow...
 */

/**
 *
 * @brief Send an exit signal to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of local process
 *                              to send signal to.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 * @param[in]     normal_kills  If non-zero, also normal exit
 *                              reason will kill the receiver
 *                              if it is not trapping exit.
 *
 */
void
erts_proc_sig_send_exit(Process *c_p, Eterm from, Eterm to,
                        Eterm reason, Eterm token, int normal_kills);

/**
 *
 * @brief Send an exit signal due to broken link to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     lnk           Pointer to link structure
 *                              from the sending side. It
 *                              should contain information
 *                              about receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 */
void
erts_proc_sig_send_link_exit(Process *c_p, Eterm from, ErtsLink *lnk,
                             Eterm reason, Eterm token);

/**
 *
 * @brief Send an link signal to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     lnk           Pointer to link structure to
 *                              insert on receiver side.
 *
 * @return                      A non-zero value if
 *                              signal was successfully
 *                              sent. If a zero, value
 *                              the signal was not sent
 *                              due to the receiver not
 *                              existing. The sender
 *                              needs to deallocate the
 *                              link structure.
 *
 */
int
erts_proc_sig_send_link(Process *c_p, Eterm to, ErtsLink *lnk);

/**
 *
 * @brief Send an unlink signal to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     lnk           Pointer to link structure from
 *                              the sending side. It should
 *                              contain information about
 *                              receiver.
 */
void
erts_proc_sig_send_unlink(Process *c_p, ErtsLink *lnk);

/**
 *
 * @brief Send an exit signal due to broken link to a process.
 *
 * This function is used instead of erts_proc_sig_send_link_exit()
 * when the signal arrives via the distribution and
 * no link structure is available.
 *
 * @param[in]     dep           Distribution entry of channel
 *                              that the signal arrived on.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 */
void
erts_proc_sig_send_dist_link_exit(struct dist_entry_ *dep,
                                  Eterm from, Eterm to,
                                  Eterm reason, Eterm token);

/**
 *
 * @brief Send an unlink signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_unlink()
 * when the signal arrives via the distribution and
 * no link structure is available.
 *
 * @param[in]     dep           Distribution entry of channel
 *                              that the signal arrived on.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 */
void
erts_proc_sig_send_dist_unlink(struct dist_entry_ *dep,
                               Eterm from, Eterm to);

/**
 *
 * @brief Send a monitor down signal to a process.
 *
 * @param[in]     mon           Pointer to target monitor
 *                              structure from the sending
 *                              side. It should contain
 *                              information about receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 */
void
erts_proc_sig_send_monitor_down(ErtsMonitor *mon, Eterm reason);

/**
 *
 * @brief Send a demonitor signal to a process.
 *
 * @param[in]     mon           Pointer to origin monitor
 *                              structure from the sending
 *                              side. It should contain
 *                              information about receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 */
void
erts_proc_sig_send_demonitor(ErtsMonitor *mon);

/**
 *
 * @brief Send a monitor signal to a process.
 *
 * @param[in]     mon           Pointer to target monitor
 *                              structure to insert on
 *                              receiver side.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @return                      A non-zero value if
 *                              signal was successfully
 *                              sent. If a zero, value
 *                              the signal was not sent
 *                              due to the receiver not
 *                              existing. The sender
 *                              needs to deallocate the
 *                              monitor structure.
 *
 */
int
erts_proc_sig_send_monitor(ErtsMonitor *mon, Eterm to);

/**
 *
 * @brief Send a monitor down signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_monitor_down()
 * when the signal arrives via the distribution and
 * no link structure is available.
 *
 * @param[in]     dep           Pointer to distribution entry
 *                              of channel that the signal
 *                              arrived on.
 *
 * @param[in]     ref           Reference identifying the monitor.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 */
void
erts_proc_sig_send_dist_monitor_down(DistEntry *dep, Eterm ref,
                                     Eterm from, Eterm to,
                                     Eterm reason);

/**
 *
 * @brief Send a demonitor signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_demonitor()
 * when the signal arrives via the distribution and
 * no monitor structure is available.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     ref           Reference identifying the monitor.
 *
 */
void
erts_proc_sig_send_dist_demonitor(Eterm to, Eterm ref);

/**
 *
 * @brief Send a persistent monitor triggered signal to a process.
 *
 * Used by monitors that are not auto disabled such as for
 * example 'time_offset' monitors.
 *
 * @param[in]     type          Monitor type.
 *
 * @param[in]     key           Monitor key.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     msg           Message template.
 *
 * @param[in]     msg_sz        Heap size of message template.
 *
 */
void
erts_proc_sig_send_persistent_monitor_msg(Uint16 type, Eterm key,
                                          Eterm from, Eterm to,
                                          Eterm msg, Uint msg_sz);

/**
 *
 * @brief Send a trace change signal to a process.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     on            Trace flags to enable.
 *
 * @param[in]     off           Trace flags to disable.
 *
 * @param[in]     tracer        Tracer to set. If the non-value,
 *                              tracer will not be changed.
 *
 */
void
erts_proc_sig_send_trace_change(Eterm to, Uint on, Uint off,
                                Eterm tracer);

/**
 *
 * @brief Send a group leader signal to a process.
 *
 * Set group-leader of receiving process. If sent locally,
 * a response message '{Ref, Result}' is sent to the original
 * sender when performed where Ref is the reference passed
 * as 'ref' argument, and Result is either 'true' or 'badarg'.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *                              NULL if signal arrived via
 *                              distribution.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     gl            Identifier of new group leader.
 *
 * @param[in]     ref           Reference to use in response
 *                              message to locally sending
 *                              process (i.e., c_p when c_p
 *                              is non-null).
 *
 */
void
erts_proc_sig_send_group_leader(Process *c_p, Eterm to, Eterm gl,
                                Eterm ref);

/**
 *
 * @brief Send an 'is process alive' signal to a process.
 *
 * A response message '{Ref, Result}' is sent to the
 * sender when performed where Ref is the reference passed
 * as 'ref' argument, and Result is either 'true' or 'false'.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *                              NULL if signal arrived via
 *                              distribution.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     ref           Reference to use in response
 *                              message to the sending
 *                              process (i.e., c_p).
 *
 */
void
erts_proc_sig_send_is_alive_request(Process *c_p, Eterm to,
                                    Eterm ref);

/*
 * End of send operations of currently supported process signals.
 */


/**
 *
 * @brief Handle incoming signals.
 *
 * Called by an ordinary scheduler in order to handle incoming
 * signals for a process. The work is done on the middle part
 * of the signal queue. The maximum amount of signals handled
 * is limited by the amount of reductions given when calling.
 * Note that a reduction does not necessarily map to a signal.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[out]    statep        Pointer to process state after
 *                              signal handling. May not be NULL.
 *
 * @param[in,out] redsp         Pointer to an integer containing
 *                              reductions. On input, the amount
 *                              of preferred reductions to be
 *                              used by the call. On output, the
 *                              amount of reductions consumed.
 *
 * @param[in]     max_reds      Absolute maximum of reductions
 *                              to use. If the process cannot
 *                              make progress after the preferred
 *                              amount of reductions has been
 *                              consumed, signal handling may
 *                              proceed up to a maximum of
 *                              'max_reds' in order to make
 *                              the process able to proceed
 *                              with other tasks after handling
 *                              has finished.
 *
 * @param[in]     local_only    If is zero, new signals may be
 *                              fetched from the outer queue and
 *                              put in the middle queue before
 *                              signal handling is performed. If
 *                              non-zero, no new signals will be
 *                              fetched before handling begins.
 *
 * @return                      Returns a non-zero value, when
 *                              no more signals to handle in the
 *                              middle queue remain. A zero
 *                              return value means that there
 *                              remains signals in the middle
 *                              queue.
 */
int
erts_proc_sig_handle_incoming(Process *c_p, erts_aint32_t *statep,
                              int *redsp, int max_reds,
                              int local_only);

/**
 *
 * @brief Handle remaining signals for an exiting process
 *
 * Called as part of termination of a process. It will handle
 * remaining signals.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in,out] redsp         Pointer to an integer containing
 *                              reductions. On input, the amount
 *                              of maximum reductions to be
 *                              used by the call. On output, the
 *                              amount of reductions consumed.
 *
 * @return                      Returns a non-zero value, when
 *                              no more signals to handle in the
 *                              middle queue remain. A zero
 *                              return value means that there
 *                              remains signals in the middle
 *                              queue.
 */
int
erts_proc_sig_handle_exit(Process *c_p, int *redsp);

/**
 *
 * @brief Helper for loop_rec instruction.
 *
 * This function should only be called from the loop_rec
 * instruction (or equivalents). It is called when loop_rec
 * reach the end of the inner queue (which is the only
 * part of the signal queue that receive is allowed to
 * operate on). When called, this function tries to make
 * more messages available in the inner queue. This by
 * fetching signals from the outer queue to the middle
 * queue and/or processing signals in the middle queue.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]   fcalls          Content of FCALLS in
 *                              process_main()
 *
 * @param[in]   neg_o_reds      Content of neg_o_reds in
 *                              process_main()
 *
 * @param[out]  msgpp           Pointer to pointer to next
 *                              available message to process.
 *                              If *msgpp == NULL, no more
 *                              messages are available.
 *
 * @param[out]  get_outp        Pointer to an integer
 *                              indicating how to respond
 *                              if no more messages are
 *                              available (msgpp). If integer
 *                              is set to zero, loop_rec
 *                              should jump to an appropriate
 *                              wait instruction. If zero,
 *                              the message queue lock remain
 *                              locked since the test for
 *                              more messages was done.
 *                              If the integer is set to a
 *                              value larger that zero, the
 *                              process exited. If the integer
 *                              is set to a value less than
 *                              zero, the process is required
 *                              to yield.
 *
 *
 * @return                      The amount of reductions
 *                              consumed.
 *
 */
int
erts_proc_sig_receive_helper(Process *c_p, int fcalls,
                             int neg_o_reds, ErtsMessage **msgpp,
                             int *get_outp);

/**
 *
 * @brief Fetch signals from the outer queue
 *
 * Fetches signals from outer queue and places them in the
 * middle queue ready for signal handling. If the middle
 * queue is empty, only message signals were present in the
 * outer queue, and no receive tracing has been enabled on
 * the process, the middle queue is bypassed and messages
 * are delivered directly to the inner queue instead.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 */
void erts_proc_sig_fetch(Process *p);

typedef struct {
    Uint size;
    ErtsMessage *msgp;
} ErtsMessageInfo;

/**
 *
 * @brief Prepare signal queue for inspection by process_info()
 *
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]   rp              Pointer to process struct of
 *                              process to inspect.
 *
 * @param[in]   rp_locks        Process locks held on 'rp'.
 *
 * @param[in]   mip             Pointer to array of
 *                              ErtsMessageInfo structures.
 *                              
 */

Uint erts_proc_sig_prep_msgq_for_inspection(Process *c_p,
                                            Process *rp,
                                            ErtsProcLocks rp_locks,
                                            ErtsMessageInfo *mip);


/**
 *
 * @brief Move message data of messages in private queues to heap
 *
 * Move message data of messages in private queues to the heap.
 * This is part of GC of processes that uses on-heap message
 * data.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 */
void erts_proc_sig_move_msgs_to_heap(Process *c_p);

/**
 *
 * @brief Size of signal in bytes.
 *
 * @param[in]   sig             Signal to inspect.
 *
 */
Uint erts_proc_sig_signal_size(ErtsSignal *sig);


/**
 *
 * @brief Clear seq trace tokens on all signals
 *
 * Assumes thread progress has been blocked!
 *
 * @param[in]   c_p             Pointer to process
 *
 */
void
erts_proc_sig_clear_seq_trace_tokens(Process *c_p);

/**
 * @brief Initialize this functionality
 */
void erts_proc_sig_queue_init(void);

void
erts_proc_sig_debug_foreach_sig(Process *c_p,
                                void (*msg_func)(ErtsMessage *, void *),
                                void (*oh_func)(ErlOffHeap *, void *),
                                void (*mon_func)(ErtsMonitor *, void *),
                                void (*lnk_func)(ErtsLink *, void *),
                                void *arg);

extern Process *erts_dirty_process_signal_handler;
extern Process *erts_dirty_process_signal_handler_high;
extern Process *erts_dirty_process_signal_handler_max;

#endif /* ERTS_PROC_SIG_QUEUE_H__ */
