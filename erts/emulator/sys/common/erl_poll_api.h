/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
/**
 * @description: Poll interface functions
 * @author Lukas Larsson
 *
 * The functions in the header are used to interact with the poll
 * implementation. Iff the kernel-poll implementation needs a fallback
 * pollset, then all functions are exported twice. Once with a _flbk
 * suffix and once without any suffix. If no fallback is needed, then
 * only the non-suffix version is exported.
 */

/**
 * Initialize the poll implementation. Has to be called before any other function.
 * @param[out] concurrent_waiters if not NULL, set to 1 if more then one thread
 * is allowed to wait in the pollsets at the same time.
 */
void ERTS_POLL_EXPORT(erts_poll_init)(int *concurrent_waiters);
/**
 * @brief Create a new pollset.
 * @param id The unique debug id of this pollset.
 */
ErtsPollSet *ERTS_POLL_EXPORT(erts_poll_create_pollset)(int id);

/**
 * Modify the contents of a pollset. This function can be called while one
 * (or possibly more) thread is waiting in the pollset.
 *
 * @param ps the pollset to modify
 * @param fd the file descriptor to modify
 * @param op the type of operation to do. Normal usage is ADD,MOD...MOD,DEL.
 * @param evts the events that we are changing interest to. Ignored if op is DEL.
 * @param[in] wake_poller if set to 1 any thread waiting in the pollset will be woken.
 *   This parameter is ignored if the pollset supports concurrent waiters.
 * @param[out] wake_poller set to 1 if the waiting thread was woken.
 * @return The events set, or ERTS_POLL_EV_NVAL if it was not possible to add the
 * fd to the pollset.
 */
ErtsPollEvents ERTS_POLL_EXPORT(erts_poll_control)(ErtsPollSet *ps,
                                                   ErtsSysFdType fd,
                                                   ErtsPollOp op,
                                                   ErtsPollEvents evts,
                                                   int *wake_poller);

/**
 * Wait for events to be ready in the pollset. If the erts_poll_init call
 * set concurrent_waiters to 1, then multiple threads are allowed to call
 * this function at the same time.
 *
 * When an event has been triggered on a fd, that event is disabled. To
 * re-enable it the implementation has to call erts_poll_control again.
 *
 * @param ps the pollset to wait for events in
 * @param res an array of fd results that the ready fds are put in.
 * @param[in] length the length of the res array
 * @param[out] length the number of ready events returned in res
 * @param tpd the thread progress data to note sleep state in
 * @param timeout_time the time in native to wake up at
 * @return 0 on success, else the ERRNO of the error that happened.
 */
int ERTS_POLL_EXPORT(erts_poll_wait)(ErtsPollSet *ps,
                                     ErtsPollResFd res[],
                                     int *length,
                                     ErtsThrPrgrData *tpd,
                                     ErtsMonotonicTime timeout_time);
/**
 * Interrupt the thread waiting in the pollset. This function should be called
 * with set = 0 before any thread calls erts_poll_wait in order to clear any
 * interrupts that have happened while the thread was awake.
 *
 * This function has no effect on pollsets that support concurrent waiters.
 *
 * @param ps the pollset to wake
 * @param set if 1, interrupt the pollset, if 0 clear the interrupt flag.
 */
void ERTS_POLL_EXPORT(erts_poll_interrupt)(ErtsPollSet *ps, int set);

/* Debug functions */

/**
 * Get the maximum number of fds supported by the pollset
 */
int ERTS_POLL_EXPORT(erts_poll_max_fds)(void);
/**
 * Get information about the given pollset
 */
void ERTS_POLL_EXPORT(erts_poll_info)(ErtsPollSet *ps,
                                      ErtsPollInfo *info);
/**
 * Get information about which events are currently selected.
 *
 * The unix fd is used to index into the array, so naturally this function does
 * not work on windows. If the pollset cannot figure out what the selected
 * events for a given fd is, it is set to ERTS_POLL_EV_NONE.
 *
 * @param ps the pollset to get events from
 * @param evts an array of which events are selected on.
 */
void ERTS_POLL_EXPORT(erts_poll_get_selected_events)(ErtsPollSet *ps,
                                                     ErtsPollEvents evts[],
                                                     int length);

#ifdef ERTS_ENABLE_LOCK_COUNT
/**
 * Enable lock counting of any locks within the pollset.
 */
void ERTS_POLL_EXPORT(erts_lcnt_enable_pollset_lock_count)(ErtsPollSet *, int enable);
#endif
