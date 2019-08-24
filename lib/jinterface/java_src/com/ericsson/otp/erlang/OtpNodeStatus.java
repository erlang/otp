/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
package com.ericsson.otp.erlang;

/**
 * <p>
 * Provides a callback mechanism for receiving status change information about
 * other nodes in the system. Register an instance of this class (or a subclass)
 * with your {@link OtpNode OtpNode} when you wish to be notified about such
 * status changes and other similar events.
 * </p>
 *
 * <p>
 * This class provides default handers that ignore all events. Applications are
 * expected to extend this class in order to act on events that are deemed
 * interesting.
 * </p>
 *
 * <p>
 * <b> Note that this class is likely to change in the near future </b>
 * </p>
 */
public class OtpNodeStatus {
    public OtpNodeStatus() {
    }

    /**
     * Notify about remote node status changes.
     *
     * @param node
     *            the node whose status change is being indicated by this call.
     *
     * @param up
     *            true if the node has come up, false if it has gone down.
     *
     * @param info
     *            additional info that may be available, for example an
     *            exception that was raised causing the event in question (may
     *            be null).
     *
     */
    public void remoteStatus(final String node, final boolean up,
            final Object info) {
    }

    /**
     * Notify about local node exceptions.
     *
     * @param node
     *            the node whose status change is being indicated by this call.
     *
     * @param up
     *            true if the node has come up, false if it has gone down.
     *
     * @param info
     *            additional info that may be available, for example an
     *            exception that was raised causing the event in question (may
     *            be null).
     */
    public void localStatus(final String node, final boolean up,
            final Object info) {
    }

    /**
     * Notify about failed connection attempts.
     *
     * @param node
     *            The name of the remote node
     *
     * @param incoming
     *            The direction of the connection attempt, i.e. true for
     *            incoming, false for outgoing.
     *
     * @param info
     *            additional info that may be available, for example an
     *            exception that was raised causing the event in question (may
     *            be null).
     */
    public void connAttempt(final String node, final boolean incoming,
            final Object info) {
    }
}
