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

// package scope
class Links {
    Link[] links;
    int count;

    Links() {
        this(10);
    }

    Links(final int initialSize) {
        links = new Link[initialSize];
        count = 0;
    }

    synchronized void addLink(final OtpErlangPid local,
            final OtpErlangPid remote) {
        if (find(local, remote) == -1) {
            if (count >= links.length) {
                final Link[] tmp = new Link[count * 2];
                System.arraycopy(links, 0, tmp, 0, count);
                links = tmp;
            }
            links[count++] = new Link(local, remote);
        }
    }

    synchronized void removeLink(final OtpErlangPid local,
            final OtpErlangPid remote) {
        int i;

        if ((i = find(local, remote)) != -1) {
            count--;
            links[i] = links[count];
            links[count] = null;
        }
    }

    synchronized boolean exists(final OtpErlangPid local,
            final OtpErlangPid remote) {
        return find(local, remote) != -1;
    }

    synchronized int find(final OtpErlangPid local, final OtpErlangPid remote) {
        for (int i = 0; i < count; i++) {
            if (links[i].equals(local, remote)) {
                return i;
            }
        }
        return -1;
    }

    int count() {
        return count;
    }

    /* all local pids get notified about broken connection */
    synchronized OtpErlangPid[] localPids() {
        OtpErlangPid[] ret = null;
        if (count != 0) {
            ret = new OtpErlangPid[count];
            for (int i = 0; i < count; i++) {
                ret[i] = links[i].local();
            }
        }
        return ret;
    }

    /* all remote pids get notified about failed pid */
    synchronized OtpErlangPid[] remotePids() {
        OtpErlangPid[] ret = null;
        if (count != 0) {
            ret = new OtpErlangPid[count];
            for (int i = 0; i < count; i++) {
                ret[i] = links[i].remote();
            }
        }
        return ret;
    }

    /* clears the link table, returns a copy */
    synchronized Link[] clearLinks() {
        Link[] ret = null;
        if (count != 0) {
            ret = new Link[count];
            for (int i = 0; i < count; i++) {
                ret[i] = links[i];
                links[i] = null;
            }
            count = 0;
        }
        return ret;
    }

    /* returns a copy of the link table */
    synchronized Link[] links() {
        Link[] ret = null;
        if (count != 0) {
            ret = new Link[count];
            System.arraycopy(links, 0, ret, 0, count);
        }
        return ret;
    }
}
