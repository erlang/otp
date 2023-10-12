/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2021. All Rights Reserved.
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
    int active;

    Links() {
        this(10);
    }

    Links(final int initialSize) {
        links = new Link[initialSize];
        count = 0;
        active = 0;
    }

    // Try add link and return if it was added or not...
    // If already existing it will not be added again.
    // If force is true it is added even if it is
    // currently unlinking; otherwise not.
    synchronized boolean addLink(final OtpErlangPid local,
                                 final OtpErlangPid remote,
                                 final boolean force) {
        int i = find(local, remote);
        if (i != -1) {
            if (links[i].getUnlinking() != 0 && force) {
                links[i].setUnlinking(0);
                active++;
                return true;
            }
            return false;
        }
        else {
            if (count >= links.length) {
                final Link[] tmp = new Link[count * 2];
                System.arraycopy(links, 0, tmp, 0, count);
                links = tmp;
            }
            links[count++] = new Link(local, remote);
            active++;
            return true;
        }
    }

    // Try remove link and return whether it was active or not...
    synchronized boolean removeLink(final OtpErlangPid local,
                                    final OtpErlangPid remote) {
        int i;

        if ((i = find(local, remote)) != -1) {
            long unlinking = links[i].getUnlinking();
            count--;
            links[i] = links[count];
            links[count] = null;
            if (unlinking == 0) {
                active--;
                return true;
            }
        }
        return false;
    }

    // Try remove active link and return whether it was removed or not...
    synchronized boolean removeActiveLink(final OtpErlangPid local,
                                          final OtpErlangPid remote) {
        int i;

        if ((i = find(local, remote)) != -1) {
            long unlinking = links[i].getUnlinking();
            if (unlinking != 0)
                return false;
            count--;
            active--;
            links[i] = links[count];
            links[count] = null;
            return true;
        }
        return false;
    }

    // Remove link if unlink_id match and return whether it was removed or not...
    synchronized boolean removeUnlinkingLink(final OtpErlangPid local,
                                             final OtpErlangPid remote,
                                             final long unlink_id) {
        int i;

        if (unlink_id == 0) {
            return false;
        }

        if ((i = find(local, remote)) != -1) {
            long unlinking = links[i].getUnlinking();
            if (unlinking != unlink_id)
                return false;
            count--;
            links[i] = links[count];
            links[count] = null;
            return true;
        }
        return false;
    }

    synchronized boolean setUnlinking(final OtpErlangPid local,
                                      final OtpErlangPid remote,
                                      final long unlink_id) {
        int i;

        if (unlink_id == 0) {
            return false;
        }

        if ((i = find(local, remote)) != -1) {
            if (links[i].getUnlinking() == 0) {
                links[i].setUnlinking(unlink_id);
                active--;
                return true;
            }
        }
        return false;
    }

    synchronized int find(final OtpErlangPid local, final OtpErlangPid remote) {
        for (int i = 0; i < count; i++) {
            if (links[i].equals(local, remote)) {
                return i;
            }
        }
        return -1;
    }

    synchronized OtpErlangPid[] remotePids() {
        OtpErlangPid[] ret = null;
        if (active != 0) {
            int a = 0;
            ret = new OtpErlangPid[active];
            for (int i = 0; a < active; i++) {
                if (links[i].getUnlinking() == 0) {
                    ret[a++] = links[i].remote();
                }
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

}
