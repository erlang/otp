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
 * This class implements a generic FIFO queue. There is no upper bound on the
 * length of the queue, items are linked.
 */

public class GenericQueue {
    private static final int open = 0;
    private static final int closing = 1;
    private static final int closed = 2;

    private int status;
    private Bucket head;
    private Bucket tail;
    private int count;

    private void init() {
        head = null;
        tail = null;
        count = 0;
    }

    /** Create an empty queue */
    public GenericQueue() {
        init();
        status = open;
    }

    /** Clear a queue */
    public void flush() {
        init();
    }

    public void close() {
        status = closing;
    }

    /**
     * Add an object to the tail of the queue.
     *
     * @param o
     *            Object to insert in the queue
     */
    public synchronized void put(final Object o) {
        final Bucket b = new Bucket(o);

        if (tail != null) {
            tail.setNext(b);
            tail = b;
        } else {
            // queue was empty but has one element now
            head = tail = b;
        }
        count++;

        // notify any waiting tasks
        notify();
    }

    /**
     * Retrieve an object from the head of the queue, or block until one
     * arrives.
     *
     * @return The object at the head of the queue.
     */
    public synchronized Object get() {
        Object o = null;

        while ((o = tryGet()) == null) {
            try {
                this.wait();
            } catch (final InterruptedException e) {
            }
        }
        return o;
    }

    /**
     * Retrieve an object from the head of the queue, blocking until one arrives
     * or until timeout occurs.
     *
     * @param timeout
     *            Maximum time to block on queue, in ms. Use 0 to poll the
     *            queue.
     *
     * @exception InterruptedException
     *                if the operation times out.
     *
     * @return The object at the head of the queue, or null if none arrived in
     *         time.
     */
    public synchronized Object get(final long timeout)
            throws InterruptedException {
        if (status == closed) {
            return null;
        }

        long currentTime = System.currentTimeMillis();
        final long stopTime = currentTime + timeout;
        Object o = null;

        while (true) {
            if ((o = tryGet()) != null) {
                return o;
            }

            currentTime = System.currentTimeMillis();
            if (stopTime <= currentTime) {
                throw new InterruptedException("Get operation timed out");
            }

            try {
                this.wait(stopTime - currentTime);
            } catch (final InterruptedException e) {
                // ignore, but really should retry operation instead
            }
        }
    }

    // attempt to retrieve message from queue head
    public Object tryGet() {
        Object o = null;

        if (head != null) {
            o = head.getContents();
            head = head.getNext();
            count--;

            if (head == null) {
                tail = null;
                count = 0;
            }
        }

        return o;
    }

    public synchronized int getCount() {
        return count;
    }

    /*
     * The Bucket class. The queue is implemented as a linked list of Buckets.
     * The container holds the queued object and a reference to the next Bucket.
     */
    class Bucket {
        private Bucket next;
        private final Object contents;

        public Bucket(final Object o) {
            next = null;
            contents = o;
        }

        public void setNext(final Bucket newNext) {
            next = newNext;
        }

        public Bucket getNext() {
            return next;
        }

        public Object getContents() {
            return contents;
        }
    }
}
