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
 * Provides a collection of constants used when encoding and decoding Erlang
 * terms.
 */
public class OtpExternal {
    // no constructor
    private OtpExternal() {
    }

    /** The tag used for small integers */
    public static final int smallIntTag = 97;

    /** The tag used for integers */
    public static final int intTag = 98;

    /** The tag used for floating point numbers */
    public static final int floatTag = 99;
    public static final int newFloatTag = 70;

    /** The tag used for atoms */
    public static final int atomTag = 100;

    /** The tag used for old stype references */
    public static final int refTag = 101;

    /** The tag used for ports */
    public static final int portTag = 102;
    public static final int newPortTag = 89;

    /** The tag used for PIDs */
    public static final int pidTag = 103;
    public static final int newPidTag = 88;

    /** The tag used for small tuples */
    public static final int smallTupleTag = 104;

    /** The tag used for large tuples */
    public static final int largeTupleTag = 105;

    /** The tag used for empty lists */
    public static final int nilTag = 106;

    /** The tag used for strings and lists of small integers */
    public static final int stringTag = 107;

    /** The tag used for non-empty lists */
    public static final int listTag = 108;

    /** The tag used for binaries */
    public static final int binTag = 109;

    /** The tag used for bitstrs */
    public static final int bitBinTag = 77;

    /** The tag used for small bignums */
    public static final int smallBigTag = 110;

    /** The tag used for large bignums */
    public static final int largeBigTag = 111;

    /** The tag used for old new Funs */
    public static final int newFunTag = 112;

    /** The tag used for external Funs (M:F/A) */
    public static final int externalFunTag = 113;

    /** The tag used for new style references */
    public static final int newRefTag = 114;
    public static final int newerRefTag = 90;

    /** The tag used for maps */
    public static final int mapTag = 116;

    /** The tag used for old Funs */
    public static final int funTag = 117;

    /** The tag used for unicode atoms */
    public static final int atomUtf8Tag = 118;

    /** The tag used for small unicode atoms */
    public static final int smallAtomUtf8Tag = 119;

    /** The tag used for compressed terms */
    public static final int compressedTag = 80;

    /** The version number used to mark serialized Erlang terms */
    public static final int versionTag = 131;

    /** The largest value that can be encoded as an integer */
    public static final int erlMax = (1 << 27) - 1;

    /** The smallest value that can be encoded as an integer */
    public static final int erlMin = -(1 << 27);

    /** The longest allowed Erlang atom */
    public static final int maxAtomLength = 255;
}
