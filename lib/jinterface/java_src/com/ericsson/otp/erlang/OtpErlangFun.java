/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

import java.util.Arrays;

public class OtpErlangFun extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = -3423031125356706472L;

    private final OtpErlangPid pid;
    private final String module;
    private final long index;
    private final long old_index;
    private final long uniq;
    private final OtpErlangObject[] freeVars;
    private final int arity;
    private final byte[] md5;

    public OtpErlangFun(final OtpInputStream buf)
            throws OtpErlangDecodeException {
        final OtpErlangFun f = buf.read_fun();
        pid = f.pid;
        module = f.module;
        arity = f.arity;
        md5 = f.md5;
        index = f.index;
        old_index = f.old_index;
        uniq = f.uniq;
        freeVars = f.freeVars;
    }

    public OtpErlangFun(final OtpErlangPid pid, final String module,
            final long index, final long uniq, final OtpErlangObject[] freeVars) {
        this.pid = pid;
        this.module = module;
        arity = -1;
        md5 = null;
        this.index = index;
        old_index = 0;
        this.uniq = uniq;
        this.freeVars = freeVars;
    }

    public OtpErlangFun(final OtpErlangPid pid, final String module,
            final int arity, final byte[] md5, final int index,
            final long old_index, final long uniq,
            final OtpErlangObject[] freeVars) {
        this.pid = pid;
        this.module = module;
        this.arity = arity;
        this.md5 = md5;
        this.index = index;
        this.old_index = old_index;
        this.uniq = uniq;
        this.freeVars = freeVars;
    }

    @Override
    public void encode(final OtpOutputStream buf) {
        buf.write_fun(pid, module, old_index, arity, md5, index, uniq, freeVars);
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof OtpErlangFun)) {
            return false;
        }
        final OtpErlangFun f = (OtpErlangFun) o;
        if (!pid.equals(f.pid) || !module.equals(f.module) || arity != f.arity) {
            return false;
        }
        if (md5 == null) {
            if (f.md5 != null) {
                return false;
            }
        } else {
            if (!Arrays.equals(md5, f.md5)) {
                return false;
            }
        }
        if (index != f.index || uniq != f.uniq) {
            return false;
        }
        if (freeVars == null) {
            return f.freeVars == null;
        }
        return Arrays.equals(freeVars, f.freeVars);
    }

    @Override
    protected int doHashCode() {
        final OtpErlangObject.Hash hash = new OtpErlangObject.Hash(1);
        hash.combine(pid.hashCode(), module.hashCode());
        hash.combine(arity);
        if (md5 != null) {
            hash.combine(md5);
        }
        hash.combine(index);
        hash.combine(uniq);
        if (freeVars != null) {
            for (final OtpErlangObject o : freeVars) {
                hash.combine(o.hashCode(), 1);
            }
        }
        return hash.valueOf();
    }

    @Override
    public String toString() {
        return "#Fun<" + module + "." + old_index + "." + uniq + ">";
    }

}
