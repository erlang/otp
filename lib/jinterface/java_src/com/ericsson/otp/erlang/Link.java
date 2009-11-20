/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
package com.ericsson.otp.erlang;

// package scope
class Link {
    private final OtpErlangPid local;
    private final OtpErlangPid remote;
    private int hashCodeValue = 0;

    public Link(final OtpErlangPid local, final OtpErlangPid remote) {
	this.local = local;
	this.remote = remote;
    }

    public OtpErlangPid local() {
	return local;
    }

    public OtpErlangPid remote() {
	return remote;
    }

    public boolean contains(final OtpErlangPid pid) {
	return local.equals(pid) || remote.equals(pid);
    }

    public boolean equals(final OtpErlangPid local, final OtpErlangPid remote) {
	return this.local.equals(local) && this.remote.equals(remote)
		|| this.local.equals(remote) && this.remote.equals(local);
    }
    
    public int hashCode() {
	if (hashCodeValue == 0) {
	    OtpErlangObject.Hash hash = new OtpErlangObject.Hash(5);
	    hash.combine(local.hashCode() + remote.hashCode());
	    hashCodeValue = hash.valueOf();
	}
	return hashCodeValue;
    }
}
